# data-raw/data-prep.R
# Produces: cog-data/min_flow_inundation_<alternative>_lake_<lakelevel>.tif
# Input: data-raw/plans-long.csv with columns:
#   alternative, geom_name, geom_id, plan_name, plan_id, flow_cfs, result_dts, lake_level, (optional) rast_path

library(terra)
library(sf)
library(dplyr)
library(tidyr)
library(readr)
library(stringr)
library(purrr)
library(glue)
library(here)

# ---------------- CONFIG ----------------

# Output CRS and resolution 
PROJ_CRS <- "EPSG:3857"  # Web Mercator for leaflet map
CELL_SIZE <- 0.6096   # meters

# Directory where HEC-RAS results live
RAS_RESULTS_DIR <- "Y:/Shared/Active_Projects/114-01_Kuhlanapo_Preserve/Modeling/01_Kuulanapo_RASv66"

# Input CSV
PLANS_CSV <- here("data", "plans-long.csv")

# Output COG directory
OUT_COG_DIR <- here("data", "cog")
dir.create(OUT_COG_DIR, recursive = TRUE, showWarnings = FALSE)

# AOI shapefile (used for safe cropping/template alignment)
AOI_SHP <- here("data-raw", "kuulanapo_bnd.shp.zip")
if (!file.exists(PLANS_CSV)) stop("Missing plans-long.csv at: ", PLANS_CSV)
if (!file.exists(AOI_SHP)) warning("AOI shapefile not found at: ", AOI_SHP, "  (processing will continue but cropping may be broader)")

# ---------------- READ METADATA ----------------
plans <- read_csv(PLANS_CSV, show_col_types = FALSE) |>
  mutate(
    plan_name = as.character(plan_name),
    alternative = as.character(alternative),
    lake_level = as.character(lake_level),
    flow_cfs = as.numeric(flow_cfs),
  )

# Validate required columns
req_cols <- c("alternative", "plan_name", "flow_cfs", "result_dts", "lake_level")
missing_cols <- setdiff(req_cols, names(plans))
if (length(missing_cols) > 0) stop("plans-long.csv must include these columns: ", paste(req_cols, collapse = ", "),
                                 "\nMissing: ", paste(missing_cols, collapse = ", "))

# ---------------- AOI / TEMPLATE SETUP ----------------

# snap extents to aligned grid
snap_extent_to_origin <- function(extent_obj, res) {
  xmin <- floor(extent_obj[1] / res) * res
  xmax <- ceiling(extent_obj[2] / res) * res
  ymin <- floor(extent_obj[3] / res) * res
  ymax <- ceiling(extent_obj[4] / res) * res
  terra::ext(xmin, xmax, ymin, ymax)
}

aoi_vect <- NULL
if (file.exists(AOI_SHP)) {
  aoi_sf <- st_read(AOI_SHP, quiet = TRUE)
  # Project AOI to web mercator (3857)
  aoi_sf_proj <- st_transform(aoi_sf, crs = st_crs(PROJ_CRS))
  aoi_vect <- vect(aoi_sf_proj)
  aoi_ext <- aoi_sf_proj$geometry |>
    st_union() |>
    st_buffer(dist = 100) |>
    vect() |>
    ext() |>
    snap_extent_to_origin(res = CELL_SIZE)
  # write geojson in 4326 for the app (if desired)
  aoi_sf |> 
    st_transform("EPSG:4326") |> 
    st_write(here("data","project_boundary.geojson"), delete_dsn = TRUE, quiet = TRUE)
} else {
  message("AOI shapefile not present; cropping will use raster extents instead.")
}


read_sf(here("data-raw", "Manning_Creek_Centerline_20260102.shp.zip")) |> 
  st_transform("EPSG:4326") |> 
  st_write(here("data","stream_lines.geojson"), delete_dsn = TRUE, quiet = TRUE)

read_sf(here("data-raw", "stage_gage_locations_approx.shp.zip")) |> 
  st_transform("EPSG:4326") |> 
  st_write(here("data","stream_gages.geojson"), delete_dsn = TRUE, quiet = TRUE)


# ---------------- HELPERS ----------------

# Build expected WSE filename from result_dts 
build_wse_name_from_dts <- function(result_dts) {
  dt <- as.POSIXct(result_dts, tz = "UTC")
  if (is.na(dt)) return(NA_character_)
  fmt <- toupper(format(dt, "%d%b%Y %H %M %S"))
  glue("WSE ({fmt}).vrt")
}

# Discover WSE file in plan folder
find_wse_in_plan_folder <- function(plan_name, flow_cfs) {
  plan_dir <- file.path(RAS_RESULTS_DIR, plan_name)
  if (!dir.exists(plan_dir)) return(NULL)
  files <- list.files(plan_dir, pattern = "WSE.*(vrt|tif|tiff)$", full.names = TRUE, ignore.case = TRUE)
  if (length(files) == 0) return(NULL)
  # try to match flow token in filenames
  tok <- as.character(flow_cfs)
  matched <- files[str_detect(files, fixed(tok))]
  if (length(matched) >= 1) return(matched[1])
  # fallback: first WSE file
  return(files[1])
}

resolve_rast_path <- function(row) {
  # precedence: explicit rast_path -> result_dts-derived WSE filename -> discover in plan folder
  if (!is.null(row$rast_path) && nzchar(row$rast_path) && file.exists(row$rast_path)) return(row$rast_path)
  if (!is.null(row$result_dts) && nzchar(row$result_dts)) {
    candidate <- file.path(RAS_RESULTS_DIR, row$plan_name, build_wse_name_from_dts(row$result_dts))
    if (file.exists(candidate)) return(candidate)
  }
  discovered <- find_wse_in_plan_folder(row$plan_name, row$flow_cfs)
  if (!is.null(discovered)) return(discovered)
  return(NA_character_)
}

# read + project + resample/crop to template
read_prepare <- function(fp, template = NULL, filename = NULL) {
  if(!file.exists(filename)) {
    r <- rast(fp)
    if (is.na(crs(r))) stop("Raster has no CRS: ", fp)
    # resample to template grid if it exists
    r_crop <- r |> crop(project(aoi_ext, PROJ_CRS, crs(r)))
    r_crop_proj <- r_crop |> project(PROJ_CRS)
    r_crop_proj_rs <- r_crop_proj |> resample(template)
    r_crop_proj_rs |> writeRaster(filename, overwrite=T)
  } else {
    r_crop_proj_rs <- rast(filename)
  }
  return(r_crop_proj_rs)
}

# ---------------- BUILD RAST PATHS ----------------
plans <- plans |>
  mutate(rast_path = NA_character_,
         rast_resolved = FALSE)

for (i in seq_len(nrow(plans))) {
  plans$rast_path[i] <- resolve_rast_path(plans[i, , drop = FALSE])
  plans$rast_resolved[i] <- !is.na(plans$rast_path[i])
  if (!plans$rast_resolved[i]) {
    warning("Could not resolve WSE raster for row ", i,
            " plan=", plans$plan_name[i],
            " flow=", plans$flow_cfs[i], " lake_level=", plans$lake_level[i])
  } else {
    message("Resolved raster: ", plans$rast_path[i])
  }
}

plans_avail <- filter(plans, rast_resolved)

if (nrow(plans_avail) == 0) stop("No WSE rasters resolved. Fix plans-long.csv or RAS_RESULTS_DIR.")

# ------------- PROCESS GROUPS: alternative x lake_level
groups <- plans_avail |>
  group_by(alternative, lake_level) |>
  group_split()

for (grp in groups) {
  alt <- unique(grp$alternative)
  lake_level <- unique(grp$lake_level)
  message("Processing alternative=", alt, " lake_level=", lake_level)

  # determine template: use first raster in group to build template aligned to CELL_SIZE
  first_fp <- grp$rast_path[1]
  first_r <- rast(first_fp)
  # construct template raster with ext_snap
  ncol <- ceiling((aoi_ext[2] - aoi_ext[1]) / CELL_SIZE)
  nrow <- ceiling((aoi_ext[4] - aoi_ext[3]) / CELL_SIZE)
  template <- rast(nrows = nrow, ncols = ncol,
                   xmin = aoi_ext[1], xmax = aoi_ext[2],
                   ymin = aoi_ext[3], ymax = aoi_ext[4],
                   crs = PROJ_CRS)

  # read and resample all rasters in the group to template
  rlist <- list()
  flow_vals <- numeric(0)
  for (j in seq_len(nrow(grp))) {
    fp <- grp$rast_path[j]
    fv <- grp$flow_cfs[j]
    filename <- here("data-raw", "rasters", str_glue("wse_{alt}_{lake_level}_{fv}.tif"))
    message("  reading: ", fp, " (flow=", fv, ")")
    message(" to ", filename)
    rj <- tryCatch(read_prepare(fp, template = template, filename = filename),
                   error = function(e) { warning("  failed reading ", fp, ": ", e$message); NULL })
    if (!is.null(rj)) {
      rlist[[length(rlist) + 1]] <- rj
      flow_vals <- c(flow_vals, fv)
    }
  }

  if (length(rlist) == 0) {
    warning("No rasters successfully read for alternative=", alt, " lake_level=", lake_level, " (skipping)")
    next
  }

  # stack; order matches flow_vals
  s_r <- rast(rlist)
  names(s_r) <- paste0("flow_", seq_along(flow_vals))

  # compute per-pixel minimum flow
  flow_values <- flow_vals
  min_flow_fun <- function(...) {
    vals <- c(...)
    inund <- which(!is.na(vals)) # & vals >= lake_level)
    if (length(inund) == 0) return(NA_real_)
    return(min(flow_values[inund]))
  }
  message("  computing min modeled flow that inundates pixel")
  min_flow_rast <- app(s_r, fun = min_flow_fun, cores = 1)
  names(min_flow_rast) <- "min_flow"

  # write COG per alternative+lake_level
  alt_s <- make.names(alt)
  lake_s <- ifelse(is.na(lake_level), "NA", gsub("[^0-9A-Za-z\\-\\.]", "_", as.character(lake_level)))
  outfn <- file.path(OUT_COG_DIR, paste0("min_flow_inundation_", alt_s, "_lake_", lake_s, ".tif"))
  message("  writing COG: ", outfn)
  gdal_opts <- c("TILED=YES", "BLOCKXSIZE=512", "BLOCKYSIZE=512",
                 "COMPRESS=DEFLATE", "PREDICTOR=3", "BIGTIFF=IF_SAFER",
                 "COPY_SRC_OVERVIEWS=YES")
  writeRaster(min_flow_rast, outfn, overwrite = TRUE, datatype = "FLT4S", gdal = gdal_opts)

  # add overviews if gdaladdo present
  gdaladdo_cmd <- Sys.which("gdaladdo") 
  gdaladdo_cmd <- r"(C:\OSGeo4W\bin\gdaladdo)"
  if (nzchar(gdaladdo_cmd)) {
    olevels <- c(2,4,8,16,32)
    cmd <- sprintf("%s -r average %s %s", gdaladdo_cmd, shQuote(outfn), paste(olevels, collapse = " "))
    message("  running: ", cmd)
    system(cmd)
  } else {
    warning("gdaladdo not found; consider running gdaladdo to create overviews for better COG performance.")
  }

  message("Completed: ", outfn)
}

message("Data prep finished")
