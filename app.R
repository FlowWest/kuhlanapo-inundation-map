# app.R
library(shiny)
library(leaflet)
library(terra)
library(sf)
library(htmlwidgets)
library(glue)
library(dplyr)
library(shinycssloaders)
library(stringr)

# -------- CONFIG --------
COG_URL_TEMPLATE <- "https://raw.githubusercontent.com/FlowWest/kuhlanapo-inundation-map/main/data/cog/min_flow_inundation_{alt}_lake_{lake}.tif"
local_cache_dir <- "data_cache"
dir.create(local_cache_dir, recursive = TRUE, showWarnings = FALSE)

# static geojsons in repo (EPSG:4326)
boundary_geo  <- "data/project_boundary.geojson"
creeks_geo    <- "data/stream_lines.geojson"
gages_geo     <- "data/stream_gages.geojson"

# lake level attributes
lake_level_meta <- tibble::tribble(~id    , ~name    , ~ft_navd88
                                 , "90EP" , "90% EP" , 1321.7
                                 , "50EP" , "Median" , 1324.4
                                 , "Full" , "Full"   , 1320.74 + 7.56
                                 ) 

# -------- helpers --------
open_cog <- function(alt, lake, return_path = TRUE) {
  local_path <- here::here("data/cog", paste0("min_flow_inundation_", alt, "_lake_", lake, ".tif"))
  
  if (file.exists(local_path)) {
    rast_path <- local_path
  } else {
    cog_url <- glue(COG_URL_TEMPLATE, alt = alt, lake = lake)
    cache_path <- file.path(local_cache_dir, basename(local_path))
    
    if (!file.exists(cache_path)) {
      download.file(cog_url, cache_path, mode = "wb")
    }
    rast_path <- cache_path
  }
  
  if (return_path) rast_path else rast(rast_path)
}

# -------- discover alternatives --------
plans_csv <- "data/plans-long.csv"
if (!file.exists(plans_csv)) stop("plans-long.csv missing")
plans_df <- read.csv(plans_csv, stringsAsFactors = FALSE)
alternatives <- unique(plans_df$alternative)
lake_levels <- unique(plans_df$lake_level)
flows <- unique(plans_df$flow_cfs)

# read vector layers if present (EPSG:4326)
boundary_sf <- if (file.exists(boundary_geo)) st_read(boundary_geo, quiet = TRUE) else NULL
creeks_sf   <- if (file.exists(creeks_geo)) st_read(creeks_geo, quiet = TRUE) else NULL
gages_sf    <- if (file.exists(gages_geo)) st_read(gages_geo, quiet = TRUE) else NULL

# clean up lake level meta
lake_level_meta <- lake_level_meta |>
  mutate(ft_rumsey = ft_navd88 - 1320.74,
         label = str_glue("{sprintf('%.1f', ft_rumsey)} ft ({name})")) |>
  filter(id %in% lake_levels)

# -------- UI --------
ui <- fluidPage(
  shinyjs::useShinyjs(),
  tags$link(rel = "stylesheet", href = "leaflet.css"),
  sidebarLayout(
    sidebarPanel(
      class = "fixed-sidebar",  
      width = 3,  # narrow sidebar
      titlePanel("Kuhlanapo Wetland Restoration"),
      tags$fieldset(
        tags$legend("Modeled Inundation"),
        radioButtons("alternative", "Alternative", choices = alternatives, selected = alternatives[1]),
        radioButtons("lake_level", "Lake Level", choices = setNames(lake_level_meta$id, lake_level_meta$label), selected = lake_levels[1]),
        radioButtons("flow", "Flow (cfs)", choices = flows, selected = 80),
        checkboxInput("show_all", "Overlay All Flows", value = FALSE)
      )
    ),
    mainPanel(
      class = "main-map",
      width = 9,  # remaining space for the map
      withSpinner(
        leafletOutput("map", height = "100vh"),
        type = 6,
        color = "#0073B7",
        size = 2
      )
    ) 
  )
)


# -------- SERVER --------
server <- function(input, output, session) {
  
  # Observe the state of the "disable_checkbox" input
  observeEvent(input$show_all, {
    if (input$show_all == TRUE) {
      shinyjs::disable("flow")
    } else {
      shinyjs::enable("flow") 
    }
  })
  
  # reactiveValues to track last raster group
  last_group <- reactiveValues(name = NULL)
  
  # reactiveVal to hold preprocessed raster for current alt/lake
  prepped_raster <- reactiveVal(NULL)
  
  # -------------------------
  # INITIALIZE MAP
  # -------------------------
  
  add_layers_control <- function(m) {
    m |> addLayersControl(baseGroups=c("Imagery + Topo", "Imagery Only", "Topo Only"),
                          overlayGroups=c("Project Boundary","Creeks","Streamgages"),
                          options=layersControlOptions(collapsed=FALSE))
  }
  
  output$map <- renderLeaflet({
    if (!is.null(boundary_sf)) {
      bbox <- st_bbox(boundary_sf)
      lng <- (bbox$xmin + bbox$xmax)/2
      lat <- (bbox$ymin + bbox$ymax)/2
      z <- 12
    } else { lng <- 0; lat <- 0; z <- 2 }
    
    aerial_plus_topo <- leaflet() |> 
      leaflet::addProviderTiles(
        providers$Esri.WorldImagery,
        options = providerTileOptions(pane = "basemap-mono", opacity = 0.5, minZoom = 3, maxZoom = 18)
      ) |>
      leaflet::addProviderTiles(
        providers$Esri.WorldTopo,
        options = providerTileOptions(pane = "basemap-overlay", opacity = 1.0, minZoom = 3, maxZoom = 18)
      )
    
    m <- leaflet() |>
      addMapPane("basemap",    zIndex = 200) |>
      addMapPane("basemap-mono",    zIndex = 200) |>
      addMapPane("basemap-overlay",    zIndex = 200) |>
      addMapPane("inundation", zIndex = 410) |>
      addMapPane("polygons",   zIndex = 420) |>
      addMapPane("lines",      zIndex = 430) |>
      addMapPane("points",     zIndex = 440)  |>
      addProviderTiles(
        providers$Esri.WorldImagery,
        group = "Imagery Only",
        options = providerTileOptions(pane = "basemap", opacity = 0.8, minZoom = 3, maxZoom = 18)
      ) |>
      addProviderTiles(
        providers$Esri.WorldTopo,
        group = "Topo Only",
        options = providerTileOptions(pane = "basemap", opacity = 1.0, minZoom = 3, maxZoom = 18)
      ) |>
      addProviderTiles(
        providers$Esri.WorldImagery,
        group = "Imagery + Topo",
        options = providerTileOptions(pane = "basemap-mono", opacity = 0.5, minZoom = 3, maxZoom = 18)
      ) |>
      addProviderTiles(
        providers$Esri.WorldTopo,
        group = "Imagery + Topo",
        options = providerTileOptions(pane = "basemap-overlay", opacity = 1.0, minZoom = 3, maxZoom = 18)
      ) |> 
      add_layers_control()
    
    if (!is.null(boundary_sf)) m <- m |> addPolygons(data=boundary_sf, group="Project Boundary", fill=FALSE, color="#ffc600", weight=2, opacity=1, 
                                                     options=pathOptions(pane="polygons", dashArray = "5,5"))
    if (!is.null(creeks_sf)) m <- m |> addPolylines(data=creeks_sf, group="Creeks", color="#00c6ff", weight=2, opacity=1,
                                                    options=pathOptions(pane="lines", dashArray = "6,4"))
    if (!is.null(gages_sf)) m <- m |> addCircleMarkers(data=gages_sf, group="Streamgages", radius=4, color="#8B0000", opacity=1,
                                                       options=markerOptions(pane="points"))
    
    return(m)
  })
  
  # -------------------------
  # PREPROCESS RASTER PER ALT/LAKE
  # -------------------------
  observeEvent({
    input$alternative
    input$lake_level
  }, {
    req(input$alternative, input$lake_level)
    
    notif <- showNotification("Loading raster...", duration = NULL, type = "message", closeButton = FALSE)
    on.exit(removeNotification(notif), add = TRUE)
    
    r <- tryCatch(open_cog(as.character(input$alternative), as.character(input$lake_level)), 
                  error=function(e) { showNotification("Failed to open COG", type="error"); NULL })
    req(r)
    
    # Aggregate once to reasonable resolution 
    r_agg <- aggregate(rast(r), fact = 2, fun = "min")
    
    prepped_raster(r_agg)
    
    # clear old raster from map
    proxy <- leafletProxy("map")
    if (!is.null(last_group$name)) proxy |> clearGroup(last_group$name)
    last_group$name <- NULL
  })
  
  # -------------------------
  # UPDATE RASTER FOR FLOW CHANGES
  # -------------------------
  observeEvent(list(input$flow, input$show_all, prepped_raster()), {
    req(prepped_raster())
    
    proxy <- leafletProxy("map")
    
    # remove old raster
    if (!is.null(last_group$name)) proxy |> clearGroup(last_group$name)
    
    r <- prepped_raster()
    
    if (isTRUE(input$show_all)) {
      # Continuous log-scaled viridis ramp
      # compute min/max safely (avoid <= 0 values)
      stats <- terra::global(r, fun = c("min", "max"), na.rm = TRUE)
      minv <- stats[1, 1]
      maxv <- stats[1, 2]
      
      # guard against non-positive mins
      eps <- 1e-6
      minv2 <- ifelse(is.na(minv) || minv <= 0, eps, minv)
      maxv2 <- ifelse(is.na(maxv) || maxv <= minv2, minv2 * 10, maxv)
      
      # custom palette ramp 
      pal_colors <- colorRampPalette(c("#224477", "#0088FF", "#99FFCC"))(256)
      pal_fn <- colorNumeric(pal_colors, domain = log10(c(minv2, maxv2)), na.color = NA)
      n_colors <- length(flows)
      
      # create ramp
      pal_discrete <- colorRampPalette(c("#224477", "#0088FF", "#99FFCC"))(n_colors)
      legend_labels <- as.character(flows)
      
      # define breaks explicitly
      breaks <- c(0, flows) 
      
      colors_fn <- function(x) {
        xvec <- as.numeric(x)
        cols <- rep(NA_character_, length(xvec))
        valid <- !is.na(xvec)
        xvec_pos <- pmax(xvec[valid], eps)
        
        # numeric bin index
        bins <- cut(xvec_pos, breaks = breaks, include.lowest = TRUE, labels = FALSE)
        cols[valid] <- pal_discrete[bins]
        
        dim(cols) <- dim(x)
        cols
      }
      
      # add raster with continuous colors
      new_group <- paste0("Inundation_all_", as.integer(Sys.time()))
      proxy |> addRasterImage(r, colors = colors_fn, opacity = 1, group = new_group, project = FALSE, options = gridOptions(pane = "inundation"))
      last_group$name <- new_group
      
      # build simple legend values (log-spaced, shown as original values)
      leg_vals <- signif(10^seq(log10(minv2), log10(maxv2), length.out = 5), digits = 3)
      leg_colors <- pal_fn(log10(leg_vals))
      
      # re-add controls (clearControls removes all controls including layers control)
      proxy |> clearControls() |> add_layers_control() |>
        addLegend(
          position = "bottomright",
          colors = rev(pal_discrete),
          labels = rev(legend_labels),
          title = "Flow (cfs)",
          opacity = 1
        )
      
      
    } else {
      
      # Thresholded rendering (original behavior)
      req(input$flow)
      flow_val <- as.numeric(input$flow)  # sliderTextInput returns strings, numeric conversion is fine
      colors_fn <- function(x) ifelse(x <= flow_val, "#0088ff", NA)
      
      new_group <- paste0("Inundation_", as.integer(Sys.time()))
      proxy |> addRasterImage(r, colors = colors_fn, opacity = 1, group = new_group, project = FALSE, options = gridOptions(pane = "inundation"))
      last_group$name <- new_group
      
      # ensure legend removed (clearControls then re-add layers control)
      proxy |> clearControls() |> add_layers_control()
    }
  })  
  # -------------------------
  # SHOW/HIDE VECTOR LAYERS
  # -------------------------
  # observe({
  #   proxy <- leafletProxy("map")
  #   if (!is.null(boundary_sf)) {
  #     if ("Project Boundary" %in% input$show_layers) proxy %>% showGroup("Project Boundary") else proxy %>% hideGroup("Project Boundary")
  #   }
  # })
  # 
  # observe({
  #   proxy <- leafletProxy("map")
  #   if (!is.null(creeks_sf)) {
  #     if ("Creeks" %in% input$show_layers) proxy %>% showGroup("Creeks") else proxy %>% hideGroup("Creeks")
  #   }
  # })
  # 
  # observe({
  #   proxy <- leafletProxy("map")
  #   if (!is.null(gages_sf)) {
  #     if ("Streamgages" %in% input$show_layers) proxy %>% showGroup("Streamgages") else proxy %>% hideGroup("Streamgages")
  #   }
  # })
  
}

# -------- RUN APP --------
shinyApp(ui, server)
