# app.R
library(shiny)
library(leaflet)
library(terra)
library(sf)
library(htmlwidgets)
library(glue)
library(dplyr)
library(shinycssloaders)

# -------- CONFIG --------
COG_URL_TEMPLATE <- "https://raw.githubusercontent.com/FlowWest/kuhlanapo-inundation-map/main/data/cog/min_flow_inundation_{alt}_lake_{lake}.tif"
local_cache_dir <- "data_cache"
dir.create(local_cache_dir, recursive = TRUE, showWarnings = FALSE)

# static geojsons in repo (EPSG:4326)
boundary_geo  <- "data/project_boundary.geojson"
creeks_geo    <- "data/stream_lines.geojson"
gages_geo     <- "data/stream_gages.geojson"

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

# -------- UI --------
ui <- fluidPage(
  tags$link(rel = "stylesheet", href = "leaflet.css"),
  sidebarLayout(
    sidebarPanel(
      width = 3,  # narrow sidebar
      radioButtons("alternative", "Alternative", choices = alternatives, selected = alternatives[1]),
      radioButtons("lake_level", "Lake Level", choices = lake_levels, selected = lake_levels[1]),
      radioButtons("flow", "Flow (cfs)", choices = flows, selected = flows[1])
    ),
    mainPanel(
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
  
  # reactiveValues to track last raster group
  last_group <- reactiveValues(name = NULL)
  
  # reactiveVal to hold preprocessed raster for current alt/lake
  prepped_raster <- reactiveVal(NULL)
  
  # -------------------------
  # INITIALIZE MAP
  # -------------------------
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
      )|>
    addLayersControl(baseGroups=c("Imagery Only", "Imagery + Topo", "Topo Only"),
                       overlayGroups=c("Esri Topographic Labels", "Project Boundary","Creeks","Streamgages"),
                       options=layersControlOptions(collapsed=FALSE))
    
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
  observeEvent(input$flow, {
    req(prepped_raster())
    
    flow_val <- as.numeric(input$flow)
    colors_fn <- function(x) ifelse(x <= flow_val, "#0088ff", NA)
    
    proxy <- leafletProxy("map")
    
    # remove old raster
    if (!is.null(last_group$name)) proxy |> clearGroup(last_group$name)
    
    # add new raster with current flow
    new_group <- paste0("Inundation_", as.integer(Sys.time()))
    proxy |> addRasterImage(prepped_raster(), colors = colors_fn, opacity = 1, group = new_group, project = FALSE,
                            options = gridOptions(pane = "inundation"))
    
    last_group$name <- new_group
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
