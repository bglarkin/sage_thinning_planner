# MPG Sage Thinning Planner
# Shiny app
# Dev 24 March 2026
# Beau Larkin

# Libraries ———————— ####
library(shiny)
library(bslib)
library(dplyr)
library(readr)
library(leaflet)
library(sf)
library(terra)

# Data ———————— ####
boundary_sf <- st_read("data/mpg_boundary.geojson", quiet = TRUE)
thinpolys_sf <- st_read("data/sage_treatment.geojson", quiet = TRUE)

sagevp_tif <- "data/predictions_xentropy_10m.tif"
sagevp_r <- rast(sagevp_tif)  # SpatRaster

herbs <- read_csv("data/gv_vp_herbaceous.csv", show_col_types = FALSE)

# Layer/group names (single source of truth)
grp_boundary <- "Boundary"
grp_mortality <- "Sage mortality"
grp_thins <- "Existing sage thins"

# UI ———————— ####
ui <- fluidPage(
    theme = bs_theme(version = 5, bootswatch = "flatly"),
    titlePanel("Sage Thinning Planner"),
    sidebarLayout(
        sidebarPanel(
            width = 3,
            tags$h4("Layers"),
            checkboxGroupInput(
                "overlays",
                label = NULL,
                choices = c(
                    "Show sage mortality" = "mortality",
                    "Show existing sage thins" = "thins",
                    "Show MPG boundary" = "boundary"
                ),
                selected = c("mortality", "boundary")
            ),
            
            sliderInput(
                "mortality_opacity",
                "Sage mortality opacity",
                min = 0, max = 100, value = 60, step = 1,
                post = "%",
                ticks = FALSE
            ),
            
            tags$hr(),
            tags$h4("Information"),
            tags$p("Explanatory text")
        ),
        mainPanel(
            width = 9,
            leafletOutput("map", height = "calc(100vh - 120px)")
        )
    )
)

# Server ———————— ####
server <- function(input, output, session) {
    # Palette for raster
    rng <- terra::global(sagevp_r, c("min", "max"), na.rm = TRUE)
    rng <- c(rng[1, 1], rng[1, 2])
    pal <- leaflet::colorNumeric(
        palette = viridisLite::inferno(256),
        domain = rng,
        na.color = "transparent"
    )
    
    output$map <- renderLeaflet({
        leaflet(options = leafletOptions(preferCanvas = TRUE)) %>%
            addProviderTiles("Esri.WorldImagery", group = "Imagery") %>%
            addProviderTiles("CartoDB.Positron", group = "Light") %>%
            
            # Boundary outline
            addPolygons(
                data = boundary_sf,
                fill = FALSE,
                color = "#F7B33C",
                opacity = 0.8,
                weight = 2,
                group = grp_boundary
            ) %>%
            
            # Sage mortality raster overlay (added once; toggled via show/hide, plus legend)
            addRasterImage(
                sagevp_r,
                colors = pal,
                opacity = 0.60,
                project = TRUE,
                group = grp_mortality
            ) %>%
            addLegend(
                position = "bottomright",
                pal = pal,
                values = rng,
                title = "Sage mortality",
                opacity = 1
            ) %>% 
            # Existing thinning polygons (added once; toggled via show/hide)
            addPolygons(
                data = thinpolys_sf,
                weight = 2,
                opacity = 0.9,
                fillOpacity = 0.15,
                group = grp_thins,
                popup = ~paste0(
                    "<b>Objective:</b> ", Objective, "<br/>",
                    "<b>Action:</b> ", Action, "<br/>",
                    "<b>Method:</b> ", Method, "<br/>",
                    "<b>Type:</b> ", Type
                )
            ) %>%
            # Map-side control for base layers only (Imagery vs Light)
            addLayersControl(
                baseGroups = c("Imagery", "Light"),
                # overlayGroups = c(grp_boundary, grp_mortality, grp_thins),
                options = layersControlOptions(collapsed = TRUE)
            ) %>%
            # Control initial state of overlays
            showGroup(grp_mortality) %>%
            hideGroup(grp_thins) %>%
            showGroup(grp_boundary) 
    })
    
    # Sidebar overlay toggles observer
    observeEvent(input$overlays, {
        sel <- input$overlays
        if (is.null(sel)) sel <- character(0)
        
        proxy <- leafletProxy("map")
        
        if ("mortality" %in% sel) {
            proxy %>%
                clearGroup(grp_mortality) %>%
                addRasterImage(
                    sagevp_r,
                    colors = pal,
                    opacity = input$mortality_opacity / 100,
                    project = TRUE,
                    group = grp_mortality
                )
        } else {
            proxy %>% clearGroup(grp_mortality)
        }
        
        if ("thins" %in% sel) proxy %>% showGroup(grp_thins) else proxy %>% hideGroup(grp_thins)
        if ("boundary" %in% sel) proxy %>% showGroup(grp_boundary) else proxy %>% hideGroup(grp_boundary)
    }, ignoreInit = TRUE, ignoreNULL = FALSE)
    
    # Opacity control observer
    observeEvent(input$mortality_opacity, {
        sel <- input$overlays
        if (is.null(sel)) sel <- character(0)
        
        if (!("mortality" %in% sel)) return()
        
        leafletProxy("map") %>%
            clearGroup(grp_mortality) %>%
            addRasterImage(
                sagevp_r,
                colors = pal,
                opacity = input$mortality_opacity / 100,
                project = TRUE,
                group = grp_mortality
            )
    }, ignoreInit = TRUE)
}

# Run application ———————— ####
shinyApp(ui, server)
