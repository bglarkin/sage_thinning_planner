# MPG Sage Thinning Planner
# Shiny app
# Dev 24 March 2026
# Beau Larkin

# Libraries ———————— ####
library(shiny)
library(bslib)
library(dplyr)
library(readr)
library(tidyr)
library(markdown)
library(colorspace)
library(leaflet)
library(leaflet.extras2)
library(sf)
library(terra)

# Data ———————— ####
boundary_sf <- st_read("data/mpg_boundary.geojson", quiet = TRUE)
thinpolys_sf <- st_read("data/sage_treatment.geojson", quiet = TRUE)
aoi_raw <- st_read("data/aoi_alphashape_2024.geojson", quiet = TRUE) |> st_buffer(30)
aoi_sf  <- st_intersection(aoi_raw, st_transform(boundary_sf, st_crs(aoi_raw))) |> st_transform(4326)
sagevp_tif <- "data/predictions_xentropy_10m.tif"
sagevp_r <- rast(sagevp_tif)  # SpatRaster
herbs <- read_csv("data/gv_vp_herbaceous.csv", show_col_types = FALSE)
pie_radius_init <- 10

# Layer/group names (single source of truth)
grp_boundary <- "Boundary"
grp_mortality <- "Sage mortality"
grp_thins <- "Existing sage thins"
grp_herb_pies <- "Herbaceous composition"
grp_aoi <- "2024 Drone Survey"

# UI ———————— ####
ui <- tagList(
    tags$head(
        tags$style(HTML("
            .leaflet-div-icon.herb-pie-icon {
                background: transparent;
                border: none;
            }

            .section-gap {
                margin-top: 1rem;
                margin-bottom: 1rem;
            }

            .subtle-divider {
                border-top: 1px solid rgba(0, 0, 0, 0.08);
                margin-top: 1rem;
                margin-bottom: 1rem;
            }

            .helper-text {
                font-size: 0.9rem;
                color: #6c757d;
                margin-top: 0.35rem;
                margin-bottom: 0;
            }

            .control-stack > * {
                margin-bottom: 0.75rem;
            }

            .sidebar-note p:last-child {
                margin-bottom: 0;
            }
        "))
    ),
    
    navbarPage(
        "Sage Thinning Planner",
        theme = bs_theme(version = 5, bootswatch = "flatly"),
        id = "main_nav",
        
        tabPanel(
            "Map and data",
            sidebarLayout(
                sidebarPanel(
                    width = 3,
                    
                    tags$h4("Description"),
                    div(
                        class = "sidebar-note",
                        tags$p("This map links sage mortality with herbaceous community composition."),
                        tags$p("Following sage loss, dominant herbaceous groups are expected to expand."),
                        tags$p("Click pie charts to view detailed cover, composition, and data source. Pie charts show composition as a percent of total herbaceous cover, highlighting which groups dominate each site.")
                    ),
                    
                    tags$div(class = "subtle-divider"),
                    
                    tags$h4("Layers"),
                    div(
                        class = "control-stack",
                        checkboxGroupInput(
                            "overlays",
                            label = NULL,
                            choices = c(
                                "Show sage mortality" = "mortality",
                                "Show herbaceous composition" = "herbaceous",
                                "Show existing sage thins" = "thins",
                                "Show 2024 drone survey" = "aoi",
                                "Show MPG boundary" = "boundary"
                            ),
                            selected = c("mortality", "herbaceous", "aoi", "boundary")
                        )
                    ),
                    
                    tags$div(class = "subtle-divider"),
                    
                    tags$h4("Display"),
                    div(
                        class = "control-stack",
                        sliderInput(
                            "mortality_opacity",
                            "Sage mortality opacity",
                            min = 0, max = 100, value = 100, step = 1,
                            post = "%",
                            ticks = FALSE
                        ),
                        
                        sliderInput(
                            "pie_radius",
                            "Pie chart radius",
                            min = 0, max = 24, value = pie_radius_init, step = 1,
                            post = "px",
                            ticks = FALSE
                        )
                    ),
                    
                    tags$div(class = "subtle-divider"),
                    
                    tags$h4("Notes"),
                    div(
                        class = "sidebar-note",
                        tags$p("The 2024 Drone Survey polygon defines the bounds of the sagebrush mapping effort."),
                        tags$p("Herbaceous data come from both long-term grid surveys and a 2025 vole-impact survey."),
                        tags$p("Sage pixels indicate areas with \u22650.25 m\u00B2 of sage per 10 \u00D7 10 m cell."),
                        tags$p("Plant group codes indicate origin (E = exotic, N = native), life span (A = annual, P = perennial), and growth form (F = forb, G = grass).")
                    )
                ),
                
                mainPanel(
                    width = 9,
                    leafletOutput("map", height = "calc(100vh - 120px)")
                )
            )
        ),
        
        tabPanel(
            "Readme",
            includeMarkdown("README.md")
        )
    )
)

# Functions ———————— ####
make_pie_svg <- function(values, colors, radius = pie_radius_init, stroke = "#222222", stroke_width = 1) {
    values <- as.numeric(values)
    
    if (all(is.na(values)) || sum(values, na.rm = TRUE) <= 0) {
        return(
            paste0(
                "<svg xmlns='http://www.w3.org/2000/svg' width='", radius * 2 + 2,
                "' height='", radius * 2 + 2, "' viewBox='0 0 ", radius * 2 + 2, " ", radius * 2 + 2, "'>",
                "<circle cx='", radius + 1, "' cy='", radius + 1, "' r='", radius,
                "' fill='white' stroke='", stroke, "' stroke-width='", stroke_width, "'/>",
                "</svg>"
            )
        )
    }
    
    values[is.na(values)] <- 0
    props <- values / sum(values)
    cumprops <- c(0, cumsum(props))
    
    cx <- radius + 1
    cy <- radius + 1
    
    arc_path <- function(start_p, end_p, fill) {
        theta1 <- 2 * pi * start_p - pi / 2
        theta2 <- 2 * pi * end_p   - pi / 2
        
        x1 <- cx + radius * cos(theta1)
        y1 <- cy + radius * sin(theta1)
        x2 <- cx + radius * cos(theta2)
        y2 <- cy + radius * sin(theta2)
        
        large_arc <- ifelse(end_p - start_p > 0.5, 1, 0)
        
        paste0(
            "<path d='M ", cx, " ", cy,
            " L ", x1, " ", y1,
            " A ", radius, " ", radius, " 0 ", large_arc, " 1 ", x2, " ", y2,
            " Z' fill='", fill, "'/>"
        )
    }
    
    slices <- vapply(
        seq_along(values),
        function(i) arc_path(cumprops[i], cumprops[i + 1], colors[i]),
        character(1)
    )
    
    paste0(
        "<svg xmlns='http://www.w3.org/2000/svg' width='", radius * 2 + 2,
        "' height='", radius * 2 + 2, "' viewBox='0 0 ", radius * 2 + 2, " ", radius * 2 + 2, "'>",
        paste(slices, collapse = ""),
        "<circle cx='", cx, "' cy='", cy, "' r='", radius,
        "' fill='none' stroke='", stroke, "' stroke-width='", stroke_width, "'/>",
        "</svg>"
    )
}

make_pie_legend_html <- function(cols, pal) {
    
    items <- paste0(
        "<div style='display:flex; align-items:center; margin-bottom:3px;'>",
        "<span style='display:inline-block; width:12px; height:12px; margin-right:6px; border:1px solid #444; background:",
        unname(pal[cols]),
        ";'></span>",
        "<span>", cols, "</span>",
        "</div>",
        collapse = ""
    )
    
    paste0(
        "<div style='background: rgba(255,255,255,0.25); padding: 5px 5px; border-radius: 6px; font-size: 14px; line-height: 1.1;'>",
        "<div style='font-weight: 600; margin-bottom: 4px;'>Plant groups</div>",
        items,
        "</div>"
    )
}

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
    
    # Herbaceous data for pies
    herbs_wide <- herbs %>%
        select(dataset, trans_num, cvr_code, pct_comp, lon, lat) %>%
        arrange(cvr_code) %>%
        pivot_wider(
            names_from = cvr_code,
            values_from = pct_comp,
            values_fill = 0
        )
    pfg_cols <- sort(unique(herbs$cvr_code))
    pie_mat <- herbs_wide %>%
        select(all_of(pfg_cols)) %>%
        as.matrix()
    storage.mode(pie_mat) <- "numeric"
    
    # Palette for pies and html legend
    exotic_pal <- rev(sequential_hcl(palette = "OrYel", n = 4))
    native_pal <- rev(sequential_hcl(palette = "TealGrn", n = 4))
    pfg_pal <- setNames(c(exotic_pal, native_pal), pfg_cols)
    pie_legend_html <- make_pie_legend_html(pfg_cols, pfg_pal)
    
    # HTML pies
    herbs_wide$pie_html <- apply(
        herbs_wide[, pfg_cols],
        1,
        function(x) make_pie_svg(x, unname(pfg_pal[pfg_cols]), radius = pie_radius_init)
    )
    
    # Popups for point-level data
    herb_popups <- herbs %>%
        arrange(trans_num, cvr_code) %>%
        mutate(
            row_html = paste0(
                "<tr>",
                "<td style='padding:2px 6px; border-bottom:1px solid #ddd;'>", cvr_code, "</td>",
                "<td style='padding:2px 6px; text-align:right; border-bottom:1px solid #ddd;'>", sprintf("%.1f", pct_cvr), "</td>",
                "<td style='padding:2px 6px; text-align:right; border-bottom:1px solid #ddd;'>", sprintf("%.1f", pct_comp), "</td>",
                "</tr>"
            )
        ) %>%
        group_by(dataset, trans_num, lon, lat) %>%
        summarise(
            rows_html = paste(row_html, collapse = ""),
            .groups = "drop"
        ) %>%
        mutate(
            dataset_label = case_when(dataset == "gv_data" ~ "Grid Veg Data", dataset == "vp_data" ~ "Volepocalypse Data"),
            popup_html = paste0(
                "<div style='min-width:220px;'>",
                "<div style='font-weight:600; margin-bottom:6px;'>", dataset_label, ", Transect ", trans_num,"</div>",
                "<table style='border-collapse:collapse; width:100%; font-size:12px;'>",
                "<thead>",
                "<tr>",
                "<th style='text-align:left; padding:2px 6px; border-bottom:2px solid #999;'>Plant group</th>",
                "<th style='text-align:right; padding:2px 6px; border-bottom:2px solid #999;'>Cover (%)</th>",
                "<th style='text-align:right; padding:2px 6px; border-bottom:2px solid #999;'>Composition (%)</th>",
                "</tr>",
                "</thead>",
                "<tbody>",
                rows_html,
                "</tbody>",
                "</table>",
                "</div>"
            )
        )
    herbs_wide <- herbs_wide %>%
        left_join(
            herb_popups,
            by = c("dataset", "trans_num", "lon", "lat")
        )
    
    # Bounding box based on pies
    bbox <- st_bbox(c(xmin = min(herbs_wide$lon), xmax = max(herbs_wide$lon), ymin = min(herbs_wide$lat), ymax = max(herbs_wide$lat)), crs = 4326)
    xmin <- as.numeric(bbox["xmin"])
    ymin <- as.numeric(bbox["ymin"])
    xmax <- as.numeric(bbox["xmax"])
    ymax <- as.numeric(bbox["ymax"])
    
    # Map
    output$map <- renderLeaflet({
        leaflet(options = leafletOptions(preferCanvas = TRUE)) %>%
            addProviderTiles("Esri.WorldImagery") %>%
            fitBounds(xmin, ymin, xmax, ymax) %>%
            # Boundary outline
            addPolygons(
                data = boundary_sf,
                fill = FALSE,
                color = "#F7B33C",
                opacity = 0.8,
                weight = 2,
                group = grp_boundary
            ) %>%
            # 2024 Drone Survey AOI
            addPolygons(
                data = aoi_sf,
                fill = FALSE,
                color = "#00FFFF",
                opacity = 0.9,
                weight = 2,
                group = grp_aoi
            ) %>%
            # # Sage mortality raster overlay (added once; toggled via show/hide, plus legend)
            addRasterImage(
                sagevp_r,
                colors = pal,
                opacity = 1,
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
            # Herbaceous pies
            leaflet.extras2::addDivicon(
                data = herbs_wide,
                lng = ~lon,
                lat = ~lat,
                html = ~pie_html,
                popup = ~popup_html,
                className = "herb-pie-icon",
                group = grp_herb_pies
            ) %>%
            addControl(
                html = pie_legend_html,
                position = "bottomright"
            ) %>%
            addControl(
                html = paste0(
                    "<div style='background: rgba(255,255,255,0.25); padding: 5px 5px; border-radius: 6px; font-size: 14px; line-height: 1.1;'>",
                    "<div style='display:flex; align-items:center;'>",
                    "<span style='display:inline-block; width:18px; height:0; border-top:2px solid #00FFFF; margin-right:6px;'></span>",
                    "<span>2024 Drone Survey</span>",
                    "</div></div>"
                ),
                position = "bottomright"
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
            # Control initial state of overlays
            showGroup(grp_mortality) %>%
            hideGroup(grp_thins) %>%
            showGroup(grp_herb_pies) %>%
            showGroup(grp_aoi) %>%
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
        
        if ("herbaceous" %in% sel) {
            proxy %>% showGroup(grp_herb_pies)
        } else {
            proxy %>% hideGroup(grp_herb_pies)
        }
        
        if ("thins" %in% sel) {
            proxy %>% showGroup(grp_thins)
        }  else {
            proxy %>% hideGroup(grp_thins)
        }
        
        if ("aoi" %in% sel) {
            proxy %>% showGroup(grp_aoi)
        } else {
            proxy %>% hideGroup(grp_aoi)
        }

        if ("boundary" %in% sel) {
            proxy %>% showGroup(grp_boundary)
        } else {
            proxy %>% hideGroup(grp_boundary)
        }
        
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
    
    # Radius control observer
    observeEvent(input$pie_radius, {
        sel <- input$overlays
        if (is.null(sel)) sel <- character(0)
        
        if (!("herbaceous" %in% sel)) return()
        
        herbs_wide$pie_html <- apply(
            herbs_wide[, pfg_cols],
            1,
            function(x) make_pie_svg(x, unname(pfg_pal[pfg_cols]), radius = input$pie_radius)
        )
        
        leafletProxy("map") %>%
            clearGroup(grp_herb_pies) %>%
            leaflet.extras2::addDivicon(
                data = herbs_wide,
                lng = ~lon,
                lat = ~lat,
                html = ~pie_html,
                popup = ~popup_html,
                className = "herb-pie-icon",
                group = grp_herb_pies
            )
        
    }, ignoreInit = TRUE)
}

# Run application ———————— ####
shinyApp(ui, server)
