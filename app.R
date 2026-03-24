# MPG Plant Finder
# Shiny app
# Dev 6 February 2026
# Beau Larkin

# Libraries ———————— ####
library(shiny)
library(bslib)
library(leaflet)
library(dplyr)



# Data ———————— ####

# UI ———————— ####
ui <- fluidPage(
    theme = bs_theme(version = 5, bootswatch = "flatly"),
    titlePanel("New app"),
    sidebarLayout(
        sidebarPanel(
            helpText("Controls go here.")
        ),
        mainPanel(
            h4("Hello."),
            verbatimTextOutput("info")
        )
    )
)

# Server ———————— ####
server <- function(input, output, session) {
    output$info <- renderPrint({
        list(
            time = Sys.time(),
            user = Sys.info()[["user"]],
            wd = getwd()
        )
    })
}

# Run application ———————— ####
shinyApp(ui, server)