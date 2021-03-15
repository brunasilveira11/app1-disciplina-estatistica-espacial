library(shiny)
library(leaflet)

library(tidyverse)
library(lubridate)
library(RSocrata)

years_ago <- today() - years(2)
crash_url <- glue::glue("https://data.cityofchicago.org/Transportation/Traffic-Crashes-Crashes/85ca-t3if?$where=CRASH_DATE > '{years_ago}'")
crash_raw <- as_tibble(read.socrata(crash_url))

crash <- crash_raw %>%
    arrange(desc(crash_date)) %>%
    transmute(
        injuries = if_else(injuries_total > 0, "injuries", "none"),
        crash_date,
        crash_hour,
        report_type = if_else(report_type == "", "UNKNOWN", report_type),
        num_units,
        posted_speed_limit,
        weather_condition,
        lighting_condition,
        roadway_surface_cond,
        first_crash_type,
        trafficway_type,
        prim_contributory_cause,
        latitude, longitude
    ) %>%
    na.omit()



# ui object
ui <- fluidPage(
    titlePanel(p("Predicting injuries for Chicago traffic crashes", style = "color:#3474A7")),
    p("Bruna Silveira e Gabriela Rech"),
    sidebarLayout(
        sidebarPanel(
            selectInput(
                inputId = "select_injure",
                label = "Select",
                choices = c("injuries", "none")
            ),
        ),
        mainPanel(
            leafletOutput(outputId = "map", height = 800)
        )
    )
)

# server object
server <- function(input, output, session) {
    
    # make palette
    pal <- colorFactor(c("red", "navy"), domain = c("injuries", "none"))
    
    output$map <- renderLeaflet({
        
        crash2 = crash[which(crash$injuries==input$select_injure),]
        
        leaflet(crash2) %>%
            addProviderTiles(providers$OpenStreetMap.HOT) %>%
            addMarkers(lng = ~longitude,
                       lat = ~latitude,
            clusterOptions = markerClusterOptions()
                             
            
            )
    })
}
shinyApp(ui, server)

