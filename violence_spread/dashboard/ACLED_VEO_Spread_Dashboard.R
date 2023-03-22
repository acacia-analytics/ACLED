# ACLED VEO Spread 
library(tidyverse)
library(highcharter) 
library(shiny)
library(shinydashboard)
library(leaflet)

# Load in model results and data 
# 1) Africa-wide VEO
load("R_Data_Files/Africa_Wide_VEO_Step_3.RData")

# UI
ui <- 
  fluidPage(
    tabsetPanel(id = "tabs", selected = "analysis_tab", 
                tabPanel(value = "analysis_tab", title = "Analysis",
                         fluidRow(
                           column(3,
                               selectInput(inputId = "area", label = "Area",
                                           choices = c("Mali", "Burkina Faso",
                                                       "Egypt", "Somalia", "Algeria",
                                                       "Mozambique", "Libya"))
                           ),
                           column(9,
                               # Smaller tabs to switch between views of covariates
                               tabBox(
                                 type = "tabs", width = "100%", id = "MapOptions",
                                 tabPanel(
                                   "Forecasts"
                                 ), 
                                 tabPanel(
                                   "Poverty", 
                                   leafletOutput("poverty_map")
                                 ), 
                                 tabPanel(
                                   "Population", 
                                   leafletOutput("population_map")                                   
                                 ), 
                                 tabPanel(
                                   "Youth Population", 
                                   leafletOutput("youth_population_map")
                                 )
                               )
                           )
                         ), 
                         # Fanchart visual
                         fluidRow(
                           column(
                             12,
                             highchartOutput("Fanchart")
                           )
                           )
                        ),
                tabPanel(value = "explanation_tab", title = "Explanation", 
                          fluidRow(
                            column(12, 
                                   includeHTML("ACLED_Model_Documentation.html")
                                   
                                   )
                          )
                        )
                    )                                         
                )




server <- function(input, output, session){

  output$population_map <- renderLeaflet({
    leaflet(data = africa) %>%
      addTiles() %>% 
      addPolygons() 
  })
}


shinyApp(ui = ui, server = server)


# Example of forecasting data 
# data("weather", package = "highcharter")
# 
# weather <- weather %>% mutate(
#   supermin_temp = min_temperaturec - 10,
#   supermax_temp = max_temperaturec + 10
#   
# )
# weather <- weather %>% 
#   mutate(
#     min_temperaturec = ifelse(date < as.Date('2014-05-04'), NA, min_temperaturec) , 
#     max_temperaturec = ifelse(date < as.Date('2014-05-04'), NA, max_temperaturec) , 
#     supermin_temp = ifelse(date < as.Date('2014-05-04'), NA, supermin_temp) , 
#     supermax_temp = ifelse(date < as.Date('2014-05-04'), NA, supermax_temp) 
#     
#   )
# 
# make_fan_chart <- function(fanchart_data) {
# return(hchart(fanchart_data,
#        type = "line",
#        hcaes(x = Date, y = Actual),
#        name = "Temperature",
#        zIndex = 1,
#        marker = list(fillColor = "white",
#                      lineWidth = 2,
#                      lineColor = "#7cb5ec")) %>% 
#   hc_add_series(type = "line",
#                 data = fanchart_data,
#                 hcaes(x = Date, y = Simulation_Mean),
#                 name = "Mean",
#                 zIndex = 1,
#                 marker = list(fillColor = "white",
#                               lineWidth = 2,
#                               lineColor = "#7cb5ec")) %>%     
#   hc_add_series(type = "line",
#                 data = fanchart_data,
#                 hcaes(x = Date, y = Simulation_P50),
#                 name = "Median",
#                 zIndex = 1,
#                 marker = list(fillColor = "white",
#                               lineWidth = 1,
#                               lineColor = "#7cb5ec")) %>%   
#   hc_add_series(type = "arearange",
#                 data = fanchart_data,
#                 hcaes(x = Date, low = Simulation_P10, high = Simulation_P90),
#                 name = "Range",
#                 lineWidth = 0,
#                 linkedTo = ":previous",
#                 color = "#FF6347",
#                 fillOpacity = 0.3,
#                 zIndex = 0,
#                 marker = list(enabled = FALSE)) %>%   
#   hc_add_series(type = "arearange",
#                 data = fanchart_data,
#                 hcaes(x = Date, low = Simulation_P30, high = Simulation_P70),
#                 name = "Range",
#                 lineWidth = 0,
#                 linkedTo = ":previous",
#                 color = "#7cb5ec",
#                 fillOpacity = 0.3,
#                 zIndex = 0,
#                 marker = list(enabled = FALSE)) %>% 
#   hc_title(text = "Violence Forecast",
#            align = "left") %>% 
#   hc_yAxis(title = list(text = FALSE)) %>% 
#   hc_tooltip(crosshairs = TRUE,
#              shared = TRUE,
#              valueSuffix = "°C")
# )
# }
