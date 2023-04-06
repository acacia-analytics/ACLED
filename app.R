#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinyWidgets)
library(shinydashboard)
library(DBI)
library(dbplyr)
library(dplyr)
library(tidyr)
library(geojsonsf)
library(sf)
library(leaflet)
library(leaflet.extras)
library(highcharter)
library(lubridate)
library(xts)
library(rgdal)

neighbors <- read.csv("../geojson/neighbors.csv")
country <- geojsonsf::geojson_sf("../geojson/country.geojson")
b_25 <- geojsonsf::geojson_sf("../geojson/buffer_25.geojson")
b_50 <- geojsonsf::geojson_sf("../geojson/buffer_50.geojson")
b_75 <- geojsonsf::geojson_sf("../geojson/buffer_75.geojson")
b_100 <- geojsonsf::geojson_sf("../geojson/buffer_100.geojson")
centroid <- geojsonsf::geojson_sf("../geojson/centroid.geojson")

con <- DBI::dbConnect(
  RPostgres::Postgres(),
  dbname = 'acled',
  host = 'localhost',
  port = 5432,
  user = 'postgres',
  password = rstudioapi::askForPassword("Database password")
)

##### Data for the VEO Centroid
# Set query for all of the countries that have terrorist activities
veo_query <- "SELECT DISTINCT country, iso3 FROM acled.acled_master WHERE continent = 'Africa' AND (veo1 IS NOT NULL OR veo2 IS NOT NULL);"

# Extract country names and iso codes from database
veo <- DBI::dbGetQuery(con, veo_query)

veo <- veo[order(veo$country), ]

# Set the values for the dropdown as a named list.  The country will appear in the dropdown but the iso code wil be used as the filter
veoDropdown <- setNames(as.list(veo$iso3), as.list(veo$country))

# Remove the veo data and the query
rm(veo, veo_query)


##### Data for the contagion zone analysis
# Set query for all African Countries in the dataset
contagion_query <- "SELECT DISTINCT country, iso3 FROM acled.acled_master WHERE continent = 'Africa';"

# Extract country names and iso codes from database
contagion <- DBI::dbGetQuery(con, contagion_query)

contagion <- contagion[order(contagion$country), ]

# Set the values for the dropdown as a named list.  The country will appear in the dropdown but the iso code wil be used as the filter
contagionDropdown <- setNames(as.list(contagion$iso3), as.list(contagion$country))

# Remove the contagion data and the query
rm(contagion, contagion_query)

##### Data for the violence analysis
# Set query for all African Countries in the dataset
country_query <- "SELECT DISTINCT country, iso3 FROM acled.acled_master WHERE continent = 'Africa';"

# Extract country names and iso codes from database
countries <- DBI::dbGetQuery(con, country_query)

countries <- countries[order(countries$country), ]

# Set the values for the dropdown as a named list.  The country will appear in the dropdown but the iso code wil be used as the filter
violenceDropdown <- setNames(as.list(countries$iso3), as.list(countries$country))

# Remove the veo data and the query
rm(countries, country_query)

# Time frame
period1 <- c('Daily', 'Weekly', 'Monthly', 'Quarterly', 'Yearly')
period2 <- c('day', 'week', 'month', 'quarter', 'year')
periodDropdown <- setNames(period2, period1)

# Remove the period data
rm(period1, period2)

##### Demonstration Data Preparation
# Set query for all African Countries in the dataset
dem_country_query <- "SELECT DISTINCT country, iso3 FROM acled.acled_master WHERE continent = 'Africa';"

# Extract country names and iso codes from database
dem_countries <- DBI::dbGetQuery(con, dem_country_query)

dem_countries <- dem_countries[order(dem_countries$country), ]

# Set the values for the dropdown as a named list.  The country will appear in the dropdown but the iso code wil be used as the filter
# This isn't used in this workbook but will be used in the app
demDropdown <- setNames(as.list(dem_countries$iso3), as.list(dem_countries$country))

# Remove the veo data and the query
rm(dem_countries, dem_country_query)

# Time frame
demperiod1 <- c('Daily', 'Weekly', 'Monthly', 'Quarterly', 'Yearly')
demperiod2 <- c('day', 'week', 'month', 'quarter', 'year')
demPeriodDropdown <- setNames(demperiod2, demperiod1)

# Remove the period data
rm(demperiod1, demperiod2)

#####
region1 <- c('African Union', 'Africa Command', 'United Nations')
region2 <- c('au_region', 'ac_region', 'un_region')
regionGroupDropdown <- setNames(region2, region1)

# Time frame
period1 <- c('Daily', 'Weekly', 'Monthly', 'Quarterly', 'Yearly')
period2 <- c('day', 'week', 'month', 'quarter', 'year')
periodDropdown <- setNames(period2, period1)

# Remove the period data
rm(period1, period2)

# Set query for all African Countries in the dataset
start_region_query <- "SELECT DISTINCT au_region FROM iso.region ORDER BY au_region;"

# Extract country names and iso codes from database
start_region <- DBI::dbGetQuery(con, start_region_query)

# Set the values for the dropdown as a named list.  The country will appear in the dropdown but the iso code wil be used as the filter
# This isn't used in this workbook but will be used in the app
startRegionDropdown <- as.list(start_region)
# Remove the veo data and the query
rm(start_region, start_region_query)

##### This is the user interface for the dashboard
# Define UI for application
ui <- bootstrapPage(
  
  tags$head(
    tags$style(HTML("
      .bar-label {width: 200px; white-space: normal; text-align: left;}
    "))
  ),
  
  navbarPage("ACLED",
  
    # Application title
    windowTitle = "ACLED",
    
    tabPanel("VEO Centriod",
        # div(class="outer",
        #     tags$head(includeCSS("styles.css")),
        # 
        #     leafletOutput("map", width="100%", height="95vh"),
        # 
        #     # Sidebar with a slider input for number of bins 
        #     absolutePanel(id = "controls", class = "panel panel-default",
        #                   top = 75, right = 55, width = 250, fixed=TRUE,
        #                   draggable = TRUE, height = "auto",
        #                   tags$br(),
        #                   tags$h3("Select Country"),
        #                   selectInput("veoSelector", "", veoDropdown),
        #                   uiOutput("sliderUI")
        #     )
        # )
        sidebarLayout(
          sidebarPanel(
            
            selectInput("veoSelector", "Select Country:",
                        choices = veoDropdown,
                        multiple = FALSE),
            
            uiOutput("sliderUI")
          ),
          
          mainPanel(
            leafletOutput("map", width="100%", height="45vh"),
            tags$br(),
            leafletOutput("cumulativeVeoMap", width="100%", height="45vh"),
          )
        )
    ),
    
    tabPanel("Contagion Zones",
             
             sidebarLayout(
               sidebarPanel(
                 
                 selectInput("contagionSelector", "Country:",
                             contagionDropdown,
                             # selected = c("GHA"),
                             multiple = FALSE),
                 
                 highchartOutput("contagionGraph")
               ),
               
               mainPanel(
                 leafletOutput("contagionMap", width="100%", height="95vh")          
               )
             )
             
    ),
    
    navbarMenu("Violence",
               
        tabPanel("Violence by Country",
                 
                 sidebarLayout(
                   sidebarPanel(
                     
                     selectInput("violenceSelector", "Country:",
                                 choices = violenceDropdown,
                                 # selected = c("GHA"),
                                 multiple = FALSE),
                     
                     uiOutput("dateTimeUI"),
                     
                     selectInput("periodSelector", "Time Period:",
                                 choices = periodDropdown,
                                 selected = 'year',
                                 multiple = FALSE),
                     
                     highchartOutput("violenceCountGraph"),
                     highchartOutput("violenceFatalGraph")
                   ),
                   
                   mainPanel(
                     leafletOutput("violenceMap", width="100%", height="95vh")
                   )
                 )
                 
        ),
        
        tabPanel("Violence by Region",
                 
                 tags$head(
                   tags$style("
                            .input-daterange input {
                                min-height: 34px;
                            }
                          ")
                 ),
                 
                 fixedRow(
                   column(12, wellPanel(
                     fixedRow(
                       column(6,
                              selectInput("vioRegionGroupSelector", "Region Group:",
                                          choices = regionGroupDropdown,
                                          multiple = FALSE)
                       ),
                       column(6,
                              selectInput("vioRegionSelector", "Region:",
                                          choices = startRegionDropdown,
                                          multiple = FALSE)
                       )
                     ),
                     fixedRow(
                       column(6,
                              selectInput("vioRegionPeriodSelector", "Time Period:",
                                          choices = periodDropdown,
                                          selected = 'year',
                                          multiple = FALSE)
                       ),
                       column(6,
                              uiOutput("vioRegionDateTimeUI")
                       )
                     )
                   ))
                 ),
                 
                 fixedRow(style = "background-color:#FFFFFF; padding-top: 5%; padding-bottom: 5%",
                          column(12,
                                 fixedRow(
                                   column(12, tags$h1('Overall Violence', style='padding-bottom: 2%;'))
                                 )
                          ),
                          column(12,
                                 fixedRow(
                                   column(6,
                                          highchartOutput("vioRegionTimeCount", height=500),
                                          highchartOutput("vioRegionTypeCount", height=400)
                                   ),
                                   column(6, 
                                          highchartOutput("vioRegionMap", height=900)
                                   )
                                 )
                          )
                 ),
                 
                 fixedRow(style = "background-color:#f9f9f9; padding-top: 5%; padding-bottom: 5%",
                          column(12,
                                 fixedRow(
                                   column(12, tags$h1('VEO Violence', style='padding-bottom: 2%;'))
                                 )
                          ),
                          column(12,
                                 fixedRow(
                                   column(6, 
                                          highchartOutput("vioRegionVeoMap", height=900)
                                   ),
                                   column(6,
                                          highchartOutput("vioRegionVeoTimeCount", height=500),
                                          highchartOutput("vioRegionVeoTypeCount", height=400)
                                   )
                                 )
                          )
                 )
        )
    ),
    navbarMenu("Demonstrations",
    
        tabPanel("Demonstrations by Country",
                 
                 sidebarLayout(
                   sidebarPanel(
                     
                     selectInput("demSelector", "Country:",
                                 choices = demDropdown,
                                 multiple = FALSE),
                     
                     uiOutput("demDateTimeUI"),
                     
                     selectInput("demPeriodSelector", "Time Period:",
                                 choices = demPeriodDropdown,
                                 selected = 'year',
                                 multiple = FALSE),
                     
                     highchartOutput("protestCountGraph"),
                     highchartOutput("riotCountGraph")
                   ),
                   
                   mainPanel(
                     leafletOutput("demonstrationMap", width="100%", height="95vh")
                   )
                 )
        ),
        tabPanel("Demonstrations by Region",
                 
                 tags$head(
                   tags$style("
                            .input-daterange input {
                                min-height: 34px;
                            }
                          ")
                 ),
                 
                 fixedRow(
                   column(12, wellPanel(
                     fixedRow(
                       column(6,
                              selectInput("demRegionGroupSelector", "Region Group:",
                                          choices = regionGroupDropdown,
                                          multiple = FALSE)
                       ),
                       column(6,
                              selectInput("demRegionSelector", "Region:",
                                          choices = startRegionDropdown,
                                          multiple = FALSE)
                       )
                     ),
                     fixedRow(
                       column(6,
                              selectInput("demRegionPeriodSelector", "Time Period:",
                                          choices = periodDropdown,
                                          selected = 'year',
                                          multiple = FALSE)
                       ),
                       column(6,
                              uiOutput("demRegionDateTimeUI")
                       )
                     )
                   ))
                 ),
                 
                 fixedRow(style = "background-color:#FFFFFF; padding-top: 5%; padding-bottom: 5%",
                          column(12,
                                 fixedRow(
                                   column(12, tags$h1('Demonstrations', style='padding-bottom: 2%;'))
                                 )
                          ),
                          column(12,
                                 fixedRow(
                                   column(6,
                                          highchartOutput("demRegionTimeCount", height=500),
                                          highchartOutput("demRegionTypeCount", height=400)
                                   ),
                                   column(6, 
                                          highchartOutput("demRegionMap", height=900)
                                   )
                                 )
                          )
                 ),
                 
                 fixedRow(style = "background-color:#f9f9f9; padding-top: 5%; padding-bottom: 5%",
                          column(12,
                                 fixedRow(
                                   column(12, tags$h1('Protests', style='padding-bottom: 2%;'))
                                 )
                          ),
                          column(12,
                                 fixedRow(
                                   column(6, 
                                          highchartOutput("protestRegionTypeCount", height=450)
                                   ),
                                   column(6,
                                          highchartOutput("protestRegionFatalPercent", height=450)
                                   )
                                 ),
                                 tags$br(),
                                 fixedRow(
                                   column(6,
                                          highchartOutput("protestRegionTimeCount", height=450),
                                   ),
                                   column(6,
                                          highchartOutput("protestRegionFatalCount", height=450)
                                   )
                                 )
                          )
                 ),
                 
                 fixedRow(style = "background-color:#FFFFFF; padding-top: 5%; padding-bottom: 5%",
                          column(12,
                                 fixedRow(
                                   column(12, tags$h1('Riots', style='padding-bottom: 2%;'))
                                 )
                          ),
                          column(12,
                                 fixedRow(
                                   column(6, 
                                          highchartOutput("riotRegionTypeCount", height=450)
                                   ),
                                   column(6,
                                          highchartOutput("riotRegionFatalPercent", height=450)
                                   )
                                 ),
                                 tags$br(),
                                 fixedRow(
                                   column(6,
                                          highchartOutput("riotRegionTimeCount", height=450)
                                   ),
                                   column(6,
                                          highchartOutput("riotRegionFatalCount", height=450)
                                   )
                                 )
                          )
                 )
        )
    )
  )
)

##### This is the server logic that ties all of the data to the user interface
# Define server logic
server <- function(input, output, session) {
    
##### VEO Data  
    veoDf <- reactive({
      req(input$veoSelector)
      query <- sqlInterpolate(ANSI(),
                              "SELECT *
                              FROM acled.acled_master
                              WHERE iso3 = ?selector AND event = 'Violent Event'
                              AND (veo1 IS NOT NULL OR veo2 IS NOT NULL);",
                              selector = input$veoSelector
                              )
      output <- as.data.frame(DBI::dbGetQuery(con, query))
      output <- output %>%
        group_by(year, week) %>%
        summarize(lat = mean(latitude), lon = mean(longitude), count = n(), fatalities = sum(fatalities))
      
      output$cum_x = cummean(output$lon)
      output$cum_y = cummean(output$lat)
      
      output
    })
  
    ce1 <- reactive({
      req(input$veoSelector)
      ct <- centroid %>%
          filter(iso_a3 == input$veoSelector)
      ct
    })
  
    observe({
      
        slideLength <- as.numeric(rownames(veoDf()))
        
        output$sliderUI <- renderUI({
            sliderInput("slider", "Time Slider",
                      label = NULL,
                      ticks = FALSE,
                      min = min(slideLength),
                      max = max(slideLength),
                      value = min(slideLength),
                      step = 1,
                      animate = animationOptions(interval=200, loop=FALSE)
            )
        })
    })
  
    filteredVeoData <- reactive({
        req(input$slider)
        veoDf()[input$slider, ]
    })

    output$map <- renderLeaflet({
        leaflet(options=leafletOptions(scrollWheelZoom=FALSE)) %>%
            addProviderTiles(providers$CartoDB.Positron) %>%
            setView(lng = st_coordinates(ce1())[1, 1], lat = st_coordinates(ce1())[1, 2], zoom = 5)
    })
    
    observe({
        veoMapData <- filteredVeoData()
      
        leafletProxy("map", data = veoMapData) %>%
                       # clearMarkers() %>%
                       addCircleMarkers(data=veoMapData,
                                        lat = veoMapData$lat,
                                        lng = veoMapData$lon,
                                        color='red',
                                        radius= log(veoMapData$count) * 2)
        
    })
    
    output$cumulativeVeoMap <- renderLeaflet({
      leaflet(options=leafletOptions(scrollWheelZoom=FALSE)) %>%
        addProviderTiles(providers$CartoDB.Positron) %>%
        setView(lng = st_coordinates(ce1())[1, 1], lat = st_coordinates(ce1())[1, 2], zoom = 5) %>%
        clearMarkers()
    })
    
    observe({
      veoCumMapData <- filteredVeoData()
      
      leafletProxy("cumulativeVeoMap", data = veoCumMapData) %>%
        # clearMarkers() %>%
        addCircleMarkers(data=veoCumMapData,
                         lat = veoCumMapData$cum_y,
                         lng = veoCumMapData$cum_x,
                         color='black',
                         radius = 1)
      
    })
    
##### Contagion Data
    contagionDf <- reactive({
      req(input$contagionSelector)
      
      nb <- filter(neighbors, iso_a3 == input$contagionSelector)
      
      n <- c(strsplit(nb$neighbors, ", ")[[1]])
      
      if (nb$neighbors == '') {
        a = paste('AND iso3 =', dbQuoteString(ANSI(), input$contagionSelector))
      } else {
        a = paste('AND (iso3 =', dbQuoteString(ANSI(), input$contagionSelector))
        for (i in 1:length(n)) {
          if (i < length(n)) {
            d = paste('OR iso3 =', dbQuoteString(ANSI(), n[i]))
            a = paste(a, d) 
          } else {
            d = paste0('OR iso3 = ', dbQuoteString(ANSI(), n[i]), ")")
            a = paste(a, d)
          }
        }
      }
      
      query <- sqlInterpolate(ANSI(),
                              "SELECT * FROM acled.acled_master WHERE event = 'Violent Event' AND (veo1 IS NOT NULL OR veo2 IS NOT NULL) ?params;",
                              params = SQL(a)
      )
      
      # Extract country names and iso codes from database
      df <- DBI::dbGetQuery(con, query)
      
      rm(nb, n, a, query)
      
      df <- st_as_sf(df, coords = c("longitude", "latitude"), crs = 4326)
      
      df
      
    })
    
    co <- reactive({
      req(input$contagionSelector)
      output <- country %>%
        filter(country$iso_a3 == input$contagionSelector)
      output
    })
    
    ce <- reactive({
      req(input$contagionSelector)
      output <- centroid  %>%
        filter(centroid$iso_a3 == input$contagionSelector)
      output
    })
    
    b2 <- reactive({
      req(input$contagionSelector)
      output <- b_25  %>%
        filter(b_25$iso_a3 == input$contagionSelector)
      output
    })
    
    b5 <- reactive({
      req(input$contagionSelector)
      output <- b_50  %>%
        filter(b_50$iso_a3 == input$contagionSelector)
      output
    })
    
    b7 <- reactive({
      req(input$contagionSelector)
      output <- b_75  %>%
        filter(b_75$iso_a3 == input$contagionSelector)
      output
    })
    
    b1 <- reactive({
      req(input$contagionSelector)
      output <- b_100  %>%
        filter(b_100$iso_a3 == input$contagionSelector)
      output
    })
    
    output$contagionMap <- renderLeaflet({
      leaflet() %>%
        addProviderTiles(providers$CartoDB.Positron) %>%
        setView(lng = st_coordinates(ce())[1, 1], lat = st_coordinates(ce())[1, 2], zoom = 6) %>%
        addCircleMarkers(data=contagionDf()$geometry,
                         color='rgba(0, 0, 0, 0.2)',
                         radius=.01) %>%
        addGeoJSON(b1()$geometry, color='rgba(0, 255, 0, 0.6)') %>%
        addGeoJSON(b7()$geometry, color='rgba(255, 255, 0, 0.6)') %>%
        addGeoJSON(b5()$geometry, color='rgba(255, 215, 0, 0.6)') %>%
        addGeoJSON(b2()$geometry, color='rgba(255, 0, 0, 0.6)') %>%
        addGeoJSON(co()$geometry, color='rgba(240, 248, 255, 1)')
    })
    
    ring_counts <- reactive({
      w_country <- length(st_contains(co(), contagionDf())[[1]])
      w_25 <- length(st_contains(b2(), contagionDf())[[1]]) - w_country
      w_50 <- length(st_contains(b5(), contagionDf())[[1]]) - w_country - w_25
      w_75 <- length(st_contains(b7(), contagionDf())[[1]]) - w_country - w_25 - w_50
      w_100 <- length(st_contains(b1(), contagionDf())[[1]]) - w_country - w_25 - w_50 - w_75
      o_rings <- nrow(contagionDf()) - w_country - w_25 - w_50 - w_75 - w_100
      
      output <- data.frame(x = c(1, 2, 3, 4, 5, 6), y = c(w_country, w_25, w_50, w_75, w_100, o_rings))
      
      output
    })
    
    # set options
    hcoptslang <- getOption("highcharter.lang")
    hcoptslang$thousandsSep <- ","
    options(highcharter.lang = hcoptslang)
    
    output$contagionGraph <- renderHighchart({
      highchart() %>% 
        hc_chart(type = "bar") %>% 
        hc_title(text = 'Total Number of Violent Events by VEOs', align = 'left', margin = 50) %>% 
        hc_subtitle(text = 'Source: ACLED', align = 'left') %>% 
        hc_xAxis(categories = list('Internal', 'Within 25 Miles', 'Within 50 Miles', 'Within 75 Miles', 'Within 100 Miles', 'External')) %>% 
        hc_yAxis(title = list(text = 'Violent Event Count', align = 'high')) %>% 
        hc_add_series(name = 'Violent Events', data = ring_counts()$y ) %>%
        hc_legend(enabled = FALSE)
    })
    
##### Violence Data
    periodLength <- reactiveVal(1)
    periodValue <- reactiveVal('year')
    periodOther <- reactiveVal('year')
    
    observeEvent(input$periodSelector, {
      if (input$periodSelector == 'day') {
        periodLength(1)
      } else if (input$periodSelector == 'week') {
        periodLength(7)
      } else if (input$periodSelector == 'month') {
        periodLength(1)
      } else if (input$periodSelector == 'quarter') {
        periodLength(3)
      } else if (input$periodSelector == 'year') {
        periodLength(1)
      }
    })
    
    observeEvent(input$periodSelector, {
      periodValue(input$periodSelector)
    })
    
    observeEvent(input$periodSelector, {
      if (input$periodSelector == 'day') {
        periodOther('day')
      } else if (input$periodSelector == 'week') {
        periodOther('day')
      } else if (input$periodSelector == 'month') {
        periodOther('month')
      } else if (input$periodSelector == 'quarter') {
        periodOther('month')
      } else if (input$periodSelector == 'year') {
        periodOther('year')
      }
    })
    
    violenceDf <- reactive({
      req(input$violenceSelector)
      
      violenceQuery <- sqlInterpolate(ANSI(),
                                      "SELECT * FROM acled.acled_master WHERE event = 'Violent Event' AND iso3 = ?params;",
                                      params = dbQuoteString(ANSI(), input$violenceSelector)
      )
      
      df <- DBI::dbGetQuery(con, violenceQuery)
      
      df$veo <- ifelse(!is.na(df$veo1) | !is.na(df$veo2), 'VEO', 'Non-VEO')
      
      # df$fdate <- as.Date(floor_date(df$date, unit = periodValue()))
      
      df$event_count <- 1
      
      df
    })
    
    observe({
      
      dfMin <- min(violenceDf()$date, na.rm = TRUE)
      dfMax <- max(violenceDf()$date, na.rm = TRUE)
      
      output$dateTimeUI <- renderUI({
        dateRangeInput("daterange", "Date Range:",
                       start = dfMin,
                       end = dfMax,
                       min = dfMin,
                       max = dfMax,
                       format = "yyyy-mm-dd",
                       separator = " to "
        )
      })
    })
    
    filteredViolenceData <- reactive({
      req(input$daterange)
      
      results <- violenceDf()
      
      results$fdate <- as.Date(floor_date(results$date, unit = periodValue()))
      
      results <- results %>%
        filter(date >= input$daterange[1] & date <= input$daterange[2])
      
      results
    })
    
    violenceCounts <- reactive({
      
      results <- filteredViolenceData() %>%
        complete(fdate = seq.Date(min(as.Date(fdate), na.rm = TRUE), max(as.Date(fdate), na.rm = TRUE), by = periodValue())) %>%
        group_by(fdate, veo) %>%
        summarise(count = sum(event_count), fatalities = sum(fatalities))
      
      c1 <- c(min(as.Date(results$fdate), na.rm = TRUE))
      c2 <- c("VEO")
      c3 <- c(NA)
      c4 <- c(NA)
      
      df_new <- data.frame(c1, c2, c3, c4) %>%
        rename(fdate = c1, veo = c2, count = c3, fatalities = c4)
      
      if (!("VEO" %in% results$veo) == TRUE) {
        results <- bind_rows(results, df_new)
      }
      
      results <- results %>%
        subset(select = c('fdate', 'veo', 'count')) %>%
        pivot_wider(names_from = veo, values_from = count) %>%
        subset(select = c('Non-VEO', 'VEO')) %>%
        gather() %>%
        group_by(name = key) %>%
        do(data = .$value)
      
      results <- list_parse(results)
      
      results
    })
    
    fatalityCounts <- reactive({
      
      results <- filteredViolenceData() %>%
        complete(fdate = seq.Date(min(as.Date(fdate), na.rm = TRUE), max(as.Date(fdate), na.rm = TRUE), by = periodValue())) %>%
        group_by(fdate, veo) %>%
        summarise(count = sum(event_count), fatalities = sum(fatalities))
      
      c1 <- c(min(as.Date(results$fdate), na.rm = TRUE))
      c2 <- c("VEO")
      c3 <- c(NA)
      c4 <- c(NA)
      
      df_new <- data.frame(c1, c2, c3, c4) %>%
        rename(fdate = c1, veo = c2, count = c3, fatalities = c4)
      
      if (!("VEO" %in% results$veo) == TRUE) {
        results <- bind_rows(results, df_new)
      }
      
      results <- results %>%
        subset(select = c('fdate', 'veo', 'fatalities')) %>%
        pivot_wider(names_from = veo, values_from = fatalities) %>%
        subset(select = c('Non-VEO', 'VEO')) %>%
        gather() %>%
        group_by(name = key) %>%
        do(data = .$value)
      
      results <- list_parse(results)
      
      results
    })
    
    ce3 <- reactive({
      req(input$violenceSelector)
      results <- centroid  %>%
        filter(centroid$iso_a3 == input$violenceSelector)
      results
    })
    
    # set options
    hcoptslang <- getOption("highcharter.lang")
    hcoptslang$thousandsSep <- ","
    options(highcharter.lang = hcoptslang)
    
    output$violenceCountGraph <- renderHighchart({
      highchart(type = 'stock') %>%
        hc_chart(type = "column") %>%
        hc_title(text = 'Total Number of Violent Events', align = 'left', margin = 50) %>%
        hc_subtitle(text = 'Source: ACLED', align = 'left') %>%
        hc_xAxis(type = 'datetime') %>%
        hc_plotOptions(series = list(pointStart = datetime_to_timestamp(as.Date(min(filteredViolenceData()$fdate, na.rm = TRUE))), pointIntervalUnit = periodOther(), pointInterval = periodLength(), stacking='normal')) %>%
        hc_yAxis(title = list(text = 'Violent Event Count', align = 'high')) %>%
        hc_add_series_list(violenceCounts()) %>%
        hc_legend(enabled = TRUE) %>%
        hc_tooltip(shared = TRUE, crosshairs = TRUE)
    })
    
    output$violenceFatalGraph <- renderHighchart({
      highchart(type = 'stock') %>%
        hc_chart(type = "column") %>%
        hc_title(text = 'Total Number of Fatalities', align = 'left', margin = 50) %>%
        hc_subtitle(text = 'Source: ACLED', align = 'left') %>%
        hc_xAxis(type = 'datetime') %>%
        hc_plotOptions(series = list(pointStart = datetime_to_timestamp(as.Date(min(filteredViolenceData()$fdate, na.rm = TRUE))), pointIntervalUnit = periodOther(), pointInterval = periodLength(), stacking='normal')) %>%
        hc_yAxis(title = list(text = 'Fatality Count', align = 'high')) %>%
        hc_add_series_list(fatalityCounts()) %>%
        hc_legend(enabled = TRUE) %>%
        hc_tooltip(shared = TRUE, crosshairs = TRUE)
    })
    
    output$violenceMap <- renderLeaflet({
      leaflet() %>%
        addProviderTiles(providers$CartoDB.Positron) %>%
        setView(lng = st_coordinates(ce3())[1, 1], lat = st_coordinates(ce3())[1, 2], zoom = 6) %>%
        # addHeatmap(data=violenceDf$geometry, blur = 20, max = .05, radius = 10)
        addCircleMarkers(data=filteredViolenceData(),
                         lat = filteredViolenceData()$latitude,
                         lng = filteredViolenceData()$longitude,
                         color='rgba(255, 0, 0, 0.2)',
                         radius=0)
    })
    
##### Demonstration data
    demPeriodLength <- reactiveVal(1)
    demPeriodValue <- reactiveVal('year')
    demPeriodOther <- reactiveVal('year')
    
    observeEvent(input$demPeriodSelector, {
      if (input$demPeriodSelector == 'day') {
        demPeriodLength(1)
      } else if (input$demPeriodSelector == 'week') {
        demPeriodLength(7)
      } else if (input$demPeriodSelector == 'month') {
        demPeriodLength(1)
      } else if (input$demPeriodSelector == 'quarter') {
        demPeriodLength(3)
      } else if (input$demPeriodSelector == 'year') {
        demPeriodLength(1)
      }
    })
    
    observeEvent(input$demPeriodSelector, {
      demPeriodValue(input$demPeriodSelector)
    })
    
    observeEvent(input$demPeriodSelector, {
      if (input$demPeriodSelector == 'day') {
        demPeriodOther('day')
      } else if (input$demPeriodSelector == 'week') {
        demPeriodOther('day')
      } else if (input$demPeriodSelector == 'month') {
        demPeriodOther('month')
      } else if (input$demPeriodSelector == 'quarter') {
        demPeriodOther('month')
      } else if (input$demPeriodSelector == 'year') {
        demPeriodOther('year')
      }
    })
    
    demDf <- reactive({
      req(input$demSelector)
      
      demQuery <- sqlInterpolate(ANSI(),
                                 "SELECT * FROM acled.acled_master WHERE event = 'Demonstration' AND iso3 = ?params;",
                                 params = dbQuoteString(ANSI(), input$demSelector)
      )
      
      df <- DBI::dbGetQuery(con, demQuery)
      
      df$event_count <- 1
      
      df
    })
    
    observe({
      
      demDfMin <- min(demDf()$date, na.rm = TRUE)
      demDfMax <- max(demDf()$date, na.rm = TRUE)
      
      output$demDateTimeUI <- renderUI({
        dateRangeInput("demdaterange", "Date Range:",
                       start = demDfMin,
                       end = demDfMax,
                       min = demDfMin,
                       max = demDfMax,
                       format = "yyyy-mm-dd",
                       separator = " to "
        )
      })
    })
    
    filteredDemData <- reactive({
      req(input$demdaterange)
      
      results <- demDf()
      
      results$fdate <- as.Date(floor_date(results$date, unit = demPeriodValue()))
      
      results <- results %>%
        filter(date >= input$demdaterange[1] & date <= input$demdaterange[2])
      
      results
    })
    
    protestCounts <- reactive({
      
      results <- filteredDemData() %>%
        complete(fdate = seq.Date(min(as.Date(fdate), na.rm = TRUE), max(as.Date(fdate), na.rm = TRUE), by = demPeriodValue())) %>%
        filter(event_type == 'Protests') %>%
        group_by(fdate, sub_event_type) %>%
        summarise(count = sum(event_count))
      
      c1 <- c(min(as.Date(results$fdate), na.rm = TRUE))
      c2 <- c("Protest with intervention")
      c3 <- c(NA)
      
      df_new <- data.frame(c1, c2, c3) %>%
        rename(fdate = c1, sub_event_type = c2, count = c3)
      
      if (!("Protest with intervention" %in% results$sub_event_type) == TRUE) {
        results <- bind_rows(results, df_new)
      }
      
      c1 <- c(min(as.Date(results$fdate), na.rm = TRUE))
      c2 <- c("Peaceful protest")
      c3 <- c(NA)
      
      df_new <- data.frame(c1, c2, c3) %>%
        rename(fdate = c1, sub_event_type = c2, count = c3)
      
      if (!("Peaceful protest" %in% results$sub_event_type) == TRUE) {
        results <- bind_rows(results, df_new)
      }
      
      c1 <- c(min(as.Date(results$fdate), na.rm = TRUE))
      c2 <- c("Excessive force against protesters")
      c3 <- c(NA)
      
      df_new <- data.frame(c1, c2, c3) %>%
        rename(fdate = c1, sub_event_type = c2, count = c3)
      
      if (!("Excessive force against protesters" %in% results$sub_event_type) == TRUE) {
        results <- bind_rows(results, df_new)
      }
      
      results <- results %>%
        subset(select = c('fdate', 'sub_event_type', 'count')) %>%
        pivot_wider(names_from = sub_event_type, values_from = count) %>%
        subset(select = c('Peaceful protest', 'Protest with intervention', 'Excessive force against protesters')) %>%
        gather() %>%
        group_by(name = key) %>%
        do(data = .$value)
      
      results <- list_parse(results)
      
      results
    })
    
    riotCounts <- reactive({
      
      results <- filteredDemData() %>%
        complete(fdate = seq.Date(min(as.Date(fdate), na.rm = TRUE), max(as.Date(fdate), na.rm = TRUE), by = demPeriodValue())) %>%
        filter(event_type == 'Riots') %>%
        group_by(fdate, sub_event_type) %>%
        summarise(count = sum(event_count))
      
      c1 <- c(min(as.Date(results$fdate), na.rm = TRUE))
      c2 <- c("Violent demonstration")
      c3 <- c(NA)
      
      df_new <- data.frame(c1, c2, c3) %>%
        rename(fdate = c1, sub_event_type = c2, count = c3)
      
      if (!("Violent demonstration" %in% results$sub_event_type) == TRUE) {
        results <- bind_rows(results, df_new)
      }
      
      c1 <- c(min(as.Date(results$fdate), na.rm = TRUE))
      c2 <- c("Mob violence")
      c3 <- c(NA)
      
      df_new <- data.frame(c1, c2, c3) %>%
        rename(fdate = c1, sub_event_type = c2, count = c3)
      
      if (!("Mob violence" %in% results$sub_event_type) == TRUE) {
        results <- bind_rows(results, df_new)
      }
      
      results <- results %>%
        subset(select = c('fdate', 'sub_event_type', 'count')) %>%
        pivot_wider(names_from = sub_event_type, values_from = count) %>%
        subset(select = c('Violent demonstration', 'Mob violence')) %>%
        gather() %>%
        group_by(name = key) %>%
        do(data = .$value)
      
      results <- list_parse(results)
      
      results
    })
    
    ce4 <- reactive({
      req(input$demSelector)
      results <- centroid  %>%
        filter(centroid$iso_a3 == input$demSelector)
      results
    })
    
    # set options
    hcoptslang <- getOption("highcharter.lang")
    hcoptslang$thousandsSep <- ","
    options(highcharter.lang = hcoptslang)
    
    output$protestCountGraph <- renderHighchart({
      highchart(type = 'stock') %>%
        hc_chart(type = "column") %>%
        hc_title(text = 'Total Number of Protests', align = 'left', margin = 50) %>%
        hc_subtitle(text = 'Source: ACLED', align = 'left') %>%
        hc_xAxis(type = 'datetime') %>%
        hc_plotOptions(series = list(pointStart = datetime_to_timestamp(as.Date(min(filteredDemData()$fdate, na.rm = TRUE))), pointIntervalUnit = demPeriodOther(), pointInterval = demPeriodLength(), stacking='normal')) %>%
        hc_yAxis(title = list(text = 'Protest Count', align = 'high')) %>%
        hc_add_series_list(protestCounts()) %>%
        hc_legend(enabled = TRUE) %>%
        hc_tooltip(shared = TRUE, crosshairs = TRUE)
    })
    
    output$riotCountGraph <- renderHighchart({
      highchart(type = 'stock') %>%
        hc_chart(type = "column") %>%
        hc_title(text = 'Total Number of Riots', align = 'left', margin = 50) %>%
        hc_subtitle(text = 'Source: ACLED', align = 'left') %>%
        hc_xAxis(type = 'datetime') %>%
        hc_plotOptions(series = list(pointStart = datetime_to_timestamp(as.Date(min(filteredDemData()$fdate, na.rm = TRUE))), pointIntervalUnit = demPeriodOther(), pointInterval = demPeriodLength(), stacking='normal')) %>%
        hc_yAxis(title = list(text = 'Riot Count', align = 'high')) %>%
        hc_add_series_list(riotCounts()) %>%
        hc_legend(enabled = TRUE) %>%
        hc_tooltip(shared = TRUE, crosshairs = TRUE)
    })
    
    output$demonstrationMap <- renderLeaflet({
      leaflet() %>%
        addProviderTiles(providers$CartoDB.Positron) %>%
        setView(lng = st_coordinates(ce4())[1, 1], lat = st_coordinates(ce4())[1, 2], zoom = 6) %>%
        # addHeatmap(data=violenceDf$geometry, blur = 20, max = .05, radius = 10)
        addCircleMarkers(data=filteredDemData(),
                         lat = filteredDemData()$latitude,
                         lng = filteredDemData()$longitude,
                         color='rgba(255, 0, 0, 0.2)',
                         radius=0)
    })
    
#####
# Violence by Region
    # Populate the region dropdown based on the value of the group dropdown
    vioRegionDDData <- reactive({
      req(input$vioRegionGroupSelector)
      
      regionGroupQuery <- sqlInterpolate(ANSI(),
                                         "SELECT DISTINCT ?params FROM iso.region ORDER BY ?params;",
                                         params = SQL(input$vioRegionGroupSelector)
      )
      
      df <- DBI::dbGetQuery(con, regionGroupQuery)
      
      df <- as.data.frame(df)
      
      df[1]
    })
    
    # Use the output from the previous step to update the region selectInput
    observe({
      
      x <- vioRegionDDData()
      
      updateSelectInput(session, "vioRegionSelector",
                        label = "Region:",
                        choices = x
      )
    })
    
    # Use the group and region dropdown values to extract data from the database
    vioRegionDf <- reactive({
      req(input$vioRegionSelector)
      
      vioRegionQuery <- sqlInterpolate(ANSI(),
                                       "SELECT * FROM acled.acled_master WHERE event = 'Violent Event' AND ?cat = ?params;",
                                       cat = SQL(input$vioRegionGroupSelector),
                                       params = dbQuoteString(ANSI(), input$vioRegionSelector)
      )
      
      df <- DBI::dbGetQuery(con, vioRegionQuery)
      
      df
    })
    
    # Update the minimum and maximum dates for the date selector
    observe({
      
      vioRegionDfMin <- min(vioRegionDf()$date, na.rm = TRUE)
      vioRegionDfMax <- max(vioRegionDf()$date, na.rm = TRUE)
      
      output$vioRegionDateTimeUI <- renderUI({
        dateRangeInput("vioRegionDaterange", "Date Range:",
                       start = vioRegionDfMin,
                       end = vioRegionDfMax,
                       min = vioRegionDfMin,
                       max = vioRegionDfMax,
                       format = "yyyy-mm-dd",
                       separator = " to "
        )
      })
    })
    
    # Set the initial values for the period lengths and values for the highcharts time scale x-axis
    vioRegionPeriodLength <- reactiveVal(1)
    vioRegionPeriodValue <- reactiveVal('year')
    vioRegionPeriodOther <- reactiveVal('year')
    
    # Set the period length for each period used in the highcharts time scale
    observeEvent(input$vioRegionPeriodSelector, {
      if (input$vioRegionPeriodSelector == 'day') {
        vioRegionPeriodLength(1)
      } else if (input$vioRegionPeriodSelector == 'week') {
        vioRegionPeriodLength(7)
      } else if (input$vioRegionPeriodSelector == 'month') {
        vioRegionPeriodLength(1)
      } else if (input$vioRegionPeriodSelector == 'quarter') {
        vioRegionPeriodLength(3)
      } else if (input$vioRegionPeriodSelector == 'year') {
        vioRegionPeriodLength(1)
      }
    })
    
    # Sets the period value based on the dropdown selection
    observeEvent(input$vioRegionPeriodSelector, {
      vioRegionPeriodValue(input$vioRegionPeriodSelector)
    })
    
    # Changes the period value to a value that can be used by highcharts
    observeEvent(input$vioRegionPeriodSelector, {
      if (input$vioRegionPeriodSelector == 'day') {
        vioRegionPeriodOther('day')
      } else if (input$vioRegionPeriodSelector == 'week') {
        vioRegionPeriodOther('day')
      } else if (input$vioRegionPeriodSelector == 'month') {
        vioRegionPeriodOther('month')
      } else if (input$vioRegionPeriodSelector == 'quarter') {
        vioRegionPeriodOther('month')
      } else if (input$vioRegionPeriodSelector == 'year') {
        vioRegionPeriodOther('year')
      }
    })
    
    # Filters the data to use the period values. This changes the time period of the graphs
    filteredVioRegionData <- reactive({
      req(input$vioRegionDaterange)
      
      results <- vioRegionDf()
      
      # Sets a new column 'fdate' that is used to aggregate the data based on date ranges
      results$fdate <- as.Date(floor_date(results$date, unit = vioRegionPeriodValue()))
      
      # Creates a new column to denote if a fatality occured at the event
      results$fatal <- ifelse(results$fatalities > 0, 1, 0)
      
      # Filters the data to only include dates between the selected minimum and maximum
      results <- results %>%
        filter(date >= input$vioRegionDaterange[1] & date <= input$vioRegionDaterange[2])
      
      results
    })
    
    dateRangeTitle <- reactive({
      mnd <- input$vioRegionDaterange[1]
      mxd <- input$vioRegionDaterange[2]
      drt <- paste('Dates Included:', mnd, 'to', mxd)
      drt
    })
    
    ##### 
    # Violent event Overview
    # Original violent event data for region
    vioRegionData <- reactive({
      
      data <- filteredVioRegionData() %>%
        arrange(desc(fdate)) %>%
        complete(fdate = seq.Date(min(as.Date(fdate), na.rm = TRUE), max(as.Date(fdate), na.rm = TRUE), by = vioRegionPeriodValue())) %>%
        group_by(fdate, event_type) %>%
        summarise(n = n(), fatalities = sum(fatalities), deadly = sum(fatal)) %>% 
        mutate(nondeadly = n - deadly) %>%
        ungroup() %>%
        complete(fdate, event_type, fill = list(n = 0, freq = 0)) %>%
        group_by(fdate) %>%
        mutate(n = ifelse(is.na(n), 0, n)) %>%
        mutate(fatalities = ifelse(is.na(fatalities), 0, fatalities)) %>%
        mutate(deadly = ifelse(is.na(deadly), 0, deadly)) %>%
        mutate(nondeadly = ifelse(is.na(nondeadly), 0, nondeadly)) %>%
        drop_na()
      
      data
    })
    
    # Time series demonstration data for region
    # Feeds demRegionTimeCount graph
    vioRegionTSData <- reactive({
      
      data <- vioRegionData() %>%
        subset(select = c('fdate', 'event_type', 'n')) %>%
        pivot_wider(names_from = event_type, values_from = n) %>%
        subset(select = c('fdate', 'Battles', 'Explosions/Remote violence', 'Violence against civilians')) %>%
        gather() %>%
        group_by(name = key) %>%
        do(data = .$value)
      
      data <- list_parse(data)
      
      data
    })
    
    # Categorical type demonstration data for region
    # Feeds demRegionTypeCount graph
    vioRegionTypeData <- reactive({
      
      data <- vioRegionData() %>%
        group_by(event_type) %>%
        summarise(count = sum(n))
      
      data
    })
    
    vioRegionMapData <- reactive({
      data <- filteredVioRegionData() %>%
        group_by(iso3) %>%
        summarise(value = n())
      
      data
    })
    
    hcoptslang <- getOption("highcharter.lang")
    hcoptslang$thousandsSep <- ","
    options(highcharter.lang = hcoptslang)
    
    getContent <- function(url) {
      library(httr)
      content(GET(url))
    }
    
    africageojson <- getContent("https://code.highcharts.com/mapdata/custom/africa.geo.json")
    
    output$vioRegionMap <- renderHighchart({
      highchart() %>%
        hc_chart(type = "map", backgroundColor='#FFFFFF') %>%
        hc_title(text = 'Count of Violent Events by Country', align = 'left', margin = 50) %>%
        hc_subtitle(text = paste0('Source: ACLED<br>', dateRangeTitle()), align = 'left') %>%
        hc_add_series_map(
          map = africageojson,
          df = vioRegionMapData(),
          value = "value",
          joinBy = c("iso-a3", "iso3"),
          name = "Violent Events",
          dataLabels = list(enabled = TRUE),
          states = list(hover = list(color = '#a4edba'))
        ) %>%
        hc_colorAxis(visible=FALSE) %>%
        hc_plotOptions(map = list(borderColor = "#000000", borderWidth = 0.1)) %>%
        hc_mapNavigation(
          enabled = TRUE,
          enableMouseWheelZoom = FALSE,
          enableDoubleClickZoom = TRUE,
          buttonOptions = list(verticalAlign = 'bottom')
        )
    })
    
    # Build the time series chart for the demonstration data
    output$vioRegionTimeCount <- renderHighchart({
      highchart(type = 'stock') %>%
        hc_chart(type = "column", backgroundColor='#FFFFFF') %>%
        hc_title(text = 'Count of Violent Events by Time Period', align = 'left', margin = 50) %>%
        hc_subtitle(text = paste0('Source: ACLED<br>', dateRangeTitle()), align = 'left') %>%
        hc_xAxis(type = 'datetime') %>%
        hc_plotOptions(series = list(pointStart = datetime_to_timestamp(as.Date(min(vioRegionTSData()[3][[1]]$data, na.rm=TRUE))), pointIntervalUnit = vioRegionPeriodOther(), pointInterval = vioRegionPeriodLength(), stacking='normal')) %>%
        hc_yAxis(title = list(text = 'Violent Event Count', align = 'high'), opposite=FALSE) %>%
        hc_add_series_list(vioRegionTSData()[c(1:2, 4)]) %>%
        hc_legend(enabled = TRUE, align = 'left') %>%
        hc_tooltip(shared = TRUE, crosshairs = TRUE)
    })
    
    # Graph of the total number of demonstrations by event type
    output$vioRegionTypeCount <- renderHighchart({
      highchart() |> 
        hc_chart(backgroundColor='#FFFFFF') |>
        hc_add_series(vioRegionTypeData(), "bar", hcaes(x = event_type, y = count), name = "Event Counts") %>%
        hc_xAxis(type = 'category', labels=list(useHTML = TRUE, format = '<div class="bar-label">{value}</div>')) %>%
        hc_title(text = 'Count of Violent Events by Type', align = 'left', margin = 50) %>%
        hc_subtitle(text = paste0('Source: ACLED<br>', dateRangeTitle()), align = 'left') %>%
        hc_legend(enabled = FALSE)
    })
    
    ##### 
    # VEO Data and visualizations
    # violent event data for veo by region
    vioRegionVeoData <- reactive({
      
      data <- filteredVioRegionData() %>%
        filter(!is.na(veo1) | !is.na(veo2)) %>%
        arrange(desc(fdate)) %>%
        complete(fdate = seq.Date(min(as.Date(fdate), na.rm = TRUE), max(as.Date(fdate), na.rm = TRUE), by = vioRegionPeriodValue())) %>%
        group_by(fdate, event_type) %>%
        summarise(n = n(), fatalities = sum(fatalities), deadly = sum(fatal)) %>% 
        mutate(nondeadly = n - deadly) %>%
        ungroup() %>%
        complete(fdate, event_type, fill = list(n = 0, freq = 0)) %>%
        group_by(fdate) %>%
        mutate(n = ifelse(is.na(n), 0, n)) %>%
        mutate(fatalities = ifelse(is.na(fatalities), 0, fatalities)) %>%
        mutate(deadly = ifelse(is.na(deadly), 0, deadly)) %>%
        mutate(nondeadly = ifelse(is.na(nondeadly), 0, nondeadly)) %>%
        drop_na()
      
      data
    })
    
    # Time series demonstration data for region
    # Feeds demRegionTimeCount graph
    vioRegionVeoTSData <- reactive({
      
      data <- vioRegionVeoData() %>%
        subset(select = c('fdate', 'event_type', 'n')) %>%
        pivot_wider(names_from = event_type, values_from = n) %>%
        subset(select = c('fdate', 'Battles', 'Explosions/Remote violence', 'Violence against civilians')) %>%
        gather() %>%
        group_by(name = key) %>%
        do(data = .$value)
      
      data <- list_parse(data)
      
      data
    })
    
    # Categorical type demonstration data for region
    # Feeds demRegionTypeCount graph
    vioRegionVeoTypeData <- reactive({
      
      data <- vioRegionVeoData() %>%
        group_by(event_type) %>%
        summarise(count = sum(n))
      
      data
    })
    
    vioRegionVeoMapData <- reactive({
      data <- filteredVioRegionData() %>%
        filter(!is.na(veo1) | !is.na(veo2)) %>%
        group_by(iso3) %>%
        summarise(value = n())
      
      data
    })
    
    output$vioRegionVeoMap <- renderHighchart({
      highchart() %>%
        hc_chart(type = "map", backgroundColor='#FFFFFF') %>%
        hc_title(text = 'Count of VEO Violent Events by Country', align = 'left', margin = 50) %>%
        hc_subtitle(text = paste0('Source: ACLED<br>', dateRangeTitle()), align = 'left') %>%
        hc_add_series_map(
          map = africageojson,
          df = vioRegionVeoMapData(),
          value = "value",
          joinBy = c("iso-a3", "iso3"),
          name = "VEO Violent Events",
          dataLabels = list(enabled = TRUE),
          states = list(hover = list(color = '#a4edba'))
        ) %>%
        hc_colorAxis(visible=FALSE) %>%
        hc_plotOptions(map = list(borderColor = "#000000", borderWidth = 0.1)) %>%
        hc_mapNavigation(
          enabled = TRUE,
          enableMouseWheelZoom = FALSE,
          enableDoubleClickZoom = TRUE,
          buttonOptions = list(verticalAlign = 'bottom')
        )
    })
    
    # Build the time series chart for the demonstration data
    output$vioRegionVeoTimeCount <- renderHighchart({
      highchart(type = 'stock') %>%
        hc_chart(type = "column", backgroundColor='#FFFFFF') %>%
        hc_title(text = 'Count of VEO Violent Events by Time Period', align = 'left', margin = 50) %>%
        hc_subtitle(text = paste0('Source: ACLED<br>', dateRangeTitle()), align = 'left') %>%
        hc_xAxis(type = 'datetime') %>%
        hc_plotOptions(series = list(pointStart = datetime_to_timestamp(as.Date(min(vioRegionVeoTSData()[3][[1]]$data, na.rm=TRUE))), pointIntervalUnit = vioRegionPeriodOther(), pointInterval = vioRegionPeriodLength(), stacking='normal')) %>%
        hc_yAxis(title = list(text = 'VEO Violent Event Count', align = 'high'), opposite=FALSE) %>%
        hc_add_series_list(vioRegionVeoTSData()[c(1:2, 4)]) %>%
        hc_legend(enabled = TRUE, align = 'left') %>%
        hc_tooltip(shared = TRUE, crosshairs = TRUE)
    })
    
    # Graph of the total number of demonstrations by event type
    output$vioRegionVeoTypeCount <- renderHighchart({
      highchart() |> 
        hc_chart(backgroundColor='#FFFFFF') |>
        hc_add_series(vioRegionVeoTypeData(), "bar", hcaes(x = event_type, y = count), name = "VEO Event Counts") %>%
        hc_xAxis(type = 'category', labels=list(useHTML = TRUE, format = '<div class="bar-label">{value}</div>')) %>%
        hc_title(text = 'Count of VEO Violent Events by Type', align = 'left', margin = 50) %>%
        hc_subtitle(text = paste0('Source: ACLED<br>', dateRangeTitle()), align = 'left') %>%
        hc_legend(enabled = FALSE)
    })
    
#####
# Demonstrations by Region
    # Populate the region dropdown based on the value of the group dropdown
    demRegionDDData <- reactive({
      req(input$demRegionGroupSelector)
      
      regionGroupQuery <- sqlInterpolate(ANSI(),
                                         "SELECT DISTINCT ?params FROM iso.region ORDER BY ?params;",
                                         params = SQL(input$demRegionGroupSelector)
      )
      
      df <- DBI::dbGetQuery(con, regionGroupQuery)
      
      df <- as.data.frame(df)
      
      df[1]
    })
    
    # Use the output from the previous step to update the region selectInput
    observe({
      
      x <- demRegionDDData()
      
      updateSelectInput(session, "demRegionSelector",
                        label = "Region:",
                        choices = x
      )
    })
    
    # Use the group and region dropdown values to extract data from the database
    demRegionDf <- reactive({
      req(input$demRegionSelector)
      
      demRegionQuery <- sqlInterpolate(ANSI(),
                                       "SELECT * FROM acled.acled_master WHERE event = 'Demonstration' AND ?cat = ?params;",
                                       cat = SQL(input$demRegionGroupSelector),
                                       params = dbQuoteString(ANSI(), input$demRegionSelector)
      )
      
      df <- DBI::dbGetQuery(con, demRegionQuery)
      
      df
    })
    
    # Update the minimum and maximum dates for the date selector
    observe({
      
      demRegionDfMin <- min(demRegionDf()$date, na.rm = TRUE)
      demRegionDfMax <- max(demRegionDf()$date, na.rm = TRUE)
      
      output$demRegionDateTimeUI <- renderUI({
        dateRangeInput("demRegionDaterange", "Date Range:",
                       start = demRegionDfMin,
                       end = demRegionDfMax,
                       min = demRegionDfMin,
                       max = demRegionDfMax,
                       format = "yyyy-mm-dd",
                       separator = " to "
        )
      })
    })
    
    # Set the initial values for the period lengths and values for the highcharts time scale x-axis
    demRegionPeriodLength <- reactiveVal(1)
    demRegionPeriodValue <- reactiveVal('year')
    demRegionPeriodOther <- reactiveVal('year')
    
    # Set the period length for each period used in the highcharts time scale
    observeEvent(input$demRegionPeriodSelector, {
      if (input$demRegionPeriodSelector == 'day') {
        demRegionPeriodLength(1)
      } else if (input$demRegionPeriodSelector == 'week') {
        demRegionPeriodLength(7)
      } else if (input$demRegionPeriodSelector == 'month') {
        demRegionPeriodLength(1)
      } else if (input$demRegionPeriodSelector == 'quarter') {
        demRegionPeriodLength(3)
      } else if (input$demRegionPeriodSelector == 'year') {
        demRegionPeriodLength(1)
      }
    })
    
    # Sets the period value based on the dropdown selection
    observeEvent(input$demRegionPeriodSelector, {
      demRegionPeriodValue(input$demRegionPeriodSelector)
    })
    
    # Changes the period value to a value that can be used by highcharts
    observeEvent(input$demRegionPeriodSelector, {
      if (input$demRegionPeriodSelector == 'day') {
        demRegionPeriodOther('day')
      } else if (input$demRegionPeriodSelector == 'week') {
        demRegionPeriodOther('day')
      } else if (input$demRegionPeriodSelector == 'month') {
        demRegionPeriodOther('month')
      } else if (input$demRegionPeriodSelector == 'quarter') {
        demRegionPeriodOther('month')
      } else if (input$demRegionPeriodSelector == 'year') {
        demRegionPeriodOther('year')
      }
    })
    
    # Filters the data to use the period values. This changes the time period of the graphs
    filteredDemRegionData <- reactive({
      req(input$demRegionDaterange)
      
      results <- demRegionDf()
      
      # Sets a new column 'fdate' that is used to aggregate the data based on date ranges
      results$fdate <- as.Date(floor_date(results$date, unit = demRegionPeriodValue()))
      
      # Creates a new column to denote if a fatality occured at the event
      results$fatal <- ifelse(results$fatalities > 0, 1, 0)
      
      # Filters the data to only include dates between the selected minimum and maximum
      results <- results %>%
        filter(date >= input$demRegionDaterange[1] & date <= input$demRegionDaterange[2])
      
      results
    })
    
    dateRangeTitle <- reactive({
      mnd <- input$demRegionDaterange[1]
      mxd <- input$demRegionDaterange[2]
      drt <- paste('Dates Included:', mnd, 'to', mxd)
      drt
    })
    
    ##### Demonstrations Overview
    # Original demonstration data for region
    demRegionData <- reactive({
      
      data <- filteredDemRegionData() %>%
        arrange(desc(fdate)) %>%
        complete(fdate = seq.Date(min(as.Date(fdate), na.rm = TRUE), max(as.Date(fdate), na.rm = TRUE), by = demRegionPeriodValue())) %>%
        group_by(fdate, event_type) %>%
        summarise(n = n(), fat = sum(fatalities), fat_events = sum(fatal)) %>% 
        ungroup() %>%
        complete(fdate, event_type, fill = list(n = 0, freq = 0)) %>%
        group_by(fdate) %>%
        mutate(n = ifelse(is.na(n), 0, n)) %>%
        drop_na()
      
      data
    })
    
    # Time series demonstration data for region
    # Feeds demRegionTimeCount graph
    demRegionTSData <- reactive({
      
      data <- demRegionData() %>%
        subset(select = c('fdate', 'event_type', 'n')) %>%
        pivot_wider(names_from = event_type, values_from = n) %>%
        subset(select = c('fdate', 'Protests', 'Riots')) %>%
        gather() %>%
        group_by(name = key) %>%
        do(data = .$value)
      
      data <- list_parse(data)
      
      data
    })
    
    # Categorical type demonstration data for region
    # Feeds demRegionTypeCount graph
    demRegionTypeData <- reactive({
      
      data <- demRegionData() %>%
        group_by(event_type) %>%
        summarise(count = sum(n))
      
      data
    })
    
    demRegionMapData <- reactive({
      data <- filteredDemRegionData() %>%
        group_by(iso3) %>%
        summarise(value = n())
      
      data
    })
    
    hcoptslang <- getOption("highcharter.lang")
    hcoptslang$thousandsSep <- ","
    options(highcharter.lang = hcoptslang)
    
    getContent <- function(url) {
      library(httr)
      content(GET(url))
    }
    
    africageojson <- getContent("https://code.highcharts.com/mapdata/custom/africa.geo.json")
    
    output$demRegionMap <- renderHighchart({
      highchart() %>%
        hc_chart(type = "map", backgroundColor='#FFFFFF') %>%
        hc_title(text = 'Count of Demonstrations by Country', align = 'left', margin = 50) %>%
        hc_subtitle(text = paste0('Source: ACLED<br>', dateRangeTitle()), align = 'left') %>%
        hc_add_series_map(
          map = africageojson,
          df = demRegionMapData(),
          value = "value",
          joinBy = c("iso-a3", "iso3"),
          name = "Demonstrations",
          dataLabels = list(enabled = TRUE),
          states = list(hover = list(color = '#a4edba'))
        ) %>%
        hc_colorAxis(visible=FALSE) %>%
        hc_plotOptions(map = list(borderColor = "#000000", borderWidth = 0.1)) %>%
        hc_mapNavigation(
          enabled = TRUE,
          enableMouseWheelZoom = FALSE,
          enableDoubleClickZoom = TRUE,
          buttonOptions = list(verticalAlign = 'bottom')
        )
    })
    
    # Build the time series chart for the demonstration data
    output$demRegionTimeCount <- renderHighchart({
      highchart(type = 'stock') %>%
        hc_chart(type = "column", backgroundColor='#FFFFFF') %>%
        hc_title(text = 'Count of Demonstrations by Time Period', align = 'left', margin = 50) %>%
        hc_subtitle(text = paste0('Source: ACLED<br>', dateRangeTitle()), align = 'left') %>%
        hc_xAxis(type = 'datetime') %>%
        hc_plotOptions(series = list(pointStart = datetime_to_timestamp(as.Date(min(demRegionTSData()[1][[1]]$data, na.rm=TRUE))), pointIntervalUnit = demRegionPeriodOther(), pointInterval = demRegionPeriodLength(), stacking='normal')) %>%
        hc_yAxis(title = list(text = 'Demonstration Count', align = 'high'), opposite=FALSE) %>%
        hc_add_series_list(demRegionTSData()[2:3]) %>%
        hc_legend(enabled = TRUE, align = 'left') %>%
        hc_tooltip(shared = TRUE, crosshairs = TRUE)
    })
    
    # Graph of the total number of demonstrations by event type
    output$demRegionTypeCount <- renderHighchart({
      highchart() |> 
        hc_chart(backgroundColor='#FFFFFF') |>
        hc_add_series(demRegionTypeData(), "bar", hcaes(x = event_type, y = count), name = "Event Counts") %>%
        hc_xAxis(type = 'category', labels=list(useHTML = TRUE, format = '<div class="bar-label">{value}</div>')) %>%
        hc_title(text = 'Count of Demonstrations by Type', align = 'left', margin = 50) %>%
        hc_subtitle(text = paste0('Source: ACLED<br>', dateRangeTitle()), align = 'left') %>%
        hc_legend(enabled = FALSE)
    })
    
    
    # Original demonstration data for region
    protestRegionData <- reactive({
      
      data <- filteredDemRegionData() %>%
        filter(event_type == 'Protests') %>%
        arrange(desc(fdate)) %>%
        complete(fdate = seq.Date(min(as.Date(fdate), na.rm = TRUE), max(as.Date(fdate), na.rm = TRUE), by = demRegionPeriodValue())) %>%
        group_by(fdate, sub_event_type) %>%
        summarise(n = n(), fatalities = sum(fatalities), deadly = sum(fatal)) %>% 
        mutate(nondeadly = n - deadly) %>% 
        ungroup() %>%
        complete(fdate, sub_event_type, fill = list(n = 0, freq = 0)) %>%
        group_by(fdate) %>%
        mutate(n = ifelse(is.na(n), 0, n)) %>%
        mutate(fatalities = ifelse(is.na(fatalities), 0, fatalities)) %>%
        mutate(deadly = ifelse(is.na(deadly), 0, deadly)) %>%
        mutate(nondeadly = ifelse(is.na(nondeadly), 0, nondeadly)) %>%
        drop_na() %>%
        mutate(percent = case_when(n > 0 ~ round(deadly/n, digits=4) * 100, n == 0 ~ 0))
      
      data
    })
    
    # Time series protest data for region
    # Feeds protestRegionTimeCount graph
    protestRegionTSData <- reactive({
      
      data <- protestRegionData() %>%
        subset(select = c('fdate', 'sub_event_type', 'n')) %>%
        pivot_wider(names_from = sub_event_type, values_from = n) %>%
        subset(select = c('fdate', 'Peaceful protest', 'Protest with intervention', 'Excessive force against protesters')) %>%
        gather() %>%
        group_by(name = key) %>%
        do(data = .$value)
      
      data <- list_parse(data)
      
      data
    })
    
    # Categorical protest data by type for region
    # Feeds proRegionTypeCount graph
    protestRegionTypeData <- reactive({
      
      data <- protestRegionData() %>%
        group_by(sub_event_type) %>%
        summarise(nondeadly = sum(nondeadly), deadly = sum(deadly))
      
      data <- as.data.frame(data)
      row.names(data) <- data$sub_event_type
      data <- data[c('nondeadly', 'deadly')]
      
      data
    })
    
    protestRegionFatalData <- reactive({
      
      data <- protestRegionData() %>%
        subset(select = c(fdate, n, fatalities, deadly)) %>%
        group_by(fdate) %>%
        summarize(n = sum(n), fatalties = sum(fatalities), deadly = sum(deadly)) %>%
        mutate(percent = case_when(n > 0 ~ round(deadly/n, digits=4) * 100, n == 0 ~ 0)) %>%
        gather() %>%
        group_by(name = key) %>%
        do(data = .$value)
      
      data <- list_parse(data)
      
      data
    })
    
    output$protestRegionTypeCount <- renderHighchart({
      highchart() |> 
        hc_chart(type = "bar", backgroundColor='#FFFFFF') %>%
        hc_xAxis(type = 'category', categories = row.names(protestRegionTypeData()), labels=list(useHTML = TRUE, format = '<div class="bar-label">{value}</div>')) %>%
        hc_title(text = 'Count of Protests', align = 'left', margin = 50) %>%
        hc_subtitle(text = paste0('Source: ACLED<br>', dateRangeTitle()), align = 'left') %>%
        hc_plotOptions(series = list(stacking='normal')) %>%
        hc_legend(align='left') %>%
        hc_add_series(protestRegionTypeData()$deadly, name='Deadly') %>%
        hc_add_series(protestRegionTypeData()$nondeadly, name='Non-Deadly')
    })
    
    output$protestRegionTimeCount <- renderHighchart({
      highchart(type = 'stock') %>%
        hc_chart(type = "column", backgroundColor='#FFFFFF') %>%
        hc_title(text = 'Count of Protests by Time Period', align = 'left', margin = 50) %>%
        hc_subtitle(text = paste0('Source: ACLED<br>', dateRangeTitle()), align = 'left') %>%
        hc_xAxis(type = 'datetime') %>%
        hc_plotOptions(series = list(pointStart = datetime_to_timestamp(as.Date(min(protestRegionTSData()[2][[1]]$data, na.rm = TRUE))), pointIntervalUnit = demRegionPeriodOther(), pointInterval = demRegionPeriodLength(), stacking='normal')) %>%
        hc_yAxis(title = list(text = 'Protest Count', align = 'high'), opposite=FALSE) %>%
        hc_add_series_list(protestRegionTSData()[c(1, 3, 4)]) %>%
        hc_legend(enabled = TRUE, align = 'left') %>%
        hc_tooltip(shared = TRUE, crosshairs = TRUE)
    })
    
    output$protestRegionFatalPercent <- renderHighchart({
      highchart(type = 'stock') %>%
        hc_chart(type = "column", backgroundColor='#FFFFFF') %>%
        hc_title(text = 'Percentage of Protests Resulting in Death by Time Period', align = 'left', margin = 50) %>%
        hc_subtitle(text = paste0('Source: ACLED<br>', dateRangeTitle()), align = 'left') %>%
        hc_xAxis(type = 'datetime') %>%
        hc_plotOptions(series = list(pointStart = datetime_to_timestamp(as.Date(min(protestRegionFatalData()[3][[1]]$data, na.rm = TRUE))), pointIntervalUnit = demRegionPeriodOther(), pointInterval = demRegionPeriodLength(), stacking='normal')) %>%
        hc_yAxis(title = list(text = 'Percentage of Protests', align = 'high'), opposite=FALSE) %>%
        hc_add_series_list(protestRegionFatalData()[5]) %>%
        hc_legend(enabled = FALSE, align = 'left') %>%
        hc_tooltip(shared = TRUE, crosshairs = TRUE, pointFormat = 'Percentage Resulting in Death: {point.y}')
    })
    
    output$protestRegionFatalCount <- renderHighchart({
      highchart(type = 'stock') %>%
        hc_chart(type = "column", backgroundColor='#FFFFFF') %>%
        hc_title(text = 'Count of Deaths from Protests by Time Period', align = 'left', margin = 50) %>%
        hc_subtitle(text = paste0('Source: ACLED<br>', dateRangeTitle()), align = 'left') %>%
        hc_xAxis(type = 'datetime') %>%
        hc_plotOptions(series = list(pointStart = datetime_to_timestamp(as.Date(min(protestRegionFatalData()[3][[1]]$data, na.rm = TRUE))), pointIntervalUnit = demRegionPeriodOther(), pointInterval = demRegionPeriodLength(), stacking='normal')) %>%
        hc_yAxis(title = list(text = 'Count of Deaths', align = 'high'), opposite=FALSE) %>%
        hc_add_series_list(protestRegionFatalData()[2]) %>%
        hc_legend(enabled = FALSE, align = 'left') %>%
        hc_tooltip(shared = TRUE, crosshairs = TRUE, pointFormat = 'Death Count: {point.y}')
    })
    
    # Original riot data for region
    riotRegionData <- reactive({
      
      data <- filteredDemRegionData() %>%
        filter(event_type == 'Riots') %>%
        arrange(desc(fdate)) %>%
        complete(fdate = seq.Date(min(as.Date(fdate), na.rm = TRUE), max(as.Date(fdate), na.rm = TRUE), by = demRegionPeriodValue())) %>%
        group_by(fdate, sub_event_type) %>%
        summarise(n = n(), fatalities = sum(fatalities), deadly = sum(fatal)) %>% 
        mutate(nondeadly = n - deadly) %>%
        ungroup() %>%
        complete(fdate, sub_event_type, fill = list(n = 0, freq = 0)) %>%
        group_by(fdate) %>%
        mutate(n = ifelse(is.na(n), 0, n)) %>%
        mutate(fatalities = ifelse(is.na(fatalities), 0, fatalities)) %>%
        mutate(deadly = ifelse(is.na(deadly), 0, deadly)) %>%
        mutate(nondeadly = ifelse(is.na(nondeadly), 0, nondeadly)) %>%
        drop_na() %>%
        mutate(percent = case_when(n > 0 ~ round(deadly/n, digits=4) * 100, n == 0 ~ 0))
      
      data
    })
    
    # Time series riot data for region
    # Feeds riotRegionTimeCount graph
    riotRegionTSData <- reactive({
      
      data <- riotRegionData() %>%
        subset(select = c('fdate', 'sub_event_type', 'n')) %>%
        pivot_wider(names_from = sub_event_type, values_from = n) %>%
        subset(select = c('fdate', 'Violent demonstration', 'Mob violence')) %>%
        gather() %>%
        group_by(name = key) %>%
        do(data = .$value)
      
      data <- list_parse(data)
      
      data
    })
    
    # Categorical riot data by type for region
    # Feeds riotRegionTypeCount graph
    riotRegionTypeData <- reactive({
      
      data <- riotRegionData() %>%
        group_by(sub_event_type) %>%
        summarise(nondeadly = sum(nondeadly), deadly = sum(deadly))
      
      data <- as.data.frame(data)
      row.names(data) <- data$sub_event_type
      data <- data[c('nondeadly', 'deadly')]
      
      data
    })
    
    riotRegionFatalCountData <- reactive({
      
      data <- riotRegionData() %>%
        subset(select = c(fdate, n, fatalities, deadly)) %>%
        group_by(fdate) %>%
        summarize(n = sum(n), fatalties = sum(fatalities), deadly = sum(deadly)) %>%
        mutate(percent = case_when(n > 0 ~ round(deadly/n, digits=4) * 100, n == 0 ~ 0)) %>%
        gather() %>%
        group_by(name = key) %>%
        do(data = .$value)
      
      data <- list_parse(data)
      
      data
    })
    
    output$riotRegionTimeCount <- renderHighchart({
      highchart(type = 'stock') %>%
        hc_chart(type = "column", backgroundColor='#FFFFFF') %>%
        hc_title(text = 'Count of Riots by Time Period', align = 'left', margin = 50) %>%
        hc_subtitle(text = paste0('Source: ACLED<br>', dateRangeTitle()), align = 'left') %>%
        hc_xAxis(type = 'datetime') %>%
        hc_plotOptions(series = list(pointStart = datetime_to_timestamp(as.Date(min(riotRegionTSData()[1][[1]]$data, na.rm = TRUE))), pointIntervalUnit = demRegionPeriodOther(), pointInterval = demRegionPeriodLength(), stacking='normal')) %>%
        hc_yAxis(title = list(text = 'Riot Count', align = 'high'), opposite=FALSE) %>%
        hc_add_series_list(riotRegionTSData()[2:3]) %>%
        hc_legend(enabled = TRUE, align = 'left') %>%
        hc_tooltip(shared = TRUE, crosshairs = TRUE)
    })
    
    output$riotRegionTypeCount <- renderHighchart({
      highchart() |> 
        hc_chart(type = "bar", backgroundColor='#FFFFFF') %>%
        hc_xAxis(type = 'category', categories = row.names(riotRegionTypeData()), labels=list(useHTML = TRUE, format = '<div class="bar-label">{value}</div>')) %>%
        hc_title(text = 'Count of Riots', align = 'left', margin = 50) %>%
        hc_subtitle(text = paste0('Source: ACLED<br>', dateRangeTitle()), align = 'left') %>%
        hc_plotOptions(series = list(stacking='normal')) %>%
        hc_legend(align='left') %>%
        hc_add_series(riotRegionTypeData()$deadly, name='Deadly') %>%
        hc_add_series(riotRegionTypeData()$nondeadly, name='Non-Deadly')
    })
    
    output$riotRegionFatalPercent <- renderHighchart({
      highchart(type = 'stock') %>%
        hc_chart(type = "column", backgroundColor='#FFFFFF') %>%
        hc_title(text = 'Percentage of Riots Resulting in Death by Time Period', align = 'left', margin = 50) %>%
        hc_subtitle(text = paste0('Source: ACLED<br>', dateRangeTitle()), align = 'left') %>%
        hc_xAxis(type = 'datetime') %>%
        hc_plotOptions(series = list(pointStart = datetime_to_timestamp(as.Date(min(riotRegionFatalCountData()[3][[1]]$data, na.rm = TRUE))), pointIntervalUnit = demRegionPeriodOther(), pointInterval = demRegionPeriodLength(), stacking='normal')) %>%
        hc_yAxis(title = list(text = 'Percentage of Riots', align = 'high'), opposite=FALSE) %>%
        hc_add_series_list(riotRegionFatalCountData()[5]) %>%
        hc_legend(enabled = FALSE, align = 'left') %>%
        hc_tooltip(shared = TRUE, crosshairs = TRUE, pointFormat = 'Percentage Resulting in Death: {point.y}')
    })
    
    output$riotRegionFatalCount <- renderHighchart({
      highchart(type = 'stock') %>%
        hc_chart(type = "column", backgroundColor='#FFFFFF') %>%
        hc_title(text = 'Count of Deaths from Riots by Time Period', align = 'left', margin = 50) %>%
        hc_subtitle(text = paste0('Source: ACLED<br>', dateRangeTitle()), align = 'left') %>%
        hc_xAxis(type = 'datetime') %>%
        hc_plotOptions(series = list(pointStart = datetime_to_timestamp(as.Date(min(riotRegionFatalCountData()[3][[1]]$data, na.rm = TRUE))), pointIntervalUnit = demRegionPeriodOther(), pointInterval = demRegionPeriodLength(), stacking='normal')) %>%
        hc_yAxis(title = list(text = 'Count of Fatalities', align = 'high'), opposite=FALSE) %>%
        hc_add_series_list(riotRegionFatalCountData()[2]) %>%
        hc_legend(enabled = FALSE, align = 'left') %>%
        hc_tooltip(shared = TRUE, crosshairs = TRUE, pointFormat = 'Death Count: {point.y}')
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
