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

# Remove the veo data and the query
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

##### This is the user interface for the dashboard
# Define UI for application
ui <- bootstrapPage(
  
  navbarPage("ACLED",
  
    # Application title
    windowTitle = "ACLED",
    
    tabPanel("VEO Centriod",
        div(class="outer",
            tags$head(includeCSS("styles.css")),
      
            leafletOutput("map", width="100%", height="95vh"),
        
            # Sidebar with a slider input for number of bins 
            absolutePanel(id = "controls", class = "panel panel-default",
                          top = 75, right = 55, width = 250, fixed=TRUE,
                          draggable = TRUE, height = "auto",
                          tags$br(),
                          tags$h3("Select Country"),
                          selectInput("veoSelector", "", veoDropdown),
                          uiOutput("sliderUI")
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
            setView(lng = st_coordinates(ce1())[1, 1], lat = st_coordinates(ce1())[1, 2], zoom = 6)
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
}

# Run the application 
shinyApp(ui = ui, server = server)
