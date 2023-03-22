library(tidyverse)
library(leaflet)
library(leaflet.extras)
library(lubridate)
library(mapview)
library(sf)
library(mapsf)
library(geojsonsf)
library(maptools)
library(spacetime)
# Read in ACLED Data 
setwd("~/Documents/AFRICOM/acled/")

temp = list.files("~/Documents/AFRICOM/acled/acled-main/Split_Data/")
temp <- paste0("acled-main/Split_Data/", temp)
data_list <- lapply(temp, read_csv)
data <- bind_rows(data_list)

regions <- c("Western Africa",
             "Middle Africa",
             "Eastern Africa",
             "Southern Africa",
             "Northern Africa")
data <- data %>% filter(region %in% regions) %>% 
  mutate(event_date = as.Date(event_date, format = '%d %B %Y'),
         Month = month(event_date)
         )

# Join manually labeled details 
event <- read_csv('data/event.csv')
inter_code <- read_csv('data/inter_code.csv')
interaction <- read_csv('data/interaction.csv')
# sub_event_type <- read_csv('data/sub_event_type.csv')
veo <- read_csv('data/veo.csv')

data_joined <- data %>% 
  inner_join(event, by = c("event_type" = "event_type")) %>% 
  left_join(veo %>% rename(veo1 = veo), by = c("actor1" = "actor")) %>% 
  left_join(veo %>% rename(veo2 = veo), by = c("actor2" = "actor")) %>%
  left_join(inter_code %>% rename(inter_code1 = inter_code), by = c("inter1" = "inter_code_id")) %>% 
  left_join(inter_code %>% rename(inter_code2 = inter_code), by = c("inter2" = "inter_code_id"))

data_joined <- data_joined %>% filter(event == "Violent Event",
                       (!is.na(veo1) | !is.na(veo2))
                       )

# Take out early years where there is little activity - this is maybe just due to ACLED being in an early stage
data_joined <- data_joined %>% 
  filter(year > 2009)

df_sf <- data_joined %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326) %>% 
  mutate(YearMonth = year * 100 + data_joined$Month)

save(df_sf, file = "acled_cleaned.RData")
# mapview(df_sf, cex = 3, alpha = .5, popup = NULL)

area_honeycomb_grid = st_make_grid(df_sf, cellsize = c(1, 1), what = "polygons", square = FALSE)

honeycomb_grid_sf = st_sf(area_honeycomb_grid) %>%
  mutate(grid_id = 1:length(lengths(area_honeycomb_grid)))

# Remove oceans - focus only on Africa
# world <- read_file("geojson/world.geojson")

# sf <- geojson_sf(world)
# africa <- sf[sf$CONTINENT == 'Africa',]

# africa_grid <- honeycomb_grid_sf[africa,]
# mapview(africa_grid)

# Focus on a particular area for now 
# Let's start with West Africa 
data("wrld_simpl")
africa_countries <- wrld_simpl[wrld_simpl$REGION == 2,]
countries <- c('Niger','Nigeria', 'Benin', 'Ghana', "Cote d'Ivoire", 'Burkina Faso', 
               'Senegal', 'Mali', 'Togo', 'Chad', 'Cameroon')
africa_countries <- africa_countries[africa_countries$NAME %in% countries, ]

africa_grid_west_africa <- honeycomb_grid_sf[st_as_sf(africa_countries),]

# Save as shapefile: 
# africa_grid_west_africa %>% st_write("data/africa_grid.shp")

africa_grid_west_africa_geom <- st_geometry(africa_grid_west_africa)

population_data_2 <- population_data %>% 
  mutate(longitude = round(longitude, 2), latitude = round(latitude, 2)) %>% 
  group_by(longitude, latitude) %>% 
  summarize(gha_general_2020 = sum(gha_general_2020))
nrow(population_data_2)

population_data_2sf <- st_as_sf(population_data_2, coords = c("longitude", "latitude"))
 
# Create grids for each year/month  
years <- unique(df_sf$year)
min_year <- min(years)
max_year <- max(years)
min_month <- min(df_sf[df_sf$year == min_year, ]$Month)
max_month <- max(df_sf[df_sf$year == max_year, ]$Month)

grids_list <- list()

for(y in min_year:max_year){
  if(y == min_year) range = min_month:12
  else if(y == max_year) range = 1:max_month
  else range = 1:12
  for(m in range){
    honeycomb_grid_sf_my <- africa_grid_west_africa
    df_sf_m_y <- df_sf %>% filter(year == y & Month == m) 
    honeycomb_grid_sf_my$n_colli = lengths(st_intersects(honeycomb_grid_sf_my, df_sf_m_y))
    honeycomb_grid_sf_my$year <- y
    honeycomb_grid_sf_my$month <- m
    
    grids_list[[length(grids_list) + 1]] <- honeycomb_grid_sf_my
    
  }
}

grids_list <- do.call(rbind, grids_list)

save(grids_list, file = "data/grid_violence_monthly_data.rData")


grids_list_year = list()
for(y in min_year:2020){
    honeycomb_grid_sf_my <- africa_grid_west_africa
    df_sf_m_y <- df_sf %>% filter(year == y) 
    honeycomb_grid_sf_my$n_colli = lengths(st_intersects(honeycomb_grid_sf_my, df_sf_m_y))
    honeycomb_grid_sf_my$year <- y
    honeycomb_grid_sf_my$month <- m
    
    grids_list_year[[length(grids_list_year) + 1]] <- honeycomb_grid_sf_my
}

africa_grid_west_africa_plot = africa_grid_west_africa
africa_grid_west_africa_plot$n_colli = lengths(st_intersects(africa_grid_west_africa_plot, df_sf))
mapview(africa_grid_west_africa_plot %>% mutate(ln_colli = log(n_colli + 1)) %>% filter(n_colli != 0), zcol = "ln_colli")

grids_list_year <- do.call(rbind, grids_list_year)

save(grids_list_year, file = "data/grid_violence_monthly_data.rData")
mapview(grids_list_year %>% filter(year == 2010), zcol = "n_colli")
mapview(grids_list_year %>% filter(year == 2012), zcol = "n_colli")
mapview(grids_list_year %>% filter(year == 2014), zcol = "n_colli")
mapview(grids_list_year %>% filter(year == 2016), zcol = "n_colli")
mapview(grids_list_year %>% filter(year == 2018), zcol = "n_colli")
mapview(grids_list_year %>% filter(year == 2020), zcol = "n_colli")

mapview(df_sf)

# Exploratory data - take a look at the change across all locations over time 
time_means <- data.frame(Month = numeric(), Year = numeric(), Total = numeric())
for(i in 1:length(grids_list)){
  current <- grids_list[[i]]
  current_month <- min(current$month)
  current_year <- min(current$year)
  total <- sum(current$n_colli)
  df <- data.frame(Month = current_month, Year = current_year, Total = total)
  time_means <- time_means %>% bind_rows(df)
}
time_means$Date <- zoo::as.yearmon(paste(time_means$Year, time_means$Month), "%Y %m")
time_means %>% 
ggplot(aes(x = Date, y = Total)) + 
  geom_line() + 
  stat_smooth(method = "loess", formula = y ~ x, size = 1) 

library(fable)
library(tsibble)
library(tsibbledata)
time_means_ts <- zoo::zoo(time_means$Total, time_means$Date) 

time_means_ts <- as_tsibble(time_means, index = time_means$Date)

time_means_ts %>% 
  model(
    ets = ETS(box_cox(Turnover, 0.3)),
    arima = ARIMA(log(Turnover)),
    snaive = SNAIVE(Turnover)
  ) %>%
  forecast(h = "2 years") %>% 
  autoplot(filter(aus_retail, year(Month) > 2010), level = NULL)


# Combine all the grids into one data.frame
grids_over_time <- do.call(grids_list, rbind)

# Convert to centroids
grids_over_time_pt <- grids_over_time %>% st_centroid()


grids_over_time_pt$Date <- zoo::as.yearmon(paste(grids_over_time_pt$year, grids_over_time_pt$month), "%Y %m")

grids_over_time_pt <- grids_over_time_pt %>% mutate(long = unlist(map(grids_over_time_pt$area_honeycomb_grid,1)),
                              lat = unlist(map(grids_over_time_pt$area_honeycomb_grid,2)))

# Convert to a spacetime dataframe 
grids_over_time_df <- as.data.frame(grids_over_time_pt)
grids_spacetime <- stConstruct(grids_over_time_df, space = c('long', 'lat'), time = 'Date')

library(FRK)
library(STRbook)

G <- auto_basis(data = SpatialPoints(cbind(grids_over_time_pt$long, grids_over_time_pt$lat)),
                nres = 1, # One resolution
                type = "Gaussian") # Gaussian

S <- eval_basis(basis = G, # basis functions
                s = cbind(grids_over_time_pt$long, grids_over_time_pt$lat) %>%
                  as.matrix()) %>% # conv. to matrix
  as.matrix()
colnames(S) <- paste0("B", 1:ncol(S)) # assign

basis_df <- cbind(grids_over_time_df,S) %>% select(-grid_id, -area_honeycomb_grid, -Date)

plot_basis <- basis_df %>% 
  group_by(long,  lat) %>% 
  summarize(B1 = min(B1), B2 = min(B2), B3 = min(B3), B4 = min(B4), B5 = min(B5), B6 = min(B6),
            B7 = min(B7), B8 = min(B8), B9 = min(B9), B10 = min(B10), B11 = min(B11), B12 = min(B12),
            B13 = min(B13), B14 = min(B14), B15 = min(B15)) %>% 
  group_by()
plot_basis <- plot_basis %>% 
  mutate(long = round(long, 2), lat = round(lat, 2))

# Visualize the basis functions 
plot_ly(data = plot_basis, x = ~lat, y = ~long, z = ~B1, type = "mesh3d")
plot_ly(data = plot_basis, x = ~lat, y = ~long, z = ~B2, type = "mesh3d")
plot_ly(data = plot_basis, x = ~lat, y = ~long, z = ~B3, type = "mesh3d")
plot_ly(data = plot_basis, x = ~lat, y = ~long, z = ~B4, type = "mesh3d")
plot_ly(data = plot_basis, x = ~lat, y = ~long, z = ~B5, type = "mesh3d")
plot_ly(data = plot_basis, x = ~lat, y = ~long, z = ~B6, type = "mesh3d")
plot_ly(data = plot_basis, x = ~lat, y = ~long, z = ~B7, type = "mesh3d")
plot_ly(data = plot_basis, x = ~lat, y = ~long, z = ~B8, type = "mesh3d")
plot_ly(data = plot_basis, x = ~lat, y = ~long, z = ~B9, type = "mesh3d")
plot_ly(data = plot_basis, x = ~lat, y = ~long, z = ~B10, type = "mesh3d")
plot_ly(data = plot_basis, x = ~lat, y = ~long, z = ~B11, type = "mesh3d")
plot_ly(data = plot_basis, x = ~lat, y = ~long, z = ~B12, type = "mesh3d")
plot_ly(data = plot_basis, x = ~lat, y = ~long, z = ~B13, type = "mesh3d")
plot_ly(data = plot_basis, x = ~lat, y = ~long, z = ~B14, type = "mesh3d")
plot_ly(data = plot_basis, x = ~lat, y = ~long, z = ~B15, type = "mesh3d")


library(plotly)
plot_ly(
  x = as.numeric(colnames(plot_matrix)), 
  y = as.numeric(rownames(plot_matrix)), 
  z = plot_matrix
) %>% 
  add_markers() %>%
  layout(
    title = "",
    scene = list(
      xaxis = list(title = "Longitude"),
      yaxis = list(title = "Latitude"),
      zaxis = list(title = "Median")
    ))


library(pscl)

basis_glm = zeroinfl(n_colli ~ lat + long + year + B1 + B2 + B3 + B4 + B5 + B6 + B7 + B8 + B9 + B10 + B11 + B12 + B13 + B14 + B15 | 
                       lat + long + year + B1 + B2 + B3 + B4 + B5 + B6 + B7 + B8 + B9 + B10 + B11 + B12 + B13 + B14 + B15, data = basis_df)

# Is it poisson? 
hist(basis_df$n_colli[basis_df$n_colli != 0])

basis_df %>% 
  filter(n_colli != 0) %>% 
  ggplot(aes(x = n_colli)) + 
  geom_histogram(binwidth=1)

boot::inv.logit(396.15162)

library(gstat)
data("STObj3", package = "STRbook")
STObj4 <- STObj3[, "1993-07-01::1993-07-31"]

vv <- variogram(object = z~1 + lat, # fixed effect component
                data = STObj4, # July data
                width = 80, # spatial bin (80 km)
                cutoff = 1000, # consider pts < 1000 km apart
                tlags = 0.01:6.01) # 0 days to 6 days


vv <- variogram(n_colli ~ 1 , grids_spacetime, dx = 0) # 0 days to 6 days

