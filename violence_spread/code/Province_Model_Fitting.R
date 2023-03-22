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
library(Matrix)
library(geostan)
library(glue)
library(zoo)
library(surveillance)

setwd("~/Documents/AFRICOM/acled/")

load("R_Data_Files/Africa_Data_for_Analysis.RData")

# Function to generate adjacency matrix 
construct_sts <- function(geo_df, time_geo_df, id_col, 
                          date_col,
                          population_col, 
                          max_train_date = NA,
                          min_train_date = NA,
                          veo_col,
                          future_months = 1:12/12
                          ){
  

  adj_mat <- poly2adjmat(geo_df)
  nbOrder <- nbOrder(adj_mat, maxlag = Inf)
  colnames(nbOrder) <- as.character(geo_df[[id_col]])
  rowSums(adj_mat) # check that adjacency matrix looks right   
  spatial_polygon_wav <- as_Spatial(st_geometry(geo_df))
  row.names(spatial_polygon_wav) <- as.character(geo_df[[id_col]])
  population <- geo_df[[population_col]]
  
  if(!is.na(max_train_date)){
    in_training_sample <- time_geo_df[[date_col]] <= max_train_date
    time_geo_df <- time_geo_df[in_training_sample,]
  }
  if(!is.na(min_train_date)){
    in_training_sample <- time_geo_df[[date_col]] >= min_train_date
    time_geo_df <- time_geo_df[in_training_sample,]
  }
  
  maxDate <- max(time_geo_df[[date_col]])
  one_year_ahead <- maxDate + future_months

  grids_list_forecast <- time_geo_df
  
  for(y in one_year_ahead){
    future_df <- geo_df
    future_df[[date_col]] <- zoo::as.yearmon(y)
    future_df[[date_col]] <- zoo::as.yearmon(future_df[[date_col]] )
    grids_list_forecast <- grids_list_forecast %>% bind_rows(future_df)
  }
  
  grids_list_forecast[[veo_col]] <- ifelse(is.na(grids_list_forecast[[veo_col]]), NA, grids_list_forecast[[veo_col]])
  
  observed <- grids_list_forecast %>% as.data.frame() 
  observed <- data.frame(
    Date = observed[[date_col]],
    id = observed[[id_col]],
    veos = observed[[veo_col]]
  ) %>% pivot_wider(names_from = id, values_from = veos)
  observed <- observed[, -1]
  
  counts <- grids_list_forecast[[veo_col]] 
  
  # Get initial values as year and month  
  year_min <- lubridate::year(as.Date(min(time_geo_df[[date_col]]))) 
  
  month_min <- filter(time_geo_df, lubridate::year(as.Date((time_geo_df[[date_col]]))) == year_min)
  month_min <- min(lubridate::month(as.Date((month_min[[date_col]]))))
  
  pop_mat <- t(matrix(rep(population / sum(population), nrow(time_geo_df)), nrow = length(population), ncol = nrow(time_geo_df)))
  
  
  sts_veo <- sts(observed = observed, 
                 start = c(year_min, month_min), 
                 frequency = 12,
                 neighbourhood = nbOrder,
                 population = population / sum(population),
                 map = spatial_polygon_wav
  )
  
  return(sts_veo)
}

# Construct STS's for: 
# Quarterly, Monthly 
# East Africa, West Africa (split into 2?), Mozambique

# Libya + Algeria seem to not have enough recent violence 
sts_veo_east <- construct_sts(geo_df = geo_data$east_africa_geo,
                         time_geo_df = monthly_data$east_africa, 
                         id_col = "id", 
                         date_col = "Date",
                         population_col = "T_TL", 
                         min_train_date = zoo::as.yearmon(as.Date('2010-01-01')),
                         max_train_date = zoo::as.yearmon(as.Date('2021-07-01')),
                         
                         veo_col = "veo_events")

# Push Mozambique later
sts_veo_mozambique <- construct_sts(geo_df = geo_data$mozambique_geo,
                              time_geo_df = monthly_data$mozambique, 
                              id_col = "id", 
                              date_col = "Date",
                              population_col = "T_TL", 
                              min_train_date = zoo::as.yearmon(as.Date('2017-10-01')),
                              max_train_date = zoo::as.yearmon(as.Date('2021-07-01')),
                              
                              veo_col = "veo_events")

# How to divide west africa? Nigeria vs Burkina Faso? 
nigeria_geo <- geo_data$west_africa_geo %>% 
  filter(iso_3 %in% c("NGA", "NER", "TCD", "CMR"))  
burkina_faso_geo <- geo_data$west_africa_geo %>% 
  filter(iso_3 %in% c("BEN", "BFA", "CIV", "MLI", "GHA", "TGO", "SEN")) 

# Weird outliers that are not really contiguous with the rest of the data 
# They are not wrong... but I don't want to deal with modeling them if unconnected

outliers <- burkina_faso_geo %>% 
  filter((adm1_src %in% c('BJ10', 'CI01', 'CI20', 'CI24', 'CI30', "GH16", "GH15")) ) %>% select(id) %>% as.data.frame() 
outliers_ids <- outliers$id

burkina_faso_geo <- burkina_faso_geo %>% filter(!(id %in% outliers_ids))

nigeria_monthly <- monthly_data$west_africa %>% 
  filter(iso_3 %in% c("NGA", "NER", "TCD", "CMR"))  
burkina_faso_monthly <- monthly_data$west_africa %>% 
  filter(iso_3 %in% c("BEN", "BFA", "CIV", "MLI", "GHA", "TGO", "SEN")) %>% 
  filter(!(id %in% outliers_ids))

nigeria_monthly

nigeria_monthly_ts <- nigeria_monthly %>% 
  as.data.frame() %>% 
  group_by(Date) %>% 
  summarize(veo_events = sum(veo_events))

ggplot(nigeria_monthly_ts, aes(x = Date, y = veo_events)) + geom_line() 

burkina_faso_monthly_ts <- burkina_faso_monthly %>% 
  as.data.frame() %>% 
  group_by(Date) %>% 
  summarize(veo_events = sum(veo_events))

ggplot(burkina_faso_monthly_ts, aes(x = Date, y = veo_events)) + geom_line() 

sts_veo_nigeria <- construct_sts(geo_df = nigeria_geo,
              time_geo_df = nigeria_monthly, 
              id_col = "id", 
              date_col = "Date",
              population_col = "T_TL", 
              min_train_date = zoo::as.yearmon(as.Date('2017-01-01')),
              max_train_date = zoo::as.yearmon(as.Date('2019-01-01')),
              veo_col = "veo_events")
              #,
#

# plot(sts_veo, type = observed ~ unit)

sts_veo_control <- list(end = list( 
                         offset = population(sts_veo)), 
              ar = list(f = addSeason2formula(~1 + t + t^2, period = sts_veo@freq)), 
              ne = list(f = ~1, weights = neighbourhood(sts_veo) == 1), 
              family = "NegBin1")

sts_veo_control <- list(
  ar = list(f = ~1 + ~t ), 
  ne = list(f = ~1, weights = neighbourhood(sts_veo) == 1, offset = 1), 
  family = "NegBin1")


# Model training function 

model <- hhh4(stsObj = sts_veo, control = sts_veo_control)

summary(model)
# Random effects 
#model <- update(model,
#       end = list(f = update(formula(model)$end, ~. + ri() - 1)),
#       ar = list(f = update(formula(model)$ar, ~. + ri() - 1)),
#       ne = list(f = update(formula(model)$ne, ~. + ri() - 1)))
# AIC(model, update(model, family = "Poisson"))

# Control for population - coefficient not significant and AIC is higher 
# Youth population 
# youth_pop = west_africa_veo %>% 
#   as.data.frame() %>% 
#   select(P_M_15_29) %>% 
#   as.matrix() 
# youth_pop <- matrix(youth_pop, nrow=dim(sts_veo)[1], ncol=dim(sts_veo)[2], byrow=TRUE)

#model_pop <- update(model, 
#                           ne = list(f = ~log(pop) * youth_pop), 
#                           data = list(pop = population(sts_veo), youth_pop = youth_pop))

#model_pop <- update(model, 
#                           ne = list(f = ~log(pop)), 
#                           data = list(pop = population(sts_veo)))

#coef(model_pop, se = TRUE)

# Allow power-law spatial interaction 
#model_power <-update(model, ne=list(weights=W_np(maxlag=3)))
#coef(model_power, se = TRUE)

#summary(model_power, idx2Exp = TRUE, amplitudeShift = TRUE, maxEV = TRUE)
# Random effects model - allow for different coefficients for different locations 
# Should research these models more to understand what's going on better 

# Evaluate predictive performance 
#AIC(model_power, model_pop, model)

#pred <- oneStepAhead(model, nrow(sts_veo) - 5, type = "rolling", which.start = "final", verbose = FALSE)

#plot(pred, probs = c(.1,.9), means.args = list())

#preds_2 <- oneStepAhead(model, nrow(sts_veo), type = "rolling", which.start = "final", verbose = FALSE)

# Forecasting - the future observations that we forecast for need to be included in sts_veo 
# But the model does not need to be trained on them

# Train the model on the missing 12 months 
model2 <- update(model, subset.upper = nrow(sts_veo) - 12)
hhh4sim1 <- simulate(model2, nsim = 1000, seed = 726, 
                     subset = (nrow(sts_veo) - 11):nrow(sts_veo),
                     y.start = observed(sts_veo)[nrow(sts_veo) - 12,])


# Make nice fancharts 
# Starting params 
model_simulation <- hhh4sim1
sts_actual <- sts_veo

observed <- observed(sts_actual)
total_observed <- rowSums(observed)
total_observed <- total_observed[1:(length(total_observed) - 12)]

simulation_result_mean <- apply(model_simulation, 1:2, mean)
simulation_result_p99 <- apply(model_simulation, 1:2, FUN = function(x) quantile(x, 0.99) )
simulation_result_p90 <- apply(model_simulation, 1:2, FUN = function(x) quantile(x, 0.9) )
simulation_result_p70 <- apply(model_simulation, 1:2, FUN = function(x) quantile(x, 0.7) )
simulation_result_p50 <- apply(model_simulation, 1:2, FUN = function(x) quantile(x, 0.5) )
simulation_result_p30 <- apply(model_simulation, 1:2, FUN = function(x) quantile(x, 0.3) )
simulation_result_p10 <- apply(model_simulation, 1:2, FUN = function(x) quantile(x, 0.1) )
simulation_result_p01 <- apply(model_simulation, 1:2, FUN = function(x) quantile(x, 0.01) )

simulation_result_mean <- rowSums(simulation_result_mean)
simulation_result_p99 <- rowSums(simulation_result_p99)
simulation_result_p90 <- rowSums(simulation_result_p90)
simulation_result_p70 <- rowSums(simulation_result_p70)
simulation_result_p50 <- rowSums(simulation_result_p50)
simulation_result_p30 <- rowSums(simulation_result_p30)
simulation_result_p10 <- rowSums(simulation_result_p10)
simulation_result_p01 <- rowSums(simulation_result_p01)

# Alternative 
simulation_result_mean <- apply(model_simulation, 1:2, mean)
simulation_result_p99 <- apply(model_simulation, 1:2, FUN = function(x) quantile(x, 0.99) )
simulation_result_p90 <- apply(model_simulation, 1:2, FUN = function(x) quantile(x, 0.9) )
simulation_result_p70 <- apply(model_simulation, 1:2, FUN = function(x) quantile(x, 0.7) )
simulation_result_p50 <- apply(model_simulation, 1:2, FUN = function(x) quantile(x, 0.5) )
simulation_result_p30 <- apply(model_simulation, 1:2, FUN = function(x) quantile(x, 0.3) )
simulation_result_p10 <- apply(model_simulation, 1:2, FUN = function(x) quantile(x, 0.1) )
simulation_result_p01 <- apply(model_simulation, 1:2, FUN = function(x) quantile(x, 0.01) )

simulation_result_mean <- rowSums(simulation_result_mean)
simulation_result_p99 <- rowSums(simulation_result_p99)
simulation_result_p90 <- rowSums(simulation_result_p90)
simulation_result_p70 <- rowSums(simulation_result_p70)
simulation_result_p50 <- rowSums(simulation_result_p50)
simulation_result_p30 <- rowSums(simulation_result_p30)
simulation_result_p10 <- rowSums(simulation_result_p10)
simulation_result_p01 <- rowSums(simulation_result_p01)



first_date = sort(unique(grids_list$Date))[1]
dates = rep(first_date, length(total_observed) + length(simulation_result_mean))

for(i in 1:length(dates)){
  dates[i] = first_date
  first_date = first_date + 1/12
}
dates <- zoo::as.yearmon(dates)

# Pad with NAs 
pad_for_simulation <- rep(NA, length(total_observed))
pad_for_actual <- rep(NA, length(simulation_result_mean))

simulation_result_mean <- c(pad_for_simulation, simulation_result_mean)
simulation_result_p99 <- c(pad_for_simulation, simulation_result_p99)
simulation_result_p90 <- c(pad_for_simulation, simulation_result_p90)
simulation_result_p70 <- c(pad_for_simulation, simulation_result_p70)
simulation_result_p50 <- c(pad_for_simulation, simulation_result_p50)
simulation_result_p30 <- c(pad_for_simulation, simulation_result_p30)
simulation_result_p10 <- c(pad_for_simulation, simulation_result_p10)
simulation_result_p01 <- c(pad_for_simulation, simulation_result_p01)
total_observed <- c(total_observed, pad_for_actual)

fanchart_data <- data.frame(
  Date = dates, 
  Actual = total_observed,
  Simulation_Mean = rep(NA, length(simulation_result_mean)),
  Simulation_P99 = simulation_result_p99,
  Simulation_P90 = simulation_result_p90,
  Simulation_P70 = simulation_result_p70,
  Simulation_P50 = simulation_result_p50,
  Simulation_P30 = simulation_result_p30,
  Simulation_P10 = simulation_result_p10,
  Simulation_P01 = simulation_result_p01
)

make_fan_chart(fanchart_data)
# 
# simulation_result <- apply(hhh4sim1, 1:2, mean)
# IDs <- as.numeric(colnames(simulation_result))
# t_matrix <- t(simulation_result)
# colnames(t_matrix) <- paste0("Month", 1:12)
# t_matrix <- as.data.frame(t_matrix)
# t_matrix$id <- IDs
# 
# geo_w_forecasts <-west_africa_veo %>% left_join(t_matrix)
# 
# mapView(geo_w_forecasts, zcol = 'Month1')
# mapView(geo_w_forecasts, zcol = 'Month2')
# mapView(geo_w_forecasts, zcol = 'Month3')
# mapView(geo_w_forecasts, zcol = 'Month4')
# mapView(geo_w_forecasts, zcol = 'Month5')
# mapView(geo_w_forecasts, zcol = 'Month6')
# mapView(geo_w_forecasts, zcol = 'Month7')
# mapView(geo_w_forecasts, zcol = 'Month8')
# mapView(geo_w_forecasts, zcol = 'Month9')
# mapView(geo_w_forecasts, zcol = 'Month10')
# mapView(geo_w_forecasts, zcol = 'Month11')
# mapView(geo_w_forecasts, zcol = 'Month12')
# 
# plot(hhh4sim1, "fan", means.args = list(), key.args = list())
# 
# # Need to combine simulation in fan chart with real data 
# 
# 
# 
# model_Sim <- simulate(model,
#                           nsim = 150, 
#                           seed = 1, 
#                           subset = 129:139,
#                       )
# 
# plot(model_Sim, "fan", means.args = list(), key.args = list())
# 
# 
# plot(colSums(colSums(model_Sim, dims = 2)), type = "line")
# 
# grids_list_ts <- names(grids_list) %>% 
#   as.data.frame() %>% 
#   group_by(Date) %>% 
#   summarize(veo_events = sum(n_colli))
# 
# grids_list_ts %>% 
#   ggplot(aes(x = Date, y = veo_events)) + 
#   geom_line() + 
#   stat_smooth(method = "loess", formula = y ~ x, size = 1) 
# 
# # Specific ID 
# grids_list %>% 
#   as.data.frame() %>% 
#   filter(id == 13) %>%
#   group_by(Date) %>% 
#   summarize(veo_events = sum(n_colli)) %>% 
#   ggplot(aes(x = Date, y = veo_events)) + 
#   geom_line() + 
#   stat_smooth(method = "loess", formula = y ~ x, size = 1) 
# 
# 
# locations <- colnames(model_Sim)
# projected_data <- t(model_Sim)[, ncol(model_Sim)]
# 
# final_pred <- model_Sim[, , 12]
# 
# 
# 
# 
