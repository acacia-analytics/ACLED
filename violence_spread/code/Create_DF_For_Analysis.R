load("R_Data_Files/Africa_Wide_VEO_Step_3.RData")

# Eliminate extraneous columns
africa <- africa %>% select(-adm1_id, -adm1_name1, -adm1_name2, -adm0_id,
                            -adm0_name2, -src_lvl, -src_lang, -src_lang1, 
                            -src_lang2, -src_date, -src_update, -src_name, -src_name1,
                            -src_lic, -src_url, -src_grp, -iso_2, -iso_3_grp, 
                            -status_cd, -wld_date, -wld_update, -wld_view, -wld_notes, 
                            -ISO_Numeric, -ISO3, -Country, -Region, -Survey,-Year, 
                            -Province.x, -Alternative_Name, -Notes, -Province.y) 

load("acled_cleaned.RData")
years <- unique(df_sf$year)
min_year <- min(years)
max_year <- max(years)
min_month <- min(df_sf[df_sf$year == min_year, ]$Month)
max_month <- max(df_sf[df_sf$year == max_year, ]$Month)

acled_monthly <- list()

for(y in min_year:max_year){
  if(y == min_year) range = min_month:12
  else if(y == max_year) range = 1:max_month
  else range = 1:12
  for(m in range){
    grid_sf_my <- africa
    df_sf_m_y <- df_sf %>% filter(year == y & Month == m) 
    grid_sf_my$n_colli = lengths(st_intersects(grid_sf_my, df_sf_m_y))
    grid_sf_my$year <- y
    grid_sf_my$month <- m
    
    acled_monthly[[length(acled_monthly) + 1]] <- grid_sf_my
    
  }
}

acled_monthly <- do.call(rbind, acled_monthly)

acled_monthly <- acled_monthly %>% 
  select(-veo_events) %>% 
  mutate(Date = zoo::as.yearmon(paste(year, month), "%Y %m")) %>% 
  rename(veo_events = n_colli) %>% 
  filter(Neighboring_Violence)

# Generate monthly datasets for each region and plot 
# Algeria 
nigeria_countries <- c('Niger', 'Nigeria', 'Chad',  'Cameroon')
burkina_faso_countries <- c('Benin', 'Ghana', "Côte d'Ivoire",'Senegal', 'Togo','Burkina Faso', 'Mali') 
east_africa_countries <- c('Kenya', 'Ethiopia', 'Djibouti', "Somalia")
libya_countries <- c('Libya')
algeria_countries <- c('Algeria')
mozambique_countries <- c('Mozambique')

# Spatial data frames not based on time 
nigeria_geo <- africa %>% 
  filter(Neighboring_Violence, adm0_name1 %in% nigeria_countries) 
burkina_faso_geo <- africa %>% 
  filter(Neighboring_Violence, adm0_name1 %in% burkina_faso_countries) 
east_africa_geo <- africa %>% 
  filter(Neighboring_Violence, adm0_name1 %in% east_africa_countries) 
libya_geo <- africa %>% 
  filter(Neighboring_Violence, adm0_name1 %in% libya_countries) 
algeria_geo <- africa %>% 
  filter(Neighboring_Violence, adm0_name1 %in% algeria_countries) 
mozambique_geo <- africa %>% 
  filter(Neighboring_Violence, adm0_name1 %in% mozambique_countries) 

nigeria_monthly <- acled_monthly %>% 
  filter(adm0_name1 %in% nigeria_countries) 
burkina_faso_monthly <- acled_monthly %>% 
  filter(adm0_name1 %in% burkina_faso_countries) 
east_africa_monthly <- acled_monthly %>% 
  filter(adm0_name1 %in% east_africa_countries) 
libya_monthly <- acled_monthly %>% 
  filter(adm0_name1 %in% libya_countries) 
algeria_monthly <- acled_monthly %>% 
  filter(adm0_name1 %in% algeria_countries) 
mozambique_monthly <- acled_monthly %>% 
  filter(adm0_name1 %in% mozambique_countries) 

africa_monthly_ts <- acled_monthly %>% 
  as.data.frame() %>% 
  group_by(Date) %>% 
  summarize(veo_events = sum(veo_events)) 

nigeria_monthly_ts <- acled_monthly %>% 
  filter(adm0_name1 %in% nigeria_countries) %>% 
  as.data.frame() %>% 
  group_by(Date) %>% 
  summarize(veo_events = sum(veo_events))

burkina_faso_monthly_ts <- acled_monthly %>% 
  filter(adm0_name1 %in% burkina_faso_countries) %>% 
  as.data.frame() %>% 
  group_by(Date) %>% 
  summarize(veo_events = sum(veo_events))

libya_monthly_ts <- acled_monthly %>% 
  filter(adm0_name1 %in% libya_countries) %>% 
  as.data.frame() %>% 
  group_by(Date) %>% 
  summarize(veo_events = sum(veo_events))

algeria_monthly_ts <- acled_monthly %>% 
  filter(adm0_name1 %in% algeria_countries) %>% 
  as.data.frame() %>% 
  group_by(Date) %>% 
  summarize(veo_events = sum(veo_events))

east_africa_monthly_ts <- acled_monthly %>% 
  filter(adm0_name1 %in% east_africa_countries) %>% 
  as.data.frame() %>% 
  group_by(Date) %>% 
  summarize(veo_events = sum(veo_events))

mozambique_monthly_ts <- acled_monthly %>% 
  filter(adm0_name1 %in% mozambique_countries) %>% 
  as.data.frame() %>% 
  group_by(Date) %>% 
  summarize(veo_events = sum(veo_events))

  ggplot(nigeria_monthly_ts, aes(x = Date, y = veo_events)) + geom_line() 
  ggplot(burkina_faso_monthly_ts, aes(x = Date, y = veo_events)) + geom_line()   
  ggplot(libya_monthly_ts, aes(x = Date, y = veo_events)) + geom_line() 
  ggplot(algeria_monthly_ts, aes(x = Date, y = veo_events)) + geom_line() 
  ggplot(east_africa_monthly_ts, aes(x = Date, y = veo_events)) + geom_line() 
  ggplot(mozambique_monthly_ts, aes(x = Date, y = veo_events)) + geom_line() 
  
  # Fix regions that don't start until later 
  min_date <- min(mozambique_monthly[mozambique_monthly$veo_events != 0, ]$Date)
  mozambique_monthly_ts <- mozambique_monthly_ts %>% filter(Date >= min_date)
  mozambique_monthly <- mozambique_monthly %>% filter(Date >= min_date)
  
  # Fix burkina faso?
  
  
# Get quarterly ACLED Data 
  df_sf <- df_sf %>% mutate(
    quarter = case_when(
      Month >= 1 & Month <= 3 ~ 1,  
      Month >= 4 & Month <= 6 ~ 2,  
      Month >= 7 & Month <= 9 ~ 3,  
      Month >= 10 & Month <= 12 ~ 4,
    )
  )
  years <- unique(df_sf$year)
  min_year <- min(years)
  max_year <- max(years)
  min_quarter <- min(df_sf[df_sf$year == min_year, ]$quarter)
  max_quarter <- max(df_sf[df_sf$year == max_year, ]$quarter)
  
  acled_quarterly <- list()
  
  for(y in min_year:max_year){
    if(y == min_year) range = min_month:12
    else if(y == max_year) range = 1:max_quarter
    else range = 1:4
    for(q in range){
      grid_sf_qy <- africa
      df_sf_q_y <- df_sf %>% filter(year == y & quarter == q) 
      grid_sf_qy$n_colli = lengths(st_intersects(grid_sf_qy, df_sf_q_y))
      grid_sf_qy$year <- y
      grid_sf_qy$quarter <- q
      
      acled_quarterly[[length(acled_quarterly) + 1]] <- grid_sf_qy
      
    }
  }
  acled_quarterly <- do.call(rbind, acled_quarterly)
  
  acled_quarterly <- acled_quarterly %>% 
    select(-veo_events) %>% 
    mutate(Date = zoo::as.yearqtr(paste(year, quarter), "%Y %q")) %>% 
    rename(veo_events = n_colli) %>% 
    filter(Neighboring_Violence) 
  
  # Take out last quarter because it is incomplete
  acled_quarterly <- acled_quarterly %>% filter(!(year == 2021 & quarter == 3))

  # Min date for mozambique - because violence doesn't start until later in the sample
  
  africa_quarterly_ts <- acled_quarterly %>% 
    as.data.frame() %>% 
    group_by(Date) %>% 
    summarize(veo_events = sum(veo_events))
  
  nigeria_quarterly_ts <- acled_quarterly %>% 
    filter(adm0_name1 %in% nigeria_countries) %>% 
    as.data.frame() %>% 
    group_by(Date) %>% 
    summarize(veo_events = sum(veo_events))

  burkina_faso_quarterly_ts <- acled_quarterly %>% 
    filter(adm0_name1 %in% burkina_faso_countries) %>% 
    as.data.frame() %>% 
    group_by(Date) %>% 
    summarize(veo_events = sum(veo_events))
  
  libya_quarterly_ts <- acled_quarterly %>% 
    filter(adm0_name1 %in% libya_countries) %>% 
    as.data.frame() %>% 
    group_by(Date) %>% 
    summarize(veo_events = sum(veo_events))
  
  algeria_quarterly_ts <- acled_quarterly %>% 
    filter(adm0_name1 %in% algeria_countries) %>% 
    as.data.frame() %>% 
    group_by(Date) %>% 
    summarize(veo_events = sum(veo_events))
  
  east_africa_quarterly_ts <- acled_quarterly %>% 
    filter(adm0_name1 %in% east_africa_countries) %>% 
    as.data.frame() %>% 
    group_by(Date) %>% 
    summarize(veo_events = sum(veo_events))
  
  mozambique_quarterly_ts <- acled_quarterly %>% 
    filter(adm0_name1 %in% mozambique_countries) %>% 
    as.data.frame() %>% 
    group_by(Date) %>% 
    summarize(veo_events = sum(veo_events)) 
  
  # Filter out stuff that starts later 
  min_date_mozambique <- min(mozambique_quarterly_ts[mozambique_quarterly_ts$veo_events != 0, ]$Date)
  mozambique_quarterly_ts <- mozambique_quarterly_ts %>% filter(Date >= min_date_mozambique)
  
  nigeria_quarterly <- acled_quarterly %>% 
    filter(adm0_name1 %in% nigeria_countries)

  burkina_faso_quarterly <- acled_quarterly %>% 
    filter(adm0_name1 %in% burkina_faso_countries)
  
  libya_quarterly <- acled_quarterly %>% 
    filter(adm0_name1 %in% libya_countries)
  
  algeria_quarterly <- acled_quarterly %>% 
    filter(adm0_name1 %in% algeria_countries)
  
  east_africa_quarterly <- acled_quarterly %>% 
    filter(adm0_name1 %in% east_africa_countries) 
  
  mozambique_quarterly <- acled_quarterly %>% 
    filter(adm0_name1 %in% mozambique_countries)  %>% filter(Date >= min_date_mozambique)
    
  ggplot(nigeria_quarterly_ts, aes(x = Date, y = veo_events)) + geom_line() 
  ggplot(burkina_faso_quarterly_ts, aes(x = Date, y = veo_events)) + geom_line() 
  ggplot(libya_quarterly_ts, aes(x = Date, y = veo_events)) + geom_line() 
  ggplot(algeria_quarterly_ts, aes(x = Date, y = veo_events)) + geom_line() 
  ggplot(east_africa_quarterly_ts, aes(x = Date, y = veo_events)) + geom_line() 
  ggplot(mozambique_quarterly_ts, aes(x = Date, y = veo_events)) + geom_line() 
  
# Combine each region into list and then save quarterly and monthly data together
  monthly_data <- list(
    nigeria = nigeria_monthly,
    burkina_faso = burkina_faso_monthly,
    libya = libya_monthly, 
    algeria = algeria_monthly, 
    east_africa = east_africa_monthly, 
    mozambique = mozambique_monthly
  )
  
monthly_data_ts <- list(
  nigeria = nigeria_monthly_ts,
  burkina_faso = burkina_faso_monthly_ts,
  libya = libya_monthly_ts, 
  algeria = algeria_monthly_ts, 
  east_africa = east_africa_monthly_ts, 
  mozambique = mozambique_monthly_ts
)

quarterly_data_ts <- list(
  nigeria = nigeria_quarterly_ts,
  burkina_faso = burkina_faso_quarterly_ts,
  libya = libya_quarterly_ts,
  algeria = algeria_quarterly_ts,
  east_africa = east_africa_quarterly_ts,
  mozambique = mozambique_quarterly_ts
)

quarterly_data <- list(
  nigeria = nigeria_quarterly,
  burkina_faso = burkina_faso_quarterly, 
  libya = libya_quarterly,
  algeria = algeria_quarterly,
  east_africa = east_africa_quarterly,
  mozambique = mozambique_quarterly
)

geo_data <- list(
  nigeria = nigeria_geo,
  burkina_faso = burkina_faso_geo, 
  east_africa = east_africa_geo,
  libya = libya_geo,
  algeria = algeria_geo,
  mozambique = mozambique_geo
)
save(monthly_data, monthly_data_ts, quarterly_data, quarterly_data_ts, geo_data, file = "R_Data_Files/Africa_Data_for_Analysis.RData")

