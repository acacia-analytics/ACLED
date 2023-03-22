
# Better Organized script

# Initialization
library(tidyverse)
library(sf)
library(mapview)
library(glue)
library(igraph)
library(surveillance)
library(zoo)
setwd("~/Documents/AFRICOM/acled/")
sf_use_s2(FALSE) # This prevents an error when trying to use the full African continent 

## Create geometries for all Africa - adjust Libya to show lower level regions 
world <- st_read("geojson/adm1_polygons.shp/adm1_polygons.shp")
libya <- st_read("geojson/lby_adm_unosat_lbsc_20180507_SHP/lby_admbnda_adm2_unosat_lbsc_20180507.shp")

africa_x_libya <- africa %>% filter(iso_3 != "LBY")
# Rename columns to match 
libya_for_append <- libya %>% rename(adm1_src = ADM2_PCODE,adm0_name = ADM0_EN, adm1_name = ADM2_EN
)
africa_w_libya <- bind_rows(africa_x_libya, libya_for_append)

africa <- world %>% filter(region1_nm == "Africa")

# 1 - Get poverty estimates 
poverty_data_fix <- read_csv("/Users/joejohnson/Desktop/Poverty_Data_Province_All Fix (Autosaved).csv")
poverty_data_fix <-  poverty_data_fix %>% filter(!is.na(Province_Code))
africa <- africa %>% left_join(poverty_data_fix, by = c("adm1_src" = "Province_Code"))

save(africa, file = "R_Data_Files/Africa_Wide_W_Poverty.RData")

# 2 - Get population estimates - separate script 
source("Province_Population_Estimates.R")

# 3 - Get VEO estimates - create quarterly and monthly data frame - separate script? 
load("R_Data_Files/Africa_Wide_W_Population_Step2.RData")
source("Province_VEO_Estimates.R")

# Split up into regions, remove most zero values 
load("R_Data_Files/R_Data_Files/Africa_Wide_VEO_Step_3.RData")
source("Create_DF_for_Analysis.R")

# Now run the model 




