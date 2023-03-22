# Population Estimates  
load("R_Data_Files/Africa_Wide_W_Poverty.RData")

# Concatenate all population data - names are unfortunately not normalized
pop_list <- list()
files <- list.files("data/Population_Provinces/")
# Exclude Mali -> no youth population info - not near terrorism anyway
files <- files[files != "Mali_Population_by_Age.csv"]

for(f in 1:length(files)){
  iso_3 <- substr(files[f], 1, 3)
  print(iso_3)
  if(iso_3 %in% c('ben')){
    pop_list[[f]] <- read_csv(glue("data/Population_Provinces/", files[f])) %>% 
      mutate(P_M_15_29 = (M_15_19 + M_20_24 + M_25_29) / T_TL) %>% 
      select(PCODE = admin1Pcod, T_TL, P_M_15_29)
  } else if(iso_3 %in% c('ken', 'ago', 'bdi', 'bfa', 'bwa', 'cmr', 'gha', 'nga', 
                         'tcd', 'cmr', 'gab', 'gin', 'ken', 'lso', 'moz', 'mrt', 
                         'mwi', 'nam', 'rwa', 'sdn', 'swz', 'tcd', 'tun', 'tza',
                         'uga', 'zaf', 'zwe')){
    pop_list[[f]] <- read_csv(glue("data/Population_Provinces/", files[f])) %>% 
      mutate(P_M_15_29 = (M_15_19 + M_20_24 + M_25_29) / T_TL) %>% 
      select(PCODE = ADM1_PCODE, T_TL, P_M_15_29)
  } 
  else if(iso_3 %in% c('cod')){
    # Use population 5 - 19 ? Could balance it out with the adult percent maybe?
    pop_list[[f]] <- read_csv(glue("data/Population_Provinces/", files[f])) %>% 
      # mutate(P_M_15_29 = (M_05_19) / T_TL) %>% 
      mutate(P_M_15_29 = NA) %>% 
      select(PCODE = admin1Pcode, T_TL, P_M_15_29)
  } else if(iso_3 %in% c('dza')){
    # Algeria does not have youth population data
    pop_list[[f]] <- readxl::read_excel(glue("data/Population_Provinces/", files[f])) %>% 
      mutate(P_M_15_29 = NA)
  } else if(iso_3 %in% c('ssd')){
    # Use population 5 - 17 ? Could balance it out with the adult percent maybe?
    pop_list[[f]] <- read_csv(glue("data/Population_Provinces/", files[f])) %>% 
      # mutate(P_M_15_29 = (M_05_17) / T_TL) %>% 
      mutate(P_M_15_29 = NA) %>% 
      select(PCODE = ADM1_PCODE, T_TL, P_M_15_29)
  } else if(iso_3 %in% c('caf')){
    # Use population 0 - 18 ? Could balance it out with the adult percent maybe?
    pop_list[[f]] <- read_csv(glue("data/Population_Provinces/", files[f])) %>% 
      # mutate(P_M_15_29 = (Enfants_00_18 / T_TL) * (T_TM / T_TL)) %>% 
      mutate(P_M_15_29 = NA) %>% 
      select(PCODE = Adm1_pcode, T_TL, P_M_15_29)
  } else if(iso_3 %in% c('eth')){
    pop_list[[f]] <- read_csv(glue("data/Population_Provinces/", files[f])) %>% 
      mutate(P_M_15_29 = (M_15_17	+ M_18_19 +	M_20_24	+ M_25_29) / T_TL) %>% 
      select(PCODE = admin1Pcode, T_TL, P_M_15_29)
  } else if(iso_3 %in% c('gmb')){
    pop_list[[f]] <- read_delim(glue("data/Population_Provinces/", files[f]), delim = ";") %>% 
      mutate(P_M_15_29 = (M_15_19 + M_20_24 + M_25_29) / T_TL) %>% 
      select(PCODE = ADM1_PCODE, T_TL, P_M_15_29)
  } else if(iso_3 %in% c('egy')){
    # National level youth percentage data
    # source: https://unstats.un.org/unsd/demographic-social/products/dyb/dyb_2020/
    pop_list[[f]] <- readxl::read_excel(glue("data/Population_Provinces/", files[f])) %>% 
      # mutate(P_M_15_29 = (M_TL / T_TL) * ((4959005 +  4489725 +  4275161) / 51799324)) %>% 
      mutate(P_M_15_29 = NA) %>% 
      select(PCODE = ADM1_PCODE, T_TL, P_M_15_29)
  } else if(iso_3 %in% c('ner')){
    pop_list[[f]] <- read_csv(glue("data/Population_Provinces/", files[f])) %>% 
      mutate(P_M_15_29 = (M_15_19 + M_20_24 + M_25_29) / T_TL) %>% 
      mutate(ADM1_PCODE = str_replace(ADM1_PCODE, "R", "")) %>% 
      select(PCODE = ADM1_PCODE, T_TL, P_M_15_29)
  } else if(iso_3 %in% c('tgo')){
    pop_list[[f]] <- read_csv(glue("data/Population_Provinces/", files[f])) %>% 
      mutate(P_M_15_29 = (M_15_19 + M_20_24 + M_25_29) / T_TL) %>% 
      select(PCODE = ADM1_PCODE, T_TL, P_M_15_29) %>% 
      filter(!is.na(PCODE))
  } else if(iso_3 %in% c('som')){
    # National level youth percentage - from CIA world factbook (not reliable enough - make NA now)
    pop_list[[f]] <- read_csv(glue("data/Population_Provinces/", files[f])) %>% 
      mutate(P_M_15_29 = NA) %>% #.1981 * .5
      select(PCODE = admin1Pcode, T_TL, P_M_15_29) %>% 
      filter(!is.na(PCODE))
  } else if(iso_3 %in% c('lby')){
    # National level youth percentage - from CIA world factbook (not reliable enough - make NA now)
    pop_list[[f]] <- read_csv(glue("data/Population_Provinces/", files[f])) %>% 
      mutate(P_M_15_29 = NA) %>% 
      select(PCODE, T_TL, P_M_15_29) %>% 
      filter(!is.na(PCODE))
  }
  else if(iso_3 %in% c('eri')){
    # Fill in with national statistics on youth male percent 
    # source: https://unstats.un.org/unsd/demographic-social/products/dyb/dyb_2020/
    pop_list[[f]] <- read_csv(glue("data/Population_Provinces/", files[f])) %>% 
      mutate(P_M_15_29 = (156082 + 141888 + 166664) / 3464588) %>% 
      select(PCODE = Admn1code, T_TL, P_M_15_29) %>% 
      filter(!is.na(PCODE))
  } else if(iso_3 %in% c('dji')){
    # Fill in with national statistics on youth male percent 
    # source: https://unstats.un.org/unsd/demographic-social/products/dyb/dyb_2020/
    pop_list[[f]] <- read_xlsx(glue("data/Population_Provinces/", files[f]), sheet = 2) %>% 
      mutate(P_M_15_29 = NA) %>% 
      select(PCODE = ADM1_PCODE, T_TL, P_M_15_29) %>% 
      filter(!is.na(PCODE))
  }else if(iso_3 %in% c('civ',  'sen', 'lbr', 'sle')){
    # Estimate with male and female percentage multiplied by youth percentage
    pop_list[[f]] <- read_csv(glue("data/Population_Provinces/", files[f])) %>% 
      mutate(P_M_15_29 = (M_TL  / T_TL) * (T_15_19 + T_20_24 + T_25_29) / T_TL) %>% 
      select(PCODE = ADM1_PCODE, T_TL, P_M_15_29) 
  } else if(iso_3 %in% c('mli')) {
    # Estimate percent youth by using the national percent youth 
    # Then multiply by the gender breakdown at the subnational level
    mali_pop_age <- read_csv("data/Population_Provinces/Mali_Population_by_Age.csv") %>% 
      filter(Age != "Total") %>% 
      mutate(Youth = Age %in% c('15 - 19', '20 - 24', '25 - 29')) %>% 
      summarize(Percent_Youth = sum(ifelse(Youth, Total, 0)) / sum(Total)) %>% 
      unlist()
    # Maleka data - must be subtracted from Gao
    # Male: 35475
    # Female: 31644
    # Total: 67119
    mali_pop <- read_csv("data/Population_Provinces/mli_pop_adm1_v2.csv") %>% 
      mutate(M_TL = ifelse(admin1Name_fr == "Gao", M_TL - 35475, M_TL),
             T_TL = ifelse(admin1Name_fr == "Gao", T_TL - 67119, T_TL)) 
    Menaka <- data.frame(
      admin1Pcode = "ML10",
      M_TL = 35475,
      T_TL = 67119
    )
    mali_pop <- bind_rows(mali_pop, Menaka) %>% 
      mutate(P_M_15_29 = (M_TL / T_TL) * mali_pop_age) %>% 
      select(PCODE = admin1Pcode, T_TL, P_M_15_29)
    pop_list[[f]] <- mali_pop
  }
}

population <- bind_rows(pop_list)
population <- population %>% filter(!is.na(PCODE))

africa <- africa  %>%  left_join(population, by = c("adm1_src" = "PCODE"))
save(africa, file = "R_Data_Files/Africa_Wide_W_Population_Step2.RData")
