 
# Get ACLED Data in df_sf data frame
load("acled_cleaned.RData")
load("R_Data_Files/Africa_Wide_W_Population_Step2.RData")

africa$veo_events <- lengths(st_intersects(africa, st_geometry(df_sf)))

# Get the provinces that border areas with VEO activity
adj_mat <- poly2adjmat(africa)

africa$id <- 1:nrow(africa)

africa <- africa %>% mutate(VEO = veo_events > 0)

g <- graph.adjacency(adj_mat)
edge_list <- get.edgelist(g)

edge_list_int <- apply(edge_list, 2, FUN = as.integer)
edge_list_df <- edge_list_int %>% as.data.frame()

edge_list_df <- edge_list_df %>% 
  inner_join(as.data.frame(africa) %>% select(id, VEO1 = VEO), by = c("V1" = "id")) %>% 
  inner_join(as.data.frame(africa) %>% select(id, VEO2 = VEO), by = c("V2" = "id"))

violence_neighborhoods <- bind_rows(
  edge_list_df %>% group_by(V1) %>% summarize(Neighboring_Violence = any(VEO1) | any(VEO2)) %>% rename(id = V1),
  edge_list_df %>% group_by(V2) %>% summarize(Neighboring_Violence = any(VEO1) | any(VEO2)) %>% rename(id = V2)
) %>% 
  group_by(id) %>% 
  summarize(Neighboring_Violence = any(Neighboring_Violence))

africa <- africa %>% 
  left_join(violence_neighborhoods) 
  
  # Save full data frame - so remove filters - need to check why those IDs were removed 
  # filter(Neighboring_Violence) %>%
  # filter(!(id %in% c(83, 84, 26, 55, 49, 45)))
save(africa, file = "R_Data_Files/Africa_Wide_VEO_Step_3.RData")



