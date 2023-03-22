# ACLED get the various data clusters 

library(rgeoda)
library(sf)
library(geodaData)
library(glue)
library(tidyverse)
library(mapview)
library(dbscan)


setwd("~/Documents/AFRICOM/acled/")

load(file = "acled_cleaned.RData")
load(file = "rmd_data/africa_veo_map.RData")
load(file = "data/grid_violence_monthly_data.rData")

df_sf %>% dplyr::sample_n(20000) %>% mapView()

coordinates <- st_coordinates(df_sf)
plot(coordinates)

kmean_withinss <- function(k) {
  cluster <- kmeans(coordinates, k)
  return(cluster$tot.withinss)
}
max_k <- 20 
wss <- sapply(2:max_k, kmean_withinss)
elbow <- data.frame(2:max_k, wss)
ggplot(elbow, aes(x = X2.max_k, y = wss)) +
  geom_point() + 
  geom_line() + 
  scale_x_continuous(breaks = seq(1, 20, by = 1))

coordinates_df <- as.data.frame(coordinates)

# kmeans_result <- hclust(coordinates, centers = 5, nstart = 3)
# coordinates_df$cluster <- kmeans_result$cluster

ggplot(coordinates_df, aes(x = X, y = Y, color = as.factor(cluster))) + geom_point()

optics_output <- dbscan::optics(coordinates_df, eps = 20, minPts = 7)
plot(optics_output)
#plot(coordinates_df, col = "grey")
#polygon(coordinates_df[optics_output$order, ])
res <- extractDBSCAN(optics_output, eps_cl = 2)
hullplot(coordinates_df,res)

hullplot

st_convex_hull(boro_union) 

coordinates_df$cluster <- optics_output$cluster

ggplot(coordinates_df, aes(x = X, y = Y, color = as.factor(cluster))) + geom_point()


res <- extractDBSCAN(optics_output, eps_cl = 1.7)
hullplot(coordinates_df,res)


plot(coordinates_df, col = "grey")
polygon(coordinates_df[optics_output$order, ])

df_sf_clusters <- df_sf %>% mutate(Cluster = res$cluster)
df_sf_clusters %>% st_convex_hull() 

hullslist <- list()
for(i in unique(df_sf_clusters$Cluster)){
  hulls <- st_convex_hull(st_union(df_sf_clusters %>% filter(Cluster == i) )) %>% 
    st_sf()
  hullslist[[length(hullslist) + 1]] <- hulls
}
test <- bind_rows(hullslist)
mapView(test)
st_convex_hull(st_union(df_sf_clusters %>% filter(Cluster == i) )) %>% 
st_sf()

# Split into clusters 
Somalia
ap <- available.packages()
ap["dbscan", "Depends"]






