# to run each line, press the Ctrl+Enter key on windows (or use the Run toolbar button)
# or press command and return key on mac
# anything after # (hash) is a comment. They will be ignored when run.
##install and load packages
pacman::p_load(sf,raster, tidyverse, tmap, exactextractr)

################################################################################
#Basics: How to read/clean it
#read shapefile
sf <- st_read("data-raw/sdr_subnational_boundaries2.shp")

#let's see what's in this file
sf

#map it
tmap_mode("view")
tm_shape(sf) + tm_borders() 

#let's save csv file of the attribute table in the shapefile
sf %>% st_drop_geometry() %>% write.csv("data-raw/sdr_subnational_boundaries2.csv")

################################################################################
#how to merge data into sf
##read DHS data
dhs_data <- read_csv("data-raw/dhs_indicators.csv")

##merge
sf <- sf %>% left_join(., dhs_data, by="DHSREGEN")

##make sure that the data was successfully merged
view(sf)

################################################################################
#some examples of how to manipulate shapefile
##how to filter shapefile by characteristics
sf_zanzibar <- sf %>% 
  filter(DHSREGEN=="Kaskazini Pemba" | DHSREGEN=="Kaskazini Unguja" | DHSREGEN=="Kusini Pemba" | DHSREGEN=="Kusini Unguja" | DHSREGEN=="Mjini Magharibi")
tm_shape(sf_zanzibar) + tm_borders() 

##how to merge polygons by group
sf_zanzibar <- sf_zanzibar %>%
  mutate(zanzibar = 1)
sf_zanzibar_one <- sf_zanzibar %>% group_by(zanzibar) %>% summarize()
tm_shape(sf_zanzibar_one) + tm_borders() 

################################################################################
#Basics: How to match with another dataset
##crop and mask raster data

##load WorldPop population data for the entire country of Tanzania
pop <- raster("data-raw/tza_ppp_2020_constrained.tif")

##let's crop and mask WorldPop population data for Zanzibar only
pop_zanzibar <- pop %>% crop(., sf_zanzibar) %>% mask(., sf_zanzibar)

plot(st_geometry(sf))
plot(pop, add=TRUE)
plot(st_geometry(sf_zanzibar))
plot(pop_zanzibar, add=TRUE)

##line data: extract road network
road <- st_read(paste0("data-raw/tanzania_gis_osm_paved.shp")) %>% 
  filter(fclass=="primary" | fclass=="secondary")

road_zanzibar <- st_join(road, sf_zanzibar) %>% filter(!is.na(zanzibar))

plot(st_geometry(sf))
plot(st_geometry(road), add=TRUE, col="blue")
plot(st_geometry(sf_zanzibar))
plot(st_geometry(road_zanzibar), add=TRUE, col="blue")

##compute total population by region based on WorldPop raster data
pop_by_region <- exact_extract(pop, sf, fun = "sum") %>%
  as.data.frame() 

names(pop_by_region) <- c("pop1")
sf <- cbind(sf,pop_by_region)
mymap <- tm_shape(sf)+
  tm_fill("pop1",
            palette="OrRd",
            style="quantile",
            title=paste0("Population from WorldPop"),
            legend.reverse = TRUE) +
  tm_shape(sf) + tm_borders(lwd=0.1) +
  tmap_options(check.and.fix = TRUE)

mymap
tmap_save(tm = mymap,
          filename = paste0("maps/pop_by_region.png"))

##point data: cities
cities<-read.csv(paste0("data-raw/tz.csv")) %>% 
  st_as_sf(coords = c('lng', 'lat'),crs=st_crs(4326))

plot(st_geometry(sf))
plot(st_geometry(cities), add=TRUE, col="red")

##now let's compute distance between the centroids of each region and Dar.
dar <- cities %>% filter(admin_name == "Dar es Salaam") %>% st_transform(21037)

##now let's calculate distance to dar es salaam from each region
sf_cent <- sf %>% st_centroid() %>% st_transform(21037)

dist_all <- st_distance(sf_cent, dar) %>%
  as_tibble() %>%
  mutate(DHSREGEN = sf$DHSREGEN) %>%
  mutate(dist = as.numeric(value)/1000)

sf <- left_join(sf, dist_all , by="DHSREGEN")

tmap_mode("view")
mymap <- tm_shape(sf)+
  tm_fill("dist",
          palette="OrRd",
          style="quantile",
          title=paste0("Distance to Dar"),
          legend.reverse = TRUE) +
  tm_shape(sf) + tm_borders(lwd=0.1) +
  tm_shape(dar) + tm_dots()
mymap

tmap_save(tm = mymap,
          filename = paste0("maps/dist_to_dar.png"))

