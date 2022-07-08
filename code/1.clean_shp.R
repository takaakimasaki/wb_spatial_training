##install and load packages
package_list <- c("sf","raster","dplyr","tmap","exactextractr","geosphere","tidyverse")
new.packages <- package_list[!(package_list %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
for(p in package_list) {
  library(p, character.only=T)
}

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
sf_zanzibar_one <- sf_zanzibar %>% group_by(zanzibar) %>% summarize(zanzibar = mean(zanzibar), na.rm=T)
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

sf_cent <- sf %>% st_centroid() %>% st_set_crs(4326)

dist_all <- NULL
st_distance(sf_cent, cities) %>%
  as_tibble() %>%
  mutate(OBJECTID = sf$OBJECTID) %>%
  gather(v, value, -OBJECTID) %>%
  mutate(value = as.numeric(value)) %>%
  group_by(OBJECTID) %>%
  summarise(dist = min(value)) %>%
  mutate(keyword = 'dist_city') %>%
  bind_rows(dist_all, .) -> dist_all  

sf <- left_join(sf,dist_all , by="OBJECTID")

tmap_mode("view")
mymap <- tm_shape(sf)+
  tm_fill("dist",
          palette="OrRd",
          style="quantile",
          title=paste0("Distance to the closest city"),
          legend.reverse = TRUE) +
  tm_shape(sf) + tm_borders(lwd=0.1) +
  tm_shape(cities) + tm_dots()
  tmap_options(check.and.fix = TRUE)
mymap

tmap_save(tm = mymap,
          filename = paste0("maps/pop_by_region.png"))



