
package_list <- c("sf","raster","dplyr","tmap","exactextractr","geosphere")
new.packages <- package_list[!(package_list %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
for(p in package_list) {
  library(p, character.only=T)
}

################################################################################
#Basics: How to read/clean it
#read shapefile
sf <- st_read("data-raw/Kondoa EAs_EUTF/Kondoa_EAs.shp")

#check if the shapefile is valid
sf::st_is_valid(sf)

#this shapefile is not valid. To make it valid:
sf <- st_make_valid(sf)

#map it
tmap_mode("view")
tm_shape(sf) + tm_borders() + tmap_options(check.and.fix = TRUE) 

################################################################################
#how to filter shapefile by characteristics
sf_atta <- sf %>% filter(Kijiji_Mta=="Atta")
tm_shape(sf) + tm_borders() + tm_shape(sf_atta) + tm_fill(col="red") + tmap_options(check.and.fix = TRUE) 

#how to merge polygons by group
Kijiji_Mta <- sf %>% group_by(Kijiji_Mta) %>% summarize(Kijiji_M_1 = mean(Kijiji_M_1), na.rm=T)
tm_shape(Kijiji_Mta) + tm_borders() + tmap_options(check.and.fix = TRUE) 

################################################################################
#Basics: How to match with another dataset
##crop and mask raster data
pop <- raster("data-raw/tza_ppp_2020_constrained.tif")
pop <- pop %>% crop(., sf) %>% mask(., sf)

tmap_mode("plot")
mymap <- tm_shape(pop, bbox=extent(sf))+
  tm_raster(labels(pop),
            palette="OrRd",
            style="quantile",
            title=paste0("Population from WorldPop"),
            legend.reverse = TRUE) +
  tm_shape(Kijiji_Mta) + tm_borders() +
  tmap_options(check.and.fix = TRUE)

tmap_save(tm = mymap,
          filename = paste0("maps/pop.png"))

##extract road network in Kondoa
road <- st_read(paste0("data-raw/tanzania_gis_osm_paved.shp")) %>% filter(fclass=="primary" | fclass=="secondary")
road_kondoa <- st_join(road, sf) %>% filter(!is.na(OBJECTID))
mymap <- tm_shape(pop, bbox=extent(sf))+
                   tm_raster(labels(pop),
                   palette="OrRd",
                   style="quantile",
                   title=paste0("Population from WorldPop"),
                   legend.reverse = TRUE) +
  tm_shape(road_kondoa) + tm_lines(col="blue", lwd=2, title.lwd = "Road network") +
  tm_shape(Kijiji_Mta) + tm_borders() +
  tmap_options(check.and.fix = TRUE)

tmap_save(tm = mymap,
          filename = paste0("maps/road.png"))

##compute total population by EA based on WorldPop raster data
pop_by_ea <- exact_extract(pop,sf, fun = "sum") %>% as.data.frame() 
names(pop_by_ea) <- c("pop1")
sf <- cbind(sf,pop_by_ea)
mymap <- tm_shape(sf)+
  tm_fill("pop1",
            palette="OrRd",
            style="quantile",
            title=paste0("Population from WorldPop"),
            legend.reverse = TRUE) +
  tm_shape(sf) + tm_borders(lwd=0.1) +
  tmap_options(check.and.fix = TRUE)
tmap_save(tm = mymap,
          filename = paste0("maps/pop_by_ea.png"))


##compute distance to the closest city
cities<-read.csv(paste0("data-raw/tz.csv")) %>% 
  st_as_sf(coords = c('lng', 'lat'),crs=st_crs(4326))

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
          filename = paste0("maps/pop_by_ea.png"))



