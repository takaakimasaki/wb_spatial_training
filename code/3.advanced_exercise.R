# install packages --------------------------------------------------------
pacman::p_load(sf,tmap,tidyverse,sfheaders,smoothr,geosphere,exactextractr,
               raster,paletteer,leaflet)

# Spatial data clean-up procedure  ----------------------------------------
admin1<-st_read("data-raw/admin1.shp")
admin1_before<-tm_shape(admin1) + tm_borders() + tm_fill('green')
tmap_save(admin1_before,"maps/admin1_before.png")

# st_make_valid()
admin1_valid<- admin1 %>% 
  st_make_valid()
tm_shape(admin1_valid) + tm_borders() + tm_fill('green')

# sf_remove_holes()
admin1_hole_removed<-admin1_valid %>% 
  sf_remove_holes()
admin1_after<-tm_shape(admin1_hole_removed) + tm_borders() + tm_fill('green')
tmap_save(admin1_after,"maps/admin1_after.png")


# More advanced operation ------------------------------------------------
# compute distance to the closest city
cities<-read.csv(paste0("data-raw/tz.csv")) %>% 
  st_as_sf(coords = c('lng', 'lat'),crs=st_crs(4326))

sf <- st_read("data-raw/Kondoa EAs_EUTF/Kondoa_EAs.shp") %>% 
  st_make_valid()
tm_shape(cities)+tm_dots()+tm_shape(sf)+tm_borders()

# get centroid
sf_cent <- sf %>% st_centroid() %>% st_set_crs(4326)
tm_shape(sf_cent)+tm_dots()

# calculate distance
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

mymap <- tm_shape(sf)+
  tm_fill("dist",
          palette="OrRd",
          style="quantile",
          title=paste0("Distance to the closest city"),
          legend.reverse = TRUE) +
  tm_shape(sf) + tm_borders(lwd=0.1) +
  tm_shape(cities) + tm_dots()
tmap_options(check.and.fix = TRUE)
tmap_save(tm = mymap,
          filename = paste0("maps/dist_to_cities.png"))


# compute Rural accessibility index
road <- st_read(paste0("data-raw/tanzania_gis_osm_paved.shp")) %>% 
  filter(fclass=="primary" | fclass=="secondary") %>% 
  st_join(., sf) %>% filter(!is.na(OBJECTID))

##now create buffer
osm_2km <- road %>% 
  st_transform(3857) %>% 
  st_buffer(dist = 2000) %>% #2km radius
  st_union() %>% 
  st_transform(4326) %>% 
  st_make_valid()

#get urban extent for tanzania
urban <- st_read("data-raw/grump-v1-urban-ext-polygons-rev02-shp/global_urban_extent_polygons_v1.01.shp") %>% 
  dplyr::filter(ISO3=="TZA") %>% 
  st_make_valid()

##read population data
pop<-raster("data-raw/tza_ppp_2020_constrained.tif") %>% 
  projectRaster(crs="+init=EPSG:4326") %>% 
  crop(.,sf) 

tm_shape(pop)+tm_raster(palette = "viridis")+tm_shape(sf)+tm_borders()+tm_shape(osm_2km)+tm_fill("blue")

##urban population
comb_sf_urban <- st_intersection(sf, urban) %>% 
  st_collection_extract() %>% 
  st_make_valid()

##population within 2km away from all-season roads
comb_sf_osm_2km <- st_intersection(sf, osm_2km) %>% 
  st_collection_extract() %>% 
  st_make_valid()

##urban population within 2km away from all-season roads
comb_osm_2km_urban <- st_intersection(comb_sf_osm_2km, urban) %>% 
  st_collection_extract() %>% 
  st_make_valid()

tmap_mode("view")
tm_shape(sf)+tm_borders()+tm_shape(urban)+tm_polygons("red")
tm_shape(comb_sf_osm_2km)+tm_borders()
tm_shape(comb_osm_2km_urban)+tm_borders()

#ok now compute population in each of the layers being created above
#now compute zonal statistics of population
stats <- c('sum')
pop_sf<-sf %>% 
  mutate(pop_all=exact_extract(pop, .,stats))

pop_sf_urban<-comb_sf_urban %>% 
  mutate(pop_sf_urban=exact_extract(pop, .,stats)) %>% 
  group_by(OBJECTID) %>% summarize(pop_sf_urban=sum(pop_sf_urban))

pop_sf_osm_2km<-comb_sf_osm_2km %>% 
  mutate(pop_sf_osm_2km=exact_extract(pop, .,stats)) %>% 
  group_by(OBJECTID) %>% summarize(pop_sf_osm_2km=sum(pop_sf_osm_2km))

pop_osm_2km_urban <- comb_osm_2km_urban %>% 
  mutate(pop_osm_2km_urban=exact_extract(pop, .,stats)) %>%
  group_by(OBJECTID) %>% summarize(pop_osm_2km_urban=sum(pop_osm_2km_urban))

#ok now combine all the datasets
sf_df <- data.frame(pop_sf)
pop_sf_urban <- data.frame(pop_sf_urban)
pop_sf_osm_2km <- data.frame(pop_sf_osm_2km)
pop_osm_2km_urban <- data.frame(pop_osm_2km_urban)
sf_df <- merge(sf_df,pop_sf_urban,
                       by.x="OBJECTID",by.y="OBJECTID",
                       all.x=TRUE,all.y=TRUE)
sf_df <- merge(sf_df,pop_sf_osm_2km,
                       by.x="OBJECTID",by.y="OBJECTID",
                       all.x=TRUE,all.y=TRUE)
sf_df <- merge(sf_df,pop_osm_2km_urban,
                       by.x="OBJECTID",by.y="OBJECTID",
                       all.x=TRUE,all.y=TRUE)
sf_df <- sf_df[,c("OBJECTID","pop_all","pop_sf_urban","pop_sf_osm_2km","pop_osm_2km_urban")]
sf_df[is.na(sf_df)] <- 0

# calculate rural accessibility index
sf_df<-sf_df %>% 
  mutate(pop_rural=pop_all-pop_sf_urban,
         pop_urban=pop_sf_urban,
         pop_urban_s=(pop_sf_urban/pop_all)*100,
         pop_rural_osm=pop_sf_osm_2km-pop_osm_2km_urban,
         rai=(pop_rural_osm/pop_rural)*100,
         ##for those sfs where 100% pop is urban, we assume 100% rural access.
         rai=replace(rai, pop_urban_s==100, 100))
# merge with sf
sf_merged <- merge(sf,sf_df,
            by="OBJECTID",
            all.x=TRUE,all.y=FALSE)

# Visualization  ----------------------------------------------------------
rai_map<-tm_shape(sf_merged) + 
  tm_fill("rai",
          title="Rural Access Index",
          style="pretty", 
          legend.reverse = TRUE, 
          palette=paletteer_d("ggsci::purple_material")) +
  tm_borders(col="black", lwd=0, alpha = 0) +
  tm_shape(road) +
  tm_lines()+
  tm_compass(type="arrow",position = c("right","top"))+
  tm_scale_bar(position=c("left", "bottom"))+
  tm_credits("Data: Global Rural-Urban Mapping Project; WorldPop; OpenStreetMap \n Projection: EPSG:4326", 
             position=c("right", "bottom"))+
  tm_layout(inner.margins = 0.05,
            legend.outside=FALSE, 
            legend.position= c("left", "top"),
            legend.title.size=1,
            legend.text.size=1,
            main.title= '% of Rural Population within 2km from Road',  
            main.title.position = c('center'),
            title.size = 1)+
  tmap_style("white")

# save the map
tmap_save(rai_map,"maps/rural_accessibility_index.png")

# leaflet()
# setView
sf_centroid<-sf %>% 
  st_union() %>% 
  st_centroid() %>% 
  st_coordinates()
tmap_leaflet(rai_map) %>% 
  setView(sf_centroid[1], sf_centroid[2], zoom = 9)

# fitBounds()
sf_boundary<-st_bbox(sf)
tmap_leaflet(rai_map) %>% 
  fitBounds(as.numeric(sf_boundary[1]),
            as.numeric(sf_boundary[2]),
            as.numeric(sf_boundary[3]),
            as.numeric(sf_boundary[4]))

# clearBounds()
tmap_leaflet(rai_map) %>% 
  clearBounds()






