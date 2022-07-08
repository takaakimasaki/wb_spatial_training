# How to install packages -------------------------------------------------
# Let's install pacman first
if(!require(pacman)){
  install.packages("pacman")
  library(pacman)
}

# use pacman::p_load to load packages in one line
pacman::p_load(sf,raster,
               tidyverse,tmap,
               exactextractr,geosphere,
               GISTools,ggplot2,spdplyr)

# How to read geospatial data  --------------------------------------------
# shapefile
sf <- st_read("data-raw/Kondoa EAs_EUTF/Kondoa_EAs.shp")

# check if the shapefile is valid
sf::st_is_valid(sf)

# this shapefile is not valid. To make it valid:
sf <- st_make_valid(sf)

# visually check the shape
tmap_mode("view")
tm_shape(sf) + tm_borders() + tmap_options(check.and.fix = TRUE) 

# Basics on tidyverse  ----------------------------------------
# To see what’s in the attribute table 
head(sf) 

# To report the structure of the data set
str(sf)

# To report summary statistics 
summary(sf) 

# select variables
# this symbol (%>% ) is called pipe, which can pass the value to line
sf %>% dplyr::select(c(OBJECTID, Kitongoji, Estimated_))

# filter variables
sf %>% filter(Kijiji_Mta=="Atta") %>% 
  filter((Kitongoji=="Atta kwa Mariri")| # | means OR
         (Kitongoji=="Atta Shuleni")& # & means AND
         (Estimated_>50))

# Add new variables
sf %>% mutate(Kitongoji_upper_case=toupper(Kitongoji)) # change to uppercase

# Orders the rows of a data frame by the values of selected columns 
sf %>% arrange(EA_Na,Kitongoji) # first column gets the priority in sorting

# Group data 
sf %>% group_by(Kijiji_Mta) %>% 
  summarise(mean=mean(Estimated_))

# Basic on sf -------------------------------------------------------------
# Create, get, set or replace the coordinate reference system (CRS)
st_crs(sf) 

# Transform coordinates of object to new projection. 
# change it to pseudo-mercator 
# use local projection (e.g., 21037) for more accurate calculation
sf_3857<-st_transform(sf, 3857) %>% 
  mutate(area_km=st_area(geometry)/1000000)
tm_shape(sf_3857) + tm_polygons("area_km",style="quantile")

# Merge spatial datasets 
road <- st_read(paste0("data-raw/tanzania_gis_osm_paved.shp")) %>% 
  filter(fclass=="primary" | fclass=="secondary") %>% 
  st_transform(3857) 
road_kondoa <- st_join(road, sf_3857) %>% filter(!is.na(OBJECTID))
tm_shape(sf) + tm_borders(lwd=0.5) + tm_shape(road_kondoa) + tm_lines(col="Wilaya",palette = "viridis",lwd = 2) + tmap_options(check.and.fix = TRUE) 

# Compute a buffer around this geometry/each geometry 
road_kondoa_2km_buffer<-st_buffer(road_kondoa,2000) %>% 
  st_union() %>%  # Combine geometries without resolving borders 
  st_transform(4326) %>% 
  st_make_valid()
tm_shape(road_kondoa_2km_buffer) + tm_polygons(col="red") + tm_shape(sf) + tm_borders() + tmap_options(check.and.fix = TRUE) 

# intersection 
sf_road_kondoa_2km_buffer<-st_intersection(sf,road_kondoa_2km_buffer)
tm_shape(sf_road_kondoa_2km_buffer) + tm_borders() + tmap_options(check.and.fix = TRUE) 

# Save a geometry as a new file 
st_write(sf_road_kondoa_2km_buffer,
         dsn="data-raw",
         layer="sf_road_kondoa_2km_buffer.shp",
         driver = "ESRI Shapefile",
         append=FALSE)


# Basic on raster ---------------------------------------------------------
pop<-raster("data-raw/tza_ppp_2020_constrained.tif")

# Return a geographic subset of an object as specified by an Extent object 
pop_cropped<-crop(pop,sf)
tm_shape(pop_cropped) + tm_raster(labels(pop_cropped),palette = "viridis") + tm_shape(sf) + tm_borders(lwd = 0.5) + tmap_options(check.and.fix = TRUE) 

# Create a new raster object that has the same values 
#except for the cells that are NA in a mask object.  
pop_masked<-mask(pop_cropped,sf)
tm_shape(pop_masked) + tm_raster(labels(pop_masked),palette = "viridis") + tm_shape(sf) + tm_borders(lwd = 0.5) + tmap_options(check.and.fix = TRUE) 

# Calculate values for a new Raster* object 
# from another Raster* object, using a formula. 
pop_mean<-cellStats(pop_masked,"mean") # calculate mean
pop_sd<-cellStats(pop_masked,"sd") # calculate standard deviation
pop_z_score<-calc(pop_masked,fun=function(x){(x-pop_mean)/pop_sd}) # calculate z score
tm_shape(pop_z_score) + tm_raster(labels(pop_z_score),palette = "viridis") + tm_shape(sf) + tm_borders(lwd = 0.5) + tmap_options(check.and.fix = TRUE) 

# reclassify raster values
pop_quantile<-quantile(pop_masked)
m <- c(pop_quantile[1], pop_quantile[2], 25,  
       pop_quantile[2], pop_quantile[3], 50,  
       pop_quantile[3], pop_quantile[4], 75,
       pop_quantile[4], pop_quantile[5], 100)
rclmat <- matrix(m, ncol=3, byrow=TRUE)
pop_reclassified <- reclassify(pop_masked,rclmat,include.lowest=TRUE)
pop_reclassified <- ratify(pop_reclassified) # add raster attribute table
rat <- levels(pop_reclassified)[[1]]
rat$percentile <- c('0-25%', '25-50%', '50-75%','75-100%')
rat$code <- c(25,50,75,100)
levels(pop_reclassified) <- rat
tm_shape(pop_reclassified) + tm_raster(labels(pop_reclassified),palette = "viridis") + tm_shape(sf) + tm_borders(lwd = 0.5) + tmap_options(check.and.fix = TRUE) 

# save raster
writeRaster(pop_reclassified,"data-raw/pop_reclassified.tif")


# How to compute zonal statistics  ----------------------------------------
# Compute zonal statistics from raster
sf_pop<-sf %>% 
  mutate(pop=exact_extract(pop, .,"sum"))
tm_shape(sf_pop) + tm_borders(lwd = 0.5) + tm_fill("pop",style="quantile") + tmap_options(check.and.fix = TRUE) 

# Count number of points  
primary_school<-st_read("data-raw/primary_schools_2019.shp") %>% 
  st_transform(4326) %>% 
  as_Spatial()
sf_primary_school<-sf_pop %>% 
  st_transform(4326) %>% 
  as_Spatial() %>% 
  mutate(school_num=poly.counts(primary_school,.),
         schoo_per_1000=school_num/pop*1000) %>% 
  st_as_sf()
tm_shape(sf) + tm_borders() + tm_shape(primary_school) + tm_dots(col='blue') + tmap_options(check.and.fix = TRUE) 
tm_shape(sf_primary_school) + tm_borders() + tm_fill("schoo_per_1000") + tmap_options(check.and.fix = TRUE) 


# How to visualize  -------------------------------------------------------
# plot()
png('maps/pop_plot.png')
plot(sf_pop['pop'])
dev.off()

# ggplot() + geom_sf() 
sf_pop %>% 
  ggplot()+
  geom_sf(aes(fill = pop))
ggsave("maps/pop_ggplot.png")

# qtm() 
qtm<-qtm(sf_pop,"pop")
tmap_save(qtm,"maps/pop_qtm.png")

# tm_shape() + tm_ploygons() 
tmap_polygon<-tm_shape(sf_pop) + tm_polygons("pop") 
tmap_save(tmap_polygon,"maps/pop_tmap_polygon.png")

# tm_shape() + tm_symbols() 
tmap_symbol<-tm_shape(sf_pop) + tm_symbols("pop") 
tmap_save(tmap_symbol,"maps/pop_tmap_symbol.png")
