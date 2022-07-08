pacman::p_load(sf,tidyverse,s2)

tanzania <- st_read("data-raw/tzvillage/tzvillage.shp") %>% 
  st_transform(4326) %>% 
  st_make_valid() %>% 
  {
    # add Songwe to admin1 level
    data <-.
    admin1<-st_read("data-raw/tza_admbnda_adm1_20181019/tza_admbnda_adm1_20181019.shp") %>% 
      st_transform(4326) %>% 
      sf::st_make_valid()
    data <- data %>% 
      st_join(.,admin1,largest=TRUE)
    data
  } %>% 
  # create unique Pcod for each admin level by grouping admin names
  group_by(ADM1_EN) %>% 
  mutate(id=sprintf("%03d",id = cur_group_id()),
         admin1Pcod=paste0(ADM0_PCODE,id)) %>% 
  ungroup() %>% 
  {
    # create unique Pcod for each unique combination of admin1Pcod & District_N
    data<-.
    data_admin2<-data %>% 
      group_by(admin1Pcod,District_N) %>% 
      summarize() %>% 
      ungroup() %>% 
      group_by(admin1Pcod) %>% 
      mutate(id=sprintf("%03d",row_number()),
             admin2Pcod=paste0(admin1Pcod,id)) %>% 
      st_drop_geometry()
    # left join back to the orignal data
    data<-data %>%
      left_join(.,data_admin2[,c("District_N","admin1Pcod","admin2Pcod")],
                by=c("District_N","admin1Pcod"))
    
    # create unique Pcod for each unique combination of Ward_Name & admin2Pcod
    data_admin3<-data %>%
      group_by(admin2Pcod,Ward_Name) %>%
      summarize() %>%
      ungroup() %>%
      group_by(admin2Pcod) %>%
      mutate(id=sprintf("%03d",row_number()),
             admin3Pcod=paste0(admin2Pcod,id))%>%
      st_drop_geometry()
    # left join back to the orignal data
    data<-data %>%
      left_join(.,data_admin3[,c("Ward_Name","admin2Pcod","admin3Pcod")],
                by=c("Ward_Name","admin2Pcod"))
    
    # create unique Pcod for each unique combination of Vil_Mtaa_N & admin3Pcod
    data_admin4<-data %>%
      group_by(admin3Pcod,Vil_Mtaa_N) %>%
      summarize() %>%
      ungroup() %>%
      group_by(admin3Pcod) %>%
      mutate(id=sprintf("%03d",row_number()),
             admin4Pcod=paste0(admin3Pcod,id))%>%
      st_drop_geometry()
    # left join back to the orignal data
    data<-data %>%
      left_join(.,data_admin4[,c("Vil_Mtaa_N","admin3Pcod","admin4Pcod")],
                by=c("Vil_Mtaa_N","admin3Pcod")) %>% 
      as.data.frame() %>% 
      st_as_sf()
    data
  } %>% 
  # create necessary columns
  mutate(admin0Pcod="TZ",
         admin0Name="Tanzania",
         admin1Name=ADM1_EN,
         admin2Name=str_c(District_N, " (", ADM1_EN, ")"),
         admin3Name=str_c(Ward_Name, " (", District_N, ", ", ADM1_EN, ")"),
         admin4Name=str_c(Vil_Mtaa_N, " (", Ward_Name, ", ", District_N, ", ", ADM1_EN,  ")"),
         area=as.numeric(st_area(geometry))/1000000
  ) %>% 
  arrange(admin1Pcod, admin2Pcod, admin3Pcod, admin4Pcod) %>% 
  dplyr::select(contains(c("admin0","admin1","admin2","admin3","admin4")), area)  %>%
  tidyr::drop_na(tidyr::contains(c("Pcod","Name"))) %>% 
  st_cast("MULTIPOLYGON")

# admin1 ------------------------------------------------------------------
adm1 <-
  tanzania %>%
  dplyr::group_by_at(vars(tidyr::contains("admin1"), tidyr::contains("admin0"))) %>%
  {
    # clean s2 geometry and join back to the sf object
    data<-.
    clean_geometry<-data %>% 
      st_as_s2(check=FALSE) %>%
      s2_rebuild(s2_options(split_crossing_edges = TRUE)) %>%
      st_as_sf()
    data<-data %>%
      st_drop_geometry() %>% 
      cbind(.,clean_geometry) %>% 
      st_as_sf()%>% 
      st_make_valid()
    data
  } %>% 
  summarise(area  = sum(area, na.rm = TRUE)) %>%
  ungroup() %>%
  group_by(admin1Pcod) %>%
  mutate(n = n()) %>%
  ungroup() %>%
  mutate(agg_level="admin1",agg_title="Regions",year="") %>% 
  dplyr::select(-n) %>%
  dplyr::select(-contains("admin0")) %>%
  dplyr::select(agg_level,agg_title,admin1Pcod,admin1Name,area,year) %>%
  distinct(admin1Pcod, .keep_all = TRUE) %>% 
  st_cast("MULTIPOLYGON") %>% 
  st_write(dsn="data-raw",layer="admin1",driver="ESRI Shapefile", append=FALSE)