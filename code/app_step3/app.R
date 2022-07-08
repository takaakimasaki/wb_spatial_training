pacman::p_load(sf,raster,dplyr,tmap,exactextractr,geosphere,
  paletteer,shiny,here,DT,spdplyr,mapview,shinythemes,leaflet, tidyr)

# load datasets
kondoa_ea_sf <- st_read(here("data-raw/Kondoa EAs_EUTF/Kondoa_EAs.shp")) %>% 
  st_make_valid()
kondoa_ea_sf_df<-st_drop_geometry(kondoa_ea_sf) %>% # drop geometry
  gather(.,indicator,value,4:ncol(.)) %>% # wide to long format
  mutate(value=as.numeric(value)) %>% # convert value to numeric
  filter(!is.na(value)) # filter out value that are NA (which used to be character)


# ui ----------------------------------------------------------------------
ui <- navbarPage(
  # set a theme (try out the following themes too: cosmo, cyborg, darkly, etc)
  # check the list here: https://shiny.rstudio.com/gallery/shiny-theme-selector.html
  theme = shinytheme("cerulean"),
  
  # top title + some tags
  title = "Spatial app",
  
  # tab for map
  tabPanel(title="Map",
   # header
   wellPanel(titlePanel(p("This app is step 3!", 
                          style = "color:#3474A7")), # set the color (e.g., red, blue, etc)
            tags$p("This is an example of a simple shiny app"),
            tags$img(height=75,width=150,src="shiny-og-fb.png")),
   
   sidebarLayout(
     # sidebar
     sidebarPanel(
       # choose variable from column names
       selectInput(
         inputId = "selected_variable",
         label = "Select variable",
         choices = unique(kondoa_ea_sf_df$indicator) # use the unique set of indicator as choices
       )
     ),
     
     # main
     mainPanel(
       leafletOutput(outputId = "map")
       )
     )
  ),
  
  # tab for table
  tabPanel(title="Table",
    # show data table
    DT::dataTableOutput("table")
    )
)


# server ------------------------------------------------------------------
server <- function(input, output) {
  
  # create a reactive expression that filters kondoa_ea_sf_df based on the selected variable
  kondoa_ea_sf_df_filtered <- reactive({
    kondoa_ea_sf_df %>%
      filter(indicator==input$selected_variable)
  })
  
  # map
  map<-reactive({
    # create palette
    pal <- colorBin(as.character(paletteer_d("rcartocolor::ag_Sunset")),
      domain = kondoa_ea_sf_df_filtered()$value,
      bins = 4)
    
    # set labels
    labels <- sprintf("%s: %g", kondoa_ea_sf_df_filtered()$Kitongoji, kondoa_ea_sf_df_filtered()$value) %>%
      lapply(htmltools::HTML)
    
    leaflet(kondoa_ea_sf) %>%
      addTiles() %>%
      addPolygons(
        fillColor = ~ pal(kondoa_ea_sf_df_filtered()$value),
        color = "white", # set the border color (e.g., black, blue, etc)
        dashArray = "3", # set the dash of the border (e.g., 1,2,3, etc)
        weight = 1, # set the thickness of the border (e.g., 1,2,3, etc)
        fillOpacity = 0.7, # set the transparency of the border (range: 0-1)
        label = labels) %>%
      leaflet::addLegend(
        pal = pal, 
        values = ~kondoa_ea_sf_df_filtered()$value,
        opacity = 0.7, # set the transparency of the legend (range: 0-1)
        title = input$selected_variable)
    })
  
  # create map
  output$map <- renderLeaflet({
    map()
  })
  
  # data table
  output$table <- DT::renderDataTable(kondoa_ea_sf_df)
}

# shinyApp()
shinyApp(ui = ui, server = server)