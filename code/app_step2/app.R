pacman::p_load(sf,raster,dplyr,tmap,exactextractr,geosphere,
               paletteer,shiny,here,DT,spdplyr,mapview,shinythemes,leaflet,tidyr)


# load datasets
sf <- st_read("data-raw/sdr_subnational_boundaries2.shp")
##read DHS data
dhs_data <- read_csv("data-raw/dhs_indicators.csv")

##merge
sf <- sf %>% left_join(., dhs_data, by="DHSREGEN")
sf_df <-st_drop_geometry(sf) %>% # drop geometry
  gather(.,indicator,value,28:ncol(.)) %>% # wide to long format
  mutate(value=as.numeric(value)) %>% # convert value to numeric
  filter(!is.na(value)) # filter out value that are NA (which used to be character)

# ui ----------------------------------------------------------------------
ui <-fluidPage(
  # Application title
  titlePanel("This app is step 2!"),
  
  # use sidebar layout (sidebar panel + main panel)
  sidebarLayout(
    # sidebar
    sidebarPanel(
      # choose variable from column names
      selectInput(
        inputId = "selected_variable",
        label = "Select variable",
        choices = unique(sf_df$indicator)
      )
    ),
    
    # main
    mainPanel(
      leafletOutput(outputId = "map")
    )
  )
)


# server ------------------------------------------------------------------
server <- function(input, output) {
  
  # create a reactive expression that filters sf_df based on the selected variable
  sf_df_filtered <- reactive({
    sf_df %>%
      filter(indicator==input$selected_variable)
    })

  # map
  map<-reactive({
    # create palette
    pal <- colorQuantile(as.character(paletteer_d("rcartocolor::ag_Sunset")),
      domain = sf_df_filtered()$value,
      n = 4)

    # set labels
    labels <- sprintf("%s: %g", sf_df_filtered()$DHSREGEN, sf_df_filtered()$value) %>%
      lapply(htmltools::HTML)
    
    leaflet(sf) %>%
      addTiles() %>%
      addPolygons(
        fillColor = ~ pal(sf_df_filtered()$value),
        color = "white", # set the border color (e.g., black, blue, etc)
        dashArray = "3", # set the dash of the border (e.g., 1,2,3, etc)
        weight = 1, # set the thickness of the border (e.g., 1,2,3, etc)
        fillOpacity = 0.7, # set the transparency of the border (range: 0-1)
        label = labels) %>%
      leaflet::addLegend(
        pal = pal,
        values = ~sf_df_filtered()$value,
        opacity = 0.7, # set the transparency of the legend (range: 0-1)
        title = input$selected_variable)
  })
  
  # create map
  output$map <- renderLeaflet({
    map()
  })
}

# shinyApp()
shinyApp(ui = ui, server = server)