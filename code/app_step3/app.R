##this code is pretty much the same as app_step2/app.R but now data are embedded in the same folder as app.R.
##now launch this app from code/2.dashboard.R
##shiny::runApp(here::here("code/app_step3"))
##and then after launching the app, you can deploy or publish by clicking on the "Publish" icon at the top right corner. 
pacman::p_load(sf,tidyverse,paletteer,shiny,here,leaflet,htmltools)

# load datasets
sf <- st_read("data/sdr_subnational_boundaries2.shp")
##read DHS data
dhs_data <- read_csv("data/dhs_indicators.csv")

## transform data
dhs_data <-dhs_data %>% # drop geometry
  gather(.,indicator,value,2:ncol(.)) %>% # wide to long format
  mutate(value=as.numeric(value)) %>% # convert value to numeric
  filter(!is.na(value)) # filter out value that are NA (which used to be character)
# ui ----------------------------------------------------------------------
ui <-fluidPage(
  # Application title
  titlePanel("This app is step 3!"),
  
  # use sidebar layout (sidebar panel + main panel)
  sidebarLayout(
    # sidebar
    sidebarPanel(
      # choose variable from column names
      selectInput(
        inputId = "selected_variable",
        label = "Select variable",
        choices = unique(dhs_data$indicator)
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
  
  # create a reactive expression that filters dhs_data based on the selected variable
  dhs_data_filtered <- reactive({
    dhs_data %>%
      filter(indicator==input$selected_variable)
    })

  # map
  map<-reactive({
    # create palette
    pal <- colorQuantile(as.character(paletteer_d("rcartocolor::ag_Sunset")),
      domain = dhs_data_filtered()$value,
      n = 4)

    # set labels
    labels <- sprintf("%s: %g", dhs_data_filtered()$DHSREGEN, dhs_data_filtered()$value) %>%
      lapply(htmltools::HTML)
    
    leaflet(sf) %>%
      addTiles() %>%
      addPolygons(
        fillColor = ~ pal(dhs_data_filtered()$value),
        color = "white", # set the border color (e.g., black, blue, etc)
        dashArray = "3", # set the dash of the border (e.g., 1,2,3, etc)
        weight = 1, # set the thickness of the border (e.g., 1,2,3, etc)
        fillOpacity = 0.7, # set the transparency of the border (range: 0-1)
        label = labels) %>%
      leaflet::addLegend(
        pal = pal,
        values = ~dhs_data_filtered()$value,
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