package_list <- c("sf","raster","dplyr","tmap","exactextractr","geosphere",
                  "paletteer","shiny","here","DT","spdplyr","mapview","shinythemes",
                  "leaflet")
new.packages <- package_list[!(package_list %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
for(p in package_list) {
  library(p, character.only=T)
}

# load datasets
sf <- st_read(here("data-raw/Kondoa EAs_EUTF/Kondoa_EAs.shp")) %>% 
  st_make_valid()
sf_df<-st_drop_geometry(sf) %>% 
  select_if(.,is.numeric)
colnames<-colnames(sf_df)


# ui ----------------------------------------------------------------------
ui <- navbarPage(
  theme = shinytheme("cerulean"),
  # top title + some tags
  title = "Spatial app",
  
  # tab
  tabPanel(title="map & histogram",
           # header
           wellPanel( titlePanel(p("Spatial app", style = "color:#3474A7")),
                      tags$p("This is an example of a simple shiny app"),
                      tags$img(height=75,width=150,src="shiny-og-fb.png")),
           
           sidebarLayout(
             # sidebar
             sidebarPanel(
               # choose variable from column names
               selectInput(
                 inputId = "variableselected",
                 label = "Select variable",
                 choices = colnames,
                 selected = colnames[1]
               ),
               # choose the number of breaks
               numericInput(
                 inputId = "number_palette",
                 label = "Select the number of palettes",
                 value=5,
                 min=2,
                 max=10
               ),
               # Button to download data as csv
               downloadButton("csv", "Download csv"),
               # add some space between buttons
               tags$div(style="margin-bottom:10px"),
               # Button to download data as shapefile
               downloadButton("shapefile", 
                              "Download shapefile")
             ),
             
             # main
             mainPanel(
               leafletOutput(outputId = "map"),
               plotOutput(outputId = "hist")
               )
             )
           )
)


# server ------------------------------------------------------------------
server <- function(input, output) {
  
  # set reactive value
  sf_reactive<-reactive({
    value<-st_drop_geometry(sf[input$variableselected]) %>% 
      as.vector()
    sf["variableplot"]<-value
    sf
    })
  
  # map
  map<-reactive({
    # create palette
    number_palette<-reactive({as.character(input$number_palette)}) 
    pal <- colorBin(as.character(paletteer_d("rcartocolor::ag_Sunset")),
                    domain = sf_reactive()$variableplot,
                    bins = as.numeric(number_palette()))
    
    # set labels
    labels <- sprintf("%s: %g", sf_reactive()$Kitongoji, sf_reactive()$variableplot) %>%
      lapply(htmltools::HTML)
    
    leaflet(sf_reactive()) %>%
      addTiles() %>%
      addPolygons(
        fillColor = ~ pal(variableplot),
        color = "white",
        dashArray = "3",
        weight = 1,
        fillOpacity = 0.7,
        label = labels
      ) %>%
      leaflet::addLegend(
        pal = pal, values = ~variableplot,
        opacity = 0.7, title = input$variableselected
        )
    })
  
  # histogram
  histogram<-reactive({
    hist(sf_reactive()$variableplot, col = 'darkgray', border = 'white',
         main=paste0("Histogram of ",input$variableselected),
         xlab=input$variableselected)
  })
  
  # create histogram
  output$hist <- renderPlot({
    histogram()
  })

  # create map
  output$map <- renderLeaflet({
    map()
  })
  
  # Downloadable csv of selected dataset
  output$csv <- downloadHandler(
    filename =paste0("downloaded_data.csv"),
    content = function(file) {
      write.csv(st_drop_geometry(sf), file, row.names = FALSE)
    }
  )
  
  output$shapefile <- downloadHandler(
    filename = "downloaded_shape.zip",
    content = function(file){
      if(length(Sys.glob("downloaded_shape.*"))>0){
        file.remove(Sys.glob("downloaded_shape.*"))
      }
      st_write(sf, dsn = "downloaded_shape.shp", layer= "downloaded_shape" ,driver= "ESRI Shapefile", overwrite_layer = T)
      zip(zipfile = 'downloadedshape.zip', files= Sys.glob("downloaded_shape.*"))
      file.copy("downloadedshape.zip", file)
      if(length(Sys.glob("downloaded_shape.*"))>0){
        file.remove(Sys.glob("downloaded_shape.*"))
      }
    }
  )
}

# shinyApp()
shinyApp(ui = ui, server = server)