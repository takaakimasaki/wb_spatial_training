library(shiny)
library(sf)

# User interface ----
sf <- st_read("data-raw/sdr_subnational_boundaries2.shp")

# User interface ----
ui <- fluidPage(
  titlePanel("Tanzania Dashboard"),
  mainPanel(
    plotOutput("plot")
  )
)

# Server logic ----
server <- function(input, output) {
  output$plot = renderPlot({
  plot(st_geometry(sf), col="blue")
  })
}

# Run app ----
shinyApp(ui, server)