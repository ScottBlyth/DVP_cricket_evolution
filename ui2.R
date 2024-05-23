
library(shiny)
library(leaflet)
library(brochure)

brochureApp(
  page(
    hred="/",
  ui <- shinyUI(fluidPage(
    h1("Evolution: How has Cricket Changed?", align="center")
  )), 
  server <- function(input, output) {
    
  }), 
  page(
    href = "/page2", 
    ui =  fluidPage(
      h1("This is my second page"), 
      tags$p("There is no server function in this one")
    )
  )
  
)


