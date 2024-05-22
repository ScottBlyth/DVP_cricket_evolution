library(shiny)
library(leaflet)

# Define UI for miles per gallon application
shinyUI(fluidPage( 
  
  # Application title
  tags$head(
    tags$style(
      ".title {color: green}"
    )
  ),
  headerPanel(h1("Evolution: How has Cricket Changed?", class="title", align="center")),

  
  sidebarLayout(sidebarPanel(
    sliderInput("year", "Year", as.Date("2003", "%Y"),as.Date("2023", "%Y"), as.Date("1", "%Y"), round=TRUE, timeFormat="%Y"),
    checkboxInput("T20", "T20", value=TRUE),
    checkboxInput("ODI", "ODI", value=TRUE),
    checkboxInput("Test", "Test", value=TRUE),
    textInput("country", "Country", value="World")
    ,width=2),
    mainPanel(
      splitLayout(leafletOutput("gamesMap",width = "100%", height = 400),
                  plotOutput("country_plot", height="200px", width="100%")) 
        )
      ))
  
)

