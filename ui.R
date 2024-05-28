library(shiny)
library(shinydashboard)
library(leaflet)

# Define UI for miles per gallon application
dashboardPage( 
  dashboardHeader(), 
  dashboardSidebar(), 
  dashboardBody(fluidPage(
  tags$head(
    tags$style(
      ".title {color: green; font-size: 40px;}", 
      ".subtitle {color: black; font-size: 20px; align: center;}",
      ".info {color: black; font-size: 16px}",
      ".legend {color: blue; font-size: 16px;}"
    )
  ),
  # Application title
  headerPanel(h1("Evolution: How has Cricket Changed?", class="title", align="center")),

  
  sidebarLayout(sidebarPanel(
    sliderInput("year", "Year", as.Date("2003", "%Y"),as.Date("2023", "%Y"), as.Date("1", "%Y"), round=TRUE, timeFormat="%Y"),
    checkboxInput("T20", "T20", value=TRUE),
    checkboxInput("ODI", "ODI", value=TRUE),
    checkboxInput("Test", "Test", value=TRUE),
    checkboxInput("male", "Male", value=TRUE),
    checkboxInput("female", "Female", value=TRUE),
    textInput("country", "Country", value="World")
    ,width=2),
    mainPanel(
      h1("What Countries Play Cricket?", class="subtitle", align="center"),
      splitLayout(leafletOutput("gamesMap",width = "100%", height = 300),
                  plotOutput("country_plot", height="200px", width="100%")
          ), 
        tags$text("Use the year slider on the left to see the number of T20/ODI/Test matches each country has played on the map. 
                          You can also filter by match type (T20/ODI/Test).", class="legend"),
        div(class="info",
          p("Some interesting trends to look out for: rise of T20 cricket! The T20 format was created in 2003 and has since seen a rise, 
                           with more and more countries competing. Women's cricket too, has seen a dramatic increase over the last 
                           two decades. However, they mostly participtate in T20/ODI, with very few games played in the Test cricket format relative to men")),
        )
      ),
  h1("What Countries have Played Cricket the Most?",class="subtitle",align="center"),
  img(src="matches_played_race.gif",align="center", height='850px', width='1400px'),
  
  h1("Batting Performance: Average Runs of Each Country by Year", class="subtitle"),
  
  sidebarLayout(
    sidebarPanel(
      sliderInput("runsYear", "Year", as.Date("2003", "%Y"),as.Date("2023", "%Y"), as.Date("1", "%Y"), round=TRUE, timeFormat="%Y")
    ,width=2),
    mainPanel(
      splitLayout(
        leafletOutput("runsMap", width = "100%", height = 300),
        plotOutput("runsGraph", height="200px", width="100%")
      )
    )
  ),
)
))
