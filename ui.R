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
      ".blueTitle {color: blue; font-weight: bold;}",
      ".title {color: green; font-size: 50px; font-family: Trebuchet MS; font-weight: bold}", 
      ".subsection {color: black; font-size: 30px; align: center;}",
      ".subtitle {color: black; font-size: 20px; align: center; font-weight: bold}",
      ".pinfo {color: black; font-size: 16px; font-family: Arial; width=500px;}",
      ".legend {color: blue; font-size: 16px; font-family: Arial; width=500px;}",
      ".legend_male {color: steelblue; font-size: 16px; font-family: Arial; width=300px}",
      ".legend_female {color: pink; font-size: 16px; font-family: Arial; width=300px;}"
    )
  ),
  # Application title
  headerPanel(h1("Evolution: How has Cricket Changed?", class="title", align="center")),
  
  # Introduction 
  
  # First Plot
  h1(span(class="blueTitle","Participation:"), " How much Cricket is Played?", class="subsection"),
  h1("Which Countries have Played Cricket the Most? (2003 to 2023)",class="subtitle",align="center"),
  
  HTML('<center><img src="matches_played_race.gif", height="850px", width="1400px"></center>'),
  
  
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
      splitLayout(leafletOutput("gamesMap",width = "100%", height = 600),
                  div(plotOutput("country_plot", height="500px", width="100%"), 
                      p(class="pinfo",
                        "The ", span("Blue", class="legend_male"), " areas show matches played in Male Cricket"),
                      p(class="pinfo",
                          "The ", span("Pink", class="legend_female"), " areas show matches played in Female Cricket"))
          ), 
        tags$text("Click on a Country for more information! Also use the year slider on the left to see the number of T20/ODI/Test matches each country has played on the map. 
                  You can also filter by match type (T20/ODI/Test).", class="legend"),
        div(class="pinfo",
          p("Some interesting trends to look out for: rise of T20 cricket! The T20 format was created in", 
          a("2003", href="https://en.wikipedia.org/wiki/Twenty20"), "and has since seen a rise with more and more countries competing.
          Women's cricket too, has seen a dramatic increase over the last two decades. 
            However, they mostly participtate in T20/ODI, with very few games played in the Test cricket format relative to men, as seen in the graph to the right.")),
        ,width=10),
      ),
  
  h1(span("Performance:", class="blueTitle"), "Average Runs of Each Country by Year", class="subsection"),
  sidebarLayout(
    sidebarPanel(
      sliderInput("runsYear", "Year", as.Date("2003", "%Y"),as.Date("2023", "%Y"), as.Date("1", "%Y"), round=TRUE, timeFormat="%Y"),
      checkboxInput("T20performance", "T20", value=TRUE),
      checkboxInput("ODIperformance", "ODI", value=TRUE),
      checkboxInput("Testperformance", "Test", value=TRUE),
      checkboxInput("malePerformance", "Male", value=TRUE),
      checkboxInput("femalePerformance", "Female", value=TRUE),
      textInput("countryPerformance", "Country", value="World")
    ,width=2),
    mainPanel(
      splitLayout(
        leafletOutput("runsMap", width = "100%", height = 600),
        div(plotOutput("runsGraph", height="200px", width="100%"),
        plotOutput("winrateGraph", height="200px", width="100%"))
      )
    ,width=10)
  ),
)
))
