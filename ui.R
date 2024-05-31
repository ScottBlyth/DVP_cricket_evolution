library(shiny)
library(shinydashboard)
library(leaflet)

# Define UI for miles per gallon application
dashboardPage( 
  dashboardHeader(), 
  dashboardSidebar(collapsed=TRUE), 
  dashboardBody(fluidPage(
  tags$head(
    tags$style(
      ".blueTitle {color: blue; font-weight: bold;}",
      ".title {color: green; font-size: 50px; font-family: Georgia; font-weight: bold}", 
      ".subsection {color: black; font-size: 30px; align: center;}",
      ".subtitle {color: black; font-size: 20px; align: center; font-weight: bold}",
      ".pinfo {color: black; font-size: 16px; font-family: Arial; width=500px;}",
      ".legend {color: blue; font-size: 16px; font-family: Arial; width=500px;}",
      ".legend_male {color: steelblue; font-size: 16px; font-family: Arial; width=300px}",
      ".legend_female {color: pink; font-size: 16px; font-family: Arial; font-weight: bold, width=300px;}",
      ".yearRange {color: black; font-weight: bold}", 
      ".conclusion {color: black; font-weight: bold; font-family: Geogia; font-size: 30px}"
    )
  ),
  # Application title
  headerPanel(h1("Evolution: How has Cricket Changed?", class="title", align="center")),
  
  # Introduction 
  h1(span(class="blueTitle","Introduction:"), "What is the Purpose of this Visulualisation?", class="subsection"),
  p("This data visulisation is used to study trends between", span("2002 to 2023", class="yearRange"), 
      "in cricket. The dataset used includes information from each game in international cricket (T20/ODI/Test).", class="pinfo"),
  p("This visualisation explores how cricket has changed in three areas: Participation, Relative Performance and Country Connectivity.
    The Participation section, will explore which countries play the most cricket, and how this has changed overtime. Furthermore, 
      the increased participation of all countries around the globe will be shown, and especially the increasing participation of Women in 
      cricket. This visualisation also seeks so show the rise of T20 cricket, and how it relates to increased global participation in cricket.", class='pinfo'),
  p("Secondly, the win rates of each country, and how it has changed overtime will be shown, in order to show how different countries 
    perform against one another. The goal is to see if trends such as which teams dominate the sport over a particular time period can be seen.", class="pinfo"),
  # First Plot
  h1(span(class="blueTitle","Participation:"), " How much Cricket is Played?", class="subsection"),
  h1("Which Countries have Played Cricket the Most? (2002 to 2023)",class="subtitle",align="center"),
  
  HTML('<center><img src="matches_played_race.gif", height="850px", width="1400px"></center>'),
  p("This bar chart race shows the number of matches played across all forms of cricket (T20/ODI/Test) by each 
    country over the last 21 years (2002 to 2023).", class="legend"),
  p("A simple, inital observation is the top countries, Australia, India, England, South Africa etc. play 
    the significantely more matches then other countries. Specifically, by the end of 2023, 
    India had played 1026 compared to 204 by Netherlands. This is due to the only recent mass adoption of cricket globally
    beyond those 10 main cricket playing countries. Furthermore, they play all three forms of cricket, whereas most teams 
    only play T20 and ODI. Another observation is how quickly the top 10 order is determined, where no country
    initially below the top 10 ever 'break' into it. It s However, in recent years, Ireland is catching up to Zimbabwe, suggesting 
    that the 10 countries that make up the core of cricket, may change due to the recent mass adoption of cricket. ", class="pinfo"),
  sidebarLayout(sidebarPanel(
    sliderInput("year", "Year", as.Date("2002", "%Y"),as.Date("2023", "%Y"), as.Date("1", "%Y"), round=TRUE, timeFormat="%Y"),
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
                        "The ", span("Blue", class="legend_male"), " areas show matches played in", 
                        span("Male",class="legend_male"), "Cricket"),
                      p(class="pinfo",
                          "The ", span("Pink", class="legend_female"), " areas show matches played in", 
                        span("Female", class="legend_female"), "Cricket"))
          ), 
        tags$text("Click on a Country for more information! Also use the year slider on the left to see the number of T20/ODI/Test matches each country has played on the map. 
                  You can also filter by match type (T20/ODI/Test).", class="legend"),
        div(class="pinfo",
          p("Some interesting trends to look out for: rise of T20 cricket! The T20 format was created in", 
          a("2003", href="https://en.wikipedia.org/wiki/Twenty20"), "and has since seen a rise with more and more countries competing. 
          Women's cricket too, has seen a dramatic increase over the last two decades. 
            However, they mostly participate in T20/ODI, with very few games played in the Test cricket format relative to men, as seen in the graph to the right.
          Recall the quick jump of United Arab Emirates in the bar chart race - have a look at the UAE plot for matches played. See the increasing trend is mostly with in T20. 
          These observations show that T20 cricket has likely helped in the global participation of cricket. In fact, the T20 
          format of the game will be played in the", a(href="https://www.cricket.com.au/news/3738922/cricket-confirmed-for-2028-la-olympics", 
                                                       "2028 Olympics."))),
        ,width=10),
      ),
  
  h1(span("Performance:", class="blueTitle"), "How do Countries Compare?", class="subsection"),
  p("Win Rates by Country and Relative Performance",class="subtitle", align="center"),
  sidebarLayout(
    sidebarPanel(
      sliderInput("runsYear", "Year", as.Date("2002", "%Y"),as.Date("2023", "%Y"), c(as.Date("2002", "%Y"), as.Date("2023", "%Y")), round=TRUE, timeFormat="%Y"),
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
        plotOutput("winrateGraph", height="400px", width="100%")),
      ),
      p("The year slider has two ends, where the maps shows the win rates between those two years. You can either 
        change end at once, by dragging the circle. Or you can slider both ends, by dragging from the blue line between the two 
        ends.",class="legend"),
      p("When using this graph, it should be noted that a teams high perfrormance (or low) has of course been effected by 
        what teams they compete against. This can bee seen with the large variability when moving year slider 
        year by year. However, as you extend this both ends of the slider across the a larger area, some trends do emerge: the more recent 
        teams (China, Turkey, South Korea etc.) perform worse (try setting year slider to 2019 to 2023). For specific trends, have a look at Australia 
        at 5 year intervals (2002 to 2007, 2008 to 2013, 2014 to 2019, 2020 to 2023) for Test male cricket. Notice the initial"
        , a(href="https://en.wikipedia.org/wiki/Australia_national_cricket_team", "golden era"), "followed by a cold spell and now a general 
        increase in performance. Intestingly, when looking at the average 
        runs for T20/ODI/Test, it appears to be unchanging overtime, evene for the newer T20 sport.\n For specific country 
        observations, be sure to search for specific countries to see their win rate and average runs plot.",class="pinfo")
    ,width=10)
  ),
  
  h1("Conclusion", class="conclusion", align="center"),
  p("The introduction of T20 cricket has resulted in a signifcant increase in global participation in cricket. However, 
    this increase is not seen across all forms of the game, namely ODI and Test cricket. Furthermore, women's cricket has also increased.
    There is an observed great variablity in win rates.",class="pinfo")
  
)
), title="Evolution: How has Cricket Changed?", skin="black")
