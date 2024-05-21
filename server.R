library(leaflet)
library(leaflet.extras)
library(dplyr)
library(memoise)
library(tmaptools)
library(sf)
library(maps)
library(RColorBrewer)
data <- read.csv("cricsheet_data.csv")

data_counts <- read.csv("games_played.csv")

get_location <- memoise(geocode_OSM)

data_games = data %>% group_by(match.id, team_1, team_2,match_type) %>% 
          reframe(year=format(as.Date(start.date, "%Y-%m-%d"), "%Y"))



data_count <- data_games %>% group_by(team_1, year, match_type) %>% tally() %>% reframe(team_1=team_1, year=year,n=n)
data_count2 <- data_games %>% group_by(team_2, year, match_type) %>% tally() %>% reframe(team_1=team_2, year=year,match_type=match_type, n=n)
data_count3 <- merge(x=data_count, y=data_count2, by.x="team_1", by.y="team_1", all = TRUE) %>% 
  filter(year.x==year.y, team_1!="ICC World XI") %>% group_by(team_1, year.x, match_type) %>% 
  reframe(n=n.x+n.y)

data_counts <- data_counts %>% group_by(team_1, year.x) %>% reframe(n=sum(n))
func <- function(x) {
  
  if(x == "England") {
    return("UK")
  }
  return(x)
  
}
data_counts$team_1 <- lapply(data_counts$team_1, func)

worldMap <- map("world", fill = TRUE, plot = FALSE) 

# taken from week 4 moodle content
splitNames <- strsplit(worldMap$names, ":") 
firstPartNames <- lapply(splitNames, function(x) x[1]) 

cpal = colorNumeric("RdBu",data_counts$n, reverse=TRUE)
shinyServer(function(input, output) {
  # Compute the formula text in a reactive expression since it is 
  # shared by the output$caption and output$mpgPlot expressions
  test <- reactive(filter(data_counts, 
                          abs(as.numeric(year.x)-as.numeric(format(input$year, "%Y")))<=1))
  
  game_forms = reactive(c(input$T20, input$ODI, input$Test))
  
  games_played <- reactive(test()$n[match(firstPartNames, test()$team_1)])
  
  output$gamesMap <- renderLeaflet(leaflet(worldMap) %>% 
                                     setView(lng = 0, lat = 0, zoom = 1.25) %>%
                                     addTiles() %>%  addProviderTiles("Esri.WorldGrayCanvas") %>%
                                     addPolygons(
                                       stroke = FALSE, 
                                       smoothFactor = 0.2, 
                                       fillOpacity = 1,
                                       color = cpal(games_played()),
                                       popup = paste("Country", test()$team_1[match(firstPartNames, test()$team_1)], "n: ",
                                                     test()$n[match(firstPartNames, test()$team_1)])
                                     ) %>% addLegend(pal=cpal, values=data_counts$n))
  
})
