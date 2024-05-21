library(leaflet)
library(leaflet.extras)
library(dplyr)
library(memoise)
library(tmaptools)
library(sf)
library(maps)
library(RColorBrewer)
library(comprehenr)
library(leafpop)
library(ggplot2)
data <- read.csv("cricsheet_data.csv")

data_counts <- read.csv("games_played.csv")

get_location <- memoise(geocode_OSM)

# data_games = data %>% 
#           reframe(id=match.id, team_1=team_1, team_2=team_2, match_type=match_type,year=format(as.Date(start.date, "%Y-%m-%d"), "%Y")) %>% 
#           group_by(id, team_1, team_2,match_type, year) %>% tally()
# 
# 
# data_count <- data_games %>% group_by(team_1, year, match_type) %>% tally() %>% reframe(n=n)
# data_count2 <- data_games %>% group_by(team_2, year, match_type) %>% tally() %>%
#           reframe(team_1=team_2, year=year,match_type=match_type, n=n)
# data_count3 <- merge(x=data_count, y=data_count2, by.x="team_1", by.y="team_1", all = TRUE) %>% 
#   filter(year.x==year.y,match_type.x==match_type.y, team_1!="ICC World XI") %>% group_by(team_1, year.x, match_type) %>% 
#   reframe(n=n.x+n.y, lat=get_location(team_1)$coords[1], long=get_location(team_1)$coords[2])
# 
# data_count3$lat = apply(data_count3$team_1, function(x) {return(get_location(x)$coords[[1]][1])})
# data_count3$long = apply(data_count3$team_1, function(x) {return(get_location(x)$coords[[2]])})

#data_counts <- data_counts %>% group_by(team_1, year.x) %>% reframe(n=sum(n))
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
game_types = c("T20", "ODI", "Test")

create_games_played_graph = function(team) {
  data_counts_team <- data_counts %>% filter(team_1==team)
  p <- ggplot(data_counts_team, aes(x=as.numeric(year.x),y=n,colour=match_type.x))+geom_point()
  return(p)
}
counts_smaller <- data_counts %>% filter(team_1=="Australia") %>% group_by(team_1) %>% reframe()
counts_smaller$graphs <- lapply(counts_smaller$team_1, create_games_played_graph)

team_names <- counts_smaller$team_1[match(firstPartNames, counts_smaller$team_1)]
game_graphs = counts_smaller$graphs[match(firstPartNames, counts_smaller$team_1)]


shinyServer(function(input, output) {
  
  form_bools = reactive(c(input$T20, input$ODI, input$Test))
  game_forms = reactive(to_vec(for(i in 1:3) if(form_bools()[i]) game_types[i]))
  
  test <- reactive(filter(data_counts, 
                          abs(as.numeric(year.x)-as.numeric(format(input$year, "%Y")))<=1, 
                          match_type.x %in% game_forms()))

  games_played <- reactive(test()$n[match(firstPartNames, test()$team_1)])
  
  output$gamesMap <- renderLeaflet(leaflet(worldMap) %>% 
                                     setView(lng = 0, lat = 0, zoom = 1.25) %>%
                                     addTiles() %>%  addProviderTiles("Esri.WorldGrayCanvas") %>%
                                     addPolygons(
                                       stroke = FALSE, 
                                       smoothFactor = 0.2, 
                                       fillOpacity = 1,
                                       color = cpal(games_played()),
                                       popup="hi"
                                     ) %>%
                                     addLegend(pal=cpal, values=data_counts$n))
  
#popup = paste("Country", test()$team_1[match(firstPartNames, test()$team_1)], "n: ",
#  test()$n[match(firstPartNames, test()$team_1)])
})
