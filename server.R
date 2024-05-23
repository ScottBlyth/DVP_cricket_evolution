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

data_counts <- read.csv("games_played2.csv")

data_runs <- data %>% 
  reframe(gender=gender, batting=team.batting,runs=runs, match_type=match_type,year=format(as.Date(start.date, "%Y-%m-%d"), "%Y")) %>% 
  group_by(batting, year,match_type) %>% summarise(runsAvg=mean(runs))


get_location <- memoise(geocode_OSM)
 
  # data_games = data %>% 
  #           reframe(gender=gender, id=match.id, team_1=team_1, team_2=team_2, match_type=match_type,year=format(as.Date(start.date, "%Y-%m-%d"), "%Y")) %>% 
  #           group_by(id, team_1, team_2,match_type, year, gender) 
  
#  
#  data_count <- data_games %>% group_by(team_1, year, match_type, gender) %>% tally() %>% reframe(n=n)
#  data_count2 <- data_games %>% group_by(team_2, year, match_type, gender) %>% tally() %>%
#            reframe(team_1=team_2,gender=gender, year=year,match_type=match_type, n=n)
#  data_count3 <- merge(x=data_count, y=data_count2, by.x="team_1", by.y="team_1", all = TRUE) %>% 
#    filter(year.x==year.y,match_type.x==match_type.y, team_1!="ICC World XI") %>% group_by(team_1, year.x, match_type.x, gender) %>% 
#    reframe(n=n.x+n.y, lat=get_location(team_1)$coords[1], long=get_location(team_1)$coords[2])
#  
#  data_count3$lat = apply(data_count3$team_1, function(x) {return(get_location(x)$coords[[1]][1])})
#  data_count3$long = apply(data_count3$team_1, function(x) {return(get_location(x)$coords[[2]])})

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

game_types = c("T20", "ODI", "Test")

genderColour <- colorFactor(palette = c( "steelblue1", "maroon1"), domain=c("male", "female"))
create_games_played_graph = function(team) {
  if(team != "World") {
    data_counts_team <- data_counts %>% filter(team_1==team)
    p <- ggplot(data_counts_team, aes(x=as.numeric(year.x),y=n,fill=genderColour(gender)))+geom_col()+
      labs(title=team, x="Year",  y="Matches Played",colour="Match Type", fill="Gender")+expand_limits(y=0)+
      scale_color_manual(values=c("male", "female"))
    return(p)
  }
  p <- ggplot(data_counts,  aes(x=as.numeric(year.x),y=n,colour=match_type.x))+geom_smooth()+
    labs(title=team, x="Year",  y="Matches Played",colour="Match Type")+expand_limits(y=0)
  return(p)
}

create_avg_runs_graph = function(team) {
  d_team <- data_runs %>% filter(batting==team)
  p <- ggplot(d_team, aes(x=as.numeric(year),y=runsAvg,colour=match_type))+geom_smooth()+
    labs(title=team, x="Year",  y="Average Runs",colour="Match Type")+expand_limits(y=0)
  return(p)
}



d <- data_counts %>% group_by(team_1, year.x) %>% summarise(n=sum(n))
cpal = colorNumeric("RdBu",d$n, reverse=TRUE)
cpal_runs = colorNumeric("Blues", data_runs$runsAvg)



shinyServer(function(input, output) {
  
  p <- reactive(create_games_played_graph(input$country))
  output$country_plot <- renderPlot(p())
  
  form_bools = reactive(c(input$T20, input$ODI, input$Test))
  game_forms = reactive(to_vec(for(i in 1:3) if(form_bools()[i]) game_types[i]))
  
  test <- reactive(filter(data_counts, 
                          year.x == format(input$year, "%Y"), 
                          match_type.x %in% game_forms()) %>% 
                          group_by(team_1) %>% summarise(n=sum(n))) 

  games_played <- reactive(test()$n[match(firstPartNames, test()$team_1)])
  
  output$gamesMap <- renderLeaflet(leaflet(worldMap) %>% 
                                     setView(lng = 0, lat = 0, zoom = 1.25) %>%
                                     addTiles() %>% addProviderTiles("Esri.WorldGrayCanvas") %>%
                                     addPolygons(
                                       stroke = FALSE, 
                                       smoothFactor = 0.2, 
                                       fillOpacity = 1,
                                       color = cpal(games_played()),
                                       popup = paste("Country", test()$team_1[match(firstPartNames, test()$team_1)], 
                                          "\nn: ", test()$n[match(firstPartNames, test()$team_1)])
                                     ) %>% addLegend(pal=cpal, values=d$n))
  
   runs <- reactive(filter(data_runs, year==format(input$runsYear, "%Y"), 
                    match_type %in% game_forms()) %>% group_by(batting) %>% summarise(n=mean(runsAvg)))
   
   runs_list <- reactive(runs()$n[match(firstPartNames, runs()$batting)])
   output$runsMap <- renderLeaflet(leaflet(worldMap) %>% 
                                     addProviderTiles("Esri.WorldGrayCanvas") %>%
                                   addPolygons(
                                     stroke = FALSE, 
                                     smoothFactor = 0.2, 
                                     fillOpacity = 1,
                                     color = cpal_runs(runs_list()), 
                                     popup=paste("Country", runs()$batting[match(firstPartNames, runs()$batting)], 
                                                 "\nAverage Runs: ", runs()$n[match(firstPartNames, runs()$batting)])
                                   ))
   p2 <- reactive(create_avg_runs_graph(input$country)) 
   output$runsGraph <- renderPlot(p2())
})


