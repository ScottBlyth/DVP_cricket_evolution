library(leaflet)
library(leaflet.extras)
library(tidyverse)
library(memoise)
library(tmaptools)
library(sf)
library(maps)
library(RColorBrewer)
library(comprehenr)
library(leafpop)
library(ggplot2)
library(gganimate)



data <- read.csv("cricsheet_data.csv")

data_wins_1 = data %>% group_by(match.id, winner,match_type) %>%
  reframe(team=team_1,gender=gender,
          match_type=match_type,year=format(as.Date(start.date, "%Y-%m-%d"), "%Y")) %>%
  group_by(team, year,match_type,gender) %>% reframe(wins=sum(team==winner),games_played=sum(team==team), match_type=match_type)

data_wins_2 = data %>% group_by(match.id, winner,match_type) %>%
  reframe(team=team_2,gender=gender,
          match_type=match_type,year=format(as.Date(start.date, "%Y-%m-%d"), "%Y")) %>%
  group_by(team, year,match_type, gender) %>% reframe(wins=sum(team==winner),games_played=sum(team==team), match_type=match_type)


data_wins = rbind(data_wins_1, data_wins_2) %>% group_by(team, year,match_type, gender) %>%
  reframe(wins=sum(wins), games_played=sum(games_played), match_type=match_type)


data_counts <- read.csv("games_played2.csv")

data_runs <- data %>% 
  reframe(gender=gender, batting=team.batting,runs=runs, match_type=match_type,year=format(as.Date(start.date, "%Y-%m-%d"), "%Y")) %>% 
  group_by(gender, batting, year,match_type) %>% summarise(runsAvg=mean(runs))


get_location <- memoise(geocode_OSM)
 
  #  data_games = data %>% 
  #            reframe(gender=gender, id=match.id, team_1=team_1, team_2=team_2, match_type=match_type,year=format(as.Date(start.date, "%Y-%m-%d"), "%Y")) %>% 
  #            group_by(id, team_1, team_2,match_type, year, gender) 
  # 
  # 
  # data_count <- data_games %>% group_by(team_1, year, match_type, gender) %>% tally() %>% reframe(n=n)
  # data_count2 <- data_games %>% group_by(team_2, year, match_type, gender) %>% tally() %>%
  #           reframe(team_1=team_2,gender=gender, year=year,match_type=match_type, n=n)
  # data_count3 <- merge(x=data_count, y=data_count2, by.x="team_1", by.y="team_1", all = TRUE) %>% 
  #   filter(year.x==year.y,match_type.x==match_type.y, team_1!="ICC World XI") %>% group_by(team_1, year.x, match_type.x, gender) %>% 
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

data_wins$team = lapply(data_wins$team, func)

data_counts$team_1 <- lapply(data_counts$team_1, func)
data_runs$batting <- lapply(data_runs$batting, func)

# create ranked data runs
# reference
# https://www.youtube.com/watch?v=FOEoKbRUsT8

data_ranked <- data_runs %>% filter(gender=="male", 
                                    match_type=="T20") %>% 
                            group_by(batting, year, runsAvg) %>%
                            reframe(year=as.Date(year, "%Y"))

data_ranked <- data_ranked[order(data_ranked$year),]
      
data_ranked <- data_ranked %>%
            pivot_wider(names_from = batting,
                        values_from = runsAvg) %>%
          fill(-year)


df2 <- data_ranked %>% 
        gather(key = batting, 
               value = runsAvg, -year)

df_ranked <- df2 %>% 
            group_by(year) %>% 
            arrange(year, -runsAvg) %>% 
            mutate(rank = 1:n()) %>% 
            filter(rank <= 10)

chart <- ggplot(df_ranked, aes(rank, runsAvg)) +
        geom_bar(stat="identity") +
        scale_x_reverse()+
        coord_flip()+
        scale_x_reverse()+
        geom_text(aes(rank, y=0, label=batting),
                  hjust=0, fontface="bold", size=3)+
        geom_text(aes(label=sprintf("%1.0f", runsAvg)), 
                  hjust=1.1, fontface="bold", size=3)+
        geom_text(aes(x=16,y=15, label=format(year, "%Y")), size=25, 
                  colour="red")
        theme_minimal()+
        theme(pane.grid=element_blank(),
              legend.position = "none",
              plot.margin = margin(1,6,1,6), "cm")
animation <- chart + 
            transition_states(year, transition_length=10, state_length=0)+
            view_follow(fixed_x=TRUE)+
            ease_aes('quadratic-in-out')


counts_anim <- data_counts %>% 
              group_by(year.x, team_1) %>% 
              summarise(n=sum(n)) %>% 
              reframe(year=as.Date(year.x, "%Y"))
counts_anim <- counts_anim %>%
  pivot_wider(names_from = team_1,
              values_from = n) %>%
              fill(-year)


worldMap <- map("world", fill = TRUE, plot = FALSE) 

# taken from week 4 moodle content
splitNames <- strsplit(worldMap$names, ":") 
firstPartNames <- lapply(splitNames, function(x) x[1]) 

game_types = c("T20", "ODI", "Test")

genderColour <- colorFactor(palette = c( "pink", "steelblue1"), domain=c("female", "male"))
create_games_played_graph = function(team) {
  data_counts_team <- data_counts
  if(team != "World") {
    data_counts_team <- data_counts %>% filter(team_1==team)
  }
  data_counts_team$colour <- lapply(data_counts_team$gender, genderColour)
  data_counts_team <- data_counts_team[order(data_counts_team$gender),]
  gender_pallete = sapply(unique(data_counts_team$gender), genderColour)
  p <- ggplot(data_counts_team, aes(x=as.numeric(year.x),y=n,fill=colour))+geom_col()+
    labs(title=team, x="Year",  y="Matches Played",colour="Match Type", fill="Gender")+expand_limits(y=0)+
    scale_colour_identity()+facet_wrap(vars(match_type.x))
  return(p)
}

create_avg_runs_graph = function(team) {
  d_team <- data_runs
  if(team != "World") {
    d_team <- d_team %>% filter(batting==team)
  }
  
  p <- ggplot(d_team, aes(x=as.numeric(year),y=runsAvg,colour=match_type))+geom_smooth()+
    labs(title=team, x="Year",  y="Average Runs",colour="Match Type")+expand_limits(y=0)
  return(p)
}



d <- data_counts %>% group_by(team_1, year.x) %>% summarise(n=sum(n))
cpal = colorNumeric("RdBu",d$n, reverse=TRUE)
cpal_wins = colorNumeric("RdBu", data_wins$wins/data_wins$games_played, reverse=TRUE)



shinyServer(function(input, output) {
  
  # FIRST PLOT
  
  p <- reactive(create_games_played_graph(input$country))
  output$country_plot <- renderPlot(p())
  
  form_bools = reactive(c(input$T20, input$ODI, input$Test))
  game_forms = reactive(to_vec(for(i in 1:3) if(form_bools()[i]) game_types[i]))
  genders = c("male", "female")
  gender_bools = reactive(c(input$male, input$female))
  gender_list = reactive(to_vec(for(i in 1:2) if (gender_bools()[i]) genders[i]))
  
  
  test <- reactive(filter(data_counts, 
                          year.x == format(input$year, "%Y"), gender %in% gender_list(), 
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
                                     ) %>% addLegend(pal=cpal, values=d$n, title="Matches Played"))
  
   wins <- reactive(filter(data_wins, year==format(input$runsYear, "%Y"), 
                    match_type %in% game_forms(), gender %in% gender_list()) %>% group_by(team) %>% reframe(n=wins/games_played))
   
   # SECOND plot
   
   wins_list <- reactive(wins()$n[match(firstPartNames, wins()$team)])
   output$runsMap <- renderLeaflet(leaflet(worldMap) %>% 
                                     addProviderTiles("Esri.WorldGrayCanvas") %>%
                                   addPolygons(
                                     stroke = FALSE, 
                                     smoothFactor = 0.2, 
                                     fillOpacity = 1,
                                     color = cpal_wins(wins_list()), 
                                     popup="hello"
                                   ) %>% addLegend(pal=cpal_wins, values=data_wins$wins/data_wins$games_played, title="Win Rate") )
   p2 <- reactive(create_avg_runs_graph(input$country)) 
   output$runsGraph <- renderPlot(p2())
})


