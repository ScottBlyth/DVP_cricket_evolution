library(leaflet)
library(leaflet.extras)
library(tidyverse)
library(tmaptools)
library(sf)
library(maps)
library(RColorBrewer)
library(comprehenr)
library(ggplot2)
library(ggnewscale)
#library(gganimate) # uncomment to be able to make bar chart animation


data <- read.csv("cricsheet_data.csv")
data_counts <- read.csv("games_played.csv")

# computng win rates
data_wins_1 = data %>% group_by(match.id, team_1, team_2, winner, match_type, gender, start.date) %>% tally() %>%
  mutate(year=format(as.Date(start.date, "%Y-%m-%d"), "%Y"), team=team_1) %>%
  group_by(team, year,match_type, gender) %>% summarise(wins=sum(team==winner),games_played=sum(team==team))

data_wins_2 = data %>% group_by(match.id, team_1, team_2, winner, match_type, gender, start.date) %>% tally() %>%
  mutate(year=format(as.Date(start.date, "%Y-%m-%d"), "%Y"), team=team_2) %>%
  group_by(team, year,match_type, gender) %>% summarise(wins=sum(team==winner),games_played=sum(team==team))

data_wins = rbind(data_wins_1, data_wins_2) %>% group_by(team, year,match_type, gender) %>%
  reframe(wins=sum(wins), games_played=sum(games_played)) %>% 
  group_by(team, year,match_type, gender, wins,games_played) %>% tally()

win_rates <- data_wins %>% group_by(team, match_type) %>%
            summarise(wins=sum(wins), games_played=sum(games_played))


# compute data frame for average runs per gender,matchtype,team and year
data_runs <- data %>% 
  reframe(gender=gender, batting=team.batting,runs=runs, match_type=match_type,year=format(as.Date(start.date, "%Y-%m-%d"), "%Y")) %>% 
  group_by(gender, batting, year,match_type) %>% summarise(runsAvg=mean(runs))

 

make_data_counts <- function(){
  data_games = data %>% 
     reframe(gender=gender, id=match.id, team_1=team_1, team_2=team_2, match_type=match_type,year=format(as.Date(start.date, "%Y-%m-%d"), "%Y")) %>% 
       group_by(id, team_1, team_2,match_type, year, gender) %>% tally()
  
   data_count <- data_games %>% group_by(team_1, year, match_type, gender) %>% tally()
      data_count2 <- data_games %>% group_by(team_2, year, match_type, gender) %>% tally()
                reframe(team_1=team_2,gender=gender, year=year,match_type=match_type, n=n)
       data_count3 <- full_join(x=data_count,y=data_count2, by=join_by(team_1==team_2, year==year, match_type==match_type, 
                                                                       gender==gender)) %>% 
         filter(team_1!="ICC World XI") %>%
         mutate(n=n.x+n.y)
      
   return(data_count3) 
}

#data_counts <- data_counts %>% group_by(team_1, year.x) %>% reframe(n=sum(n))

# have to rename some teams to fit match with polygons
func <- function(x) {
  
  if(x == "England") {
    return("UK")
  }
  if(x == "United States of America") {
    return("USA")
  }
  return(x)
  
}

# data_wins$team = lapply(data_wins$team, func)
# 
# 
# data_counts$team_1 <- lapply(data_counts$team_1, func)
# data_runs$batting <- lapply(data_runs$batting, func)

# create ranked data runs
# reference
# https://www.youtube.com/watch?v=FOEoKbRUsT8

make_anim <- function() {
counts_anim <- data_counts %>% 
              group_by(year, team_1) %>% 
              reframe(n=sum(n, na.rm=TRUE))

counts_anim <- counts_anim %>% group_by(team_1) %>% 
              mutate(n=cumsum(n))

counts_anim <- counts_anim %>%
  pivot_wider(names_from = team_1,
              values_from = n) %>%
              fill(-year)

df2 <- counts_anim %>% 
  gather(key = team_1, 
         value = n, -year)
df2 <- df2[order(df2$year),]

df_ranked <- df2 %>% 
  group_by(year) %>% 
  arrange(year, -n) %>% 
  mutate(rank = 1:n()) %>% 
  filter(rank <= 15)

chart <- ggplot(df_ranked, aes(rank, n))+
  geom_bar(stat="identity", aes(fill=team_1))+
  coord_flip()+
  scale_x_reverse()+
  labs(y="Matches Played Across all Cricket")+
  geom_text(aes(rank, y=0, label=team_1),
            hjust=0,colour="black",family="sans", fontface="bold", size=3)+
  geom_text(aes(label=sprintf("%1.0f", n)), 
            hjust=1.1, fontface="bold", size=3)+
  geom_label(aes(label=sprintf("%1.0f", year),x=16, y=500),size=25, fontface="bold")+
  theme_minimal()+
    theme(pane.grid=element_blank(),
          axis.title.y=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks.y=element_blank(),
          axis.text = element_text(size=10),
          legend.position = "none",
          plot.margin = margin(1,6,1,6), "cm")
  
  
  
animation <- chart + 
    transition_states(year, transition_length=5, state_length=5)+
    ease_aes('linear')
    return(animation)
}

worldMap <- map("world", fill = TRUE, plot = FALSE) 

# taken from week 4 moodle content
splitNames <- strsplit(worldMap$names, ":") 
firstPartNames <- lapply(splitNames, function(x) x[1]) 

game_types = c("T20", "ODI", "Test")



# games played Plot
####################
genderColour <- colorFactor(palette = c("pink", "steelblue1"), domain=c("female", "male"))
create_games_played_graph = function(team) {
  data_counts_team <- data_counts
  if(team != "World") {
    data_counts_team <- data_counts %>% filter(team_1==team)
  }
  if(nrow(data_counts_team)==0) {
    p <- ggplot()+geom_text(aes(x=0,y=0, label=paste('"',team, '" country not found')), size=10, 
                            colour="red", fontface="bold")
    return(p)
  }
  data_counts_team$colour <- lapply(data_counts_team$gender, genderColour)
  data_counts_team <- data_counts_team[order(data_counts_team$gender),]
  gender_pallete = sapply(unique(data_counts_team$gender), genderColour)
  p <- ggplot(data_counts_team, aes(x=as.numeric(year),y=n,fill=colour))+geom_col()+
    labs(title=team, x="Year",  y="Matches Played",colour="Match Type", fill="Gender")+expand_limits(y=0)+
    scale_colour_identity()+facet_wrap(vars(match_type), dir="v")+ 
        theme(text=element_text(size=20), axis.text.x=element_text(size=10, angle=45))+
    scale_x_continuous(breaks=2002:2023)
  return(p)
}

# Average Runs Plot 
#####################
create_avg_runs_graph = function(team) {
  d_team <- data_runs
  if(team != "World") {
    d_team <- d_team %>% filter(batting==team)
  }
  if(nrow(d_team)==0) {
    p <- ggplot()+geom_text(aes(x=0,y=0, label=paste('"',team, '" country not found')), size=10, 
                            colour="red", fontface="bold")
    return(p)
  }
  p <- ggplot(d_team, aes(x=as.numeric(year),y=runsAvg,colour=match_type))+geom_smooth()+
    labs(title=team, x="Year",  y="Average Runs",colour="Match Type")+expand_limits(y=0)+
    theme(text=element_text(size=20))
  return(p)
}

# Win Rate plot 
#################
create_win_rate_graph <- function(team_1) {
  d_wins <- data_wins %>% filter(team==team_1) 
  if(nrow(d_wins)==0) {
    p <- ggplot()+geom_text(aes(x=0,y=0, label=paste('"',team_1, '" country not found')), size=10, 
                            colour="red", fontface="bold")
    return(p)
  }
  d_wins$colour <- lapply(d_wins$gender, genderColour)
  p <- ggplot(d_wins, aes(x=as.numeric(year), y=wins/games_played, group=gender, fill=colour, color=colour))+
        scale_colour_identity()+
        geom_smooth()+labs(title=team_1, x="Year", y="Win Rate")+
        geom_hline(yintercept=0.5, colour="black")+
        scale_y_continuous(limits=c(0,1))+
        facet_wrap(~gender+~match_type)+
        theme(text=element_text(size=20))
  return(p)
}

d <- data_counts %>% group_by(team_1, year) %>% summarise(n=sum(n,na.rm=TRUE))
cpal = colorNumeric("Blues",d$n, reverse=TRUE)
cpal_wins = colorNumeric("RdBu", data_wins$wins/data_wins$games_played, reverse=TRUE)


shinyServer(function(input, output) {
  
  # FIRST PLOT
  
  p <- reactive(create_games_played_graph(input$country))
  output$country_plot <- renderPlot(p())
  
  form_bools = reactive(c(input$T20, input$ODI, input$Test))
  game_forms = reactive(to_vec(for(i in 1:3) if(form_bools()[i]) game_types[i]))
  
  form_bools_perform <- reactive(c(input$T20performance, input$ODIperformance, input$Testperformance))
  game_forms_perform = reactive(to_vec(for(i in 1:3) if(form_bools_perform()[i]) game_types[i]))
  
  genders = c("male", "female")
  gender_bools = reactive(c(input$male, input$female))
  gender_list = reactive(to_vec(for(i in 1:2) if (gender_bools()[i]) genders[i]))
  
  gender_bools_perform <- reactive(c(input$malePerformance, input$femalePerformance))
  gender_list_perform <- reactive(to_vec(for(i in 1:2) if (gender_bools_perform()[i]) genders[i]))
  
  
  test <- reactive(filter(data_counts, 
                          year == format(input$year, "%Y"), gender %in% gender_list(), 
                          match_type %in% game_forms()) %>% 
                          group_by(team_1) %>% summarise(n=sum(n))) 

  games_played <- reactive(test()$n[match(firstPartNames, lapply(test()$team_1, func))])
  
  output$gamesMap <- renderLeaflet(leaflet(worldMap) %>% 
                                     setView(lng = 0, lat = 0, zoom = 1.25) %>%
                                     addTiles() %>% addProviderTiles("Esri.WorldGrayCanvas") %>%
                                     addPolygons(
                                       stroke = FALSE, 
                                       smoothFactor = 0.2, 
                                       fillOpacity = 1,
                                       color = cpal(games_played()),
                                       popup = paste("Country", test()$team_1[match(firstPartNames,lapply(test()$team_1, func))], 
                                          "\nn: ", test()$n[match(firstPartNames, lapply(test()$team_1, func))])
                                     ) %>% addLegend(pal=cpal, values=d$n, title="Matches Played"))
  
   wins <- reactive(filter(data_wins, as.numeric(year)>=as.numeric(format(input$runsYear[1], "%Y")),
                           as.numeric(year)<=as.numeric(format(input$runsYear[2], "%Y")), 
                    match_type %in% game_forms_perform(), gender %in% gender_list_perform()) 
                    %>% group_by(team) %>% reframe(n=sum(wins)/sum(games_played)))
   
   # SECOND map/plot
   
   wins_list <- reactive(wins()$n[match(firstPartNames, lapply(wins()$team, func))])
   output$runsMap <- renderLeaflet(leaflet(worldMap) %>% 
                                     addProviderTiles("Esri.WorldGrayCanvas") %>%
                                   addPolygons(
                                     stroke = FALSE, 
                                     smoothFactor = 0.2, 
                                     fillOpacity = 1,
                                     color = cpal_wins(wins_list()), 
                                     popup=wins()$team[match(firstPartNames, lapply(wins()$team, func))]
                                   ) %>% addLegend(pal=cpal_wins, values=data_wins$wins/data_wins$games_played, title="Win Rate") )
   p2 <- reactive(create_avg_runs_graph(input$countryPerformance)) 
   output$runsGraph <- renderPlot(p2())
   p3 <- reactive(create_win_rate_graph(input$countryPerformance)) # change input country 
   output$winrateGraph <- renderPlot(p3())

   
})


