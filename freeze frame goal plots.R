#Plot shot Freeze Frames from StatsBomb free event data
#Eliot McKinley
#June 23, 2019
#This was hacked together pretty quickly and could be optimized

library(tidyverse)
library(StatsBombR)

source('createPitch_StatsBomb.R')

matches=FreeMatches(72)

events=StatsBombFreeEvents(MatchesDF = matches)

events=allclean(events)

match.select=12 #choose which match to plot

#choose which team you want to plot
team.select=matches$home_team.home_team_name[match.select] 
opposition.select=matches$away_team.away_team_name[match.select]

match.info=matches[match.select,]


#extract goals
shots=events %>%  filter( type.name=="Shot", shot.outcome.name =="Goal", team.select==team.select, match_id==match.info$match_id[1])


#build data frame of freeze frame locations
freeze=NULL
for (shot.row in 1:nrow(shots)){
  
  shot.data=data.frame(shots$shot.freeze_frame[shot.row])
  shot.data=data.frame(shot.data, data.frame(matrix(unlist(shot.data$location),ncol=2, byrow=T, dimnames = list(NULL,c("location.x", "location.y")))))
  shot.data$teammate[shot.data$position.name=='Goalkeeper']='Keeper'
  shot.data$teammate=factor(shot.data$teammate, levels=c('Keeper', 'FALSE', 'TRUE'))
  shot.data$name=paste0(shots$player.name[shot.row]," ", shots$minute[shot.row], ":", shots$second[shot.row],"\n", shots$shot.outcome.name[shot.row],  " ", shots$shot.body_part.name[shot.row], "\n", shots$play_pattern.name[shot.row], " xG: ", round(shots$shot.statsbomb_xg[shot.row],2) )
  freeze=rbind(freeze, shot.data)
}

#qdd name column for faceting
shots=shots %>% 
  mutate(name=paste0(player.name," ", minute, ":", second,"\n", shots$shot.outcome.name[shot.row]," ", shots$shot.body_part.name, "\n", shots$play_pattern.name,  " xG: ",  round(shot.statsbomb_xg,2) )) %>% 
  rename(shot.x=location.x, shot.y=location.y) 

#joing the freeze frame and shot
shots.2=left_join(freeze, shots, by="name")%>% 
  mutate(Team=ifelse(teammate==TRUE, gsub("Women's","",team.select), gsub("Women's","",opposition.select )))


createPitch_StatsBomb(data=shots.2)+
  geom_point(aes(y=location.x.x, x=location.y, color=Team), size=3)+
  geom_segment(aes(y=shot.x, x=shot.y, yend=shot.end_location.x, xend=shot.end_location.y), arrow = arrow(length = unit(0.01, "npc")))+
  geom_point(aes(y=shot.x, x=shot.y), shape=15, size=3)+
  
  scale_color_manual(values=c('blue', "red", "purple"))+
  labs(title=paste0(gsub("Women's","",team.select)),
       subtitle=paste0(match.info$match_date," ", gsub("Women's","",match.info$home_team.home_team_name),
                       " ", match.info$home_score, " v. ", gsub("Women's","",match.info$away_team.away_team_name), " ", match.info$away_score),
       caption="@etmckinley data:@StatsBomb",
       color="Team")+
  theme(aspect.ratio = 80/120, legend.position = "bottom")+
  facet_wrap(~name, ncol=7)
