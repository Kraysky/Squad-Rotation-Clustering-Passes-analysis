##PREPARING DATA

#loading neccessary libraries
library(rjson)
library(data.table)
library(dplyr)

#loading competitions data from JSON file into a list
competitions <- fromJSON(file = "C:\\Users\\pkrajewski\\Desktop\\data\\competitions.json")

#converting list into dataframe
competitions.df <- data.frame(do.call(rbind,competitions),stringsAsFactors = FALSE)

View(competitions.df)

#loading matches data from folder and subfolders
match.file <- list.files(path = "C:\\Users\\pkrajewski\\Desktop\\data\\matches",
                         full.names = TRUE, recursive = TRUE)

matches.list <- list()
for(i in 1:length(match.file)){
  match.temp <- fromJSON(file = match.file[i]) #looping through each file and converting to list
  
  matches <- lapply(match.temp, function(x) data.frame(t(unlist(x)),stringsAsFactors = FALSE))
  matches.df <- rbindlist(matches, fill = TRUE)
  matches.list[[i]] <- matches.df
}

all.matches.df <- data.frame(rbindlist(matches.list, fill = TRUE)) #combining all matches data from lists

#cleaning matches data - removing columns with NA values
columns.to.keep <- names(which(unlist(lapply(all.matches.df, function(x) length(which(is.na(x)))))==0))

all.matches.clean <- all.matches.df[,columns.to.keep]
str(all.matches.clean)

#converting character variables to numeric
all.matches.clean$match_week <- as.numeric(all.matches.clean$match_week)
all.matches.clean$home_score <- as.numeric(all.matches.clean$home_score)
all.matches.clean$away_score <- as.numeric(all.matches.clean$away_score)


###Obtain Events###
event.files <- list.files(path="C:\\Users\\pkrajewski\\Desktop\\data\\events",
                          full.names = TRUE, recursive = TRUE)

event.list <- list()
for(i in 1:length(event.files)){
  event.temp <- fromJSON(file = event.files[i])
  
  #get unique id's of teams participating in a match
  teamids <- c()
  
  #obtain index where we find the event that talks about Starting XI
  starting.x11.index <- which(unlist(lapply(event.temp, function(x) x$type$name)) == "Starting XI")
  starting.x11.list <- list()
  for(s in 1:2){
    starting.x11.team1 <- data.frame(matrix(t(unlist(event.temp[[s]]$tactics$lineup)),ncol=5, byrow=TRUE),stringsAsFactors = FALSE)
    colnames(starting.x11.team1) <- names(unlist(event.temp[[s]]$tactics$lineup))[1:5]
    starting.x11.team1$formation <- event.temp[[s]]$tactics$formation
    starting.x11.team1$team_id <- event.temp[[s]]$team$id
    
    teamids <- c(teamids, event.temp[[s]]$team$id)
    
    starting.x11.team1$team_name <- event.temp[[s]]$team$name
    starting.x11.list[[s]] <- starting.x11.team1
  }
  
  pass.index <- which(unlist(lapply(event.temp, function(x) x$type$name)) =="Pass")
  
  pass.team1 <- pass.index[which(unlist(lapply(pass.index, function(x) event.temp[[x]]$team$id))==teamids[1])]
  pass.team1.df <- data.frame(matrix(NA,nrow=1, ncol=11))
  colnames(pass.team1.df) <- c("Possesion", "Passer", "X.Pass", "Y.Pass",
                               "Pass.Type", "Receiver", "X.Receive", "Y.Receive",
                               "Pass.Length", "Pass.Angle", "Body.Part")
  
  for(p in 1:length(pass.team1)){
    pass.temp <- event.temp[[pass.team1[p]]]
    possession <- pass.temp$possession
    passer <- pass.temp$player$id
    pass.location <- pass.temp$location
    pass.type <- pass.temp$pass$height$name
    receiver <- pass.temp$pass$recipient$id
    receive.location <- pass.temp$pass$end_location
    pass.length <- pass.temp$pass$length
    pass.angle <- pass.temp$pass$angle
    body.part <- pass.temp$pass$body_part$name
    
    row.toadd <- c(possession, passer, pass.location, pass.type, receiver, receive.location, pass.length, pass.angle, body.part)
    pass.team1.df <- rbind(pass.team1.df, row.toadd)
  }
  pass.team1.df <- pass.team1.df[-1,]
  pass.team1.df[,c(1:4,6:10)] <- lapply(pass.team1.df[,c(1:4,6:10)],as.numeric)
  pass.team1.df <- pass.team1.df %>% group_by("Possession") %>% mutate(seq = row_number())
  pass.team1.df$team_id <- teamids[1]
  
  pass.team2 <- pass.index[which(unlist(lapply(pass.index, function(x) event.temp[[x]]$team$id))==teamids[2])]
  pass.team2.df <- data.frame(matrix(NA,nrow=1, ncol=11))
  colnames(pass.team2.df) <- c("Possesion", "Passer", "X.Pass", "Y.Pass",
                               "Pass.Type", "Receiver", "X.Receive", "Y.Receive",
                               "Pass.Length", "Pass.Angle", "Body.Part")
  
  for(p in 1:length(pass.team2)){
    pass.temp <- event.temp[[pass.team2[p]]]
    possession <- pass.temp$possession
    passer <- pass.temp$player$id
    pass.location <- pass.temp$location
    pass.type <- pass.temp$pass$height$name
    receiver <- pass.temp$pass$recipient$id
    receive.location <- pass.temp$pass$end_location
    pass.length <- pass.temp$pass$length
    pass.angle <- pass.temp$pass$angle
    body.part <- pass.temp$pass$body_part$name
    
    row.toadd <- c(possession, passer, pass.location, pass.type, receiver, receive.location, pass.length, pass.angle, body.part)
    pass.team2.df <- rbind(pass.team2.df, row.toadd)
  }
  pass.team2.df <- pass.team2.df[-1,]
  pass.team2.df[,c(1:4,6:10)] <- lapply(pass.team2.df[,c(1:4,6:10)],as.numeric)
  pass.team2.df <- pass.team2.df %>% group_by("Possession") %>% mutate(seq = row_number())
  pass.team2.df$team_id <- teamids[2]
  
  pass.list <- list(pass.team1.df, pass.team2.df)
  
  match.id <- strsplit(basename(event.files[1]),"[.]")[[1]][1]
  
  event.list[[match.id]] <- list(starting.x11.list, pass.list)
  }


##ANALYSIS

# squad rotation per match / we will use competition_id = 37 and season_id = 4

matches.wsl.1819 <- all.matches.clean[which(all.matches.clean$competition.competition_id == 37 & all.matches.clean$season.season_id == 4),]
matches.wsl.1819 <- matches.wsl.1819[order(matches.wsl.1819$match_week),]

wsl.teams <- unique(matches.wsl.1819$home_team.home_team_name)

squad.rotation.list <- list()
team.starting.x11 <- list()
for(w in 1:length(wsl.teams)){
  squad.rotation.list[[wsl.teams[w]]] <- list()
  team.starting.x11[[wsl.teams[w]]] <- list()
  team.matches <- matches.wsl.1819[which(matches.wsl.1819$home_team.home_team_name == wsl.teams[w] |
                              matches.wsl.1819$away_team.away_team_name == wsl.teams[w]),]
  team.matches$GD <- team.matches$home_score - team.matches$away_score
  
  team.events.index <- which(names(event.list) %in% team.matches$match_id)
  team.events <- event.list[team.events.index]
  team.id <- as.numeric(unique(matches.wsl.1819[which(matches.wsl.1819$home_team.home_team_name == wsl.teams[w]),]$home_team.home_team_id))
  team.matches$Team.GD <- ifelse(team.matches$home_team.home_team_id == team.id, team.matches$GD, team.matches$GD*-1)
  team.matches$Result <- ifelse(team.matches$Team.GD > 0, "W",
                        ifelse(team.matches$Team.GD == 0, "D", "L"))
  
  for(i in 1:length(team.events)){
    starting.x11 <- team.events[[i]][[1]]
    starting.x11.index <- which(lapply(starting.x11, function(x) unique(x$team_id)) == team.id)
    
    team.11 <- starting.x11[[starting.x11.index]]
    team.starting.x11[[wsl.teams[w]]][[1]] <- team.11$player.name
  }
  
  num.matches <- length(team.events)
  squad.rotation <- c(0, sapply(seq(1:(num.matches-1)), function(x) length(setdiff(team.starting.x11[[w]][[x]], team.starting.x11[[w]][[x+1]]))))
  team.matches$Rotated <- squad.rotation
  squad.rotation.list[[w]] <- team.matches[,c("matches_week", "Result", "Rotated")]
}

result.colors <- c("W"="forestgreen","L"="red","D" = "yellow") #define a set of colors to use in our plot

#ggplot is where you bind the data. the aes stands for aesthetic and defines what data is bound to what part of the graph
ggplot(data=squad.rotation.list[[1]], aes(x=match_week,y=Rotated,fill=Result)) + geom_bar(stat="identity",width=0.5)+
  scale_fill_manual(values=result.colors)

all.squad.rotations <- ldply(squad.rotation.list,.id="Team") #binds all the rows of the list elements together and adds the list element name as an additional column

ggplot(data=all.squad.rotations, aes(x=match_week,y=Rotated,fill=Result)) + geom_bar(stat="identity",width=0.5)+
  scale_fill_manual(values=result.colors) + facet_grid(rows=vars(Team)) #adds a plot for each team

