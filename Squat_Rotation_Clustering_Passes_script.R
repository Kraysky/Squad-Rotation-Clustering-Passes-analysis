#loading neccessary libraries
library(rjson)
library(data.table)

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
}


