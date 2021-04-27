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

