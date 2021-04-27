#loading neccessary libraries
library(rjson)
library(data.table)

#load data from JSON file into a list
competitions <- fromJSON(file = "C:\\Users\\pkrajewski\\Desktop\\data\\competitions.json")

#converting list into dataframe
competitions.df <- data.frame(do.call(rbind,competitions),stringsAsFactors = FALSE)

View(competitions.df)