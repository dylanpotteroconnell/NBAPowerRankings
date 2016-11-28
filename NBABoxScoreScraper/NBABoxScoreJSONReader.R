library("rjson")
#http://stats.nba.com/stats/boxscoreplayertrackv2/?GameID=0021301217
#http://stats.nba.com/stats/boxscoresummaryv2?GameID=0021501205
#http://stats.nba.com/stats/boxscoretraditionalv2?EndPeriod=10&EndRange=28800&GameID=0021301217&RangeType=0&Season=2013-14&SeasonType=Regular+Season&StartPeriod=1&StartRange=0
#json_file <- "http://stats.nba.com/stats/boxscoretraditionalv2?EndPeriod=10&EndRange=28800&GameID=0021301217&RangeType=0&Season=2013-14&SeasonType=Regular+Season&StartPeriod=1&StartRange=0"
#json_data <- fromJSON(paste(readLines(json_file), collapse=""))

fetchBoxScore= function(gameID ="0021501205", season = "2013-2014"){
  json_file <- paste("http://stats.nba.com/stats/boxscoretraditionalv2?EndPeriod=10&EndRange=28800&GameID=", gameID, "&RangeType=0&Season=", season, "&SeasonType=Regular+Season&StartPeriod=1&StartRange=0", sep="")
  json_data <- fromJSON(gsub(pattern = "null", 
                             replacement = "\"NA\"", 
                             paste(readLines(json_file), 
                                   collapse="")))
  
  BSD <- json_data$resultSets[[1]]$rowSet
  
  #for(i in 1:length(BSD))
  #  for(j in 1:length(BSD[[i]])) # second level
  #    if(is.null(BSD[[i]][[j]])) BSD[[i]][[j]]<-NA
  
  boxscore <- data.frame(matrix(unlist(BSD),
                                byrow = TRUE,
                                ncol = length(json_data$resultSets[[1]]$headers)))
  
  colnames(boxscore) <- json_data$resultSets[[1]]$headers
  
  # CLEANING
  # The data was all factors. Turn the columns into the right data types.
  
  boxscore$GAME_ID <- as.integer(as.character(boxscore$GAME_ID))
  boxscore$TEAM_ID <- as.integer(as.character(boxscore$TEAM_ID))
  boxscore$PLAYER_ID <- as.integer(as.character(boxscore$PLAYER_ID))
  boxscore$PLAYER_NAME <- as.character(boxscore$PLAYER_NAME)
  boxscore$MIN <- as.character(boxscore$MIN)
  boxscore$FGM <- as.integer(as.character(boxscore$FGM))
  boxscore$FGA <- as.integer(as.character(boxscore$FGA))
  boxscore$FG_PCT <- as.numeric(as.character(boxscore$FG_PCT))
  boxscore$FG3M <- as.integer(as.character(boxscore$FG3M))
  boxscore$FG3A <- as.integer(as.character(boxscore$FG3A))
  boxscore$FG3_PCT <- as.numeric(as.character(boxscore$FG3_PCT))
  boxscore$FTM <- as.integer(as.character(boxscore$FTM))
  boxscore$FTA <- as.integer(as.character(boxscore$FTA))
  boxscore$FT_PCT <- as.numeric(as.character(boxscore$FT_PCT))
  boxscore$OREB <- as.integer(as.character(boxscore$OREB))
  boxscore$DREB <- as.integer(as.character(boxscore$DREB))
  boxscore$REB <- as.integer(as.character(boxscore$REB))
  boxscore$AST <- as.integer(as.character(boxscore$AST))
  boxscore$STL <- as.integer(as.character(boxscore$STL))
  boxscore$BLK <- as.integer(as.character(boxscore$BLK))
  boxscore$TO <- as.integer(as.character(boxscore$TO))
  boxscore$PF <- as.integer(as.character(boxscore$PF))
  boxscore$PTS <- as.integer(as.character(boxscore$PTS))
  boxscore$PLUS_MINUS <- as.integer(as.character(boxscore$PLUS_MINUS))
  return(boxscore)
}
x <- fetchBoxScore()







