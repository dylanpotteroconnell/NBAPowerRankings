# Dylan O'Connell
# NBA Season Schedule Scraper


# Each game in the season will correspond to a row in our data frame
x <- data.frame(date=character(), team.home=character(), team.away=character(), 
                pts.home=integer(), pts.away=integer())
# Each page shows one month of games, so we substitute in the month name 
# to get the correct URL.
baseURL <- "http://www.basketball-reference.com/leagues/NBA_2016_games-XXX.html"
for (month in c("october", "november", "december", "january", "february", "march", "april" )){
  thepage = readLines(gsub("XXX", month, baseURL))
  # In april, some of the games shown are playoff games. We only want
  # to compile regular season games, so we simply cut off our file after
  # the specified string is found.
  if(month=="april"){
    thepage <- thepage[1:grep(">Playoffs</th", thepage)[1]]
  }
  # The first result is extraneous, so we leave it out
  # This string will only grab the lines that correspond to a game.
  z <- grep("game_start_time", thepage, value=TRUE)[-1]
  # For each game (which is a line of z)
  for(i in 1:length(z)){
    # We grab the date, and put it in the correct format.
    date <- gsub(".*month=([0-9]*)&amp;day=([0-9]*)&amp;year=([0-9]*)\\\">.*" , "\\1.\\2.\\3", z[i])
    date <- as.POSIXct(date, format="%m.%d.%Y")
    # Each team name is denoted by the 3 character all caps code. This is the 
    # basketball reference set of team codes (not the same as the ones on, say, reddit)
    team.home <- gsub(".*\\\"home_team_name\\\" csk=\\\"([A-Z]*).[0-9]*[A-Z]*\\\".*", "\\1", z[i])
    team.away <- gsub(".*\\\"visitor_team_name\\\" csk=\\\"([A-Z]*).[0-9]*[A-Z]*\\\".*", "\\1", z[i])
    pts.home <- as.numeric(gsub(".*data-stat=\\\"home_pts\\\" >([0-9]*)</td>.*", "\\1", z[i]))
    pts.away <- as.numeric(gsub(".*data-stat=\\\"visitor_pts\\\" >([0-9]*)</td>.*", "\\1", z[i]))
    x <- rbind(x, data.frame(date=date, team.home=team.home, team.away=team.away,
                             pts.home=pts.home, pts.away=pts.away))
  }
}

# Once all games have been added as rows, we save the result for easy access. 
write.table(x, "15-16-season.csv")

#z <- read.table("15-16-season.csv", as.is=TRUE)


