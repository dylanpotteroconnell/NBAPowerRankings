# Dylan O'Connell
# NBA Season Schedule Scraper

x <- data.frame(date=character(), team.home=character(), team.away=character(), 
                pts.home=integer(), pts.away=integer())
baseURL <- "http://www.basketball-reference.com/leagues/NBA_2016_games-XXX.html"
for (month in c("october", "november", "december", "january", "february", "march", "april" )){
  thepage = readLines(gsub("XXX", month, baseURL))
  # The first result is extraneous, so we leave it out
  if(month=="april"){
    thepage <- thepage[1:grep(">Playoffs</th", thepage)[1]]
  }
  #<th colspan="9">Playoffs</th>
  z <- grep("game_start_time", thepage, value=TRUE)[-1]
  for(i in 1:length(z)){
    date <- gsub(".*month=([0-9]*)&amp;day=([0-9]*)&amp;year=([0-9]*)\\\">.*" , "\\1.\\2.\\3", z[i])
    date <- as.POSIXct(date, format="%m.%d.%Y")
    team.home <- gsub(".*\\\"home_team_name\\\" csk=\\\"([A-Z]*).[0-9]*[A-Z]*\\\".*", "\\1", z[i])
    team.away <- gsub(".*\\\"visitor_team_name\\\" csk=\\\"([A-Z]*).[0-9]*[A-Z]*\\\".*", "\\1", z[i])
    pts.home <- as.numeric(gsub(".*data-stat=\\\"home_pts\\\" >([0-9]*)</td>.*", "\\1", z[i]))
    pts.away <- as.numeric(gsub(".*data-stat=\\\"visitor_pts\\\" >([0-9]*)</td>.*", "\\1", z[i]))
    x <- rbind(x, data.frame(date=date, team.home=team.home, team.away=team.away,
                             pts.home=pts.home, pts.away=pts.away))
  }
}



write.table(x, "15-16-season.csv")

#z <- read.table("15-16-season.csv", as.is=TRUE)


