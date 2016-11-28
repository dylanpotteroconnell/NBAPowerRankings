library("rjson")

#http://stats.nba.com/stats/scoreboardV2?DayOffset=0&LeagueID=00&gameDate=10%2F13%2F2016
require(RSelenium)
RSelenium::startServer()
remDr <- remoteDriver()
remDr$open()
remDr$navigate("http://stats.nba.com/stats/scoreboardV2?DayOffset=0&LeagueID=00&gameDate=10%2F13%2F2016")

#doc <- remDr$getPageSource()[[1]]
#require(rvest)
#current_doc <- read_html(doc)
