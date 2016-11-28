
teamnames.reddit <- c("ATL", "BKN", "BOS", "CHA", "CHI", "CLE", "DAL", "DET", "DEN", 
               "GSW", "HOU", "IND", "LAC", "LAL", "MEM", "MIA", "MIL", "MIN", 
               "NOP", "NYK", "OKC", "ORL", "PHI", "PHX", "POR", "SAC", "SAS", 
               "TOR", "UTA", "WAS")

#BKN/BRK  #CHA/CHO #PHX/PHO
teamnames.BBR <- c("ATL", "BRK", "BOS", "CHO", "CHI", "CLE", "DAL", "DET", "DEN", 
                   "GSW", "HOU", "IND", "LAC", "LAL", "MEM", "MIA", "MIL", "MIN", 
                   "NOP", "NYK", "OKC", "ORL", "PHI", "PHO", "POR", "SAC", "SAS", 
                   "TOR", "UTA", "WAS")

convertRedditToBBR <- function(x){
  x$team <- gsub("BKN", "BRK", x$team)
  x$team <- gsub("CHA", "CHO", x$team)
  x$team <- gsub("PHX", "PHO", x$team)
  x
}

convertBBRToReddit <- function(x){
  x$team <- gsub("BRK", "BKN", x$team)
  x$team <- gsub("CHO", "CHA", x$team)
  x$team <- gsub("PHO", "PHX", x$team)
  x
}

# scrapePowerRankings1617 <- function(URL, date){
#   source <- "reddit"
#   x <- data.frame(rankingdate=character(), source=character(), team=character(), teamrank=integer(), delta=character(), record=character())
#   thepage = readLines(URL)
#   rankingdate <- as.POSIXct(date, format="%m.%d.%Y")
#   for(team in teamnames.reddit){
#     index <- grep(paste("<td align=\"left\"><a href=\"/", team, 
#                         "\"></a> [A-z,0-9]+</td>", sep=""), thepage)#, value=TRUE)
#     teamrank <- gsub("<td align=\"right\">([+,-]?[0-9]+).?</td>", "\\1", thepage[index-1])
#     delta <- gsub("<td align=\"left\">([+,-]?[0-9,-]+)?</td>", "\\1", thepage[index+1])
#     record <- gsub("<td align=\"center\">([0-9]+)-([0-9]+)</td>", "\\1-\\2", thepage[index+2])
#     x <- rbind(x, data.frame(rankingdate=rankingdate, source=source, team=team, teamrank=as.integer(teamrank), delta=delta, record=record))
#   }
#   x
# }

scrapePowerRankings1516 <- function(URL, date){
  source <- "reddit"
  x <- data.frame(rankingdate=character(), source=character(), team=character(), teamrank=integer(), 
                  delta=character(), record.w=integer(), record.l=integer(), avg.PD=numeric())
  thepage = readLines(URL)
  rankingdate <- as.POSIXct(date, format="%m.%d.%Y")
  standings <- convertBBRToReddit(getHistoricalStandings(rankingdate))
  for(team in teamnames.reddit){
    index <- grep(paste("<td align=\"left\"><a href=\"/", team, 
                        "\"></a> [A-z,0-9]+</td>", sep=""), thepage)#, value=TRUE)
    teamrank <- gsub("<td align=\"right\">([+,-]?[0-9]+).?</td>", "\\1", thepage[index-1])
    record.w <- standings[standings$team==team,]$wins
    record.l <- standings[standings$team==team,]$losses
    avg.PD <- standings[standings$team==team,]$avg.PD
    x <- rbind(x, data.frame(rankingdate=rankingdate, source=source, team=team, teamrank=as.integer(teamrank), delta=NA, 
                             record.w=record.w, record.l=record.l, avg.PD=avg.PD))
  }
  convertRedditToBBR(x)
}

#http://www.basketball-reference.com/friv/standings.cgi?month=2&day=24&year=2016&lg_id=NBA


# Given a date in history, this uses basketball reference to return a dataframe
# containing the current standings of all teams in the league
getHistoricalStandings <- function(date){
  day <- format(date, "%d")
  month <- format(date, "%m")
  year <- format(date, "%Y")
  URL <- paste("http://www.basketball-reference.com/friv/standings.cgi?month=", month,"&day=", day, "&year=", year, "&lg_id=NBA", sep="")
  thepage = readLines(URL)
  x <- data.frame(date=character(), team=character(), wins=integer(), losses=integer(), avg.PD=numeric())
  thepage <- grep("full_table", thepage, value=TRUE)
  if(length(thepage)==30){
    for(team in teamnames.BBR){
      #print(team)
      z <- grep(paste("/teams/", team, "/", sep=""), thepage, value=TRUE)
      wins <- as.numeric(gsub(".*data-stat=\\\"wins\\\" >([0-9]*)<.*", "\\1", z))
      losses <- as.numeric(gsub(".*data-stat=\\\"losses\\\" >([0-9]*)<.*", "\\1", z))
      avg.PF <- as.numeric(gsub(".*data-stat=\"pts_per_g\\\" >([0-9]*\\.?[0-9]*)<.*", "\\1", z))
      #data-stat="pts_per_g" >113.9<
      avg.PA <- as.numeric(gsub(".*data-stat=\"opp_pts_per_g\\\" >([0-9]*\\.?[0-9]*)<.*", "\\1", z))
      avg.PD <- avg.PF - avg.PA
      x <- rbind(x, data.frame(date=date, team=team, 
                               wins=wins, losses=losses, avg.PD=avg.PD))
    }
  } else {
    x <- data.frame(date=rep(date,30), team=teamnames.BBR, 
               wins=rep(NA,30), losses=rep(NA,30), avg.PD=rep(NA,30))
  }
  x
}

rd <- data.frame(rankingdate=character(), source=character(), team=character(), teamrank=integer(), delta=character(), record=character())

URL <- "https://www.reddit.com/r/nba/comments/3qbhvw/official_rnba_power_rankings_1_102615/"
date <- "10.26.2015"
rd <- rbind(rd, scrapePowerRankings1516(URL, date))

URL <- "https://www.reddit.com/r/nba/comments/3s61nc/official_rnba_power_rankings_2_110915/"
date <- "11.09.2015"
rd <- rbind(rd, scrapePowerRankings1516(URL, date))

#3 
URL <- "https://www.reddit.com/r/nba/comments/3tys9b/official_rnba_power_rankings_3_112315/"
date <- "11.23.2015"
rd <- rbind(rd, scrapePowerRankings1516(URL, date))

#4
URL <- "https://www.reddit.com/r/nba/comments/3vtr13/official_rnba_power_rankings_4_12715/"
date <- "12.07.2015"
rd <- rbind(rd, scrapePowerRankings1516(URL, date))

#5
URL <- "https://www.reddit.com/r/nba/comments/3xqad5/official_rnba_power_rankings_5_20151221/"
date <- "12.21.2015"
rd <- rbind(rd, scrapePowerRankings1516(URL, date))

#6
URL <- "https://www.reddit.com/r/nba/comments/3zfzht/official_rnba_power_rankings_6_20160104/"
date <- "01.04.2016"
rd <- rbind(rd, scrapePowerRankings1516(URL, date))

#7 
URL <- "https://www.reddit.com/r/nba/comments/41k940/official_rnba_power_rankings_7_20160118/"
date <- "01.18.2016"
rd <- rbind(rd, scrapePowerRankings1516(URL, date))

#8 
URL <- "https://www.reddit.com/r/nba/comments/43ptj6/official_rnba_power_rankings_8_20160201/"
date <- "02.01.2016"
rd <- rbind(rd, scrapePowerRankings1516(URL, date))

#9 
URL <- "https://www.reddit.com/r/nba/comments/45g36u/official_rnba_power_rankings_9_all_star_break/"
date <- "02.12.2016"
rd <- rbind(rd, scrapePowerRankings1516(URL, date))

#10
URL <- "https://www.reddit.com/r/nba/comments/48bsts/official_rnba_power_rankings_10_leap_day_edition/"
date <- "02.29.2016"
rd <- rbind(rd, scrapePowerRankings1516(URL, date))

#11
URL <- "https://www.reddit.com/r/nba/comments/4aehcu/official_rnba_power_rankings_11_crunch_time/"
date <- "03.14.2016"
rd <- rbind(rd, scrapePowerRankings1516(URL, date))

#12
URL <- "https://www.reddit.com/r/nba/comments/4cbmg4/official_rnba_power_rankings_12_32816/"
date <- "03.28.2016"
rd <- rbind(rd, scrapePowerRankings1516(URL, date))

#13
URL <- "https://www.reddit.com/r/nba/comments/4edc3v/official_rnba_power_rankings_13_41116/"
date <- "04.11.2016"
rd <- rbind(rd, scrapePowerRankings1516(URL, date))

#14
#URL <- "https://www.reddit.com/r/nba/comments/4edc3v/official_rnba_power_rankings_13_41116/"
#date <- "04.11.2016"
#rd <- rbind(rd, scrapePowerRankings1516(URL, date))

#URL <- "https://www.reddit.com/r/nba/comments/596vdf/official_rnba_power_rankings_0_102416/"
#date <- "10.24.2016"
#qrd <- rbind(rd, scrapePowerRankings1617(URL, date))

write.table(rd, file="powerrankings15-16.csv")

