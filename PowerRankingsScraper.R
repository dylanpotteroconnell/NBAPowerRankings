# Dylan O'Connell
# NBA Power Rankings Scraper

# The set of team name codes used by reddit and basketball reference are different.
# For easy iteration, we define each of these sets of 30 codes, in alphabetical order
# The team codes used on the reddit website
teamnames.reddit <- c("ATL", "BKN", "BOS", "CHA", "CHI", "CLE", "DAL", "DET", "DEN", 
               "GSW", "HOU", "IND", "LAC", "LAL", "MEM", "MIA", "MIL", "MIN", 
               "NOP", "NYK", "OKC", "ORL", "PHI", "PHX", "POR", "SAC", "SAS", 
               "TOR", "UTA", "WAS")

# The team codes used on the basketball reference website
teamnames.BBR <- c("ATL", "BRK", "BOS", "CHO", "CHI", "CLE", "DAL", "DET", "DEN", 
                   "GSW", "HOU", "IND", "LAC", "LAL", "MEM", "MIA", "MIL", "MIN", 
                   "NOP", "NYK", "OKC", "ORL", "PHI", "PHO", "POR", "SAC", "SAS", 
                   "TOR", "UTA", "WAS")


# We write two functions, that simply scan a dataframe, and replace all
# instances of any team codes that differ between reddit and BBR, and 
# swap those codes to the other form.
convertRedditToBBR <- function(x){
  x$team <- gsub("BKN", "BRK", x$team)
  x$team <- gsub("CHA", "CHO", x$team)
  x$team <- gsub("PHX", "PHO", x$team)
  x
}
# Same as above, but taking BBR codes and converting to reddit.
convertBBRToReddit <- function(x){
  x$team <- gsub("BRK", "BKN", x$team)
  x$team <- gsub("CHO", "CHA", x$team)
  x$team <- gsub("PHO", "PHX", x$team)
  x
}

# We originally scraped the rankings from the 2016-2017 format.
# These are not in the same format as the 2015-2016 season, so
# we must change the code quite a bit. Left in for now, but do not 
# use unless adjusted. For now, we focus on the 2015-2016 season.
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

# This takes in a URL of a reddit power ranking, and the date of said ranking,
# and will return a dataframe where the rows are the team and its rank, with
# the win/loss record and point differential of that team at the date of the ranking.
# This only works for the reddit 2015-2016 power rankings
# Date should be inputted in the format "%m.%d.%Y", as a string
scrapePowerRankings1516 <- function(URL, date){
  # We record that the source is "reddit", in case we later compile this dataframe
  # with power rankings from other sites.
  source <- "reddit"
  # Each row in x will correspond to one team's ranking at a certain date.
  x <- data.frame(rankingdate=character(), source=character(), team=character(), teamrank=integer(), 
                  delta=character(), record.w=integer(), record.l=integer(), avg.PD=numeric())
  thepage = readLines(URL)
  rankingdate <- as.POSIXct(date, format="%m.%d.%Y")
  # We use getHistoricalStandings to get a list of team records and point differentials
  # on this given date. This is in BBR form, so we convert it to the reddit team codes.
  standings <- convertBBRToReddit(getHistoricalStandings(rankingdate))
  # Now, for each team, we get its ranking, and set its wins, losses, and point differential
  # using the data in standings.
  for(team in teamnames.reddit){
    index <- grep(paste("<td align=\"left\"><a href=\"/", team, 
                        "\"></a> [A-z,0-9]+</td>", sep=""), thepage)#, value=TRUE)
    teamrank <- gsub("<td align=\"right\">([+,-]?[0-9]+).?</td>", "\\1", thepage[index-1])
    record.w <- standings[standings$team==team,]$wins
    record.l <- standings[standings$team==team,]$losses
    avg.PD <- standings[standings$team==team,]$avg.PD
    # Each row of x is one ranking of a team
    x <- rbind(x, data.frame(rankingdate=rankingdate, source=source, team=team, teamrank=as.integer(teamrank), delta=NA, 
                             record.w=record.w, record.l=record.l, avg.PD=avg.PD))
  }
  # As the BBR form of team codes is more standard, before returning our set of 
  # power rankings, we convert it back to BBR form.
  convertRedditToBBR(x)
}


# Given a date in history, this uses basketball reference to return a dataframe
# containing the current standings of all teams in the league, as well as their 
# average point differential.
getHistoricalStandings <- function(date){
  day <- format(date, "%d")
  month <- format(date, "%m")
  year <- format(date, "%Y")
  # We need to tell BBR a specific date to fetch the historical standings
  URL <- paste("http://www.basketball-reference.com/friv/standings.cgi?month=", month,"&day=", day, "&year=", year, "&lg_id=NBA", sep="")
  thepage = readLines(URL)
  x <- data.frame(date=character(), team=character(), wins=integer(), losses=integer(), avg.PD=numeric())
  thepage <- grep("full_table", thepage, value=TRUE)
  # Sometimes, we may access a date that is not during the season. If there are not
  # 30 teams in the standings, we simply return the standings as all NA
  if(length(thepage)==30){
    # For each team, we get their standing information
    for(team in teamnames.BBR){
      # We only look at the line that involves the specified team
      z <- grep(paste("/teams/", team, "/", sep=""), thepage, value=TRUE)
      # We find the wins and losses of that team, as numeric variables
      wins <- as.numeric(gsub(".*data-stat=\\\"wins\\\" >([0-9]*)<.*", "\\1", z))
      losses <- as.numeric(gsub(".*data-stat=\\\"losses\\\" >([0-9]*)<.*", "\\1", z))
      # We find their average points for (PF), points against (PA), and thus
      # their average point differential per game is avg.PF-avg.PA
      avg.PF <- as.numeric(gsub(".*data-stat=\"pts_per_g\\\" >([0-9]*\\.?[0-9]*)<.*", "\\1", z))
      avg.PA <- as.numeric(gsub(".*data-stat=\"opp_pts_per_g\\\" >([0-9]*\\.?[0-9]*)<.*", "\\1", z))
      avg.PD <- avg.PF - avg.PA
      x <- rbind(x, data.frame(date=date, team=team, 
                               wins=wins, losses=losses, avg.PD=avg.PD))
    }
    # If there are not 30 teams in the standings, return NAs
  } else {
    x <- data.frame(date=rep(date,30), team=teamnames.BBR, 
               wins=rep(NA,30), losses=rep(NA,30), avg.PD=rep(NA,30))
  }
  # This function returns the completed data frame of historical standings
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

