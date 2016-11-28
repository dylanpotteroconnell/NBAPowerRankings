# NBA Result Predicting
# Rad in the powerrankings data and the results data.
# Then calculate which of our predictions would be correct for each game 

# See PowerRankingsScraper.R to generate these two files
rd <- read.table("powerrankings15-16.csv")
results <- read.table("15-16-season.csv")

# When we read in the table, we do not properly convert dates to POSIXct
results$date <- as.POSIXct(results$date)
rd$rankingdate <- as.POSIXct(rd$rankingdate)

#rd$pct <- NA
#rd$pct[!is.na(rd$record.w)] <- rd[!is.na(rd$record.w),]$record.w/
#  (rd[!is.na(rd$record.w),]$record.w+rd[!is.na(rd$record.w),]$record.l)

# For each game, we record whether our prediction would be correct using 
# the last power ranking (PR), their win loss record at the time of the last 
# power ranking (WL), or their Point Differential at the last power rankin (PD)
results$correct.PR <- NA
results$correct.WL <- NA
results$correct.PD <- NA

# We manually run through each game in the season
for(i in 1:length(results$date)){
  # Find most recent power ranking
  # This simply records the last ranking of that team before this date.
  # Standings MUST be sorted by ascending date for this to work
  for(j in 1:length(rd$rankingdate)){ #(rec in rd){
    if(rd[j,]$rankingdate < results[i,]$date & rd[j,]$team==results[i,]$team.home){
      lastranking.home <- rd[j,]
    }
    if(rd[j,]$rankingdate < results[i,]$date & rd[j,]$team==results[i,]$team.away){
      lastranking.away <- rd[j,]
    }
  }
  # For now, if the game occurred in the period where the last power ranking
  # did not have win/loss/point differential stats (at the start of the year),
  # we leave it as NA, as we cannot compare predictions.
  if(!is.na(lastranking.home$record.w) & !is.na(lastranking.away$record.w)){
    # If the power ranking correctly predicted outcome
    if((results[i,]$pts.home > results[i,]$pts.away &
        lastranking.home$teamrank < lastranking.away$teamrank) |
       (results[i,]$pts.away > results[i,]$pts.home &
        lastranking.away$teamrank < lastranking.home$teamrank)){
      results$correct.PR[i] <- TRUE
    }
    if((results[i,]$pts.home < results[i,]$pts.away &
        lastranking.home$teamrank < lastranking.away$teamrank) |
       (results[i,]$pts.away < results[i,]$pts.home &
        lastranking.away$teamrank < lastranking.home$teamrank)){
      results$correct.PR[i] <- FALSE
    }
    # If avg.PD got it right
    if((results[i,]$pts.home > results[i,]$pts.away &
        lastranking.home$avg.PD > lastranking.away$avg.PD) |
       (results[i,]$pts.away > results[i,]$pts.home &
        lastranking.away$avg.PD > lastranking.home$avg.PD)){
      results$correct.PD[i] <- TRUE
    }
    # if avg.PD got it wrong
    if((results[i,]$pts.home < results[i,]$pts.away &
        lastranking.home$avg.PD > lastranking.away$avg.PD) |
       (results[i,]$pts.away < results[i,]$pts.home &
        lastranking.away$avg.PD > lastranking.home$avg.PD)){
      results$correct.PD[i] <- FALSE
    }
    # If WL record got it right
    if((results[i,]$pts.home > results[i,]$pts.away &
        lastranking.home$record.w - lastranking.home$record.l >
        lastranking.away$record.w - lastranking.away$record.l) |
       (results[i,]$pts.away > results[i,]$pts.home &
        lastranking.away$record.w - lastranking.away$record.l > 
        lastranking.home$record.w - lastranking.home$record.l)){
      results$correct.WL[i] <- TRUE
    }
    # if WL record got it wrong
    if((results[i,]$pts.home < results[i,]$pts.away &
        lastranking.home$record.w - lastranking.home$record.l >
        lastranking.away$record.w - lastranking.away$record.l) |
       (results[i,]$pts.away < results[i,]$pts.home &
        lastranking.away$record.w - lastranking.away$record.l >
        lastranking.home$record.w - lastranking.home$record.l)){
      results$correct.WL[i] <- FALSE
    }
  }
}

write.table(results, "prediction-16season.csv")
#q <- read.table("prediction-16season.csv")

summary(results$correct.PR)
summary(results$correct.WL)
summary(results$correct.PD)
prop.test(c(737,742),c(388+737, 742+389), alternative="less", correct=FALSE)

