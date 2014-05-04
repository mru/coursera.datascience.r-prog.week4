rankhospital <- function(state, outcome, rank) {
  if(outcome == "heart attack") outcome.form <- "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack"
  else if(outcome == "heart failure") outcome.form <- "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure"
  else if(outcome == "pneumonia") outcome.form <- "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia"
  else stop("invalid outcome")
  if(is.numeric(rank) && rank > 0) rank  <- floor(rank)
  else if(rank == "best") rank  <- 1
  else if(rank == "worst") rank <- "worst"
  else stop("Invalid Rank Value")
  outcome.orig <- read.csv("outcome-of-care-measures.csv", colClasses="character")
  outcome.orig[, 11]  <- as.numeric(outcome.orig[, 11])
  outcome.orig[, 17]  <- as.numeric(outcome.orig[, 17])
  outcome.orig[, 23]  <- as.numeric(outcome.orig[, 23])
  outcome.orig  <- outcome.orig[complete.cases(outcome.orig), ]
  outcome.state <- outcome.orig[, "State"] == state
  a  <- any(outcome.state)
  if(!a) stop("Invalid State")
  outcome.orig  <- outcome.orig[outcome.state, ]
  outcome.ordered <- outcome.orig[order(outcome.orig[,outcome.form], outcome.orig[,"Hospital.Name"]),]
  if(rank == "worst") rank  <- nrow(outcome.ordered)
  outcome.ordered[rank, "Hospital.Name"]
  ## subset(outcome.orig, Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack == outcome.min)
  ## outcome.agg <- aggregate(as.formula(paste(outcome.form, "~", "Hospital.Name", sep="")), outcome.orig, FUN=min)
  ## outcome.min <- merge(outcome.agg, outcome.orig)
  ## outcome.min[1,]$Hospital.Name
  ## Return hospital name in that state with lowest 30-day death  
}
