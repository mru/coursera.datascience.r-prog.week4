rankall <- function(outcome="heart attack", num="best") {
  rank <- num
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

  outcome.orig  <- outcome.orig[complete.cases(outcome.orig$State), ]
  outcome.ordered <- outcome.orig[order(outcome.orig[, "State"], outcome.orig[,outcome.form], outcome.orig[,"Hospital.Name"], na.last=TRUE),]
  outcome.ordered <- outcome.ordered[complete.cases(outcome.ordered[outcome.form]), ]
  X <- split(outcome.ordered[, c("State", "Hospital.Name", outcome.form)], outcome.ordered$State)
  Y <- lapply(names(X), function(x) {
    
    tmp <- X[[x]]
    rownames(tmp) <- 1:nrow(tmp)
    if(rank == "worst") rank  <- nrow(tmp)
    tmp <- tmp[rank, ]
    df <- data.frame(hospital=tmp$Hospital.Name, state=x)
    rownames(df) <-df$state 
    df
    })
  do.call(rbind, Y)
}