
# Write a function called best that take two arguments: 
# the 2-character abbreviated name of a state and an outcome name. 
# The function returns a character vector 
# with the name of the hospital that has the best (i.e. lowest) 30-day mortality 
# for the specified outcome in that state. 

# The outcomes can be one of "heart attack", "heart failure", or "pneumonia"
best <- function(state, outcome) 
{
  ## Read outcome data
  outcome.data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  
  ## Check that state and outcome are valid
  if(!(state %in% outcome.data$State ))
  {
    stop("invalid state")
  }
  
  if(outcome == "heart attack") idx <- 11
  else if(outcome == "heart failure") idx <- 17
  else if(outcome == "pneumonia") idx <- 23
  else stop("invalid outcome")
  
  outcome.data[, idx] <- as.numeric(outcome.data[, idx])
  outcome.data <- subset(outcome.data, 
                         !is.na(outcome.data[,idx]) & State == state)
  
  hospital.data <- read.csv("hospital-data.csv", colClasses = "character")
  hospital.data <- subset(hospital.data, State == state)
  
  # merge the two datasets together 
  # to match the Hospital.Ownership variable to the death rate data
  outcome.hospital <- merge(outcome.data, hospital.data, by = "Hospital.Name")
  
  death <- as.numeric(outcome.hospital[, idx])
  Hospital.Name <- factor(outcome.hospital$Hospital.Name)
  
  data <- data.frame("Hospital.Name" = Hospital.Name, "rate" = death)
  
  death.rate <- tapply(data$rate, INDEX=list(data$Hospital.Name),FUN=sum)
  
  names(sort(death.rate))[1]
  ## Return hospital name in that state with lowest 30-day death rate
}


## Read outcome data
## Check that state and outcome are valid
## Return hospital name in that state with the given rank
## 30-day death rate
rankhospital <- function(state, outcome, num) 
{
  ## Read outcome data
  outcome.data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  
  ## Check that state and outcome are valid
  if(!(state %in% outcome.data$State ))
  {
    stop("invalid state")
  }
  
  if(outcome == "heart attack") idx <- 11
  else if(outcome == "heart failure") idx <- 17
  else if(outcome == "pneumonia") idx <- 23
  else stop("invalid outcome")
  
  outcome.data[, idx] <- as.numeric(outcome.data[, idx])
  outcome.data <- subset(outcome.data, 
                         !is.na(outcome.data[,idx]) & State == state)
  
  hospital.data <- read.csv("hospital-data.csv", colClasses = "character")
  hospital.data <- subset(hospital.data, State == state)
  
  # merge the two datasets together 
  # to match the Hospital.Ownership variable to the death rate data
  outcome.hospital <- merge(outcome.data, hospital.data, by = "Hospital.Name")
  
  death <- as.numeric(outcome.hospital[, idx])
  Hospital.Name <- factor(outcome.hospital$Hospital.Name)
  
  data <- data.frame("Hospital.Name" = Hospital.Name, "rate" = death)
  
  death.rate <- tapply(data$rate, INDEX=list(data$Hospital.Name),FUN=sum)
  
  ranks <- names(sort(death.rate))
  
  if(num == "best") ranks[1]
  else if(num == "worst") tail(ranks, n=1)
  else 
  {
    n <- as.numeric(num)
    
    if(!is.na(n) && n <= length(ranks)) ranks[n]
    else NA
  }
}

## Read outcome data
## Check that state and outcome are valid
## For each state, find the hospital of the given rank
## Return a data frame with the hospital names and the
## (abbreviated) state name
rankall <- function(outcome, num = "best") 
{
  outcome.data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  States <- names(table(outcome.data$State))
  
  hospitals <- vector()
  
  for(state in States)
  {
    hospitals <- append(hospitals, rankhospital(state, outcome, num))
  }
  
  data.frame("state" = States, "hospital" = hospitals)
}