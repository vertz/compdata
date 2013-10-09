
hw3_1 <- function()
{
  # make a simple histogram of the 30-day death rates from heart attack 
  # (column 11 in the outcome dataset)
  
  outcome <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  
  outcome[, 11] <- as.numeric(outcome[, 11])
  
  hist(outcome[, 11], 
       main = 'Heart Attack 30-day Death Rate',
       xlab = '30-day Death Rate')
}

hw3_2 <- function()
{
  # heart attack(11), heart failure(17), and pneumonia(23)
  
  outcome <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  
  # This sets the plot window to have 3 rows and 1 column.
  par(mfrow = c(3, 1))
  
  idx <- c(11 ,17 ,23)
  title <- c('Heart Attack' ,
             'Heart Failure',
             'Pneumonia')
  
  for(i in 1:3)
  {
    x <- idx[i]
    
    outcome[, x] <- as.numeric(outcome[, x])
    
    hist(outcome[, x], xlim=range(5:20), prob=TRUE,
         main = title[i],
         xlab = '30-day Death Rate')
    
    # draw a vertical line on each histogram at the location of
    # the median for that outcome
    abline(v = median(outcome[, x], na.rm = TRUE), col = "blue", lwd = 2)
    
    # Add a smooth density estimate on top of the histogram
    # need to set prob=TRUE when calling hist
    lines(density(outcome[, x], na.rm = TRUE), col = "red", lwd = 1)
  }
}

hw3_3 <- function()
{
  # plot the hospital 30-day death rates by state
  
  outcome <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  outcome[, 11] <- as.numeric(outcome[, 11])
  
  state.table <- table(outcome$State)
  
  # Subset the original dataset 
  # exclude states that contain less than 20 hospitals
  outcome2 <- subset(outcome , state.table[State] > 20 & !is.na(outcome[,11]))
  
  death <- outcome2[, 11]
  
  # Sort the states by their median 30-day death rate
  state <- with(outcome2,reorder(State, death, median)) 
  
  # Set the x- and y-axis tick labels to be perpendicular to the axis 
  # so that the abbreviated names of all the states will appear on the plot
  par(las=2)
  
  boxplot(death ~ state,
          ylab = '30-day Death Rate',
          main = 'Heart Attack 30-day Death Rate by State')
}

hw3_4 <- function()
{
  # plot the relationship between the number of patients a hospital sees 
  # for a certain outcome and the 30-day death rate for that outcome
  
  outcome <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  hospital <- read.csv("hospital-data.csv", colClasses = "character")
  
  # merge the two datasets together 
  # to match the Hospital.Ownership variable to the death rate data
  outcome.hospital <- merge(outcome, hospital, by = "Provider.Number")
  
  death <- as.numeric(outcome.hospital[, 11]) ## Heart attack outcome
  npatient <- as.numeric(outcome.hospital[, 15])
  owner <- factor(outcome.hospital$Hospital.Ownership)
  
  library(lattice)
  
  xyplot(death ~ npatient | owner,
         xlab = 'Number of Patients Seen',
         ylab = '30-day Death Rate',
         main = 'Heart Attack 30-day Death Rate by Ownership',
         panel = function(x, y){
           panel.xyplot(x, y) 
           panel.lmline(x, y)}) 
}