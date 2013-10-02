complete <- function(directory, id = 1:332) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'id' is an integer vector indicating the monitor ID numbers
  ## to be used
  
  ## Return a data frame of the form:
  ## id nobs
  ## 1  117
  ## 2  1041
  ## ...
  ## where 'id' is the monitor ID number and 'nobs' is the
  ## number of complete cases
  
  if(!exists("getmonitor", mode="function")) source("getmonitor.R", local = TRUE)
  
  nobs <- vector()
  
  for(i in id)
  {
      data <- getmonitor(i, directory)
      nobs <- append(nobs, nrow(data[complete.cases(data),]))
  }
  
  data.frame("id" = id, "nobs" = nobs)
}