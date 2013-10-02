getmonitor <- function(id, directory, summarize = FALSE) {
  ## 'id' is a vector of length 1 indicating the monitor ID
  ## number. The user can specify 'id' as either an integer, a
  ## character, or a numeric.
  
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'summarize' is a logical indicating whether a summary of
  ## the data should be printed to the console; the default is
  ## FALSE
  
  ## Your code here
  
  pad_id <- sprintf("%03d",as.numeric(id))
  path <- paste0(directory,"/", pad_id, ".csv")
  
  data <- read.csv(path)
  
  if(summarize)
  {
    print(summary(data))
  }
  
  data
}

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
  
  nobs <- vector()
  
  for(i in id)
  {
    data <- getmonitor(i, directory)
    nobs <- append(nobs, nrow(data[complete.cases(data),]))
  }
  
  data.frame("id" = id, "nobs" = nobs)
}

corr <- function(directory, threshold = 0) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'threshold' is a numeric vector of length 1 indicating the
  ## number of completely observed observations (on all
  ## variables) required to compute the correlation between
  ## nitrate and sulfate; the default is 0
  
  ## Return a numeric vector of correlations
  
  cor_v <- vector()
  
  filenames <- list.files(directory, pattern="*.csv")
  paths <- paste(directory, filenames, sep="/")
  
  for(path in paths)
  {
    data <- read.csv(path)
    data <- data[complete.cases(data),]
    
    if( nrow(data) > threshold)
    {
      if("sulfate" %in% names(data) && "nitrate" %in% names(data))
      {
        cor_v <- append(cor_v, cor(data$sulfate , data$nitrate))
      }
      else
      {
        msg <- paste0("sulfate or nitrate aint exist in file ", path)
        warning(msg)
      }
    }
  }
  
  cor_v
}
