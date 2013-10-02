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