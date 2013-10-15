## takes one argument, a character string indicating the cause of death. 
## return an integer representing the number of homicides from that cause in the dataset
##
count <- function(cause = NULL) {
  ## Check that "cause" is non-NULL; else throw error
  ## Check that specific "cause" is allowed; else throw error
  ## Read "homicides.txt" data file
  ## Extract causes of death
  ## Return integer containing count of homicides for that cause
  
  if(is.null(cause))
  {
    stop("cause is NULL")
  }
  
  if(grepl("[Aa]sphyxiation", cause)) cause_reg <- "[Aa]sphyxiation"
  else if(grepl("[Ss]hooting", cause)) cause_reg <- "[Ss]hooting"
  else if(grepl("[Ss]tabbing", cause)) cause_reg <- "[Ss]tabbing"
  else if(grepl("[Uu]nknown", cause)) cause_reg <- "[Uu]nknown"
  else if(grepl("[Oo]ther", cause)) cause_reg <- "[Oo]ther"
  else if(grepl("[Bb]lunt [Ff]orce", cause)) cause_reg <- "[Bb]lunt [Ff]orce"
  else stop("invalid cause")
  
  homicides <- readLines("homicides.txt")
  
  cause_reg <- paste0("<dd>[Cc]ause: ",cause_reg,"</dd>")
  
  sum(grepl(cause_reg, homicides))
}

## returns the number of homicide victims of a given age
## 
agecount <- function(age = NULL) {
  ## Check that "age" is non-NULL; else throw error
  ## Read "homicides.txt" data file
  ## Extract ages of victims; ignore records where no age is
  ## given
  ## Return integer containing count of homicides for that age
  
  if(is.null(age)) stop("age is NULL")
  else if(as.numeric(age) < 0) stop("age is negative")
  
  homicides <- readLines("homicides.txt")
  cause_reg <- paste0("(.*?) ",age,"( *)[Yy]ears [Oo]ld</dd>")
  sum(grepl(cause_reg, homicides))
}