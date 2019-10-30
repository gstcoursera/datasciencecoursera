complete <- function(directory, id = 1:332) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  if(grep("specdata", directory) == 1) {
    directory <- ("./specdata/")
  }
  
  ## 'id' is an integer vector indicating the monitor ID numbers
  ## to be used
  
  id_len <- length(id)
  complete_data <- rep(0, id_len)
  
  all_files <- as.character( list.files(directory) )
  file_paths <- paste(directory, all_files, sep="")
  
  ## Return a data frame of the form:
  ## id nobs
  ## 1  117
  ## 2  1041
  ## ...
  ## where 'id' is the monitor ID number and 'nobs' is the
  ## number of complete cases

  j <- 1 
  for (i in id) {
    current_file <- read.csv(file_paths[i], header=T, sep=",")
    complete_data[j] <- sum(complete.cases(current_file))
    j <- j + 1
  }
  
  result <- data.frame(id = id, nobs = complete_data)
  return(result)
} 