pollutantmean <- function(directory, pollutant, id = 1:332) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files

  if(grep("specdata", directory) == 1) {
    directory <- ("./specdata/")
  }

  ## 'pollutant' is a character vector of length 1 indicating
  ## the name of the pollutant for which we will calculate the
  ## mean; either "sulfate" or "nitrate".

  mean_vector <- c()

  ## 'id' is an integer vector indicating the monitor ID numbers
  ## to be used

  all_files <- as.character( list.files(directory) )
  file_paths <- paste(directory, all_files, sep="")

  for(i in id) {
    current_file <- read.csv(file_paths[i], header=T, sep=",")
    head(current_file)
    pollutant
    na_removed <- current_file[!is.na(current_file[, pollutant]), pollutant]
    mean_vector <- c(mean_vector, na_removed)
  }

  ## Return the mean of the pollutant across all monitors list
  ## in the 'id' vector (ignoring NA values)

  result <- mean(mean_vector)
  return(round(result, 3)) 
}
