## Load dataset
filename <- "./household_power_consumption.txt"

## Download dataset if not exist
if(!file.exists(filename)){
  fileUrl <- "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2Fhousehold_power_consumption.zip"
  zipFile <- "./household_power_consumption.zip"
  download.file(fileUrl,destfile = zipFile)
  unzip(zipFile)
}

## Fix timezone
Sys.setlocale("LC_TIME","English")

## Process dataset
data <- read.table(filename, header = TRUE, sep = ";", colClasses = c("character", "character", rep("numeric",7)), na = "?")
dim(data) # Check if dataset has 2,075,259 rows and 9 columns

## We will only be using data from the dates 2007-02-01 and 2007-02-02
attach(data)
subset <- Date == "1/2/2007" | Date == "2/2/2007"

## Create the new dataset with only these dates
newdata <- data[subset, ]
attach(newdata) # ignore warnings
dim(newdata) # values should be 2880 rows and 9 columns

## Add a new column to convert the Date and Time variables to Date/Time classes
x <- paste(Date, Time)
newdata$DateTime <- strptime(x, "%d/%m/%Y %H:%M:%S")
rownames(newdata) <- 1:nrow(newdata)
dim(newdata) # values should be 2880 rows and 10 columns
attach(newdata) # ignore warnings
