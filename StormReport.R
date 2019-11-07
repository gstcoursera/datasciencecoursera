# Get the dataset
filename <- "./repdata_data_StormData.csv"
if(!file.exists(filename)){
  fileUrl <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2"
  zipFile <- "./repdata_data_StormData.csv.bz2"
  download.file(fileUrl,destfile = zipFile, method = "curl")
  unzip(zipFile)
}

require(dplyr)

# Load the csv file
data <- read.csv(filename)

# Examine the columns
colnames(data)

# Keep only columns needed for the analysis
data <- select(data, EVTYPE, FATALITIES, INJURIES, PROPDMG, PROPDMGEXP, CROPDMG, CROPDMGEXP)

# check new data
str(data)

# Create a dataset to store the total of fatalities and injuries per weather event
totalHealthEffects <- data %>% select(EVTYPE, FATALITIES, INJURIES) %>% 
  group_by(EVTYPE) %>%
  summarise_at(vars(FATALITIES, INJURIES), sum) %>%
  mutate(TOTAL = FATALITIES + INJURIES) %>%
  arrange(desc(TOTAL))

# Include only the top 10
totalHealthEffects <- totalHealthEffects[1:10, ]

# Create a function to fix values of PROPDMGEXP and CROPDMGEXP
fixValues <- function(x, y) { 
  if (x > 0) {
    data[, x] <- as.character(data[, toupper(x)])
    data[data[, x] == "B", x] <- "9"
    data[data[, x] == "M", x] <- "6"
    data[data[, x] == "K", x] <- "3"
    data[data[, x] == "H", x] <- "2"
    data[data[, x] == "", x] <- "0"
    data[, x] <- 10^(as.numeric(data[, x]))
    data[is.na(data[, x]), x] <- 0
  }
  if (y > 0) {
    data[, y] <- as.character(data[, toupper(y)])
    data[data[, y] == "B", y] <- "9"
    data[data[, y] == "M", y] <- "6"
    data[data[, y] == "K", y] <- "3"
    data[data[, y] == "H", y] <- "2"
    data[data[, y] == "", y] <- "0"
    data[, y] <- 10^(as.numeric(data[, y]))
    data[is.na(data[, y]), y] <- 0
  }
  return(data)
}

# Fix values of PROPDMGEXP and CROPDMGEXP
data <- fixValues("PROPDMGEXP", "CROPDMGEXP")

# Create a dataset to store the total of economy effects per weather event
totalEconomyEffects <- data %>% select(EVTYPE, PROPDMGEXP, PROPDMG, CROPDMG, CROPDMGEXP) %>%
  mutate(PROPDMG = PROPDMG * PROPDMGEXP, CROPDMG= CROPDMG * CROPDMGEXP) %>% 
  group_by(EVTYPE) %>% 
  summarise_at(vars(PROPDMG, CROPDMG), sum) %>% 
  mutate(TOTAL = PROPDMG + CROPDMG) %>% 
  arrange(desc(TOTAL))

# Reorder columns for better graph presentation
totalEconomyEffects = totalEconomyEffects[, c("EVTYPE", "CROPDMG", "PROPDMG", "TOTAL")]

# Include only the top 10
totalEconomyEffects <- totalEconomyEffects[1:10, ]

# Top 10 most harmful events for population health
totalHealthEffects

# Graphic presentation
barplot(t(as.matrix(totalHealthEffects[,4:2])),
        names.arg = totalHealthEffects$EVTYPE,
        las=3,
        cex.names = 0.5,
        beside = TRUE,
        ylab = "Number of Fatalities/Injuries",
        main = "Top 10 most harmful events for population health",
        legend = c("Total", "Injuries", "Fatalities"),
        col = c("Red", "Green", "Blue"))

# Top 10 most harmful events for economy
totalEconomyEffects

# Graphic presentation
barplot(t(as.matrix(totalEconomyEffects[,4:2])),
        names.arg = totalEconomyEffects$EVTYPE,
        las=3,
        cex.names = 0.5,
        beside = TRUE,
        ylab = "Cost in $",
        main = "Top 10 most harmful events for Economy",
        legend = c("Total", "Property", "Crop"),
        col = c("Red", "Green", "Blue"))
