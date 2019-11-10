# Get the datasets
filename <- "./exdata_data_NEI_data.zip"
if(!file.exists(filename)){
  fileUrl <- "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2FNEI_data.zip"
  download.file(fileUrl,destfile = filename, method = "curl")
  unzip(zipFile)
}
unzip(filename)

# Read the files
NEI <- readRDS("./summarySCC_PM25.rds")
SCC <- readRDS("./Source_Classification_Code.rds")

# Assign variable to get emissions by year
totalEmissions <- aggregate(Emissions ~ year, NEI, sum)

# Structure the png file 
png("exdata_plot1.png", width = 480, height = 480)

# Make the plot
plot(totalEmissions$year, totalEmissions$Emissions/10^6,
     type="b",
     col = "steelblue",
     xlab = "Year",
     ylab=expression(PM[2.5] ~ " (millions tons)"),
     main = expression("Annual emission levels of" ~ PM[2.5] ~ "in the US"))

# Close and generate the png file
dev.off()