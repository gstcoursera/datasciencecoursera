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

# Assign variable to get only the Baltimore emissions
emissionsBaltimore <- subset(NEI, fips == "24510")

# Assign variable to get emissions by year for Baltimore
totalEmissions <- aggregate(Emissions ~ year, emissionsBaltimore, sum)

# Structure the png file 
png("exdata_plot2.png", width = 480, height = 480)

# Make the plot
plot(totalEmissions$year, totalEmissions$Emissions,
     type="b",
     col = "steelblue",
     xlab = "Year",
     ylab=expression(PM[2.5] ~ " (tons)"),
     main = expression("Annual emission levels of" ~ PM[2.5] ~ "in Baltimore"))

# Close and generate the png file
dev.off()  