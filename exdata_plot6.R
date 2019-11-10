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

# Load required library
require(ggplot2)

# Assign variable to get only the Baltimore and Los Angeles County emissions
emissionsBaltimore <- subset(NEI, fips == "24510")
emissionsLA <- subset(NEI, fips == "06037")

# Get the motor vehicle codes
vehicles <- subset(SCC, grepl("Mobile", EI.Sector), ignore.case=T)

# Filter emissions to get only motor vehicles related
vehicleBaltimoreEmissions <- subset(emissionsBaltimore, SCC %in% vehicles$SCC)
vehicleLAEmissions <- subset(emissionsLA, SCC %in% vehicles$SCC)

# Set the city names
vehicleBaltimoreEmissions$city="Baltimore"
vehicleLAEmissions$city="Los Angeles County"

# Combine both data
vehicleEmissions <- rbind(vehicleBaltimoreEmissions,vehicleLAEmissions)

# Assign variable to get motor vehicles emissions by year and city
totalEmissions <- aggregate(Emissions ~ year + city, vehicleEmissions, sum)

# Structure the png file 
png("exdata_plot6.png", width = 640, height = 480)

# Make the plot
ggplot(totalEmissions, aes(x = year, y = Emissions, colour=city)) + 
  geom_line() + 
  geom_point() +
  xlab("Year") + 
  ylab(expression(PM[2.5] ~ " (tons)")) +
  ggtitle(expression("Annual Motor Vehicles emission levels of" ~ PM[2.5] ~ " in Baltimore and Los Angeles County")) +
  theme(plot.title = element_text(face="bold", size=15),
  legend.title = element_text(size=15),
  panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
  panel.background = element_blank(), axis.line = element_line(colour = "black"))

# Close and generate the png file
dev.off()
