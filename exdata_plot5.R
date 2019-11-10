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

# Assign variable to get only the Baltimore emissions
emissionsBaltimore <- subset(NEI, fips == "24510")

# Get the motor vehicle codes
vehicles <- subset(SCC, grepl("Mobile", EI.Sector), ignore.case=T)

# Filter emissions to get only motor vehicles related
vehicleEmissions <- subset(emissionsBaltimore, SCC %in% vehicles$SCC)

# Assign variable to get motor vehicles emissions by year
totalEmissions <- aggregate(Emissions ~ year, vehicleEmissions, sum)

# Structure the png file 
png("exdata_plot5.png", width = 480, height = 480)

# Make the plot
ggplot(totalEmissions, aes(x = year, y = Emissions/10^2)) + 
  geom_line(colour = "steelblue") + 
  geom_point(colour = "blue") +
  xlab("Year") + 
  ylab(expression(PM[2.5] ~ " (hundred tons)")) +
  ggtitle(expression("Annual Motor Vehicles emission levels of" ~ PM[2.5] ~ " in Baltimore")) +
  theme(plot.title = element_text(face="bold", size=15),
        legend.title = element_text(size=15),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))

# Close and generate the png file
dev.off()