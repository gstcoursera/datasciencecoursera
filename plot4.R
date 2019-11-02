source("load_dataset.R") # ignore warnings generated
png(filename = "plot4.png", width = 480, height = 480, units = "px", bg = "transparent")
par(mfrow = c(2, 2))
## Top-left Plot
plot(DateTime, Global_active_power, type = "l", xlab = "", ylab = "Global Active Power")
## Top-right Plot
plot(DateTime, Voltage, type = "l", xlab = "datetime", ylab = "Voltage")
## Bottom-left Plot
plot(DateTime, Sub_metering_1, type = "l", col = "black", xlab = "", ylab = "Energy sub metering")
lines(DateTime, Sub_metering_2, col = "red")
lines(DateTime, Sub_metering_3, col = "blue")
# Fix border
legend("topright", bty = "n", col = c("black", "red", "blue"), c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"), lwd = 1)
## Bottom-right Plot
plot(DateTime, Global_reactive_power, type = "l", col = "black", xlab = "datetime", ylab = colnames(newdata)[4])
dev.off()
