source("readData.R")
data <- readData()

# preparing png file
plotWidth = 480
plotHeight = 480
png("plota4.png", width = plotWidth, height = plotHeight, bg = "white")

# plotting
par(mfrow = c(2, 2))

plot(data$DateTime, data$Global_active_power, ylab = "Global Active Power", xlab = "", type = "l")
plot(data$DateTime, data$Voltage, ylab = "Voltage", xlab = "datetime", type = "l")

plot(data$DateTime, data$Sub_metering_1, col = "black", type = "l", ylab = "Energy sub metering", xlab = "")
points(data$DateTime, data$Sub_metering_2, col = "red", type = "l")
points(data$DateTime, data$Sub_metering_3, col = "blue", type = "l")
legend("topright", lty = 1, col = c("black", "red", "blue"), legend = c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"))

plot(data$DateTime, data$Global_reactive_power, ylab = "Global_reactive_power", xlab = "datetime", type = "l")

dev.off()