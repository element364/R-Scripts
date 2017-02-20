source("readData.R")
data <- readData()

# preparing png file
plotWidth = 480
plotHeight = 480
png("plot3.png", width = plotWidth, height = plotHeight, bg = "white")

# plotting
plot(data$DateTime, data$Sub_metering_1, col = "black", type = "l", ylab = "Energy sub metering", xlab = "")
points(data$DateTime, data$Sub_metering_2, col = "red", type = "l")
points(data$DateTime, data$Sub_metering_3, col = "blue", type = "l")
legend("topright", lty = 1, col = c("black", "red", "blue"), legend = c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"))

dev.off()