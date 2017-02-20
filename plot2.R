source("readData.R")
data <- readData();

# preparing png file
plotWidth = 480
plotHeight = 480
png("plot2.png", width = plotWidth, height = plotHeight, bg = "white")

# plotting
plot(data$DateTime, data$Global_active_power, type = "l", ylab = "Global Active Power (kilowatts)", xlab = "")

dev.off()