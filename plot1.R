source("readData.R")
data <- readData();

# preparing png file
plotWidth = 480
plotHeight = 480
png("plot1.png", width = plotWidth, height = plotHeight, bg = "white")

# plotting
hist(t$Global_active_power, col = "red", main= "Global Active Power", xlab = "Global Active Power (kilowatts)")

dev.off()