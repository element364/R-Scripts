# reading data
readData <- function () {
    setwd("/Users/DmitryZhukovsky/Dropbox/Mind Maps/R Language/PA4/ExData_Plotting1/")
    
    # classes for columns
    colCls <- c("character", "character", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric")
    
    t <- read.table("household_power_consumption.txt", sep = ";",  header = T, na.strings = "?", colClasses = colCls)
    
    # convert character to date
    t$Date <- as.Date(t$Date, "%d/%m/%Y")
    
    # filter by date
    t <- t[t$Date >= "2007-02-01" & t$Date <= "2007-02-02", ]
    
    # convert date and time to datetime
    t$DateTime <- strptime(paste(t$Date, t$Time), "%Y-%m-%d %H:%M:%S")
    
    return (t)
}