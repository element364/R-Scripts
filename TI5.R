library(Quandl)
library(ggplot2)
library(reshape2)

Quandl.api_key("hLPJ4MgVuz5d5h_e2b-N")

byr_usd_data <- Quandl("CUR/BYR", start_date = "2011-01-01")

int_data <- data.frame(
  DATE = c("2011-08-17", "2012-08-26", "2013-08-03", "2014-07-18", "2015-07-27"),
  RATE = c(1000000, 1000000, 1437204, 5025051, 6298993)
)

int_data$DATE = as.Date(int_data$DATE)
# int_data$RATE = int_data$RATE / 100

all_data <- data.frame()
all_data <- rbind(all_data, byr_usd_data)
all_data <- rbind(all_data, int_data)

all_data_melt <- melt(data = all_data, id.vars = "DATE")

int_data$RATE <- int_data$RATE / int_data[1,]$RATE
byr_usd_data$RATE <- byr_usd_data$RATE / sort(byr_usd_data$RATE)[1]

g <- ggplot(all_data_melt, aes(RATE), xlab = "Дата", ylab = "USD")
g <- g + geom_line(aes(x = DATE, y = RATE), data = byr_usd_data, color = "red")
g <- g + geom_line(aes(x = DATE, y = RATE), data = int_data, color = "green", size = 2)
g <- g + geom_point(aes(x = DATE, y = RATE), data = int_data, color = "darkgreen", size = 3)
g <- g + ylab("USD") + xlab("Дата")
