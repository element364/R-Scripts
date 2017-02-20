#install.packages(c('Quandl', 'reshape2', 'ggplot2'))

library(Quandl)
library(reshape2)
library(ggplot2)

Quandl.api_key("hLPJ4MgVuz5d5h_e2b-N")

byr_usd_data <- Quandl("CUR/BYR", start_date = "2015-01-01")
rur_usd_data <- Quandl("CUR/RUB", start_date = "2015-01-01")
g <- ggplot(byr_usd_data, aes(RATE), xlab = "Белки", ylab = "USD")
g <- g + geom_line(aes(x = DATE, y = RATE), data = byr_usd_data, color = "red")
g <- g + geom_line(aes(x = DATE, y = RATE), data = rur_usd_data, color = "green")

m <- lm(byr_usd_data$RATE ~ rur_usd_data$RATE)
predicted <- predict(m, newdata = byr_usd_data$DATE)
g <- g + geom_line(aes(x = DATE, y = predicted), data = byr_usd_data, color = "yellow")