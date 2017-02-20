install.packages("ggplot2")
library(ggplot2)

OutcomeGoogleDocsUrl <- "https://docs.google.com/spreadsheets/d/15-jx2jVDzgww0sYT3k2OAOgbN3CZWWXwMEw5c9atoxM/pub?gid=0&single=true&output=csv"
byr_usd <- 17845

readOutcomeData <- function () {
  download.file(OutcomeGoogleDocsUrl, destfile = "outcome_from_google.csv")
  outcome_data <- read.csv("outcome_from_google.csv", encoding = "UTF-8")
  outcome_data <- completeOutcomeData(outcome_data)
  outcome_data
}

completeOutcomeData <- function(t) {
  t$Цена <- as.character(t$Цена)
  t$Цена <- gsub("[^0-9$]", "", t$Цена)

  for (r in 1:nrow(t)) {
    if (t[r, ]$День.недели == '') {
      t[r, ]$День.недели = t[r - 1, ]$День.недели
    }
    
    if (t[r, ]$Дата == '') {
      t[r, ]$Дата = t[r - 1, ]$Дата
    }
    
    if (is.na(t[r, ]$Сеанс) | t[r, ]$Сеанс == '') {
      t[r, ]$Сеанс = t[r - 1, ]$Сеанс
    }
    
    if (r > 1 & t[r, ]$Место == '') {
      t[r, ]$Место = t[r - 1, ]$Место
    }
    
    if (substring(t[r, ]$Цена, nchar(t[r, ]$Цена)) == '$') {
      t[r, ]$Цена <- as.numeric(substring(t[r, ]$Цена, 1, nchar(t[r, ]$Цена) - 1)) * byr_usd
    }
  }
  
  t$День.недели <- factor(t$День.недели)
  #t$Дата <- as.Date(t$Дата, "%d.%m.%y")
  t$Цена <- as.numeric(t$Цена)
  
  t
}

build_bar <- function (t) {
  t$Сеанс <- factor(t$Сеанс)
  ggplot(t, aes(Дата, fill = Сеанс)) + geom_bar(aes(y = Цена), stat = "identity") +  scale_y_continuous(labels=fancy_scientific) 
}

fancy_scientific <- function(l) {
  # turn in to character string in scientific notation
  l <- format(l, scientific = TRUE)
  # quote the part before the exponent to keep all the digits
  l <- gsub("^(.*)e", "'\\1'e", l)
  # turn the 'e+' into plotmath format
  l <- gsub("e", "%*%10^", l)
  # return this as an expression
  parse(text=l)
}