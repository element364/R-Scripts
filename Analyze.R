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
  t$���� <- as.character(t$����)
  t$���� <- gsub("[^0-9$]", "", t$����)

  for (r in 1:nrow(t)) {
    if (t[r, ]$����.������ == '') {
      t[r, ]$����.������ = t[r - 1, ]$����.������
    }
    
    if (t[r, ]$���� == '') {
      t[r, ]$���� = t[r - 1, ]$����
    }
    
    if (is.na(t[r, ]$�����) | t[r, ]$����� == '') {
      t[r, ]$����� = t[r - 1, ]$�����
    }
    
    if (r > 1 & t[r, ]$����� == '') {
      t[r, ]$����� = t[r - 1, ]$�����
    }
    
    if (substring(t[r, ]$����, nchar(t[r, ]$����)) == '$') {
      t[r, ]$���� <- as.numeric(substring(t[r, ]$����, 1, nchar(t[r, ]$����) - 1)) * byr_usd
    }
  }
  
  t$����.������ <- factor(t$����.������)
  #t$���� <- as.Date(t$����, "%d.%m.%y")
  t$���� <- as.numeric(t$����)
  
  t
}

build_bar <- function (t) {
  t$����� <- factor(t$�����)
  ggplot(t, aes(����, fill = �����)) + geom_bar(aes(y = ����), stat = "identity") +  scale_y_continuous(labels=fancy_scientific) 
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