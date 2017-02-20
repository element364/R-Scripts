readDotabuffData <- function (url) {
    result <- data.frame(nm = c(), winRate = c(), pickRate = c())
    
    htmlRaw <- htmlTreeParse(url, useInternalNodes = T)
    if( is(htmlRaw,"try-error") ) {
        stop("Cant read page")
    }
    
    parsedHeroes <- xpathApply(htmlRaw, path='//table/tbody/tr')
    for (parsedHero in parsedHeroes) {
        tds <- xpathApply(parsedHero, path="td")
        heroName <- xpathApply(tds[[2]], path="a")
        hero <- data.frame(
            nm = xmlValue(heroName[[1]]),
            winRate = as.numeric(gsub("%", "", xmlValue(tds[[3]]))),
            pickRate = as.numeric(gsub("%", "", xmlValue(tds[[4]]))) 
        )
        result <- rbind(result, hero)
    }
    
    result
    
    # library(XML)
    # library(ggplot2)
    # setwd("c:/Users/Администратор/Dropbox/Mind Maps/R Language/DB")
    # source("dotabuff.R")
    # t <- readDotabuffData("dotabuff.html")
    # qplot(t$pickRate, t$winRate, xlab = "Pick rate", ylab = "Win rate", label = t$nm, size = 1) + geom_text(color="darkgreen", fontface=2, vjust=-1) + geom_smooth(method="lm") + theme(legend.position="none")
}