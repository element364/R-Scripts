ininstall.packages("XML")
library(XML)

parseBizoomie <- function (sourceUrl) {
    result <- data.frame(category = c(), title = c(), url = c())
    
    page <- 1
    
    repeat {
        url <- sprintf("%s/page/%d/", sourceUrl,  page)
        htmlRaw <- try(htmlTreeParse(url, useInternalNodes = T), silent = T)
        if( is(htmlRaw,"try-error") ) {
            break
        }
        
        parsedNodes <- xpathApply(htmlRaw, path='//div[@class="archive-item__title"]/a')
		
		for (node in parsedNodes) {
			result <- rbind(result, data.frame(category = sourceUrl, title = xmlValue(node), url = xmlAttrs(node)["href"]))
		}
        
        print(page)
        
        page <- page + 1        
    }
    
    result
}

categories <- c("/biznes-na-absurde/", "/brachnyj-biznes/", "/medicina/", "/moda/", "/video/", "/moshennichestvo/", "/nedvizhimost/", "/dizajn-i-arxitektura/", "/neobychnye-tovary/", "/novye-texnologii/", "/eda-i-napitki/", "/obrazovanie/", "/odezhda/", "/rabota/", "/zhivotnye/", "/razvlecheniya/", "/raznye-idei/", "/internet/", "/from-users/", "/reklama/", "/restorany-i-kafe/", "/krasota-i-zdorove/", "/selskoe-xozyajstvo/", "/sport/", "/transport/", "/turizm/", "/uslugi/", "/finansy/")
texts <- data.frame(category = c(), title = c(), url = c())

for (cat in categories) {
	texts <- rbind(texts, parseBizoomie(sprintf("http://bizoomie.com/%s", cat)))
}