#install.packages("xml2")
library(xml2)
library(ggplot2)

# Parse 1 telderi page
parseTelderi <- function (page = 1) {
    result <- data.frame()
    
    url <- sprintf("http://www.telderi.ru/ru/search/index/page/%d", page)
    
    htmlRaw <- read_html(url)
    
    parsedNodes <- xml_find_all(htmlRaw, '//table[@id="sites_list"]/tr')
    #return(parsedNodes)
    for (i in 2:length(parsedNodes)) {
        node <- parsedNodes[i]
        
        # Parsing site url
        site_url <- NA
        site_url_node <- xml_find_all(node, './/span[@class="siteurl"]')
        if (length(site_url_node) > 0) {
            site_url <- xml_text(site_url_node[1])
        }
        
        # Parsing profit
        profit <- xml_text(xml_find_one(node, './/table[@class="site"]/tr[2]/td[4]'), trim = T)
        profit <- gsub("Доход", "", profit)
        profit <- gsub("руб.", "", profit)
        profit <- gsub("\\s", "", profit, perl = T)
        
        # Parsing trafic
        trafic <- xml_text(xml_find_one(node, './/table[@class="site"]/tr[2]/td[3]'), trim = T)
        trafic <- gsub("Трафик", "", trafic)
        trafic <- gsub("\\s", "", trafic)
        
        # Parsing tic and PR
        tic <- xml_text(xml_find_one(node, './/table[@class="site"]/tr[2]/td[2]/strong[1]'), trim = T)
        pr <- xml_text(xml_find_one(node, './/table[@class="site"]/tr[2]/td[2]/strong[2]'), trim = T)        

        # Parsing price
        price_node <- xml_find_one(node, './/td[4]/span[1]')
        price <- xml_text(price_node, trim = T)
        price <- gsub("руб.", "", price)
        price <- gsub("\\s", "", price)
        price_completed = nchar(xml_attr(price_node, 'class')) > 0

        # Parsing blitz price
        blitz_price = NA
        blitz_price_node = xml_find_all(node, './/td[4]/span[2]')
        if (length(blitz_price_node) > 0) {
            blitz_price <- xml_text(blitz_price_node[1])
            blitz_price <- gsub("руб.", "", blitz_price)
            blitz_price <- gsub("\\s", "", blitz_price)
        }

        result <- rbind(result, data.frame(
            telderi_url = xml_attr(xml_find_one(node, './/a[@class="click"]'), 'href'),
            site_url = site_url,
            name = xml_text(xml_find_one(node, './/div[@class="long_link"]'), trim = T),
            profit = as.integer(profit),
            trafic = as.integer(trafic),
            price = as.integer(price),
            price_completed = price_completed,
            blitz = as.integer(blitz_price)
        ))
    }
    
    result
}

# Parse telderi page range
parseTelderiPages <- function (pages) {
    result <- data.frame()
    
    for (page in pages) {
        parsedPage <- parseTelderi(page)
        result <- rbind(result, parsedPage)
    }
    
    result
}

countReturn <- function (t) {
    t$ret <- t$price / t$profit
    t <- t[order(t$ret),]
    t
}

qp <- function (anal, xVal, yVal, title) {
    qplot(anal[[xVal]], anal[[yVal]], xlab = xVal, ylab = yVal, main = title, label = anal$Ниша, size = 1) +
    geom_text(color="darkgreen", fontface=2, vjust=-1, label = anal$name) +
    geom_smooth(method="lm") +
    theme(legend.position="none")
}

#telderi <- parseTelderiPages(1:5)

