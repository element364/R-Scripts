library(XML)
library(igraph)

# Get domain name from url
getDomain <- function(x) strsplit(gsub("http://|https://|www\\.", "", x), "/")[[c(1, 1)]]

parsePage <- function (url, result, depths) {
  if (missing(result)) {
    print("Creating result")
    result <- list(
      frame = data.frame(),
      graph = make_empty_graph()
    )
  }
  
  last3 <- substr(url, nchar(url) - 2, nchar(url))
  if (last3 == "jpg") {
    return(result)
  }
  
  if (missing(depths)) {
    depths <- 1
  }
  
  print(sprintf("Entering %s - %d", url, depths))
  
  if (depths > 8) {
    #return(result)
  }
  
  print(sprintf("Parsing %s", url))
  
  pageDomain <- getDomain(url)
  
  htmlRaw <- try(htmlTreeParse(url, useInternalNodes = T), silent = T)
  
  if( is(htmlRaw,"try-error") ) {
    message(sprintf("  Error while parsing %s", url))
  }
  
  pageTitleNodes = xpathApply(htmlRaw, path = "//title", xmlValue)
  
  if (length(pageTitleNodes) == 0) {
    # probably its not html
    return(result)
  }
  
  pageTitle <- pageTitleNodes[[1]]
  
  print(sprintf("%s - %s", pageTitle, url))
  
  result$frame <- rbind(result$frame, data.frame(
    pageTitle = pageTitle,
    url = url,
    html = saveXML(htmlRaw)
  ))
  
  pageId <- nrow(result$frame)

  print(pageId)
  
  result$graph <- add.vertices(result$graph, 1, color = depths, attr = list(
    id = pageId,
    pageTitle = pageTitle
  ))
  
  linkNodes <- xpathApply(htmlRaw, path='//a')
  
  for (linkNode in linkNodes) {
    link <- xmlAttrs(linkNode)["href"]
    
    if (is.na(link)) {
      next()
    }
    
    linkDomain <- getDomain(link)

    if (linkDomain == pageDomain | substr(link, 1, 1) == '/') {
      if (linkDomain == '') {
        link <- paste0("http://", pageDomain, link)
      }
      
      linkIdx <- match(link, result$frame$url)
      if (is.na(linkIdx)) {
        result <- parsePage(link, result, depths + 1)
      }
      
      linkIdx <- match(link, result$frame$url)
      if (!is.na(linkIdx)) {
        result$graph <- add.edges(result$graph, c(pageId, linkIdx))
      }
    }
  }
  
  result
}

viewGraph <- function (result) {
  result$graph <- simplify(result$graph, remove.multiple = F, remove.loops = T) 
  plot(result$graph, vertex.label=V(result$graph)$pageTitle)
}

viewTable <- function (result) {
  View(result$frame)
}

#koti <- parsePage("http://musja.ru/category/kormlenie-koshek/")
koti <- parsePage("http://musja.ru/")