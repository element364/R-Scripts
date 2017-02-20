domain <- read.csv("DomainAnal.csv", encoding="UTF-8")

getAuthorEmdPercentage <- function (authorName) {
    authorDomains <- domain[domain$Author == authorName,]
    nrow(authorDomains[authorDomains$EMD == T,]) / nrow(authorDomains)
}

authors <- factor(domain$Author)
emd <- vector("numeric", 4)
names(emd) <- levels(authors)

emd["F-SEO"] <- getAuthorEmdPercentage("F-SEO")
emd["F-SEO Student"] <- getAuthorEmdPercentage("F-SEO Student")
emd["Puzat"] <- getAuthorEmdPercentage("Puzat")
emd['Puzat Student'] <- getAuthorEmdPercentage("Puzat Student")

emd <- emd * 100

barplot(emd, main="% EMD доменов")