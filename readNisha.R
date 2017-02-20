library(ggplot2)

#anal <- read.csv("C:/Users/Администратор/Dropbox/MFA/Nisha-mean.csv", sep=",", dec=",", encoding="UTF-8")
anal <- read.csv("Dropbox/MFA/Nisha-mean.csv", sep=",", dec=",", encoding="UTF-8")
# Удаляем строчку про Элона Маска с данными vk.com
anal <- anal[-28,]

# Построение гистограммы с линией медианы и среднми значением
bp <- function (anal, name) {
    v <- anal[[name]];
    names(v) <- substr(anal$Ниша,0, 8)
    barplot(v, las=2, main=name);
    meanValue <- mean(v)
    abline(h = meanValue)
    medianValue <- median(v)
    abline(h = medianValue, col="gray60")
}

# Гистограмма CPC
bp(anal, 'CPC')
# Гистограмма Xtool
bp(anal, 'Xtool')

# Постороение графика зависимости
qp <- function (anal, xVal, yVal, title) {
    qplot(anal[[xVal]], anal[[yVal]], xlab = xVal, ylab = yVal, main = title, label = anal$Ниша, size = 1) + geom_text(color="darkgreen", fontface=2, vjust=-1) + geom_smooth(method="lm") + theme(legend.position="none")
}

# CPC и Xtool
qp(anal, 'Xtool', 'CPC', 'CPC и Xtool')

# CPC и Интерес
qp(anal, 'Интерес', 'CPC', 'CPC и Интерес')

# CPC и тИЦ
qp(anal, 'тИЦ', 'CPC', 'CPC и тИЦ')

# CPC и PR
qp(anal, 'PR', 'CPC', 'CPC и PR')

# CPC и Возраст домена
qp(anal, 'Возраст.домена', 'CPC', 'CPC и Возраст домена')

# Xtool и Возраст домена
qp(anal, 'Возраст.домена', 'Xtool', 'Xtool и Возраст домена')

# PR и тИЦ
qp(anal, 'тИЦ', 'PR', 'PR и тИЦ')

#
#plot(anal$Количество.ссылок, anal$тИЦ)
#line <- lm(anal$тИЦ ~ anal$Количество.ссылок)
#abline(line)
#

# Поиск корреляций
anal2 <- anal
anal2 <- anal2[, -2]
cor_result <- cor(anal2)

# Поиск наибольших корреляций
corColNames <- colnames(cor_result)

corList <- data.frame()
for (i in 1:(nrow(cor_result) - 1)) {
    for (j in (i + 1):ncol(cor_result)) {
        df <- data.frame(v1 = corColNames[i], v2 = corColNames[j], value = cor_result[i, j])
        corList <- rbind(corList, df)
    }
}

corList2 <- corList[order(-corList$value), ]