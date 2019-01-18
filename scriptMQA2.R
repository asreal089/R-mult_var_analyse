indx <- read.csv("C:/Users/mark7/Desktop/index-violent-property-and-firearm-rates-by-county-beginning-1990.csv")

names(indx)

plot(Index.Rate~Year, data = (indx), xlab= 'ANO', ylab = 'Índice Total de Crimes')

abline(h=mean(indx$Index.Rate,na.rm = T))

meanIDCA = mean(indx$Index.Rate,na.rm = T)

meanIDCA

mediana = median(indx$Index.Rate)

mediana

boxplot(indx$Index.Rate[623: 1736] , col="lightblue")
