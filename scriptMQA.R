
# recebe dataset
prison.adm <- read.csv("C:/Users/mark7/Desktop/prison-admissions-beginning-2008.csv")

attach(prison.adm)
dim(prison.adm)
names(prison.adm)

#total para geral porcentagem
count <- table(Admission.Type)
#porcentagem de tipos de admissão
perc <- table(Admission.Type)/228930

perc

#grafico de barra de tipo de admissão
barplot(perc , main = 'TIPO DE ADIMISSÃO', xlab='TIPO', ylab = '%', col= "blue4")

#total de crimes mais serios ordenados por quantidade cresente
count2 <- sort(table(Most.Serious.Crime))

#exibindo crimes mais graves com maior incidencia.
barplot(count2[353:356], main = 'CRIMES MAIS COMETIDOS', xlab='CRIME', ylab = 'QTD', col= "blue4")

#total de crimes cometidos por municipio
count3 <- sort(table(County.of.Commitment[]))
#municipios com maior indice
count3[60:64]
#grafico de barras com municipios com maiores numeres numeros de detentos admitidos na prisão
barplot(count3[60:63] , main = 'Municípios', xlab='Municípios', ylab = 'CRIMES', col= "red")

#recebendo dataset2
indx <- read.csv("C:/Users/mark7/Desktop/index-violent-property-and-firearm-rates-by-county-beginning-1990.csv")

names(indx)

#grafico com indices crimes por municipio por ano
plot(Index.Rate~Year, data = (indx), xlab= 'ANO', ylab = 'Índice Total de Crimes')
#media de indice de crimes 'plotada' em linha no grafico acima
abline(h=mean(indx$Index.Rate,na.rm = T))

#boxplot do indice de criminalidade
boxplot(indx$Index.Rate[623: 1736] , col="lightblue")

#testes para segunda parte - Mergiando datasets
indexViolence <- read.csv("C:/Users/mark7/Desktop/index-violent-property-and-firearm-rates-by-county-beginning-1990.csv")
incomeTax <- read.csv("C:/Users/mark7/Desktop/total-income-and-tax-liability-by-place-of-residence-beginning-tax-year-1999.csv")
resultado <- merge (indexViolence, incomeTax, by=c("Year","County"))
write.table(resultado, "C:/Users/mark7/Desktop/resultado.csv", sep=";")
