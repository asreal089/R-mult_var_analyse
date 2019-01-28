#Carregar dataset de indices de crimilidades
indexViolence <- read.csv("C:/Users/mark7/Desktop/index-violent-property-and-firearm-rates-by-county-beginning-1990.csv")
#carregar dataset de impostos e renda em NY
incomeTax <- read.csv("C:/Users/mark7/Desktop/total-income-and-tax-liability-by-place-of-residence-beginning-tax-year-1999.csv")
#merge dos datasets
resultado <- merge (indexViolence, incomeTax, by=c("Year","County"))


#############################################################
# DESCRIÇÃO QUE FALTAVA DAS VARIÁVEIS########################
#############################################################
ls(resultado)
summary(resultado$Index.Rate)
hist(resultado$Index.Rate)
boxplot(resultado$Index.Rate , col="grey" , xlab="bottom & left box")

summary(resultado$Index.Count)
hist(resultado$Index.Count)
boxplot(resultado$Index.Count , col="grey" , xlab="bottom & left box")

summary(resultado$Population)
hist(resultado$Population)
boxplot(resultado$Index.Population , col="grey" , xlab="bottom & left box")

summary(resultado$Average.NY.AGI.of.All.Returns)
hist(resultado$Average.NY.AGI.of.All.Returns)
boxplot(resultado$Average.NY.AGI.of.All.Returns, col="grey" , xlab="Média de Impostos")

summary(resultado$NY.AGI.of.Nontaxable.Returns..in.thousands.../1000000)
hist(resultado$NY.AGI.of.Nontaxable.Returns..in.thousands...)
boxplot(resultado$NY.AGI.of.Nontaxable.Returns..in.thousands.../1000000, col="grey" , xlab="renda não tributável")

summary(resultado$NY.AGI.of.All.Returns..in.thousands.../1000000)
boxplot(resultado$NY.AGI.of.All.Returns..in.thousands.../1000000, col="grey" , xlab="Total de Impostos por bilhões")


#regressao linear multipla
regressao <- lm(Index.Rate ~ Average.NY.AGI.of.All.Returns + Population + NY.AGI.of.All.Returns..in.thousands...+Tax.Liability.of.All.Returns..in.thousands.... , data = resultado)
#saida dos resultado numericos das regressoes
summary(regressao)
#variavel extra para manipulacao de indices das tabelas
myvars <- c("Index.Rate", "Average.NY.AGI.of.All.Returns", "Population")
#variavel extra para manipulacao de indices das tabelas
myvars <- c("Index.Count", "Average.NY.AGI.of.All.Returns", "Population")
#selecionando apenas dados a serem analisados
newdata <- resultado[myvars]
#primeira tentativa de regressao linear multipla
novaregressao <- lm(Index.Rate ~ Average.NY.AGI.of.All.Returns + Population, data = resultado)
#segunda tentativa de regressao linear multipla
novaregressao <- lm(Index.Count ~ Average.NY.AGI.of.All.Returns + Population, data = resultado)
#terceira tentativa de regressao linear multipla
novaregressao <- lm(Index.Count ~ NY.AGI.of.All.Returns..in.thousands... + Population, data = resultado)


#saida dos resultado numericos das regressoes
summary(novaregressao)
#variancia residual
(summary(novaregressao)$sigma)**2
#variancia dos coeficientes
vcov(novaregressao)
#graficos de relações entre variaveis em forma matricial
pairs(newdata, col = 2, pch = 19)
#plot de regressão linear multipla - distribuicao de residuos 
plot(novaregressao, which = 1)
#plot de normalidade dos dos residuos
plot(novaregressao, which = 2)
#gráfico da distancia de cook
plot(novaregressao, which = 5)