# Desafio Aula 4 - MIT Big Data

# Nicolas Marcos - nicolasmarcos.ti@gmail.com
# Allan da Silva - allan.andrade5@hotmail.com 
# Fábio Domingues Machado - fabio.dmachado@al.infnet.edu.br
# Ernani Fantinatti - ernanif@fantinatti.com


# Leitura da base de dados

setwd("/cloud/project")

# bibliotecas necessárias

#install.packages("party", dependencies=TRUE)
library(party)

#install.packages("caTools", dependencies=TRUE)
library(caTools)

#install.packages("readr", dependencies=TRUE)
library(readr)

#install.packages("rpart", dependencies=TRUE)
library(rpart)

#install.packages("rpart.plot", dependencies=TRUE)
library(rpart.plot)

#install.packages("rattle", dependencies=TRUE)
library(rattle)

#install.packages('dplyr', dependencies="TRUE")
library(dplyr)

# Realiza pré-processamento, considerando colunas como numeric

library(readr)
forestfires <- read_csv("forestfires.csv", 
                        col_types = cols(DC = col_number(), DMC = col_number(), 
                                         FFMC = col_number(), ISI = col_number(), 
                                         RH = col_number(), X = col_integer(), 
                                         Y = col_integer(), area = col_number(), 
                                         rain = col_number(), temp = col_number(), 
                                         wind = col_number()))
View(forestfires)

df <- forestfires
str(df)

# área 
df_queimado <- forestfires[forestfires$area > 0,]
df_nqueimado <- forestfires[forestfires$area == 0,]

summary(df_queimado)
summary(df_nqueimado)

# 1) Relatório com análise definida pelo grupo

# Fontes de Referência: 
# https://www.malagaweather.com/fwi-txt.htm
# http://cwfis.cfs.nrcan.gc.ca/background/summary/fwi

# FFMC - Código é um indicador da relativa facilidade de ignição e da inflamabilidade do combustível fino
# DMC - (Superficial) Uma classificação DMC superior a 30 é seca e acima de 40 indica que ocorrerá uma queima intensiva nos combustíveis médios e médios. 
# DC - (Profundo) O Código da Seca (DC) é uma classificação numérica do teor médio de umidade de camadas orgânicas compactas e profundas. Este código é um indicador útil dos efeitos sazonais da seca nos combustíveis florestais e da quantidade de latentes em camadas profundas e grandes toras.
# ISI - O Índice Inicial de Spread (ISI) é uma classificação numérica da taxa esperada de propagação do fogo. Combina os efeitos do vento e do FFMC na taxa de propagação sem a influência de quantidades variáveis de combustível.

# 2) Apresentação dos dados:

# 2.1) Descrição sucinta da aplicação

# Estimar a área queimada por incêndios florestais.
# Foi utilizado modelo de regressão linear através de árvore de decisão para estimar a área queimada.
# A baixa correlação das variáveis corroborou na escolha deste algoritmo, uma vez que não enviesou dados.

# 2.2) Tipo do Problema (Regressão / Classificação)

# Regressão

# Observações: 
# Para o cálculo da regressão com o dataframe de áreas queimadas e não queimadas, foi utilizado
# o pré-processamento de retirada de outliers, presente no item (3.3). 
# Para a regressão, não foram consideradas as latitudes, longitudes, mês e dia, por não serem
# considerados fatores relevantes no problema abordado. A possível sazonalidade de mês foi já contemplada
# havendo o atributo de temperatura.


# Analisando os dados gerais, a temperatura se mostra como a variável independente mais significativa
# para a estimativa da área queimada.
set.seed(1)
arvore_total <- rpart(area ~ FFMC + DMC + DC + ISI + temp + RH + wind + rain, method="anova", data=df)
fancyRpartPlot(arvore_total)

# Analisando apenas os dados de áreas queimadas, a umidade relativa se mostra como fator preponderante, 
# sendo acompanhada pela temperatura, conforme a visão anterior com todos os dados. 
# Portanto, pode-se inferir que a umidade relativa dos registros não queimados estão enviesando
# o resultado dos registros queimados no cenário anterior
set.seed(1)
arvore_queimados <- rpart(area ~ FFMC + DMC + DC + ISI + temp + RH + wind + rain, method="anova", data=df_queimado)
fancyRpartPlot(arvore_queimados)

# Como a área não queimada é zero, a regressão linear culmina em valor 0
set.seed(1)
arvore_nqueimados <- rpart(area ~ FFMC + DMC + DC + ISI + temp + RH + wind + rain, method="anova", data=df_nqueimado)
fancyRpartPlot(arvore_nqueimados)

# 2.3) Números de Variáveis e Observações

# O arquivo original dispunha de 517 observações e 13 variáveis

str(forestfires)

# 2.4) Existência de Valores Ausentes

# Não existe
any(is.nan(df$X) | is.na(df$X))
any(is.nan(df$Y) | is.na(df$Y))
any(is.nan(df$month) | is.na(df$month))
any(is.nan(df$day) | is.na(df$day))
any(is.nan(df$FFMC) | is.na(df$FFMC))
any(is.nan(df$DMC) | is.na(df$DMC))
any(is.nan(df$DC) | is.na(df$DC))
any(is.nan(df$ISI) | is.na(df$ISI))
any(is.nan(df$temp) | is.na(df$temp))
any(is.nan(df$RH) | is.na(df$RH))
any(is.nan(df$wind) | is.na(df$wind))
any(is.nan(df$rain) | is.na(df$rain))
any(is.nan(df$area) | is.na(df$area))

# 3) Análise Explorartória

# 3.1) Estatísticas básicas: mínimo, máximo, média, desvio padrão

# Resumo de todas as variáveis
summary(df)

# Desvio padrão das variáveis numéricas
sd(df$X)
sd(df$Y)
sd(df$FFMC)
sd(df$DMC)
sd(df$DC)
sd(df$ISI)
sd(df$temp)
sd(df$RH)
sd(df$wind)
sd(df$rain)
sd(df$area)

# 3.2 ) Histogramas e distribuição das variáveis:

# Não se viu necessidade de fazer histograma utilizando separadamente a latitude e longitude, tampouco a área queimada.  

#var(df$X)
#var(df$Y)

# Inferiu-se como relevante realizar histogramas e análises apenas dos casos de queimada para saber se há
# dias ou meses com maior incidência
library(ggplot2)
ggplot(count(df_queimado,day))

library(ggplot2)
count(df_queimado,month)

# FFMC alto a partir de 70

hist(df$FFMC)
var(df$FFMC)

# DMC acima 30,40 alta chance de fogo
hist(df$DMC)
var(df$DMC)

# DC acima de 200 alta chance de fogo
hist(df$DC)
var(df$DC)

# ISI taxa de propagação do fogo. Acima de 10 é alta
hist(df$ISI)
var(df$ISI)

# Temperatura
hist(df$temp)
var(df$temp)

# Umidade relativa
hist(df$RH)
var(df$RH)

# Velocidade do Vento
hist(df$wind)
var(df$wind)

# Índice de chuva
hist(df$rain)
var(df$rain)

# Área Queimada
hist(df$area)
var(df$area)

# 3.3) Verificar a existência de outliers

summary(df)

# Através do summary(), identificou-se outliers no FFMC de valores mínimos em relação ao primeiro quartil e 
# média e mediana. 
# FFMC acima de 70 é considerado índice alto na teoria. 
# Pela distribuição dos dados, foram retirados os FFMC < 70.

hist(df$FFMC)
df <- df[df$FFMC >= 70,]

summary(df)

# Em teoria, DMC acima de 30 pode ser considerado alto. Foram encontrados valores altos. 
# Porém, devido a distribuição destes valores, foram mantidos.
# Em pesquisa, verificou-se que mundialmente este índice pode chegar até 500
hist(df$DMC)

# Em teoria, DC acima de 10 pode ser considerado alto. Foram encontrados valores altos. 
# Porém, devido a distribuição destes valores, foram mantidos.
hist(df$DC)

# Foi encontrado um outlier de 56.1 que foi retirado da amostragem
hist(df$ISI)

df <- df[df$ISI < 56.1,]

hist(df$ISI)

# análise temperatura
# Não foram encontrados outliers
summary(df)
hist(df$temp)

# análise umidade
# Não foram encontrados outliers
hist(df$RH, labels =TRUE)

# análise vento
# Não foram encontrados outliers
hist(df$wind, labels =TRUE)

# análise chuva
# Foram encontrados outliers, foram foram mantidos, pois um dia de chuva é capaz de alterar a incidência de queimada
# e seus demais indicadores
hist(df$rain, labels =TRUE)

# análise área
# Apenas foi encontrado sentido de histograma, utilizando a visão de área queimada. 
# Visto que há 247 observações onde a área queimada é 0
# Analisando as áreas queimadas, nota-se que os incêndios florestais são concentrados em áreas de até 100 hectares
hist(df_queimado$area, labels =TRUE)

# 3.4) Correlações

# Etapa 2c tem gráficos de correlação

# Analisando dados normais
cor(df$FFMC,df$area)
cor(df$DMC,df$area)
cor(df$DC,df$area)
cor(df$ISI,df$area)
cor(df$temp,df$area)
cor(df$wind,df$area)
cor(df$rain,df$area)

# Analisando dados de queimada
cor(df_queimado$FFMC,df_queimado$area)
cor(df_queimado$DMC,df_queimado$area)
cor(df_queimado$DC,df_queimado$area)
cor(df_queimado$ISI,df_queimado$area)
cor(df_queimado$temp,df_queimado$area)
cor(df_queimado$wind,df_queimado$area)
cor(df_queimado$rain,df_queimado$area)

# Analisando dados de área não queimada, não possível por serem valores zerados
cor(df_nqueimado$FFMC,df_nqueimado$area)

# Não foram encontrados dados de correlação significativos, analisando os dados gerais, dados de áreas queimadas e de áreas não queimadas

# 4) Apresentar as conclusões:

# 4.1) Analisando os dados gerais, a temperatura se mostra como a variável independente mais significativa
# para a estimativa da área queimada.

# 4.2) Analisando apenas os dados de áreas queimadas, a umidade relativa se mostra como fator preponderante, 
# sendo acompanhada pela temperatura, conforme a visão anterior com todos os dados. 
# Portanto, pode-se inferir que a umidade relativa dos registros não queimados estão enviesando
# o resultado dos registros queimados no cenário anterior

# 4.3) Como a área não queimada é zero, a regressão linear culmina em valor 0

# 4.4) Não existem valores ausentes

# 4.5) # Através do summary(), identificou-se outliers no FFMC de valores mínimos em relação ao primeiro quartil e 
# média e mediana. 
# FFMC acima de 70 é considerado índice alto na teoria. 
# Pela distribuição dos dados, foram retirados os FFMC < 70.

# Através do summary(), identificou-se outliers no FFMC de valores mínimos em relação ao primeiro quartil e 
# média e mediana. 
# FFMC acima de 70 é considerado índice alto na teoria. 
# Pela distribuição dos dados, foram retirados os FFMC < 70.

# Em teoria, DMC acima de 30 pode ser considerado alto. Foram encontrados valores altos. 
# Porém, devido a distribuição destes valores, foram mantidos.
# Em pesquisa, verificou-se que mundialmente este índice pode chegar até 500

# Em teoria, DC acima de 10 pode ser considerado alto. Foram encontrados valores altos. 
# Porém, devido a distribuição destes valores, foram mantidos.

# Foi encontrado um outlier de 56.1 de ISI que foi retirado da amostragem

# análise temperatura
# Não foram encontrados outliers

# análise umidade
# Não foram encontrados outliers

# análise vento
# Não foram encontrados outliers

# análise chuva
# Foram encontrados outliers, foram foram mantidos, pois um dia de chuva é capaz de alterar a incidência de queimada
# e seus demais indicadores

# análise área
# Apenas foi encontrado sentido de histograma, utilizando a visão de área queimada. 
# Visto que há 247 observações onde a área queimada é 0
# Analisando as áreas queimadas, nota-se que os incêndios florestais são concentrados em áreas de até 100 hectares

# 4.6) # Não foram encontrados dados de correlação significativos, analisando os dados gerais, dados de áreas queimadas e de áreas não queimadas

