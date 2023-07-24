# Instalação de Pacotes
install.packages("arules")
install.packages("arulesViz")
install.packages("tidyverse")
install.packages("readxl")
install.packages("knitr")
install.packages("lubridate")
install.packages("plyr")

# Carregando libraries
library(arules)
library(arulesViz)
library(tidyverse)
library(readxl)
library(knitr)
library(ggplot2)
library(lubridate)
library(plyr)
library(dplyr)

# Ler os dados do Excel para o R dataframe
library(readxl)
retail <- read_excel('Online Retail.xlsx')
view(retail)

# Limpando os dados
retail <- retail[complete.cases(retail), ]
retail %>% mutate(Description = as.factor(Description))
retail %>% mutate(Country = as.factor(Country))
retail$Date <- as.Date(retail$InvoiceDate)
TransTime<- format(retail$InvoiceDate,"%H:%M:%S")
InvoiceNo <- as.numeric(as.character(retail$InvoiceNo))
cbind(retail,TransTime)
cbind(retail,InvoiceNo)
glimpse(retail)

# Agrupando transações
library(plyr)
transactionData <- ddply(retail,c("InvoiceNo","Date"),
                         function(df1)paste(df1$Description, collapse = ","))
transactionData

# Retirando InvoiceNo e Date
transactionData$InvoiceNo <- NULL
transactionData$Date <- NULL
colnames(transactionData) <- c("items")
View(transactionData)

# Armazenando os dados em basket em um arquivo .csv
write.csv(transactionData,"market_basket_transactions.csv", quote = FALSE,
          row.names = FALSE)

# Movendo os dados para um objeto da classe de transação
library(arules)
tr <- read.transactions('market_basket_transactions.csv', format = 'basket',
sep=',')

# Crinado um gráfico de frequência de itens com os top 20
if (!require("RColorBrewer")) {
  # install color package of R
  install.packages("RColorBrewer")
  #include library RColorBrewer
  library(RColorBrewer)
}
itemFrequencyPlot(tr,topN=20,type="absolute",col=brewer.pal(8,'Pastel2'),
                  main="Frequencia Absoluta de Itens")

# Agora, um gráfico com a frequência relativa
itemFrequencyPlot(tr,topN=20,type="relative",col=brewer.pal(8,'Pastel2'),main="Frequenci
a Relativa de Itens")

# Gerando regras
association.rules <- apriori(tr, parameter = list(supp=0.001, conf=0.8,maxlen=10))
summary(association.rules)

# Limitando as regras
shorter.association.rules <- apriori(tr, parameter = list(supp=0.001,
                                                          conf=0.8,maxlen=3))
inspect(shorter.association.rules[1:10])

# Removendo as regras redundantes
subset.rules <- which(colSums(is.subset(association.rules, association.rules)) > 1)
subset.association.rules. <- association.rules[-subset.rules] 

# Regras para um item específico
metal.association.rules <- apriori(tr, parameter = list(supp=0.001, conf=0.8),appearance
                                   = list(default="lhs",rhs="METAL"))
inspect(head(metal.association.rules))

# Clientes que compram um item específico
metal.association.rules <- apriori(tr, parameter = list(supp=0.001, conf=0.8),appearance
                                   = list(lhs="METAL",default="rhs"))

############### Visualização de Dados #################

# Gráfico de dispersão
library(arulesviz)
subRules<-association.rules[quality(association.rules)$confidence>0.4]
plot(subRules)

# Gráfico two-key
plot(subRules,method="two-key plot")

# Gráfico dispersão interativo
plot(subRules)

# Visualização das regras
top10subRules <- head(subRules, n = 10, by = "confidence")
plot(top10subRules, method = "graph", engine = "htmlwidget")
saveAsGraph(head(subRules, n = 1000, by = "lift"), file = "rules.graphml")

# Regra individual
subRules2<-head(subRules, n=20, by="lift")
plot(subRules2, method="paracoord")

