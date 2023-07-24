# Carregando dados

library(MASS)
data(package="MASS")
boston<-Boston
dim(boston)
names(boston)
View(boston)

# Definindo subset de treino
train = sample(1:nrow(boston), 300)

# Carregando Boosting
install.packages("gbm")
require(gbm)
library(gbm)

# Importância de cada variável
boost.boston = gbm(medv~., data = boston[train,], distribution =
                     "gaussian", n.trees = 10000, shrinkage = 0.01, interaction.depth = 4)
summary(boost.boston)

# Gráfico das variáveis lstat e rm
plot(boost.boston,i="lstat")
plot(boost.boston,i="rm")

# Predição de resultados usando boosting
n.trees = seq(from = 100, to = 10000, by = 100)
predmat = predict(boost.boston, newdata = boston[-train,], n.trees =
                    n.trees)
dim(predmat)

# Aferição 
boost.err = with(boston[-train,], apply( (predmat - medv)^2, 2, mean))
plot(n.trees, boost.err, pch = 23, ylab = "Erro Médio Quadrado", xlab
     = "Número de Árvores", main = "Teste do Erro de Boosting")