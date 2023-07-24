# Carregando o dataframe Carseats

install.packages("ISLR")
library(ISLR)
data(package="ISLR")
carseats<-Carseats

# Carregando o pacote de árvore de decisões
install.packages("tree")
require(tree)


# Classificação das vendas. Tida como alta se > 8 (mil)

High = ifelse(carseats$Sales<=8, "No", "Yes")
carseats = data.frame(carseats, High)
carseats$High <- as.factor(carseats$High)

# Criando a árvore de decisão
tree.carseats = tree(High~.-Sales, data=carseats)


######### Podando a árvore ###########

# Cria subconjunto de dadaos para treino

set.seed(101)
train=sample(1:nrow(carseats), 250)

# Cria árvore usando os dados de treino
tree.carseats = tree(High~.-Sales, carseats, subset=train)

# Predição dos dados reais usando a árvore de criada
tree.pred = predict(tree.carseats, carseats[-train,], type="class")

# Aferição do índice de acerto
with(carseats[-train,], table(tree.pred, High))

# Usando validação cruzada (cv) para descobrir o tamanho ideal
cv.carseats = cv.tree(tree.carseats, FUN = prune.misclass)
plot(cv.carseats)

# Escolhendo 12 como o tamanho ideal
prune.carseats = prune.misclass(tree.carseats, best = 12)

# Aferição do novo índice de acerto
tree.pred = predict(prune.carseats, carseats[-train,], type='class')
with(carseats[-train,], table(tree.pred, High))