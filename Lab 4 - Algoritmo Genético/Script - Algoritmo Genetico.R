# Instalação do Pacote
install.packages("GA")
library(GA)

# Dados do Problema
item <- c('casaco de chuva','canivete','água mineral','luvas','saco de dormir','barraca','fogão portátil','comida enlatada','lanches')
peso <- c(2,1,6,1,4,9,5,8,3)
sobrevivencia <- c(5,3,15,5,6,18,8,20,8)
dados=data.frame(item,peso,sobrevivencia)
peso_maximo = 25
View(dados)

# Criação da Função de Otimização
fitness=function(x)
{
  pontos=x%*%dados$sobrevivencia
  pesos=x%*%dados$peso
  if(pesos>peso_maximo)
  {
    return(0)
  }
  else
  {
    return(pontos)
  }
}

# Processo de otimização usando o algoritmo genético
GA=ga(type='binary',fitness=fitness,nBits=nrow(dados),maxiter=30,popSize=50,seed=1234,keepBest=TRUE)
summary(GA)
plot(GA)

# Repetição do treinamento com mais gerações
GA2=ga(type='binary',fitness=fitness,nBits=nrow(dados),maxiter=50,popSize=50,seed=1234,keepBest=TRUE)
GA3=ga(type='binary',fitness=fitness,nBits=nrow(dados),maxiter=100,popSize=50,seed=1234,keepBest=TRUE)

# Primeiro o GA2, para 50 gerações
plot(GA2)

# Depois para o GA3, para 100 gerações
plot(GA3)
summary(GA3)

