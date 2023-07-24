##### Instalando pacotes #####
Needed <- c("tm", "SnowballC", "RColorBrewer",
            "ggplot2", "wordcloud", "biclust",
            "cluster", "igraph", "fpc")
install.packages(Needed, dependencies = TRUE)
install.packages("Rcampdf", repos = "http://datacube.wu.ac.at/",
                 type = "source")

##### Carregando os texos #####
library(tm)
textos <- DirSource(directory = "C:/Users/absol/OneDrive/Arquivo/Área de Trabalho/USP - Técnicas de Aprendizado de Máquina/Laboratório de Análise de Texto/Textos", encoding = "UTF-8")
docs <- VCorpus(x=textos)

##### Pré-Processamento dos Textos #####

# remove a pontuação
docs <- tm_map(docs,removePunctuation)

# remove os números
docs <- tm_map(docs, removeNumbers)

# converte letras para minúsculas
docs <- tm_map(docs, content_transformer(tolower)) 
docs <- tm_map(docs, PlainTextDocument)

# Remove palavras comuns
docsCopia <- docs
length(stopwords("portuguese"))
stopwords("portuguese")
docs <- tm_map(docs, removeWords, stopwords("portuguese"))
docs <- tm_map(docs, PlainTextDocument)

# Remove as palavras "sobre", "ser", "pode", "esta", "desta"
docs <- tm_map(docs, removeWords, c("sobre", "ser", "pode", "esta", "desta"))

# Remove espaços em branco
docs <- tm_map(docs, stripWhitespace)

# Conclusão do Pré-Processamento Básico
docs <- tm_map(docs, PlainTextDocument)

##### Criação da Matriz de Termos de Documentos #####
dtm <- DocumentTermMatrix(docs)

##### Exploração de Dados #####

# Ordernar os dados pela frequência
freq <- colSums(as.matrix(dtm))
ord <- order(freq)

# Expotar a matriz para Excel
m <- as.matrix(dtm)
write.csv(m, file="DocumentTermMatrix.csv")

##### Focando nos dados interassantes #####
dtms <- removeSparseTerms(dtm, 0.4)

##### Análise da frequência de termos #####
freq <- colSums(as.matrix(dtms))
freq

# outra maneira
wf <- data.frame(word=names(freq), freq=freq)


##### Plotar frequência de termos em gráfico #####
library(ggplot2)
p <- ggplot(subset(wf, freq>100), aes(x = reorder(word, -freq), y = freq)) +
  geom_bar(stat = "identity") +
  theme(axis.text.x=element_text(angle=45, hjust=1))
p

##### Análise da correlação entre termos #####
findAssocs(dtm, c("estudo" , "pesquisa", "empresa"), corlimit=0.60)

##### Visualizando em núvem de palavras #####
library(wordcloud)
set.seed(142)
wordcloud(names(freq), freq, min.freq=50)

# Mostrando as 100 palavras mais utilizadas
set.seed(142)
wordcloud(names(freq), freq, max.words=100)

# 50 Palvras com cores
set.seed(142)
wordcloud(names(freq), freq, min.freq=50, scale=c(5, .1), colors=brewer.pal(6, "Dark2"))

# 100 Palvras com cores
set.seed(142)
dark2 <- brewer.pal(6, "Dark2")
wordcloud(names(freq), freq, max.words=100, rot.per=0.2, colors=dark2)

##### Análise de agrupamento #####

# Similaridade de termos
dtms <- removeSparseTerms(dtm, 0.50)

# Agrupamento hierárquico
library(cluster)
d <- dist(t(dtms), method="euclidian")
fit <- hclust(d=d, method="complete")
plot(fit, hang=-1)

# Agrupamento hierárquico com borda vermelha
plot.new()
plot(fit, hang=-1)
groups <- cutree(fit, k=5)
rect.hclust(fit, k=5, border="red")

# Agrupamento por partição (k-means)
library(fpc)
d <- dist(t(dtms), method="euclidian")
kfit <- kmeans(d, 3)
clusplot(as.matrix(d), kfit$cluster, color=T, shade=T, labels=2, lines=0)

