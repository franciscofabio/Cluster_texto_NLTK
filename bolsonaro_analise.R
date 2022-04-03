install.packages(c("tm", "SnowballC", "wordcloud"))
install.packages(c("factoextra", "quanteda"))
install.packages(c("topicmodels"))

library(tm)
library(tmap)
library(SnowballC)
library(wordcloud)
library(factoextra)
library(quanteda)
library(topicmodels)

#criar o corpus do texto

noticias <- read.csv("D:/base_de_dados_mapa_de_problemas/twitter/bolsonaro.csv", sep = ",", header = TRUE, encoding = "UTF-8")
noticias

docs <- Corpus(VectorSource(noticias$tweet))

docs_full <- Corpus(VectorSource(noticias$tweet))

#tranformar o texto em minúsculo 

docs <- tm_map(docs, content_transformer(tolower))

#funcao limpa caracteres 

toSpace <- content_transformer(function(x, pattern) 
{ return (gsub(pattern, " ", x))})

docs <- tm_map(docs, toSpace, "-")
docs <- tm_map(docs, toSpace, ":")
docs <- tm_map(docs, toSpace, "'")
docs <- tm_map(docs, toSpace, "'")
docs <- tm_map(docs, toSpace, " -")
docs <- tm_map(docs, toSpace, "/") 
docs <- tm_map(docs, toSpace, "[^[:graph:]^[:blank:]]")#remove símbolos como emojis

#remove pontuação

docs <- tm_map(docs, removePunctuation)
docs <- tm_map(docs, removeNumbers)


docs <- tm_map(docs, removeWords, stopwords("portuguese"))

dtm.word.full <- DocumentTermMatrix(docs, control=list(wordLengths=c(3, 20)))

#termos mais frequentes 
freq <- colSums(as.matrix(dtm.word.full))
ord <- order(freq, decreasing = T)
freq[ord[1:50]]

exclui <- c("tco", "https", "pra", "vai", "ser", "tá", "aí")

docs <- tm_map(docs, removeWords, exclui)

dtm.word.full <- DocumentTermMatrix(docs, control=list(wordLengths=c(3, 20)))
#termos mais frequentes
freq <- colSums(as.matrix(dtm.word.full))
ord <- order(freq, decreasing = T)
freq[ord[1:50]]

#nuvem de palavras

set.seed(1234567)
wordcloud(names(freq), freq, max.words = 120, colors = brewer.pal(5, "Dark2"))


#aplicação do TF - IDF

dtm_tfidf <- DocumentTermMatrix(docs, 
                                control=list(wordLengths=c(3, 20), 
                                             weighting = weightTfIdf))

dtm_tfidf <- removeSparseTerms(dtm_tfidf, sparse = 0.97)

m_tfidf <- as.matrix(dtm_tfidf)

fviz_nbclust(m_tfidf, kmeans, method = "wss")

#Kmeans
k <- 6 
set.seed(123)
kmeansResult_tfidf <- kmeans(m_tfidf, k, nstart = 10) 

#frequencia de cada cluster

freq_clus_tfidf <- table(kmeansResult_tfidf$cluster)
freq_clus_tfidf


#lista as palavras mais frequentes de cada cluster
for (i in 1:k) {
  cat(paste("cluster ", i, ": ", sep=""))
  if(freq_clus_tfidf[i] > 1){
    freq = colSums(m_tfidf[kmeansResult_tfidf$cluster==i,])
  } else{
    freq = m_tfidf[kmeansResult_tfidf$cluster==i,]
  }
  ord <- order(freq, decreasing = T)
  cat(names(freq[ord[1:10]]), "\n")
}

#nuvem de palavras do cluster 1
set.seed(42)
dtm.word.full %>%
  as.matrix() %>%
  .[kmeansResult_tfidf$cluster==1,] %>%
  colSums() %>%
  wordcloud(names(.), 
            ., 
            max.words = 40, 
            colors = brewer.pal(3, "Dark2"))

#nuvem de palavras do cluster 2
set.seed(42)
dtm.word.full %>%
  as.matrix() %>%
  .[kmeansResult_tfidf$cluster==2,] %>%
  colSums() %>%
  wordcloud(names(.), 
            ., 
            max.words = 40, 
            colors = brewer.pal(3, "Dark2"))
#nuvem de palavras do cluster 
set.seed(42)
dtm.word.full %>%
  as.matrix() %>%
  .[kmeansResult_tfidf$cluster==3,] %>%
  colSums() %>%
  wordcloud(names(.), 
            ., 
            max.words = 40, 
            colors = brewer.pal(3, "Dark2"))

#nuvem de palavras do cluster 
set.seed(42)
dtm.word.full %>%
  as.matrix() %>%
  .[kmeansResult_tfidf$cluster==4,] %>%
  colSums() %>%
  wordcloud(names(.), 
            ., 
            max.words = 40, 
            colors = brewer.pal(3, "Dark2"))

#nuvem de palavras do cluster 
set.seed(42)
dtm.word.full %>%
  as.matrix() %>%
  .[kmeansResult_tfidf$cluster==5,] %>%
  colSums() %>%
  wordcloud(names(.), 
            ., 
            max.words = 70, 
            colors = brewer.pal(3, "Dark2"))

#nuvem de palavras do cluster 
set.seed(42)
dtm.word.full %>%
  as.matrix() %>%
  .[kmeansResult_tfidf$cluster==6,] %>%
  colSums() %>%
  wordcloud(names(.), 
            ., 
            max.words = 70, 
            colors = brewer.pal(3, "Dark2"))

docs[kmeansResult_tfidf$cluster==6]%>%
  .[10] %>%
  inspect()

co_matriz <- fcm(aux_nuvem_tfidf, context = "document")
dim(co_matriz)
co_temos <-names(topfeatures(co_matriz, 50))

set.seed(12345)
fcm_select(co_matriz, pattern = co_temos) %>%
  textplot_network()

findAssocs(dtm.word.full, c("bolsonaro", "anitta", "tsejusbr", "carlazambelli", "brasil", "tarcisiogdf", "govbr"), 0.2)