#Análise inicial dos arquivos geraos pelo e-Lattes

#Pacotes para serem ativados
library(tidyverse)
library(jsonlite); library(listviewer)
library(igraph)
library(ggplot2)
library(dplyr)
library(tm)
library(SnowballC)
library(qdap)
library(wordcloud)

#Pasta com códigos e arquivos
setwd("~/Projects/DS4ALL/Relatorio") #Pasta contendo os arquivos

#upload de arquivo com funções para transformar listas em Data Frames e objeto igraph
source("elattes.ls2df.R")

#perfil <- fromJSON("./unbpos/unbpos.profile.json")
perfil <- fromJSON("./sociologia/so.profile.json")

perfil.df.professores <- extrai.perfis(perfil)
perfil.df.publicacoes <- extrai.producoes(perfil)

#############################################
### Analise dos perfis
#############################################
curriculos <- c()
nrow(perfil.df.professores)
for (i in 1:nrow(perfil.df.professores)){
  curriculos[[i]] <- perfil.df.professores$resumo_cv[[i]]
}
##criar o corpus
corpus_titulos <- VCorpus(VectorSource(curriculos))
corpus_titulos <- corpus_titulos %>%
  tm_map(content_transformer(tolower)) %>%
  tm_map(stripWhitespace) %>%
  tm_map(removePunctuation) %>%
  tm_map(removeWords, stopwords("en")) %>%
  tm_map(removeWords, stopwords("portuguese")) %>%
  tm_map(removeWords, c("brasil","brazil", "universidade", "sociologia", "sobre", "n", "vol")) 

wordcloud(corpus_titulos, scale=c(4,0.25), max.words=150, random.order=FALSE, rot.per=0.5, use.r.layout=FALSE, colors=brewer.pal(8, "Dark2"))
#############################################
### Analise das publicacoes
#############################################
titulos <- c()
nrow(perfil.df.publicacoes)
perfil.df.publicacoes
for (i in 1:nrow(perfil.df.publicacoes)){
   if (is.na(perfil.df.publicacoes$titulo[[i]])){
     print("capitulo")
    titulos[[i]] <- perfil.df.publicacoes$titulo_do_livro[[i]]
  }
  else{
    print("periodico")
     titulos[[i]] <- perfil.df.publicacoes$titulo[[i]]
  }
}
 
##criar o corpus
corpus_titulos <- VCorpus(VectorSource(titulos))
corpus_titulos <- corpus_titulos %>%
  tm_map(content_transformer(tolower)) %>%
  tm_map(stripWhitespace) %>%
  tm_map(removePunctuation) %>%
  tm_map(removeWords, stopwords("en")) %>%
  tm_map(removeWords, stopwords("portuguese")) %>%
  tm_map(removeWords, c("brasil","brazil", "sociologia", "sobre", "n", "vol")) 

#inspect(corpus_titulos)
#term_count <- freq_terms(corpus_titulos, 40)
#term_count
wordcloud(corpus_titulos, scale=c(3.5,0.25), max.words=50, random.order=FALSE, rot.per=0.5, use.r.layout=FALSE, colors=brewer.pal(8, "Dark2"))


