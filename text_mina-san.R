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
so.perfil <- fromJSON("./sociologia/so.profile.json")
ps.perfil <- fromJSON("./politica_social/ps.profile.json")
ci.perfil <- fromJSON("./ciencia_informacao/ci.profile.json")
co.perfil <- fromJSON("./comunicacao/co.profile.json")

#############################################
### gerando dataframes de perfis
#############################################
so.perfil.df.professores <- extrai.perfis(so.perfil)
ps.perfil.df.professores <- extrai.perfis(ps.perfil)
ci.perfil.df.professores <- extrai.perfis(ci.perfil)
co.perfil.df.professores <- extrai.perfis(co.perfil)

#############################################
### gerando dataframes de publicacoes
#############################################
so.perfil.df.publicacoes <- extrai.producoes(so.perfil)
ps.perfil.df.publicacoes <- extrai.producoes(ps.perfil)
ci.perfil.df.publicacoes <- extrai.producoes(ci.perfil)
co.perfil.df.publicacoes <- extrai.producoes(co.perfil)


#############################################
### Funções para as análises
#############################################
limpa_corpus <- function(corpus_sujo){
  corpus_limpo <- corpus_sujo %>%
    tm_map(content_transformer(tolower)) %>%
    tm_map(stripWhitespace) %>%
    tm_map(removePunctuation) %>%
    tm_map(removeWords, stopwords("en")) %>%
    tm_map(removeWords, stopwords("portuguese")) %>%
    tm_map(removeWords, c("brasil","brazil", "sociologia", "comunicacao", "comunicação", "ciência", "informação","serviço social","sobre", "n", "vol", "pesquisa", "universidade", "sobre"))
  return(corpus_limpo)
}

## popula o vetor de titulos
pega_titulos <- function(df_perfil_publ){
  titulos <- c()
  for (i in 1:nrow(df_perfil_publ)){
    if (is.na(df_perfil_publ$titulo[[i]])){
      print("capitulo")
      titulos[[i]] <- df_perfil_publ$titulo_do_livro[[i]]
    }
    else{
      print("periodico")
      titulos[[i]] <- df_perfil_publ$titulo[[i]]
    }
  }
  return(titulos)
}

pega_curriculos <- function(df_perfil_prof){
  ## vetor de curriculos
  curriculos <- c()
  ## popula o vetor de curriculos
  for (i in 1:nrow(df_perfil_prof)){
    curriculos[[i]] <- df_perfil_prof$resumo_cv[[i]]
  }
  return(curriculos)
}
#############################################
### Analise dos perfis
#############################################

so.curriculos <- pega_curriculos(so.perfil.df.professores)
ps.curriculos <- pega_curriculos(ps.perfil.df.professores)
ci.curriculos <- pega_curriculos(ci.perfil.df.professores)
co.curriculos <- pega_curriculos(co.perfil.df.professores)

## criar o corpus
so.corpus_curriculos <- VCorpus(VectorSource(so.curriculos))
so.corpus_curriculos <- limpa_corpus(so.corpus_curriculos)
#-----
ps.corpus_curriculos <- VCorpus(VectorSource(ps.curriculos))
ps.corpus_curriculos <- limpa_corpus(ps.corpus_curriculos)
#-----
ci.corpus_curriculos <- VCorpus(VectorSource(ci.curriculos))
ci.corpus_curriculos <- limpa_corpus(ci.corpus_curriculos)
#----
co.corpus_curriculos <- VCorpus(VectorSource(co.curriculos))
co.corpus_curriculos <- limpa_corpus(co.corpus_curriculos)


## nuvem de palavras para visualizar as palavras mais frequentes
wordcloud(so.corpus_curriculos, scale=c(4,0.25), max.words=150, random.order=FALSE, rot.per=0.5, use.r.layout=FALSE, colors=brewer.pal(8, "Dark2"))
wordcloud(ps.corpus_curriculos, scale=c(4,0.25), max.words=150, random.order=FALSE, rot.per=0.5, use.r.layout=FALSE, colors=brewer.pal(8, "Dark2"))
wordcloud(ci.corpus_curriculos, scale=c(4,0.25), max.words=150, random.order=FALSE, rot.per=0.5, use.r.layout=FALSE, colors=brewer.pal(8, "Dark2"))
wordcloud(co.corpus_curriculos, scale=c(4,0.25), max.words=150, random.order=FALSE, rot.per=0.5, use.r.layout=FALSE, colors=brewer.pal(8, "Dark2"))


#############################################
### Analise das publicacoes
#############################################


so.titulos <- pega_titulos(so.perfil.df.publicacoes)
ps.titulos <- pega_titulos(ps.perfil.df.publicacoes)
ci.titulos <- pega_titulos(ci.perfil.df.publicacoes)
co.titulos <- pega_titulos(co.perfil.df.publicacoes)

 
##criar o corpus
so.corpus_titulos <- VCorpus(VectorSource(so.titulos))
so.corpus_titulos <- limpa_corpus(so.corpus_titulos)
#-----
ps.corpus_titulos <- VCorpus(VectorSource(ps.titulos))
ps.corpus_titulos <- limpa_corpus(ps.corpus_titulos)
#-----
ci.corpus_titulos <- VCorpus(VectorSource(ci.titulos))
ci.corpus_titulos <- limpa_corpus(ci.corpus_titulos)
#-----
co.corpus_titulos <- VCorpus(VectorSource(co.titulos))
co.corpus_titulos <- limpa_corpus(co.corpus_titulos)


## criação da nuvem de palavras
wordcloud(so.corpus_titulos, scale=c(3.5,0.25), max.words=100, random.order=FALSE, rot.per=0.5, use.r.layout=FALSE, colors=brewer.pal(8, "Dark2"))
wordcloud(ps.corpus_titulos, scale=c(3.5,0.25), max.words=100, random.order=FALSE, rot.per=0.5, use.r.layout=FALSE, colors=brewer.pal(8, "Dark2"))
wordcloud(ci.corpus_titulos, scale=c(3.5,0.25), max.words=100, random.order=FALSE, rot.per=0.5, use.r.layout=FALSE, colors=brewer.pal(8, "Dark2"))
wordcloud(co.corpus_titulos, scale=c(3.5,0.25), max.words=100, random.order=FALSE, rot.per=0.5, use.r.layout=FALSE, colors=brewer.pal(8, "Dark2"))


