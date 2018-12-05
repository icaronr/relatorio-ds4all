#Análise inicial dos arquivos geraos pelo e-Lattes

#Pacotes para serem ativados
library(tidyverse)
library(jsonlite); library(listviewer)
library(igraph)
library(ggplot2)
library(dplyr)
#Pasta com códigos e arquivos
setwd("~/Projects/DS4ALL/Relatorio") #Pasta contendo os arquivos

#upload de arquivo com funções para transformar listas em Data Frames e objeto igraph
source("elattes.ls2df.R")

#Definição da pasta e leitura de arquivos
#public <- fromJSON("./unbpos/unbpos.publication.json")
public <- fromJSON("./sociologia/so.publication.json")
#public <- fromJSON("./politica_social/ps.publication.json")


####
###Publicação
##Análise dos dados no formato lista
#Número de Publicações em periódicos
sum(sapply(public$PERIODICO, function(x) length(x$natureza)))
#anos analisados
names(public$PERIODICO)
#20 revistas mais publicadas
head(sort(table(as.data.frame(unlist
                              (sapply(public$PERIODICO, function(x) unlist(x$periodico)))
)), decreasing = TRUE),20)

##Análise dos dados no formato DF
public.periodico.df <- pub.ls2df(public, 1) #artigos
public.periodico.df <- public.periodico.df %>% mutate(t_pub = "periodico")

public.livros.df <- pub.ls2df(public, 2) #livros
public.livros.df <- public.livros.df %>% mutate(t_pub = "livro")

public.capitulos.df <- pub.ls2df(public, 3) #capitulos de livros
public.capitulos.df <- public.capitulos.df %>% mutate(t_pub = "capitulo")

public.jornais.df <- pub.ls2df(public, 4) #texto em jornais
public.jornais.df <- public.jornais.df %>% mutate(t_pub = "jornal")

public.eventos.df <- pub.ls2df(public, 5) #eventos
public.eventos.df <- public.eventos.df %>% mutate(t_pub = "evento") %>% rename(ano = ano_do_trabalho)

public.aceitos.df <- pub.ls2df(public, 6) #artigo aceito
public.aceitos.df <- public.aceitos.df %>% mutate(t_pub = "artigo aceito")

public.demais.df <- pub.ls2df(public, 7) #demais tipos de publis
public.demais.df <- public.demais.df %>% mutate(t_pub = "demais tipos")

topper <- public.periodico.df %>%
            full_join(public.livros.df) %>%
            full_join(public.capitulos.df) %>%
            full_join(public.jornais.df) %>%
            full_join(public.eventos.df) %>%
            full_join(public.aceitos.df) %>%
            full_join(public.demais.df)

#############################################
# MEU PLOT
#############################################

topper %>% 
  group_by(ano, t_pub) %>%
  summarise(Quantidade = n()) %>%
  ggplot(aes(x=ano, y=Quantidade, col=t_pub)) +
  geom_bar(stat = "identity") +
  facet_grid(t_pub~.)

ggplot(topper, aes(x=ano ,y=t_pub)) +
  geom_jitter() 



#Publicação por ano
table(public.periodico.df$ano)
#20 revistas mais publicadas
#Mesma visão que anterior mas agora trabalhando no DataFrame
head(sort(table(public.periodico.df$periodico), decreasing = TRUE), 20)

#Visualização
# Gráfico de barras
public.periodico.df %>%
  group_by(ano) %>%
  summarise(Quantidade = n()) %>%
  ggplot(aes(x = ano, y = Quantidade)) +
  geom_bar(position = "stack",stat = "identity", fill = "darkcyan")+
  geom_text(aes(label=Quantidade), vjust=-0.3, size=2.5)+
  theme_minimal()

#publicação de livros fora do Brasil
public.livros.df %>%
  group_by(pais_de_publicacao) %>%
  summarise(Quantidade = n()) %>%
  filter(pais_de_publicacao != "Brasil") %>% 
  ggplot(aes(x = pais_de_publicacao, y = Quantidade)) +
  geom_bar(width=0.8, height = 0.3, position = "stack",stat = "identity", fill = "coral")+
  geom_text(aes(label=Quantidade), vjust=-0.3, size=2.5) +
  theme_minimal()

public.livros.df %>%
  filter(pais_de_publicacao %in% c("Brasil", "Estados Unidos", "Holanda",
                                   "Grã-Bretanha", "Alemanha", "Suiça")) %>%
  group_by(ano,pais_de_publicacao) %>%
  ggplot(aes(x=ano,y=pais_de_publicacao, color= pais_de_publicacao)) +
  xlab("Ano") + ylab("Pais") + geom_point() + geom_jitter()


#Eventos
public.eventos.df %>%
  filter(pais_do_evento %in% 
           c(names(head(sort(table(public.eventos.df$pais_do_evento)
                             , decreasing = TRUE), 10)))) %>%
  group_by(ano_do_trabalho,pais_do_evento) %>%
  ggplot(aes(x=ano_do_trabalho,y=pais_do_evento, color= pais_do_evento)) +
  xlab("Ano") + ylab("Pais") + geom_point() + geom_jitter()



