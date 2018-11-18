#Análise inicial dos arquivos geraos pelo e-Lattes

#Pacotes para serem ativados
library(tidyverse)
library(jsonlite); library(listviewer)
library(igraph)

#Pasta com códigos e arquivos
setwd("~/Projects/DS4ALL/Relatorio") #Pasta contendo os arquivos

#upload de arquivo com funções para transformar listas em Data Frames e objeto igraph
source("elattes.ls2df.R")

#Definição da pasta e leitura de arquivos
perfil <- fromJSON("./unbpos/unbpos.profile.json")
public <- fromJSON("./unbpos/unbpos.publication.json")
orient <- fromJSON("./unbpos/unbpos.advise.json")
graphl <- fromJSON("./unbpos/unbpos.graph.json")
res.area <- fromJSON("./unbpos/unbpos.researchers_by_area.json")
df.prog <- read.table("./unbpos/PesqPosCapes.csv", sep = ",", 
                      colClasses = "character", encoding = "UTF-8", header = TRUE)
#ciencia da informacao
ci.perfil <- fromJSON("./ciencia_informacao/ci.profile.json")
ci.public <- fromJSON("./ciencia_informacao/ci.publication.json")
ci.orient <- fromJSON("./ciencia_informacao/ci.advise.json")
ci.graphl <- fromJSON("./ciencia_informacao/ci.graph.json")
#comunicacao
co.perfil <- fromJSON("./comunicacao/co.profile.json")
ci.public <- fromJSON("./ciencia_informacao/ci.publication.json")
ci.orient <- fromJSON("./ciencia_informacao/ci.advise.json")
ci.graphl <- fromJSON("./ciencia_informacao/ci.graph.json")
#sociologia
#politica social
######
#Análise do arquivo perfil
jsonedit(perfil)

#Número de Pessoas que foram encontradas
length(perfil)

##Análise dos dados em formato list
# Número de áreas de atuação cumulativo
sum(sapply(perfil, function(x) nrow(x$areas_de_atuacao)))
# Número de áreas de atuação por pessoa
table(unlist(sapply(perfil, function(x) nrow(x$areas_de_atuacao))))
# Número de pessoas por grande area
table(unlist(sapply(perfil, function(x) (x$areas_de_atuacao$grande_area))))
table(unlist(sapply(ci.perfil, function(x) (x$areas_de_atuacao$grande_area))))
table(unlist(sapply(ci.perfil, function(x) (x$areas_de_atuacao$especialidade))))
# Número de pessoas que produziram os específicos tipos de produção
table(unlist(sapply(perfil, function(x) names(x$producao_bibiografica))))
# Número de publicações por tipo
sum(sapply(perfil, function(x) length(x$producao_bibiografica$ARTIGO_ACEITO$ano)))
sum(sapply(perfil, function(x) length(x$producao_bibiografica$CAPITULO_DE_LIVRO$ano)))
sum(sapply(perfil, function(x) length(x$producao_bibiografica$LIVRO$ano)))
sum(sapply(perfil, function(x) length(x$producao_bibiografica$PERIODICO$ano)))
sum(sapply(perfil, function(x) length(x$producao_bibiografica$TEXTO_EM_JORNAIS$ano)))
# Número de pessoas por quantitativo de produções por pessoa 0 = 1; 1 = 2...
table(unlist(sapply(perfil, function(x) length(x$producao_bibiografica$ARTIGO_ACEITO$ano))))
table(unlist(sapply(perfil, function(x) length(x$producao_bibiografica$CAPITULO_DE_LIVRO$ano))))
table(unlist(sapply(perfil, function(x) length(x$producao_bibiografica$LIVRO$ano))))
table(unlist(sapply(perfil, function(x) length(x$producao_bibiografica$PERIODICO$ano))))
table(unlist(sapply(perfil, function(x) length(x$producao_bibiografica$TEXTO_EM_JORNAIS$ano))))
# Número de produções por ano
table(unlist(sapply(perfil, function(x) (x$producao_bibiografica$ARTIGO_ACEITO$ano))))
table(unlist(sapply(perfil, function(x) (x$producao_bibiografica$CAPITULO_DE_LIVRO$ano))))
table(unlist(sapply(perfil, function(x) (x$producao_bibiografica$LIVRO$ano))))
table(unlist(sapply(perfil, function(x) (x$producao_bibiografica$PERIODICO$ano))))
table(unlist(sapply(perfil, function(x) (x$producao_bibiografica$TEXTO_EM_JORNAIS$ano))))
# Número de pessoas que realizaram diferentes tipos de orientações
length(unlist(sapply(perfil, function(x) names(x$orientacoes_academicas))))
# Número de pessoas por tipo de orientação
table(unlist(sapply(perfil, function(x) names(x$orientacoes_academicas))))
#Número de orientações concluidas
sum(sapply(perfil, function(x) length(x$orientacoes_academicas$ORIENTACAO_CONCLUIDA_MESTRADO$ano)))
sum(sapply(perfil, function(x) length(x$orientacoes_academicas$ORIENTACAO_CONCLUIDA_DOUTORADO$ano)))
sum(sapply(perfil, function(x) length(x$orientacoes_academicas$ORIENTACAO_CONCLUIDA_POS_DOUTORADO$ano)))

# Número de pessoas por quantitativo de orientações por pessoa 0 = 1; 1 = 2...
table(unlist(sapply(perfil, function(x) length(x$orientacoes_academicas$ORIENTACAO_CONCLUIDA_MESTRADO$ano))))
table(unlist(sapply(perfil, function(x) length(x$orientacoes_academicas$ORIENTACAO_CONCLUIDA_DOUTORADO$ano))))
table(unlist(sapply(perfil, function(x) length(x$orientacoes_academicas$ORIENTACAO_CONCLUIDA_POS_DOUTORADO$ano))))

# Número de orientações por ano
table(unlist(sapply(perfil, function(x) (x$orientacoes_academicas$ORIENTACAO_CONCLUIDA_MESTRADO$ano))))
table(unlist(sapply(perfil, function(x) (x$orientacoes_academicas$ORIENTACAO_CONCLUIDA_DOUTORADO$ano))))
table(unlist(sapply(perfil, function(x) (x$orientacoes_academicas$ORIENTACAO_CONCLUIDA_POS_DOUTORADO$ano))))

###Análise dos dados em formato Data Frame
#Arquivo Profile por Currículo
# extrai perfis dos professores 
perfil.df.professores <- extrai.perfis(co.perfil)

# extrai producao bibliografica de todos os professores 
perfil.df.publicacoes <- extrai.producoes(perfil)

#extrai orientacoes 
perfil.df.orientacoes <- extrai.orientacoes(perfil)

#extrai areas de atuacao 
perfil.df.areas.de.atuacao <- extrai.areas.atuacao(perfil)

#cria arquivo com dados quantitativos para análise
perfil.df <- data.frame()
perfil.df <- perfil.df.professores %>% 
  select(idLattes, nome, resumo_cv, senioridade) %>% 
  left_join(
    perfil.df.orientacoes %>% 
      select(orientacao, idLattes) %>% 
      filter(!grepl("EM_ANDAMENTO", orientacao)) %>% 
      group_by(idLattes) %>% 
      count(orientacao) %>% 
      spread(key = orientacao, value = n), 
    by = "idLattes") %>% 
  left_join(
    perfil.df.publicacoes %>% 
      select(tipo_producao, idLattes) %>% 
      filter(!grepl("ARTIGO_ACEITO", tipo_producao)) %>% 
      group_by(idLattes) %>% 
      count(tipo_producao) %>% 
      spread(key = tipo_producao, value = n), 
    by = "idLattes") %>% 
  left_join(
    perfil.df.areas.de.atuacao %>% 
      select(area, idLattes) %>% 
      group_by(idLattes) %>% 
      summarise(n_distinct(area)), 
    by = "idLattes")

glimpse(perfil.df)


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
public.periodico.df <- pub.ls2df(ci.public, 1) #artigos
public.livros.df <- pub.ls2df(public, 2) #livros
public.eventos.df <- pub.ls2df(public, 5) #eventos
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


####
#Orientação
#Análise dos dados em formato lista
##Número de Orientações Mestrado e Doutorado
sum(sapply(orient$ORIENTACAO_CONCLUIDA_DOUTORADO, function(x) length(x$natureza))) + 
  sum(sapply(orient$ORIENTACAO_CONCLUIDA_MESTRADO, function(x) length(x$natureza)))

##Análise dos dados no formato DF
orient.posdoutorado.df <- ori.ls2df(orient, 6) #pos-Doutorado concluído
orient.doutorado.df <- ori.ls2df(orient, 7) #Doutorado concluído
orient.mestrado.df <- ori.ls2df(orient, 8) #Mestrado concluído

orient.df <- rbind(rbind(orient.posdoutorado.df, orient.doutorado.df), orient.mestrado.df)

ggplot(orient.df,aes(ano,fill=natureza)) +
  geom_bar(stat = "count", position="dodge") +
  ggtitle("Natureza das Orientações Completas Por Ano") +
  theme(legend.position="right",legend.text=element_text(size=7)) +
  guides(fill=guide_legend(nrow=5, byrow=TRUE, title.position = "top")) +
  labs(x="Ano",y="Quantidade")

###
#Grafo
g <- g.ls2ig(graphl)
df <- as.data.frame(V(g)$name); colnames(df) <- "Idlattes"
df <- left_join(df, df.prog, by = "Idlattes")
#Apenas para fins de análise inicial, foram retiradas as observações 
#com duplicação de pesquisadores no caso de haver professores em mais 
#de um programa
df <- df %>% group_by(Idlattes) %>% 
  slice(1L)
V(g)$programa <- df$Programa
V(g)$orient_dout <- perfil.df$ORIENTACAO_CONCLUIDA_DOUTORADO
V(g)$orient_mest <- perfil.df$ORIENTACAO_CONCLUIDA_MESTRADO
V(g)$publicacao <- perfil.df$PERIODICO
V(g)$eventos <- perfil.df$EVENTO

###
#Areas de pesquisa
area.df <- area.ls2df(res.area)
