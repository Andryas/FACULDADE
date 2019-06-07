## ===============================================================================
## Pacotes utilizados
pacotes <- list("tm","SnowballC","topicmodels",
                "ggplot2","plyr","wordcloud","reshape2")
# # Verifica pacotes instalados e instala caso necessário
# sapply(pacotes,FUN = function(x) 
#   if (!x %in% installed.packages()) install.packages(x))
## Funções auxiliares para o shiny
## ===============================================================================
# Carrega dados advindos do web-scraping
load("../Web-Scraping/Pdata.RData")
info <- read.table("../Web-Scraping/fun/info.txt",header = T,sep = ":",stringsAsFactors = F)
df <- noticias ; rm(noticias)

## Chama pacotes
# --------------------------------------------------------------------------------
sapply(pacotes, require, character.only = TRUE)
# --------------------------------------------------------------------------------

# Preprocessamento: Cria o Corpus
Preprocessamento <- function(df,start,end,
                             jornal=c("ESTADAO","FolhaSP","OGLOBO"),
                             rST = 0.9){
  df <- subset(df,df$Publicado <= end & df$Publicado >= start & 
                 df$Jornal %in% jornal)
  # Cria o Corpus
  cps <- VCorpus(VectorSource(x = df$Materia),
                 readerControl = list(language = "pt_br",
                                      load = T))
  # Meta-dados
  #-------------------------------------------------------------------------------
  # Veiculo
  meta(cps,type = "local",tag = "Jornal") <- df$Jornal
  # Titulo
  meta(cps,type = "local",tag = "Titulo") <- df$Titulo
  # Publicado
  meta(cps,type = "local",tag = "Publicado") <- df$Publicado
  # Texto
  meta(cps,type = "local", tag = "Materia") <- df$Materia
  
  # Preprocessamento
  #-------------------------------------------------------------------------------
  # Documentos para caixa-baixa
  cps <- tm_map(cps, FUN = content_transformer(tolower))
  # Remove pontuação
  cps <- tm_map(cps, FUN = removePunctuation,preserve_intra_word_dashes= TRUE)
  # Remove números
  cps <- tm_map(cps, FUN = removeNumbers)
  # Remove espaços em branco
  cps <- tm_map(cps, FUN = stripWhitespace)
  # Remove stopwords
  cps <- tm_map(cps,
                FUN = removeWords,
                words = c(stopwords("portuguese"),"ser"))
  
  # Cria matrix de documentos e termos
  dtm <- DocumentTermMatrix(cps)
  
  # Remove esparsidade da matrix
  dtm <- removeSparseTerms(dtm,sparse = rST)
  
  return(list(dtm,cps))
}


# Modelagem de tópicos: LDA
ModelTopic <- function(dtm,cps, k = 2){
  
  # Modelagem de tópicos
  #-------------------------------------------------------------------------------
  # Cria k tópicos
  lda <- LDA(dtm,k = k,control = list(seed = 7))
  
  # Termos ponderados
  words <- as.data.frame(t(lda@beta))
  
  
  # Processamento da informação 
  #-------------------------------------------------------------------------------
  # Traz o nome do termo
  names(words) <- terms(lda)
  
  # Junta os termos com seus respectivos tópicos
  words <- cbind(term = lda@terms,words)
  
  # 15 termos mais frequentes por tópico
  tops <- lapply(words[, terms(lda)],
                 FUN = function(x) {
                   o <- head(order(x, decreasing = TRUE), n = 15)
                   data.frame(term = words$term[o],
                              lprob = x[o])
                 })
  tops <- ldply(tops, .id = "topic")
  
  # Topicos
  topicos <- as.character(terms(lda))
  
  # i <- 1
  # j <- 1
  # for(i in i:length(topicos)){
  #   if(j==3){
  #     j <- 1
  #   }
  #   topicos[i] <- paste0("<div class='column",j,"'>","<h1 id=",i,
  #               " class= needed>", topicos[i], "</h1></div>")
  #   j <- j + 1
  # }
  # 
  
  # Titulos
  #-------------------------------------------------------------------------------
  # Recupera titulo dos textos do corpus
  tit <- sapply(cps,meta,tag = "Titulo")
  
  # Separa cada titulo pelos seu topico correspondente
  tit <- split(tit,topics(lda))
  names(tit) <- topicos
  i <- 1
  j <- 1
  for(i in i:length(tit)){
    for(j in j:length(tit[[i]])){
      tit[[i]][j] <- paste0("<li id=",j," class= needed2>",tit[[i]][[j]],"</li>")
      j <- j + 1
    }
    i <- i +1
    j <- 1
  }
  
  # Materias
  #-------------------------------------------------------------------------------
  # Recupera cada matéria do corpus
  mat <- sapply(cps,meta,tag = "Materia")
  
  # Separa cada matéria pelo seu tópico correspondente
  mat <- split(mat,topics(lda))
  names(mat) <- terms(lda)
  
  # Data de publicação
  #-------------------------------------------------------------------------------
  # Recupera cada data de publicação do corpus
  Dt_public <- sapply(cps,meta,tag = "Publicado")
  
  # Transforma em Data
  Dt_public <- as.POSIXct(Dt_public)
  
  # Proporção de cada termo 
  prop <- as.data.frame(lda@gamma)
  names(prop) <- terms(lda)
  
  # Data frame de tempo e proporção
  prop <- cbind(Dt= Dt_public,prop)
  
  # Transforma as colunas topicos em coluna factor de topicos
  prop <- melt(prop,id.var="Dt")
  
  
  # Lista:  titulos; materias; palavras frequentes nos topicos e proporção 
  # de matérias por dia por tópicos
  r <- list(tops,tit,mat,prop)
  
  return(r)
}



