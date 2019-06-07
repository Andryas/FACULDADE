#!/usr/bin/Rscript
source('./fun/fun.R', echo=TRUE)

# Informações email
#-------------------------------------------------------------------------------
info <- read.table(path.expand("./fun/info.txt"),sep = ":",header = T,
                   stringsAsFactors = F)
info$Campo[4] <- tolower(info$Campo[4])
# Workspace
#-------------------------------------------------------------------------------
# - Guarda workspace
work1 <- getwd()

# Arquivo para log
name <- paste0("P_UOL_",as.integer(Sys.Date()),".txt")

# Direciona para pasta log
setwd("../log")

# - Guarda workspace desta pasta
work2 <- getwd()

# Cria arquivo
log <- file(name,open = 'a')
cat(as.character(Sys.time()),file = log,sep = "\n")
#-------------------------------------------------------------------------------

h <- htmlParse("http://www1.folha.uol.com.br/poder/?cmpid=menulate")

# - Path: parte 1
#-------------------------------------------------------------------------------

# Manchete
path.1 <- "//section/ol/li/a/div/h3"

# http
path.2 <- "//section/ol/li/a"

# Data publicado
path.3 <- "//section/ol/li/a/div/time"

# - Texto: parte 1
#-------------------------------------------------------------------------------

# Manchete
x.1 <- xpathSApply(h,path.1,fun = xmlValue,trim = T)
x.1 <- as.character(gsub(pattern = "[\n|\t|\"]",x.1,replacement = ""))

# http
x.2 <- as.character(xpathSApply(h,path.2,fun =xmlGetAttr,name = "href"))

# Data Publicado
x.3 <- as.character(strptime(xpathSApply(h,path.3,fun = xmlGetAttr,
                                      name = "datetime"),format = "%Y-%m-%d"))

# - Path: parte 2
#-------------------------------------------------------------------------------

# Manchete
path.4 <- "//div/ol/li/ol/li/a"

# http
path.5 <- "//div/ol/li/ol/li/a"

# Data publicado
path.6 <- "//div/ol/li/ol/li/time"

# - Texto: parte 2
#-------------------------------------------------------------------------------

# Manchete
x.4 <- xpathSApply(h,path.4,fun = xmlValue,trim = T)
x.4 <- as.character(gsub(pattern = "[\n|\t|\"]",x.4,replacement = ""))

# http
x.5 <- as.character(xpathSApply(h,path.5,fun = xmlGetAttr,name="href"))

# Data publicado
x.6 <- as.character(strptime(xpathSApply(h,path.6,fun = xmlGetAttr,
                                        name = "datetime"),format = "%Y-%m-%d"))


# Concatena vetores com Manchetes, Url's e Data de publicação
#-------------------------------------------------------------------------------
# Manchete
manchete <- c(x.1,x.4)

# Url
url <- c(x.2,x.5)

# Data publicado
dtpub <- c(x.3,x.6)


# - Extrair
#-------------------------------------------------------------------------------
# Data-frame das notícias
# Titulo; Url; DtPublicado; Matéria e Jornal.
noticias <- data.frame(Titulo=0,Url=0,Materia=0,Jornal=0,Publicado=0)

n <- length(manchete)
i <- 1
for(i in i:n){
    noticias[i,1] <- manchete[i]
    noticias[i,2] <- url[i]
    noticias[i,5] <- dtpub[i]
    noticias[i,4] <- "FolhaSP"
}

# Retira urls com problema
del <- CleanUrl(noticias$Url)
noticias <- if(length(del)>0){noticias[-del,]}else{noticias}

n <- nrow(noticias)
i <- 1
for(i in i:n){
  url <- noticias[i,2]
  h <- try(htmlParse(url,isURL = T,validate = T),silent = T)
  h <- if(class(h)[1]=="try-error"){
    cat(paste0("Erro: ",url,"   ",
               attr(h,'condition'),"\n"),file = log)
    next
  }else{
    h
  }
  path <- "//article/div/p"
  l <- list(xpathSApply(h,path = path,fun = xmlValue,trim = T))
  m <- length(l[[1]])
  j <- 1
  for(j in j:m){
    l[[1]][j] <- paste0(l[[1]][j]," \n")
  }
  l <- paste0(l[[1]],collapse = "")
  noticias[i,3] <- l
  cat("Noticia: ",i,"\n")
}

n <- nrow(noticias)

# Informação do script
#-------------------------------------------------------------------------------
log_text <- paste0("Quantidade total de matérias capturadas: ",n)
cat(log_text,file = log,sep = "\n")

# Verificar arquivo fun.R para mais detalhes
del<- CleanDF(noticias)
noticias <- if(length(del) > 0){noticias[-del,]}else{noticias}

n <- nrow(noticias)

# Quantidade de matérias extraídas
log_text_2 <- paste0("Quantidade de matérias validas: ",n)
cat(log_text_2,file = log,sep = "\n")

# Update RData
#-------------------------------------------------------------------------------
setwd(work1)

new <- noticias ; rm(noticias)
load("Pdata.RData")
noticias_novas <- nrow(noticias)
noticias <- rbind(noticias,new) ; rm(new)
  
# Seleciona colunas importantes para removação de duplicatas
dp <- noticias[,c(1,4)]

# Remove duplicados
noticias <- noticias[!duplicated(dp),]

noticias_novas <- nrow(noticias)-noticias_novas
# Update
save(noticias,file = "Pdata.RData")

# Quantidade de noticias novas
cat(paste0("> Quantidade de matérias novas: ",noticias_novas),file = log,sep = "\n")

# Finaliza o log
cat("Update Completado.",file = log,sep = "\n")

# - Envia email-log
#-------------------------------------------------------------------------------
try(if(info$Campo[4] == "sim"){
  send.mail(to = c(info$Campo[1]),
          from = info$Campo[2],
          subject = "SCRIPT-UOL",
          body = " ",
          # encoding = "utf-8",
          attach.files = c(paste0(work2,'/',name)),
          smtp = list(
            host.name = "smtp.gmail.com",
            port = 465,
            ssl = TRUE,
            user.name = info$Campo[2],
            passwd = info$Campo[3]),
          authenticate = TRUE,
          send = TRUE
  )
})
close(log)
setwd(work1)
rm(list = ls())
#-------------------------------------------------------------------------------