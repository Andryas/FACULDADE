#!/usr/bin/Rscript
source('./fun/fun.R', echo=TRUE)

# Informações email e banco de dados
#-------------------------------------------------------------------------------
info <- read.table(path.expand("./fun/info.txt"),sep = ":",header = T,
                   stringsAsFactors = F)
info$Campo[4] <- tolower(info$Campo[4])
#-------------------------------------------------------------------------------
# - Guarda workspace
work1 <- getwd()

# Arquivo para log
name <- paste0("P_G1",as.integer(Sys.Date()),".txt")

# Direciona para pasta log
setwd("../log")

# - Guarda workspace desta pasta
work2 <- getwd()

# Cria arquivo
log <- file(name,open = 'a')
cat(as.character(Sys.time()),file = log,sep = "\n")
#-------------------------------------------------------------------------------
# Simula um usúario e abre o browser.
# rD <- rsDriver(browser = "firefox")
# remDr <- rD[["client"]]
# 
# # Direciona a url para link abaixo.
# remDr$navigate("http://g1.globo.com/politica/")
# 
# # Encontra o fim da página
# webElem <- remDr$findElement("css", "body")
# 
# i <- 1
# # Desce até o fim da página.
# for(i in i:4){
#   Sys.sleep(3)
# webElem$sendKeysToElement(list(key = "end"))
# }

h <- htmlParse("http://g1.globo.com/politica/")

# - Path
#-------------------------------------------------------------------------------
# Manchete
path_tit_1 <- "//div/a[@class='feed-post-link']/div/div/p"

# http
path_href_1 <- "//div/a[@class='feed-post-link']"

# - Texto
#-------------------------------------------------------------------------------
# Manchete
tit_part_1 <- xpathSApply(h,path_tit_1,fun = xmlValue)

# http
href_part_1 <- as.character(xpathSApply(h,path_href_1,fun = xmlGetAttr, 
                                        name = "href"))

# Verificar arquivo fun.R para mais detalhes. 
del <- CleanUrl(href_part_1)

# Limpa http
href_part_1 <- if(length(del)>0){href_part_1[-del]}else{href_part_1}

# Limpa manchetes
tit_part_1 <- if(length(del)>0){tit_part_1[-del]}else{tit_part_1}

# Blog e não Blog
#-------------------------------------------------------------------------------
# Verifica quais são matérias do blog.
tipo_path <- grepl("http://g1.globo.com/politica/blog",href_part_1)

# Data publicado - blog
path_blog_publicado <- "//header/time[@class='post-date']"

# Matéria - blog
path_blog_materia <- "//article/section/p"

# Data publicado - não blog
path_nblog_publicado <- "//main/div/div/p/time"

# Matéria - não blog
path_nblog_materia <- "//main/div/p"


# - Extrair
#-------------------------------------------------------------------------------
# Data Frame
noticias <- data.frame(Titulo=0,Url=0,Materia=0,Jornal=0,Publicado=0)

n <- length(tit_part_1)
i <- 1
for(i in i:n){
  # Manchete
  noticias[i,1] <- tit_part_1[i]
  # Url
  noticias[i,2] <- href_part_1[i]
  if(tipo_path[i]){
    h <- htmlParse(noticias[i,2])
    x <- xpathSApply(h,path_blog_publicado,fun = xmlGetAttr,name = 'datetime')
    
    # Data publicado
    noticias[i,5] <- regmatches(x,regexpr("[0-9]{4}-[0-9]{2}-[0-9]{2}",x))
    
    # Matéria
    l <- xpathSApply(h,path_blog_materia,fun = xmlValue,trim=T)
    
    m <- try(length(l[[1]]),T)
    j <- 1
    m <- if(class(m)=="try-error"){
      next
    }else{m}
    for(j in j:m){
      # Agrupa o texto dividido em várias partes em uma só
      l[[1]][j] <- paste0(l[[1]][j]," \n")
    }
    l <- paste(l[[1]],collapse = "")
    noticias[i,3] <- l
    noticias[i,4] <- "OGLOBO"
    cat("Noticia: ",i,"\n")
  }else{
    h <- htmlParse(noticias[i,2])
    x <- xpathSApply(h,path_nblog_publicado,fun = xmlGetAttr,
                     name = 'datetime')
    
    # Data publicado
    noticias[i,5] <- regmatches(x,regexpr("[0-9]{4}-[0-9]{2}-[0-9]{2}",x))
    
    # Matéria
    l <- list(xpathSApply(h,path_nblog_materia,fun = xmlValue,trim=T))
    
    m <- length(l[[1]])
    j <- 1
    for(j in j:m){
      l[[1]][j] <- paste0(l[[1]][j]," \n")
    }
      l <- paste(l[[1]],collapse = "")
      noticias[i,3] <- l
      noticias[i,4] <- "OGLOBO"
      cat("Noticia: ",i,"\n")
  }
}

# Informação do script
#-------------------------------------------------------------------------------
cat(paste0("Quantidade total de matérias capturadas: ",nrow(noticias)),
                                                file = log,sep = "\n")

# Verificar arquivo fun.R para mais detalhes
del <- CleanDF(noticias)
noticias <- if(length(del) > 0){noticias[-del,]}else{noticias}

# Quantidade de matérias extraídas
cat(paste0("Quantidade de matérias verificas e validadas: ",
                                    nrow(noticias)),file =log,sep = "\n")

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
noticias <- noticias[!duplicated(dp),] ; rm(dp)

noticias_novas <- nrow(noticias)-noticias_novas
# Update
save(noticias,file = "Pdata.RData")

# Quantidade de noticias novas
cat(paste0("> Quantidade de matérias novas: ",noticias_novas),file = log,sep = "\n")

# Finaliza o Log
cat("Update completado.",file = log,sep = "\n")

# - Envia email-log
#-------------------------------------------------------------------------------

try(if(info$Campo[4] == "sim"){
  send.mail(to = c(info$Campo[1]),
          from = info$Campo[2],
          subject = "SCRIPT-G1",
          body = " ",
          # encoding = "utf-8",
          attach.files = c(paste0(work2,"/",name)),
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

