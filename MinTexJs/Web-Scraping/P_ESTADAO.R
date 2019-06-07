#!/usr/bin/Rscript
source('./fun/fun.R',echo=TRUE)

# Informações email
#-------------------------------------------------------------------------------
info <- read.table(path.expand("./fun/info.txt"),sep = ":",header = T,
                   stringsAsFactors = F)
info$Campo[4] <- tolower(info$Campo[4])
#-------------------------------------------------------------------------------
# - Guarda workspace
work1 <- getwd()

# Arquivo para log
name <- paste0("P_ESTADÃO_",as.integer(Sys.Date()),".txt")

# Direciona para pasta log
setwd("../log")

# - Guarda workspace desta pasta
work2 <- getwd()

# Cria arquivo
log <- file(name,open = 'a')
cat(as.character(Sys.time()),file = log,sep = "\n")
#-------------------------------------------------------------------------------
# # Simula um usúario e abre o browser.
# rD <- rsDriver(browser = "firefox")
# remDr <- rD[["client"]]
# 
# # Direciona a url para link abaixo.
# remDr$navigate("http://politica.estadao.com.br/")
# 
# # Espera necessária devido a propagadas
# Sys.sleep(15)
# 
# i <- 1
# # Loop para descer a página
# for(i in i:3){
#   remDr$findElement("css", "body")$sendKeysToElement(list(key = "end"))
#   Sys.sleep(2)
#   remDr$findElement('xpath',
#          "//div/a[@class='go more-list-news btn-mais fn brd-e']")$clickElement()
#   Sys.sleep(1)
# }
# 
# h <- htmlParse(remDr$getPageSource()[[1]])

h <- htmlParse("http://politica.estadao.com.br/")
## - Path: parte 1
#-------------------------------------------------------------------------------

# http
path_href_1 <- "//section/div/div/a[1]"

# Manchete
path_tit_1 <- "//section/div/div/a/h3"

# Dentro do http - data de publicação
path_publicado_1 <- "//section/div/div[@class='n--noticia__state']/p[2]"

# Dentro do http matéria
path_materia_1 <- "//section/div/div/p"

## - Textos
#-------------------------------------------------------------------------------
# http 
href_part_1 <- as.character(xpathSApply(h,path_href_1,
                                        fun = xmlGetAttr,name ='href'))

# manchete
tit_part_1 <- xpathSApply(h,path_tit_1,fun = xmlValue,trim=T)
tit_part_1 <- as.character(gsub(pattern = "[\n|\t|\"]",tit_part_1,
                                replacement = ""))

# Verificar arquivo fun.R para mais detalhes. 
del <- CleanUrl(href_part_1)

# Limpa http
href_part_1 <- if(length(del)>0){href_part_1[-del]}else{href_part_1}
# Limpa manchetes
tit_part_1 <- if(length(del)>0){tit_part_1[-del]}else{tit_part_1}

# - Path: 2
#-------------------------------------------------------------------------------

# http
path_href_2 <- paste0("//section[@class='col-md-6 col-sm-6 col-xs-12",
                    " col-margin' or @class='col-md-12 col-sm-12 col-xs-12",
                    " init item-lista']//a[@class='link-title']")
# Manchete
path_tit_2 <- paste0("//section[@class='col-md-6 col-sm-6 col-xs-12",
                    " col-margin' or @class='col-md-12 col-sm-12 col-xs-12",
                    " init item-lista']//a/h3[@class='third']")

## - Textos
#-------------------------------------------------------------------------------

# http
href_part_2 <- as.character(xpathSApply(h,path_href_2,
                                        fun = xmlGetAttr,name = 'href'))

# Manchetes
tit_part_2 <- xpathSApply(h,path_tit_2,fun = xmlValue,trim=T)
tit_part_2 <- as.character(gsub(pattern = "[\n|\t|\"]",tit_part_2,
                                replacement = ""))

# Verificar arquivo fun.R para mais detalhes.
del <- CleanUrl(href_part_2)

# Limpa http
href_part_2 <- if(length(del)>0){href_part_2[-del]}else{href_part_2}

# Limpa manchetes
tit_part_2 <- if(length(del)>0){tit_part_2[-del]}else{tit_part_2}


# - Extrair
#-------------------------------------------------------------------------------
# Data Frame
noticias <- data.frame(Titulo=0,Url=0,Materia=0,Jornal=0,Publicado=0)

# Manchetes
manchete <- c(tit_part_1,tit_part_2)

# Url 
url <- c(href_part_1,href_part_2)


n <- length(manchete)
i <- 1
try(
for(i in i:n){
  # Manchete
  noticias[i,1] <- manchete[i]
  
  # Url
  noticias[i,2] <- url[i]
  
  # Parse da página da matéria
  h <- try(htmlParse(url[i],isURL = T),silent = T)
  h <- if(class(h)[1]=="try-error"){
          cat(paste0("Erro: ",url[i],"   ",
                     attr(h,'condition'),"\n"),file = log)
          next
       }else{
          h
            }
  x <- xpathSApply(h,path_publicado_1,fun = xmlValue,trim=T)
  x <- if(is.null(x)){"01 Janeiro 9999"}else{x}
  
  # Data da publicação
  noticias[i,5] <- regmatches(x,regexpr("[0-9]{2} [A-z]+ [0-9]{4}",x))
  noticias[i,5] <- as.character(strptime(noticias[i,5],
                                            format = "%d %B %Y",tz = ""))
  
  # Matéria
  l <- xpathSApply(h,path_materia_1,fun = xmlValue,trim=T)
  
  # Retira o último texto porque é desnecessário. (Caso particular de blog)
  l <- list(l[-length(l)])
  
  m <- length(l[[1]])
  j <- 1
  for(j in j:m){
    # Agrupa o texto dividido em várias partes em uma só.
    l[[1]][j] <- paste0(l[[1]][j]," \n")
  }
  l <- paste(l[[1]],collapse = "")
  noticias[i,3] <- l
  noticias[i,4] <- "ESTADAO"
  cat("Notícias: ",i,"\n")
}
)

# Informações do script
#-------------------------------------------------------------------------------

log_text <- paste0("Quantidade total de matérias capturadas: ",nrow(noticias))
cat(log_text,file = log,sep = "\n")

# Verificar arquivo fun.R para mais detalhes.
del <- CleanDF(noticias)
noticias <- if(length(del) > 0){noticias[-del,]}else{noticias}

# Quantidade de matérias extraídas.
log_text_2 <- paste0("Quantidade de matérias verificadas e validas: ",
                          nrow(noticias))
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

# Finaliza o Log
cat("Update completado.",file = log,sep = "\n")

# - Envia email-log
#-------------------------------------------------------------------------------

try(if(info$Campo[4] == "sim"){
  send.mail(to = c(info$Campo[1]),
          from = info$Campo[2],
          subject = "SCRIPT-ESTADAO",
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
rm(list = ls())
#-------------------------------------------------------------------------------