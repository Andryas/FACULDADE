## =============================================================================
## Pacotes utilizados.
pacotes <- list("mailR","XML")
# Verifica pacotes instalados e instala caso necessário
# sapply(pacotes,FUN = function(x) 
#   if (!x %in% installed.packages()) install.packages(x))
## Funções auxiliares para os scripts de web-scraping.
## =============================================================================
## Chama pacotes
# ------------------------------------------------------------------------------
sapply(pacotes, require, character.only = TRUE)

# Função para limpar a lista de notícias de erros.
# ------------------------------------------------------------------------------

CleanDF <- function(x){
  i <- 1
  j <- 1
  n <- nrow(x)
  del <- integer()
  # Retira a posição das notícias com textos menores que 100 caracteres.
  for(i in i:n){
    if(nchar(x[i,3])<200 | is.na(x[i,3])){
      del[j] <- i
      j <- j + 1
    }
  }
  del
}


# Função para resolver problemas de não captura da url
# ------------------------------------------------------------------------------

CleanUrl <- function(x){
  # Url invalidas. (Erros cometidos ao por a url linkado ao titulo de uma materia.)
  Inv_url <- paste0("http://politica.estadao.com.br/blogs/coluna-do-estadao|",
                    "http://novoemfolha.blogfolha.uol.com.br")
  bad_urls <- grepl(Inv_url,x = x)
  i <- 1
  j <- 1
  del <- integer()
  n <- length(x)
  for(i in i:n){
    if(nchar(x[i])<10 | bad_urls[i]){
      del[j] <- i
      j <- j+1
    }    
  }
  del
}


