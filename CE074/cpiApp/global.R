library(stringr)

length_line <- function(s,k) {
  split_str_by_index <- function(target, index) {
    index <- sort(index)
    substr(rep(target, length(index) + 1),
           start = c(1, index),
           stop = c(index -1, nchar(target)))
  }
  
  interleave <- function(v1,v2){
    ord1 <- 2*(1:length(v1))-1
    ord2 <- 2*(1:length(v2))
    c(v1,v2)[order(c(ord1,ord2))]
  }
  
  insert_str <- function(target, insert, index) {
    insert <- insert[order(index)]
    index <- sort(index)
    paste(interleave(split_str_by_index(target, index), insert), collapse="")
  }
  
  x <- readLines(paste0("textos/text",k), encoding = "latin1")
  
  lista_final <- list()
  
  # s = split
  s <- s
  
  # pos = posição dos campos brancos
  comp <- length(x)
  j <- 1
  for(j in j:comp){
    pos <- as.data.frame(str_locate_all(x[j]," ")[[1]])$start
    
    # Positions menores que s
    poss <- integer()
    init <- 0
    i <- 1
    n <- ceiling(max(pos)/s)
    
    for(i in i:n) {
      init <- pos[which(pos < (s+init))]
      init <- init[length(init)]
      poss[i] <- init
    }
      
    y <- insert_str(x[j],rep("\n",times = length(poss)),poss)
  
    fn <- strsplit(y,"\n")
  
    fn <- sapply(fn, trimws)
    
    if(nchar(fn[length(fn)-1])+nchar(fn[length(fn)]) <= s){
      fn[length(fn)-1] <- paste0(fn[length(fn)-1]," ",fn[length(fn)])
      fn <- fn[1:(length(fn)-1)]
    }
    
    
    lista_final[[j]] <- fn
  }
  lista_final
}


htmliza <- function(x){
  x <- sapply(x, FUN = function(x){
  paste0("<p>",x,"</p>")
    })

  n <- length(x)
  i <- 1

  for(i in i:n){
    m <- length(x[[i]]) + 1
    x[[i]][m] <- "<br />"
  }
  unlist(x)
}
