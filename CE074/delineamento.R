# Experimento 2^4

# Font-family: serif e arial
# Font-size: 14px e 18px
# line-height: 120% e 180%
# qtc: 60 e 75 # quantidade de caracteres

options(scipen = 5,digits = 5)

text <- paste0("text", 1:4)
pessoa <- c("I","II","III","IV")
ql <- expand.grid(pessoa,text)
ql$trat <- NA
ql$tempo <- NA

font_family <- c("serif","arial")
font_size <- c("14px","18px")
line_height <- c("120%","180%")
width_text <- c(60,75)

comb <- expand.grid(font_family,font_size,line_height,width_text,
                    stringsAsFactors = FALSE)
colnames(comb) <- c("font-family","font-size","line-height","width-text")

set.seed(123)

index <- as.integer(runif(12,100,10000))
# duplicated(index)
index <- list(index[1:4],index[5:8],index[9:12])
seed <- c(07031995,19950703,03199507) # Semente
i <- 1

l <- list()

for(i in 1:3){
  set.seed(seed[i])
  ql$id <- rep(c(index[[i]]),times = 4)
  colnames(ql) <- c("pessoa","texto","trat","tempo","id")
  # Aleatoriza combinações e aplica aos blocos
  index2 <- sample(1:nrow(comb),nrow(comb),replace = FALSE) 
  comb2 <- comb[index2,]
  j <- 1
  for(j in j:nrow(comb)){
    ql$trat[j] <- paste(comb2[j,],collapse = ";")  
  }
  l[[i]] <- ql
}


qlf <- dplyr::bind_rows(l)
qlf$rep <- rep(c("rep1","rep2","rep3"), each = 16)

write.csv(qlf,file = "ql.csv",row.names = FALSE)
saveRDS(qlf, "ql.rds")
# ------------------------------------------------------------------------------
# Aleatorização das pessoas que vão executar o experimento

set.seed(1234567)

# Nome das pessoas que vão participar
nomes <- c(paste0("Amigo ",1:12))

ids <- unique(qlf$id)

cbind(
  nomes[sample(1:length(nomes),length(nomes),replace = FALSE)],
  ids[sample(1:length(ids),length(ids),replace = FALSE)]
)
# ==============================================================================

