knitr::opts_chunk$set(
  message = FALSE,
  warning = FALSE,
  fig.align = "center",
  fig.height = 7,
  fig.width = 11
)

library(knitr)
library(ggplot2)

# Gerador de splines
# n: número de observalções
# pq: pontos de quebra
myrandom <- function(n, pq = NULL) {
  x <- runif(n, 0, 25)
  y <- 50 + rnorm(n, sd = 2)
  
  # Ponto de quebra
  if(max(pq) > 25 | min(pq) < 0) stop("pq (ponto de quebra, deve estar entre 0-25")
  if(is.null(pq)) stop("pq é um argumento obrigátorio")
  
  lxs <- lapply(pq, function(b) {
    ifelse(x <= b, 0, x - b)
  })

  xls <- do.call(cbind, lxs)
  colnames(xls) <- paste0("x", pq)
  
  df <- as.data.frame(cbind(y, x, xls))
  
  fit <- lm(y ~ ., data = df)
  
  df$y <- predict(fit) + rnorm(n)
  
  df[,c("y","x")]
}


df <- myrandom(100,pq = c(10,15))

ggplot(df, aes(x = x, y = y)) + geom_point()                 

# mc: (TRUE) média constante (FALSE) regressão linear
# pq: ponto de quebra
reg_segmentada <- function(x, y, pq = NULL , mc = TRUE) {
  # Ponto de quebra
  if(max(pq) > 25 | min(pq) < 0) stop("pq (ponto de quebra, deve estar entre 0-25")
  if(is.null(pq)) stop("pq é um argumento obrigátorio")
  pq <- cut(x, unique(c(0,pq,25)) , include.lowest = TRUE)
  
  df <- data.frame(x = x, y = y, r = pq, stringsAsFactors = FALSE)
  
  # Separa os pontos de quebra
  df <- lapply(unique(df$r), function(x) {
     df[df$r == x, ]
  })  
  
  # Modelo de média constante
  if (isTRUE(mc)) {
    df <- lapply(df, function(w) {
      w$pred <- predict(lm(y ~ 1, data = w))
      w
    })
  } else {
    # Modelo de regressão linear simples
    df <- lapply(df, function(w) {
      w$pred <- predict(lm(y ~ x, data = w))
      w
    })
  }
  
  do.call(rbind, df)
}

dfseg <- reg_segmentada(df$x, df$y , pq = c(10,18))

# Modelo de média consante
ggplot(dfpart, aes(x,y, group = r, color = r)) + geom_point() + 
  geom_smooth(aes(x = x, y = pred), method = "lm")

# Modelo de regressão linear simples
dfseg2 <- reg_segmentada(df$x,df$y , pq = c(10, 18), mc = FALSE)

ggplot(dfpart2, aes(x,y,group = r, color = r)) + geom_point() + 
  geom_smooth(aes(x = x, y = pred), method = "lm")

reg_por_partes <- function(x, y, pq = NULL) {
  # Ponto de quebra
  if(max(pq) > 25 | min(pq) < 0) stop("pq (ponto de quebra, deve estar entre 0-25")
  if(is.null(pq)) stop("pq é um argumento obrigátorio")
  pq <- cut(x, unique(c(0,pq,25)) , include.lowest = TRUE, labels = c(pq,25))
  
  df <- data.frame(x = x, y = y, r = pq)
  
  pq <- as.integer(levels(df$r))[-length(levels(df$r))]
  
  pqs <- sapply(pq, function(z) {
    ifelse(df$x <= z, 0, df$x - z) 
  })  
  colnames(pqs) <- paste0("x",2:length(levels(df$r)))  
  
  df <- cbind(df, pqs)
  
  fit <- lm(y ~ ., data = df[,-which(names(df) == "r")])
  
  df$pred <- predict(fit)  
  list(df = df, model = fit)
}


dfpartes <- reg_por_partes(df$x, df$y, pq = c(10,20))

ggplot(dfsegmentado$df, aes(x = x, y = pred)) + geom_point(aes(y = y))  + geom_line(color = "blue")


dfsegmentado2 <- reg_segmentada(df$x, df$y, pq = c(5,13,20))
ggplot(dfsegmentado2$df, aes(x = x, y = pred)) + geom_point(aes(y = y))  + geom_line(color = "blue")

# Função soma de quadrados do resíduo
sqres <- function(obs, pred) {
  sum((obs - pred)^2)
}


reg_segmentada2 <- function(df, mc = TRUE) {
  # Determina o melhor ponto de corte (um único)
  corte <- lapply(df$x, function(x) {
    g1 <- df$y[df$x <= x]
    g2 <- df$y[df$x > x]
    
    data.frame(
      sqres = sqres(mean(g1),g1) + sqres(mean(g2), g2),
      corte = x
    )
  })
  cutoff <- do.call(rbind, corte)
  
  melhor_corte <- cutoff[which.min(cutoff$sqres),]$corte  
  
 reg_segmentada(df$x, df$y, pq = c(0, melhor_corte, 25), mc = mc)
}

dfc1 <- reg_segmentada2(df)

ggplot(dfc1, aes(x = x, y = y, group = r, color = r)) + geom_point() + 
  geom_smooth(aes(x = x, y = pred), method = "lm")

dfc2 <- reg_segmentada2(df,mc = FALSE)

ggplot(dfc2, aes(x = x, y = y, group = r, color = r)) + geom_point() + 
  geom_smooth(aes(x = x, y = pred), method = "lm")

# Ponto de corte somen
reg_por_partes2 <- function(df) {
  # Determina o melhor ponto de corte (um único)
  corte <- lapply(df$x, function(x) {
    g1 <- df$y[df$x <= x]
    g2 <- df$y[df$x > x]
    
    data.frame(
      sqres = sqres(mean(g1),g1) + sqres(mean(g2), g2),
      corte = x
    )
  })
  cutoff <- do.call(rbind, corte)
  
  melhor_corte <- cutoff[which.min(cutoff$sqres),]$corte  
  
  reg_por_partes(df$x, df$y, pq = melhor_corte)
}


dfc3 <- reg_por_partes2(df)

ggplot(dfc3$df, aes(x = x, y = pred)) + geom_point(aes(y = y)) +
  geom_line(color = "blue")
