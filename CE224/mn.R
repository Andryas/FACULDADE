# Métodos Númericos

# 4 tipos de erros utilizados como criterio de parada
# xts: solução verdadeira
# xns: solução númerica

# 1: erro real
# 2: tolerância em f(x)
# 3: tolerância na solução
# 4: erro relativo
erro <- function(type = 1, xts, xns, fx, b, a) {
  type <- as.character(type)
  switch(type,
    '1' = return(xts - xns[length(xns)]),                   ## Erro real
    '2' = return(abs(fx(xts) - fx(xns[length(xns)]))),      ## Erro tolerância na função
    '3' = return(abs((b - a) / 2)),                         ## Erro tolerância na solução
    '4' = if (length(xns) <= 1) {
      return(TRUE)                                            ## Erro relativo estimado
      } else {                                 
      return(abs((xns[length(xns)] - xns[length(xns) - 1]) / xns[length(xns) - 1]))
    },
    print("type: 1-4")
  )
}

# Método da bisseção
met.bissecao <- function(fx, a, b, xts, tol, max_iter, type) {
  TOL <- as.integer(nchar(gsub("0\\.", "", sprintf("%.3f", tol)))) + 1
  fa <- fx(a) - xts
  fb <- fx(b) - xts
  
  if (fa * fb > 0) stop("Solução não está no intervalo.")
  
  xns <- (a + b) / 2
  solucao <- xns
  limites <- NULL
  
  i <- 1
  while (erro(type, xts, solucao, fx, b, a) > tol & i <= max_iter) {
    test <- (fx(a) - xts) * (fx(xns) - xts)
    
    if (test < 0) {
      solucao <- c(solucao, (a + xns) / 2)
      b <- xns
    } 
    
    if (test > 0) {
      solucao <- c(solucao, (b + xns) / 2)
      a <- xns
    }

    xns <- solucao[length(solucao)]
    limites <- rbind(limites, data.frame(a = a, b = b))  
    i <- i + 1

    if (sum(duplicated(round(limites, TOL))) >= 2) {
      limites <- limites[-nrow(limites):-(nrow(limites) - 1), ]
      cat("Convergiu!")
      break
    }
  }

  # Animação
  
  if (i >= max_iter) cat("Número máximo de iteração atingido!")
  
  list(solucao = solucao[length(solucao)], 
       limites = limites,
       solucoes = solucao
       )
}

# Método Regula falsi
met.regula.falsi <- function(fx, a, b, xts, tol, max_iter, type) {
  
  TOL <- as.integer(nchar(gsub("0\\.", "", sprintf("%.3f", tol)))) + 1
  
  xns <-  (a * (fx(b) - xts) - b * (fx(a) - xts)) / ((fx(b) - xts) - (fx(a) - xts))
  solucao <- xns
  limites <- NULL
  
  i <- 1
  while (erro(type, xts, solucao, fx, b, a) > tol & i <= max_iter) {
    test <- (fx(a) - xts) * (fx(xns) - xts)
    
    if (test < 0) {
      solucao <- c(solucao, (a * (fx(xns) - xts) - xns * (fx(a) - xts)) / 
                     ((fx(xns) - xts) - (fx(a) - xts)))
      b <- xns
    } 
    
    if (test > 0) {
      solucao <- c(solucao, (xns * (fx(b) - xts) - b * (fx(xns) - xts)) / 
                     ((fx(b) - xts) - (fx(xns) - xts)))
      a <- xns
    }
    
    xns <- solucao[length(solucao)]
    limites <- rbind(limites, data.frame(a = a, b = b))  
    i <- i + 1
    
    if (sum(duplicated(round(limites, TOL))) >= 2) {
      limites <- limites[-nrow(limites):-(nrow(limites) - 1), ]
      cat("Covergiu!")
      break
    }
  }
  
  # Animação
  
  if (i >= max_iter) cat("Número máximo de iteração atingido!")
  
  list(solucao = solucao[length(solucao)], 
       limites = limites,
       solucoes = solucao
  )
  
}

# Método de Newton
met.newton <- function(fx, fxdx, xts, tol, max_iter, type) {
  if (type == 3)
    stop("Para este método somente os tipos de erro 1,2 e 4 estão disponíveis")
  TOL <- as.integer(nchar(gsub("0\\.", "", sprintf("%.3f", tol)))) + 1
  xns <- x1
  i <- 1
  while (erro(type, xts, xns, fx, b = NULL, a = NULL) > tol & i <= max_iter) {
    xns <- c(xns, xns[length(xns)] - (fx(xns[length(xns)]) - xts)/fxdx(xns[length(xns)]))
    i <- i + 1
    if (sum(duplicated(round(xns, TOL))) >= 2) {
      xns <- xns[-(length(xns) - 2):-length(xns)]
      break
    }
  }
  
  if (i >= max_iter) cat("Número máximo de iteração atingido!")
  
  return(xns)
  
}

# Método Gradiente Descendente
met.grad.desc <- function(fx, x1, alpha, tol, max_iter) {
  if (type == 3)
    stop("Para este método somente os tipos de erro 1,2 e 4 estão disponíveis")
  TOL <- as.integer(nchar(gsub("0\\.", "", sprintf("%.3f", tol)))) + 1
  xns <- x1
  i <- 1
  while (erro(type, xts, xns, fx, b = NULL, a = NULL) > tol & i <= max_iter) {
    xns <- c(xns, xns[length(xns)] + alpha * (fx(xns[length(xns)]) - xts))
    i <- i + 1
    if (sum(duplicated(round(xns, TOL))) >= 2) {
      xns <- xns[-(length(xns) - 2):-length(xns)]
      break
    }
  }
  
  if (i >= max_iter) cat("Número máximo de iteração atingido!")
  
  return(xns)
}
