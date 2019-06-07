# Calculo Diferencial e Integral

## Função para shiny
ce_cria_sliders <- function(fx) {
  # Identifica o número de parâmetros
  params <- unique(as.vector(str_extract_all(fx, "[A-z]+", simplify = TRUE)))
  params <- unique(gsub("[[:punct:]]+", "", params))
  
  # Remove funções do R (beta, gamma, log, etc...)
  params <- gsub(paste0("(beta)|(gamma)|(log)|(exp)|(sqrt)|(abs)|(cos)|(sin)"), "", params)
  if (length(which(params == "")) > 0) params <- params[-which(params == "")]
  
  # Cria slideinputers
  params <- params[-which(params == "x")]
  mysliders <- lapply(params, function(x) {
    shiny::sliderInput(paste0("input_",x), x, min = -10, max = 10, value = 1,
                       step = 0.1, width = "100%", animate = TRUE)
  })
  
  return(list(params = params, sliders = mysliders))
}

## Função usada na ce_calcula_fx
ce_gera_fx <- function(fx) {
  # Identifica o número de parâmetros
  params <- unique(as.vector(str_extract_all(fx, "[A-z]+", simplify = TRUE)))
  params <- unique(gsub("[[:punct:]]+", "", params))
  
  # Remove funções do R (beta, gamma, log, etc...)
  params <- gsub(paste0("beta|gamma|log|exp|sqrt|abs|cos|sin"), "", params)
  if (length(which(params == "")) > 0) params <- params[-which(params == "")]
  
  # mais que um parâmetro...
  params2 <- ifelse(length(params) > 1, paste(params, collapse = ", "), params)
  fnf <- paste0("function(", params2, ")")
  fxf <- as.function(x = , eval(parse(text = paste0(fnf, " ", fx))))
  
  fxf
}

## Gera função R apartir de texto e calcula derivadas e a exp.taylor
ce_calcula_fx <- function(fx) {
  fx <- ce_gera_fx(fx = fx) # Constroi a função em linguagem R
  fxdx <- Deriv::Deriv(fx, "x") # Calcula a primeira derivada
  fxdxdx <- Deriv::Deriv(fxdx, "x") # Calcula a segunda derivada
  fx_taylor <- function(x, x0, ...) {
    fx(x = x0, ...) +  (x - x0) * fxdx(x = x0, ...) + (x - x0)^2 * fxdxdx(x = x0, ...) * 1/2
  }
  
  # fx para gerar os gráficos
  fx_cond <- "function(x, ...) {\n \ty <- fx(x, ...) \n \tifelse(is.infinite(y), NA, y)\n}"
  fxg <- as.function(x = , eval(parse(text = fx_cond)))
  
  return(list(fx = fx, fxdx = fxdx, fxdxdx = fxdxdx, fx_taylor = fx_taylor, fxg = fxg))
}

## Gera o gráfico
ce_grafico_fx <- function(fx, from = -5, to = 5, tg = FALSE, exp_taylor = FALSE, 
                          ponto_a = NULL, args = NULL) {
  ## Layer 1
  p <- ggplot(data.frame(x = seq(from, to, 0.1)), aes(x, group = 1)) +
    stat_function(fun = fx$fxg, args = args, na.rm = TRUE) +
    # ylim(c(fromY, toY)) +
    xlim(c(from, to)) + 
    theme_minimal() + 
    labs(x = "x", y = "fx")
  
  ## Layer 2 - Adiciona reta tangente
  if (isTRUE(tg)) {
    pa <- ponto_a
    args2 <- append(list(x = pa), args)
    slope <- do.call(fx$fxdx, args2) ## Slope
    intercept <- slope * -pa + do.call(fx$fx, args2) ## Intercept
    
    xs <- c(ifelse(pa - 2 <= from, from, pa - 2), 
            ifelse(pa + 2 >= to, to, pa + 2))
    ys <- c(intercept + slope * (xs[1]), 
            intercept + slope * (xs[2]))
    
    p <- p +
      geom_segment(aes(x = xs[1], y = ys[1], xend = xs[2], yend = ys[2]), 
                   col = "red") +
      ## Zoom no ponto da reta tangente
      # ylim(min(ys), max(ys)) + 
      xlim(min(xs), max(xs))
    
  } 

  ## Layer 3 - Adiciona Serie de Taylor de segunda ordem  
  if (isTRUE(exp_taylor)) {
    pa <- ponto_a
    args3 <- append(list(x0 = pa), args)
    p <- p +
      stat_function(fun = fx$fx_taylor, args = args3,
                    n = 1000, color = "blue", linetype = 2)
  }
  
  return(p)
}

# Exemplos

fx <- "1/x"
fx <- ce_calcula_fx(fx)
ce_grafico_fx(fx)
ce_grafico_fx(fx, tg = TRUE, exp_taylor = TRUE, ponto_a = 1)


fx <- "2*(x*log(x/theta)-x+theta)+mu"
fx <- ce_calcula_fx(fx)
args <- list(theta = 2, mu = 1)
ce_grafico_fx(fx, from = 0, args = args)
ce_grafico_fx(fx, tg = TRUE, exp_taylor = TRUE, ponto_a = 2, args = args)


fx <- "2 * (1 - cos(x - theta))"
fx <- ce_calcula_fx(fx)
args <- list(theta = 2)
ce_grafico_fx(fx, args = args)
ce_grafico_fx(fx, tg = TRUE, exp_taylor = TRUE, ponto_a = 0.5, args = args)
ce_grafico_fx(fx, tg = TRUE, exp_taylor = TRUE, ponto_a = 1, args = args)
ce_grafico_fx(fx, tg = TRUE, exp_taylor = TRUE, ponto_a = 2, args = args)


fx <- "(x-1)^3"
fx <- ce_calcula_fx(fx)
ce_grafico_fx(fx)
ce_grafico_fx(fx, tg = TRUE, exp_taylor = TRUE, ponto_a = 0.5)
