# Andryas Waurzenczak
library(tidyverse)

# ==================================================================================================
# 1. Calcular a média entre os três dados nas seguintes situações:
# ==================================================================================================

## I
## As observações são: 22, 25, 32
y <- c(22, 25, 32)

# Soma os valores de y dividindo pela quantidade de elementos em y.
sum(y) / length(y)

mean(y)

# Adotando uma variância igual a 9
L0 <- function(par, y, sd = 3) {
    sapply(par, function(x) sum(dnorm(y, mean = x, sd = sd, log = TRUE)))
}

ggplot(data.frame(x=seq(15, 35, by = 1)), aes(x)) + stat_function(fun = L0, args = list(y = y))

max1 <- optimize(L0, interval = c(10, 40), y = y, maximum = TRUE)
max1

ggplot(data.frame(x=seq(15, 35, by = 1)), aes(x)) +
    stat_function(fun = L0, args = list(y = y, sd = 3), aes(colour = "3")) +
    stat_function(fun = L0, args = list(y = y, sd = 3.25), aes(colour = "3.25")) +
    stat_function(fun = L0, args = list(y = y, sd = 3.5), aes(colour = "3.5")) +
    stat_function(fun = L0, args = list(y = y, sd = 3.75), aes(colour = "3.75")) +
    stat_function(fun = L0, args = list(y = y, sd = 4), aes(colour = "4")) +
    theme(legend.title = element_blank()) + geom_vline(xintercept = max1$maximum)

## II
## As observações são: <24, [23, 28] , >30
y2 <- c("<24", "[23-28]", ">30")

L1 <- function(par, y, sd = 3) {
  # Notação: <numero,  >numero ou [numero-numero]
  sapply(par, function(x) {
    sum(sapply(y, function(w) {
      # Identifica um padrão >numero (maior que)
      if (grepl(">[0-9]+", w)) {
        pnorm(as.numeric(str_extract(w, "[0-9]+")),
              mean = x, sd = sd, log.p = TRUE, lower.tail = FALSE)
        # Identifica um padrão <numero (menor que)
      } else if (grepl("<[0-9]+", w)) {
        pnorm(as.numeric(str_extract(w, "[0-9]+")),
              mean = x, sd = sd, lower.tail = TRUE, log.p = TRUE)
      } else {
        # Identifica um padrão contido em um intervalo
        log(diff(pnorm(c(as.numeric(str_extract_all(w,"\\(?[0-9,.]+\\)?")[[1]][1]),
                         as.numeric(str_extract_all(w,"\\(?[0-9,.]+\\)?")[[1]][2])),
                       mean = x, sd = sd)))
      }
    }))
  })
}

ggplot(data.frame(x = seq(15, 35, by = 1)), aes(x)) +
  stat_function(fun = L1, args = list(y = y2))

max2 <- optimize(L1, interval = c(10, 50), y = y2, maximum = TRUE)
max2

## III
## As observações são: >24, [23, 28] , <30
y3 <- c(">24", "[23-28]", "<30")

ggplot(data.frame(x = 15:35), aes(x)) +
    stat_function(fun = L1, args = list(y = y3))

max3 <- optimize(L1, interval = c(10, 50), y = y3, maximum = TRUE)
max3

## IV
## De todas as observações: pontuais e intervalares
y4 <- c(y,y2,y3)

L2 <- function(par, y, sd = 3) {
    # Notação: <numero,  >numero ou [numero-numero] para dados intervalares
    sapply(par, function(x) {
        sum(sapply(y, function(w) {
          # Somente número
            if (grepl("^[0-9]+", w)) {
                dnorm(as.numeric(w), mean = x, sd = sd, log = TRUE)
              # Identifica >numero (maior que)
            } else if (grepl(">[0-9]+", w)) {
                pnorm(as.numeric(str_extract(w, "[0-9]+")), mean = x,
                      sd = sd, log.p = TRUE, lower.tail = FALSE)
              # Identifica <numero (menor que)
            } else if (grepl("<[0-9]+", w)) {
                pnorm(as.numeric(str_extract(w, "[0-9]+")), mean = x,
                      sd = sd, lower.tail = TRUE, log.p = TRUE)
              # Identifica intervalo fechado [numero,numero]
            } else {
              ob1 <- pnorm(as.numeric(str_extract_all(w,"\\(?[0-9,.]+\\)?")[[1]][1]), mean = x,
                           sd = sd)
              ob2 <- pnorm(as.numeric(str_extract_all(w,"\\(?[0-9,.]+\\)?")[[1]][2]), mean = x,
                           sd = sd)
              log(ob2 - ob1)
            }
        }))
    })
}

ggplot(data.frame(x = 15:35), aes(x)) +
    stat_function(fun = L2, args = list(y = y4))

max4 <- optimize(L2, interval = c(10, 50), y = y4, maximum = TRUE)
max4

## VI
## Generalizar o arquivo de comandos anterior para incluir a estimação da variância

LN <- function(par, y) {
  #  par: vetor de parâmetros
  mu <- par[1]
  sigma <- par[2]
  # Notação: <numero,  >numero ou [numero-numero] para dados intervalares
    sum(sapply(y, function(w) {
      # Somente número
      if (grepl("^[0-9]+", w)) {
        dnorm(as.numeric(w), mean = mu, sd = sigma, log = TRUE)
        # Identifica >numero (maior que)
      } else if (grepl(">[0-9]+", w)) {
        pnorm(as.numeric(str_extract(w, "[0-9]+")),
              mean = mu, sd = sigma, log.p = TRUE, lower.tail = FALSE)
        # Identifica <numero (menor que)
      } else if (grepl("<[0-9]+", w)) {
        pnorm(as.numeric(str_extract(w, "[0-9]+")),
              mean = mu, sd = sigma, lower.tail = TRUE, log.p = TRUE)
        # Identifica intervalo fechado [numero,numero]
      } else {
        ob1 <- pnorm(as.numeric(str_extract_all(w,"\\(?[0-9,.]+\\)?")[[1]][1]), mean = mu, sd = sigma)
        ob2 <- pnorm(as.numeric(str_extract_all(w,"\\(?[0-9,.]+\\)?")[[1]][2]), mean = mu, sd = sigma)
        log(ob2 - ob1)
      }
    }))
}

max5 <- optim(par = c(20,1), LN, y = y4[1:5], control = list(fnscale=-1))
max5

# ==================================================================================================
# 2 De forma semelhante ao problema anterior, deseja-se calcular a média os casos a seguir,
# sabendo-se agora que se referem a dados de contagem.
# ==================================================================================================

## I
## As observações são: 2, 0, 5, 3, 1, 3, 1, 2
y <- c(2,0,5,3,1,3,1,2)

mean(y)

## II
## As observações são: > 0, 0, [3-7], >= 1, 1, 3, < 3, ⇐ 4
y2 <- c(">0", "0", "[3-7]", ">=1", 1, 3, "<3", "<4")

LP <- function(par, y) {
  lambda <- par
  # Notação: <numero,  >numero ou [numero-numero] para dados intervalares

  sum(sapply(y, function(w){
    type <- str_c(str_extract_all(w,"[><=\\[\\]()]",simplify = TRUE),collapse = "")
    type <- if (length(type) == 0) "num" else type
    switch (type,
            '>' = ppois(as.numeric(str_extract(w, "[0-9]+")) + 1, lambda = lambda,
                        log.p = TRUE, lower.tail = FALSE),
            '<' = ppois(as.numeric(str_extract(w, "[0-9]+")) - 1, lambda = lambda,
                        lower.tail = TRUE, log.p = TRUE),
            '>=' = ppois(as.numeric(str_extract(w, "[0-9]+")), lambda = lambda,
                        log.p = TRUE, lower.tail = FALSE),
            '<=' = ppois(as.numeric(str_extract(w, "[0-9]+")), lambda = lambda,
                        log.p = TRUE, lower.tail = FALSE),
            '(]' = {
              ob1 <- ppois(as.numeric(str_extract_all(w,"[0-9]+")[[1]][1]) + 1,
                           lambda = lambda)
              ob2 <- ppois(as.numeric(str_extract_all(w,"[0-9]+")[[1]][2]),
                           lambda = lambda)
              log(ob2 - ob1)
            },
            '[)' = {
              ob1 <- ppois(as.numeric(str_extract_all(w,"[0-9]+")[[1]][1]),
                           lambda = lambda)
              ob2 <- ppois(as.numeric(str_extract_all(w,"[0-9]+")[[1]][2]) - 1,
                           lambda = lambda)
              log(ob2 - ob1)
            },
            '()' = {
              ob1 <- ppois(as.numeric(str_extract_all(w,"[0-9]+")[[1]][1]) + 1,
                           lambda = lambda)
              ob2 <- ppois(as.numeric(str_extract_all(w,"[0-9]+")[[1]][2]) - 1,
                           lambda = lambda)
              log(ob2 - ob1)
            },
            '[]' = {
              ob1 <- ppois(as.numeric(str_extract_all(w,"[0-9]+")[[1]][1]),
                           lambda = lambda)
              ob2 <- ppois(as.numeric(str_extract_all(w,"[0-9]+")[[1]][2]),
                           lambda = lambda)
              log(ob2 - ob1)
            },
            'num' = dpois(as.numeric(str_extract(w, "[0-9]+")), lambda = lambda,log = TRUE)
     )
  }))
}

max <- optimize(LP, c(1,10),y = y2, maximum = TRUE)
max
