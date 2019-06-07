# Andryas Waurzenczak
library(tidyverse)

# ==================================================================================================
# 1. Seja uma variável (resposta) Y e uma covariável X com valores dados conforme abaixo.
# Ajuste um modelo de regressão linear simples.
# ==================================================================================================

## I
## X 	0.4	1.2	1.8	1.9	2.0	6.8	7.6	8.3	8.7	9.3	10.7	11.3	13.0	13.4	14.2
## Y 	0.8	2.4	1.8	2.4	2.4	2.9	3.6	3.7	3.1	4.9	3.6	3.2	4.1	4.6	3.8

x <- c(0.4, 1.2, 1.8, 1.9, 2.0, 6.8, 7.6, 8.3, 8.7, 9.3, 10.7, 11.3, 13.0, 13.4, 14.2)
X <- cbind(1,x) # Acrescentando um vetor de 1 (Intercepto)

y <- c(0.8, 2.4, 1.8, 2.4, 2.4,	2.9, 3.6, 3.7, 3.1, 4.9, 3.6, 3.2, 4.1, 4.6, 3.8)

# (X'X)-1
XX <- solve(t(X) %*% X)

# X'y
Xy <- t(X) %*% y

XX %*% Xy

lm(y ~ x)

LG <- function(par, y, x) {
    #  par: vetor de parâmetros
    mu <- par[1] + par[2] * x
    sd <- par[3]
    # Notação: <numero,  >numero ou [numero-numero] para dados intervalares
    sum(sapply(y, function(w) {
        # Somente número
        if (grepl("^[0-9]+", w)) {
            dnorm(as.numeric(w), mean = mu, sd = sd, log = TRUE)
            # Identifica >numero (maior que)
        } else if (grepl(">[0-9]+", w)) {
            pnorm(as.numeric(str_extract_all(w,"\\(?[0-9,.]+\\)?",simplify = TRUE)[1]),
                  mean = mu, sd = sd, log.p = TRUE, lower.tail = FALSE)
            # Identifica <numero (menor que)
        } else if (grepl("<[0-9]+", w)) {
            pnorm(as.numeric(str_extract_all(w,"\\(?[0-9,.]+\\)?",simplify = TRUE)[1]),
                  mean = mu, sd = sd, lower.tail = TRUE, log.p = TRUE)
            # Identifica intervalo fechado [numero,numero]
        } else {
            ob1 <- pnorm(as.numeric(str_extract_all(w,"[0-9]+")[[1]][1]), mean = mu, sd = sd)

            ob2 <- pnorm(as.numeric(str_extract_all(w,"[0-9]+")[[1]][2]), mean = mu, sd = sd)

            # Se a chance de eu ver o que eu vi de um intervalo [numero-numero] for muito pequena
            # a contribuição para verossimilhança vai ser 0. Seria casos em que este intevalo
            # esteja a mais de 3 desvios para cima ou para a baixo da média e do desvio padrão
            # avaliado naquele instante.
            ifelse(ob1 == ob2, 0, log(ob2 - ob1))
        }
    })
    )
}

## II
## X 	0.4	1.2	1.8	1.9	2.0	6.8	7.6	8.3	8.7	9.3	10.7	11.3	13.0	13.4	14.2
## Y 	0.8	< 1.8	[1,5; 20]	2.4	> 2	2.9	3.6	< 4	[2,5; 4]	> 4.5	< 4	3.2	4.1	[4; 5]	< 5
y2 <- c(0.8, "<1.8", "[1.5-20]", "2.4", ">2", "2.9", "3.6", "<4", "[2.5-4]", ">4.5",
        "<4","3.2","4.1","[4-5]", "<5")

optim(par = c(3, 0.1, 0.1), LG, x = x, y = y2, control = list(fnscale=-1))
