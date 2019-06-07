options(warnings=-1)
# Bibliotecas ---------------------------------------------------------------
suppressMessages(library(tidyquant,  quietly =  TRUE, verbose = FALSE))
suppressMessages(library(tseries))
suppressMessages(library(ggplot2))
suppressMessages(library(dplyr))
library(stochvol)

Sys.setlocale("LC_TIME", "C")

# Funções -------------------------------------------------------------------
# Retorno Contínuo
rc <- function(r) {
    rc <- c(0)
    for (i in 2:length(r)) {
        rc <- c(rc, log(r[i] / r[i - 1]))
    }
    rc
}

# Volatilidade EWMA
vewma <- function(r, lambda) {
    # n observações
    n <- length(r) + 1

    sig.s <- rep(0, n)

    for (i in 2:n) {
        sig.s[i] <- sig.s[i-1]*lambda + (r[i-1]^2)*(1 - lambda)
    }
    sqrt(sig.s[-1])
}

# Volatilidade Estocastica Simples
# ?ar
x <- logret(df$Price)
x[1:10]
df$rc[1:10]

par(mfrow = c(2, 1))
plot(x, type = "l",  xlim =  c(1, 100))
plot(df$rc, type = "l",  xlim = c(1, 100))

res <- svsample(x,  priormu = c(0, 1))
summary(res, showlatent = FALSE)

x <- latent(res)
x <- apply(x, 2, mean)
df$ve <- c(0, exp(x/2)) * -1

# Entrada dos Dados ---------------------------------------------------------
df <- read.csv("PETR4.csv")
head(df)

# Transforma Data para as.Date
df$Date <- as.Date(df$Date, format =  "%B %d, %Y")

# Seleciona colunas: Date, preço (fechamento), abertura, máxima e miníma
df <- df[, c(1:5)]

# Orderna por Data
df <- df[order(df$Date), ]

df$tempo <- lubridate::year(df$Date)
df$tempo <- as.factor(df$tempo)

# Log-Retorno
df$rc <- rc(r = df$Price)

vs <- df %>%
    group_by(tempo) %>%
    summarise(vs = sd(rc))

df <- merge(x = df,
            y = vs,
            by =  "tempo"
            )
# head(df)
df$vs <- df$vs * -1




# Parte Gráfica -------------------------------------------------------------
# Preço
qplot(x = Date,
      y = Price,
      geom = "line",
      data = df) +
    theme_bw()

# Log-Retorno
qplot(x = Date,
      y = rc,
      geom = "line",
      data =  df) +
    theme_bw()

# Log-Retorno  + Volatilidade Simples (Janela) 7, 30 e 90
ggplot(df, aes(x = Date)) +
    geom_line(aes(y = rc)) +
    geom_line(aes(y = vs * qnorm(0.95)), colour =  "blue") +
    geom_line(aes(y = ve * qnorm(0.95)),  colour =  "red") +
    theme_bw()

# VaR simples
sum(df$rc < df$vs[1] * qnorm(0.95)) / nrow(df)

# VaR GARCH
sum(df$rc < df$ve * qnorm(0.95), na.rm = TRUE) / nrow(df)



# # Acrescenta Retorno & Volatilidade -----------------------------------------
# # df$rc <- rc(r = df$Price)
# df$vs <- sd(df$Price)
#
# mg0 <- garch(df$Price, order = c(1, 1), trace = FALSE,  grad = "numerical")
#
# df$vg <- predict(mg0)[, 1]
#
# # Parte Gráfica -------------------------------------------------------------
# # Preço
# qplot(x = Date,
#       y = Price,
#       geom = "line",
#       data = df)
#
# # Log-Retorno
# # qplot(x = Date,
# #       y = rc,
# #       geom = "line",
# #       data =  df)
#
# # Log-Retorno  + Volatilidade Simples (Janela) 7, 30 e 90
# ggplot(df, aes(x = Date)) +
#     geom_line(aes(y = Price)) +
#     geom_line(aes(y = vs * qnorm(0.90)), colour =  "blue") +
#     geom_line(aes(y = vg * qnorm(0.90)),  colour =  "red")
#
# # VaR simples
# sum(df$Price >  df$vs[1] * qnorm(0.90)) / nrow(df)
#
# # VaR GARCH
# sum(df$Price >  df$vg * qnorm(0.90), na.rm = TRUE) / nrow(df)
