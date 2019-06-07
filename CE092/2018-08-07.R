# Andryas Waurzenczak
library(tidyverse)


# ==================================================================================================
# 1. Utilizando este arquivo de dados, efetue as análises das regressões de Y1 vs x e Y2 vs x,
# cada uma delas com os modelos de regressão linear simples inicialmente e depois com:
# ==================================================================================================

## Baixa os dados
# download.file("www.leg.ufpr.br/lib/exe/fetch.php/disciplinas:ce092-2015-02:df02.txt",
#               destfile = "data/dados07-08.txt")

df <- read.table("data/dados07-08.txt", dec =  ".", header = TRUE)

gridExtra::grid.arrange(
  ggplot(df, aes(x = x, y = Y1)) + geom_point()  + stat_smooth(method = "lm"),
  ggplot(df, aes(x = x, y = Y2)) + geom_point()  + stat_smooth(method = "lm"),
  top = "Regressão Linear Simples",
  ncol = 2
  )

gridExtra::grid.arrange(
  ggplot(df, aes(x = x, y = log(Y1))) + geom_point()  + stat_smooth(method = "lm"),
  ggplot(df, aes(x = x, y = log(Y2))) + geom_point()  + stat_smooth(method = "lm"),
  top = "Regressão Linear Simples com transformação log",
  ncol = 2
  )

gridExtra::grid.arrange(
  ggplot(df, aes(x = x, y = sqrt(Y1))) + geom_point()  + stat_smooth(method = "lm"),
  ggplot(df, aes(x = x, y = sqrt(Y2))) + geom_point()  + stat_smooth(method = "lm"),
  top = "Regressão Linear Simples com transformação raiz quadrada",
  ncol = 2
  )

par(mfrow=c(1,2))
lambda1 <- MASS::boxcox(Y1 ~ x, data = df)
lambda2 <- MASS::boxcox(Y2 ~ x, data = df)

gridExtra::grid.arrange(
  ggplot(df, aes(x = x, y = Y1^(lambda1$x[which.max(lambda1$y)]))) + geom_point()  + stat_smooth(method = "lm"),
  ggplot(df, aes(x = x, y = Y2^(lambda2$x[which.max(lambda2$y)]))) + geom_point()  + stat_smooth(method = "lm"),
  top = "Regressão Linear Simples com transformação BOXCOX",
  ncol = 2
  )

gridExtra::grid.arrange(
  ggplot(df, aes(x = x, y = Y1)) + geom_point()  + stat_smooth(method = "glm",
                                                               method.args = list(family = Gamma)),
  ggplot(df, aes(x = x, y = Y2)) + geom_point()  + stat_smooth(method = "glm",
                                                               method.args = list(family = Gamma)),
  top = "Regressão Linear Simples com distribuição Gamma para a resposta",
  ncol = 2
  )


# ==================================================================================================
# 2. Ainda utilizando os mesmos dados, ajuste modelos:
# ==================================================================================================

## I
## de média constante,
m1y1 <- mean(df$Y1)
m1y1

m1y2 <- mean(df$Y2)
m1y2

gridExtra::grid.arrange(
  ggplot(df, aes(x = x, y = Y1)) + geom_point() + geom_hline(yintercept = m1y1),
  ggplot(df, aes(x = x, y = Y2)) + geom_point() + geom_hline(yintercept = m1y2),
  top = "Modelo de média constante",
  ncol = 2
  )



## II
## e médias constantes por partes/intervalos

# Função soma de quadrados do resíduo
sqres <- function(obs, pred) {
  sum((obs - pred)^2)
}

media_por_partes <- lapply(df$Y1, function(x) {
  g1 <- df$Y1[df$Y1 <= x]
  g2 <- df$Y1[df$Y1 > x]

  data.frame(
    sqres = sqres(mean(g1),g1) + sqres(mean(g2),g2),
    corte = x
  )
})

media_por_partes <- do.call(rbind, media_por_partes)
media_por_partes


corte <- media_por_partes[which.min(media_por_partes$sqres),]$corte
media_por_partes[media_por_partes$corte == corte, ]

df$grupo <- ifelse(df$Y1 <= corte, "g1", "g2")

meanData <- df %>%
  group_by(grupo) %>%
  summarise(mean = mean(Y1),
            min = min(x),
            max = max(x))


ggplot(df, aes(x = x, y = Y1, group = grupo, colour = grupo)) + geom_point() +
  geom_segment(data = meanData, aes(x = min, xend = max, y = mean, yend = mean))

ggplot(df, aes(x = x, y = Y1, group = x > 1.2, colour = x > 1.2)) + geom_point() +
  stat_smooth(method = "lm")


## III
## de regressão linear simples

m1 <- lm(Y1 ~ x, data = df[df$x > 1.2,])
m1
m2 <- lm(Y1 ~ x, data = df[df$x <= 1.2,])
m2
sum(residuals(m1)^2) + sum(residuals(m2)^2)

## IV
## de regressão segmentada. Defina (arbitrariamente) um "ponto de corte" em 1,2
lm_segment <- lapply(df$x, function(x) {
  g1 <- df[df$x <= x,]
  g2 <- df[df$x > x,]

  m1 <- if(nrow(g1) != 0) lm(Y1 ~ x, data = g1)
  m2 <- if(nrow(g2) != 0) lm(Y1 ~ x, data = g2)

  data.frame(
    sqres = sum(c(if(!is.null(m1)) sum(residuals(m1)^2),
                  if(!is.null(m2)) sum(residuals(m2)^2))),
    corte = x
  )

})

lm_segment <- do.call(rbind, lm_segment)
lm_segment

corte2 <- lm_segment[which.min(lm_segment$sqres),"corte"]
lm_segment[lm_segment$corte == corte2,]


df$grupo2 <- ifelse(df$x <= corte2, "g1", "g2")

ggplot(df, aes(x = x, y = Y1, group = grupo2, colour = grupo2)) + geom_point() +
  stat_smooth(method = "lm")


df %>%
  group_by(grupo2) %>%
  do(fit = lm(Y1 ~ x, data = .)) %>%
  broom::tidy(fit)
