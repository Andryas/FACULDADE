##
##
##
df <- list()
(df$x <- sort(round(runif(25, 0, 20), dig=1)))
ndf <- data.frame(x = seq(0, 20, l=201))
err <- round(rnorm(25, m=0, sd=3), dig=1)
##
## Exemplo 1: modelo de uma média
##
df$y1 <- 50 + err
plot(y1 ~  x, data=df)

(fit1 <- lm(y1 ~ 1, data=df))
mean(df$y1)

abline(fit1)

ndf$y1 <- predict(fit1, newdata=ndf)
lines(y1 ~  x, data=ndf, type="l", col=2)

##
## Exemplo 2: modelo de duas médias
##
df$x10 <- ifelse(df$x >= 10, 1, 0)
ndf$x10 <- ifelse(ndf$x >= 10, 1, 0)
df <- transform(df, y2 = 50 - 5 * x10 + err)
plot(y2 ~  x, data=df)

(fit2 <- lm(y2 ~ x10, data=df))
aggregate(y2 ~ x10, data=df, mean)
sum(coef(fit2))

ndf$y2 <- predict(fit2, newdata=ndf)
lines(y2 ~ x, data=ndf, type="l", col=2)

## Obs 1: fazer para 3 ou mais médias

##
## Exemplo 3: regressão linear simples
##
df <- transform(df, y3 = 5 + 2 * df$x + err)
plot(y3 ~  x, data=df)

(fit3 <- lm(y3 ~ x, data=df))

abline(fit3)

ndf$y3 <- predict(fit3, newdata=ndf)
lines(y3 ~  x, data=ndf, type="l", col=2)


##
## Exemplo 4: regressões paralelas
##
df <- transform(df, y4 =  5 + 2 * x - 5*x10 + err)
plot(y4 ~  x, data=df)

(fit4 <- lm(y4 ~ x + x10, data=df))

ndf$y4 <- predict(fit4, newdata=ndf)
lines(y4 ~  x, data=ndf, type="l", col=2)

##
## Exemplo 5: regressão por partes
##
df <- transform(df, y5 = 5 + 2 * x - 10 * x10 + 2.5 * x * x10 + err)
plot(y5 ~  x, data=df)

(fit5 <- lm(y5 ~ x + x10 * I(x*x10), data=df))

ndf$y5 <- predict(fit5, newdata=ndf)
lines(y5 ~  x, data=ndf, type="l", col=2)

##
## Exemplo 6: regressão segmentada
##
## ... completar aqui


## Obs 2: e se o ponto de corte tivesse que ser estimado?
