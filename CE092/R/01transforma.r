##
## CE-092
##

## Preambulo: estimando médias

## Modelo 1
## X \sim N(\mu, \sigma^2)
## Y = \log(X)
## Y \sim LN(\mu, sigma^2)
## ou seja, \log(Y) = \beta_0 + \epsilon
## com \mu = \beta_0

set.seed(010203)
## Simulando dados
x <- rnorm(20, m=3, sd=0.25)
y <- exp(x)


plot(y ~ x)

## via lm
lmx <- lm(x ~ 1)
summary(lmx)
coef(lmx)
summary(lmx)$sigma
logLik(lmx) - sum(log(y))
## via log-normal
## verossimilhança
sum(dlnorm(y, m=coef(lmx), sd=summary(lmx)$sigma, log=TRUE))
## utilizando o estimados não viezado da variância
sum(dlnorm(y, m=coef(lmx), sd=sqrt(summary(lmx)$sigma^2*(20-1)/20), log=TRUE))

## Ainda um outro modelo possível
## Modelo 2
## Y \sim N(\mu, \sigma^2)
## log(\mu) = beta_0
## ou seja, Y = exp(\beta_0) + \epsilon

glmy <- glm(y ~ 1, family = gaussian(link="log"))
summary(glmy)
logLik(glmy)


## Escrevendo suas proprias funções
## Modelo1

## Verossimilhança usando a normal nos dados transformandos e o Jacobiano
ll1 <- function(par, dados){
    ## par = \mu, \log(\sigma)
    x <- log(dados)
    ll <- sum(dnorm(x, m=par[1], sd=exp(par[2]), log=TRUE)) - sum(x)
    return(-ll)
}
mod1 <- optim(c(2, log(0.1)), fn=ll1, dados=y)
c(mod1$par[1], exp(mod1$par[2]))
mod1$value

## alternativa equivalente usando a log-normal
ll1.a <- function(par, dados){
    ## par = \mu, \log(\sigma)
    ll <- sum(dlnorm(dados, m=par[1], sd=exp(par[2]), log=TRUE))
    return(-ll)
}
mod1.a <- optim(c(2, log(0.1)), fn=ll1.a, dados=y)
c(mod1.a$par[1], exp(mod1.a$par[2]))
mod1.a$value ## mesmo valor do anterior

## Modelo 2:
ll2 <- function(par, dados){
    ## par = \beta_0, \log(\sigma)
    ll <- sum(dnorm(dados, m=exp(par[1]), sd=exp(par[2]), log=TRUE))
    return(-ll)
}
mod2 <- optim(c(2, log(0.1)), fn=ll2, dados=y)
mod2$par
exp(mod2$par)
mod2$value  ## este tem valor diferent pq é outro modelo!


## Exemplo 1: comparar ajustes de regressões:
##  I  - com transformação da variável resposta
##      t(Y) = \beta_0 + \beta_1 x + \epsilon
##  II - com transformação na função de ligação
##      Y \sim N(\mu, \sigma^2) ; t(\mu) = \beta_0 + \beta_1 x

x <- 0:20/10

## Simulando dados de I
beta0 <- 2
beta1 <- 0.8
sigma <- 0.2
set.seed(92)
Y1 <- round(exp(rnorm(x, m=beta0 + beta1 * x, sd=sigma)), dig=1)
plot(Y1 ~ x)

## Simulando dados de II
beta0 <- 2
beta1 <- 0.8
sigma <- 0.2
set.seed(92)
Y2 <- round(rnorm(x, m=exp(beta0 + beta1 * x), sd=sigma), dig=1)
plot(Y2 ~ x)

df01 <- data.frame(x=x, Y1=Y1, Y2=Y2)
df01

write.table(df01, file="data01.txt", row.names=FALSE, sep="\t", quote=FALSE)

## Simulando (outros) dados de II
beta0 <- 0.5
beta1 <- 1
sigma <- 0.6
set.seed(92)
Y2 <- round(rnorm(x, m=exp(beta0 + beta1 * x), sd=sigma), dig=1)
par(mfrow=c(1,2))
plot(Y1 ~ x)
plot(Y2 ~ x)
par(mfrow=c(1,1))

df02 <- data.frame(x=x, Y1=Y1, Y2=Y2)
df02

write.table(df02, file="data02.txt", row.names=FALSE, sep="\t", quote=FALSE)


##
rm(list=ls())

(df <- read.table("df02.txt", head=T))
n <- nrow(df01)
par(mfrow=c(1,2))
with(df, plot(Y1 ~ x))
with(df, plot(Y2 ~ x))
par(mfrow=c(1,1))

### Modelos para Y1
##  Modelo linear
m1.1 <- lm(Y1 ~ x, data=df)
logLik(m1.1)
## a quantidade retornada por logLik corresponde aos seguintes cálculos
predict(m1.1)
## o seguinte não dá exatamente certo porque o estimador retornado de sigma não é de MV
sum(dnorm(df$Y1, m=predict(m1.1), sd=summary(m1.1)$sigma, log=TRUE))
p <- length(coef(m1.1))
## Mas usando o estimador de MV...
(sigma1.1 <- sqrt(summary(m1.1)$sigma^2 * (n-p)/n))
## obtem-se da forma a seguir o mesmo resultado retornado por logLik()
sum(dnorm(df$Y1, m=predict(m1.1), sd=sigma1.1, log=TRUE))

## Mas será que a escala da resposta é adequada?
with(df, MASS::boxcox(Y1 ~ x))
##  Modelo com transformação da resposta (log)
m1.2 <- lm(log(Y1) ~ x, data=df)
## verossimilhança na escala transformada
logLik(m1.2)
## o Jacobiano ...
(0 - 1)*sum(log(df$Y1))
## e a verossimilhança na escala dos dados originais
logLik(m1.2) + (0 - 1)*sum(log(df$Y1))
p <- length(coef(m1.2))
## Que corresponde ao cálculo utilizando a distribuição log-normal para as respostas
(sigma1.2 <- sqrt(summary(m1.2)$sigma^2 * (n-p)/n))
sum(dlnorm(df$Y1, m=predict(m1.2), sd=sigma1.2, log=TRUE))


### Modelos para Y2
##  Modelo linear
m2.1 <- lm(Y2 ~ x, data=df02)
## a verossimilhança retornada por logLik()
logLik(m2.1)
##e o cálculo correspondente usando a densidade da normal que leva ao mesmo resultado
p <- length(coef(m2.1))
(sigma2.1 <- sqrt(summary(m2.1)$sigma^2 * (n-p)/n))
sum(dnorm(df02$Y2, m=predict(m2.1), sd=sigma2.1, log=TRUE))

##  Modelo com transformação da resposta (log)
bc2 <- with(df02, MASS::boxcox(Y2 ~ x))
names(bc2)
(lam2 <- with(bc2, x[which.max(y)]))
abline(v=lam2, col=2)
abline(v=0.5, col=2)


## Por aqui ser SQRT
m2.2 <- lm(sqrt(Y2) ~ x, data=df02)
## verossimilhança na escala transformada:
logLik(m2.2)
## e novamente, apenas por ilustração o cálculo correspondente:
(sigma2.2 <- sqrt(summary(m2.2)$sigma^2 * (n-p)/n))
sum(dnorm(sqrt(df02$Y2), m=predict(m2.2), sd=sigma2.2, log=TRUE))
## E agora na escala original da resposta
## O Jacobiano:
## - n * log 2 - sum(1/2 log y) ??? \sum log(y^{-\frac{1}{2}}{2})
(0.5 - 1) * sum(log(df02$Y2))
## E a verossimilhança na escala original dos dados
logLik(m2.2) + (0.5 - 1)*sum(log(df02$Y2))


## Gráficos das Regressões ajustadas
ndf <- data.frame(x = with(df, seq(min(x), max(x), l=100)))

ndf$Y1.1 <- predict(m1.1, newdata=ndf)
## Predição na escala original (naïve)
ndf$Y1.2 <- exp(predict(m1.2, newdata=ndf))
## Predição na escala original
ndf$Y1.2c <- exp(predict(m1.2, newdata=ndf)+ 0.5*summary(m1.2)$sigma^2)

ndf$Y2.1 <- predict(m2.1, newdata=ndf)
## Predição na escala original (naïve):
ndf$Y2.2 <- predict(m2.2, newdata=ndf)^2
## Predição na escala original
##ndf$Y2.2c <- ...

par(mfrow=c(1,2))
with(df, plot(Y1 ~ x))
with(ndf, lines(Y1.1 ~ x, col=1))
with(ndf, lines(Y1.2 ~ x, col=2))
with(ndf, lines(Y1.2c ~ x, col=4))
with(df, plot(Y2 ~ x))
with(ndf, lines(Y2.1 ~ x, col=1))
with(ndf, lines(Y2.2 ~ x, col=2))
#with(ndf, lines(Y2.2c ~ x, col=2))
par(mfrow=c(1,1))

ndf <- data.frame(x = with(df, seq(min(x), max(x), l=100)))
ndf$Y1.1 <- predict(m1.1, newdata=ndf, interval="pred")
ndf$Y1.2 <- exp(predict(m1.2, newdata=ndf, interval="pred"))
ndf$Y2.1 <- predict(m2.1, newdata=ndf, interval="pred")
ndf$Y2.2 <- predict(m2.2, newdata=ndf, interval="pred")^2


par(mfrow=c(1,2))
with(df, plot(Y1 ~ x))
matlines(ndf$x, ndf$Y1.1, col=1, lty=c(1,2,2))
matlines(ndf$x, ndf$Y1.2, col=2, lty=c(1,2,2))
with(ndf, lines(Y1.2 ~ x, col=2))
with(df, plot(Y2 ~ x))
matlines(ndf$x, ndf$Y2.1, col=1, lty=c(1,2,2))
matlines(ndf$x, ndf$Y2.2, col=2, lty=c(1,2,2))
par(mfrow=c(1,1))
