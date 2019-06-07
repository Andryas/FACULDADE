##
## Exercício 1
##
## Obter a média dos seguintes dados:
## a) 22, 25, 32
## b) <24,  [23-28] , >30
## c) 22, 25, 32, <24,  [23-28] , >30

## a) 22,25,32
ya <- c(22, 25, 32)
mean(ya)

## Mas podemos pensar diferente!!!
## i. Especificar um modelo
## ii. estimar (ajusta) o modelo
## iii. obter a média como uma característica deste modelo

## i. Vamos assumir uma distribuição normal (e vamos "inicialmente" fixar a variância em 9)
## Seja $Y \sim {\rm N}(\mu ; \sigma^2 = 3^2)$ e os dados:
## Obter a função de verosimilhança e a estimativa do parâmetro em cada caso:
## a função de verossimulhança


log(dnorm(0))
dnorm(0, log = TRUE)

lla <- function(par){
    l1 <- dnorm(22, mean=par, sd=3, log=TRUE)
    l2 <- dnorm(25, mean=par, sd=3, log=TRUE)
    l3 <- dnorm(32, mean=par, sd=3, log=TRUE)
    return(l1+l2+l3)
}
lla(23)
lla(24)
lla(25)
lla(26)
lla(27)
lla(28)
lla(29)
lla(30)
curve(lla, from=15, to=38)

## outra forma de escrever a verossimilhança
lla <- function(par, y){
    sapply(par, function(x) sum(dnorm(y, mean=x, sd=3, log=TRUE)))
}
## e ainda outra mais convniente para fazer gráficos etc...
?Vectorize

llav <- Vectorize(function(par, y){
    sum(dnorm(y, mean=par[1], sd=par[2], log=TRUE))
}, "par")


teste <- function(par,y) {
  sum(dnorm(y,mean=par[1],sd=par[2],log = TRUE))
}

ll <- optim(c(0.1,0.1),teste)
ll$par[1]
ll$par[2]


## conferindo...
sum(dnorm(ya, mean=23, sd=3, log=T))
lla(23, y=ya)
lla(23:30, y=ya)
llav(20, y=ya)
llav(20:28, y=ya)

## aq função de verossimilhança
curve(lla(x, y=ya), from=18, to=35)
## o máximo obtido analiticamente ...
mean(ya)
# ... coincide com o máximo obtido numericamente
(est.a <- optimize(lla, interval=c(10,50), y=ya, maximum=TRUE))
abline(v=est.a[[1]])
optim(25, lla, y=ya, control=list(fnscale=-1))

## E podemos desenhar a função verossimilhança de cada dado!!!
curve(lla(x, y=ya[1]), from=0, to=55)
curve(lla(x, y=ya[2]), from=0, to=55, add=TRUE)
curve(lla(x, y=ya[3]), from=0, to=55, add=TRUE)
curve(lla(x, y=40), from=0, to=55, add=TRUE)


optim(25, lla, y=c(ya,40), control=list(fnscale=-1))


dnorm(0)

pnorm(0) - pnorm(-0.01)

dnorm(ya[2], mean = 26.3333, sd = 3)
dnorm(ya[3], mean = 26.3333, sd = 3)

##
## OBS: será que o resultado seria diferente se tivessemos adotado (fixado)
##      outro valor para a variância?

## b) <24,  [23-28] , >30
llb <- Vectorize(function(par){
    l1 <- pnorm(24, mean=par, sd=3, log=TRUE)
    l2 <- log(diff(pnorm(c(23,28), mean=par, sd=3)))
    l3 <- pnorm(30, mean=par, sd=3, log=TRUE, lower=FALSE)
    return(l1+l2+l3)
}, "par")
llb(25)
curve(llb, from=15, to=42, col=2)

(est.b <- optimize(llb, interval=c(10,50), maximum=TRUE))
abline(v=est.b[[1]], col=2, lty=2)

curve(llb, from=15, to=42, col=2)
curve(lla(x, y=ya), from=15, to=42, add=TRUE)

## Definindo a função deviance
dev.f <- function(par, fun, max.fun, ...){
    -2*(fun(par, ...) - max.fun)
}

dev.f(25, lla, max.fun=est.a[[2]], y=ya)
dev.f(25, llb, max.fun=est.b[[2]])

curve(dev.f(x, fun=lla, max.fun=est.a[[2]], y=ya), from=15, to=42)
abline(v=est.a[[1]], lty=2)
curve(dev.f(x, fun=llb, max.fun=est.b[[2]]), from=15, to=42, add=TRUE, col=2)
abline(v=est.b[[1]], lty=2, col=2)


## c) >24,  [23-28] , <30

stringr::str_extract_all(w, "[0-9]+",simplify = TRUE)[2]

llc <- Vectorize(function(par){
    l1 <- pnorm(24, mean=par[1], sd=par[2], lower=F, log=TRUE)
    l2 <- log(diff(pnorm(c(23,28), mean=par[1], sd=par[2])))
    l3 <- pnorm(30, mean=par[1], sd=par[2], log=TRUE)
    return(l1+l2+l3)
}, "par")

optim(c(10,50),llc)

llc(23)
llc(26)
curve(llc, from=15, to=42, col=4)

(est.c <- optimize(llc, interval=c(10,50), maximum=TRUE))
abline(v=est.c[[1]], lty=2, col=4)

curve(llc, from=15, to=45, col=4, ylim=c(-15, 0))
curve(llb, from=15, to=45, col=2, add=TRUE)
curve(lla(x, y=ya), from=15, to=45, add=TRUE)

curve(dev.f(x, fun=lla, max.fun=est.a[[2]], y=ya), from=15, to=45)
abline(v=est.a[[1]], lty=2)
curve(dev.f(x, fun=llb, max.fun=est.b[[2]]), from=15, to=45, add=TRUE, col=2)
abline(v=est.b[[1]], lty=2, col=2)
curve(dev.f(x, fun=llc, max.fun=est.c[[2]]), from=15, to=45, add=TRUE, col=4)
abline(v=est.c[[1]], lty=2, col=4)


##
## d. juntando dados intervalares e pontuais
##

llint <- Vectorize(function(par, y.p, y.i){
    ## y.p é um vetor de dados pontuais
    ## i.i é uma matriz (n x 2) de dados intervalares
    if(missing(y.p)) ll.p <- 0
    else ll.p <-  sum(dnorm(y.p, mean=par, sd=3, log=TRUE))
    if(missing(y.i)) ll.i <- 0
    else ll.i <- sum(apply(y.i, 1, function(x) log(diff(pnorm(x, mean=par, sd=3)))))
    return(ll.i+ll.p)
}, "par")

(yb  <- cbind(c(-Inf, 23, 30), c(24, 28, Inf)))
(yc  <- cbind(c(24, 23, -Inf), c(Inf, 28, 30)))

## conferindo os resultados da nova função com as anteriores
lla(25, y=ya)
llint(25, y.p=ya)
llb(25)
llint(25, y.i=yb)
llc(25)
llint(25, y.i=yc)

curve(lla(x, y=ya), from=15, to=42)
curve(llint(x, y.p=ya), from=15, to=45, add=TRUE, col=3, lty=2)

curve(llb, from=15, to=42)
curve(llint(x, y.i=yb), from=15, to=45, col=3, add=TRUE, lty=2)

curve(llc, from=15, to=42, col=4)
curve(llint(x, y.i=yc), from=15, to=45, col=3, add=TRUE, lty=2)

optimize(lla, interval=c(10,50), y=ya, maximum=TRUE)
optimize(llint, interval=c(10,50), y.p=ya, maximum=TRUE)
optimize(llb, interval=c(10,50), maximum=TRUE)
optimize(llint, interval=c(10,50), y.i=yb, maximum=TRUE)
optimize(llc, interval=c(10,50), maximum=TRUE)
optimize(llint, interval=c(10,50), y.i=yc, maximum=TRUE)

curve(dev.f(x, fun=llint, max.fun=est.a[[2]], y.p=ya), from=15, to=45)
abline(v=est.a[[1]], lty=2)
curve(dev.f(x, fun=llint, max.fun=est.b[[2]], y.i=yb), from=15, to=45, add=TRUE, col=2)
abline(v=est.b[[1]], lty=2, col=2)
curve(dev.f(x, fun=llint, max.fun=est.c[[2]], y.i=yc), from=15, to=45, add=TRUE, col=4)
abline(v=est.c[[1]], lty=2, col=4)

## Pode-se calcular a varossimilhança de cada dado individualmente!
ya
yb
yb
llint(25, y.p=ya[1])
llint(25, y.p=ya[2])
llint(25, y.p=ya[3])
llint(25, y.i=yb[1,, drop=FALSE])
llint(25, y.i=yb[2,, drop=FALSE])
llint(25, y.i=yb[3,, drop=FALSE])
llint(25, y.i=yc[1,, drop=FALSE])
llint(25, y.i=yc[2,, drop=FALSE])
llint(25, y.i=yc[3,, drop=FALSE])

## ... e as respectivas funções de verossimilhança!
par(mfrow=c(1,3), mar=c(3,3,0,0))
## a) 22, 25, 32
curve(llint(x, y.p=ya[1]), from=0, to=55)
curve(llint(x, y.p=ya[2]), from=0, to=55, add=TRUE)
curve(llint(x, y.p=ya[3]), from=0, to=55, add=TRUE)

## b) <24,  [23-28] , >30
curve(llint(x, y.i=yb[1,, drop=FALSE]), from=0, to=55, col=2)
curve(llint(x, y.i=yb[2,, drop=FALSE]), from=0, to=55, add=TRUE, col=2)
curve(llint(x, y.i=yb[3,, drop=FALSE]), from=0, to=55, add=TRUE, col=2)

## c) >24,  [23-28] , <30
curve(llint(x, y.i=yc[1,, drop=FALSE]), from=0, to=55, col=4)
curve(llint(x, y.i=yc[2,, drop=FALSE]), from=0, to=55, add=TRUE, col=4)
curve(llint(x, y.i=yc[3,, drop=FALSE]), from=0, to=55, add=TRUE, col=4)


par(mfrow=c(1,1))
## a) 22, 25, 32
curve(llint(x, y.p=ya[1]), from=0, to=55, ylim=c(-25, 0))
curve(llint(x, y.p=ya[2]), from=0, to=55, add=TRUE)
curve(llint(x, y.p=ya[3]), from=0, to=55, add=TRUE)
## b) <24,  [23-28] , >30
curve(llint(x, y.i=yb[1,, drop=FALSE]), from=0, to=55, col=2, add=TRUE)
curve(llint(x, y.i=yb[2,, drop=FALSE]), from=0, to=55, add=TRUE, col=2)
curve(llint(x, y.i=yb[3,, drop=FALSE]), from=0, to=55, add=TRUE, col=2)
## c) >24,  [23-28] , <30
curve(llint(x, y.i=yc[1,, drop=FALSE]), from=0, to=55, col=4, add=TRUE)
curve(llint(x, y.i=yc[2,, drop=FALSE]), from=0, to=55, add=TRUE, col=4)
curve(llint(x, y.i=yc[3,, drop=FALSE]), from=0, to=55, add=TRUE, col=4)


##
## É possível juntar todos os dados (de a) b) e c))!
##
curve(llint(x, y.p=ya, y.i=rbind(yb, yc)), from=15, to=38)
(est.all <- optimize(llint, interval=c(0, 50), y.p=ya, y.i=rbind(yb, yc), maximum=TRUE))
abline(v=est.all[[1]], lty=2)

curve(dev.f(x, fun=llint, max.fun=est.all[[2]], y.p=ya, y.i=rbind(yb, yc)),
      from=15, to=38)
abline(v=est.all[[1]], lty=2)
