##
## Eq (3)
## \rho_m = \frac{m rho}{[1 + (m-1)*rho]}
rhom <- Vectorize(function(rho, m) exp(log(m) + log(rho) - log(1+(m-1)*rho)))
##
## Eq 4
## m = \frac{\rho_m(1-\rho)}{[\rho (1 - \rho_m)]}
m <- Vectorize(function(rho, rhom) exp(log(rhom) + log(1-rho) - log(rho) - log(1-rhom)))

## Tabelas 1 e 2
## Redução
reduAmp <- function(rho, m) 1 - sqrt(rho + ((1-rho)/m))
grauConf <- function(rho, m) exp(log(m) - log(1+(m-1)*rho))
## Tabela 1:
round(100*reduAmp(rho=c(0.05, 0.10, 0.25, 0.50, 0.90), m=3))
round(100*(grauConf(rho=c(0.05, 0.10, 0.25, 0.50, 0.90), m=3) - 1))

## Tabela 2:
round(100*reduAmp(rho=0.50, m=c(2,3,4,5)))
round(100*(grauConf(rho=0.50, m=c(2,3,4,5)) - 1))

## Eq (6) $Var[\hat{\rho}] = \frac{2(1-\rho)^2[n+(N-n)\rho^2])}{N(N-n)(n-1)}
Var.rho <- function(rho, N, n) (2*((1-rho)^2)*(n+(N-n)*rho)^2)/(N*(N-n)*(n-1))
## Eq (7) $n_0 = \frac{N[(N-1)\rho + 2]}{(N+1)+(N-1)\rho}$
n0 <- function(rho, N) (N*((N-1)*rho + 2))/((N+1)+(N-1)*rho)
## Eq (8) $Var[\hat{\rho}|n=n_0] = \frac{8(1-\rho)^2[(N-1)\rho+1])}{(N-1)^2}
Var.rho.n0 <- function(rho, N) (8*((1-rho)^2)*((N-1)*rho+1))/((N-1)^2)

## Figura 1
rho.seq <- seq(0, 1, length=101)
Nvals <- c(30, 60, 100)

V1 <- sapply(Nvals, function(x) Var.rho.n0(rho=rho.seq, N=x))

# jpeg("Figura3")
par(mgp=c(2.5, 1.2, 0))
matplot(x=rho.seq, y=V1, type="l", col=1, lty=1:3, 
        xlab=expression(rho), ylab=expression(plain(Var)(hat(rho))))
legend("topright", paste("N = ", N.vals, sep=""), lty=1:3)
#dev.off()

## Eq 9
rho.maxVar <- function(N) (N-3)/(3*(N-1))

## Eq 10 $Var[\hat{\rho}|n=N/4] = \frac{2(1-\rho)(1+3\rho)^2}{3(N-4)}
Var.rho.m4 <- function(rho, N) (2*((1-rho)^2)*(1+3*rho)^2)/(3*(N-4))

## Figura 2
rho.seq <- seq(0, 1, length=101)
#Nvals <- c(60, 100)

Var60 <- cbind(Vn0 =  Var.rho.n0(rho=rho.seq, N=60), Vm4 = Var.rho.m4(rho=rho.seq, N=60))
Var100 <- cbind(Vn0 = Var.rho.n0(rho=rho.seq, N=100), Vm4 = Var.rho.m4(rho=rho.seq, N=100))

n0.60 <- n0(rho=rho.seq, N=60)
n0.100 <- n0(rho=rho.seq, N=100)

## grafico do base
#matplot(x=rho.seq, y=Var60, type="l", col=1, lty=1:2, 
#        xlab=expression(rho), ylab="Variância")
#legend("topright", c("Planejamento Ótimo", "n = N/4"), lty=1:2)
## adicionar os n0

Var60e100 <- merge(data.frame(N=60, rho=rho.seq, Var60, n0=n0.60),
				   data.frame(N=100, rho=rho.seq, Var100, n0=n0.60), all=T)

require(lattice)
#jpeg("Figura2")
xyplot(Vm4 + Vn0 ~ rho|as.factor(N), data=Var60e100, lty=1:2, col=1, type="l",
       xlab=expression(rho), ylab=expression(plain(Var)(hat(rho))), 
       key=list(text=list(c("Planejamento Ótimo", "n=N/4")),
                lines=list(type=c("l", "l"), lty=1:2),
                columns = 2
                )
      )
## dev.off()

## adicionar os n0

## Figura 3
rho.seq <- seq(0.05, 1, length=101)
VarRel <- cbind(
           VarRel20 = Var.rho.m4(rho=rho.seq, N=20)/Var.rho.n0(rho=rho.seq, N=20), 
           VarRel40 = Var.rho.m4(rho=rho.seq, N=40)/Var.rho.n0(rho=rho.seq, N=40), 
           VarRel60 = Var.rho.m4(rho=rho.seq, N=60)/Var.rho.n0(rho=rho.seq, N=60), 
           VarRel100 = Var.rho.m4(rho=rho.seq, N=100)/Var.rho.n0(rho=rho.seq, N=100)
          )
# jpeg("Figura3")
matplot(x=rho.seq, y=VarRel, type="l", col=gray((0:3)*2/10), lty=1:4, 
        xlab=expression(rho), ylab=expression(Rel(rho)))
legend("topright", paste("N = ", c(20, 40, 60, 100), sep=""), col=gray((0:3)*2/10), lty=1:4)
# dev.off()

##
##
##

singer <- read.table("singer.dat", dec=",", header=TRUE)
singer
singer$Local <- as.factor(singer$Local)
summary(singer)

resumo <- function(x) c(media=mean(x), var=var(x), sd=sd(x), CV=100*sd(x)/mean(x))
with(singer, tapply(Concentra, Local, resumo))
with(singer, tapply(Concentra, Local, function(x) c(mean(x), var(x))))
(singer.res <- aggregate(Concentra~Local, FUN=resumo, data=singer))
class(singer.res)
str(singer.res)
names(singer.res)
with(as.data.frame(singer.res$Concentra), plot(media, var))

var(singer.res$Concentra[,1])
mean(singer.res$Concentra[,2])

require(MASS)
with(singer, boxcox(Concentra ~Local))

##
mod1 <- lm(Concentra ~ 1, data=singer)
summary(mod1)
coef(mod1)
##
mod2 <- lm(Concentra ~ Local-1, data=singer)
summary(mod2)
mean(coef(mod2))

coef(mod2)
coef(mod2) - mean(coef(mod2))
##
require(lme4)
mod3 <- lmer(Concentra ~ 1|Local, data=singer)
summary(mod3)
ranef(mod3)
coef(mod3)$Local - fixef(mod3)
##

