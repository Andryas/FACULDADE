##
## Introdução aos efeitos aleatórios
##

## Vamos considerar (sem perda de generalidade) o modelo
## Y = f(x) + \epsilon

## O objetivo é descrever a relação entre Y e x de várias formas que sejam flexíveis
## Para isto é possível:
## i) Usar diferentes definições para f(x)
## Isto foi visto indo desde o modelo de média constante Y = \mu + \epsilon
## passando por regressões lineares simples, polinomial, splines,
## e outros métodos de suavização (paramétricos e não paramétricos)
##
## ii) Manter uma estrutura de média simples
## (constante ou uma regressão simples, por exemplo) 
## e utilizar "efeitos aleatórios" (variáveis não observadas, latentes)
## para descrever relações de forma flexível.
## A idéia é um modelo do tipo
## Y = \mu + S + \epsilon
## onde S é um vetor aleatório
## Os modelos de efeitos aleatórios consistem em diferentes
## definições/especificações para este termo

##
## Exemplo introdutório para motivação:
##   Simulação de um modelo de efeito aleatório
##   para ilustrar que é possível, com estes,
##   descrever formas suaves na relação de Y e x

## Definindo valores de x em um grid fino (para predição)
xf <- seq(0,100, by=0.1)
## definindo (aleatoriamente) valores de x onde teremos dados
IND <- sort(sample(1:length(xf), 100))
x <- xf[IND]

##
## simulando dados por "um certo modelo" de efeitos aleatórios 
## mais adiante detalharemos no Exemplo 1 este modelo
## simulando (S) no grid fino
simS <- geoR:::grf(grid=cbind(xf, 1), cov.pars=c(1, 5), kappa=2.5, RF=FALSE)$data
## extraindo os "dados" simulados
simD <- simS[IND]
## adicionando a média (5) e os \epsilon (ruído)
simDD <- 5 + simD + rnorm(length(simD), sd=0.2)

## visualização do efeito aleatório (puro) S
plot(simS ~ xf, type="n")
points(simD ~ x, pch=19, col=2, cex=0.5)
lines(simS ~ xf)

## Visualizando os dados Y (Y = \mu + S + \epsilon)
## e os components do modelo
plot(simDD ~ x, type="n", xlim=c(0, 100))
abline(h = 5, lty=2)
points(simDD ~ x, pch=19, cex=0.5)
lines(5 + simS ~ xf)
points(5 + simD ~ x, pch=19, col=2, cex=0.5)

## Outros métodos vistos antes que obtem uma f(x) suave
df <- data.frame(x=x, y=simDD)
ndf <- data.frame(x=xf)
## polinomial EXPERIMENTAR DIFERENTES GRAUS DO POLINÔMIO
regPol <- lm(y ~ poly(x, degree=8), data=df)
ndf$ypol <- predict(regPol, newdata=ndf)
with(ndf, lines(ypol ~ x, col=4))
## regressão por splines (naturais) EXPERIMENTAR DIFERENTES GRAUS DE LIBERDADE
regS <- lm(y ~ splines:::ns(x, df=8), data=df)
ndf$yns <- predict(regS, newdata=ndf)
with(ndf, lines(yns ~ x, col=3))
## polinômios locais ## EXPERIMENTAR DIFERENTES LARGURAS DE BANDA (f)
with(df, lines(lowess(y ~ x, f=1/4), col=2))

## Mas podemos ainda combinar as abordagens e colocar estrutura tanto em f(x)
## quanto dependência nos resíduos
## Por exemplo, invés de uma média constante podemos adicionar uma
## regressão linear simples ou algum modelo linear (fácil interpretação)
## Teríamos no caso mais simples:
## Y_i = \beta_0 + \beta_1 x_i + s_i + \epsilon_i 
## ou, vetorialmente
## Y = X\beta + S + \epsilon 

simS1 <- 5 + 0.1 * xf + simS
simD1 <- 5 + 0.1 * x + simD
simDD1 <- 0.1 * x + simDD  ## simDD já tinha somado 5

plot(simS1 ~ xf, type="n")
points(simDD1 ~ x, pch=19, cex=0.5)
abline(c(5,0.1), col=4)
lines(simS1 ~ xf, pch=19, col=2, cex=0.5)
points(simD1 ~ x, pch=19, cex=0.5, col=2)

## ou seja o dado é decomposto entre uma "tendência" determinística
## dada pela reta neste caso
## e uma flutuação estoástica ao redor desta tendência

##
## Modelo de efeitos aleatórios:
## Formulação geral para o caso Gaussiano
## Y = f(x) + S + \epsilon
## E vamos descrever f(x) e S por modelos lineares:
## Y = X \beta + Z \gamma + \epsilon
## \gamma \sim N(0, \tau2 R)
## \epsilon \sim N(0, \sigma2 I)
## \gamma e \epsilon são assumidos independentes
## Decorre então que 
## E[Y|\gamma] = X \beta + Z \gamma
## V[Y|\gamma] = \sigma2 I
## Logo,  [Y|\gamma] \sim N(X \beta + Z \gamma, \sigma2 I)
## E[Y] = X \beta 
## V[Y] = Z V[\gamma] Z' + \sigma2 I = \tau2 Z R Z' + \sigma2 I =
##      = sigma2 ((\tau2/sigma2) Z R Z' + I) = sigma2 (\tau2.r Z R Z' + I) =
##      = \sigma2 V
## Logo, [Y] \sim N(X \beta, \sigma2 V)
## em que V = \tau2.r Z R Z' + I

## Decorre deste modelo que a distribuição dos efeitos
## aleatórios, condicional aos dados, pode ser obtida por:
## [\gamma|y] = \frac{[Y|\gamma][\gamma]}{[Y]}
## que neste caso é tb normal e
## (PredEF) E[\gamma|y] = \tau2.r * R Z' (\tau2.r R + I)^{-1} (y - mu) = 
##                      = \tau2.r * R Z' V^{-1} (y - mu) 
## (outra forma de obtenção é o teorema da condicional em distribuições normais
##  multivariadas)

## Exemplo 01:
##    Um efeito aleatório para cada observação (para cada valor de x)
##    Efeitos aleatórios (n) com estrutura de dependência entre eles
##    Neste caso:
##    R(nxn) (efeitos aleatórios correlacionados)
##    R_{ij} = Corr(\gamma_i, \gamma_j) = exp\{ - (d_{ij}/phi)^2\}
##    d_{ij} é a distância entre pontos (x's); no caso d_{ij} = |x_i - x_j|
##    Z = I(nxn)    
##
## Iremos gerar (simular) dados deste modelo
n <- length(x)
## Definindo parâmetros
mu <- 5
sig2 <- 0.5
tau2 <- 1
tau2.r <- tau2/sig2
phi <- 10
## Calculando a matriz de covariância de Y
D <- abs(outer(x, x, "-"))
R <- exp(-(D/phi)^2)
#R <- outer(x, x, FUN=function(x, y) exp(-(abs(x-y)/phi)^2))
# V = \tau2.r Z R Z' + I mas aqui Z = I então V = \tau2.r R + I
V <- tau2.r * R + diag(n)
## Simulando Y de uma normal multivariada
y <- drop(mu + sqrt(sig2) * crossprod(chol(V), rnorm(n)))
## (poderia usar alguma função pronta como MASS:::mvrnorm() ou mvtnorm:::rmvnorm()

## Visualização dos dados
plot(y~x)

## Predizendo e visualizando os efeitos aleatórios
## Como neste exemplo Z = I, (PredEF) se reduz a
## E[\gamma|y] = \tau2.r R V^{-1} (y - mu)
ref <- (tau2.r * R) %*% solve(V, (y-mu))
points(x, mu + ref, col=4, pch=19, cex=0.5)

## predição em uma malha fina
Dp <- abs(outer(xf, x, "-"))
Rp <-  exp(-(Dp/phi)^2)
refp <- (tau2.r * Rp) %*% solve(V, (y-mu))
lines(xf, mu + refp, col=4)
abline(h=mu, col=4)

##
## Estimação por máxima verossimilhança  
## ("naive") -- algorítmos não eficientes
##

## 1a opção (+ fácil e direta, menos eficiente)
ll0 <- function(par, xx, yy){
    ## par = (mu, tau2, phi, sigma2)
    mu <- par[1]
    tau2 <- exp(par[2])
    phi <- exp(par[3])
    sigma2 <- exp(par[4])
    tau2.r <- tau2/sigma2
    n <- length(yy)
    ##
    D <- abs(outer(xx, xx, "-"))
    V <- tau2.r * exp(-(D/phi)^2) + diag(n)
    loglik <- mvtnorm:::dmvnorm(yy, rep(mu,n), sigma2 * V, log=TRUE)
    ##print(c(mu, tau2, phi, sigma2))
    return(loglik)
}   

## 2a opção (um pouco menos eficiente)
ll1 <- function(par, xx, yy, caminho=FALSE){
    ## par = (mu, log(tau2), log(phi), log(sigma2))
    mu <- par[1]
    tau2 <- exp(par[2])
    phi <- exp(par[3])
    sigma2 <- exp(par[4])
    tau2.r <- tau2/sigma2
    n <- length(yy)
    ##
    D <- abs(outer(xx, xx, "-"))
    V <- tau2.r * exp(-(D/phi)^2) + diag(n)
    Vchol <- chol(V)
    loglik <- (-n/2) * (log(2*pi)+log(sigma2)) - sum(log(diag(Vchol))) -
        0.5*drop(crossprod(backsolve(Vchol, (yy-mu), transpose=T)))/sigma2
    if(caminho) print(c(mu, tau2, phi, sigma2))
    return(loglik)
}

## 3a opção (+ eficiente)
## concentrar varossimilhança, evitar cálculo de D, tau e sigma,  ... (TO DO)

##ini <- with(df, c(mean(y), 0.5*var(y), 10, 0.5*var(y)))
##optim(ini, ll, xx=x, yy=y, method="L-BFGS-B", control=list(fnscale=-1),
##      lower=c(Inf, 0, 0, 0))
ini <- with(df, c(mean(y), log(0.5*var(y)), log(10), log(0.5*var(y))))
(fit1 <- optim(ini, ll0, xx=x, yy=y, control=list(fnscale=-1)))
(fit1 <- optim(ini, ll1, xx=x, yy=y, control=list(fnscale=-1)))

##
## Visualizando agora com os valores estimados dos parâmetros
##
## Estimativas dos Parâmetros 
mu.est <- fit1$par[1]
tau2.est <- exp(fit1$par[2])
phi.est <- exp(fit1$par[3])
sig2.est <- exp(fit1$par[4])
tau2.r.est <- tau2.est/sig2.est
c(mu.est, tau2.est, phi.est, sig2.est)

### OBS: obtendo estimativas usando um "pacote" 
(ml <- geoR:::likfit(coords=cbind(x, 1), data=y, ini=c(0.5, 5),
                     nug=0.5, cov.model="gau", message=FALSE))
##

## Calculando a matriz de covariância (estimada) de Y
R.est <- exp(-(D/phi.est)^2)
#R <- outer(x, x, FUN=function(x, y) exp(-(abs(x-y)/phi)^2))
V.est <- tau2.r.est * R.est + diag(n)

## Visualização dos dados
##plot(y~x)

## Predizendo e visualizando os efeitos aleatórios (estimados)
## \hat{E}[\gamma|y] = \hat{\tau2.r} * R \hat{V}^{-1} (y - \hat{mu})
ref.est <- (tau2.r.est * R.est) %*% solve(V.est, (y-mu.est))
points(x, mu.est + ref.est, col=2, pch=19, cex=0.5)

## predição em uma malha fina
Rp.est <-  exp(-(Dp/phi.est)^2)
refp.est <- (tau2.r.est * Rp.est) %*% solve(V.est, (y-mu.est))
lines(xf, mu.est + refp.est, col=2, pch=19, cex=0.5)
abline(h=mu.est, col=2)
legend("topleft", c("verdadeiro","estimado"), col=c(4, 2), lty=1)

## Exemplo 2:
## Modelo de efeitos aleatórios para grupos de observações:
##    grupos: 1 efeito aleatório para cada grupo
##    Ex: 10 grupos dados pelos intervalos de x em [0, 10, 20, ..., 100]
##    Neste caso:
##    R(10x10) = I_{10} (efeitos aleatórios independentes)
##    Z (nx10)

## Vamos começar: (i) definindo os grupos
##               (ii) definindo a matriz Z que indica o grupo de cada observação

INDS.f <- function(K, x)
    ifelse(x > K[1] & x <= K[2], 1, 0)

cM <- cbind(seq(0, 90, by=10), seq(10, 100, by=10))
cM
Z <- apply(cM, 1, INDS.f, x=x)#matriz do modelo(pode ser obtida com model.matrix)
colSums(Z)                    # conta o número de obs em cada grupo
sum(colSums(Z))
df <- data.frame(y=y, Z)
names(df)

Zf <- apply(cM, 1, INDS.f, x=xf)
Zf[1,1] <- 1
dim(Zf)
colSums(Zf)
sum(colSums(Zf))
ndf <- data.frame(Zf)
names(ndf)

plot(y~x)

## Um "parêntesis" ...
## predizendo com modelo de efeitos fixos:
## O modelo é:
## y_{ij} = mu + \alpha_i + e_{ij} = \beta_i + e_{ij}
## \epsilon_{ij} \sim N(0, \sigma^2)
(betas.fix <- drop(solve(crossprod(Z), crossprod(Z,y))))
pred.fix <- Zf %*% betas.fix

## que fornece o mesmo que ....
(fix01 <- lm(y ~ . - 1, data=df))
fix01pred <- predict(fix01, newdata=ndf)

plot(y~x)
lines(pred.fix ~ xf)
## fazendo "mais bonitinho"
pred.fix[round(xf) %in% seq(0,100, by=10)] <- NA
plot(y~x)
lines(pred.fix ~ xf)

## ... voltando aos efeitos aleatórios
## O modelo geral neste caso se resume a:
## y_{ij} = mu + \alpha_i + e_{ij}
## \alpha_i \sim N(0, \tau^2)
## \epsilon_{ij} \sim N(0, \sigma^2)

## predizendo efeitos aleatórios (supondo parâmetros já conhecidos ou estimados)
## E[\gamma_i] = \tau2.r R Z' V^{-1} (y - X\beta)
## com V = \tau2.r Z R Z' + I 
## lembrando que neste caso R = I  e então
## com V = \tau2.r ZZ' + I 
V <- tau2.r * tcrossprod(Z) + diag(n)
## e E[\gamma_i] = \tau2.r Z' V^{-1} (y - mu) 
alphas.ref <- drop(tau2.r * crossprod(Z, solve(V, (y - mu))))

cbind(alphas.fix=coef(fix01) - 5, alphas.ref)
plot(coef(fix01) - 5, alphas.ref, asp=1); abline(0,1)

## Predição dos Y: \hat{E}[Y] = X\beta + Z \gamma
pred.ref <- mu + Zf %*% alphas.ref
pred.ref[round(xf) %in% seq(0,100, by=10)] <- NA

plot(y~x)
abline(h=mu, lty=3)
lines(pred.fix ~ xf)
lines(pred.ref ~ xf, col=2)

##
## Estimativas de parâmetros
##

## 1a opção (+ fácil e direta, menos eficiente)
xg <- cut(x, br=seq(0, 100, by=10), include.lowest=TRUE)
Zg <- model.matrix(~xg - 1)
Rg <- diag(10)
dim(Rg)
ZRZg <- tcrossprod(Zg)  ## lembrando que aqui R = I
dim(ZRZg)

llg <- function(par, ZRZ, yy){
    ## par = (mu, tau2, sigma2)
    mu <- par[1]
    tau2 <- exp(par[2])
    sigma2 <- exp(par[3])
    tau2.r <- tau2/sigma2
    n <- length(yy)
    ## 
    V <- tau2.r * ZRZ + diag(n)
    loglik <- mvtnorm:::dmvnorm(yy, rep(mu,n), sigma2 * V, log=TRUE)
    ##print(c(mu, tau2, sigma2))
    return(loglik)
}   
inig <- c(mean(y), rep(log(0.5*var(y)), 2))
(fitg <- optim(inig, llg, ZRZ=ZRZg, yy=y, control=list(fnscale=-1)))
mug.est <- fitg$par[1]
tau2g.est <- exp(fitg$par[2])
sig2g.est <- exp(fitg$par[3])
c(mug.est, tau2g.est, sig2g.est)

## predizendo efeitos aleatórios (usando parâmetros estimados)
## \hat{E}[\gamma_i] = \hat{\tau^2.r} Z' V^{-1} (y - \mu)
Vest <- (tau2g.est/sig2g.est) * ZRZg + diag(length(y))
(alphas.ref.est <- drop((tau2g.est/sig2g.est) * crossprod(Zg, solve(Vest, (y - mug.est)))))

cbind(alphas.fix=coef(fix01) - 5, alphas.ref.est)
plot(coef(fix01) - 5, alphas.ref.est, asp=1); abline(0,1)

## Predição dos Y: \hat{E}[Y] = X\hat{\beta} + Z \hat{\gamma}
pred.ref.est <- mug.est + Zf %*% alphas.ref.est
pred.ref.est[round(xf) %in% seq(0,100, by=10)] <- NA

plot(y~x)
## verdadeitos
abline(h=mu, lty=3)
lines(pred.fix ~ xf)
lines(pred.ref ~ xf, col=2)
## estimado
abline(h=mug.est, lty=3, col=4)
lines(pred.ref.est ~ xf, col=4)

## Usando funções de um pacote (lme4)
## montar uma variável indicadora de grupo e um novo data.frame
df1 <- data.frame(y=y, x=x, gr = cut(x, br=seq(0,100, by=10)))
ndf1 <- data.frame(x=xf, gr = cut(xf, include.lowest=TRUE, br=seq(0,100, by=10)))

(fit.gr <- lme4:::lmer(y ~ 1 + (1|gr), data=df1, REML=FALSE))
summary(fit.gr)
(ranef(fit.gr))

cbind(alphas.ref.est, unlist(ranef(fit.gr)))
plot(alphas.ref.est, unlist(ranef(fit.gr)), asp=1); abline(0,1)
