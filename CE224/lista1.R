# ==============================================================================
# Introdução ao Cálculo Diferencial Integral para Estatísticos
# ==============================================================================
# Prof: Wagner Hugo Bonat - 2019
# Aluno: Andryas Waurzenczak


# ==============================================================================
# Funções, limites e continuidade
# ==============================================================================

# 1 ----------------------------------------------------------------------------
# Faça o gráfico das seguintes funções

## a) f(x) = \sqrt{x}
curve(sqrt(x), from = 0, to = 10)

## b) f(x) = log(x)
curve(log(x), from = 0, to = 10)

## c) f(x) = log_{10}(x)
curve(log(x, base = 10), from = 0, to = 10)
## obs: muda a escala no eixo-y

## d) f(x) = exp(x)
curve(exp(x), from = -10, to = 10)

## e) f(x) = \gamma{x}
curve(gamma(x), from = 0.5, to = 10)

## f) f(x) = 1/x
curve(1/x, from = -10, to = 10)

## g) f(x) = |x-1|+2
curve(abs(x - 1) + 2, from = -10, to = 10)

## h) f(x) = beta(x, 0.5)
curve(beta(x, 0.5), from = 0, to = 10)

## i) f(x) = (x - 1)^3
curve((x - 1)^3, from = -10, to = 10)

## j) f(x) = \frac{x + 1}{x}
curve((x + 1)/x, from = -10, to = 10)


# 2 ----------------------------------------------------------------------------
# Faça o gráfico das seguintes funções.
# Identifique o parâmetro que contra a função.

## a) f(x, \theta) = 2 (x log(\frac{x/\theta} - x + \theta))
f2a <- function(x, theta) {
  2 * (x * log(x/theta) - x + theta)
}

curve(f2a(x, theta = -1), from = -10, to = 0)
curve(f2a(x, theta = -5), add = TRUE, col = "red")
curve(f2a(x, theta = -10), add = TRUE, col = "blue")
legend("bottomright", legend = c("theta=-1", "theta=-5", "theta=-10"),
       col = c("black", "red", "blue"), lty = 1)

curve(f2a(x, theta = 1), from = 0, to = 10)
curve(f2a(x, theta = 5), add = TRUE, col = "red")
curve(f2a(x, theta = 10), add = TRUE, col = "blue")
legend("topleft", legend = c("theta=1", "theta=5", "theta=10"),
       col = c("black", "red", "blue"), lty = 1)

# O parâmetro que controla a fução é o \theta
# \theta < 0 e x < 0
# \theta > 0 e x > 0

## b) f(x, \theta) = \binom{100}{x} exp{xlog\frac{\theta}{1-\theta}+100log(1-\theta)}
f2b <- function(x, theta) {
  choose(100, x) * exp(x * log(theta/(1 - theta)) + 100 * log(1 - theta))
}

par(mfrow=c(2,2))
curve(f2b(x, theta = 0.1), to = 10)
title("theta=0.1")
curve(f2b(x, theta = 0.25), to = 10)
title("theta=0.25")
curve(f2b(x, theta = 0.5), to = 10)
title("theta=0.5")
curve(f2b(x, theta = 0.75), to = 10)
title("theta=0.75")
par(mfrow=c(1,1))

# O parâmetro que controla a função é o \theta
# 0 < \theta < 1

## c) f(x, \theta) = 2(\frac{x}{\theta} -log(x/\theta)-1)
f2c <- function(x, theta) {
  2 * (x/theta - log(x/theta) - 1)
}

curve(f2c(x, theta = -1), from = -10, to = 0, ylab = "y")
curve(f2c(x, theta = -5), add = TRUE, col = "red")
curve(f2c(x, theta = -10), add = TRUE, col = "blue")
legend("topright", legend = c("theta=-1", "theta=-5", "theta=-10"),
       col = c("black", "red", "blue"), lty = 1)

curve(f2c(x, theta = 1), from = 0, to = 10, ylab = "y")
curve(f2c(x, theta = 5), add = TRUE, col = "red")
curve(f2c(x, theta = 10), add = TRUE, col = "blue")
legend("topleft", legend = c("theta=1", "theta=5", "theta=10"),
       col = c("black", "red", "blue"), lty = 1)

# O parâmetro que controla a função é o \theta
# \theta > 0 e x > 0
# \theta < 0 e x < 0

## d) f(x, \theta, p) = 2 * (\frac{x^{2-p}}{(1-p)(2-p)} - \frac{x\theta^{1-p}}{1-p} + \frac{\theta^{2-p}}{2-p})
f2d <- function(x, theta, p) {
  2 * (x^(2 - p)/((1 - p) * (2 - p)) - x * theta^(1 - p)/(1 - p) + theta^(2 - p)/(2 - p))
}

par(mfrow=c(2,2))
curve(f2d(x, theta = 0, p = 0), from = -10, to = 10, ylab = "y")
curve(f2d(x, theta = 0.5, p = 0), add = TRUE, col = "red")
curve(f2d(x, theta = 2, p = 0), add = TRUE, col = "blue")
legend("top", legend = c("theta=0 e p=0", "theta=0.5 e p=0", "theta=2 e p=0"),
       col = c("black", "red", "blue"), lty = 1, cex = 0.75)

curve(f2d(x, theta = 0, p = 0.1), from = -10, to = 10, ylab = "y")
curve(f2d(x, theta = 0, p = 0.5), add = TRUE, col = "red")
curve(f2d(x, theta = 0, p = 0.75), add = TRUE, col = "blue")
legend("topleft", legend = c("theta=0 e p=0.1", "theta=0 e p=0.5", "theta=0 e p=0.75"),
       col = c("black", "red", "blue"), lty = 1, cex = 0.75)

curve(f2d(x, theta = -1, p = -1), from = -10, to = 10, ylab = "y")
curve(f2d(x, theta = -2, p = -1), add = TRUE, col = "red")
curve(f2d(x, theta = -5, p = -1), add = TRUE, col = "blue")
legend("topleft", legend = c("theta=-1 e p=-1", "theta=-2 e p=-1", "theta=-5 e p=-1"),
       col = c("black", "red", "blue"), lty = 1, cex = 0.75)

curve(f2d(x, theta = -1, p = -1), from = -10, to = 10, ylab = "y")
curve(f2d(x, theta = -1, p = -2), add = TRUE, col = "red")
curve(f2d(x, theta = -1, p = -5), add = TRUE, col = "blue")
legend("bottomright", legend = c("theta=-1 e p=-1", "theta=-1 e p=-2", "theta=-1 e p=-5"),
       col = c("black", "red", "blue"), lty = 1, cex = 0.75)
par(mfrow=c(1,1))

# O parâmetro que controla a função é o \theta e p

## e) f(x, \theta, p) = 2{1 - cos(x - \theta)}
f2e <- function(x, theta, p = NULL) {
  2 * (1 - cos(x - theta))
}

curve(f2e(x, theta = 1), from = -10, to = 10, ylab = "y")
curve(f2e(x, theta = 2), add = TRUE, col = "red")
curve(f2e(x, theta = 5), add = TRUE, col = "blue")
par(xpd=TRUE)
legend(-1.5,4.75, legend = c("theta=1", "theta=2", "theta=5"),
       col = c("black", "red", "blue"), lty = 1, cex = 0.75)

# O parâmetro que controla a função é o \theta
#  \theta \in R

# 3 ----------------------------------------------------------------------------
# Obtenha o limite e o esboçe o gráfico

## a) lim x -> 0 (\sqrt{x} + x)
f3a <- function(x) {
  sqrt(x) + x
}

f3a(x = 0)
curve(f3a, ylab = "fx")

## b) lim x -> 2 (\frac{x^2 + x}{x + 3})
f3b <- function(x) {
  (x^2 + x)/(x + 3)
}

f3b(x = 2)
curve(f3b, to = 4, ylab = "fx")

## c) lim x -> 2 (\frac{x^2 - 4}{x - 2})
f3c <- function(x) {
  (x^2 - 4)/(x - 2)
}

f3c(x = 2)
curve(f3c, to = 5, ylab = "fx")
points(2,4, cex = 2, col = "red")

## d) lim x -> -1 (\frac{x^2 - 1}{x + 1})
f3d <- function(x) {
  (x^2 - 1)/(x + 1)
}

f3d(x = -1)
curve(f3d, from = -3, ylab = "fx")
points(-1, -2, cex = 2, col = "red")

## e) lim x -> 0 (sin(x))

sin(x = 0)
curve(sin, from = -3, to = 3, ylab = "fx")

# 4 ----------------------------------------------------------------------------
## a) f(x) = \sqrt{x} em x = 0
curve(sqrt(x), to = 10, ylab = "fx") ## Continua

## b) f(x) = \frac{x^2 - 4}{x - 2} em x = 2
curve((x^2 - 4)/(x - 2), to = 2.5, ylab = "fx") ## Não é continua

## c) f(x) = {x se x < 1} {1/x se x > 1}
f4c <- function(x) {
  sapply(x, function(y) {
    if (y < 1) {
      y
    } else {
      1/y
    }
  })
}
curve(f4c, from = -3, to = 3) ## Continua


## d) f(x) = gamma(x) em x = 2
curve(gamma(x), from = -1, to = 3, ylab = "fx") ## Não é continua

## e) f(x) = \frac{|x - 2|}{x - 2} em x = 2
f4e <- function(x) {
  abs(x - 2)/(x - 2)
}
curve(f4e, from = -2, to = 3, ylab = "fx") ## Não é continua


# ==============================================================================
# Derivadas
# ==============================================================================

# 1 -----------------------------------------------------------------------------------------------------
# https://andryas.github.io/mcie/docs/derivadas.pdf


# 2 ----------------------------------------------------------------------------
# Determine a reta tangente ao gráfico de f(x) no ponto requisitado e esboce o
# gráfico de f(x) e da reta tangente

# Reta tangente: y - f(a) = f'(a)(x - a)

# Função para gerar a reta tangente
rt <- function(x, intercept, slope) intercept + slope * x

## a) f(x) = 1/x, x = 2

f2a <- deriv( ~ 1/x, "x")
x <- 2
slope <- as.numeric(attr(eval(f2a), "gradient"))
intercept <- as.numeric(attr(eval(f2a), "gradient") * (-x)  + 1/x)

curve(1/x, to = 2)
lines(0:2, rt(0:2, intercept, slope), col = "red")


## b) f(x) = x^3,  x = -3 e x = 3
f2b <- deriv( ~ x^3,  "x")

x <- -3
slope <- as.numeric(attr(eval(f2b), "gradient"))
intercept <- as.numeric(attr(eval(f2b), "gradient") * (-x)  + x^3)

curve(x^3, from = -4, to = 0, ylim = c(-70, 20))
lines(-4:-2, rt(-4:-2, intercept, slope), col = "red")

## c) f(x) = exp(x) no ponto  0
f2c <- deriv( ~ exp(x),  "x")

x <- 0
slope <- as.numeric(attr(eval(f2c), "gradient"))
intercept <- as.numeric(attr(eval(f2c), "gradient") * - x  + exp(x))

curve(exp(x), from = -4, to = 4, ylim = c(-10, 60))
lines(-2:4, rt(-4:4, intercept, slope), col = "red")


## d) f(x) = log(x) no ponto  2
f2d <- deriv( ~ log(x),  "x")

x <- 2
slope <- as.numeric(attr(eval(f2d), "gradient"))
intercept <- as.numeric(attr(eval(f2d), "gradient") * - x  + log(x))

curve(log(x), from = 0, to = 4, ylim = c(-4, 2))
lines(0:4, rt(0:4, intercept, slope), col = "red")

# 3 -----------------------------------------------------------------------------------------------------
# https://andryas.github.io/mcie/docs/derivadas.pdf

# 4 -----------------------------------------------------------------------------------------------------
# https://andryas.github.io/mcie/docs/derivadas.pdf

# 5 ----------------------------------------------------------------------------
# Aproximações usando a expansão de Taylor

## a) \sum_{i=1}^n (y_i - \mu)^2
f5a <- function(mu, y) sum((y - mu)^2)
f5adu <- function(mu, y) -2 * sum(y - mu)
f5adudu <- function(mu, y) 2 * length(y)
f5ataylor <- function(mu, mu0, y) {
  f5a(mu0, y) + (mu - mu0) * f5adu(mu0, y) +
    (mu - mu0)^2 * f5adudu(mu0, y) * 1/2
}
f5a <- Vectorize(f5a, "mu")
f5ataylor <- Vectorize(f5ataylor, "mu")

y <- c(2.09, -1.32, -0.2, 0.05, -0.07)
mu0 <- mean(y)

curve(f5a(x,y), from = -3 , to = 3, ylim = c(-10, 50), ylab = "fx")
curve(f5ataylor(x, mu0, y), add = TRUE, col = "blue")
legend("bottomleft", legend = c("fx", "taylor"),
       col = c("black", "blue"), lty = 1, cex = 0.75)
points(mean(y), f5a(mean(y), y), pch = 4)

## b) \sum_{i=1}^n 2*(y_i*log(\frac{y_i}{\mu})+\mu-y_i)
f5b <- function(mu, y) 2 * sum(y * log(y/mu) + mu - y)
f5bdu <- function(mu, y) 2 * sum(-y/mu + 1)
f5bdudu <- function(mu, y) 2 * sum((y) / mu^2)
f5btaylor <- function(mu, mu0, y) {
  f5b(mu0, y) + (mu - mu0) * f5bdu(mu0, y) +
    (mu - mu0)^2 * f5bdudu(mu0, y) * 1/2
}
f5b <- Vectorize(f5b, "mu")
f5btaylor <- Vectorize(f5btaylor, "mu")

y <- c(7, 4, 4, 6, 5)
mu0 <- mean(y)

curve(f5b(x,y) , to = 15, ylim = c(-10, 50), ylab = "fx")
curve(f5btaylor(x, mu0, y), add = TRUE, col = "blue")
legend("bottomleft", legend = c("fx", "taylor"),
       col = c("black", "blue"), lty = 1, cex = 0.75)
points(mean(y), f5b(mean(y), y), pch = 4)


## c) \sum_{i=1}^n 2 * (\frac{y_i}{\mu} - log(\frac{y_i}{\mu}) -1)
f5c <- function(mu, y) 2 * sum(y/mu - log(y/mu) - 1)
f5cdu <- function(mu, y) 2 * sum(1/mu - y/mu^2)
f5cdudu <- function(mu, y) 2 * sum(2 * y/mu^3 - 1/mu^2)
f5ctaylor <- function(mu, mu0, y) {
  f5c(mu0, y) + (mu - mu0) * f5cdu(mu0, y) +
    (mu - mu0)^2 * f5cdudu(mu0, y) * 1/2
}
f5c <- Vectorize(f5c, "mu")
f5ctaylor <- Vectorize(f5ctaylor, "mu")

y <- c(2.35, 0.16, 0.56, 1.05, 0.51)
mu0 <- mean(y)

curve(f5c(x,y) , ylab = "fx", to = 5)
curve(f5ctaylor(x, mu0, y), add = TRUE, col = "blue")
legend("topright", legend = c("fx", "taylor"),
       col = c("black", "blue"), lty = 1, cex = 0.75)
points(mean(y), f5c(mean(y), y), pch = 4)

## d) \sum_{i=1}^n 2 * (y_i * log(\frac{y_i}{\mu}) + (1 - y_i) * log(\frac{1-y_i}{1-\mu}))
# f5d <- function(mu, y) 2 * sum((y * log(y/mu) + (1 - y) * log((1 - y)/(1 - mu))))
# f5ddu <- function(mu, y) 2 * sum((1-y)/(1-mu)^2 - y/mu^2)
# f5ddudu <- function(mu, y) 2 * sum((2 * y) / mu^3 - (2 * (1 - y)) / (1 - mu^3))
# f5dtaylor <- function(mu, mu0, y) {
#   f5d(mu0, y) + (mu - mu0) * f5ddu(mu0, y) +
#     (mu - mu0)^2 * f5ddudu(mu0, y) * 1/2
# }
# f5d <- Vectorize(f5d, "mu")
# f5dtaylor <- Vectorize(f5dtaylor, "mu")
#
# y <- c(1, 0, 1, 1, 1)
# mu0 <- mean(y)
#
# curve(f5d(x,y), to = 1, ylab = "fx")
# curve(f5btaylor(x, mu0, y), add = TRUE, col = "blue")
# legend("bottomleft", legend = c("fx", "taylor"),
#        col = c("black", "blue"), lty = 1, cex = 0.75)


## e) \sum_{i=1}^n 2 * (y_i * log(\frac{y_i}{\mu}) + (m + y_i) * log(\frac{m + \mu}{m + y_i})
f5e <- function(mu, m, y) 2 * sum(y * log(y/mu) + (m + y) * log((m + mu)/(m + y)))
f5edu <- function(mu, m, y) 2 * sum((m*mu - m*y) / (mu * (m + mu)))
f5edudu <- function(mu, m, y) 2 * sum((2 * m * (-mu^2 + m * y + 2 * y * mu)) /
                                         (mu^2 * (m^2 + 2 * mu * m + mu^2)))
f5etaylor <- function(mu, mu0, y, m) {
  f5e(mu0, m, y) + (mu - mu0) * f5edu(mu0, m, y) + (mu - mu0)^2 * f5edudu(mu0, m, y) * 1/2
}
f5e <- Vectorize(f5e, "mu")
f5etaylor <- Vectorize(f5etaylor, "mu")

m <- 1
y <- c(7, 4, 4, 6, 5)
mu0 <- mean(y)

curve(f5e(x,m,y), to = 10, ylab = "fx")
curve(f5etaylor(x, mu0, y, m), add = TRUE, col = "blue")
legend("topright", legend = c("fx", "taylor"),
       col = c("black", "blue"), lty = 1, cex = 0.75)
points(mean(y), f5e(mean(y), m, y), pch = 4)

# 6 ----------------------------------------------------------------------------

## 5a) mu = 0.11,  minimo
## 5b) mu = 5.2,  minimo
## 5c) mu = 0.926, minimo
## 5d)
## 5e) mu = 5.2  minimo


# ==============================================================================
# Integrais
# ==============================================================================


# 1 ----------------------------------------------------------------------------
# https://andryas.github.io/mcie/docs/integrais.pdf


# 2 ----------------------------------------------------------------------------
# https://andryas.github.io/mcie/docs/integrais.pdf


# 3 ----------------------------------------------------------------------------
# Usando a soma de Riemann (numericamente) calcule as seguintes integrais:


soma_riemann <- function(n, a, b, fx, ...) {
  breaks <- seq(a, b, length.out = n)
  breaks2 <- dplyr::lead(breaks)
  a <- breaks[-length(breaks)]
  b <- breaks2[-length(breaks2)]

  out <- mapply(a = a, b = b, function(a, b) {
    deltai <- b - a
    ci <- (a + b) / 2
    fx(ci, ...) * deltai
  })

  return(sum(out))
}


# a) \int_{-150}^{150} exp^{-\frac{(x-5)^2}{2}} dx
f3a <- function(x) exp(-(x - 5)^2/2)

soma_riemann(100, -150, 150, fx = f3a)

# b) \int_0^100 exp^{-\frac{x}{5}} dx
f3b <- function(x) exp(-x/5)

soma_riemann(100, 0, 100, fx = f3b)

# c) \int_{0}^{100} exp^{-\frac{2(x-5)^2}{27x}} dx
f3c <- function(x) exp(-(2 * (x - 5)^2)/(27 * x))

soma_riemann(100, 0, 100, fx = f3c)

# d) \int_0^{100} exp^{-(|x-5|)/2} dx
f3d <- function(x) exp(-(abs(x - 5)) / 2)

soma_riemann(100, 0, 100, fx = f3d)

# e) \int_1^2 (\frac{1}{x} + \frac{1}{x^3})dx
f3e <- function(x) 1/x + 1/x^3

soma_riemann(100, 1, 2, fx = f3e)





# ==============================================================================
# Aplicações
# ==============================================================================

# 1 ----------------------------------------------------------------------------
fp <- function(mu, y) sum(abs(y - mu))
fp <- Vectorize(fp, "mu")

# a
y <- rnorm(100, mean = 50, sd = 9)

# b
curve(fp(x, y), from = 20, to = 80, ylab = "fx")

# c
points(mean(y), fp(mean(y), y), pch = 4)

# d
# Quando estamos interessados na mediana dos dados ou também quando se deseja maior
# robustez quanto a valores extremos.


# 2 ----------------------------------------------------------------------------

f2 <- function(x, y, b0, b1) sum(abs(y - (b0 + b1 * x)))

set.seed(123)

# a
b0 <- 10.3
b1 <- 5.7
x <- runif(100,0,10)
y <- b0 + b1 * x + rnorm(100, sd = 3)

# b
plot(y ~ x)

# c
grid <- expand.grid(b0 = -10:10,
                    b1 = 5:15)

out <- mapply(b0 = grid$b0, b1 = grid$b1, function(b0, b1, x, y) {
  out <- f2(x = x, y = y, b0 = b0, b1 = b1)
  data.frame(b0 = b0, b1 = b1, fx = out)
}, MoreArgs = list(x = x, y = y), SIMPLIFY = FALSE)
out <- do.call(rbind, out)

out[which.min(out$fx), ]
