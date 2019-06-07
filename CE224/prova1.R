library(ggplot2)

# Questão1 ---------------------------------------------------------------------
fun <- function(y, mu) sum((y - mu)^2/(y * mu^2))
y <- c(53, 12.9, 41.6, 32.8, 29, 22)
mu <- seq(15, 50, length.out = 1000)
tb <- data.frame(mu = mu, fu = sapply(mu, function(x) fun(y = y, mu = x)))
ggplot(tb, aes(x = mu, y = fu)) + geom_line() + theme_minimal()

# Questão2 ---------------------------------------------------------------------

fundx <- function(y, mu) - 2 * sum((y - mu)/(mu^3))
fun_tg <- function(mu, y, a) {
    fundx(y, a) * mu - fundx(y, a) * a + fun(y, a)
}

tb2 <- data.frame(mu = mu,
                  fu = sapply(mu, function(x) fun(y = y, mu = x)),
                  tg1 = sapply(mu, function(x) fun_tg(x, y, a = 23.75)),
                  tg2 = sapply(mu, function(x) fun_tg(x, y, a = 31.88)),
                  tg3 = sapply(mu, function(x) fun_tg(x, y, a = 39.40))
                  )

g1 <- ggplot(tb2, aes(x = mu, y = fu)) +
    geom_line() +
    geom_line(aes(y = tg1), color = "red") +
    theme_minimal()

g2 <- ggplot(tb2, aes(x = mu, y = fu)) +
    geom_line() +
    geom_line(aes(y = tg2), color = "blue") +
    theme_minimal()

g3 <-  ggplot(tb2, aes(x = mu, y = fu)) +
    geom_line() +
    geom_line(aes(y = tg3), color = "green") +
    theme_minimal()

g1
g2
g3

# Questão3 ---------------------------------------------------------------------

fundxdx <- function(y, mu) sum((6 * y - 4 * mu)/mu^4)

mu0 <- 31.88

funtaylor <- function(y, mu, mu0) {
    fun(y, mu0) + fundx(y, mu0) * (mu - mu0) + (1/2) * fundxdx(y, mu0) * (mu - mu0)^2
}

tb3 <- data.frame(fu = sapply(mu, function(x) fun(y, x)),
                  apx = funtaylor(y, mu, mu0),
                  mu = mu)

ggplot(tb3, aes(x = mu, y = fu)) + geom_line(color = "black") +
  geom_line(aes(x = mu, y = apx), color = "blue")
