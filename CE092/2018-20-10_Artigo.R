# Código para análise  do artigo "Confiabilidade e Precisão na Estimação de Médias"
# Julio M.Singer

library(ggplot2)
library(dplyr)
library(latex2exp)

df <- read.table("data/artigo.txt", dec = ",", sep = "",  quote = "", header = TRUE)

summary(df)


# Média e Desvio-padrão por (Local)
dfm <- df %>%
  group_by(Local) %>%
    summarise(Mean = mean(Concentra),
              Sd = sd(Concentra))

qplot(x = Local,
     y = Mean,
     geom = "line",
     data = dfm)

lm(Mean ~ Local, data = dfm)


ggplot(df, aes(x = Local, y = Concentra, group = Local)) + geom_boxplot()




# Precisão --------------------------------------------------------------------------------------------------------
p <- 0.8
m <- 1

precisao <- function(p, m ) {
  (1 - sqrt(p + ((1 - p) / m))) * 100
}


dfp <- expand.grid(m = c(2, 3, 4, 5),
                   p = seq(0,1,0.1))

dfp$raic <- precisao(dfp$p, dfp$m)
# dfp$raic <- precisao()


ggplot(dfp, aes(x = p, y = raic, color = as.factor(m))) + geom_line() +
  labs(x = TeX("$\\rho$"), y = "Redução na amplitude do internvalo de confiança (%)")



# Confiabilidade --------------------------------------------------------------------------------------------------
confiabilidade <- function(p, m) {
  # exp((log(m) - log(1 + (m - 1) * p))) * 100
   m / (1 + (m - 1) * p) * 100
}

dfp$conf <- confiabilidade(p = dfp$p, m = dfp$m)


ggplot(dfp, aes(x = p, y = conf, color = as.factor(m))) + geom_line() +
  labs(x = TeX("$\\rho$"), y = "Aumento no grau de confiabilidade (%)")



# Planejamento ótimo ----------------------------------------------------------------------------------------------
plan_otim <- function(p, N) {
  ((8 * (1 - p)^2) * ((N - 1) * p + 1)) / (N - 1)^2
}


dfpo <- expand.grid(N = c(30, 60 , 100),
                    p = seq(0,1,0.01))

dfpo$pest <- plan_otim(dfpo$p, dfpo$N)


ggplot(dfpo, aes(x = p, y = pest, color = as.factor(N))) + geom_line() +
  labs(x = TeX("$\\rho$"), y = "Variância sob planejamento ótimo") +
  scale_color_discrete(name = "N")



# Planejamento N/4 ------------------------------------------------------------------------------------------------
plan_N4 <- function(p, N) {
  ((2 * (1 - p)^2) * (1 + 3 * p)^2) / (3 * (N - 4))
}

dfpo2 <- expand.grid(N = c(60,100),
                     p = seq(0,1,0.01))

dfpo2$pest <- plan_otim(dfpo2$p, dfpo2$N)
dfpo2$pestn4 <- plan_N4(dfpo2$p, dfpo2$N)
dfpo3 <- reshape2::melt(dfpo2, id.vars = c("N", "p"))

ggplot(dfpo3, aes(x = p, y = value, color = as.factor(variable))) + geom_line() +
  facet_wrap(~N) +
  labs(x = TeX("$\\rho$"), y = TeX("Variância ($\\hat{\\rho}$)")) +
  scale_color_discrete(name = "",
                       breaks = c("pest", "pestn4"),
                       labels = c("Planejamento ótimo", "n=N/4"))



# Ajuste ----------------------------------------------------------------------------------------------------------
library(lme4)
fit <- lmer(Concentra ~ 1|Local, data = df %>% mutate(Local = as.factor(Local)))

summary(fit)

ea <- as.vector(ranef(fit))
n <- 5
m <- 4
N <- n * m
mu <- mean(df$Concentra)

# QMA = \sum_{i=1}^n \frac{m(\bar{y_{i.}} - \bar{y_{ii}})^2}{n-1}
QMA <- sum(m * (tapply(df$Concentra, df$Local, mean) - mu)^2) / (n - 1)
QMA

# QMR = \sum_{j=1}^n \frac{(Y_{ij} - \bar{Y}_{i.})^2}{N-n}
QMR <- sum((df$Concentra - rep(tapply(df$Concentra, df$Local, mean), each = 4))^2) / (N - n)
QMR

# \rho = \frac{QMA - QMR}{QMA +  (m-1)QMR}
rho <- (QMA - QMR) / (QMA + (m -1) * QMR)
rho

# Delineamento otimo
rhoO <- (N - 3) / (3 * (N - 1))
rhoO

## Delineamento N/4
# (13) V(\hat{\rho}|n = N/4) = \frac{2(1-\rho)^2(1+3\rho)^2}{3(N-4)}
varp <- (2 * (1 - rho)^2 * (1 + 3 * rho)^2) / (3 * (N - 4))
varp


# Valor necessario para atingir uma confiabilidade rho_m
# m = (\rho_m (1 - \rho)) / (\rho(1-\rho_m))
m <- round((0.9 * (1 - rho)) / (rho * (1 - 0.9)))
m

precisao(rho, m)



# Removendo Local 2 -----------------------------------------------------------------------------------------------
n <- 4
m <- 4
N <- n * m
mu <- mean(df2$Concentra)

# QMA = \sum_{i=1}^n \frac{m(\bar{y_{i.}} - \bar{y_{ii}})^2}{n-1}
QMA <- sum(m * (tapply(df2$Concentra, df2$Local, mean) - mu)^2) / (n - 1)
QMA

# QMR = \sum_{j=1}^n \frac{(Y_{ij} - \bar{Y}_{i.})^2}{N-n}
QMR <- sum((df2$Concentra - rep(tapply(df2$Concentra, df2$Local, mean), each = 4))^2) / (N - n)
QMR

# \rho = \frac{QMA - QMR}{QMA +  (m-1)QMR}
rho <- (QMA - QMR) / (QMA + (m -1) * QMR)
rho

## Delineamento N/4
# (13) V(\hat{\rho}|n = N/4) = \frac{2(1-\rho)^2(1+3\rho)^2}{3(N-4)}
varp <- (2 * (1 - rho)^2 * (1 + 3 * rho)^2) / (3 * (N - 4))
varp


# Valor necessario para atingir uma confiabilidade rho_m
# m = (\rho_m (1 - \rho)) / (\rho(1-\rho_m))
m <- ceiling((0.9 * (1 - rho)) / (rho * (1 - 0.9)))
m

precisao(rho, m)


