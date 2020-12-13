#Bayesian Analisys using Stan by SEE 
#Cristhian Ortiz

#Load the necessary packages
library(ggplot2)
library(tidyr)
library(gridExtra)
library(rstanarm)
library(rstan)
library(bayesplot)
library(loo)
theme_set(theme_minimal()) #ggplot theme
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

setwd("C:/Users/Cristhian/Desktop/Rstan")

deaths <- read.csv("./trafficdeaths.csv")
head(deaths)


ggplot() +
    geom_point(aes(year, deaths), data = deaths, size = 1) +
    labs(y = 'Muertes por accidentes de Transito', x= "Anio") +
    guides(linetype = F)


fit_lin <- stan_glm(deaths ~ year, data=deaths, family="poisson",
                    refresh=100, iter=1000, chains=4, seed=1234, refresh=0)
monitor(fit_lin$stanfit)


x_predict <- seq(1993,2025)
N_predict <- length(x_predict)
y_predict <- posterior_predict(fit_lin, newdata=data.frame(year=x_predict))
mu <- apply(t(y_predict), 1, quantile, c(0.05, 0.5, 0.95)) %>%
    t() %>% data.frame(x = x_predict, .) %>% gather(pct, y, -x)
pfit <- ggplot() +
    geom_point(aes(year, deaths), data = deaths, size = 1) +
    geom_line(aes(x, y, linetype = pct), data = mu, color = 'blue') +
    scale_linetype_manual(values = c(2,1,2)) +
    labs(x = 'Year', y = 'Traffic deaths') +
    guides(linetype = F)
(pfit)


fit_gam <- stan_gamm4(deaths ~ year + s(year), data=deaths,
                      family="poisson", adapt_delta=0.999, 
                      refresh=100, iter=1000, chain=4, seed=1234, refresh=0)
monitor(fit_gam$stanfit)



x_predict=seq(1993,2025)
N_predict=length(x_predict)
y_predict <- posterior_predict(fit_gam, newdata=data.frame(year=x_predict))
mu <- apply(t(y_predict), 1, quantile, c(0.05, 0.5, 0.95)) %>%
    t() %>% data.frame(x = x_predict, .) %>% gather(pct, y, -x)
pfit <- ggplot() +
    geom_point(aes(year, deaths), data = deaths, size = 1) +
    geom_line(aes(x, y, linetype = pct), data = mu, color = 'blue') +
    scale_linetype_manual(values = c(2,1,2)) +
    labs(x = 'Anio', y = 'Muertes por accidentes de Transito') +
    guides(linetype = F)

(pfit)

N<-nrow(deaths)
Ey<-mean(deaths$deaths)
d_data <- list(N=N, x=deaths$year, y=deaths$deaths, Ey=Ey, N_predict=N_predict,
               x_predict=x_predict, alpha0=2, beta0=4)
fit_gp <- stan("./poisson_gp.stan", data=d_data, refresh=100, iter=1000,
               chains=4, seed=1234, init=0, control=list(adapt_delta=0.999))
monitor(fit_gp)



gp_params <- extract(fit_gp)
mu <- apply(t(gp_params$y_predict), 1, quantile, c(0.05, 0.5, 0.95)) %>%
    t() %>% data.frame(x = x_predict, .) %>% gather(pct, y, -x)
pfit <- ggplot() +
    geom_point(aes(year, deaths), data = deaths, size = 1) +
    geom_line(aes(x, y, linetype = pct), data = mu, color = 'blue') +
    scale_linetype_manual(values = c(2,1,2)) +
    labs(x = 'Anio', y = 'Muertes por Accidentes de Transito') +
    guides(linetype = F)
(pfit)



