# Fitting a Normal Distribution to 21-Day Rolling Volatility

# Set Working Directory
setwd("C:/Random variable and probablity dis/CIA 1 A")

# Load Libraries
library(tidyverse)
library(MASS)

# Load Dataset
spy <- read.csv("SPY_Probability_Assignment.csv")

# Estimate Normal Distribution Parameters

mu_hat <- mean(spy$Volatility21)
sigma_hat <- sd(spy$Volatility21)

cat("Estimated Mean (mu):", mu_hat, "\n")
cat("Estimated Standard Deviation (sigma):", sigma_hat, "\n")

# Fit Normal Distribution using Maximum Likelihood

fit_normal <- fitdistr(
  spy$Volatility21,
  densfun = "normal"
)

fit_normal

# Log-Likelihood

logLik_normal <- fit_normal$loglik

logLik_normal

# AIC

k <- 2
AIC_normal <- -2 * logLik_normal + 2 * k

AIC_normal

# BIC

n <- nrow(spy)

BIC_normal <- -2 * logLik_normal + log(n) * k

BIC_normal

# Summary Table

normal_results <- data.frame(
  Distribution = "Normal",
  Mean = fit_normal$estimate["mean"],
  SD = fit_normal$estimate["sd"],
  LogLikelihood = logLik_normal,
  AIC = AIC_normal,
  BIC = BIC_normal
)

normal_results

# Histogram + KDE + Fitted Normal PDF

ggplot(spy, aes(x = Volatility21)) +
  
  geom_histogram(
    aes(y = after_stat(density)),
    bins = 35,
    fill = "lightblue",
    color = "black",
    alpha = 0.6
  ) +
  
  geom_density(
    color = "red",
    linewidth = 1.2,
    linetype = "dashed"
  ) +
  
  stat_function(
    fun = dnorm,
    args = list(
      mean = fit_normal$estimate["mean"],
      sd = fit_normal$estimate["sd"]
    ),
    color = "blue",
    linewidth = 1.3
  ) +
  
  labs(
    title = "21-Day Rolling Volatility: Histogram with Fitted Normal Distribution",
    x = "21-Day Rolling Volatility",
    y = "Density"
  ) +
  
  theme_minimal(base_size = 14)
