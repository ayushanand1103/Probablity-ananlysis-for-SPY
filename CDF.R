
# Set Working Directory
setwd("C:/Random variable and probablity dis/CIA 1 A")

# Load Libraries
library(tidyverse)
library(ggplot2)

# Load Dataset
spy <- read.csv("SPY_Probability_Assignment.csv")

return_ecdf <- ecdf(spy$Return)

plot(
  return_ecdf,
  main = "Empirical CDF of Daily Returns",
  xlab = "Daily Return",
  ylab = "Cumulative Probability",
  verticals = TRUE,
  do.points = FALSE
)


ggplot(spy, aes(x = Return)) +
  stat_ecdf(
    geom = "step",
    linewidth = 1,
    color = "steelblue"
  ) +
  labs(
    title = "Empirical CDF of Daily Returns",
    x = "Daily Return",
    y = "Cumulative Probability"
  ) +
  theme_minimal(base_size = 14)


ggplot(spy, aes(x = Spread)) +
  stat_ecdf(
    geom = "step",
    linewidth = 1,
    color = "forestgreen"
  ) +
  labs(
    title = "Empirical CDF of Daily Spread",
    x = "Spread",
    y = "Cumulative Probability"
  ) +
  theme_minimal(base_size = 14)
ggplot(spy, aes(x = Volatility21)) +
  stat_ecdf(
    geom = "step",
    linewidth = 1,
    color = "darkorange"
  ) +
  labs(
    title = "Empirical CDF of Rolling Volatility",
    x = "21-Day Rolling Volatility",
    y = "Cumulative Probability"
  ) +
  theme_minimal(base_size = 14)  

