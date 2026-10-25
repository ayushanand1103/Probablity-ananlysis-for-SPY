# Set Working Directory
setwd("C:/Random variable and probablity dis/CIA 1 A")

# Load Libraries
library(tidyverse)
library(ggplot2)

# Load Dataset
spy <- read.csv("SPY_Probability_Assignment.csv")

ggplot(spy, aes(x = Return)) +
  geom_histogram(
    aes(y = after_stat(density)),
    bins = 40,
    fill = "steelblue",
    color = "black",
    alpha = 0.7
  ) +
  labs(
    title = "Probability Density of Daily Returns",
    x = "Daily Return",
    y = "Density"
  ) +
  theme_minimal(base_size = 14)
ggplot(spy, aes(x = Return)) +
  geom_density(
    fill = "steelblue",
    alpha = 0.4,
    linewidth = 1
  ) +
  labs(
    title = "Kernel Density Estimate of Daily Returns",
    x = "Daily Return",
    y = "Density"
  ) +
  theme_minimal(base_size = 14)
ggplot(spy, aes(x = Return)) +
  
  geom_histogram(
    aes(y = after_stat(density)),
    bins = 40,
    fill = "lightblue",
    color = "black",
    alpha = 0.6
  ) +
  
  geom_density(
    color = "red",
    linewidth = 1.2
  ) +
  
  labs(
    title = "Probability Density Function of Daily Returns",
    x = "Daily Return",
    y = "Density"
  ) +
  
  theme_minimal(base_size = 14)




ggplot(spy, aes(x = Spread)) +
  
  geom_histogram(
    aes(y = after_stat(density)),
    bins = 35,
    fill = "forestgreen",
    color = "black",
    alpha = 0.6
  ) +
  
  geom_density(
    color = "red",
    linewidth = 1.2
  ) +
  
  labs(
    title = "Probability Density Function of Daily Spread",
    x = "Spread",
    y = "Density"
  ) +
  
  theme_minimal(base_size = 14)



ggplot(spy, aes(x = Volatility21)) +
  
  geom_histogram(
    aes(y = after_stat(density)),
    bins = 35,
    fill = "orange",
    color = "black",
    alpha = 0.6
  ) +
  
  geom_density(
    color = "red",
    linewidth = 1.2
  ) +
  
  labs(
    title = "Probability Density Function of Rolling Volatility",
    x = "21-Day Rolling Volatility",
    y = "Density"
  ) +
  
  theme_minimal(base_size = 14)

ggplot(spy, aes(x = Volatility21)) +
  
  geom_density(
    color = "darkorange",
    linewidth = 1.3
  ) +
  
  labs(
    title = "Probability Density Function of 21-Day Rolling Volatility",
    x = "21-Day Rolling Volatility",
    y = "Density"
  ) +
  
  theme_minimal(base_size = 14)

ggplot(spy, aes(x = Spread)) +
  
  geom_density(
    color = "forestgreen",
    linewidth = 1.3
  ) +
  
  labs(
    title = "Probability Density Function of Daily Spread",
    x = "Spread",
    y = "Density"
  ) +
  
  theme_minimal(base_size = 14)

ggplot(spy, aes(x = Return)) +
  
  geom_density(
    color = "red",
    linewidth = 1.3
  ) +
  
  labs(
    title = "Probability Density Function of Daily Returns",
    x = "Spread",
    y = "Density"
  ) +
  
  theme_minimal(base_size = 14)

