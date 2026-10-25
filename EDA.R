setwd("C:/Random variable and probablity dis/CIA 1 A")

# Load Libraries
library(tidyverse)
library(moments)
library(gridExtra)

# Import Dataset
spy <- read.csv("SPY_Probability_Assignment.csv")

# Check Structure
str(spy)

# Dataset Dimensions
dim(spy)

# Summary Statistics
summary(spy)


variables <- spy %>%
  select(
    Return,
    Volatility21,
    Spread,
    Volume,
    PositiveDay
  )
summary(variables)
sapply(variables, mean)
sapply(variables, median)
sapply(variables, sd)
sapply(variables, var)
sapply(variables, min)
sapply(variables, max)

sapply(variables, skewness)
sapply(variables, kurtosis)

eda_summary <- data.frame(
  
  Mean = sapply(variables, mean),
  
  Median = sapply(variables, median),
  
  SD = sapply(variables, sd),
  
  Variance = sapply(variables, var),
  
  Minimum = sapply(variables, min),
  
  Maximum = sapply(variables, max),
  
  Skewness = sapply(variables, skewness),
  
  Kurtosis = sapply(variables, kurtosis)
  
)

round(eda_summary,4)


g1 <- ggplot(spy, aes(Return)) +
  geom_histogram(bins = 40,
                 fill = "steelblue",
                 color = "black") +
  labs(title = "Histogram of Daily Returns")

g2 <- ggplot(spy, aes(Volatility21)) +
  geom_histogram(bins = 35,
                 fill = "tomato",
                 color = "black") +
  labs(title = "Histogram of Rolling Volatility")

g3 <- ggplot(spy, aes(Spread)) +
  geom_histogram(bins = 35,
                 fill = "forestgreen",
                 color = "black") +
  labs(title = "Histogram of Daily Spread")

grid.arrange(g1, g2, g3, ncol = 1)


ggplot(spy, aes(Return)) +
  geom_density(fill = "steelblue", alpha = 0.4) +
  labs(title = "Density of Daily Returns")
ggplot(spy, aes(Volatility21)) +
  geom_density(fill = "steelblue", alpha = 0.4) +
  labs(title = "Density of Daily Returns")
ggplot(spy, aes(Spread)) +
  geom_density(fill = "steelblue", alpha = 0.4) +
  labs(title = "Density of Daily Returns")


g1 <- ggplot(spy, aes(y = Return)) +
  geom_boxplot(fill = "steelblue")

g2 <- ggplot(spy, aes(y = Volatility21)) +
  geom_boxplot(fill = "tomato")

g3 <- ggplot(spy, aes(y = Spread)) +
  geom_boxplot(fill = "forestgreen")

grid.arrange(g1, g2, g3)

continuous <- spy %>%
  select(Return,
         Volatility21,
         Spread)

cor(continuous)


# Correlation Matrix

continuous <- spy %>%
  select(Return, Volatility21, Spread)

cor_matrix <- cor(continuous)

round(cor_matrix, 4)




# Load libraries
library(tidyverse)
library(ggplot2)

# Load dataset
spy <- read.csv("SPY_Probability_Assignment.csv")

# Convert Date to Date format
spy$Date <- as.Date(spy$Date)


# 1. Daily Returns - Type 1 Chart
ggplot(spy, aes(x = Date, y = Return)) +
  geom_line() +
  labs(
    title = "Daily Returns Over Time",
    x = "Date",
    y = "Daily Return"
  ) +
  theme_minimal()


# 2. 21-Day Rolling Volatility - Type 1 Chart
ggplot(spy, aes(x = Date, y = Volatility21)) +
  geom_line() +
  labs(
    title = "21-Day Rolling Volatility Over Time",
    x = "Date",
    y = "21-Day Rolling Volatility"
  ) +
  theme_minimal()


# 3. Daily Spread - Type 1 Chart
ggplot(spy, aes(x = Date, y = Spread)) +
  geom_line() +
  labs(
    title = "Daily Spread Over Time",
    x = "Date",
    y = "Spread"
  ) +
  theme_minimal()


# 4. Trading Volume - Type 1 Chart
ggplot(spy, aes(x = Date, y = Volume)) +
  geom_line() +
  labs(
    title = "Trading Volume Over Time",
    x = "Date",
    y = "Trading Volume"
  ) +
  theme_minimal()
