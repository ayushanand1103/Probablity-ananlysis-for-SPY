# Set Working Directory
setwd("C:/Random variable and probablity dis/CIA 1 A")

# Load Libraries
library(tidyverse)
library(ggplot2)

# Load Dataset
spy <- read.csv("SPY_Probability_Assignment.csv")

# PMF of PositiveDay

# Frequency Table
table(spy$PositiveDay)

# Empirical PMF
pmf_positive <- prop.table(table(spy$PositiveDay))
pmf_positive

# Convert to Data Frame
pmf_positive_df <- data.frame(
  PositiveDay = factor(
    names(pmf_positive),
    levels = c("0", "1"),
    labels = c("Negative", "Positive")
  ),
  Probability = as.numeric(pmf_positive)
)

pmf_positive_df

# PMF Bar Plot

ggplot(
  pmf_positive_df,
  aes(
    x = PositiveDay,
    y = Probability
  )
) +
  geom_col(
    fill = "steelblue",
    width = 0.5
  ) +
  geom_text(
    aes(label = round(Probability, 3)),
    vjust = -0.5,
    size = 5
  ) +
  labs(
    title = "Probability Mass Function of Positive and Negative Trading Days",
    x = "Trading Day",
    y = "Probability"
  ) +
  theme_minimal(base_size = 14)

# Bernoulli Distribution Fit

# Estimate Bernoulli Parameter
p_hat <- mean(spy$PositiveDay)
p_hat

# Theoretical Bernoulli PMF
x <- c(0, 1)

bernoulli_pmf <- ifelse(
  x == 1,
  p_hat,
  1 - p_hat
)
## 1:Positive   0:Negetive
bernoulli_df <- data.frame(
  Outcome = c("0", "1"),
  Theoretical = bernoulli_pmf
)

bernoulli_df

# Empirical vs Theoretical PMF

comparison <- data.frame(
  Outcome = c("Negative", "Positive"),
  Empirical = as.numeric(pmf_positive),
  Theoretical = bernoulli_pmf
)

comparison

# Empirical CDF of PositiveDay

cdf_positive <- cumsum(pmf_positive)

cdf_positive_df <- data.frame(
  Outcome = c("Negative", "Positive"),
  CDF = as.numeric(cdf_positive)
)

cdf_positive_df

# CDF Plot

ggplot(
  cdf_positive_df,
  aes(
    x = Outcome,
    y = CDF,
    group = 1
  )
) +
  geom_step(
    linewidth = 1.2,
    color = "steelblue"
  ) +
  geom_point(
    size = 3,
    color = "red"
  ) +
  geom_text(
    aes(label = round(CDF, 3)),
    vjust = -0.5
  ) +
  labs(
    title = "Empirical Cumulative Distribution Function of PositiveDay",
    x = "Trading Day",
    y = "Cumulative Probability"
  ) +
  theme_minimal(base_size = 14)

# Trading Volume

# Number of Unique Trading Volume Values
num_unique_volume <- length(unique(spy$Volume))

cat("Number of Unique Trading Volume Values:", num_unique_volume, "\n")

# Empirical PMF
pmf_volume <- prop.table(table(spy$Volume))

# Display First Few PMF Values
head(pmf_volume)

# Summary Statistics
summary(spy$Volume)

# Empirical CDF
volume_ecdf <- ecdf(spy$Volume)

# CDF Plot of Trading Volume

ggplot(spy, aes(x = Volume)) +
  stat_ecdf(
    geom = "step",
    linewidth = 1,
    color = "steelblue"
  ) +
  labs(
    title = "Empirical CDF of Trading Volume",
    x = "Trading Volume",
    y = "Cumulative Probability"
  ) +
  theme_minimal(base_size = 14)

# Note

cat(
  "\nTrading volume is a discrete random variable.\n",
  "However, it contains", num_unique_volume,
  "unique values, making the empirical PMF extremely sparse.\n",
  "Therefore, a PMF visualization is not presented, while the empirical CDF is used to summarize its distribution.\n"
)
