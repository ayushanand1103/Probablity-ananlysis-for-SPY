install.packages(c(
  "quantmod",
  "tidyverse",
  "PerformanceAnalytics",
  "TTR",
  "lubridate"
))

library(quantmod)
library(tidyverse)
library(PerformanceAnalytics)
library(TTR)
library(lubridate)

start_date <- "2016-01-01"
end_date   <- Sys.Date()

getSymbols(
  Symbols = "SPY",
  src = "yahoo",
  from = start_date,
  to = end_date,
  auto.assign = TRUE
)

spy <- data.frame(
  Date = index(SPY),
  coredata(SPY)
)

head(spy)

colnames(spy) <- c(
  "Date",
  "Open",
  "High",
  "Low",
  "Close",
  "Volume",
  "Adjusted"
)

spy$Return <- c(
  NA,
  diff(log(spy$Adjusted))
)
spy$AbsReturn <- abs(spy$Return)
spy$Spread <- spy$High - spy$Low
spy$RangePct <- (spy$High - spy$Low) / spy$Close
spy$PositiveDay <- ifelse(spy$Return > 0, 1, 0)
spy$Volatility21 <- runSD(
  spy$Return,
  n = 21
)
spy$Mean21 <- runMean(
  spy$Return,
  n = 21
)
spy$Mean21 <- runMean(
  spy$Return,
  n = 21
)
spy$Drawdown <- spy$Adjusted / cummax(spy$Adjusted) - 1
spy <- na.omit(spy)
str(spy)

summary(spy)
write.csv(
  spy,
  file = "C:/Random variable and probablity dis/CIA 1 A/SPY_Probability_Assignment.csv",
  row.names = FALSE
)
colSums(is.na(spy))
