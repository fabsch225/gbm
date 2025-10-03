library(ggplot2)
library(dplyr)
library(lubridate)
library(ggplot2)
library(readr)

source("src/backtest.R")
source("src/cev.R")

dax <- read_csv("data/try.csv") %>%
  mutate(Date = mdy(Date),
         Price = as.numeric(gsub(",", "", Price))) %>%
  arrange(Date)

backtest = backtest_split(dax, predict_cev, split_ratio = 0.75, dt = 1/252, alpha = 0.5)

print(backtest)

ggplot() +
  geom_line(data = backtest$train, aes(Date, Price, color = "Historic data")) +
  geom_line(data = backtest$test, aes(Date, Price, color = "Actual data")) +
  geom_ribbon(data = backtest$pred_df, aes(Date, ymin = Lower, ymax = Upper, fill = "Confidence interval (alpha=0.5)"), alpha = 0.2) +
  geom_line(data = backtest$pred_df, aes(Date, Median, color = "Most likely path")) +
  labs(title = "Backtest: CEV Forecast vs Actual DAX",
       y = "Price", x = "Date", color = "Series", fill = "") +
  scale_color_manual(values = c("Historic data" = "black", "Actual data" = "red", "Most likely path" = "blue")) +
  scale_fill_manual(values = c("Confidence interval (alpha=0.5)" = "blue")) +
  theme_minimal()

seq_backtest <- backtest_sequential(dax, predict_cev, timestep = 2)

print(seq_backtest$metrics)

ggplot(seq_backtest$pred_results, aes(x = Date)) +
  geom_point(aes(y = Actual, color = "Actual"), size = 2) +  
  geom_point(aes(y = Median, color = "Predicted"), size = 2) +     
  geom_errorbar(aes(ymin = Lower, ymax = Upper, color = "Confidence Interval"), width = 5) +
  labs(title = "Sequential CEV Backtest: TRY/USD",
       y = "Price", x = "Period", color = "Series") +
  scale_color_manual(values = c(
    "Actual" = "black",
    "Predicted" = "blue",
    "Confidence Interval" = "red"
  )) +
  theme_minimal()