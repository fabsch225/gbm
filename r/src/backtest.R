library(ggplot2)
library(dplyr)
library(lubridate)
library(ggplot2)
library(readr)

dax <- read_csv("data/dax.csv") %>%
  mutate(Date = mdy(Date),
         Price = as.numeric(gsub(",", "", Price))) %>%
  arrange(Date)

#80/20
n <- nrow(dax)
split <- floor(0.5 * n)

train <- dax[1:split, ]
test <- dax[(split+1):n, ]

log_ret_train <- diff(log(train$Price))
mu <- mean(log_ret_train) * 252
sigma <- sd(log_ret_train) * sqrt(252)

S0 <- tail(train$Price, 1)
T <- (1:nrow(test)) / 252


alpha = 0.5

pred_df <- data.frame(
  Date = test$Date,
  Price = test$Price,
  mid = S0 * exp((mu - 0.5 * sigma^2) * T),
  low = S0 * exp((mu - 0.5 * sigma^2) * T + qnorm(alpha / 2) * sigma * sqrt(T)),
  hi  = S0 * exp((mu - 0.5 * sigma^2) * T + qnorm(1 - alpha / 2) * sigma * sqrt(T))
)

ggplot() +
  geom_line(data = train, aes(Date, Price, color = "Historic data")) +
  geom_line(data = test, aes(Date, Price, color = "Actual data")) +
  geom_ribbon(data = pred_df, aes(Date, ymin = low, ymax = hi, fill = "Confidence interval (alpha=0.5)"), alpha = 0.2) +
  geom_line(data = pred_df, aes(Date, mid, color = "Most likely path")) +
  labs(title = "Backtest: GBM Forecast vs Actual DAX",
       y = "Price", x = "Date", color = "Series", fill = "") +
  scale_color_manual(values = c("Historic data" = "black", "Actual data" = "red", "Most likely path" = "blue")) +
  scale_fill_manual(values = c("Confidence interval (alpha=0.5)" = "blue")) +
  theme_minimal()


pred_df <- pred_df %>% filter(!is.na(Price))

hit_ratio <- mean(pred_df$Price >= pred_df$low & pred_df$Price <= pred_df$hi)
rmse_mid <- sqrt(mean((pred_df$Price - pred_df$mid)^2))

log_test <- log(pred_df$Price / S0)
mu_t <- (mu - 0.5 * sigma^2) * T
sigma_t <- sigma * sqrt(T)
loglik <- sum(dnorm(log_test, mean = mu_t, sd = sigma_t, log = TRUE))

# MSE
mse_mid <- mean((pred_df$Price - pred_df$mid)^2)

mape_mid <- mean(abs((pred_df$Price - pred_df$mid) / pred_df$Price)) * 100
nrmse_mid <- rmse_mid / mean(pred_df$Price)

list(
  hit_ratio = hit_ratio,
  mse_mid = mse_mid,
  nrmse_mid = nrmse_mid,
  rmse_mid = rmse_mid,
  mape_mid = mape_mid,
  log_likelihood = loglik
)


dax_yearly <- dax %>%
  mutate(Year = year(Date)) %>%
  group_by(Year) %>%
  summarise(Price = last(Price)) %>%
  ungroup()

# Initialize results dataframe
pred_results <- data.frame(
  Year = dax_yearly$Year,
  Actual = dax_yearly$Price,
  Pred = NA,
  Low = NA,
  High = NA
)

# Set first prediction equal to first actual to avoid NA metrics
pred_results$Pred[1] <- pred_results$Actual[1]
pred_results$Low[1] <- pred_results$Actual[1]
pred_results$High[1] <- pred_results$Actual[1]

# Backtest sequentially for the rest
for(i in 2:nrow(dax_yearly)) {
  train_prices <- dax_yearly$Price[1:(i-1)]
  log_ret <- diff(log(train_prices))
  
  mu <- mean(log_ret)
  sigma <- sd(log_ret)
  S0 <- tail(train_prices, 1)
  T <- 1  # 1 year ahead
  
  alpha <- 0.05
  
  pred_results$Pred[i] <- S0 * exp(mu)
  pred_results$Low[i] <- S0 * exp(mu + qnorm(alpha / 2) * sigma)
  pred_results$High[i] <- S0 * exp(mu + qnorm(1 - alpha / 2) * sigma)
}

# Performance metrics (exclude first year if desired)
hit_ratio <- mean(pred_results$Actual >= pred_results$Low & pred_results$Actual <= pred_results$High, na.rm = TRUE)
rmse <- sqrt(mean((pred_results$Actual - pred_results$Pred)^2, na.rm = TRUE))
mape <- mean(abs((pred_results$Actual - pred_results$Pred)/pred_results$Actual) * 100, na.rm = TRUE)
nrmse <- rmse / mean(pred_results$Actual, na.rm = TRUE)

metrics <- list(
  hit_ratio = hit_ratio,
  rmse = rmse,
  nrmse = nrmse,
  mape = mape
)
print(metrics)

ggplot(pred_results, aes(x = Year)) +
  geom_point(aes(y = Actual, color = "Actual"), size = 2) +  
  geom_point(aes(y = Pred, color = "Predicted"), size = 2) +     
  geom_errorbar(aes(ymin = Low, ymax = High, color = "Confidence Interval"), width = 0.2) +
  labs(title = "Yearly GBM Backtest: DAX",
       y = "Price",
       x = "Year",
       color = "Series") +
  scale_color_manual(values = c(
    "Actual" = "black",
    "Predicted" = "blue",
    "Confidence Interval" = "red"
  )) +
  theme_minimal()