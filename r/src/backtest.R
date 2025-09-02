library(ggplot2)

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
  geom_line(data = train, aes(Date, Price, color = "Train")) +
  geom_line(data = test, aes(Date, Price, color = "Test")) +
  geom_ribbon(data = pred_df, aes(Date, ymin = low, ymax = hi, fill = "Prediction Interval"), alpha = 0.2) +
  geom_line(data = pred_df, aes(Date, mid, color = "Prediction")) +
  labs(title = "Backtest: GBM Forecast vs Actual DAX",
       y = "Price", x = "Date", color = "Series", fill = "") +
  scale_color_manual(values = c("Train" = "black", "Test" = "red", "Prediction" = "blue")) +
  scale_fill_manual(values = c("Prediction Interval" = "blue")) +
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

# MAPE (in Prozent)
mape_mid <- mean(abs((pred_df$Price - pred_df$mid) / pred_df$Price)) * 100

# NRMSE (relativ zum Mittelwert des Test-Sets)
nrmse_mid <- rmse_mid / mean(pred_df$Price)

list(
  hit_ratio = hit_ratio,
  mse_mid = mse_mid,
  nrmse_mid = nrmse_mid,
  rmse_mid = rmse_mid,
  mape_mid = mape_mid,
  log_likelihood = loglik
)