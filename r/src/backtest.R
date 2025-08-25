library(ggplot2)

#80/20
n <- nrow(dax)
split <- floor(0.8 * n)
train <- dax[1:split, ]
test <- dax[(split+1):n, ]

log_ret_train <- diff(log(train$Price))
mu <- mean(log_ret_train) * 252
sigma <- sd(log_ret_train) * sqrt(252)

S0 <- tail(train$Price, 1)
T <- (1:nrow(test)) / 252

pred_df <- data.frame(
  Date = test$Date,
  Price = test$Price,
  mid = S0 * exp((mu - 0.5 * sigma^2) * T),
  low = S0 * exp((mu - 0.5 * sigma^2) * T + qnorm(0.025) * sigma * sqrt(T)),
  hi  = S0 * exp((mu - 0.5 * sigma^2) * T + qnorm(0.975) * sigma * sqrt(T))
)

ggplot() +
  geom_line(data = train, aes(Date, Price), color = "black") +
  geom_line(data = test, aes(Date, Price), color = "red") +
  geom_ribbon(data = pred_df, aes(Date, ymin = low, ymax = hi), alpha = 0.2, fill = "blue") +
  geom_line(data = pred_df, aes(Date, mid), color = "blue") +
  labs(title = "Backtest: GBM Forecast vs Actual DAX", y = "Price", x = "Date") +
  theme_minimal()


pred_df <- pred_df %>% filter(!is.na(Price))

hit_ratio <- mean(pred_df$Price >= pred_df$low & pred_df$Price <= pred_df$hi)
rmse_mid <- sqrt(mean((pred_df$Price - pred_df$mid)^2))

log_test <- log(pred_df$Price / S0)
mu_t <- (mu - 0.5 * sigma^2) * T
sigma_t <- sigma * sqrt(T)
loglik <- sum(dnorm(log_test, mean = mu_t, sd = sigma_t, log = TRUE))

list(
  hit_ratio = hit_ratio,
  rmse_mid = rmse_mid,
  log_likelihood = loglik
)