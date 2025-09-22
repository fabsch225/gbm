library(readr)
library(dplyr)
library(lubridate)
library(ggplot2)
library(Sim.DiffProc)

dax <- read_csv("data/dax.csv") %>%
  mutate(Date  = mdy(Date),
         Price = as.numeric(gsub(",", "", Price))) %>%
  arrange(Date)

dax <- dax %>%
  mutate(logret = c(NA, diff(log(Price)))) %>%
  filter(!is.na(logret))

dt_daily = 1 / 252
n <- nrow(dax)
split_idx <- floor(n - n/4)

dax_in  <- dax[1:split_idx, ]
dax_out <- dax[(split_idx+1):n, ]

sigma_start <- sd(dax_in$logret)  / sqrt(dt_daily)
mu_start    <- mean(dax_in$logret) / dt_daily - sigma_start / 2
beta_start  <- 1.0

S_ts_in <- ts(dax_in$Price, deltat = dt_daily)


drift     <- expression(theta[1] * x)           # theta1 = mu
diffusion <- expression(theta[2] * x^theta[3])    # theta2 = sigma, theta3 = beta

fit_cev_in <- fitsde(
  data      = S_ts_in,
  drift     = drift,
  diffusion = diffusion,
  start     = list(theta1 = mu_start,
                   theta2 = sigma_start,
                   theta3 = beta_start),
  pmle      = "euler",
  optim.method = "L-BFGS-B",
  lower     = c(theta1 = -Inf, theta2 = 1e-8, theta3 = 0.0),
  upper     = c(theta1 =  Inf, theta2 =  Inf, theta3 = 3.0)
)

summary(fit_cev_in)

simulate_cev_paths <- function(S0, mu, sigma, beta, dt, nsteps, npaths) {
  S <- matrix(NA, nrow = nsteps + 1, ncol = npaths)
  S[1, ] <- S0
  for (i in 1:nsteps) {
    Z <- rnorm(npaths)
    S[i+1, ] <- S[i, ] + mu * S[i, ] * dt + sigma * (S[i, ]^beta) * sqrt(dt) * Z
    S[i+1, ][S[i+1, ] <= 0] <- 1e-8
  }
  return(S)
}

theta_hat <- coef(fit_cev_in)
mu_hat    <- theta_hat[1]
sigma_hat <- theta_hat[2]
beta_hat  <- theta_hat[3]

nsteps <- nrow(dax_out) - 1
npaths <- 1000

set.seed(123)
sim_paths <- simulate_cev_paths(
  S0     = dax_in$Price[nrow(dax_in)],
  mu     = mu_hat,
  sigma  = sigma_hat,
  beta   = beta_hat,
  dt     = 1/252,
  nsteps = nsteps,
  npaths = npaths
)

alpha = 0.5

ci_lower <- apply(sim_paths, 1, quantile, probs = alpha / 2)
ci_upper <- apply(sim_paths, 1, quantile, probs = 1 - alpha / 2)
ci_median <- apply(sim_paths, 1, median)

sim_out_df <- data.frame(
  Date   = dax_out$Date,
  Actual = dax_out$Price,
  Lower  = ci_lower,
  Upper  = ci_upper,
  Median = ci_median
)

# Combine historical and out-of-sample data
sim_out_df$Type <- "Out-of-Sample"
dax_in_df <- data.frame(
  Date   = dax_in$Date,
  Price  = dax_in$Price,
  Type   = "Historical"
)

ggplot() +
  geom_ribbon(data = sim_out_df, aes(x = Date, ymin = Lower, ymax = Upper, fill = "confidence band to alpha=0.5"), alpha = 0.5) +
  geom_line(data = sim_out_df, aes(x = Date, y = Median, color = "median"), linetype = "dashed") +
  geom_line(data = sim_out_df, aes(x = Date, y = Actual, color = "out-of-sample actual")) +
  geom_line(data = dax_in_df, aes(x = Date, y = Price, color = "historical")) +
  labs(title = "DAX: CEV model 50/50 backtest: historical and out-of-sample performance",
       y = "Price", x = "Date",
       color = "Legend", fill = "") +
  scale_color_manual(values = c(
    "historical" = "black",
    "out-of-sample actual" = "blue",
    "median" = "red"
  )) +
  scale_fill_manual(values = c("confidence band to alpha=0.5" = "grey70")) +
  theme_minimal()


hits <- sum(sim_out_df$Actual >= sim_out_df$Lower & sim_out_df$Actual <= sim_out_df$Upper)

n_obs <- nrow(sim_out_df)

hit_ratio <- hits / n_obs

hit_ratio

mse_mid <- mean((sim_out_df$Actual - sim_out_df$Median)^2)

rmse_mid <- sqrt(mse_mid)

mape_mid <- mean(abs((sim_out_df$Actual - sim_out_df$Median) / sim_out_df$Actual)) * 100

nrmse_mid <- rmse_mid / mean(sim_out_df$Actual)

mse_mid
rmse_mid
mape_mid
nrmse_mid



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
  
  
  S_ts_in <- ts(train_prices, deltat = 1)
  
  drift     <- expression(theta[1] * x)           # theta1 = mu
  diffusion <- expression(theta[2] * x^theta[3])    # theta2 = sigma, theta3 = beta
  
  fit_cev_in <- fitsde(
    data      = S_ts_in,
    drift     = drift,
    diffusion = diffusion,
    start     = list(theta1 = mu_start,
                     theta2 = sigma_start,
                     theta3 = beta_start),
    pmle      = "euler",
    optim.method = "L-BFGS-B",
    lower     = c(theta1 = -Inf, theta2 = 1e-8, theta3 = 0.0),
    upper     = c(theta1 =  Inf, theta2 =  Inf, theta3 = 3.0)
  )
  
  
  theta_hat <- coef(fit_cev_in)
  mu_hat    <- theta_hat[1]
  sigma_hat <- theta_hat[2]
  beta_hat  <- theta_hat[3]
  
  nsteps <- nrow(dax_out) - 1
  npaths <- 1000
  
  set.seed(123)
  sim_paths <- simulate_cev_paths(
    S0     = train_prices[i - 1],
    mu     = mu_hat,
    sigma  = sigma_hat,
    beta   = beta_hat,
    dt     = 1/254,
    nsteps = 254,
    npaths = npaths
  )
  
  alpha = 0.5

  ci_lower <- apply(sim_paths, 1, quantile, probs = alpha / 2)
  ci_upper <- apply(sim_paths, 1, quantile, probs = 1 - alpha / 2)
  ci_median <- apply(sim_paths, 1, median)
  
  pred_results$Pred[i] <- ci_median[254]
  pred_results$Low[i] <- ci_lower[254]
  pred_results$High[i] <- ci_upper[254]
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
  labs(title = "Yearly CEV Backtest: DAX",
       y = "Price",
       x = "Year",
       color = "Series") +
  scale_color_manual(values = c(
    "Actual" = "black",
    "Predicted" = "blue",
    "Confidence Interval" = "red"
  )) +
  theme_minimal()