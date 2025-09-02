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
split_idx <- floor(n/2)

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
  pmle      = "ozaki",
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

# Total number of observations
n_obs <- nrow(sim_out_df)

# Hit ratio
hit_ratio <- hits / n_obs

hit_ratio

# Mean Squared Error (MSE)
mse_mid <- mean((sim_out_df$Actual - sim_out_df$Median)^2)

# Root Mean Squared Error (RMSE)
rmse_mid <- sqrt(mse_mid)

# Mean Absolute Percentage Error (MAPE) in percent
mape_mid <- mean(abs((sim_out_df$Actual - sim_out_df$Median) / sim_out_df$Actual)) * 100

# Normalized RMSE (NRMSE) relative to mean of actual prices
nrmse_mid <- rmse_mid / mean(sim_out_df$Actual)

# Print results
mse_mid
rmse_mid
mape_mid
nrmse_mid
