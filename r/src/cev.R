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

dt_daily <- 1/252  # trading days per year

sigma_start <- sd(dax$logret)  / sqrt(dt_daily)
mu_start    <- mean(dax$logret) / dt_daily - sigma_start / 2
beta_start  <- 1.0   # GBM as baseline

start_vals <- list(mu = mu_start,
                   sigma = sigma_start,
                   beta = beta_start)

print(start_vals)

S_ts <- ts(dax$Price, deltat = dt_daily)

drift     <- expression(theta[1] * x)           # theta1 = mu
diffusion <- expression(theta[2] * x^theta[3])    # theta2 = sigma, theta3 = beta

fit_cev <- fitsde(
  data      = S_ts,
  drift     = drift,
  diffusion = diffusion,
  start     = start_vals,
  pmle      = "euler",
  optim.method = "L-BFGS-B",
  lower     = c(mu = -Inf, sigma = 1e-8, beta = 0.0),
  upper     = c(mu =  Inf, sigma =  Inf, beta = 3.0)
)

summary(fit_cev) 
coef(fit_cev)
confint(fit_cev)

theta_hat <- coef(fit_cev)
mu_hat    <- theta_hat[1]
sigma_hat <- theta_hat[2]
beta_hat  <- theta_hat[3]

set.seed(123)

# Euler-Maruyama simulator for CEV SDE
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

set.seed(123)
nsteps <- nrow(dax) - 1
npaths <- 1000
sim_paths <- simulate_cev_paths(
  S0     = dax$Price[1],
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

sim_df <- data.frame(
  Date   = dax$Date,
  Actual = dax$Price,
  Lower  = ci_lower,
  Upper  = ci_upper,
  Median = ci_median
)
library(ggplot2)

ggplot(sim_df, aes(x = Date)) +
  geom_ribbon(aes(ymin = Lower, ymax = Upper, fill = "Confidence Band"), alpha = 0.5) +
  geom_line(aes(y = Actual, color = "Actual")) +
  geom_line(aes(y = Median, color = "Median"), linetype = "dashed") +
  labs(title = "CEV Model Simulation: alpha=0.5 Confidence Band vs Actual DAX",
       y = "Price", x = "Date",
       color = "Legend", fill = "") +
  scale_color_manual(values = c("Actual" = "blue", "Median" = "red")) +
  scale_fill_manual(values = c("Confidence Band" = "grey70")) +
  theme_minimal()

