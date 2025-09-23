library(dplyr)
library(lubridate)
library(ggplot2)
library(readr)

source("gbm.R")

dax <- read_csv("data/dax.csv") %>%
  mutate(Date = mdy(Date),
         Price = as.numeric(gsub(",", "", Price))) %>%
  arrange(Date)

log_returns <- diff(log(dax$Price))
sigma <- sd(log_returns)
mu <- mean(log_returns) + 0.5 * sigma^2

set.seed(123)
n <- 252
paths <- 10000
S0 <- tail(dax$Price, 1)
dt <- 1/n
tgrid <- c(0, (1:n) * dt)

simulations <- replicate(paths, {
  W <- c(0, cumsum(rnorm(n, 0, 1)))
  S0 * exp((mu - 0.5 * sigma^2) *  c(0, (1:n)) + sigma * W)
})

last_date <- max(dax$Date)
future_dates <- last_date + 0:n

sim_df <- data.frame(
  Date = rep(future_dates, paths),
  Price = as.vector(simulations),
  Path = rep(1:paths, each = n+1)
)
sim_df_examples <- sim_df %>% filter(Path %in% 1:4)

ggplot() +
  geom_line(data = dax, aes(x = Date, y = Price), color = "black") +
  geom_line(data = sim_df_examples[-1, ], aes(x = Date, y = Price, group = Path, color = factor(Path)), alpha = 0.7) +
  theme_minimal() +
  guides(color = "none") +
  labs(title = "DAX historical prices and simulations", x = "Date", y = "Price")


alpha = 0.05

sim_end <- simulations[n+1, ]
ci <- quantile(sim_end, c(1 - alpha/2, alpha/2))
ci

# log-Normal Interval

T <- 252
z <- qnorm(c(1 - alpha/2, alpha/2))
ci2 <- S0 * exp((mu - 0.5 * sigma^2) * T + z * sigma * sqrt(T))
ci2

n <- 252
last_date <- max(dax$Date)
future_dates <- last_date + 1:n
pred = predict_gbm(
  train  = dax,
  nsteps = n,
  npaths = 1,
  dt     = 1/252,
  alpha  = alpha
)

pred <- pred %>%
  mutate(Date = future_dates)

ggplot() +
  geom_line(data = dax, aes(Date, Price), color = "black") +
  geom_ribbon(data = pred, aes(Date, ymin = Lower, ymax = Higher), alpha = 0.2) +
  labs(title = "DAX historical prices and daily confidence intervals for the next year", x = "Date", y = "Price") +
  theme_classic()

# Vergleich von Simulation und explizitem Intervall

get_sim_ci <- function(paths, n, mu, sigma, S0, alpha = 0.05) {
  set.seed(123)
  W <- matrix(rnorm(n * paths), nrow = n, ncol = paths)
  W <- apply(W, 2, cumsum)
  S_T <- S0 * exp((mu - 0.5 * sigma^2) * n + sigma * W[n, ])
  quantile(S_T, c(1 - alpha/2, alpha/2))
}


T <- 252
alpha <- 0.05
z <- qnorm(c(1 - alpha/2, alpha/2))
ci_explicit <- S0 * exp((mu - 0.5 * sigma^2) * T + z * sigma * sqrt(T))
ci_explicit

path_sizes <- c(5, 10, 20, 50, 100, 200, 500, 1000, 2000, 5000, 10000, 20000)

results <- data.frame(
  paths = path_sizes,
  lower = NA, upper = NA
)

for (i in seq_along(path_sizes)) {
  ci <- get_sim_ci(path_sizes[i], T, mu, sigma, S0, alpha)
  results$lower[i] <- ci[2]
  results$upper[i] <- ci[1]
}

results <- results %>%
  mutate(exp_lower = ci_explicit[2],
         exp_upper = ci_explicit[1])

print(results)

ggplot(results, aes(x = paths)) +
  geom_line(aes(y = lower, color = "Simulated Lower")) +
  geom_line(aes(y = upper, color = "Simulated Upper")) +
  geom_hline(aes(yintercept = ci_explicit[2], color = "Explicit Lower"), linetype = "dashed") +
  geom_hline(aes(yintercept = ci_explicit[1], color = "Explicit Upper"), linetype = "dashed") +
  scale_x_log10() +
  labs(title = "Convergence of simulated ci to explicit lognormal ci",
       x = "Number of simulations", y = "Confidence interval bound") +
  theme_minimal() +
  scale_color_manual(values = c("red","blue","darkred","darkblue"))