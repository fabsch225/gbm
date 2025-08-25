library(dplyr)
library(lubridate)
library(ggplot2)
library(readr)

dax <- read_csv("data/dax.csv") %>%
  mutate(Date = mdy(Date),
         Price = as.numeric(gsub(",", "", Price))) %>%
  arrange(Date)

log_returns <- diff(log(dax$Price))
mu_daily <- mean(log_returns)
sigma_daily <- sd(log_returns)
mu_annual <- mu_daily * 252
sigma_annual <- sigma_daily * sqrt(252)

set.seed(123)
n <- 252
paths <- 10000
S0 <- tail(dax$Price, 1)
dt <- 1/n
tgrid <- c(0, (1:n) * dt)

simulations <- replicate(paths, {
  W <- c(0, cumsum(rnorm(n, 0, sqrt(dt))))
  S0 * exp((mu_annual - 0.5 * sigma_annual^2) * tgrid + sigma_annual * W)
})

last_date <- max(dax$Date)
future_dates <- last_date + 0:n

sim_df <- data.frame(
  Date = rep(future_dates, paths),
  Price = as.vector(simulations),
  Path = rep(1:paths, each = n+1)
)
sim_df_10 <- sim_df %>% filter(Path %in% 1:10)

ggplot() +
  geom_line(data = dax, aes(x = Date, y = Price), color = "black") +
  geom_line(data = sim_df_10[-1, ], aes(x = Date, y = Price, group = Path, color = factor(Path)), alpha = 0.7) +
  theme_minimal() +
  guides(color = "none") +
  labs(title = "DAX Historical Prices and GBM Simulations", x = "Date", y = "Price")

# Konfidenzintervalle

# Simuliert
sim_end <- simulations[n+1, ]
ci <- quantile(sim_end, c(0.025, 0.975))
ci

# log-Normal Interval
T <- 1
z <- qnorm(c(0.025, 0.975))
ci2 <- S0 * exp((mu_annual - 0.5 * sigma_annual^2) * T + z * sigma_annual * sqrt(T))
ci2


# log-Normal Interval täglich
n <- 252
dt <- 1/252
tgrid <- (1:n) * dt
last_date <- max(dax$Date)
future_dates <- last_date + 1:n

q_low <- S0 * exp((mu_annual - 0.5 * sigma_annual^2) * tgrid + qnorm(0.025) * sigma_annual * sqrt(tgrid))
q_hi  <- S0 * exp((mu_annual - 0.5 * sigma_annual^2) * tgrid + qnorm(0.975) * sigma_annual * sqrt(tgrid))
q_med <- S0 * exp((mu_annual - 0.5 * sigma_annual^2) * tgrid)

band <- data.frame(Date = future_dates, low = q_low, mid = q_med, hi = q_hi)

ggplot() +
  geom_line(data = dax, aes(Date, Price), color = "black") +
  geom_ribbon(data = band, aes(Date, ymin = low, ymax = hi), alpha = 0.2) +
  labs(title = "DAX Historical Prices and daily Confidence intervals for the next year", x = "Date", y = "Price")

