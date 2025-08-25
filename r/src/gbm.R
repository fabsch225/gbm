library(dplyr)
library(lubridate)
library(ggplot2)
library(readr)

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
  labs(title = "DAX Historical Prices and GBM Simulations", x = "Date", y = "Price")

# Konfidenzintervalle

alpha = 0.95#0.975
beta = 1 - alpha

# Simuliert
sim_end <- simulations[n+1, ]
ci <- quantile(sim_end, c(beta, alpha))
ci

# log-Normal Interval
T <- 252
z <- qnorm(c(beta, alpha))
ci2 <- S0 * exp((mu - 0.5 * sigma^2) * T + z * sigma * sqrt(T))
ci2


# log-Normal Interval täglich
n <- 252
last_date <- max(dax$Date)
future_dates <- last_date + 1:n

q_low <- S0 * exp((mu - 0.5 * sigma^2) * (1:n) + qnorm(beta) * sigma * sqrt((1:n)))
q_hi  <- S0 * exp((mu - 0.5 * sigma^2) * (1:n) + qnorm(alpha) * sigma * sqrt((1:n)))
q_med <- S0 * exp((mu - 0.5 * sigma^2) * (1:n))

band <- data.frame(Date = future_dates, low = q_low, mid = q_med, hi = q_hi)

ggplot() +
  geom_line(data = dax, aes(Date, Price), color = "black") +
  geom_ribbon(data = band, aes(Date, ymin = low, ymax = hi), alpha = 0.2) +
  labs(title = "DAX Historical Prices and daily Confidence intervals for the next year", x = "Date", y = "Price")

