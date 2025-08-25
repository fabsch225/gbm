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
n <- 2520
paths <- 100
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

ggplot() +
  geom_line(data = dax, aes(x = Date, y = Price), color = "black") +
  geom_line(data = sim_df[-1, ], aes(x = Date, y = Price, group = Path, color = factor(Path)), alpha = 0.7) +
  theme_minimal() +
  guides(color = "none") +
  labs(title = "DAX Historical Prices and GBM Simulations", x = "Date", y = "Price")


sim_end <- simulations[n+1, ]
ci <- quantile(sim_end, c(0.025, 0.975))
ci
