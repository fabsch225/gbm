library(dplyr)
library(lubridate)
library(readr)

dax <- read_csv("data/dax.csv") %>%
  mutate(Date = mdy(Date), Price = as.numeric(gsub(",", "", Price))) %>%
  arrange(Date)


set.seed(123)

log_returns <- diff(log(dax$Price))
boot_res <- replicate(1000, {
  sample_ret <- sample(log_returns, length(log_returns) / 2, replace = TRUE)
  c(mu = mean(sample_ret) * 252 - 0.5 * (sd(sample_ret) * sqrt(252)) ^ 2,
    sigma = sd(sample_ret) * sqrt(252))
})

boot_df <- as.data.frame(t(boot_res))
alpha = 0.05
mu_ci <- quantile(boot_df$mu, c(alpha / 2, 1 - alpha / 2))
sigma_ci <- quantile(boot_df$sigma, c(alpha / 2, 1 - alpha / 2))

list(
  mu_estimate = mean(boot_df$mu),
  mu_ci = mu_ci,
  sigma_estimate = mean(boot_df$sigma),
  sigma_ci = sigma_ci
)
