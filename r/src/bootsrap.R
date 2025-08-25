library(dplyr)
library(lubridate)
library(readr)

dax <- read_csv("data/dax.csv") %>%
  mutate(Date = mdy(Date), Price = as.numeric(gsub(",", "", Price))) %>%
  arrange(Date)

log_returns <- diff(log(dax$Price))

set.seed(123)
B <- 1000
boot_res <- replicate(B, {
  sample_ret <- sample(log_returns, length(log_returns), replace = TRUE)
  c(mu = mean(sample_ret) * 252,
    sigma = sd(sample_ret) * sqrt(252))
})

boot_df <- as.data.frame(t(boot_res))
mu_ci <- quantile(boot_df$mu, c(0.025, 0.975))
sigma_ci <- quantile(boot_df$sigma, c(0.025, 0.975))

list(
  mu_estimate = mean(boot_df$mu),
  mu_ci = mu_ci,
  sigma_estimate = mean(boot_df$sigma),
  sigma_ci = sigma_ci
)
