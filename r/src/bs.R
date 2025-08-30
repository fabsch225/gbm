library(dplyr)
library(lubridate)
library(ggplot2)
library(readr)

set.seed(123)

dax <- read_csv("data/dax.csv") %>%
  mutate(Date = mdy(Date),
         Price = as.numeric(gsub(",", "", Price))) %>%
  arrange(Date)

log_returns <- diff(log(dax$Price))
sigma <- sd(log_returns)              
mu <- mean(log_returns) + 0.5 * sigma^2
S0 <- tail(dax$Price, 1)            

bs_call <- function(S0, K, r, T, sigma) {
  d1 <- (log(S0/K) + (r + 0.5 * sigma^2) * T) / (sigma * sqrt(T))
  d2 <- d1 - sigma * sqrt(T)
  C <- S0 * pnorm(d1) - K * exp(-r*T) * pnorm(d2)
  return(C)
}

r <- 0.02
T <- 1   

strikes <- seq(0.7*S0, 1.3*S0, length.out = 50)
call_prices <- sapply(strikes, function(K) bs_call(S0, K, r, T, sigma))

df_calls <- data.frame(Strike = strikes, CallPrice = call_prices)

ggplot(df_calls, aes(x = Strike, y = CallPrice)) +
  geom_line(color = "blue", size = 1) +
  labs(title = "Europäische Call-Option auf den DAX (Black-Scholes)",
       subtitle = paste("S0 =", round(S0,0),
                        ", sigma =", round(sigma,4),
                        ", r =", r,
                        ", T =", T, "Jahr"),
       x = "Strike K",
       y = "Optionspreis C0") +
  theme_minimal(base_size = 14)

K <- 22000
C_bs <- bs_call(S0, K, r, T, sigma)
mc_call <- function(S0, K, r, T, sigma, n) {
  Z <- rnorm(n)
  ST <- S0 * exp((r - 0.5*sigma^2)*T + sigma*sqrt(T)*Z)
  payoff <- pmax(ST - K, 0)
  C <- exp(-r*T) * mean(payoff)
  return(C)
}

n_grid <- round(10 ^ seq(0, 5, length.out = 25))
mc_estimates <- sapply(n_grid, function(n) mc_call(S0, K, r, T, sigma, n))

df <- data.frame(n = n_grid, MC = mc_estimates)

ggplot(df, aes(x = n, y = MC)) +
  geom_point(color = "blue", size = 2) +
  geom_line(color = "blue", linewidth = 0.8) +
  geom_hline(yintercept = C_bs, color = "red", linetype = "dashed") +
  scale_x_log10() +
  labs(title = "Monte-Carlo-Simulation des Callpreises",
       subtitle = paste("Strike K =", K, ", S0 =", S0,
                        ", sigma =", round(sigma,4), ", r =", r, ", T =", T),
       x = "Anzahl Simulationen n",
       y = "Optionpreis (Monte Carlo)") +
  annotate("text", x = max(n_grid), y = C_bs,
           label = paste("BS-Preis ≈", round(C_bs,2)),
           vjust = -1, hjust = 1, color = "red") +
  theme_minimal(base_size = 14)
# Asian Option via Monte Carlo
m <- 1000
N <- 100

Z <- matrix(rnorm(N * m), nrow=N, ncol=m) # n Pfade mit m Zeitschritten
dt <- T / m
S <- matrix(0, nrow=N, ncol=m)
S[,1] <- S0
for (j in 2:m) {
  S[,j] <- S[,j-1] * exp((r - 0.5*sigma^2)*dt + sigma*sqrt(dt)*Z[,j])
}
avg_price <- rowMeans(S) # Durchschnittspreis für jeden Pfad
payoff <- pmax(avg_price - K, 0) # Bewertungsfunktion
C <- exp(-r*T) * mean(payoff) # Monte-Carlo-Schätzer
