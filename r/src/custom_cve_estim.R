library(nloptr)

estimate_cev <- function(S, dt = 1/252) {
  S <- as.numeric(S)
  stopifnot(is.numeric(S), all(is.finite(S)), length(S) >= 3)
  
  dS  <- diff(S)
  S0  <- S[-length(S)]
  eps <- 1e-12
  S0[S0 <= 0] <- eps
  sigma_start <- sd(dax_in$logret)  / sqrt(dt_daily)
  mu_start    <- mean(dax_in$logret) / dt_daily - sigma_start / 2
  beta_start  <- 1.0
  init <- c(mu_start, sigma_start, beta_start) 
  
  negloglik <- function(par) {
    mu    <- par[1]
    sigma <- par[2]
    beta  <- par[3]
    if (!is.finite(mu) || !is.finite(sigma) || !is.finite(beta)) return(1e12)
    if (sigma <= 0 || beta <= 0 || beta >= 5) return(1e12)
    
    denom <- (sigma*sigma) * (S0^(2*beta)) * dt
    denom[denom <= 0] <- eps
    val <- 0.5 * sum(log(2*pi*denom) + ((dS - mu*S0*dt)^2)/denom)
    if (!is.finite(val)) val <- 1e12
    as.numeric(val)
  }
  
  res <- nloptr(
    x0     = init,
    eval_f = negloglik,
    lb     = c(-Inf, 1e-8, 1e-6),
    ub     = c( Inf,  Inf,  4.999),
    opts   = list(algorithm = "NLOPT_LN_SBPLX", xtol_rel = 1e-8, maxeval = 3000)
  )
  
  list(mu = res$solution[1], sigma = res$solution[2], beta = res$solution[3])
}

library(readr)
library(dplyr)
library(lubridate)
library(ggplot2)

dax <- read_csv("data/lufthansa.csv") %>%
  mutate(Date  = mdy(Date),
         Price = as.numeric(gsub(",", "", Price))) %>%
  arrange(Date)

fit <- estimate_cev(dax$Price)
print(fit)
