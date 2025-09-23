library(lubridate)
library(Sim.DiffProc)

# Euler-Maruyama simulator for CEV SDE
simulate_cev_paths <- function(S0, mu, sigma, beta, dt, nsteps, npaths) {
  S <- matrix(NA, nrow = nsteps, ncol = npaths)
  S[1, ] <- S0
  for (i in 1:(nsteps-1)) {
    Z <- rnorm(npaths)
    S[i+1, ] <- S[i, ] + mu * S[i, ] * dt + sigma * (S[i, ]^beta) * sqrt(dt) * Z
    S[i+1, ][S[i+1, ] <= 0] <- 1e-8
  }
  return(S)
}

predict_cev <- function(train, nsteps, npaths, dt, alpha = 0.5, S0 = FALSE) {
  logret <- c(0, diff(log(train$Price)))
  sigma_start <- sd(logret)  / sqrt(dt)
  mu_start    <- mean(logret) / dt - sigma_start / 2
  beta_start  <- 1.0   # GBM as baseline
  
  start_vals <- list(mu = mu_start,
                     sigma = sigma_start,
                     beta = beta_start)
  
  S_ts <- ts(train$Price, deltat = dt)
  
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
  
  if (isFALSE(S0)) {
    S0 <- tail(train$Price, 1)
  }
  
  sim_paths <- simulate_cev_paths(
    S0     = S0,
    mu     = mu_hat,
    sigma  = sigma_hat,
    beta   = beta_hat,
    dt     = dt,
    nsteps = nsteps,
    npaths = npaths
  )
    
  ci_lower <- apply(sim_paths, 1, quantile, probs = alpha / 2)
  ci_upper <- apply(sim_paths, 1, quantile, probs = 1 - alpha / 2)
  ci_median <- apply(sim_paths, 1, median)
  
  return(data.frame(
    Median = ci_median,
    Lower  = ci_lower,
    Upper  = ci_upper
  ))
}