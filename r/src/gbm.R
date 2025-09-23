predict_gbm <- function(train, nsteps, npaths, dt, alpha = 0.5, S0 = FALSE) {
  log_returns <- c(0, diff(log(train$Price)))
  sigma <- sd(log_returns)
  mu <- mean(log_returns) + 0.5 * sigma^2
  if (isFALSE(S0)) {
    S0 <- tail(train$Price, 1)
  }
  q_hi <- S0 * exp((mu - 0.5 * sigma^2) * (1:nsteps) + qnorm(1 - alpha/2) * sigma * sqrt((1:nsteps)))
  q_low  <- S0 * exp((mu - 0.5 * sigma^2) * (1:nsteps) + qnorm(alpha/2) * sigma * sqrt((1:nsteps)))
  q_med <- S0 * exp((mu - 0.5 * sigma^2) * (1:nsteps))
  
  band <- data.frame(Lower = q_low, Median = q_med, Upper = q_hi)
  return(band)
}