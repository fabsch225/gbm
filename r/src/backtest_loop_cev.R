# backtest_loop.R
# Testet DAX, Lufthansa, Adesso separat für Gewichte 50/50 bis 90/10
# Ergebnisse -> out1.csv

library(readr)
library(dplyr)
library(lubridate)
library(Sim.DiffProc)

set.seed(123)

# ---------------- helper ----------------
read_price <- function(path) {
  df <- read_csv(path, show_col_types = FALSE, guess_max = 5000)
  # Date
  date_col <- names(df)[grepl("Date", names(df), ignore.case = TRUE)][1]
  if (is.na(date_col)) date_col <- names(df)[1]
  df[[date_col]] <- trimws(as.character(df[[date_col]]))
  parsed <- parse_date_time(df[[date_col]],
                            orders = c("ymd","mdy","dmy","Y-m-d","d/m/Y","m/d/Y"),
                            truncated = 3)
  df$Date <- as_date(parsed)
  # Price
  price_col <- names(df)[grepl("Price", names(df), ignore.case = TRUE)][1]
  if (is.na(price_col)) {
    price_col <- setdiff(names(df), c(date_col, "Date"))[1]
  }
  clean_num <- function(x) {
    x <- gsub("\\s+", "", x)
    x <- gsub("[^0-9,.-]", "", x)
    if (grepl(",", x) && grepl("\\.", x)) {
      x <- gsub("\\.", "", x)
      x <- gsub(",", ".", x)
    } else if (grepl(",", x) && !grepl("\\.", x)) {
      x <- gsub(",", ".", x)
    }
    as.numeric(x)
  }
  df$Price <- vapply(as.character(df[[price_col]]), clean_num, numeric(1))
  df %>% select(Date, Price) %>% arrange(Date) %>% filter(!is.na(Price))
}

simulate_cev_paths <- function(S0, mu, sigma, beta, dt, nsteps, npaths) {
  S <- matrix(NA, nrow = nsteps + 1, ncol = npaths)
  S[1, ] <- S0
  for (i in 1:nsteps) {
    Z <- rnorm(npaths)
    S[i+1, ] <- S[i, ] + mu * S[i, ] * dt + sigma * (S[i, ]^beta) * sqrt(dt) * Z
    S[i+1, ][S[i+1, ] <= 0] <- 1e-8
  }
  S
}

# ---------------- load data ----------------
assets <- list(
  dax       = read_price("data/dax.csv"),
  lufthansa = read_price("data/lufthansa.csv"),
  adesso    = read_price("data/adesso.csv")
)

# ---------------- backtest loop ----------------
weights <- seq(0.7, 0.95, by = 0.05) # 50%, 60%, … 90%

results <- data.frame()

for (asset_name in names(assets)) {
  df <- assets[[asset_name]] %>%
    mutate(logret = c(NA, diff(log(Price)))) %>%
    filter(!is.na(logret))
  
  for (w in weights) {
    n <- nrow(df)
    split_idx <- floor(n * w)
    df_in  <- df[1:split_idx, ]
    df_out <- df[(split_idx+1):n, ]
    
    dt <- 1/252
    sigma_start <- sd(df_in$logret) / sqrt(dt)
    mu_start    <- mean(df_in$logret) / dt - sigma_start / 2
    beta_start  <- 1.0
    
    S_ts_in <- ts(df_in$Price, deltat = dt)
    
    fit <- fitsde(
      data      = S_ts_in,
      drift     = expression(theta[1] * x),
      diffusion = expression(theta[2] * x^theta[3]),
      start     = list(theta1 = mu_start, theta2 = sigma_start, theta3 = beta_start),
      pmle      = "euler",
      optim.method = "L-BFGS-B",
      lower     = c(theta1 = -Inf, theta2 = 1e-8, theta3 = 0.0),
      upper     = c(theta1 =  Inf, theta2 =  Inf, theta3 = 3.0)
    )
    
    theta_hat <- coef(fit)
    mu_hat    <- theta_hat[1]
    sigma_hat <- theta_hat[2]
    beta_hat  <- theta_hat[3]
    
    nsteps <- nrow(df_out) - 1
    npaths <- 5000
    
    sim_paths <- simulate_cev_paths(
      S0     = tail(df_in$Price, 1),
      mu     = mu_hat,
      sigma  = sigma_hat,
      beta   = beta_hat,
      dt     = dt,
      nsteps = nsteps,
      npaths = npaths
    )
    
    alpha = 0.5
    ci_lower  <- apply(sim_paths, 1, quantile, probs = alpha / 2)
    ci_upper  <- apply(sim_paths, 1, quantile, probs = 1 - alpha / 2)
    ci_median <- apply(sim_paths, 1, median)
    
    sim_out_df <- data.frame(
      Date   = df_out$Date,
      Actual = df_out$Price,
      Lower  = ci_lower,
      Upper  = ci_upper,
      Median = ci_median
    )
    
    
    hits <- sum(sim_out_df$Actual >= sim_out_df$Lower &
                  sim_out_df$Actual <= sim_out_df$Upper)
    n_obs <- nrow(sim_out_df)
    hit_ratio <- hits / n_obs
    
    mse_mid  <- mean((sim_out_df$Actual - sim_out_df$Median)^2)
    rmse_mid <- sqrt(mse_mid)
    mape_mid <- mean(abs((sim_out_df$Actual - sim_out_df$Median) / sim_out_df$Actual)) * 100
    nrmse_mid <- rmse_mid / mean(sim_out_df$Actual)
    
    results <- rbind(results, data.frame(
      Asset    = asset_name,
      Weight   = w,
      HitRatio = hit_ratio,
      MSE      = mse_mid,
      RMSE     = rmse_mid,
      MAPE     = mape_mid,
      NRMSE    = nrmse_mid
    ))
  }
}

# ---------------- sequential CEV backtest (safe start) ----------------
for (asset_name in names(assets)) {
  df <- assets[[asset_name]] %>%
    mutate(logret = c(NA, diff(log(Price)))) %>%
    filter(!is.na(logret))
  
  n <- nrow(df)
  dt <- 1/252
  alpha <- 0.5  # CI width
  
  pred_results <- data.frame(
    Date   = df$Date,
    Actual = df$Price,
    Median = NA,
    Lower  = NA,
    Upper  = NA
  )
  
  # Start from i = 5 for stable parameter estimates
  for (i in 5:n:100) {
    df_train <- df[1:(i-1), ]
    
    # Safe start values
    sigma_start <- sd(df_train$logret) / sqrt(dt)
    sigma_start <- max(sigma_start, 1e-6)  # avoid zero
    mu_start    <- mean(df_train$logret) / dt - sigma_start / 2
    beta_start  <- 1.0
    
    S_ts_in <- ts(df_train$Price, deltat = dt)
    
    fit <- fitsde(
      data      = S_ts_in,
      drift     = expression(theta[1] * x),
      diffusion = expression(theta[2] * x^theta[3]),
      start     = list(theta1 = mu_start, theta2 = sigma_start, theta3 = beta_start),
      pmle      = "euler",
      optim.method = "L-BFGS-B",
      lower     = c(theta1 = -Inf, theta2 = 1e-8, theta3 = 0.0),
      upper     = c(theta1 = Inf, theta2 = Inf, theta3 = 3.0)
    )
    
    theta_hat <- coef(fit)
    mu_hat    <- theta_hat[1]
    sigma_hat <- theta_hat[2]
    beta_hat  <- theta_hat[3]
    
    # Simulate 1-step ahead
    sim_paths <- simulate_cev_paths(
      S0     = tail(df_train$Price, 1),
      mu     = mu_hat,
      sigma  = sigma_hat,
      beta   = beta_hat,
      dt     = dt,
      nsteps = 1,
      npaths = 5000
    )
    
    pred_results$Median[i] <- median(sim_paths[2, ])
    pred_results$Lower[i]  <- quantile(sim_paths[2, ], probs = alpha / 2)
    pred_results$Upper[i]  <- quantile(sim_paths[2, ], probs = 1 - alpha / 2)
  }
  
  # Metrics
  hits <- sum(pred_results$Actual >= pred_results$Lower &
                pred_results$Actual <= pred_results$Upper, na.rm = TRUE)
  n_obs <- sum(!is.na(pred_results$Median))
  hit_ratio <- hits / n_obs
  
  mse_mid  <- mean((pred_results$Actual - pred_results$Median)^2, na.rm = TRUE)
  rmse_mid <- sqrt(mse_mid)
  mape_mid <- mean(abs((pred_results$Actual - pred_results$Median) / pred_results$Actual) * 100, na.rm = TRUE)
  nrmse_mid <- rmse_mid / mean(pred_results$Actual, na.rm = TRUE)
  
  results <- rbind(results, data.frame(
    Asset    = asset_name,
    Weight   = "SEQUENTIAL",
    HitRatio = hit_ratio,
    MSE      = mse_mid,
    RMSE     = rmse_mid,
    MAPE     = mape_mid,
    NRMSE    = nrmse_mid
  ))
}

# ---------------- speichern ----------------
write_csv(results, "out1.csv")
print("Fertig! Ergebnisse in out1.csv gespeichert.")
