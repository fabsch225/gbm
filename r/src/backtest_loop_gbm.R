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

# ---------------- backtest loop with sequential integrated ----------------
weights <- seq(0.7, 0.95, by = 0.05) # 70%, 75%, … 95%
results <- data.frame()

for (asset_name in names(assets)) {
  df <- assets[[asset_name]] %>%
    arrange(Date) %>%
    mutate(logret = c(NA, diff(log(Price)))) %>%
    filter(!is.na(logret))
  
  # --- Train/Test split backtests for weights ---
  for (w in weights) {
    n <- nrow(df)
    split <- floor(w * n)
    
    train <- df[1:split, ]
    test <- df[(split+1):n, ]
    
    log_ret_train <- diff(log(train$Price))
    mu <- mean(log_ret_train) * 252
    sigma <- sd(log_ret_train) * sqrt(252)
    
    S0 <- tail(train$Price, 1)
    T <- (1:nrow(test)) / 252
    
    alpha <- 0.05
    
    sim_out_df <- data.frame(
      Date = test$Date,
      Actual = test$Price,
      Median = S0 * exp((mu - 0.5 * sigma^2) * T),
      Lower  = S0 * exp((mu - 0.5 * sigma^2) * T + qnorm(alpha / 2) * sigma * sqrt(T)),
      Upper  = S0 * exp((mu - 0.5 * sigma^2) * T + qnorm(1 - alpha / 2) * sigma * sqrt(T))
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
      Weight   = w * 100,
      HitRatio = hit_ratio,
      MSE      = mse_mid,
      RMSE     = rmse_mid,
      MAPE     = mape_mid,
      NRMSE    = nrmse_mid
    ))
  }
  
  # --- Sequential backtest ---
  n <- nrow(df)
  alpha <- 0.05
  pred_results <- data.frame(
    Date   = df$Date,
    Actual = df$Price,
    Pred   = NA,
    Low    = NA,
    High   = NA
  )
  
  for (i in 2:n) {
    train_prices <- df$Price[1:(i-1)]
    log_ret <- diff(log(train_prices))
    
    mu <- mean(log_ret) * 252
    sigma <- sd(log_ret) * sqrt(252)
    S0 <- tail(train_prices, 1)
    T <- 1 / 252
    
    pred_results$Pred[i] <- S0 * exp((mu - 0.5 * sigma^2) * T)
    pred_results$Low[i]  <- S0 * exp((mu - 0.5 * sigma^2) * T + qnorm(alpha / 2) * sigma * sqrt(T))
    pred_results$High[i] <- S0 * exp((mu - 0.5 * sigma^2) * T + qnorm(1 - alpha / 2) * sigma * sqrt(T))
  }
  
  window_idx <- i:(i + k)
  
  subset_data <- pred_results[window_idx, ]
  
  hits <- sum(subset_data$Actual >= subset_data$Lower &
                subset_data$Actual <= subset_data$Upper, na.rm = TRUE)
  
  n_obs <- sum(!is.na(subset_data$Median))
  
  hit_ratio <- hits / n_obs
  
  mse_mid  <- mean((subset_data$Actual - subset_data$Median)^2, na.rm = TRUE)
  rmse_mid <- sqrt(mse_mid)
  mape_mid <- mean(abs((subset_data$Actual - subset_data$Median) / subset_data$Actual) * 100, na.rm = TRUE)
  nrmse_mid <- rmse_mid / mean(subset_data$Actual, na.rm = TRUE)
  
  
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
write_csv(results, "out2.csv")
print("Fertig! Ergebnisse (inkl. sequential) in out2.csv gespeichert.")