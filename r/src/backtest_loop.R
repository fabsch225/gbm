library(readr)
library(dplyr)
library(lubridate)
library(Sim.DiffProc)

source("src/backtest.R")
source("src/cev.R")
source("src/gbm.R")

result_file = "results/cev_backtest_results.csv"
predict <- predict_cev

set.seed(123)

read_price <- function(path) {
  df <- read_csv(path, show_col_types = FALSE, guess_max = 5000)
  date_col <- names(df)[grepl("Date", names(df), ignore.case = TRUE)][1]
  if (is.na(date_col)) date_col <- names(df)[1]
  df[[date_col]] <- trimws(as.character(df[[date_col]]))
  parsed <- parse_date_time(df[[date_col]],
                            orders = c("ymd","mdy","dmy","Y-m-d","d/m/Y","m/d/Y"),
                            truncated = 3)
  df$Date <- as_date(parsed)
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

assets <- list(
  dax       = read_price("data/dax.csv"),
  lufthansa = read_price("data/lufthansa.csv"),
  tlira    = read_price("data/try.csv")
)

results <- data.frame()

weights <- seq(0.7, 0.95, by = 0.05)

results <- data.frame()

for (asset_name in names(assets)) {
  df <- assets[[asset_name]] %>%
    mutate(logret = c(NA, diff(log(Price)))) %>%
    filter(!is.na(logret))

  for (w in weights) {
    backtest = backtest_split(df, predict, split_ratio = w, dt = 1/252, alpha = 0.5)
    metrics = backtest$metrics
    results <- rbind(results, data.frame(
      Asset    = asset_name,
      Weight   = w,
      HitRatio = metrics$hit_ratio,
      MSE      = metrics$mse_mid,
      RMSE     = metrics$rmse_mid,
      MAPE     = metrics$mape_mid,
      NRMSE    = metrics$nrmse_mid
    ))
  }
}

for (asset_name in names(assets)) {
  backtest <- backtest_sequential(assets[[asset_name]], predict, timestep = 20, alpha = 0.75)
  metrics <- backtest$metrics
  results <- rbind(results, data.frame(
    Asset    = asset_name,
    Weight   = "Seq",
    HitRatio = metrics$hit_ratio,
    MSE      = metrics$mse_mid,
    RMSE     = metrics$rmse_mid,
    MAPE     = metrics$mape_mid,
    NRMSE    = metrics$nrmse_mid
  ))
}

write_csv(results, result_file)
print("Fertig! Ergebnisse in " + result_file + " gespeichert.")
