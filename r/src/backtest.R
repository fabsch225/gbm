backtest_split <- function(data, predictor, split_ratio = 0.75, dt = 1/252, alpha = 0.5) {
  n <- nrow(data)
  split_idx <- floor(n * split_ratio)
  train <- data[1:split_idx, ]
  test <- data[(split_idx + 1):n, ]
  
  n_steps <- nrow(test)
  n_paths <- 1000
  
  pred_df <- predictor(train, n_steps, n_paths, dt, alpha)
  pred_df <- pred_df %>%
    mutate(Date = test$Date, Price = test$Price)
  
  # pred_df <- pred_df %>%
  #   filter(if_all(everything(), ~ !is.na(.) & !is.nan(.)))
  #
  # print(pred_df$Price <= pred_df$Upper)
  
  hit_ratio <- mean(pred_df$Price >= pred_df$Lower & pred_df$Price <= pred_df$Upper)
  rmse_mid <- sqrt(mean((pred_df$Price - pred_df$Median)^2))
  mse_mid <- mean((pred_df$Price - pred_df$Median)^2)
  mape_mid <- mean(abs((pred_df$Price - pred_df$Median) / pred_df$Price)) * 100
  nrmse_mid <- rmse_mid / mean(pred_df$Price)
  
  metrics = list(
    hit_ratio = hit_ratio,
    rmse_mid = rmse_mid,
    mse_mid = mse_mid,
    mape_mid = mape_mid,
    nrmse_mid = nrmse_mid 
  )
  
  return(
    list(
      train = train,
      test = test,
      pred_df = pred_df,
      metrics = metrics
    )
  )
}
backtest_sequential <- function(data, predictor, timestep = 252, dt = 1/252, alpha = 0.05) {
  n <- nrow(data)
  
  results <- list()
  pred_rows <- list()
  
  i <- 2 * timestep
  while (i <= n) {
    train <- data[1:(i - timestep), ]
    test <- data[(i - timestep + 1):i, ]
    
    n_steps <- nrow(test)
    n_paths <- 1000
    
    pred_df <- predictor(train, n_steps, n_paths, dt, alpha)
    pred_df <- pred_df %>%
      mutate(Date = test$Date, Price = test$Price)
    
    pred_row <- pred_df %>%
      slice_tail(n = 1) %>%
      transmute(
        Date,
        Actual = Price,
        Median,
        Lower,
        Upper
      )
    
    pred_rows[[length(pred_rows) + 1]] <- pred_row
    
    i <- i + timestep
  }
  
  pred_results <- bind_rows(pred_rows)
  
  hit_ratio <- mean(pred_results$Actual >= pred_results$Lower & pred_results$Actual <= pred_results$Upper, na.rm = TRUE)
  rmse_mid <- sqrt(mean((pred_results$Actual - pred_results$Median)^2, na.rm = TRUE))
  mse_mid <- mean((pred_results$Actual - pred_results$Median)^2, na.rm = TRUE)
  mape_mid <- mean(abs((pred_results$Actual - pred_results$Median) / pred_results$Actual), na.rm = TRUE) * 100
  nrmse_mid <- rmse_mid / mean(pred_results$Actual, na.rm = TRUE)
  
  metrics <- list(
    hit_ratio = hit_ratio,
    rmse_mid = rmse_mid,
    mse_mid = mse_mid,
    mape_mid = mape_mid,
    nrmse_mid = nrmse_mid 
  )
  
  return(
    list(
      pred_results = pred_results,
      metrics = metrics
    )
  )
}

