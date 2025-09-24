library(readr)
library(dplyr)

file_in <- "results/cev_backtest_results.csv"
file_out <- "results/cev_backtest_results_processed.csv"

results <- read_csv(file_in, show_col_types = FALSE)

results <- results %>%
  mutate(Weight = as.character(Weight))

asset_means <- results %>%
  filter(Weight != "Seq") %>%
  group_by(Asset) %>%
  summarise(across(c(HitRatio, MSE, RMSE, MAPE, NRMSE), mean), .groups = "drop") %>%
  mutate(Weight = "mean")  %>%
  mutate(across(where(is.numeric), ~ round(., 3)))

results_round <- results %>%
  mutate(across(where(is.numeric), ~ round(., 3)))

final <- bind_rows(results_round, asset_means) %>%
  arrange(Asset, Weight)

write_csv(final, file_out)

print("Fertig! Ergebnisse in " + file_out + " gespeichert.")
