# postprocess_backtest.R
# Liest out1.csv ein, rundet auf 3 Nachkommastellen
# und berechnet zusätzlich pro Asset Mittelwert-Zeilen

library(readr)
library(dplyr)

# ---------------- einlesen ----------------
results <- read_csv("out2.csv", show_col_types = FALSE)

# ---------------- sicherstellen: Weight als Character ----------------
results <- results %>%
  mutate(Weight = as.character(Weight))

# ---------------- runden ----------------
results_round <- results %>%
  mutate(across(where(is.numeric), ~ round(., 3)))

# ---------------- Mittelwerte pro Asset ----------------
asset_means <- results_round %>%
  group_by(Asset) %>%
  summarise(across(c(HitRatio, MSE, RMSE, MAPE, NRMSE), mean), .groups = "drop") %>%
  mutate(Weight = "mean")

# gleiche Struktur wie results_round
final <- bind_rows(results_round, asset_means) %>%
  arrange(Asset, Weight)

# ---------------- speichern ----------------
write_csv(final, "out2_processed.csv")

print("Fertig! Ergebnisse in out2_processed.csv gespeichert.")
