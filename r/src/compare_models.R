library(readr)
library(dplyr)

cev_file_in <- "results/cev_backtest_results_processed.csv"
gbm_file_in <- "results/gbm_backtest_results_processed.csv"
file_out <- "results/compare_models.csv"

# ---- read data ----
cev <- read_csv(cev_file_in, show_col_types = FALSE) %>%
  mutate(Model = "CEV")

gbm <- read_csv(gbm_file_in, show_col_types = FALSE) %>%
  mutate(Model = "GBM")

common_cols <- intersect(names(cev), names(gbm))
common_cols <- setdiff(common_cols, "Model")  # remove Model (we add labels manually)

joined <- cev %>%
  inner_join(gbm, by = c("Asset", "Weight"), suffix = c("_CEV", "_GBM"))

metrics <- c("HitRatio", "RMSE", "MAPE", "NRMSE")

final <- joined %>%
  select(Asset, Weight,
         unlist(lapply(metrics, function(m) c(paste0(m, "_CEV"), paste0(m, "_GBM")))))

header1 <- c("","", rep(c("CEV","GBM"), length(metrics)))
header2 <- c("Asset","Weight", rep(metrics, each = 2))

con <- file(file_out, open = "w", encoding = "UTF-8")
writeLines(paste(header1, collapse=","), con)
writeLines(paste(header2, collapse=","), con)
write.table(final, file=con, sep=",", row.names=FALSE, col.names=FALSE, append=TRUE)
close(con)

print(paste("Comparison table saved to", file_out))
