# compare_models.R
# Combines out1_processed.csv (CEV) and out2_processed.csv (GBM)
# Produces out_compare.csv with side-by-side metrics

library(readr)
library(dplyr)

# ---- read data ----
cev <- read_csv("out1_processed.csv", show_col_types = FALSE) %>%
  mutate(Model = "CEV")

gbm <- read_csv("out2_processed.csv", show_col_types = FALSE) %>%
  mutate(Model = "GBM")

# ---- join side by side ----
# keep only common columns
common_cols <- intersect(names(cev), names(gbm))
common_cols <- setdiff(common_cols, "Model")  # remove Model (we add labels manually)

joined <- cev %>%
  inner_join(gbm, by = c("Asset", "Weight"), suffix = c("_CEV", "_GBM"))

# ---- reorder columns: group by metric ----
metrics <- c("HitRatio", "RMSE", "MAPE", "NRMSE")

final <- joined %>%
  select(Asset, Weight,
         unlist(lapply(metrics, function(m) c(paste0(m, "_CEV"), paste0(m, "_GBM")))))

# ---- add top header row (CEV/GBM labels) ----
# Construct a 2-row header for LaTeX / CSV
header1 <- c("","", rep(c("CEV","GBM"), length(metrics)))
header2 <- c("Asset","Weight", rep(metrics, each = 2))

# Write out: first header row, then second header row, then data
out_file <- "out_compare.csv"
con <- file(out_file, open = "w", encoding = "UTF-8")
writeLines(paste(header1, collapse=","), con)
writeLines(paste(header2, collapse=","), con)
write.table(final, file=con, sep=",", row.names=FALSE, col.names=FALSE, append=TRUE)
close(con)

print(paste("Comparison table saved to", out_file))
