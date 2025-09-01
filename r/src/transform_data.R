# R script: convert German-style dates, numbers, percentages, and volumes

# Load data
data <- read.csv("data/lufthansa.csv", stringsAsFactors = FALSE)

# --- DATE CONVERSION ---
if ("Date" %in% names(data)) {
  data$Date <- as.Date(data$Date, format = "%d.%m.%Y")
  data$Date <- format(data$Date, "%m/%d/%Y")   # keep as string mm/dd/yyyy
}

# --- HELPER FUNCTIONS ---
# Convert German number to English (1.234,56 -> 1234.56)
convert_number <- function(x) {
  ifelse(
    x == "" | is.na(x),
    NA,
    as.numeric(gsub(",", ".", gsub("\\.", "", x)))
  )
}

# Convert German percent to English percent string (0,11% -> 0.11%)
convert_percent <- function(x) {
  ifelse(
    x == "" | is.na(x),
    NA,
    paste0(
      gsub(",", ".", gsub("\\.", "", gsub("%", "", x))),
      "%"
    )
  )
}

# Convert German volume (1,98K -> 1.98K)
convert_volume <- function(x) {
  ifelse(
    x == "" | is.na(x),
    NA,
    gsub(",", ".", x)   # just replace decimal comma with dot, keep K/M/etc.
  )
}

# --- APPLY TO SPECIFIC COLUMNS ---
num_cols <- c("Price", "Open", "High", "Low")
pct_cols <- c("Change %")
vol_cols <- c("Vol.")

for (col in num_cols) {
  if (col %in% names(data)) {
    data[[col]] <- convert_number(data[[col]])
  }
}

for (col in pct_cols) {
  if (col %in% names(data)) {
    data[[col]] <- convert_percent(data[[col]])
  }
}

for (col in vol_cols) {
  if (col %in% names(data)) {
    data[[col]] <- convert_volume(data[[col]])
  }
}

# --- SAVE RESULT ---
write.csv(data, "output.csv", row.names = FALSE)

cat("✅ Transformation complete. Saved as output.csv\n")
