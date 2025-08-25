library(readr)
library(dplyr)
library(lubridate)
library(ggplot2)

dax <- read_csv("data/dax.csv")

dax <- dax %>%
  mutate(
    Date = mdy(Date),
    Price = as.numeric(gsub(",", "", Price)),
    Open  = as.numeric(gsub(",", "", Open)),
    High  = as.numeric(gsub(",", "", High)),
    Low   = as.numeric(gsub(",", "", Low)),
    Vol   = as.numeric(gsub("[^0-9.]", "", Vol.)),
    Change = as.numeric(gsub("%", "", `Change %`)) / 100
  )

ggplot(dax, aes(x = Date, y = Price)) +
  geom_line(color = "blue") +
  labs(title = "DAX Closing Price", y = "Price", x = "Date") +
  theme_minimal()
