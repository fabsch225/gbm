library(readr)
library(dplyr)
library(lubridate)
library(ggplot2)


dax <- read_csv("data/try.csv") %>%
  mutate(Date = mdy(Date),
         Price = as.numeric(gsub(",", "", Price))) %>%
  arrange(Date)

ggplot(dax, aes(x = Date, y = Price)) +
  geom_line(color = "blue") +
  labs(title = "DAX Closing Price", y = "Price", x = "Date") +
  theme_minimal()
