library(readr)
library(dplyr)
library(lubridate)
library(ggplot2)
library(Sim.DiffProc)

source("src/cev.R")

dax <- read_csv("data/dax.csv") %>%
  mutate(Date  = mdy(Date),
         Price = as.numeric(gsub(",", "", Price))) %>%
  arrange(Date)

dax <- dax %>%
  mutate(logret = c(NA, diff(log(Price)))) %>%
  filter(!is.na(logret))

dt_daily <- 1/252  # trading days per year

set.seed(123)

nsteps <- 3 * 254
npaths <- 1000

pred = predict_cev(
  train  = dax,
  nsteps = nsteps,
  npaths = npaths,
  dt     = dt_daily,
  alpha  = 0.5,
  printParams = TRUE
)

sim_df <- data.frame(
  Lower  = pred$Lower,
  Upper  = pred$Upper,
  Median = pred$Median
)

sim_df <- sim_df %>%
  mutate(Time = row_number())

library(ggplot2)

ggplot(sim_df, aes(x = Time)) +
  geom_ribbon(aes(ymin = Lower, ymax = Upper, fill = "Confidence Band"), alpha = 0.5) +
  geom_line(aes(y = Median, color = "Median")) +
  labs(title = "CEV Model Simulation: alpha=0.5 Confidence Band",
       y = "Price", color = "Legend", fill = "") +
  scale_color_manual(values = c("Median" = "red")) +
  scale_fill_manual(values = c("Confidence Band" = "grey70")) +
  theme_minimal()
