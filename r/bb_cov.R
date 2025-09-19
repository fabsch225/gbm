library(MASS)
library(ggplot2)
library(dplyr)
library(tidyr)

set.seed(123)

t <- seq(0, 1, length.out = 6)
n <- length(t)
scale_factor <- 0.2
y_vals <- seq(-3, 3, length.out = 200)

# --- Plot 1: Independent Normals ---
df_polygons <- data.frame()
for (i in 1:n) {
  dens <- dnorm(y_vals, mean = 0, sd = 1)
  df_polygons <- rbind(df_polygons, data.frame(
    t = t[i],
    y = y_vals,
    x = t[i] + dens * scale_factor,
    curve = i
  ))
}

single_indep <- rnorm(n)
df_single_indep <- data.frame(t = t, y = single_indep)

p1 <- ggplot() +
  geom_polygon(data = df_polygons, aes(x = x, y = y, group = curve, fill = "blue"),
               color = "blue", size = 0, alpha = 0.6) +
  geom_line(data = df_single_indep, aes(x = t, y = y), color = "red", size = 0.8) +
  labs(title = "Independant realisations of N(0, 1)",
       x = "Time", y = "Value") +
  scale_fill_manual(values = c("blue" = "blue"), guide = "none") +
  theme_minimal()

print(p1)

# --- Plot 2: Proper Brownian Motion ---
Sigma <- outer(t, t, FUN = function(s,u) pmin(s,u))

# Single BM path
bm_path <- mvrnorm(1, mu = rep(0, n), Sigma = Sigma)
df_bm <- data.frame(t = t, y = bm_path)

# Gaussian polygons aligned to **current BM step** (not future)
constant_variance <- 0.2
df_polygons2 <- data.frame()
for (i in 2:n) {
  mean_i <- df_bm$y[i-1]  # current BM value at t[i]
  dens <- dnorm(y_vals, mean = mean_i, sd = sqrt(constant_variance))
  df_polygons2 <- rbind(df_polygons2, data.frame(
    t = t[i],
    y = y_vals,
    x = t[i] + dens * scale_factor,
    curve = i
  ))
}

p2 <- ggplot() +
  geom_polygon(data = df_polygons2, aes(x = x, y = y, group = t, fill = "blue"),
               color = "blue", size = 0, alpha = 0.6) +
  geom_line(data = df_bm, aes(x = t, y = y), color = "red", size = 0.8) +
  labs(title = "Discrete Brownian Motion with distributions",
       x = "Time", y = "Value") +
  scale_fill_manual(values = c("blue" = "blue"), guide = "none") +
  theme_minimal()

print(p2)
