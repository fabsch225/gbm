set.seed(123)

simulate_bN <- function(N, T_max=1, steps=1000) {
  n_points <- N * T_max
  eta <- rnorm(n_points, mean=0, sd=1)
  xi <- c(0, cumsum(eta))
  t_grid <- seq(0, T_max, length.out=steps)
  k <- floor(N * t_grid)
  frac <- N * t_grid - k
  vals <- xi[k+1] + frac * (xi[k+2] - xi[k+1])
  vals <- vals / sqrt(N)
  data.frame(t=t_grid, X=vals)
}

library(ggplot2)
library(gridExtra)

T_max <- 1
N_vals <- c(1,10,50,200)

plots <- lapply(N_vals, function(N) {
  df <- simulate_bN(N, T_max)
  ggplot(df, aes(x=t, y=X)) +
    geom_line(color="steelblue") +
    theme_minimal(base_size=14) +
    labs(title=paste0("N=", N), x="t", y="Pfadwert")
})

grid.arrange(grobs=plots, ncol=2)
