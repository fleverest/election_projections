library(geozoo)
library(tourr)
library(tibble)
library(dplyr)
library(tidyr)

winner <- function(p123, p132, p213, p231, p312, p321) {
  # Compute stage 1 tallies
  t1 <- p123 + p132
  t2 <- p213 + p231
  t3 <- p321 + p312
  tallies <- c(t1,t2,t3)
  # If there is a winner, return winner
  if (any(c(tallies) > 0.5)) {
    return(which(tallies > 0.5))
  }
  # Otherwise determine first elimination
  elim <- which.min(tallies)
  if (elim == 1) {
    return(c(2,3)[which.max(c(p123+p213+p231, p132+p312+p321))])
  } else if (elim == 2) {
    return(c(1,3)[which.max(c(p123 + p132 + p213, p231 + p312 + p321))])
  } else if (elim == 3) {
    return(c(1,2)[which.max(c(p123 + p132 + p312, p213 + p231 + p321))])
  }
}

by <- 0.05
simplex_grid <- crossing(
  p123 = seq(0, 1, by = by),
  p132 = seq(0, 1, by = by),
  p213 = seq(0, 1, by = by),
  p231 = seq(0, 1, by = by),
  p312 = seq(0, 1, by = by)
) |>
  filter(p123+p132+p213+p231+p312<1) |>
  mutate(p321 = 1-p123-p132-p213-p231-p312) |>
  rowwise() |>
  mutate(winner = winner(p123, p132, p213, p231, p312, p321))


# Plot the decision boundary

proj <- f_helmert(6) |>
  _[-1,] |>
  t()
simp <- simplex(p=5)

simplex_proj <- simplex_grid |>
  select(-winner) |>
  as.matrix() |>
  (\(x) x %*% proj)() |>
  as.data.frame()
simplex_proj$winner <- simplex_grid |>
  pull(winner)
simplex_proj$labs <- rep(NA, nrow(simplex_proj))

sp <- simp$points |>
  data.frame()
colnames(sp) <- paste0("x", 1:5)
sp$winner <- 0
sp$labs <- c("p123", "p132", "p213", "p231", "p312", "p321")

colnames(simplex_proj) <- c(paste0("x", 1:5), "winner", "labs")
full_simplex <- bind_rows(sp, simplex_proj)


filtered_1wins <- simplex_proj |>
  filter(winner == 1)

simplex_to_plot <- bind_rows(sp, filtered_1wins)

simplex_to_plot |>
  select(-winner, -labs) |>
  mutate(
    x1 = jitter(x1, 2),
    x2 = jitter(x2, 2),
    x3 = jitter(x3, 2),
    x4 = jitter(x4, 2),
    x5 = jitter(x5, 2)
  ) |>
  render_gif(
    grand_tour(),
    display_xy(
      col = simplex_to_plot$winner,
      palette="Viridis",
      obs_labels = simplex_to_plot$labs,
      edges = as.matrix(simp$edges)
    ),
    gif_file="IRV_1winner_region.gif",
    frames=500
  )

simplex_to_plot <- bind_rows(sp, simplex_proj)

simplex_to_plot |>
  select(-winner, -labs) |>
  mutate(
    x1 = jitter(x1, 2),
    x2 = jitter(x2, 2),
    x3 = jitter(x3, 2),
    x4 = jitter(x4, 2),
    x5 = jitter(x5, 2)
  ) |>
  render_gif(
    grand_tour(),
    display_xy(
      col = simplex_to_plot$winner,
      palette="Viridis",
      obs_labels = simplex_to_plot$labs,
      edges = as.matrix(simp$edges)
    ),
    gif_file="IRV_allwinner_region.gif",
    frames=500
)
