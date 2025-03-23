library(geozoo)
library(tourr)
library(tibble)
library(dplyr)
library(tidyr)
library(purrr)

# Suppress summarise info
options(dplyr.summarise.inform = FALSE)

# Generate Dirichlet sample
rdirichlet <- function(n, alpha) {
  gams <- alpha |>
    map(rgamma, n = n) |>
    do.call(what = cbind)
  return(gams / rowSums(gams))
}

# From https://stackoverflow.com/a/20199902
# Produces lexicographical permutations
permutations <- function(n){
  if(n==1){
      return(matrix(1))
  } else {
      sp <- permutations(n-1)
      p <- nrow(sp)
      A <- matrix(nrow=n*p,ncol=n)
      for(i in 1:n){
          A[(i-1)*p+1:p,] <- cbind(i,sp+(sp>=i))
      }
      return(A)
  }
}

irv_recursive <- function(p, n_cand, tol = 1e-5) {
  # We assume p is a list with n_cand! elements, and the order of the ranked
  # ballots is lexicographic, e.g., for n_cand=3:
  # 123, 132, 213, 231, 312, 321.
  assertthat::is.count(n_cand)
  assertthat::assert_that(is.numeric(p))
  assertthat::are_equal(length(p), factorial(n_cand))

  if (n_cand == 2L) {
    # Return index of winner
    if (sum(max(p) - p < tol) > 1L) {
      # return -1 if there is a draw
      return(-1L)
    } else {
      return(which.max(p))
    }
  }

  df <- permutations(n_cand) |>
    cbind(p) |>
    as_tibble() |>
    suppressWarnings()

  # Compute tallies
  tallies <- df |>
    group_by(V1) |>
    summarise(p = sum(p))

  # Determine eliminated candidate(s)
  eliminated <- tallies$p - min(tallies$p) < tol

  # Loop over cases for ties to check if the order matters for the outcome.
  winner <- integer(length = n_cand)
  for (i in seq_along(eliminated)) {
    if (eliminated[i]) {
      # Distribute preferences from eliminated candidate
      
      new_p <- df |>
        apply(1L, \(x) x[-which(x == i)[1L]]) |>
        t() |>
        as_tibble() |>
        rename(p = paste0("V", n_cand)) |>
        group_by(across(-p)) |>
        summarise(p = sum(p)) |>
        pull(p)

      winner[i] <- irv_recursive(new_p, n_cand - 1L, tol = tol)
      if (winner[i] == -1L) {
        # Return -1L if there is possibility for a draw
        return(-1L)
      }
      if (winner[i] >= i) {
        # Reindex from smaller sub-index
        winner[i] <- winner[i] + 1L
      }
    }
  }
  winner <- winner[winner != 0L]
  if (length(unique(winner)) > 1L) {
    # Outcome depends on order of eliminating ties. Declare no winner.
    return(-1L)
  } else {
    # Otherwise return the only winner.
    return(unique(winner))
  }
}

# Plot the decision boundary
proj <- f_helmert(6) |>
  _[-1,] |>
  t()

simplex_points <- rdirichlet(1000, c(1,1,1,1,1,1))
simplex_proj <- as_tibble(simplex_points %*% proj)
simplex_proj$winner <- simplex_points |>
  apply(1L, irv_recursive, n_cand = 3)
simplex_proj$labs <- rep(NA, 1000)

simp <- simplex(p=5)
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
