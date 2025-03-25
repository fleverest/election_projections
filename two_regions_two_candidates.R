library(geozoo)
library(tourr)
library(tibble)
library(dplyr)
library(tidyr)
library(purrr)

# Generate Dirichlet sample
rdirichlet <- function(n, alpha) {
    gams <- alpha |>
      map(rgamma, n = n) |>
      do.call(what = cbind)
    return(gams / rowSums(gams))
  }  

social_choice <- function(p, difference = FALSE) {
    v1 <- p[1]^2 + p[2]^2
    v2 <- p[3]^2 + p[4]^2
    if (difference) {
        v1 - v2
    } else {
        if (v1 > v2) {
            1
        } else if (v2 > v1) {
            2
        } else {
            0
        }
    }
}


# Plot the decision boundary
proj <- f_helmert(4) |>
    _[-1,] |>
    t()

n_points <- 10000

simplex_points <- rdirichlet(n_points, c(1,1,1,1))
simplex_proj <- as_tibble(simplex_points %*% proj)
simplex_proj$winner <- simplex_points |>
  apply(1L, social_choice, difference = FALSE)
simplex_proj$difference <- simplex_points |>
  apply(1L, social_choice, difference = TRUE)
simplex_proj$labs <- rep(NA_character_, n_points)

simp <- simplex(p=3)
sp <- simp$points |>
    data.frame()
colnames(sp) <- paste0("x", 1:3)
sp$winner <- 0
sp$difference <- 0
sp$labs <- c("p[11]", "p[12]", "p[21]", "p[22]")

colnames(simplex_proj) <- c(paste0("x", 1:3), "winner", "difference", "labs")
full_simplex <- bind_rows(
    sp,
    simplex_proj |>
    filter(abs(difference) < 0.01)
)

# full_simplex |>
#     select(-winner, -labs) |>
#     mutate(x1=jitter(x1, 2), x2=jitter(x2, 2), x3=jitter(x3, 2)) |>
#     animate_xy(
#         col = full_simplex$winner,
#         labs = full_simplex$labs,
#         edges = as.matrix(simp$edges)
#     )

full_simplex |>
    select(-winner, -difference, -labs) |>
    render_gif(
        grand_tour(),
        display_xy(
#            col = full_simplex$winner,
            palette="Viridis",
            obs_labels = full_simplex$labs,
            edges = as.matrix(simp$edges)
        ),
        gif_file="quadratic_decision_boundary.gif",
        frames=500
    )
