library(geozoo)
library(tourr)
library(tibble)
library(dplyr)
library(tidyr)

winner <- function(p11, p12, p21, p22) {
    v1 <- p11^2 + p12^2
    v2 <- p21^2 + p22^2
    if (v1 > v2) {
        1
    } else if (v2 > v1) {
        2
    } else {
        0
    }
}

by <- 0.05
simplex_grid <- crossing(
    p11 = seq(0, 1, by = by),
    p12 = seq(0, 1, by = by),
    p21 = seq(0, 1, by = by)
) |>
    filter(p11+p12+p21<1) |>
    mutate(p22 = 1 - p11 - p12 - p21) |>
    rowwise() |>
    mutate(winner = winner(p11, p12, p21, p22))


# Plot the decision boundary

proj <- f_helmert(4) |>
    _[-1,] |>
    t()
simp <- simplex(p=3)

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
colnames(sp) <- paste0("x", 1:3)
sp$winner <- 0
sp$labs <- c("p11", "p12", "p21", "p22")

colnames(simplex_proj) <- c(paste0("x", 1:3), "winner", "labs")
full_simplex <- bind_rows(sp, simplex_proj)


# full_simplex |>
#     select(-winner, -labs) |>
#     mutate(x1=jitter(x1, 2), x2=jitter(x2, 2), x3=jitter(x3, 2)) |>
#     animate_xy(
#         col = full_simplex$winner,
#         labs = full_simplex$labs,
#         edges = as.matrix(simp$edges)
#     )

full_simplex |>
    select(-winner, -labs) |>
    mutate(x1=jitter(x1, 2), x2=jitter(x2, 2), x3=jitter(x3, 2)) |>    
    render_gif(
        grand_tour(),
        display_xy(
            col = full_simplex$winner,
            palette="Viridis",
            obs_labels = full_simplex$labs,
            edges = as.matrix(simp$edges)
        ),
        gif_file="quadratic_decision_boundary.gif",
        frames=500
    )
