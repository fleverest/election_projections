# Projections ballot-type spaces

In this repository I visualise the different Election outcomes visualised directly in the space of ballot-type proportions. For each election type, I generate a lattice in the simplex and map each point to the election outcome. Then, I draw a coloured point in the simplex for each (location, outcome) pair. `tourr` handles the rest.

The visualisations are generated using [`tourr`](https://github.com/ggobi/tourr), an R package for visualising high-dimensional data.

## IRV elections

![IRV with all three possible outcomes illustrated](./IRV_allwinner_region.gif)

![IRV with just one of three outcomes illustrated](./IRV_1winner_region.gif)

## Damjan's two candidates across two regions nonlinear election

![The nonlinear boundary is a saddle within the 4-simplex](./quadratic_decision_boundary.gif)
