
# TEST RUN ----------------------------------------------------------------

## Setup ----
library(tidyverse)
library(invburreg)

## fit an inverse Burr model ----

## use `wars` data object with {inburreg}

ibm(
  outcome = fat,
  mu = ~ pop + mil + maj + dem + post1950,
  alpha = ~ pop + mil + maj + dem + post1950,
  theta = ~ pop + mil + maj + dem + post1950,
  data = wars,
  its = 2000
) -> ft

## plot the coefficients ----

ft$out |>
  ggplot() +
  aes(
    x = estimate,
    y = term,
    xmin = estimate - 1.96 * std.error,
    xmax = estimate + 1.96 * std.error
  ) +
  geom_pointrange() +
  geom_vline(
    xintercept = 0,
    lty = 2
  ) +
  facet_wrap(~ param)

## plot some outcomes
plot.ibm(
  ft,
  newdata = wars |>
    ungroup() |>
    summarize(
      across(
        c(pop, mil, maj, dem, post1950),
        mean
      ),
      post1950 = 0
    ),
  xmin = min(ydt$fat),
  xmax = max(ydt$fat),
  legend = "< 1950"
)
plot.ibm(
  ft,
  newdata = wars |>
    ungroup() |>
    summarize(
      across(
        c(pop, mil, maj, dem, post1950),
        mean
      ),
      post1950 = 1
    ),
  add = T,
  legend = "> 1950",
  xmin = min(ydt$fat),
  xmax = max(ydt$fat),
)

ll_plot(
  50, .1, 10
)
ll_plot(
  5, 0.1, 5,
  add = T
)
