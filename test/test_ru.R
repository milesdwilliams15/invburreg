
# TEST RUN ----------------------------------------------------------------

## Setup ----
library(tidyverse)
library(peacesciencer)
library(invburreg)

## Data on war size ----

## Start with a country-pairs panel
create_dyadyears(
  directed = T,
  subset_years = 1816:2007
) |>
  add_gml_mids() |>
  add_democracy() |>
  add_cow_majors() |>
  add_nmc() -> dt

## Collapse it to the year level
dt |>
  filter(gmlmidonset == 1) |>
  filter(hostlev == 5) |>
  group_by(ccode1, year, dispnum) |>
  summarize(
    fat = unique(fatalpre1) %>%
      ifelse(. < 0, 0, .),
    pop = unique(tpop1 * 1e03),
    dem = unique(polity21),
    mil = unique(milper1 * 1e03),
    maj = unique(cowmaj1)
  ) -> cy

cy |>
  group_by(ccode1, dispnum) |>
  mutate(
    styear = min(year)
  ) |>
  filter(styear == year) |>
  group_by(dispnum, year) |>
  summarize(
    n = length(unique(ccode1)),
    across(fat:maj, ~ sum(.x, na.rm = T))
  ) |>
  mutate(
    dem = dem / n
  ) -> yd

## fit an inverse Burr model ----

ibm(
  outcome = fat,
  mu = ~ log(pop) + dem + log(1+mil) + maj,
  alpha = ~ log(pop) + dem + log(1+mil) + maj,
  theta = ~ log(pop) + dem + log(1+mil) + maj,
  data = yd
) -> ft
