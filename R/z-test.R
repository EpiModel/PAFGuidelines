nsims <- ncores <- 1
lnt <- TRUE
source("R/utils-params.R")
# pkgload::load_all("../EpiModelHIV-p/")
# orig <- readRDS("out/est/restart.rds")

## Parameters
netstats <- readRDS("out/est/netstats.rds")
epistats <- readRDS("out/est/epistats.rds")
est <- readRDS("out/est/netest.rds")

ncores <- 4
control <- control_msm(
  nsteps = 52 * 60,
  nsims = ncores,
  ncores = ncores,
  verbose = FALSE
)

# param$epi_trackers <- list()

## Simulation
sim <- netsim(est, param, init, control)
df <- as.data.frame(sim)
# saveRDS(df, "out/dftest.rds")
# df <- readRDS("out/dftest.rds")

library(tidyverse)
theme_set(theme_light())

df %>%
  mutate(
    ir100 = i___ALL / s___ALL * 5200,
    prev_dx = i_dx___ALL / n___ALL,
    linked1m = linked1m___ALL / i_dx___ALL,
    supp = i_sup___ALL / i_dx___ALL,
    prep = s_prep___ALL / s_prep_elig___ALL,
    ir100_gc = incid.gc / gc_s___ALL * 5200,
    ir100_ct = incid.ct / ct_s___ALL * 5200
  ) %>%
  ggplot(aes(x = time / 52, y = ir100_ct)) +
    # geom_vline(xintercept = 65) +
    geom_smooth()
