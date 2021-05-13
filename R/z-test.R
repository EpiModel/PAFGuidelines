nsims <- ncores <- 1
lnt <- TRUE
source("R/utils-params.R")
# orig <- readRDS("out/est/restart.rds")

## Parameters
netstats <- readRDS("out/est/netstats.rds")
epistats <- readRDS("out/est/epistats.rds")
est <- readRDS("out/est/netest.rds")

control <- control_msm(
  nsteps = 52 * 30,
  nsims = ncores,
  ncores = ncores,
  verbose = FALSE
)

## Simulation
sim <- netsim(est, param, init, control)
df <- as.data.frame(sim)
# saveRDS(df, "out/dftest.rds")
# df <- readRDS("out/dftest.rds")

library(tidyverse)
theme_set(theme_light())


ggplot(df, aes(x = time, y = ir100.ct)) +
  geom_smooth()
