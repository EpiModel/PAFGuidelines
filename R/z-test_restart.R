# This pulls in the default `param` object and the trackers
source("R/utils-params.R", local = TRUE)
pkgload::load_all("../EMPAF")
# library(EpiModelHIV)
orig <- readRDS("out/est/restart.rds")

nsteps <- 52 * 10

control <- control_msm(
  start = 125 * 52 + 1,
  nsteps = 125 * 52 + 1 + nsteps, # one year for prep riskhist then nsteps
  nsims = 1,
  ncores = 1,
  save.nwstats = FALSE,
  initialize.FUN = reinit_msm,
  save.clin.hist = FALSE,
  verbose = FALSE,
  raw_output = FALSE
)

# param$stitest.start <- Inf
# param$stitest.elig.model <- "all"
# param$stihighrisktest.cov.method <- "curr"
#
# param$sti.highrisktest.int <- 26
# param$stitest.active.int <- 52
#
# param$partnercutoff <- 1
# param$stihighrisktest.gc.hivneg.coverage <- 0.0
# param$stihighrisktest.ct.hivneg.coverage <- 0.0
# param$stihighrisktest.gc.hivpos.coverage <- 0.0
# param$stihighrisktest.ct.hivpos.coverage <- 0.0
# param$stianntest.cov.method <- "curr"
# param$stianntest.gc.hivneg.coverage <- 0.0
# param$stianntest.ct.hivneg.coverage <- 0.0
# param$stianntest.gc.hivpos.coverage <- 0.0
# param$stianntest.ct.hivpos.coverage <- 0.0
# param$tst.rect.sti.rr.hivneg <- 0.0
# param$tst.rect.sti.rr.hivpos <- 0.0

sim <- netsim(orig, param, init, control)

library(tidyverse)

d_sim <- as_tibble(sim)

d <- d_sim %>%
  filter(time > max(time - 52))

glimpse(d)

d %>%
  select(starts_with("gc."), starts_with("ct."), starts_with("sti.")) %>%
  glimpse()

d %>%
  select(starts_with("s")) %>%
  glimpse()

