source("R/utils-params.R")
pkgload::load_all("../EpiModelHIV-p/")
# orig <- readRDS("out/est/restart.rds")

## Parameters
netstats <- readRDS("out/est/netstats.rds")
epistats <- readRDS("out/est/epistats.rds")
est <- readRDS("out/est/netest.rds")

ncores <- 1
control <- control_msm(
  nsteps = 52 * 70,
  nsims = ncores,
  ncores = ncores,
  verbose = FALSE
)

# param$epi_trackers <- list()

## Simulation
sim <- netsim(est, param, init, control)
df <- as.data.frame(sim)
# saveRDS(df, "out/dftest.rds")
df <- readRDS("out/dftest.rds")

library(tidyverse)
theme_set(theme_light())

glimpse(df)

ggplot(df, aes(x = time, y = incid.ct)) +
  geom_line()

ggplot(df, aes(x = time, y = incid.gc)) +
  geom_line()

df$batch <- 1
df$scenario <- "test"
df_baseline <- df
df_sc <- df

df_res <- d_raw
ql <- 0.025
qm <- 0.5
qh <- 0.975

# do after df table
lst <- df_res %>%
    sum_quants(ql, qm, qh) %>%
    pivot_longer(-scenario) %>%
    separate(name, into = c("name", "quantile"), sep = "_/_") %>%
    pivot_wider(names_from = quantile, values_from = value) %>%
    pull(name) %>%
    unique()

for (ll in lst) {
  if (is.null(fmts[[ll]]) ) {
    print(ll)
  }
}
