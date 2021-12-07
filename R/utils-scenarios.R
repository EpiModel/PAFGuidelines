scenarios_update_time <- 52 * 140 + 1
d_scenarios <- read.csv("in/scenarios.csv")

## Example scenario data.frame
n_scenarios <- nrow(d_scenarios)
scenarios <- vector(mode = "list", n_scenarios)
names(scenarios) <- paste0(d_scenarios$table, "_", d_scenarios$simno)

for (i in seq_len(n_scenarios)) {
  sc <- list(
    at = scenarios_update_time,
    param = list()
  )
  for (p in 2:ncol(d_scenarios)) {
    sc$param[[ colnames(d_scenarios)[p] ]] <- d_scenarios[i, p]
  }

  scenarios[[i]] <- list(sc)
}

scenarios_no_sti_effect <- scenarios

for (i in seq_len(n_scenarios)) {
  scenarios_no_sti_effect[[i]][[1]][["param"]][["hiv.rgc.rr"]] <- 1
  scenarios_no_sti_effect[[i]][[1]][["param"]][["hiv.ugc.rr"]] <- 1
  scenarios_no_sti_effect[[i]][[1]][["param"]][["hiv.rct.rr"]] <- 1
  scenarios_no_sti_effect[[i]][[1]][["param"]][["hiv.uct.rr"]] <- 1
}

names(scenarios_no_sti_effect) <- paste0(
  names(scenarios_no_sti_effect), "___no_sti_effect"
)
