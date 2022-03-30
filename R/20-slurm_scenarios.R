source("R/utils-slurm_prep_helpers.R") # requires `purrr`
source("R/utils-slurm_wf.R")
test_simulation <- FALSE

set_n <- 5

# Set slurm parameters ---------------------------------------------------------
sim_per_batch <- 40    # How many simulation per bactch
batch_per_set <- ceiling(1000 / sim_per_batch)   # How many sim_per_batch replications to do per parameter
steps_to_keep <- 20 * 52 # Steps to keep in the output df. If NULL, return sim obj
partition <- "ckpt" # "preemptable" #"ckpt"     # On hyak, either ckpt or csde
job_name <- paste0("kPAF_sc", set_n) #"k-PAF_scn2"
ssh_host <- "hyak_klone" # "rsph" # "hyak_klone"
ssh_dir <- "gscratch/PAFGuidelines/" # "projects/PAFGuidelines"

# for rsph: need to remouve "account" in the "slurm/job_scripts/blah.sh"

# Options passed to slurm_wf
slurm_resources <- list(
  partition = partition,
  job_name = job_name,
  account = if (partition == "csde") "csde" else "csde-ckpt",
  n_cpus = sim_per_batch,
  memory = 5 * 1024, # in Mb and PER CPU
  walltime = 60
)

# Set orig, param, init, control -----------------------------------------------
#
lnt <- TRUE # if FALSE: set `require.lnt` to FALSE and adjust ` prep.start.prob`
source("R/utils-params.R", local = TRUE)

orig <- readRDS("out/est/restart.rds")

control <- control_msm(
  start = 125 * 52 + 1,
  nsteps = 145 * 52, # 125->130 rng; 130->135 calib2; 135->145 scenario
  nsims = sim_per_batch,
  ncores = sim_per_batch,
  save.nwstats = FALSE,
  initialize.FUN = reinit_msm,
  save.clin.hist = FALSE,
  verbose = FALSE
)

# Scenarios --------------------------------------------------------------------
# requires <list variables>
source("R/utils-scenarios.R")
borders <- round(seq(1, length(scenarios), length.out = 4))

# To subset scenarios:
if (set_n == 1) {
  scenarios <- scenarios[borders[1]:borders[2]]
} else if (set_n == 2) {
  scenarios <- scenarios[(borders[2] + 1):borders[3]]
} else if (set_n == 3) {
  scenarios <- scenarios[(borders[3] + 1):borders[4]]
} else if (set_n == 4) {
  scenarios <- scenarios_no_sti_effect
  scenarios <- scenarios[borders[1]:borders[2]]
} else if (set_n == 5) {
  scenarios <- scenarios_no_sti_effect
  scenarios <- scenarios[(borders[2] + 1):borders[3]]
} else if (set_n == 6) {
  scenarios <- scenarios_no_sti_effect
  scenarios <- scenarios[(borders[3] + 1):borders[4]]
}

# Automatic --------------------------------------------------------------------
#
param_proposals <- rep(scenarios, batch_per_set)
sim_nums <- seq_along(param_proposals)

# Required directories
paths <- make_job_paths(job_name, ssh_dir, ssh_host)
# Ensure that no job with this name is present
if (fs::dir_exists(paths$local_job_dir))
  stop("Folder: '", paths$local_job_dir,
       "' exists. Change `job_name` or delete the folder")

info <- list()
info$paths <- paths
info$job_name <- job_name
info$ssh_host <- ssh_host
info$root_dir <- fs::path(paths$jobs_dir, job_name, paths$slurm_wf)
info$df_keep <- steps_to_keep
info$param_proposals <- param_proposals

slurm_wf_tmpl_dir("inst/slurm_wf/", info$root_dir, force = TRUE)

if (test_simulation) {
  control_test <- control
  control_test$nsteps <- control$start + 1 * 52
  control_test$nsims <- 1
  control_test$ncores <- 1
  control_test$verbose <- TRUE
  n_sc <- 2 # scenario number

  test_sim <- run_netsim_updaters_fun(
    updaters = param_proposals[[n_sc]],
    sim_num = sim_nums[[n_sc]],
    scenario = names(param_proposals)[n_sc],
    orig = orig, param = param, init = init, control = control_test, info = info
  )
}

slurm_wf_Map(
  info$root_dir,
  resources = slurm_resources,
  FUN = run_netsim_updaters_fun,
  sim_num = sim_nums,
  updaters = param_proposals,
  scenario = names(param_proposals),
  MoreArgs = list(orig = orig, param = param, init = init, control = control,
                  info = info)
)

# Create out dir and save params
fs::dir_create(fs::path(paths$local_out, paths$jobs_dir))
saveRDS(info, fs::path(paths$remote_job_dir, "job_info.rds"))
# move slurm to out and cleanup
fs::file_move(paths$remote_job_dir, fs::path(paths$local_out, paths$jobs_dir))
fs::dir_delete(paths$jobs_dir)


scp_send_script <- c(
  "#!/bin/sh",
  "",
  paste0("ssh ", info$ssh_host, " \"mkdir -p '", info$ssh_host, ":",
         fs::path(paths$ssh_proj, paths$jobs_dir), "'\""),
  paste0("rsync -vr --exclude '", "out/*", "' '",
         paths$local_job_dir, "' '",
         info$ssh_host, ":", fs::path(paths$ssh_proj, paths$jobs_dir, "'"))
  )

scp_get_script <- c(
  "#!/bin/sh",
  "",
  paste0("rsync -vur '",
         info$ssh_host, ":", fs::path(paths$ssh_job_dir, paths$slurm_out),
         "' '", paths$local_job_dir, "'")
)

writeLines(scp_send_script, fs::path(paths$local_job_dir, "send_to_ssh.sh"))
writeLines(scp_get_script, fs::path(paths$local_job_dir, "get_from_ssh.sh"))

write(job_name, file = fs::path(paths$local_out, paths$jobs_dir, "last_jobs"),
      append = TRUE)
