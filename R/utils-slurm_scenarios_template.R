source("R/utils-slurm_prep_helpers.R") # requires `purrr`
source("R/utils-slurm_wf.R")
test_simulation <- TRUE

# Set slurm parameters ---------------------------------------------------------
sim_per_batch <- 28    # How many simulation per bactch
batch_per_set <- 4     # How many sim_per_batch replications to do per parameter
steps_to_keep <- 20 * 52 # Steps to keep in the output df. If NULL, return sim obj
partition <- "csde"     # On hyak, either ckpt or csde
job_name <- "PAF_bases"
ssh_host <- "hyak_mox"
ssh_dir <- "gscratch/PAFGuidelines/"

# Options passed to slurm_wf
slurm_ressources <- list(
  partition = partition,
  job_name = job_name,
  account = if (partition == "csde") "csde" else "csde-ckpt",
  n_cpus = sim_per_batch,
  memory = 5 * 1e3, # in Mb and PER CPU
  walltime = 15
)

# Set orig, param, init, control -----------------------------------------------
#
lnt <- TRUE # if FALSE: set `require.lnt` to FALSE and adjust ` prep.start.prob`
source("R/utils-params.R", local = TRUE)

orig <- readRDS("out/est/restart.rds")

# run 20 rng years before scenarios
scenario_start_step <- 70 * 52 + 1

control <- control_msm(
  start = 60 * 52 + 1,
  nsteps = 80 * 52, # 60->65 rng; 65->70 calib2; 70->80 scenario
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

# To subset scenarios:
## scenarios <- sens_scenarios


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

slurm_wf_tmpl_dir("inst/slurm_wf/", info$root_dir, force = T)

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
  resources = slurm_ressources,
  FUN = run_netsim_fun,
  sim_num = sim_nums,
  param_proposal = param_proposals,
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
