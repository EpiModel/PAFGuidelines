library(data.table)

reprocess_all <- TRUE

# One or many job_names
# job_names <- c("k-PAF_sc_sti1", "k-PAF_sc_sti2",
#                "k-PAF_sc_nosti1", "k-PAF_sc_nosti2")
job_names <- c("PAF_sc1", "PAF_sc2", "PAF_sc3", "PAF_sc4", "PAF_sc5", "PAF_sc6")

job_last_n <- NULL # if not NULL, get last N jobs. Otherwise, use job_names

if (!is.null(job_last_n))
  job_names <- tail(readLines("out/remote_jobs/last_jobs"), job_last_n)


needed_trackers <- c(
  "n",
  "i", "i_dx", "i_tx", "i_sup", "linked1m",
  "s", "s_prep", "s_prep_elig",
  "gc_i_hivneg", "gc_s_hivneg", "ct_i_hivneg", "ct_s_hivneg",
  "gc_i_hivpos", "gc_s_hivpos", "ct_i_hivpos", "ct_s_hivpos"
)

needed_pops <- c("ALL", "B", "H", "W")

needed_trackers <- vapply(
  needed_pops,
  function(pop) paste0(needed_trackers, "___", pop),
  needed_trackers
)

needed_cols <- c(
  "sim", "time", "batch", "param_batch", "num",
  "incid", "incid.gc", "incid.ct",
  "incid.gc.hivpos", "incid.gc.hivneg", "incid.ct.hivpos", "incid.ct.hivneg",
  "ir100.gc", "ir100.ct",
  "tot.tests", "tot.neg.tests",
  "sti.screening.ep.hivneg", "sti.screening.ep.hivpos", "sti.screening.ep.prep",
  "gc.tx.sympt.hivpos", "gc.tx.sympt.hivneg",
  "gc.tx.asympt.hivpos", "gc.tx.asympt.hivneg",
  "gc.tx.prep",
  "ct.tx.sympt.hivpos", "ct.tx.sympt.hivneg",
  "ct.tx.asympt.hivpos", "ct.tx.asympt.hivneg",
  "ct.tx.prep",
  "rgc.tot.test.hivpos", "rgc.tot.test.hivneg", "ugc.tot.test.hivpos",
  "ugc.tot.test.hivneg", "rgc.pos.test.hivpos", "rgc.pos.test.hivneg",
  "ugc.pos.test.hivpos", "ugc.pos.test.hivneg", "rct.tot.test.hivpos",
  "rct.tot.test.hivneg", "uct.tot.test.hivpos", "uct.tot.test.hivneg",
  "rct.pos.test.hivpos", "rct.pos.test.hivneg", "uct.pos.test.hivpos",
  "uct.pos.test.hivneg",
  needed_trackers
)

for (job in job_names) {
  infos <- readRDS(fs::path("out/remote_jobs/", job, "job_info.rds"))
  out_dir <- fs::path(infos$paths$local_job_dir, "out")
  # on HPC
  out_dir <- fs::path(infos$paths$local_job_dir, "slurm", "out")


  sim_files <- fs::dir_ls(out_dir, regexp = "\\d*.rds")
  for (fle in sim_files) {
    btch <- as.numeric(stringr::str_extract(fs::path_file(fle), "\\d+"))
    scenario_name <- names(infos$param_proposals)[btch]

    sim_dir <- fs::path("out/parts/scenarios", scenario_name)
    if (!fs::dir_exists(sim_dir)) fs::dir_create(sim_dir, recurse = TRUE)

    part_file <- fs::path(sim_dir, paste0(job, "-", btch, ".rds"))
    if (reprocess_all || !fs::file_exists(part_file)) {
      sim <- readRDS(fle)
      dff <- as.data.table(sim)

      dff[, `:=`(batch = btch, scenario = scenario_name)]

      keep_cols <- intersect(needed_cols, names(dff))
      dff <- dff[, ..keep_cols]


      saveRDS(dff, fs::path(sim_dir, paste0(job, "-", btch, ".rds")))
    }
  }
}

scenario_dir <- "out/parts/scenarios"
if (!fs::dir_exists(scenario_dir))
  fs::dir_create(scenario_dir, recurse = TRUE)

if (!fs::dir_exists("out/scenarios"))
  fs::dir_create("out/scenarios", recurse = TRUE)

scenarios <- fs::dir_ls(scenario_dir)
for (sc in scenarios) {
  elts <- fs::path_split(sc)
  scenario_name <- elts[[1]][length(elts[[1]])]

  file_names <- fs::dir_ls(sc)
  df_ls <- lapply(file_names, readRDS)

  dfs <- rbindlist(df_ls, fill = TRUE)[, scenario := scenario_name]

  saveRDS(dfs, fs::path("out/scenarios", paste0(scenario_name, ".rds")))
}
