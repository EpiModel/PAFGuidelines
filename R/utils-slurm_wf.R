#' Create a "slurm workflow" folder using a template
#'
#' @param template_dir path to the template directory
#' @param wf_dir path of the folder to be created
#' @param force should a pre-existing folder at this path be removed
#'   if it exists? (default = TRUE)
#'
#' @return the created folder path (invisible)
slurm_wf_tmpl_dir <- function(template_dir, wf_dir, force = FALSE) {
  if (fs::dir_exists(wf_dir)) {
    if (force) {
      message("Folder: '", wf_dir, "' exists. Erasing it (force == TRUE)")
      fs::dir_delete(wf_dir)
    } else {
      stop("Folder: '", wf_dir, "' exists. To erase it set `force = TRUE`")
    }
  }

  fs::dir_copy(template_dir, wf_dir)
  invisible(wf_dir)
}

#' Execute a function using the `base::do.call` syntax in a "slurm workflow"
#'
#' @param wf_dir path to the "slurm workflow" directory created by
#'   `slurm_wf_tmpl_dir`
#' @param resources a list of the parameters to pass to the `brew` templates
#' @param what function to execute
#' @param args a list of named parameters to pass to `what`
#' @param with_objects a list of named object
#' @param compress compress argument passed to `saveRDS`
slurm_wf_do.call <- function(wf_dir, resources, what, args = list(),
                             with_objects = list(), compress = TRUE) {
  wf_infos <- slurm_wf_infos(wf_dir, resources)

  resources$job_name <- wf_infos$step_name
  resources$afterany <- wf_infos$prev_step

  if (is.null(resources$log_file))
    resources$log_file <- paste0(wf_dir, "/log/%x_%a.out")

  wf_list <- list(what = what, args = args)
  saveRDS(wf_list, file = wf_infos$rds_name, compress = compress)
  saveRDS(with_objects, file = wf_infos$with_name, compress = compress)

  wf_infos$rscript_cmd <- paste0(
    "wf_list <- readRDS(\"", wf_infos$rds_name, "\");",
    "with_objects <- readRDS(\"", wf_infos$with_name, "\");",
    "with( with_objects, {do.call(wf_list$what, args = wf_list$args)} )"
  )

  slurm_wf_brew_templates(wf_dir, resources, wf_infos)
}

#' Execute a function using the `base::Map` syntax in a "slurm workflow"
#'
#' @param wf_dir path to the "slurm workflow" directory created by
#'   `slurm_wf_tmpl_dir`
#' @param resources a list of the parameters to pass to the `brew` templates
#' @param FUN function to execute
#' @param ... named vector of parameters to iterate upon and pass to `FUN`
#' @param MoreArgs a list of named parameters to pass to `FUN`
#' @param with_objects a list of named object
#' @param compress compress argument passed to `saveRDS`
slurm_wf_Map <- function(wf_dir, resources, FUN, ..., MoreArgs = NULL,
                         with_objects = list(), compress = TRUE) {
  wf_infos <- slurm_wf_infos(wf_dir, resources)

  resources$job_name <- wf_infos$step_name
  resources$afterany <- wf_infos$prev_step

  if (is.null(resources$log_file))
    resources$log_file <- paste0(wf_dir, "/log/%x_%a.out")

  dots <- list(...)
  resources$n_array <- length(dots[[1]])

  wf_list <- list(FUN = FUN, dots = dots, MoreArgs = MoreArgs)
  saveRDS(wf_list, file = wf_infos$rds_name, compress = compress)
  saveRDS(with_objects, file = wf_infos$with_name, compress = compress)

  wf_infos$rscript_cmd <- paste0(
    "wf_list <- readRDS(\"", wf_infos$rds_name, "\");",
    "with_objects <- readRDS(\"", wf_infos$with_name, "\");",
    "array_id <- as.numeric(Sys.getenv(\"SLURM_ARRAY_TASK_ID\"));",
    "args_ls <- c(lapply(wf_list$dots, `[[`, array_id), wf_list$MoreArgs);",
    "with( with_objects, {do.call(wf_list$FUN, args = args_ls)} )"
  )

  slurm_wf_brew_templates(wf_dir, resources, wf_infos)
}

slurm_wf_infos <- function(wf_dir, resources) {
  wf_infos <- list(
    job_name = resources$job_name,
    job_dir = fs::path(wf_dir, "job_scripts/"),
    starter_dir = fs::path(wf_dir, "job_starters/"),
    rds_dir = fs::path(wf_dir, "job_rds/"),
    with_dir = fs::path(wf_dir, "job_with/")
  )

  other_jobs <- list.files(wf_infos$job_dir)
  if (length(other_jobs) == 0) {
    max_num <- 0
    wf_infos$job_num <- "001"
  } else {
    max_num <- as.numeric(strsplit(max(other_jobs), "\\.")[[1]][1])
    wf_infos$job_num <- sprintf("%03d", max_num + 1)
  }

  wf_infos$step_name <- paste0(wf_infos$job_name, wf_infos$job_num)

  if (max_num > 0) {
    wf_infos$prev_step <- paste0(wf_infos$job_name, sprintf("%03d", max_num))
  } else {
    wf_infos$prev_step <- NULL
  }

  wf_infos$starter_name <- fs::path(
    wf_infos$starter_dir,
    paste0(wf_infos$job_num, ".sh")
  )
  wf_infos$rds_name <- fs::path(
    wf_infos$rds_dir,
    paste0(wf_infos$job_num, ".rds")
  )
  wf_infos$with_name <- fs::path(
    wf_infos$with_dir,
    paste0(wf_infos$job_num, ".rds")
  )
  wf_infos$job_name <- fs::path(
    wf_infos$job_dir,
    paste0(wf_infos$job_name, ".sh")
  )

  wf_infos
}

slurm_wf_brew_templates <- function(wf_dir, resources, wf_infos) {
  brew::brew(paste0(wf_dir, "/job.tmpl"), wf_infos$job_name)
  brew::brew(paste0(wf_dir, "/starter.tmpl"), wf_infos$starter_name)
}
