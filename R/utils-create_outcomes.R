library(dplyr)
library(tidyr)
source("R/utils-labels.R")

sum_quants <- function(df, ql = 0.025, qm = 0.5, qh = 0.975) {
  df %>%
    ungroup() %>%
    select(-c(batch, sim)) %>%
    group_by(scenario) %>%
    summarise(across(
      everything(),
      list(
        l = ~ quantile(.x, ql, na.rm = TRUE),
        m = ~ quantile(.x, qm, na.rm = TRUE),
        h = ~ quantile(.x, qh, na.rm = TRUE)
      ),
      .names = "{.col}_/_{.fn}"
    )) %>%
  ungroup()
}

make_outcomes <- function(baseline_file, scenarios_files,
                          scenarios_order = NULL) {
  # Calculate baseline elements
  df_baseline <- readRDS(baseline_file)

  df_base_cum <- df_baseline %>%
    filter(time >= max(time) - 52 * 10) %>%
    group_by(batch, sim) %>%
    summarise(
      cum_incid    = sum(incid, na.rm = TRUE),
      cum_incid_gc_hivpos = sum(incid.gc.hivpos, na.rm = TRUE),
      cum_incid_gc_hivneg = sum(incid.gc.hivneg, na.rm = TRUE),
      cum_incid_ct_hivpos = sum(incid.ct.hivpos, na.rm = TRUE),
      cum_incid_ct_hivneg = sum(incid.ct.hivneg, na.rm = TRUE)
    ) %>%
    ungroup() %>%
    summarise(across(-c(batch, sim), median))

    base_cum_incid    <- df_base_cum$cum_incid
    base_cum_incid_gc_hivpos <- df_base_cum$cum_incid_gc_hivpos
    base_cum_incid_gc_hivneg <- df_base_cum$cum_incid_gc_hivneg
    base_cum_incid_ct_hivpos <- df_base_cum$cum_incid_ct_hivpos
    base_cum_incid_ct_hivneg <- df_base_cum$cum_incid_ct_hivneg

    # Scenarios
    df_ls <- vector(mode = "list", length(scenarios_files))
    df_cur <- 0
    for (fle in scenarios_files) {
      fle_nst <- paste0(stringr::str_sub(fle, 1, -5), "___no_sti_effect.rds")

      df_sc <- readRDS(fle)
      df_nst <- readRDS(fle_nst)
      df_nst <- df_nst %>%
        filter(time >= max(time) - 52 * 10) %>%
        group_by(batch, sim) %>%
        summarise(
          cum_incid           = sum(incid, na.rm = TRUE),
          cum_incid_gc_hivpos = sum(incid.gc.hivpos, na.rm = TRUE),
          cum_incid_gc_hivneg = sum(incid.gc.hivneg, na.rm = TRUE),
          cum_incid_ct_hivpos = sum(incid.ct.hivpos, na.rm = TRUE),
          cum_incid_ct_hivneg = sum(incid.ct.hivneg, na.rm = TRUE)
        ) %>%
        ungroup() %>%
        summarise(across(-c(batch, sim), median))

      df_cur <- df_cur + 1

      # outcome cumulated over intervention (10y)
      df_cum <- df_sc %>%
        filter(time >= max(time) - 52 * 10) %>%
        group_by(scenario, batch, sim) %>%
        summarise(
          cum_incid           = sum(incid, na.rm = TRUE),
          cum_incid_gc_hivpos = sum(incid.gc.hivpos, na.rm = TRUE),
          cum_incid_gc_hivneg = sum(incid.gc.hivneg, na.rm = TRUE),
          cum_incid_ct_hivpos = sum(incid.ct.hivpos, na.rm = TRUE),
          cum_incid_ct_hivneg = sum(incid.ct.hivneg, na.rm = TRUE),

          test_hiv            = sum(tot.tests, na.rm = TRUE),
          test_hiv_pos        = sum(tot.tests - tot.neg.tests, na.rm = TRUE),

          test_ugc_hivpos     = sum(ugc.tot.test.hivpos, na.rm = TRUE),
          test_rgc_hivpos     = sum(rgc.tot.test.hivpos, na.rm = TRUE),
          test_ugc_pos_hivpos = sum(ugc.pos.test.hivpos, na.rm = TRUE),
          test_rgc_pos_hivpos = sum(rgc.pos.test.hivpos, na.rm = TRUE),

          test_ugc_hivneg     = sum(ugc.tot.test.hivneg, na.rm = TRUE),
          test_rgc_hivneg     = sum(rgc.tot.test.hivneg, na.rm = TRUE),
          test_ugc_pos_hivneg = sum(ugc.pos.test.hivneg, na.rm = TRUE),
          test_rgc_pos_hivneg = sum(rgc.pos.test.hivneg, na.rm = TRUE),

          test_uct_hivpos     = sum(uct.tot.test.hivpos, na.rm = TRUE),
          test_rct_hivpos     = sum(rct.tot.test.hivpos, na.rm = TRUE),
          test_uct_pos_hivpos = sum(uct.pos.test.hivpos, na.rm = TRUE),
          test_rct_pos_hivpos = sum(rct.pos.test.hivpos, na.rm = TRUE),

          test_uct_hivneg     = sum(uct.tot.test.hivneg, na.rm = TRUE),
          test_rct_hivneg     = sum(rct.tot.test.hivneg, na.rm = TRUE),
          test_uct_pos_hivneg = sum(uct.pos.test.hivneg, na.rm = TRUE),
          test_rct_pos_hivneg = sum(rct.pos.test.hivneg, na.rm = TRUE)
        ) %>%
        mutate(
          cum_incid_gc  = cum_incid_gc_hivpos + cum_incid_gc_hivneg,
          cum_incid_ct  = cum_incid_ct_hivpos + cum_incid_ct_hivneg,

          cum_incid_sti_hivpos = cum_incid_gc_hivpos + cum_incid_ct_hivpos,
          cum_incid_sti_hivneg = cum_incid_gc_hivneg + cum_incid_ct_hivneg,
          cum_incid_sti        = cum_incid_sti_hivpos + cum_incid_sti_hivpos,

          test_gc_hivpos     = test_ugc_hivpos + test_rgc_hivpos,
          test_gc_pos_hivpos = test_ugc_pos_hivpos + test_rgc_pos_hivpos,
          test_gc_hivneg     = test_ugc_hivneg + test_rgc_hivneg,
          test_gc_pos_hivneg = test_ugc_pos_hivneg + test_rgc_pos_hivneg,
          test_gc            = test_gc_hivpos + test_gc_hivneg,
          test_gc_pos        = test_gc_pos_hivpos + test_gc_pos_hivneg,

          test_ct_hivpos     = test_uct_hivpos + test_rct_hivpos,
          test_ct_pos_hivpos = test_uct_pos_hivpos + test_rct_pos_hivpos,
          test_ct_hivneg     = test_uct_hivneg + test_rct_hivneg,
          test_ct_pos_hivneg = test_uct_pos_hivneg + test_rct_pos_hivneg,
          test_ct            = test_ct_hivpos + test_ct_hivneg,
          test_ct_pos        = test_ct_pos_hivpos + test_ct_pos_hivneg,

          test_usti_hivpos     = test_ugc_hivpos + test_uct_hivpos,
          test_usti_hivneg     = test_ugc_hivneg + test_uct_hivneg,
          test_usti            = test_usti_hivpos + test_usti_hivneg,

          test_usti_pos_hivpos = test_ugc_pos_hivpos + test_uct_pos_hivpos,
          test_usti_pos_hivneg = test_ugc_pos_hivneg + test_uct_pos_hivneg,
          test_usti_pos        = test_usti_pos_hivpos + test_usti_pos_hivneg,

          test_rsti_hivpos     = test_rgc_hivpos + test_rct_hivpos,
          test_rsti_hivneg     = test_rgc_hivneg + test_rct_hivneg,
          test_rsti            = test_rsti_hivpos + test_rsti_hivneg,

          test_rsti_pos_hivpos = test_rgc_pos_hivpos + test_rct_pos_hivpos,
          test_rsti_pos_hivneg = test_rgc_pos_hivneg + test_rct_pos_hivneg,
          test_rsti_pos        = test_rsti_pos_hivpos + test_rsti_pos_hivneg,

          test_sti_hivpos      = test_rsti_hivpos + test_usti_hivpos,
          test_sti_hivneg      = test_rsti_hivneg + test_usti_hivneg,
          test_sti             = test_sti_hivneg + test_sti_hivpos,

          test_sti_pos_hivpos  = test_rsti_pos_hivpos + test_usti_pos_hivpos,
          test_sti_pos_hivneg  = test_rsti_pos_hivneg + test_usti_pos_hivneg,
          test_sti_pos         = test_sti_pos_hivneg + test_sti_pos_hivpos,

          nia = (base_cum_incid - cum_incid),
          pia = nia / base_cum_incid,
          nnt = test_hiv / nia,

          nia_gc_hivpos = (base_cum_incid_gc_hivpos - cum_incid_gc_hivpos),
          pia_gc_hivpos = nia_gc_hivpos  / base_cum_incid_gc_hivpos,
          nnt_gc_hivpos = test_gc_hivpos / nia_gc_hivpos,
          nia_gc_hivneg = (base_cum_incid_gc_hivneg - cum_incid_gc_hivneg),
          pia_gc_hivneg = nia_gc_hivneg  / base_cum_incid_gc_hivneg,
          nnt_gc_hivneg = test_gc_hivneg / nia_gc_hivneg,

          nia_ct_hivpos = (base_cum_incid_ct_hivpos - cum_incid_ct_hivpos),
          pia_ct_hivpos = nia_ct_hivpos  / base_cum_incid_ct_hivpos,
          nnt_ct_hivpos = test_ct_hivpos / nia_ct_hivpos,
          nia_ct_hivneg = (base_cum_incid_ct_hivneg - cum_incid_ct_hivneg),
          pia_ct_hivneg = nia_ct_hivneg  / base_cum_incid_ct_hivneg,
          nnt_ct_hivneg = test_ct_hivneg / nia_ct_hivneg,

          nia_gc = nia_gc_hivpos + nia_gc_hivneg,
          pia_gc = nia_gc /
                   (base_cum_incid_gc_hivpos + base_cum_incid_gc_hivneg),
          nnt_gc = (test_gc_hivpos + test_gc_hivneg) / nia_gc,

          nia_ct = nia_ct_hivpos + nia_ct_hivneg,
          pia_ct = nia_ct /
                   (base_cum_incid_ct_hivpos + base_cum_incid_ct_hivneg),
          nnt_ct = (test_ct_hivpos + test_ct_hivneg) / nia_ct,

          nnt_hiv_sti = ((test_ct_hivpos + test_ct_hivneg) +
                        (test_gc_hivpos + test_gc_hivneg)) / nia,

          cum_incid_diff           =  cum_incid - df_nst[["cum_incid"]][1],
          cum_incid_gc_hivpos_diff =  cum_incid_gc_hivpos - df_nst[["cum_incid_gc_hivpos"]][1],
          cum_incid_gc_hivneg_diff =  cum_incid_gc_hivneg - df_nst[["cum_incid_gc_hivneg"]][1],
          cum_incid_ct_hivpos_diff =  cum_incid_ct_hivpos - df_nst[["cum_incid_ct_hivpos"]][1],
          cum_incid_ct_hivneg_diff =  cum_incid_ct_hivneg - df_nst[["cum_incid_ct_hivneg"]][1],

          attributable_per_sti = cum_incid_diff / cum_incid_sti * 1000,
          perc_test_rsti = test_rsti / test_sti,
          perc_test_sti_pos = test_sti_pos / test_sti,
          perc_test_sti_hivpos = test_sti_hivpos / test_sti
        ) %>%
        ungroup()

      # Outcome at the end (mean over last year)
      df_at <- df_sc %>%
        filter(time >= max(time) - 52) %>%
        group_by(scenario, batch, sim) %>%
        summarise(
          ir100 = mean(incid / s___ALL * 5200, na.rm = TRUE),
          hiv_prev = mean(i___ALL / (i___ALL + s___ALL), na.rm = TRUE),
          hiv_diag = mean(i_dx___ALL / i___ALL, na.rm = TRUE),
          hiv_tx   = mean(i_tx___ALL / i_dx___ALL, na.rm = TRUE),
          hiv_supp = mean(i_sup___ALL / i_dx___ALL, na.rm = TRUE),
          prep_cov = mean(s_prep___ALL / s_prep_elig___ALL, na.rm = TRUE),

          ir100_gc_hivpos = mean(incid.gc.hivpos / gc_s_hivpos___ALL * 5200, na.rm = TRUE),
          ir100_gc_hivneg = mean(incid.gc.hivneg / gc_s_hivneg___ALL * 5200, na.rm = TRUE),
          ir100_gc = ir100_gc_hivpos + ir100_gc_hivneg,

          ir100_ct_hivpos = mean(incid.ct.hivpos / ct_s_hivpos___ALL * 5200, na.rm = TRUE),
          ir100_ct_hivneg = mean(incid.ct.hivneg / ct_s_hivneg___ALL * 5200, na.rm = TRUE),
          ir100_ct = ir100_ct_hivpos + ir100_ct_hivneg,

          gc_prev_hivpos = mean(gc_i_hivpos___ALL / (gc_i_hivpos___ALL + gc_s_hivpos___ALL), na.rm = TRUE),
          gc_prev_hivneg = mean(gc_i_hivneg___ALL / (gc_i_hivneg___ALL + gc_s_hivneg___ALL), na.rm = TRUE),
          gc_prev = gc_prev_hivpos + gc_prev_hivneg,

          ct_prev_hivpos = mean(ct_i_hivpos___ALL / (ct_i_hivpos___ALL + ct_s_hivpos___ALL), na.rm = TRUE),
          ct_prev_hivneg = mean(ct_i_hivneg___ALL / (ct_i_hivneg___ALL + ct_s_hivneg___ALL), na.rm = TRUE),
          ct_prev = ct_prev_hivpos + ct_prev_hivneg
        ) %>%
        ungroup()

      # binding of the dfs and formatting
      df_res <- df_cum %>%
        left_join(df_at, by = c("scenario", "batch", "sim"))

      df_ls[[df_cur]] <- df_res
    }

    df_out <- bind_rows(df_ls)

    if (!is.null(scenarios_order))
      df_out <- left_join(df_out, data.frame(scenario = scenarios_order))

    df_out
}

make_table <- function(df_res, ql = 0.025, qm = 0.5, qh = 0.975) {
   # this lines print the df with the variable in the right order
  df_res <- df_res %>%
    sum_quants(ql, qm, qh) %>%
    pivot_longer(-scenario) %>%
    separate(name, into = c("name", "quantile"), sep = "_/_") %>%
    pivot_wider(names_from = quantile, values_from = value) %>%
    filter(name %in% names(var_labels)) %>%
    mutate(
      clean_val = purrr::pmap_chr(
        list(name, l, m, h),
        ~ paste0(
          fmts[[..1]](..3), " (", fmts[[..1]](..2),
          ", ", fmts[[..1]](..4), ")"
        )
      )
    ) %>%
    select(-c(l, m, h)) %>%
    mutate(
      # scenario = scenarios_labels[scenario],
      name = var_labels[name]
    ) %>%
    pivot_wider(names_from = name, values_from = clean_val) %>%
    arrange(scenario)

  df_res <- df_res[, c("scenario", var_labels)] %>%
    select(-starts_with("__ignore__"))

  left_join(data.frame(scenario = scenarios), df_res, by = "scenario")
}


make_cum_dfs <- function(baseline_file, scenarios_files) {
  df_baseline <- readRDS(baseline_file)

  if (! "found_indexes" %in% names(df_baseline)) {
    df_baseline$found_indexes <- 0
    df_baseline$elig_indexes <- 0
    df_baseline$found_partners <- 0
    df_baseline$elig_partners <- 0
  }

  df_base_cum <- df_baseline %>%
    filter(time >= max(time) - 52 * 10) %>%
    group_by(batch, sim) %>%
    summarise(
      cum_incid = sum(incid, na.rm = TRUE),
      cum_indexes = sum(found_indexes, na.rm = TRUE)
    ) %>%
    ungroup() %>%
    summarise(
      cum_incid = median(cum_incid),
      cum_indexes = median(cum_indexes, na.rm = TRUE)
    )

    base_cum_incid <- df_base_cum$cum_incid
    base_cum_indexes <- df_base_cum$cum_indexes

    # Scenarios
    df_ls <- vector(mode = "list", length(scenarios_files))
    df_cur <- 0
    for (fle in scenarios_files) {
      df_sc <- readRDS(fle)
      if (! "found_indexes" %in% names(df_sc)) {
        df_sc$found_indexes <- 0
        df_sc$elig_indexes <- 0
        df_sc$found_partners <- 0
        df_sc$elig_partners <- 0
      }

      df_cur <- df_cur + 1

      # outcome cumulated over intervention (10y)
      df_cum <- df_sc %>%
        filter(time >= max(time) - 52 * 10) %>%
        group_by(scenario, batch, sim) %>%
        summarise(
          cum_incid = sum(incid, na.rm = TRUE),
          cum_indexes = sum(found_indexes, na.rm = TRUE)
        ) %>%
        mutate(
          nia =  (base_cum_incid - cum_incid),
          pia = nia / base_cum_incid,
          # nnt = (cum_indexes - base_cum_indexes) / nia
          nnt = cum_indexes / nia
        ) %>%
        select(- cum_indexes) %>%
        ungroup()

      df_ls[[df_cur]] <- df_cum
    }

    bind_rows(df_ls)
}
