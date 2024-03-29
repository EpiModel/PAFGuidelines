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

  cum_shared <- function(d) {
    d %>%
      summarise(
        cum_incid = sum(incid, na.rm = TRUE),
        cum_incid_gc_hivpos = sum(incid.gc.hivpos, na.rm = TRUE),
        cum_incid_gc_hivneg = sum(incid.gc.hivneg, na.rm = TRUE),
        cum_incid_ct_hivpos = sum(incid.ct.hivpos, na.rm = TRUE),
        cum_incid_ct_hivneg = sum(incid.ct.hivneg, na.rm = TRUE),
        test_hiv = sum(tot.tests, na.rm = TRUE),
        test_hiv_pos = sum(tot.tests - tot.neg.tests, na.rm = TRUE),
        test_ugc_hivpos = sum(ugc.tot.test.hivpos, na.rm = TRUE),
        test_rgc_hivpos = sum(rgc.tot.test.hivpos, na.rm = TRUE),
        test_ugc_pos_hivpos = sum(ugc.pos.test.hivpos, na.rm = TRUE),
        test_rgc_pos_hivpos = sum(rgc.pos.test.hivpos, na.rm = TRUE),
        test_ugc_hivneg = sum(ugc.tot.test.hivneg, na.rm = TRUE),
        test_rgc_hivneg = sum(rgc.tot.test.hivneg, na.rm = TRUE),
        test_ugc_pos_hivneg = sum(ugc.pos.test.hivneg, na.rm = TRUE),
        test_rgc_pos_hivneg = sum(rgc.pos.test.hivneg, na.rm = TRUE),
        test_uct_hivpos = sum(uct.tot.test.hivpos, na.rm = TRUE),
        test_rct_hivpos = sum(rct.tot.test.hivpos, na.rm = TRUE),
        test_uct_pos_hivpos = sum(uct.pos.test.hivpos, na.rm = TRUE),
        test_rct_pos_hivpos = sum(rct.pos.test.hivpos, na.rm = TRUE),
        test_uct_hivneg = sum(uct.tot.test.hivneg, na.rm = TRUE),
        test_rct_hivneg = sum(rct.tot.test.hivneg, na.rm = TRUE),
        test_uct_pos_hivneg = sum(uct.pos.test.hivneg, na.rm = TRUE),
        test_rct_pos_hivneg = sum(rct.pos.test.hivneg, na.rm = TRUE)
      ) %>%
      mutate(
        cum_incid_gc = cum_incid_gc_hivpos + cum_incid_gc_hivneg,
        cum_incid_ct = cum_incid_ct_hivpos + cum_incid_ct_hivneg,
        cum_incid_sti_hivpos = cum_incid_gc_hivpos + cum_incid_ct_hivpos,
        cum_incid_sti_hivneg = cum_incid_gc_hivneg + cum_incid_ct_hivneg,
        cum_incid_sti = cum_incid_sti_hivpos + cum_incid_sti_hivneg,
        test_gc_hivpos = test_ugc_hivpos + test_rgc_hivpos,
        test_gc_pos_hivpos = test_ugc_pos_hivpos + test_rgc_pos_hivpos,
        test_gc_hivneg = test_ugc_hivneg + test_rgc_hivneg,
        test_gc_pos_hivneg = test_ugc_pos_hivneg + test_rgc_pos_hivneg,
        test_gc = test_gc_hivpos + test_gc_hivneg,
        test_gc_pos = test_gc_pos_hivpos + test_gc_pos_hivneg,
        test_ct_hivpos = test_uct_hivpos + test_rct_hivpos,
        test_ct_pos_hivpos = test_uct_pos_hivpos + test_rct_pos_hivpos,
        test_ct_hivneg = test_uct_hivneg + test_rct_hivneg,
        test_ct_pos_hivneg = test_uct_pos_hivneg + test_rct_pos_hivneg,
        test_ct = test_ct_hivpos + test_ct_hivneg,
        test_ct_pos = test_ct_pos_hivpos + test_ct_pos_hivneg,
        test_usti_hivpos = test_ugc_hivpos + test_uct_hivpos,
        test_usti_hivneg = test_ugc_hivneg + test_uct_hivneg,
        test_usti = test_usti_hivpos + test_usti_hivneg,
        test_usti_pos_hivpos = test_ugc_pos_hivpos + test_uct_pos_hivpos,
        test_usti_pos_hivneg = test_ugc_pos_hivneg + test_uct_pos_hivneg,
        test_usti_pos = test_usti_pos_hivpos + test_usti_pos_hivneg,
        test_rsti_hivpos = test_rgc_hivpos + test_rct_hivpos,
        test_rsti_hivneg = test_rgc_hivneg + test_rct_hivneg,
        test_rsti = test_rsti_hivpos + test_rsti_hivneg,
        test_rsti_pos_hivpos = test_rgc_pos_hivpos + test_rct_pos_hivpos,
        test_rsti_pos_hivneg = test_rgc_pos_hivneg + test_rct_pos_hivneg,
        test_rsti_pos = test_rsti_pos_hivpos + test_rsti_pos_hivneg,
        test_sti_hivpos = test_rsti_hivpos + test_usti_hivpos,
        test_sti_hivneg = test_rsti_hivneg + test_usti_hivneg,
        test_sti = test_sti_hivneg + test_sti_hivpos,
        test_sti_pos_hivpos = test_rsti_pos_hivpos + test_usti_pos_hivpos,
        test_sti_pos_hivneg = test_rsti_pos_hivneg + test_usti_pos_hivneg,
        test_sti_pos = test_sti_pos_hivneg + test_sti_pos_hivpos
      )
  }

  dbc <- df_baseline %>%
    filter(time > max(time) - 52 * 10) %>%
    group_by(batch, sim) %>%
    cum_shared() %>%
    ungroup() %>%
    summarise(across(-c(batch, sim), median))

  # Scenarios
  df_ls <- vector(mode = "list", length(scenarios_files))
  df_cur <- 0
  for (fle in scenarios_files) {
    fle_nst <- paste0(stringr::str_sub(fle, 1, -5), "___no_sti_effect.rds")

    df_sc <- readRDS(fle)
    df_nsti <- readRDS(fle_nst)
    df_nst <- df_nsti %>%
      filter(time > max(time) - 52 * 10) %>%
      group_by(batch, sim) %>%
      summarise(
        cum_incid = sum(incid, na.rm = TRUE),
        cum_incid_gc_hivpos = sum(incid.gc.hivpos, na.rm = TRUE),
        cum_incid_gc_hivneg = sum(incid.gc.hivneg, na.rm = TRUE),
        cum_incid_ct_hivpos = sum(incid.ct.hivpos, na.rm = TRUE),
        cum_incid_ct_hivneg = sum(incid.ct.hivneg, na.rm = TRUE)
      ) %>%
      ungroup() %>%
      summarise(across(-c(batch, sim), median))

    # same but only last year
    df_nst2 <- df_nsti %>%
      filter(time > max(time) - 52) %>%
      group_by(batch, sim) %>%
      summarise(
        cum_incid = sum(incid, na.rm = TRUE),
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
      filter(time > max(time) - 52 * 10) %>%
      group_by(scenario, batch, sim) %>%
      cum_shared() %>%
      mutate(
        nia = (dbc$cum_incid - cum_incid),
        nia_positive = ifelse(nia < 0, NA, nia),
        pia = nia / dbc$cum_incid,
        nnt = (test_hiv - dbc$test_hiv) / nia,
        nnt_positive = (test_hiv - dbc$test_hiv) / nia_positive,

        nia_gc_hivpos = (dbc$cum_incid_gc_hivpos - cum_incid_gc_hivpos),
        nia_gc_hivpos_positive = ifelse(nia_gc_hivpos < 0, NA, nia_gc_hivpos),
        pia_gc_hivpos = nia_gc_hivpos / dbc$cum_incid_gc_hivpos,
        nnt_gc_hivpos = (test_gc_hivpos - dbc$test_gc_hivpos) / nia_gc_hivpos,
        nnt_gc_hivpos_positive = (test_gc_hivpos - dbc$test_gc_hivpos) / nia_gc_hivpos_positive,

        nia_gc_hivneg = (dbc$cum_incid_gc_hivneg - cum_incid_gc_hivneg),
        nia_gc_hivneg_positive = ifelse(nia_gc_hivneg < 0, NA, nia_gc_hivneg),
        pia_gc_hivneg = nia_gc_hivneg / dbc$cum_incid_gc_hivneg,
        nnt_gc_hivneg = (test_gc_hivneg - dbc$test_gc_hivneg) / nia_gc_hivneg,
        nnt_gc_hivneg_positive = (test_gc_hivneg - dbc$test_gc_hivneg) / nia_gc_hivneg_positive,

        nia_ct_hivpos = (dbc$cum_incid_ct_hivpos - cum_incid_ct_hivpos),
        nia_ct_hivpos_positive = ifelse(nia_ct_hivpos < 0, NA, nia_ct_hivpos),
        pia_ct_hivpos = nia_ct_hivpos / dbc$cum_incid_ct_hivpos,
        nnt_ct_hivpos = (test_ct_hivpos - dbc$test_ct_hivpos) / nia_ct_hivpos,
        nnt_ct_hivpos_positive = (test_ct_hivpos - dbc$test_ct_hivpos) / nia_ct_hivpos_positive,

        nia_ct_hivneg = (dbc$cum_incid_ct_hivneg - cum_incid_ct_hivneg),
        nia_ct_hivneg_positive = ifelse(nia_ct_hivneg < 0, NA, nia_ct_hivneg),
        pia_ct_hivneg = nia_ct_hivneg / dbc$cum_incid_ct_hivneg,
        nnt_ct_hivneg = (test_ct_hivneg - dbc$test_ct_hivneg) / nia_ct_hivneg,
        nnt_ct_hivneg_positive = (test_ct_hivneg - dbc$test_ct_hivneg) / nia_ct_hivneg_positive,

        nia_gc = nia_gc_hivpos + nia_gc_hivneg,
        nia_gc_positive = ifelse(nia_gc < 0, NA, nia_gc),
        pia_gc = nia_gc /
          (dbc$cum_incid_gc_hivpos + dbc$cum_incid_gc_hivneg),
        nnt_gc = ((test_gc_hivpos + test_gc_hivneg) -
                  (dbc$test_gc_hivpos + dbc$test_gc_hivneg)) /
                  nia_gc,
        nnt_gc_positive = ((test_gc_hivpos + test_gc_hivneg) -
                  (dbc$test_gc_hivpos + dbc$test_gc_hivneg)) /
                  nia_gc_positive,
        nia_ct = nia_ct_hivpos + nia_ct_hivneg,
        nia_ct_positive = ifelse(nia_ct < 0, NA, nia_ct),
        pia_ct = nia_ct /
          (dbc$cum_incid_ct_hivpos + dbc$cum_incid_ct_hivneg),
        nnt_ct = ((test_ct_hivpos + test_ct_hivneg) -
                  (dbc$test_ct_hivpos + dbc$test_ct_hivneg)) /
                  nia_ct,
        nnt_ct_positive = ((test_ct_hivpos + test_ct_hivneg) -
                  (dbc$test_ct_hivpos + dbc$test_ct_hivneg)) /
                  nia_ct_positive,
        nnt_hiv_sti = (
          (test_ct_hivpos + test_ct_hivneg +
           test_gc_hivpos + test_gc_hivneg) -
          (dbc$test_ct_hivpos + dbc$test_ct_hivneg +
           dbc$test_gc_hivpos + dbc$test_gc_hivneg)
         ) / nia,

        nnt_hiv_sti_positive = (
          (test_ct_hivpos + test_ct_hivneg +
           test_gc_hivpos + test_gc_hivneg) -
          (dbc$test_ct_hivpos + dbc$test_ct_hivneg +
           dbc$test_gc_hivpos + dbc$test_gc_hivneg)
         ) / nia_positive,
        cum_incid_diff = cum_incid - df_nst[["cum_incid"]][1],
        cum_incid_gc_hivpos_diff = cum_incid_gc_hivpos - df_nst[["cum_incid_gc_hivpos"]][1],
        cum_incid_gc_hivneg_diff = cum_incid_gc_hivneg - df_nst[["cum_incid_gc_hivneg"]][1],
        cum_incid_ct_hivpos_diff = cum_incid_ct_hivpos - df_nst[["cum_incid_ct_hivpos"]][1],
        cum_incid_ct_hivneg_diff = cum_incid_ct_hivneg - df_nst[["cum_incid_ct_hivneg"]][1],
        attributable_per_sti = cum_incid_diff / cum_incid_sti * 1000,
        attributable_per_gc = cum_incid_diff / cum_incid_gc * 1000,
        attributable_per_ct = cum_incid_diff / cum_incid_ct * 1000,
        perc_test_rsti = test_rsti / test_sti,
        perc_test_sti_pos = test_sti_pos / test_sti,
        perc_test_sti_hivpos = test_sti_hivpos / test_sti
      ) %>%
      ungroup()

    # Outcome at the end (mean over last year)
    df_at <- df_sc %>%
      filter(time > max(time) - 52) %>%
      group_by(scenario, batch, sim) %>%
      summarise(
        num = mean(num, na.rm = TRUE),
        infections = sum(incid, na.rm = TRUE),
        ir100 = mean(incid / s___ALL * 5200, na.rm = TRUE),
        hiv_prev = mean(i___ALL / (i___ALL + s___ALL), na.rm = TRUE),
        hiv_diag = mean(i_dx___ALL / i___ALL, na.rm = TRUE),
        hiv_tx = mean(i_tx___ALL / i_dx___ALL, na.rm = TRUE),
        hiv_supp = mean(i_sup___ALL / i_dx___ALL, na.rm = TRUE),
        prep_cov = mean(s_prep___ALL / s_prep_elig___ALL, na.rm = TRUE),
        infections_gc = sum(incid.gc.hivpos + incid.gc.hivneg, na.rm = TRUE),
        ir100_gc_hivpos = mean(incid.gc.hivpos / gc_s_hivpos___ALL * 5200, na.rm = TRUE),
        ir100_gc_hivneg = mean(incid.gc.hivneg / gc_s_hivneg___ALL * 5200, na.rm = TRUE),
        ir100_gc = mean(incid.gc / (gc_s_hivpos___ALL + gc_s_hivneg___ALL) * 5200, na.rm = TRUE),
        infections_ct = sum(incid.ct.hivpos + incid.ct.hivneg, na.rm = TRUE),
        ir100_ct_hivpos = mean(incid.ct.hivpos / ct_s_hivpos___ALL * 5200, na.rm = TRUE),
        ir100_ct_hivneg = mean(incid.ct.hivneg / ct_s_hivneg___ALL * 5200, na.rm = TRUE),
        ir100_ct = mean(incid.ct / (ct_s_hivpos___ALL + ct_s_hivneg___ALL) * 5200, na.rm = TRUE),
        gc_prev_hivpos = mean(gc_i_hivpos___ALL / (gc_i_hivpos___ALL + gc_s_hivpos___ALL), na.rm = TRUE),
        gc_prev_hivneg = mean(gc_i_hivneg___ALL / (gc_i_hivneg___ALL + gc_s_hivneg___ALL), na.rm = TRUE),
        gc_prev = gc_prev_hivpos + gc_prev_hivneg,
        ct_prev_hivpos = mean(ct_i_hivpos___ALL / (ct_i_hivpos___ALL + ct_s_hivpos___ALL), na.rm = TRUE),
        ct_prev_hivneg = mean(ct_i_hivneg___ALL / (ct_i_hivneg___ALL + ct_s_hivneg___ALL), na.rm = TRUE),
        ct_prev = ct_prev_hivpos + ct_prev_hivneg
      ) %>%
      ungroup()

    df_at2 <- df_sc %>%
      filter(time > max(time) - 52) %>%
      group_by(scenario, batch, sim) %>%
      cum_shared() %>%
      mutate(
        cum_incid_diff = cum_incid - df_nst2[["cum_incid"]][1],
        ly_attributable_per_sti = cum_incid_diff / cum_incid_sti * 1000,
        ly_attributable_per_gc = cum_incid_diff / cum_incid_gc * 1000,
        ly_attributable_per_ct = cum_incid_diff / cum_incid_ct * 1000
      ) %>%
      select(scenario, batch, sim, starts_with("ly_")) %>%
      ungroup()

    # binding of the dfs and formatting
    df_res <- df_cum %>%
      left_join(df_at, by = c("scenario", "batch", "sim")) %>%
      left_join(df_at2, by = c("scenario", "batch", "sim"))

    df_ls[[df_cur]] <- df_res
  }

  df_out <- bind_rows(df_ls)

  if (!is.null(scenarios_order)) {
    df_out <- left_join(df_out, data.frame(scenario = scenarios_order))
  }

  df_out
}

make_table <- function(df_res, ql = 0.025, qm = 0.5, qh = 0.975) {
  # this lines print the df with the variable in the right order
  df_out <- df_res %>%
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
      name = var_labels[name]
    ) %>%
    pivot_wider(names_from = name, values_from = clean_val) %>%
    arrange(scenario)

  df_res <- df_res %>%
    group_by(scenario) %>%
    summarise(
      pct_nia = mean(nia > 0),
      pct_nia_gc_hivpos = mean(nia_gc_hivpos > 0),
      pct_nia_gc_hivpos = mean(nia_gc_hivpos > 0),
      pct_nia_gc_hivneg = mean(nia_gc_hivneg > 0),
      pct_nia_ct_hivpos = mean(nia_ct_hivpos > 0),
      pct_nia_ct_hivneg = mean(nia_ct_hivneg > 0),
      pct_nia_gc = mean(nia_gc > 0),
      pct_nia_ct = mean(nia_ct > 0)
    ) %>%
    mutate(
      pct_nia = scales::label_percent(0.1)(pct_nia),
      pct_nia_gc_hivpos = scales::label_percent(0.1)(pct_nia_gc_hivpos),
      pct_nia_gc_hivneg = scales::label_percent(0.1)(pct_nia_gc_hivneg),
      pct_nia_ct_hivpos = scales::label_percent(0.1)(pct_nia_ct_hivpos),
      pct_nia_ct_hivneg = scales::label_percent(0.1)(pct_nia_ct_hivneg),
      pct_nia_gc = scales::label_percent(0.1)(pct_nia_gc),
      pct_nia_ct = scales::label_percent(0.1)(pct_nia_ct)
    )

  names(df_res) <- c(
    "scenario",
    var_labels[names(df_res)[2:length(names(df_res))]]
  )

  df_out <- left_join(df_out, df_res, scenario = scenario)

  var_labels <- var_labels[var_labels %in% colnames(df_out)]
  df_out <- df_out[, c("scenario", var_labels)] %>%
    select(-starts_with("__ignore__"))

  df_out <- left_join(data.frame(scenario = scenarios), df_out, by = "scenario")

  nms <- names(df_out)
  nms <- grepl("NIA|NNT|PIA", nms)
  df_out[1, nms] <- "-"

  df_out
}


make_cum_dfs <- function(baseline_file, scenarios_files) {
  df_baseline <- readRDS(baseline_file)

  if (!"found_indexes" %in% names(df_baseline)) {
    df_baseline$found_indexes <- 0
    df_baseline$elig_indexes <- 0
    df_baseline$found_partners <- 0
    df_baseline$elig_partners <- 0
  }

  df_base_cum <- df_baseline %>%
    filter(time > max(time) - 52 * 10) %>%
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
    if (!"found_indexes" %in% names(df_sc)) {
      df_sc$found_indexes <- 0
      df_sc$elig_indexes <- 0
      df_sc$found_partners <- 0
      df_sc$elig_partners <- 0
    }

    df_cur <- df_cur + 1

    # outcome cumulated over intervention (10y)
    df_cum <- df_sc %>%
      filter(time > max(time) - 52 * 10) %>%
      group_by(scenario, batch, sim) %>%
      summarise(
        cum_incid = sum(incid, na.rm = TRUE),
        cum_indexes = sum(found_indexes, na.rm = TRUE)
      ) %>%
      mutate(
        nia = (base_cum_incid - cum_incid),
        pia = nia / base_cum_incid,
        # nnt = (cum_indexes - base_cum_indexes) / nia
        nnt = cum_indexes / nia
      ) %>%
      select(-cum_indexes) %>%
      ungroup()

    df_ls[[df_cur]] <- df_cum
  }

  bind_rows(df_ls)
}

make_yearly_outcomes <- function(scenarios_files, scenarios_order = NULL) {
  # Scenarios
  df_ls <- vector(mode = "list", length(scenarios_files))
  df_cur <- 0
  for (fle in scenarios_files) {

    df_sc <- readRDS(fle)
    df_cur <- df_cur + 1

    # outcome cumulated over intervention (10y)
    df_cum <- df_sc %>%
      filter(time > max(time) - 52 * 10) %>%
      mutate(year = (time - 1) %/% 52 - 134) %>% # year of interv (1 to 10)
      group_by(scenario, year, batch, sim) %>%
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
        test_rct_pos_hivneg = sum(rct.pos.test.hivneg, na.rm = TRUE),
        sti_screening_ep_hivpos = sum(sti.screening.ep.hivpos, na.rm = TRUE),
        sti_screening_ep_hivneg = sum(sti.screening.ep.hivneg, na.rm = TRUE),
        sti_screening_ep_prep = sum(sti.screening.ep.prep, na.rm = TRUE),

        ct_tx_sympt_hivpos = sum(ct.tx.sympt.hivpos, na.rm = TRUE),
        ct_tx_sympt_hivneg = sum(ct.tx.sympt.hivneg, na.rm = TRUE),
        ct_tx_asympt_hivpos = sum(ct.tx.asympt.hivpos, na.rm = TRUE),
        ct_tx_asympt_hivneg = sum(ct.tx.asympt.hivneg, na.rm = TRUE),
        ct_tx_prep = sum(ct.tx.prep, na.rm = TRUE),
        gc_tx_sympt_hivpos = sum(gc.tx.sympt.hivpos, na.rm = TRUE),
        gc_tx_sympt_hivneg = sum(gc.tx.sympt.hivneg, na.rm = TRUE),
        gc_tx_asympt_hivpos = sum(gc.tx.asympt.hivpos, na.rm = TRUE),
        gc_tx_asympt_hivneg = sum(gc.tx.asympt.hivneg, na.rm = TRUE),
        gc_tx_prep = sum(gc.tx.prep, na.rm = TRUE)
      ) %>%
      mutate(
        cum_incid_gc         = cum_incid_gc_hivpos + cum_incid_gc_hivneg,
        cum_incid_ct         = cum_incid_ct_hivpos + cum_incid_ct_hivneg,
        cum_incid_sti_hivpos = cum_incid_gc_hivpos + cum_incid_ct_hivpos,
        cum_incid_sti_hivneg = cum_incid_gc_hivneg + cum_incid_ct_hivneg,
        cum_incid_sti        = cum_incid_sti_hivpos + cum_incid_sti_hivneg,

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

        perc_test_rsti       = test_rsti / test_sti,
        perc_test_sti_pos    = test_sti_pos / test_sti,
        perc_test_sti_hivpos = test_sti_hivpos / test_sti,

        sti_screening_ep_tot = sti_screening_ep_hivneg + sti_screening_ep_hivpos
      ) %>%
      ungroup()

    df_ls[[df_cur]] <- df_cum
  }

  df_out <- bind_rows(df_ls)

  if (!is.null(scenarios_order)) {
    df_out <- left_join(df_out, data.frame(scenario = scenarios_order))
  }

  df_out
}

sum_yearly_quants <- function(df, ql = 0.025, qm = 0.5, qh = 0.975) {
  df %>%
    ungroup() %>%
    select(-c(batch, sim)) %>%
    group_by(scenario, year) %>%
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

make_yearly_table <- function(df_res, ql = 0.025, qm = 0.5, qh = 0.975) {
  # this lines print the df with the variable in the right order
  df_res <- df_res %>%
    sum_yearly_quants(ql, qm, qh) %>%
    pivot_longer(-c(scenario, year)) %>%
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
      name = var_labels[name]
    ) %>%
    pivot_wider(names_from = name, values_from = clean_val) %>%
    arrange(scenario)

  var_labels <- var_labels[var_labels %in% colnames(df_res)]
  df_res <- df_res[, c("scenario", "year", var_labels)] %>%
    select(-starts_with("__ignore__"))

  left_join(data.frame(scenario = scenarios), df_res, by = "scenario")
}
