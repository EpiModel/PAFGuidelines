library(stringr)

# Conversion between variable name and final label
var_labels <- c(
  # Epi
  "ir100"                    = "HIV Incidence Rate (ly)",
  "ir100_gc"                 = "GC Incidence Rate (ly)",
  "ir100_gc_hivpos"          = "GC Incidence Rate - HIV positive (ly)",
  "ir100_gc_hivneg"          = "GC Incidence Rate - HIV negative (ly)",
  "ir100_ct"                 = "CT Incidence Rate (ly)",
  "ir100_ct_hivpos"          = "CT Incidence Rate - HIV positive (ly)",
  "ir100_ct_hivneg"          = "CT Incidence Rate - HIV negative (ly)",

  "cum_incid"                = "HIV Cumulative incidence",
  "cum_incid_gc"             = "GC Cumulative incidence",
  "cum_incid_gc_hivpos"      = "GC Cumulative incidence - HIV positive",
  "cum_incid_gc_hivneg"      = "GC Cumulative incidence - HIV negative",
  "cum_incid_ct"             = "CT Cumulative incidence",
  "cum_incid_ct_hivpos"      = "CT Cumulative incidence - HIV positive",
  "cum_incid_ct_hivneg"      = "CT Cumulative incidence - HIV negative",
  "cum_incid_sti"            = "STI Cumulative incidence",
  "cum_incid_sti_hivpos"     = "STI Cumulative incidence - HIV positive",
  "cum_incid_sti_hivneg"     = "STI Cumulative incidence - HIV negative",

  "nia"                      = "HIV NIA",
  "nia_gc"                   = "GC NIA",
  "nia_gc_hivpos"            = "GC NIA - HIV positive",
  "nia_gc_hivneg"            = "GC NIA - HIV negative",
  "nia_ct"                   = "CT NIA",
  "nia_ct_hivpos"            = "CT NIA - HIV positive",
  "nia_ct_hivneg"            = "CT NIA - HIV negative",

  "pia"                      = "HIV PIA",
  "pct_nia"                  = "Proportion of simulation with HIV PIA > 0",
  "pia_gc"                   = "GC PIA",
  "pia_gc_hivpos"            = "GC PIA - HIV positive",
  "pia_gc_hivneg"            = "GC PIA - HIV negative",
  "pia_ct"                   = "CT PIA",
  "pia_ct_hivpos"            = "CT PIA - HIV positive",
  "pia_ct_hivneg"            = "CT PIA - HIV negative",

  "nnt"                      = "HIV NNT",
  "nnt_hiv_sti"              = "HIV NNT - per STI test",
  "nnt_gc"                   = "GC NNT",
  "nnt_gc_hivpos"            = "GC NNT - HIV positive",
  "nnt_gc_hivneg"            = "GC NNT - HIV negative",
  "nnt_ct"                   = "CT NNT",
  "nnt_ct_hivpos"            = "CT NNT - HIV positive",
  "nnt_ct_hivneg"            = "CT NNT - HIV negative",

  "attributable_per_sti"     = "Attributable HIV infection per 1000 STI",
  "attributable_per_gc"      = "Attributable HIV infection per 1000 GC infections",
  "attributable_per_ct"      = "Attributable HIV infection per 1000 CT infections",

  "test_hiv"                 = "Number of HIV tests",
  "test_hiv_pos"             = "Number of HIV positive tests",

  "test_gc"                  = "Number of GC tests",
  "test_gc_hivpos"           = "Number of GC tests - HIV positive",
  "test_gc_hivneg"           = "Number of GC tests - HIV negative",
  "test_gc_pos"              = "Number of positive GC tests",
  "test_gc_pos_hivpos"       = "Number of positive GC tests - HIV positive",
  "test_gc_pos_hivneg"       = "Number of positive GC tests - HIV negative",

  "test_ct"                  = "Number of CT tests",
  "test_ct_hivpos"           = "Number of CT tests - HIV positive",
  "test_ct_hivneg"           = "Number of CT tests - HIV negative",
  "test_ct_pos"              = "Number of positive CT tests",
  "test_ct_pos_hivpos"       = "Number of positive CT tests - HIV positive",
  "test_ct_pos_hivneg"       = "Number of positive CT tests - HIV negative",

  "test_usti"                = "Number of Urethral tests",
  "test_usti_hivpos"         = "Number of Urethral tests - HIV positive",
  "test_usti_hivneg"         = "Number of Urethral tests - HIV negative",
  "test_usti_pos"            = "Number of Positive Urethral tests",
  "test_usti_pos_hivpos"     = "Number of Positive Urethral tests - HIV positive",
  "test_usti_pos_hivneg"     = "Number of Positive Urethral tests - HIV negative",

  "test_rsti"                = "Number of Rectal tests",
  "test_rsti_hivpos"         = "Number of Rectal tests - HIV positive",
  "test_rsti_hivneg"         = "Number of Rectal tests - HIV negative",
  "test_rsti_pos"            = "Number of Positive Rectal tests",
  "test_rsti_pos_hivpos"     = "Number of Positive Rectal tests - HIV positive",
  "test_rsti_pos_hivneg"     = "Number of Positive Rectal tests - HIV negative",

  "test_sti"                 = "Number of tests",
  "test_sti_hivpos"          = "Number of tests - HIV positive",
  "test_sti_hivneg"          = "Number of tests - HIV negative",
  "test_sti_pos"             = "Number of Positive tests",
  "test_sti_pos_hivpos"      = "Number of Positive tests - HIV positive",
  "test_sti_pos_hivneg"      = "Number of Positive tests - HIV negative",

  "num"                      = "Number of MSM in the network (ly)",
  "infections"               = "Number of HIV infections (ly)",
  "infections_gc"            = "Number of GC infections (ly)",
  "infections_ct"            = "Number of CT infections (ly)",
  "hiv_prev"                 = "HIV Prevalence (ly)",
  "gc_prev"                  = "GC Prevalence (ly)",
  "gc_prev_hivpos"           = "GC Prevalence - HIV positive (ly)",
  "gc_prev_hivneg"           = "GC Prevalence - HIV negative (ly)",
  "ct_prev"                  = "CT Prevalence (ly)",
  "ct_prev_hivpos"           = "CT Prevalence - HIV positive (ly)",
  "ct_prev_hivneg"           = "CT Prevalence - HIV negative (ly)",
  "prep_cov"                 = "PrEP Coverage (ly)",
  "hiv_diag"                 = "HIV+ Diagnosed (ly)",
  "hiv_tx"                   = "HIV+ Treated | Diagnosed (ly)",
  "hiv_supp"                 = "HIV+ Virally Suppressed | Diagnosed (ly)",
  "ly_attributable_per_sti"     = "Attributable HIV infection per 1000 STI (ly)",
  "ly_attributable_per_gc"      = "Attributable HIV infection per 1000 GC infections (ly)",
  "ly_attributable_per_ct"      = "Attributable HIV infection per 1000 CT infections (ly)",

  "cum_incid_diff"           = "HIV Cumulative incidence - Diff",
  "cum_incid_gc_hivpos_diff" = "GC Cumulative incidence - HIV positive - Diff",
  "cum_incid_ct_hivpos_diff" = "CT Cumulative incidence - HIV positive - Diff",
  "cum_incid_gc_hivneg_diff" = "GC Cumulative incidence - HIV negative - Diff",
  "cum_incid_ct_hivneg_diff" = "CT Cumulative incidence - HIV negative - Diff",

  "perc_test_rsti"          = "Proportion of rectal tests",
  "perc_test_sti_pos"       = "Proportion of positive tests",
  "perc_test_sti_hivpos"    = "Proportion of HIV positive STI tests"
)

# Formatters for the variables
fmts <- replicate(length(var_labels), scales::label_number(1))
names(fmts) <- names(var_labels)

format_patterns <- list(
  small_num = list(
    patterns = c("ir100", "attributable_per_sti"),
    fun = scales::label_number(0.01)
  ),
  small_perc = list(
    patterns = "pia",
    fun = scales::label_percent(0.01)
  ),
  perc = list(
    patterns = c("prev", "cov", "hiv_diag", "hiv_tx", "hiv_supp", "perc"),
    fun = scales::label_percent(0.01)
  ),
  # scientific = list(
  #   patterns = "attributable_per_sti",
  #   fun = scales::label_scientific()
  # ),
  default = list(
    patterns = ".*",
    fun = scales::label_number(1)
  )
)

for (nms in names(fmts)) {
  for (fp in format_patterns) {
    if (any(str_detect(nms, fp$patterns))) {
      fmts[[nms]] <- fp$fun
      break()
    }
  }
}

make_ordered_labels <- function(nms, named_labels) {
  ordered_labels <- named_labels[nms]
  ordered_labels <- paste0(seq_along(ordered_labels), "-", ordered_labels)
  names(ordered_labels) <- nms

  ordered_labels
}
