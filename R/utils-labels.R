# Conversion between variable name and final label
var_labels <- c(
  # Epi
  "ir100"              = "HIV Incidence Rate",
  "ir100_gc"           = "GC Incidence Rate",
  "ir100_ct"           = "CT Incidence Rate",
  "cum_incid"          = "HIV Cumulative incidence",
  "cum_incid_gc"       = "GC Cumulative incidence",
  "cum_incid_ct"       = "CT Cumulative incidence",
  "nia"                = "HIV NIA",
  "nia_gc"             = "GC NIA",
  "nia_ct"             = "CT NIA",
  "pia"                = "HIV PIA",
  "pia_gc"             = "GC PIA",
  "pia_ct"             = "CT PIA",
  "nnt"                = "HIV NNT",
  "nnt_gc"             = "GC NNT",
  "nnt_ct"             = "CT NNT",
  "nnt_hiv_sti"        = "HIV NNT - per STI test",
  "test_hiv"           = "Number of HIV tests",
  "test_hiv_pos"       = "Number of positive HIV tests",
  "test_gc"            = "Number of GC tests",
  "test_gc_pos"        = "Number of positive GC tests",
  "test_ct"            = "Number of CT tests",
  "test_ct_pos"        = "Number of positive CT tests",
  "test_sti_u"         = "Number of Urethral tests",
  "test_sti_u_pos"     = "Number of Positive Urethral tests",
  "test_sti_r"         = "Number of Rectal tests",
  "test_sti_r_pos"     = "Number of Positive Rectal tests",
  "hiv_prev"           = "HIV Prevalence",
  "gc_prev"            = "GC Prevalence",
  "ct_prev"            = "CT Prevalence",
  "prep_cov"           = "PrEP Coverage",
  "hiv_diag"           = "HIV+ Diagnosed",
  "hiv_tx"             = "HIV+ Treated | Diagnosed",
  "hiv_supp"           = "HIV+ Virally Suppressed | Diagnosed"
)

# Formatters for the variables
fmts <- replicate(length(var_labels), scales::label_number(1))
names(fmts) <- names(var_labels)
fmts[["ir100"]]    <- scales::label_number(0.01)
fmts[["ir100_gc"]] <- scales::label_number(0.01)
fmts[["ir100_ct"]] <- scales::label_number(0.01)
fmts[["pia"]]      <- scales::label_percent(0.1)
fmts[["pia_gc"]]   <- scales::label_percent(0.1)
fmts[["pia_ct"]]   <- scales::label_percent(0.1)
fmts[["hiv_prev"]] <- scales::label_percent(0.1)
fmts[["gc_prev"]]  <- scales::label_percent(0.1)
fmts[["ct_prev"]]  <- scales::label_percent(0.1)
fmts[["prep_cov"]] <- scales::label_percent(0.1)
fmts[["hiv_diag"]] <- scales::label_percent(0.1)
fmts[["hiv_tx"]]   <- scales::label_percent(0.1)
fmts[["hiv_supp"]] <- scales::label_percent(0.1)


make_ordered_labels <- function(nms, named_labels) {
  ordered_labels <- named_labels[nms]
  ordered_labels <- paste0(seq_along(ordered_labels), "-", ordered_labels)
  names(ordered_labels) <- nms

  ordered_labels
}
