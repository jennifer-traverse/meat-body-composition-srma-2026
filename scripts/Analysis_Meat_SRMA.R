# =============================================================================
# Meat Intake and Body Composition: Systematic Review and Meta-Analysis
# Peregoy JA, Fleming RA, Leidy HJ, Fleming SA
# Traverse Science | 2025
#
# This script:
#   1. Reads the analytic dataset produced by Creation_of_Meat_Analytic_Dataset.R
#   2. Applies final recodes and subsets the columns used in analysis
#   3. Runs all pooled (overall) meta-analyses and produces forest plots
#   4. Runs subgroup analyses across pre-specified moderators
#   5. Runs influence diagnostics (leave-one-out + Cook's distance)
#   6. Runs outlier detection (find.outliers)
#   7. Runs publication bias tests (Egger, Begg) and produces funnel plots
#   8. Exports all numeric results to a single Excel workbook
#
# Input:  Meat_Analytic_Dataset.xlsx (same folder as this script)
# Output: output/results/<timestamp>/  (created automatically inside the Github folder)
# =============================================================================


# =============================================================================
# 0. SET WORKING DIRECTORY ####
# =============================================================================

setwd("/Users/jenniferperegoy/Library/CloudStorage/OneDrive-SharedLibraries-TraverseScience,Inc/Internal - Documents/Projects/2024-02 Meat SRMA/Database/Github")

# =============================================================================
# 0. PACKAGE INSTALLATION AND LOADING ####
# =============================================================================

# Helper function: install packages that are not yet installed, then load them
Install_And_Load <- function(packages) {
  not_installed <- packages[!(packages %in% installed.packages()[, "Package"])]
  if (length(not_installed)) {
    install.packages(not_installed, repos = "https://cran.rstudio.com/")
  }
  for (pkg in packages) {
    library(pkg, character.only = TRUE, quietly = TRUE)
  }
}

Install_And_Load(c(
  "RColorBrewer", "tidyverse", "ggplot2", "readxl", "ggfittext",
  "ggpubr", "ggrepel", "ggtext", "ggthemes", "grid", "gridExtra", "gtable",
  "vcd", "cowplot", "ggExtra", "viridis", "stringr", "meta", "metafor",
  "pwr", "writexl", "dplyr", "furniture", "data.table", "psych", "skimr",
  "gtsummary", "kableExtra", "magick", "multcomp", "webshot2", "gmodels",
  "corrplot", "Hmisc", "purrr", "knitr", "flextable", "scales",
  "dmetar", "janitor", "ggsci", "paletteer"
))

# dmetar is not on CRAN; install from GitHub if needed
if (!requireNamespace("remotes", quietly = TRUE)) install.packages("remotes")
if (!requireNamespace("dmetar",  quietly = TRUE)) {
  remotes::install_github("MathiasHarrer/dmetar")
}
library(dmetar)


# =============================================================================
# 1. READ DATA ####
# =============================================================================

# The analytic dataset is produced by Creation_of_Meat_Analytic_Dataset.R.
# Paths are relative to the project root; set your working directory accordingly.
data_MA <- read_excel(
  "data/Meat_Analytic_Dataset.xlsx",
  sheet     = "Sheet1",
  col_names = TRUE
)

# Standardise column names (lower snake_case)
data_MA <- data_MA %>% clean_names()


# =============================================================================
# 2. DATA PREPARATION AND RECODES ####
# =============================================================================

## 2a. Force numeric types for all quantitative columns ####

numeric_cols <- c(
  "correlation", "time", "index",
  # Control group
  "n_ctrl", "value_ctrl", "se_ctrl", "sd_ctrl", "lower_ctrl", "upper_ctrl",
  # Intervention group
  "n_int",  "value_int",  "se_int",  "sd_int",  "lower_int",  "upper_int",
  # Absolute mean difference (between-group)
  "md_bg", "md_var_bg", "md_se_bg", "md_lc_bg", "md_uc_bg",
  # Percent-change mean difference (between-group)
  "pct_change_ctrl", "sd_pct_change_ctrl", "pct_change_int", "sd_pct_change_int",
  "md_pct_bg", "md_pct_var_bg", "md_pct_se_bg", "md_pct_lc_bg", "md_pct_uc_bg",
  # Hedges' g (absolute)
  "hedgesg", "hedgesg_var", "hedgesg_se", "hedgesg_lc", "hedgesg_uc",
  # Hedges' g (percent change)
  "hedgesg_pct", "hedgesg_pct_var", "hedgesg_pct_se", "hedgesg_pct_lc", "hedgesg_pct_uc",
  # Study and dietary characteristics
  "max_intervention_length_days",
  "trt_g_kg_d_protein", "comp_g_kg_d_protein",
  "trt_pcte_protein", "comp_pcte_protein",
  "trt_pcte_carb",    "comp_pcte_carb",
  "trt_pcte_fat",     "comp_pcte_fat",
  "trt_kcal",         "comp_kcal",
  "trt_g_protein",    "comp_g_protein",
  "trt_g_carb",       "comp_g_carb",
  "trt_g_fat",        "comp_g_fat",
  "trt_g_fiber",      "comp_g_fiber",
  "trt_pcte_sum",     "comp_pcte_sum"
)

data_MA[, numeric_cols] <- lapply(data_MA[, numeric_cols], as.numeric)


## 2b. Study-level exclusions from the analytic dataset ####

# Leidy 2007 and Campbell 2010 are companion papers from the same RCT.
# Leidy 2007 is the primary publication; Campbell 2010 contributes only BMD.
# To avoid double-counting in overall analyses, we retain only one row per
# outcome. The paired handling is managed later per outcome in the analysis
# loop; here we remove the pre-obese/obese subgroup rows (used only for a
# sensitivity check that was not carried forward).
#
# Magkos 2022: only the weight-maintenance phase varied in meat intake;
# the weight-loss phase rows are excluded from the primary analysis.
#
# Wells 2003: companion to Haub 2002; excluded for body composition outcomes
# where Haub 2002 is the primary report.

data_MA <- data_MA %>%
  filter(
    # %in% returns NA (not FALSE) when sub_group1 is NA, and dplyr::filter()
    # drops NA rows. Explicit is.na() guards preserve rows where sub_group1 is NA.
    !(citation == "Leidy 2007"  & !is.na(sub_group1) & sub_group1 %in% c("Pre-obese", "Obese")),
    !(citation == "Magkos 2022" & !is.na(sub_group1) & sub_group1 == "Weight loss (0-8 wks)"),
    !(citation == "Wells 2003"  & measure_cleaned %in% c("Body lean mass", "Body weight")),
    !(citation == "Wells 2003"  & measure_cleaned %in% c(
      "Knee extension", "Leg curl", "Chest press", "Leg press", "Arm pull"
    )),
    is.na(sub_group1) | !sub_group1 %in% c("Obese", "Pre-obese"),
    # Leg press and Leg extension excluded from pooled analyses (not part of
    # the primary outcome set; those rows appear only in Haub 2002 and Formica 2020)
    !measure_composite %in% c("Leg press", "Leg extension")
  )


## 2c. Recode energy balance (actual, between-group) ####

# Collapse raw values into clean 4-level and 2-level categorical variables.
# "Actual" energy balance is derived from dietary intake data rather than
# the protocol-intended classification.

data_MA <- data_MA %>%
  mutate(
    actual_EB_subjects = case_when(
      actual_energy_balance_subjects == "WL"       ~ "Weight loss",
      actual_energy_balance_subjects == "WM"       ~ "Weight maintenance",
      actual_energy_balance_subjects == "No intent" ~ "No intent",
      actual_energy_balance_subjects == "WL/WM"    ~ "Phased (WL/WM)",
      TRUE ~ "Other"
    ),
    actual_EB_grp = case_when(
      actual_energy_balance_grp == "Hypercaloric" ~ "Hypercaloric",
      actual_energy_balance_grp == "Hypocaloric"  ~ "Hypocaloric",
      actual_energy_balance_grp == "Isocaloric"   ~ "Isocaloric",
      actual_energy_balance_grp == "No intent"    ~ "No intent",
      TRUE ~ "Other"
    ),
    # Binary version: isocaloric vs. not energy-matched
    actual_EB_grp2 = case_when(
      actual_energy_balance_grp == "Isocaloric"                       ~ "Isocaloric",
      actual_energy_balance_grp %in% c("Hypercaloric", "Hypocaloric") ~ "Not energy matched",
      TRUE ~ "Other"
    )
  )


## 2d. Classify studies by primary meat type ####

# Index numbers map to specific citations (see Table 2 in manuscript).
# beef:  Daly (1), Yamashita (7), Haub (8), Poddar (13), Wells (21),
#        Magkos (26), Formica (28), Sayer (39)
# pork:  Leidy/Campbell (2, 4), Murphy (5)
# mixed: Benassi-Evans (12), Mitchell (16), Griffin (30), Li (36)

data_MA <- data_MA %>%
  mutate(
    meat_type = case_when(
      index %in% c(1, 7, 8, 13, 21, 26, 28, 39) ~ "beef",
      index %in% c(2, 4, 5)                      ~ "pork",
      index %in% c(12, 16, 30, 36)               ~ "mixed"
    ),
    meat_type = factor(meat_type, levels = c("beef", "pork", "mixed"))
  )


## 2e. Subset to columns used in analysis ####

# Keep only columns that are referenced downstream. Columns retained here
# include all effect-size variables, study characteristics used as subgroup
# moderators, and dietary intake variables used in meta-regression.

meta_data <- data_MA %>%
  dplyr::select(all_of(c(
    # Study identifiers and design
    "citation", "index", "study_design", "treatment_type", "comparator_type",
    "comparison", "sub_group1", "group_id_int", "group_id_ctrl",
    # Outcome labels
    "measure_cleaned", "measure_composite", "measure_reported",
    "unit_ctrl", "unit_int", "time", "time_unit",
    # Group-level summary statistics (for forest plot display columns)
    "value_ctrl", "value_int",
    "sd_ctrl", "sd_int", "se_ctrl", "se_int",
    "n_ctrl", "n_int", "n_total",
    "lower_ctrl", "upper_ctrl", "lower_int", "upper_int",
    # Pre-calculated effect sizes: absolute mean difference
    "md_bg", "md_var_bg", "md_se_bg", "md_lc_bg", "md_uc_bg",
    # Pre-calculated effect sizes: percent-change mean difference
    "pct_change_ctrl", "sd_pct_change_ctrl", "pct_change_int", "sd_pct_change_int",
    "md_pct_bg", "md_pct_var_bg", "md_pct_se_bg", "md_pct_lc_bg", "md_pct_uc_bg",
    # Pre-calculated effect sizes: Hedges' g (absolute and percent change)
    "hedgesg", "hedgesg_var", "hedgesg_se", "hedgesg_lc", "hedgesg_uc",
    "hedgesg_pct", "hedgesg_pct_var", "hedgesg_pct_se", "hedgesg_pct_lc", "hedgesg_pct_uc",
    # Subgroup / moderator variables
    "weight_status", "sex3", "obesity", "anyexercise", "resistance",
    "age_status", "population_region", "region",
    "duration", "duration_weeks", "max_intervention_length_days", "mean_baseline_bmi",
    "energy_balance_grp", "energy_balance_subjects",
    "actual_energy_balance_grp", "actual_energy_balance_subjects",
    "actual_EB_subjects", "actual_EB_grp", "actual_EB_grp2",
    "protein_matched", "lean", "lov", "bckgrd_redmeat",
    "trt_manipulated", "comp_manipulated", "trt_protein_srce", "comp_protein_srce",
    "meat_type",
    # Dietary intake variables (meta-regression)
    "trt_g_kg_d_protein", "comp_g_kg_d_protein",
    "trt_pcte_protein", "comp_pcte_protein", "diff_pcte_protein",
    "trt_pcte_carb",    "comp_pcte_carb",    "diff_pcte_carb",
    "trt_pcte_fat",     "comp_pcte_fat",     "diff_pcte_fat",
    "trt_kcal",         "comp_kcal",         "diff_tei",
    "trt_g_protein",    "comp_g_protein",
    "trt_g_carb",       "comp_g_carb",
    "trt_g_fat",        "comp_g_fat",
    "trt_g_fiber",      "comp_g_fiber",
    "trt_pcte_sum",     "comp_pcte_sum",
    # Risk of bias (used in RoB sensitivity analysis)
    "overall_ro_b",
    # Within-study correlation (fixed at 0.85 for all change-score calculations)
    "correlation"
  )))


## 2f. Composite display columns for forest plots ####

# Create a single label combining the group mean and its percent change from
# baseline, shown in the left-side columns of each forest plot.
meta_data$value_pct_int <- paste0(
  ifelse(meta_data$value_int  >= 0, "+", ""),
  round(meta_data$value_int,  2), " (",
  ifelse(meta_data$pct_change_int >= 0, "+", ""),
  round(meta_data$pct_change_int, 1), "%)"
)

meta_data$value_pct_ctrl <- paste0(
  ifelse(meta_data$value_ctrl >= 0, "+", ""),
  round(meta_data$value_ctrl, 2), " (",
  ifelse(meta_data$pct_change_ctrl >= 0, "+", ""),
  round(meta_data$pct_change_ctrl, 1), "%)"
)


# =============================================================================
# 3. DEFINE OUTCOME SUBSETS ####
# =============================================================================

# Count studies per outcome and generate a named list of per-outcome data frames
t_outcomes <- sort(table(meta_data$measure_composite), decreasing = TRUE)

# Subset to outcomes that have at least one non-missing effect size
data_subsets <- lapply(names(t_outcomes), function(m) {
  subset(meta_data, measure_composite == m & (!is.na(md_bg) | !is.na(hedgesg)))
})
names(data_subsets) <- names(t_outcomes)

# Drop outcomes with only one study (cannot pool)
data_subsets <- Filter(function(df) nrow(df) > 1, data_subsets)

# Build a label table: one row per outcome with counts and unit info
data_subsets_labels <- purrr::map_dfr(
  names(data_subsets),
  function(name) {
    df <- data_subsets[[name]]
    tibble::tibble(
      measure_composite  = name,
      n_rows             = nrow(df),
      n_unique_citations = dplyr::n_distinct(df$citation),
      n_measures         = dplyr::n_distinct(df$measure_reported),
      measures_list      = toString(sort(unique(trimws(df$measure_reported)))),
      units_list         = toString(sort(unique(trimws(df$unit_ctrl)))),
      first_measure      = trimws(df$measure_reported[1])
    )
  }
) %>%
  filter(n_rows > 1)

# Assign effect type (MD or SMD) per outcome.
# SMD (Hedges' g) is used for outcomes with heterogeneous units:
#   central adiposity (trunk fat / abdominal fat / VAT composite),
#   bone mineral density (site-specific measures pooled),
#   and thigh muscle variables.
# All other outcomes (reported in consistent units) use MD.

smd_outcomes <- c(
  "Central adiposity",
  "Bone mineral density"
)
data_subsets_labels <- data_subsets_labels %>%
  mutate(effect_type = if_else(measure_composite %in% smd_outcomes, "SMD", "MD"))

# Map each outcome to its corresponding pre-calculated effect size columns
data_subsets_labels <- data_subsets_labels %>%
  mutate(
    te_var = case_when(effect_type == "MD"  ~ "md_bg",
                       effect_type == "SMD" ~ "hedgesg"),
    se_var = case_when(effect_type == "MD"  ~ "md_se_bg",
                       effect_type == "SMD" ~ "hedgesg_se"),
    # Flag composite outcomes that need an extra "Measure" column in the forest plot
    include_extra_col = measure_composite %in% c("Central adiposity", "Bone mineral density")
  )

# Parallel label set for percent-change analyses
pct_data_subsets_labels <- data_subsets_labels %>%
  mutate(
    te_var = case_when(effect_type == "MD"  ~ "md_pct_bg",
                       effect_type == "SMD" ~ "hedgesg_pct"),
    se_var = case_when(effect_type == "MD"  ~ "md_pct_se_bg",
                       effect_type == "SMD" ~ "hedgesg_pct_se"),
    include_extra_col = measure_composite %in% c("Central adiposity", "Bone mineral density")
  )


# =============================================================================
# 4. OUTPUT DIRECTORY SETUP ####
# =============================================================================

# All outputs go into a timestamped folder under output/results/ so that
# successive runs do not overwrite each other.
output_dir             <- file.path("output/results", format(Sys.time(), "%Y%m%d-%H%M"))
output_mainexcel_dir   <- file.path(output_dir, "Model output")
output_mainfor_dir     <- file.path(output_dir, "Main forest plots")
output_subfor_dir      <- file.path(output_dir, "Subgroup forest plots")
output_mainfor_sens_dir <- file.path(output_dir, "Sensitivity main forest plots")
output_subfor_sens_dir  <- file.path(output_dir, "Sensitivity subgroup forest plots")
output_baujat_sens_dir  <- file.path(output_dir, "Baujat plots")
output_funnel_dir       <- file.path(output_dir, "Funnel plots")
output_metareg_dir      <- file.path(output_dir, "Metaregression")

for (d in c(output_dir, output_mainexcel_dir, output_mainfor_dir,
            output_subfor_dir, output_mainfor_sens_dir, output_subfor_sens_dir,
            output_baujat_sens_dir, output_funnel_dir, output_metareg_dir)) {
  dir.create(d, recursive = TRUE, showWarnings = FALSE)
}


# =============================================================================
# 5. HELPER FUNCTIONS ####
# =============================================================================

## safe_round: round a value, returning NA if it cannot be coerced ####
safe_round <- function(x, digits = 2) {
  if (is.null(x) || length(x) == 0 || all(is.na(x))) return(NA_real_)
  x_numeric <- suppressWarnings(as.numeric(x))
  if (length(x_numeric) == 0 || all(is.na(x_numeric))) return(NA_real_)
  round(x_numeric, digits = digits)
}

## get_scalar: extract a single scalar value from a model field ####
get_scalar <- function(x) {
  if (is.null(x) || is.atomic(x)) return(x)
  if (length(x) == 1) return(as.vector(x))
  return(NA)
}

## get_total_n: sum n_total across study arms in a fitted model ####
get_total_n <- function(m) {
  dat <- tryCatch(m$data, error = function(e) NULL)
  if (!is.null(dat) && "n_total" %in% names(dat)) {
    return(sum(dat$n_total, na.rm = TRUE))
  }
  NA_real_
}

## get_scalar_safe: pull scalar with a default fallback ####
get_scalar_safe <- function(x, default = NA) {
  if (is.null(x) || length(x) == 0) return(default)
  if (length(x) > 1) return(x[[1]])
  x
}

## outcome_direction: lookup table for which side favors the intervention ####
outcome_direction <- c(
  "Body weight"                       = "lower",
  "BMI"                               = "lower",
  "Body fat"                          = "lower",
  "Body fat mass"                     = "lower",
  "Waist circumference"               = "lower",
  "Hip circumference"                 = "lower",
  "Central adiposity"                 = "lower",
  "Waist-hip ratio"                   = "lower",
  "Bone mineral density"              = "higher",
  "Thigh muscle cross-sectional area" = "higher",
  "Thigh muscle density"              = "higher",
  "Body lean mass"                    = "higher"
)

## get_favor_labels: assign "Favors Intervention/Control" axis labels ####
# Labels are placed only on the side(s) of zero that the x-axis range covers.
get_favor_labels <- function(outcome_name,
                             xlim,
                             outcome_direction,
                             intervention_label = "Favors Intervention",
                             comparator_label   = "Favors Control") {
  left_lab  <- ""
  right_lab <- ""
  xlab      <- ""

  dir <- outcome_direction[[outcome_name]]
  if (is.null(dir)) {
    warning("Outcome not found in outcome_direction: ", outcome_name)
    return(list(left = "", right = "", xlab = ""))
  }

  crosses_zero <- xlim[1] < 0 && xlim[2] > 0

  if (crosses_zero) {
    if (dir == "lower") {
      left_lab  <- intervention_label
      right_lab <- comparator_label
    } else {
      left_lab  <- comparator_label
      right_lab <- intervention_label
    }
  } else {
    xlab <- intervention_label
  }

  list(left = left_lab, right = right_lab, xlab = xlab)
}

# --- check_multi_es_per_citation: flag outcomes where one study contributes
#     more than one effect size (relevant for clustering decisions) ----------
check_multi_es_per_citation <- function(df, label = NULL, verbose = TRUE) {
  if (!"citation" %in% names(df)) {
    if (verbose) message("No 'citation' column found",
                         if (!is.null(label)) paste0(" for ", label))
    return(FALSE)
  }
  tab   <- dplyr::count(df, citation, name = "k_es")
  multi <- tab$k_es > 1L
  if (any(multi)) {
    if (verbose) {
      message("Multiple ES per Citation detected",
              if (!is.null(label)) paste0(" for ", label, ":"))
      print(tab[multi, ])
    }
    return(TRUE)
  }
  if (verbose) message("One ES per Citation",
                       if (!is.null(label)) paste0(" for ", label))
  return(FALSE)
}

## dbg_log: print a brief diagnostic for each outcome at analysis time ####
dbg_log <- function(outcome, df) {
  cit_counts <- df %>% count(citation, name = "k_effects")
  message("\n--- ", outcome, " ---")
  message("Rows (k): ", nrow(df), " | Unique studies: ", n_distinct(df$citation))
  if (any(cit_counts$k_effects > 1)) {
    multi <- cit_counts %>% filter(k_effects > 1)
    message("Studies with >1 effect size: ",
            paste0(multi$citation, " (", multi$k_effects, ")", collapse = "; "))
  } else {
    message("All studies have 1 effect size.")
  }
}


# =============================================================================
# 6. BAUJAT INFLUENCE PLOT FUNCTION ####
# =============================================================================

# Produces a Baujat-style plot showing each study's contribution to overall
# heterogeneity (x-axis) against its influence on the pooled estimate (y-axis).
# Studies in the upper-right quadrant (high heterogeneity contribution AND
# high influence) are flagged as potential outliers.

baujat_plot <- function(model,
                        output_dir_bauj = NULL,
                        outcome_name    = NULL,
                        file_suffix     = "Baujat",
                        outliers        = NULL,
                        q_cut           = 0.90,
                        label_cex       = 0.75,
                        point_cex       = 1.2,
                        seed_jitter     = 123) {

  N_total          <- get_total_n(model)
  Q_contributions  <- model$w.random * (model$TE.random - model$TE)^2
  influence_values <- abs(model$TE.random - model$TE)

  bj_df <- data.frame(
    x       = Q_contributions,
    y       = influence_values,
    studlab = make.unique(as.character(model$studlab)),
    stringsAsFactors = FALSE
  )

  # Determine which studies to flag as outliers
  if (is.null(outliers)) {
    # Default: studies above the q_cut quantile on both axes simultaneously
    x_thr  <- stats::quantile(bj_df$x, q_cut, na.rm = TRUE)
    y_thr  <- stats::quantile(bj_df$y, q_cut, na.rm = TRUE)
    is_out <- (bj_df$x >= x_thr) & (bj_df$y >= y_thr)
    n_out  <- sum(is_out, na.rm = TRUE)
    out_method <- paste0(
      "Outliers: x \u2265 Q", round(q_cut * 100), " (", signif(x_thr, 3),
      ") & y \u2265 Q", round(q_cut * 100), " (", signif(y_thr, 3),
      "); flagged ", n_out, "/", nrow(bj_df))
  } else if (is.logical(outliers)) {
    is_out     <- outliers
    n_out      <- sum(is_out, na.rm = TRUE)
    out_method <- paste0("Outliers: user-specified; flagged ", n_out, "/", nrow(bj_df))
  } else {
    is_out <- rep(FALSE, nrow(bj_df))
    is_out[outliers] <- TRUE
    n_out      <- sum(is_out, na.rm = TRUE)
    out_method <- paste0("Outliers: user-specified; flagged ", n_out, "/", nrow(bj_df))
  }

  k           <- nrow(bj_df)
  outcome_lbl <- if (!is.null(outcome_name) && nzchar(outcome_name)) outcome_name else "Outcome"
  title_line1 <- paste0("Baujat-style influence plot: ", outcome_lbl)
  title_line2 <- if (is.finite(N_total)) {
    paste0("k = ", k, "; N = ", formatC(N_total, format = "f", digits = 0), "; ", out_method)
  } else {
    paste0("k = ", k, "; ", out_method)
  }
  main_title <- paste(title_line1, title_line2, sep = "\n")

  xlim <- grDevices::extendrange(bj_df$x, f = 0.12)
  ylim <- grDevices::extendrange(bj_df$y, f = 0.12)

  if (!is.null(output_dir_bauj)) {
    png_filename <- file.path(
      output_dir_bauj,
      paste0(outcome_name, "_", file_suffix, ".png")
    )
    message("Saving Baujat plot to: ", png_filename)
    png(filename = png_filename, width = 8, height = 6, units = "in", res = 300)
    on.exit(dev.off(), add = TRUE)
  }

  op <- par(mar = c(4, 4, 5, 8), xpd = NA)
  on.exit(par(op), add = TRUE)

  plot(bj_df$x, bj_df$y,
       xlab = "Contribution to Heterogeneity (approx.)",
       ylab = "Influence on Overall Effect (approx.)",
       xlim = xlim, ylim = ylim,
       pch  = 21, bg = "white",
       col  = ifelse(is_out, "red", "black"),
       cex  = point_cex,
       main = main_title)

  abline(h = 0, v = 0, lty = 2, col = "gray")

  set.seed(seed_jitter)
  xjit <- jitter(rep(0, nrow(bj_df)), amount = diff(range(bj_df$x, na.rm = TRUE)) * 0.02)
  yjit <- jitter(rep(0, nrow(bj_df)), amount = diff(range(bj_df$y, na.rm = TRUE)) * 0.02)

  text(x      = bj_df$x + xjit,
       y      = bj_df$y + yjit,
       labels = bj_df$studlab,
       cex    = label_cex,
       col    = ifelse(is_out, "red", "black"),
       pos    = 4)

  invisible(bj_df)
}


# =============================================================================
# 7. OVERALL (POOLED) META-ANALYSIS FUNCTION ####
# =============================================================================

# run_metagen() fits a random-effects meta-analysis for a single outcome using
# REML estimation, produces a forest plot (auto-trimmed), and returns a summary
# tibble containing pooled estimate, heterogeneity statistics, and prediction
# interval.

run_metagen <- function(data,
                        outcome_name      = "Body fat",
                        effect_type       = "MD",
                        te_var            = "md_bg",
                        se_var            = "md_se_bg",
                        xlim_vals         = NULL,
                        plot_dim_initial  = c(50, 50),
                        smlab_text        = "MD in Delta Body Fat %",
                        file_suffix       = "MD_Fplot",
                        output_dir        = NULL,
                        baujat            = FALSE,
                        output_dir_bauj   = NULL,
                        include_extra_col = FALSE,
                        ...) {

  message("Starting analysis for: ", outcome_name)

  if (is.null(output_dir)) stop("output_dir is NULL. Provide a valid directory.")
  if (nrow(data) < 2) {
    message("Not enough data for: ", outcome_name)
    return(NULL)
  }

  data_for_plot <- data

  # Round numeric display columns for the forest plot
  if ("n_total" %in% names(data_for_plot)) {
    data_for_plot$n_total <- suppressWarnings(
      as.integer(round(as.numeric(data_for_plot$n_total)))
    )
  }
  cols_2dp <- intersect(names(data_for_plot), c("value_ctrl", "value_int"))
  data_for_plot[cols_2dp] <- lapply(data_for_plot[cols_2dp], safe_round, digits = 2)

  pct_cols <- intersect(names(data_for_plot), c("pct_change_ctrl", "pct_change_int"))
  data_for_plot[pct_cols] <- lapply(data_for_plot[pct_cols], function(x) {
    x_num <- suppressWarnings(as.numeric(x))
    ifelse(is.na(x_num), "\u2014", formatC(x_num, format = "f", digits = 1))
  })

  # Shorten display labels for forest plot columns
  if ("lean" %in% names(data_for_plot)) {
    data_for_plot$lean <- dplyr::case_when(
      grepl("^lean", data_for_plot$lean, ignore.case = TRUE)              ~ "Lean",
      grepl("not lean|not specified", data_for_plot$lean, ignore.case = TRUE) ~ "Not lean/NS",
      TRUE ~ data_for_plot$lean
    )
  }
  if ("age_status" %in% names(data_for_plot)) {
    data_for_plot$age_status <- dplyr::case_when(
      grepl("General Pop/Mixed", data_for_plot$age_status) ~ "Mixed",
      TRUE ~ data_for_plot$age_status
    )
  }
  if ("energy_balance_subjects" %in% names(data_for_plot)) {
    data_for_plot$energy_balance_subjects <- dplyr::case_when(
      grepl("loss.*maintenance|maintenance.*loss", data_for_plot$energy_balance_subjects,
            ignore.case = TRUE)                                        ~ "WL/WM",
      grepl("weight\\s*loss",        data_for_plot$energy_balance_subjects,
            ignore.case = TRUE)                                        ~ "WL",
      grepl("weight\\s*maintenance", data_for_plot$energy_balance_subjects,
            ignore.case = TRUE)                                        ~ "Maint",
      grepl("no\\s*intent",          data_for_plot$energy_balance_subjects,
            ignore.case = TRUE)                                        ~ "No intent",
      TRUE ~ data_for_plot$energy_balance_subjects
    )
  }
  if ("protein_matched" %in% names(data_for_plot)) {
    data_for_plot$protein_matched <- dplyr::case_when(
      grepl("protein-matched",          data_for_plot$protein_matched,
            ignore.case = TRUE)         ~ "Matched",
      grepl("high vs\\.\\s*low protein", data_for_plot$protein_matched,
            ignore.case = TRUE)         ~ "High/Low",
      TRUE ~ data_for_plot$protein_matched
    )
  }

  # Fit the random-effects model (REML, Q-profile CI for tau^2)
  metagen_fit <- metagen(
    TE            = data_for_plot[[te_var]],
    seTE          = data_for_plot[[se_var]],
    studlab       = data_for_plot$citation,
    sm            = effect_type,
    random        = TRUE,
    common        = FALSE,
    method.tau       = "REML",
    method.tau.ci    = "QP",
    method.random.ci = "classic",
    label.left    = "Favors Intervention",
    label.right   = "Favors Comparator",
    overall       = TRUE,
    data          = data_for_plot
  )

  # Auto-pad x-axis limits if not specified
  if (is.null(xlim_vals)) {
    ci_vals <- c(min(metagen_fit$lower, na.rm = TRUE),
                 max(metagen_fit$upper, na.rm = TRUE))
    if (anyNA(ci_vals)) {
      ci_vals <- metagen_fit$TE.random + c(-2, 2) * metagen_fit$seTE.random
    }
    range_pad <- 0.5 * diff(range(ci_vals, na.rm = TRUE))
    xlim_vals <- range(ci_vals, na.rm = TRUE) + c(-range_pad, range_pad)
  }

  lab         <- get_favor_labels(outcome_name, xlim_vals, outcome_direction)
  labels_adj2 <- c(lab$left, lab$right)
  xlab_adj    <- lab$xlab

  # Left-hand forest plot columns; composite outcomes get an extra "Measure" column
  leftcols_default <- c("studlab", "protein_matched", "lean",
                        "energy_balance_subjects", "age_status", "n_total",
                        "duration_weeks", "value_ctrl", "value_int")
  leftlabs_default <- c("Study", "Protein", "Lean", "Diet", "Age", "n",
                        "Weeks", "CTRL \u0394", "INT \u0394")

  if (isTRUE(include_extra_col) && "measure_cleaned" %in% names(data_for_plot)) {
    insert_pos   <- which(leftcols_default == "studlab")
    leftcols_use <- append(leftcols_default, "measure_cleaned", after = insert_pos)
    leftlabs_use <- append(leftlabs_default, "Measure",         after = insert_pos)
  } else {
    leftcols_use <- leftcols_default
    leftlabs_use <- leftlabs_default
  }

  png_filename <- file.path(output_dir, paste0(outcome_name, "_", file_suffix, ".png"))
  message("Saving forest plot to: ", png_filename)

  png(filename = png_filename,
      width    = plot_dim_initial[1],
      height   = plot_dim_initial[2],
      units    = "in", res = 300)

  forest(
    metagen_fit,
    prediction         = TRUE,
    print.predict      = TRUE,
    details            = FALSE,
    print.tau2         = TRUE,
    weight.study       = "random",
    digits.se          = 3,
    text.random        = "Random effects model",
    label.left         = labels_adj2[1],
    label.right        = labels_adj2[2],
    xlab               = xlab_adj,
    leftcols           = leftcols_use,
    leftlabs           = leftlabs_use,
    rightcols          = c("effect.ci", "seTE", "w.random"),
    rightlabs          = c(paste(effect_type, "(95%CI)"), "SE", "Weight"),
    colgap.studlab     = "4mm", colgap.left = "2mm", colgap.forest.left = "1mm",
    just.addcols.left  = "right", calcwidth.pooled = FALSE,
    just.studlab       = "left", sortvar = TE,
    text.main          = paste("Pooled Effect for", outcome_name),
    addrow             = FALSE, addrow.overall = TRUE, addrows.below.overall = 0,
    smlab              = smlab_text, width = 6.5, fontsize = 8, xlim = xlim_vals,
    overall            = TRUE, overall.hetstat = TRUE,
    col.diamond.random = "blue", col.diamond = "blue", col.square = "grey",
    col.square.lines   = "grey", col.inside = "black",
    col.error          = "black", col.study = "black"
  )
  dev.off()

  # Auto-trim whitespace from the PNG
  if (identical(plot_dim_initial, c(50, 50))) {
    img <- image_read(png_filename)
    img <- image_trim(img)
    img <- image_border(img, color = "white", geometry = "20x20")
    image_write(img, png_filename)
  }

  # Optionally produce a Baujat influence plot
  if (baujat) {
    baujat_plot(
      model           = metagen_fit,
      outcome_name    = outcome_name,
      output_dir_bauj = output_dir_bauj,
      ...
    )
  }

  # Compile summary statistics into a tibble for export
  summary_tbl <- tibble(
    Outcome          = outcome_name,
    Subgroup_Var     = "Overall",
    k                = metagen_fit$k,
    k.study          = metagen_fit$k.study,
    n_total          = as.integer(get_total_n(metagen_fit)),
    SM               = as.character(get_scalar(metagen_fit$sm)),
    Effect           = safe_round(metagen_fit$TE.random, 2),
    CI_Lower         = safe_round(metagen_fit$lower.random, 2),
    CI_Upper         = safe_round(metagen_fit$upper.random, 2),
    SE               = safe_round(metagen_fit$seTE.random, 3),
    P.random         = safe_round(metagen_fit$pval.random, 3),
    I2               = safe_round(metagen_fit$I2, 3),
    I2_Lower         = safe_round(metagen_fit$lower.I2, 3),
    I2_Upper         = safe_round(metagen_fit$upper.I2, 3),
    PI_Lower         = safe_round(metagen_fit$lower.predict, 2),
    PI_Upper         = safe_round(metagen_fit$upper.predict, 2),
    Tau_betw         = safe_round(metagen_fit$tau[1], 3),
    Tau_Lower_betw   = safe_round(metagen_fit$lower.tau[1], 3),
    Tau_Upper_betw   = safe_round(metagen_fit$upper.tau[1], 3),
    Tau2_betw        = safe_round(metagen_fit$tau2[1], 3),
    Tau2_Lower_betw  = safe_round(metagen_fit$lower.tau2[1], 3),
    Tau2_Upper_betw  = safe_round(metagen_fit$upper.tau2[1], 3),
    rho              = safe_round(get_scalar(metagen_fit$rho), 3),
    method.random    = as.character(get_scalar(metagen_fit$method.random)),
    method.random.ci = as.character(get_scalar(metagen_fit$method.random.ci)),
    method.predict   = as.character(get_scalar(metagen_fit$method.predict)),
    method.tau       = as.character(get_scalar(metagen_fit$method.tau)),
    method.tau.ci    = as.character(get_scalar(metagen_fit$method.tau.ci)),
    method.bias      = as.character(get_scalar(metagen_fit$method.bias)),
    method.ci        = as.character(get_scalar(metagen_fit$method.ci)),
    method.mean      = as.character(get_scalar(metagen_fit$method.mean))
  )

  attr(summary_tbl, "model") <- metagen_fit
  message("Done: forest plot and summary created for: ", outcome_name)

  return(list(summary = summary_tbl, model = metagen_fit))
}


# =============================================================================
# 8. SUBGROUP ANALYSIS FUNCTION ####
# =============================================================================

# run_meta_subgroup_analysis() fits the same REML random-effects model but
# passes a subgroup variable to metagen(). A separate forest plot is produced
# per outcome × subgroup combination, and per-subgroup pooled estimates are
# returned alongside the overall pooled estimate.

run_meta_subgroup_analysis <- function(data,
                                       outcome_name     = "Body Fat",
                                       effect_type      = "SMD",
                                       te_var           = "hedgesg",
                                       se_var           = "hedgesg_se",
                                       subgroup_var     = "region",
                                       xlim_vals        = NULL,
                                       plot_dim_initial = c(50, 50),
                                       smlab_text       = "SMD in Delta Body Fat %",
                                       file_suffix      = "SMD_Fplot",
                                       output_dir       = NULL,
                                       ...) {

  df <- data %>%
    dplyr::filter(
      !is.na(.data[[te_var]]),
      !is.na(.data[[se_var]]),
      !is.na(.data[[subgroup_var]])
    )

  if ("n_total" %in% names(df)) {
    df$n_total <- as.integer(round(df$n_total))
  }

  if (nrow(df) == 0) {
    message("No valid rows for ", outcome_name, " by ", subgroup_var, ". Skipping.")
    return(NULL)
  }

  k_obs <- sum(is.finite(df[[te_var]]) & is.finite(df[[se_var]]))

  # Require at least 5 studies before running a subgroup analysis
  if (k_obs < 5) {
    message("Skipping ", outcome_name, " for subgroup ", subgroup_var,
            " (k = ", k_obs, " < 5 after filtering).")
    return(NULL)
  }

  multi_es     <- check_multi_es_per_citation(df, label = paste(outcome_name, "by", subgroup_var),
                                              verbose = FALSE)
  cluster_flag <- multi_es

  # Compute total N overall and per subgroup (accounting for clustering where needed)
  if ("n_total" %in% names(df)) {
    if (cluster_flag) {
      n_by_subgroup <- df %>%
        dplyr::group_by(.data[[subgroup_var]], citation) %>%
        dplyr::summarise(n_total_study = max(n_total, na.rm = TRUE), .groups = "drop") %>%
        dplyr::group_by(.data[[subgroup_var]]) %>%
        dplyr::summarise(N_total = sum(n_total_study, na.rm = TRUE), .groups = "drop")
      overall_N_total <- df %>%
        dplyr::group_by(citation) %>%
        dplyr::summarise(n_total_study = max(n_total, na.rm = TRUE), .groups = "drop") %>%
        dplyr::summarise(N_total = sum(n_total_study, na.rm = TRUE)) %>%
        dplyr::pull(N_total)
    } else {
      n_by_subgroup   <- df %>%
        dplyr::group_by(.data[[subgroup_var]]) %>%
        dplyr::summarise(N_total = sum(n_total, na.rm = TRUE), .groups = "drop")
      overall_N_total <- sum(df$n_total, na.rm = TRUE)
    }
  } else {
    n_by_subgroup   <- tibble(!!subgroup_var := character(), N_total = integer())
    overall_N_total <- NA_integer_
  }

  # Display cleanup (same as in run_metagen())
  cols_2dp <- intersect(names(df), c("value_ctrl", "value_int"))
  df[cols_2dp] <- lapply(df[cols_2dp], safe_round, digits = 2)

  pct_cols <- intersect(names(df), c("pct_change_ctrl", "pct_change_int"))
  df[pct_cols] <- lapply(df[pct_cols], function(x) {
    x_num <- suppressWarnings(as.numeric(x))
    ifelse(is.na(x_num), as.character(x), formatC(x_num, format = "f", digits = 1))
  })

  if ("lean" %in% names(df)) {
    df$lean <- dplyr::case_when(
      grepl("^lean", df$lean, ignore.case = TRUE)                  ~ "Lean",
      grepl("not lean|not specified", df$lean, ignore.case = TRUE) ~ "Not lean/NS",
      TRUE ~ df$lean
    )
  }
  if ("duration" %in% names(df)) {
    df$duration <- dplyr::case_when(
      grepl("< 120 days",  df$duration) ~ "<4 months",
      grepl(">= 120 days", df$duration) ~ "4+ months",
      TRUE ~ df$duration
    )
  }
  if ("age_status" %in% names(df)) {
    df$age_status <- dplyr::case_when(
      grepl("General Pop/Mixed", df$age_status) ~ "Mixed",
      TRUE ~ df$age_status
    )
  }
  if ("energy_balance_subjects" %in% names(df)) {
    df$energy_balance_subjects <- dplyr::case_when(
      grepl("loss.*maintenance|maintenance.*loss", df$energy_balance_subjects,
            ignore.case = TRUE)                                  ~ "WL/WM",
      grepl("weight\\s*loss",        df$energy_balance_subjects,
            ignore.case = TRUE)                                  ~ "WL",
      grepl("weight\\s*maintenance", df$energy_balance_subjects,
            ignore.case = TRUE)                                  ~ "Maint",
      grepl("no\\s*intent",          df$energy_balance_subjects,
            ignore.case = TRUE)                                  ~ "No intent",
      TRUE ~ df$energy_balance_subjects
    )
  }

  message("Starting analysis for: ", outcome_name, " by ", subgroup_var,
          if (multi_es) " (clustered by Citation)" else "")

  if (is.null(output_dir)) stop("'output_dir' is not provided.")

  metagen_args <- list(
    TE            = df[[te_var]],
    seTE          = df[[se_var]],
    studlab       = df$citation,
    sm            = effect_type,
    subgroup      = df[[subgroup_var]],
    random        = TRUE,
    common        = FALSE,
    method.tau       = "REML",
    method.tau.ci    = "QP",
    method.random.ci = "classic",
    label.left    = "Favors Intervention",
    label.right   = "Favors Comparator",
    overall       = TRUE,
    data          = df
  )
  if (cluster_flag) metagen_args$cluster <- df$citation

  metagen_fit <- tryCatch(
    do.call(meta::metagen, metagen_args),
    error = function(e) {
      warning("Model failed for ", outcome_name, " by ", subgroup_var, ": ", e$message)
      NULL
    }
  )
  if (is.null(metagen_fit)) return(NULL)

  if (is.null(xlim_vals)) {
    ci_vals <- c(min(metagen_fit$lower, na.rm = TRUE),
                 max(metagen_fit$upper, na.rm = TRUE))
    if (anyNA(ci_vals)) {
      ci_vals <- metagen_fit$TE.random + c(-2, 2) * metagen_fit$seTE.random
    }
    range_pad <- 0.75 * diff(range(ci_vals, na.rm = TRUE))
    xlim_vals <- range(ci_vals, na.rm = TRUE) + c(-range_pad, range_pad)
  }

  lab         <- get_favor_labels(outcome_name, xlim_vals, outcome_direction)
  labels_adj2 <- c(lab$left, lab$right)
  xlab_adj    <- lab$xlab

  png_filename <- file.path(output_dir,
                            paste0(outcome_name, "_", subgroup_var, ".png"))
  message("Saving subgroup forest plot to: ", png_filename)

  png(filename = png_filename,
      width    = plot_dim_initial[1],
      height   = plot_dim_initial[2],
      units    = "in", res = 300)

  if ("n_total" %in% names(df)) df$n_total <- as.integer(round(df$n_total))

  forest(
    metagen_fit,
    prediction         = TRUE, print.predict = TRUE, details = FALSE,
    print.tau2         = TRUE, weight.study  = "random", digits.se = 3,
    text.random        = "Random effects model",
    label.left         = labels_adj2[1], label.right = labels_adj2[2], xlab = xlab_adj,
    leftcols  = c("studlab", "protein_matched", "lean", "energy_balance_subjects",
                  "age_status", "n_total", "duration_weeks", "value_ctrl", "value_int"),
    leftlabs  = c("Study", "Treatment", "Lean", "Diet", "Age", "n", "Weeks",
                  "CTRL \u0394", "INT \u0394"),
    rightcols = c("effect.ci", "seTE", "w.random"),
    rightlabs = c(paste(effect_type, "(95%CI)"), "SE", "Weight"),
    colgap.studlab     = "4mm", colgap.left = "2mm", colgap.forest.left = "1mm",
    just.addcols.left  = "right", calcwidth.pooled = FALSE, just.studlab = "left",
    addrow             = FALSE, addrow.overall = TRUE, addrows.below.overall = 0,
    smlab              = smlab_text, xlim = xlim_vals,
    overall            = FALSE, overall.hetstat = FALSE,
    test.subgroup      = TRUE, sortvar = TE,
    sort.subgroup      = TRUE, print.subgroup.name = FALSE,
    subgroup.name      = subgroup_var, fontsize = 8,
    text.main          = paste("Subgroup analysis by", subgroup_var),
    col.diamond.random = "blue", col.diamond = "black", col.square = "grey",
    col.square.lines   = "grey", col.inside = "black",
    col.error          = "black", col.study = "black"
  )
  dev.off()

  if (identical(plot_dim_initial, c(50, 50))) {
    img <- image_read(png_filename)
    img <- image_trim(img)
    img <- image_border(img, color = "white", geometry = "20x20")
    image_write(img, png_filename)
  }

  # Per-subgroup summary tibble
  meta_summary_subgroup <- tibble(
    Analysis_Type    = "Subgroup",
    Outcome          = outcome_name,
    Subgroup_Var     = subgroup_var,
    Subgroup_Level   = metagen_fit$subgroup.levels,
    Clustered        = cluster_flag,
    k                = metagen_fit$k.w,
    k.study          = metagen_fit$k.study.w,
    SM               = as.character(get_scalar(metagen_fit$sm)),
    Effect           = safe_round(metagen_fit$TE.random.w, 2),
    CI_Lower         = safe_round(metagen_fit$lower.random.w, 2),
    CI_Upper         = safe_round(metagen_fit$upper.random.w, 2),
    SE               = safe_round(metagen_fit$seTE.random.w, 3),
    P.random         = safe_round(metagen_fit$pval.random.w, 3),
    PI_Lower         = safe_round(metagen_fit$lower.predict.w, 2),
    PI_Upper         = safe_round(metagen_fit$upper.predict.w, 2),
    Tau_betw         = safe_round(metagen_fit$tau.w[1], 3),
    Tau_withn        = safe_round(metagen_fit$tau.w[2], 3),
    Tau2_betw        = safe_round(metagen_fit$tau2.w[1], 3),
    Tau2_withn       = safe_round(metagen_fit$tau2.w[2], 3),
    I2               = safe_round(metagen_fit$I2.w, 3),
    rho              = safe_round(get_scalar(metagen_fit$rho), 3),
    between_subgr_Q  = safe_round(get_scalar(metagen_fit$Q.b.random), 3),
    between_subgr_df = safe_round(get_scalar(metagen_fit$df.Q.b.random), 3),
    between_subgr_p  = safe_round(get_scalar(metagen_fit$pval.Q.b.random), 3),
    within_subgr_QE  = safe_round(get_scalar(metagen_fit$Q.w.random), 3),
    within_subgr_QEp = safe_round(get_scalar(metagen_fit$pval.Q.w.random), 3),
    method.random    = as.character(get_scalar(metagen_fit$method.random)),
    method.random.ci = as.character(get_scalar(metagen_fit$method.random.ci)),
    method.predict   = as.character(get_scalar(metagen_fit$method.predict)),
    method.tau       = as.character(get_scalar(metagen_fit$method.tau)),
    method.tau.ci    = as.character(get_scalar(metagen_fit$method.tau.ci)),
    method.bias      = as.character(get_scalar(metagen_fit$method.bias)),
    method.ci        = as.character(get_scalar(metagen_fit$method.ci)),
    method.mean      = as.character(get_scalar(metagen_fit$method.mean)),
    # Placeholder columns to align schema with influence/LOO table
    Full_Effect = NA_real_, Full_CI_Lower = NA_real_, Full_CI_Upper = NA_real_,
    Delta_Effect = NA_real_, LOO_Tau2 = NA_real_, LOO_I2 = NA_real_,
    CooksD = NA_real_, Hat = NA_real_, DFFITS = NA_real_,
    CooksD_Thresh = NA_real_, Flag_CooksD = NA,
    Flag_FlipDirection = NA, Flag_OutsideFullCI = NA, Influential = NA
  )

  n_by_subgroup_renamed <- n_by_subgroup %>%
    dplyr::rename(Subgroup_Level = .data[[subgroup_var]])

  meta_summary_subgroup <- meta_summary_subgroup %>%
    dplyr::left_join(n_by_subgroup_renamed, by = "Subgroup_Level") %>%
    dplyr::rename(n_total = N_total)

  # Overall pooled row (appended to the subgroup rows)
  overall_summary <- tibble(
    Analysis_Type    = "Overall",
    Outcome          = outcome_name,
    Subgroup_Var     = "Overall",
    Subgroup_Level   = "All",
    Clustered        = cluster_flag,
    k                = metagen_fit$k,
    k.study          = metagen_fit$k.study,
    n_total          = as.integer(overall_N_total),
    SM               = as.character(get_scalar(metagen_fit$sm)),
    Effect           = safe_round(metagen_fit$TE.random, 2),
    CI_Lower         = safe_round(metagen_fit$lower.random, 2),
    CI_Upper         = safe_round(metagen_fit$upper.random, 2),
    SE               = safe_round(metagen_fit$seTE.random, 3),
    P.random         = safe_round(metagen_fit$pval.random, 3),
    PI_Lower         = safe_round(metagen_fit$lower.predict, 2),
    PI_Upper         = safe_round(metagen_fit$upper.predict, 2),
    Tau_betw         = safe_round(metagen_fit$tau[1], 3),
    Tau_withn        = safe_round(metagen_fit$tau[2], 3),
    Tau2_betw        = safe_round(metagen_fit$tau2[1], 3),
    Tau2_withn       = safe_round(metagen_fit$tau2[2], 3),
    I2               = safe_round(metagen_fit$I2, 3),
    rho              = safe_round(get_scalar(metagen_fit$rho), 3),
    between_subgr_Q  = NA_real_, between_subgr_df = NA_real_, between_subgr_p = NA_real_,
    within_subgr_QE  = NA_real_, within_subgr_QEp = NA_real_,
    method.random    = as.character(get_scalar(metagen_fit$method.random)),
    method.random.ci = as.character(get_scalar(metagen_fit$method.random.ci)),
    method.predict   = as.character(get_scalar(metagen_fit$method.predict)),
    method.tau       = as.character(get_scalar(metagen_fit$method.tau)),
    method.tau.ci    = as.character(get_scalar(metagen_fit$method.tau.ci)),
    method.bias      = as.character(get_scalar(metagen_fit$method.bias)),
    method.ci        = as.character(get_scalar(metagen_fit$method.ci)),
    method.mean      = as.character(get_scalar(metagen_fit$method.mean)),
    Full_Effect = NA_real_, Full_CI_Lower = NA_real_, Full_CI_Upper = NA_real_,
    Delta_Effect = NA_real_, LOO_Tau2 = NA_real_, LOO_I2 = NA_real_,
    CooksD = NA_real_, Hat = NA_real_, DFFITS = NA_real_,
    CooksD_Thresh = NA_real_, Flag_CooksD = NA,
    Flag_FlipDirection = NA, Flag_OutsideFullCI = NA, Influential = NA
  )

  return(list(
    summary = dplyr::bind_rows(overall_summary, meta_summary_subgroup),
    model   = metagen_fit
  ))
}


# =============================================================================
# 9. OUTLIER DETECTION FUNCTIONS ####
# =============================================================================

# full_outlier_analysis(): uses dmetar::find.outliers() to identify studies
# whose removal substantially changes the pooled estimate, then re-fits the
# model without those studies and returns a comparison table.

full_outlier_analysis <- function(data,
                                  outcome_name     = "Body Fat",
                                  effect_type      = "SMD",
                                  te_var           = "hedgesg",
                                  se_var           = "hedgesg_se",
                                  xlim_vals        = NULL,
                                  plot_dim_initial = c(50, 50),
                                  smlab_text       = "SMD in Delta Body Fat %",
                                  file_suffix      = "SMD_Fplot",
                                  output_dir       = NULL,
                                  forest           = TRUE,
                                  ...) {

  # Internal helpers for extracting tau and tau^2 (indexed for multi-level models)
  .get_tau  <- function(m, i) as.numeric(m$tau[i])
  .get_tau2 <- function(m, i) as.numeric(m$tau2[i])

  if (is.null(output_dir)) stop("'output_dir' must be provided.")
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

  need <- c(te_var, se_var, "citation")
  miss <- setdiff(need, names(data))
  if (length(miss)) stop("Missing columns in 'data': ", paste(miss, collapse = ", "))

  data <- data[is.finite(data[[te_var]]) & is.finite(data[[se_var]]), , drop = FALSE]
  if (nrow(data) < 2) {
    message("Not enough data for: ", outcome_name)
    return(NULL)
  }

  message("Starting outlier analysis for: ", outcome_name)

  # Fit unclustered model (required by find.outliers)
  model_noclust <- metagen(
    TE = data[[te_var]], seTE = data[[se_var]],
    sm = effect_type, random = TRUE, common = FALSE,
    method.tau = "REML", method.tau.ci = "QP", method.random.ci = "classic",
    label.left = "Favors Intervention", label.right = "Favors Comparator",
    overall = TRUE, data = data
  )

  # Fit clustered model (accounting for multiple effect sizes per study)
  model_clust <- metagen(
    TE = data[[te_var]], seTE = data[[se_var]],
    sm = effect_type, random = TRUE, common = FALSE,
    method.tau = "REML", method.tau.ci = "QP", method.random.ci = "classic",
    label.left = "Favors Intervention", label.right = "Favors Comparator",
    overall = TRUE, cluster = data$citation, data = data
  )

  outliers_out <- tryCatch(
    find.outliers(model_noclust, ...),
    error = function(e) { warning("find.outliers() failed: ", e$message); NULL }
  )

  idx <- if (is.null(outliers_out)) integer(0) else outliers_out$out.study.random
  if (length(idx) == 0) {
    message("No outliers detected for: ", outcome_name)
    return(NULL)
  }

  meta_data_outliers <- data[-idx, , drop = FALSE]

  model_clust_outliers <- metagen(
    TE      = meta_data_outliers[[te_var]],
    seTE    = meta_data_outliers[[se_var]],
    studlab = meta_data_outliers$citation,
    sm = effect_type, random = TRUE, common = FALSE,
    method.tau = "REML", method.tau.ci = "QP", method.random.ci = "classic",
    label.left = "Favors Intervention", label.right = "Favors Comparator",
    overall = TRUE, cluster = meta_data_outliers$citation,
    data = meta_data_outliers
  )

  which_outliers    <- idx
  which_no_outliers <- setdiff(seq_len(nrow(data)), idx)

  outlier_tbl <- tibble::tibble(
    Outcome      = outcome_name,
    Subgroup_Var = "Overall", Subgroup_Level = "All",
    k            = model_clust$k, k.study = model_clust$k.study,
    SM           = as.character(get_scalar(model_clust$sm)),
    Effect       = safe_round(model_clust$TE.random, 2),
    CI_Lower     = safe_round(model_clust$lower.random, 2),
    CI_Upper     = safe_round(model_clust$upper.random, 2),
    SE           = safe_round(model_clust$seTE.random, 3),
    P.random     = safe_round(model_clust$pval.random, 3),
    PI_Lower     = safe_round(model_clust$lower.predict, 2),
    PI_Upper     = safe_round(model_clust$upper.predict, 2),
    Tau          = safe_round(.get_tau(model_clust, 1), 3),
    Tau_Lower    = safe_round(model_clust$lower.tau[1], 3),
    Tau_Upper    = safe_round(model_clust$upper.tau[1], 3),
    Tau2         = safe_round(.get_tau2(model_clust, 1), 3),
    Tau2_Lower   = safe_round(model_clust$lower.tau2[1], 3),
    Tau2_Upper   = safe_round(model_clust$upper.tau2[1], 3),
    I2           = safe_round(model_clust$I2, 3),
    rho          = safe_round(get_scalar(model_clust$rho), 3),
    Outliers_no  = paste0(which_outliers,    collapse = ", "),
    Non_outliers_no = paste0(which_no_outliers, collapse = ", "),
    Outliers_s   = paste0(data[which_outliers,    "citation", drop = TRUE], collapse = ", "),
    Non_outliers_s = paste0(data[which_no_outliers, "citation", drop = TRUE], collapse = ", "),
    Effect_out   = safe_round(model_clust_outliers$TE.random, 2),
    CI_Lower_out = safe_round(model_clust_outliers$lower.random, 2),
    CI_Upper_out = safe_round(model_clust_outliers$upper.random, 2),
    SE_out       = safe_round(model_clust_outliers$seTE.random, 3),
    P_out        = safe_round(model_clust_outliers$pval.random, 3),
    PI_Lower_out = safe_round(model_clust_outliers$lower.predict, 2),
    PI_Upper_out = safe_round(model_clust_outliers$upper.predict, 2),
    Tau_out      = safe_round(.get_tau(model_clust_outliers, 1), 3),
    Tau_Lower_out = safe_round(model_clust_outliers$lower.tau[1], 3),
    Tau_Upper_out = safe_round(model_clust_outliers$upper.tau[1], 3),
    Tau2_out     = safe_round(.get_tau2(model_clust_outliers, 1), 3),
    Tau2_Lower_out = safe_round(model_clust_outliers$lower.tau2[1], 3),
    Tau2_Upper_out = safe_round(model_clust_outliers$upper.tau2[1], 3),
    I2_out       = safe_round(model_clust_outliers$I2, 3),
    rho_out      = safe_round(get_scalar(model_clust_outliers$rho), 3)
  )

  return(outlier_tbl)
}


# subgroup_outlier_analysis(): same approach as full_outlier_analysis() but
# applied within each level of a subgroup moderator variable.

subgroup_outlier_analysis <- function(data,
                                      outcome_name     = "Body Fat",
                                      effect_type      = "SMD",
                                      te_var           = "hedgesg",
                                      se_var           = "hedgesg_se",
                                      subgroup_var     = "region",
                                      xlim_vals        = NULL,
                                      plot_dim_initial = c(50, 50),
                                      smlab_text       = "SMD in Delta Body Fat %",
                                      file_suffix      = "SMD_Fplot",
                                      output_dir       = NULL,
                                      forest           = TRUE,
                                      ...) {

  .get_tau  <- function(m, i) as.numeric(m$tau[i])
  .get_tau2 <- function(m, i) as.numeric(m$tau2[i])

  if (is.null(output_dir)) stop("'output_dir' must be provided.")
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

  need <- c(te_var, se_var, "citation", subgroup_var)
  miss <- setdiff(need, names(data))
  if (length(miss)) stop("Missing columns in 'data': ", paste(miss, collapse = ", "))

  data <- data[is.finite(data[[te_var]]) & is.finite(data[[se_var]]), , drop = FALSE]
  if (nrow(data) < 5) {
    message("Not enough data for subgroup outlier analysis: ", outcome_name)
    return(NULL)
  }

  message("Starting subgroup outlier analysis for: ", outcome_name, ", ", subgroup_var)

  # Helper: fit subgroup meta-regression to test between-subgroup heterogeneity
  run_subgroup_metareg <- function(meta_model, subgroup_var) {
    metafor::rma.mv(
      yi     = meta_model$TE,
      V      = meta_model$seTE^2,
      mods   = as.formula(paste0("~", subgroup_var)),
      random = ~ 1 | meta_model$studlab,
      data   = meta_model$data,
      method = "REML"
    )
  }

  model_noclust <- metagen(
    TE = data[[te_var]], seTE = data[[se_var]],
    sm = effect_type, random = TRUE, common = FALSE,
    method.tau = "REML", method.tau.ci = "QP", method.random.ci = "classic",
    overall = TRUE, data = data
  )
  model_clust <- metagen(
    TE = data[[te_var]], seTE = data[[se_var]],
    sm = effect_type, random = TRUE, common = FALSE,
    method.tau = "REML", method.tau.ci = "QP", method.random.ci = "classic",
    overall = TRUE, cluster = data$citation, data = data
  )
  model_clust_subgroup <- metagen(
    TE = data[[te_var]], seTE = data[[se_var]],
    subgroup = data[[subgroup_var]],
    sm = effect_type, random = TRUE, common = FALSE,
    method.tau = "REML", method.tau.ci = "QP", method.random.ci = "classic",
    overall = TRUE, cluster = data$citation, data = data
  )

  outliers_out <- tryCatch(
    find.outliers(model_noclust, ...),
    error = function(e) { warning("find.outliers() failed: ", e$message); NULL }
  )
  idx <- if (is.null(outliers_out)) integer(0) else outliers_out$out.study.random
  message("Number of outliers detected: ", length(idx))
  if (length(idx) == 0) {
    message("No outliers detected for: ", outcome_name)
    return(NULL)
  }

  meta_data_outliers <- data[-idx, , drop = FALSE]

  model_clust_outliers <- tryCatch(
    metagen(
      TE = meta_data_outliers[[te_var]], seTE = meta_data_outliers[[se_var]],
      studlab = meta_data_outliers$citation,
      subgroup = meta_data_outliers[[subgroup_var]],
      sm = effect_type, random = TRUE, common = FALSE,
      method.tau = "REML", method.tau.ci = "QP",
      label.left = "Favors Intervention", label.right = "Favors Comparator",
      overall = TRUE, cluster = meta_data_outliers$citation,
      data = meta_data_outliers
    ),
    error = function(e) { warning("Model (with subgroup) failed: ", e$message); NULL }
  )
  if (is.null(model_clust_outliers)) return(list(model_clust_outliers = NULL))

  model_clust_no_subgroup_outliers <- tryCatch(
    metagen(
      TE = meta_data_outliers[[te_var]], seTE = meta_data_outliers[[se_var]],
      studlab = meta_data_outliers$citation,
      sm = effect_type, random = TRUE, common = FALSE,
      method.tau = "REML", method.tau.ci = "QP",
      label.left = "Favors Intervention", label.right = "Favors Comparator",
      overall = TRUE, cluster = meta_data_outliers$citation,
      data = meta_data_outliers
    ),
    error = function(e) { warning("Model (no subgroup) failed: ", e$message); NULL }
  )
  if (is.null(model_clust_no_subgroup_outliers))
    return(list(model_clust_no_subgroup_outliers = NULL))

  # Map outlier rows to subgroup levels for reporting
  which_outliers    <- idx
  which_no_outliers <- setdiff(seq_len(nrow(data)), idx)

  is_outlier_vec   <- integer(nrow(data))
  is_outlier_vec[which_outliers] <- 1
  df_is_outlier    <- data.frame(
    Row        = seq_len(nrow(data)),
    citation   = data$citation,
    Subgroup   = data[[subgroup_var]],
    is_outlier = is_outlier_vec
  )
  df_outliers      <- subset(df_is_outlier, is_outlier == 1)

  which_outliers_by_sg   <- unlist(lapply(
    split(df_outliers$Row,     df_outliers$Subgroup), function(x) paste0(x,       collapse = ", ")))
  which_outliers_s_by_sg <- unlist(lapply(
    split(df_outliers$citation, df_outliers$Subgroup), function(x) paste0(x,       collapse = ", ")))

  subgroup_levels       <- as.character(model_clust_subgroup$subgroup.levels)
  which_outliers_by_sg   <- which_outliers_by_sg[subgroup_levels]
  which_outliers_s_by_sg <- which_outliers_s_by_sg[subgroup_levels]

  metareg_output      <- tryCatch(run_subgroup_metareg(model_clust, subgroup_var),
                                  error = function(e) NULL)
  metareg_sens_output <- tryCatch(run_subgroup_metareg(model_clust_no_subgroup_outliers,
                                                       subgroup_var),
                                  error = function(e) NULL)

  meta_summary_original <- tibble::tibble(
    Outcome          = outcome_name, Subgroup_Var = subgroup_var,
    Subgroup_Level   = model_clust_subgroup$subgroup.levels,
    k                = model_clust_subgroup$k.w,
    k.study          = model_clust_subgroup$k.study.w,
    SM               = as.character(get_scalar(model_clust_subgroup$sm)),
    Effect           = safe_round(model_clust_subgroup$TE.random.w, 2),
    CI_Lower         = safe_round(model_clust_subgroup$lower_random, 2),
    CI_Upper         = safe_round(model_clust_subgroup$upper_random, 2),
    SE               = safe_round(model_clust_subgroup$seTE.random.w, 3),
    P.random.w       = safe_round(model_clust_subgroup$pval.random.w, 3),
    PI_Lower         = safe_round(model_clust_subgroup$lower_predict.w, 2),
    PI_Upper         = safe_round(model_clust_subgroup$upper_predict.w, 2),
    Tau              = safe_round(.get_tau(model_clust_subgroup, 1), 3),
    Tau2             = safe_round(.get_tau2(model_clust_subgroup, 1), 3),
    I2               = safe_round(model_clust_subgroup$I2.w, 3),
    subgr_test_QM    = safe_round(get_scalar(if (!is.null(metareg_output))  metareg_output$QM  else NA), 3),
    subgr_test_QMdf  = safe_round(get_scalar(if (!is.null(metareg_output))  metareg_output$QMdf else NA), 3),
    subgr_test_QMp   = safe_round(get_scalar(if (!is.null(metareg_output))  metareg_output$QMp  else NA), 3),
    subgr_test_QE    = safe_round(get_scalar(if (!is.null(metareg_output))  metareg_output$QE   else NA), 3),
    subgr_test_QEp   = safe_round(get_scalar(if (!is.null(metareg_output))  metareg_output$QEp  else NA), 3),
    Outliers_no      = which_outliers_by_sg,
    Outliers_s       = which_outliers_s_by_sg
  )

  meta_subgroup_outliers <- tibble::tibble(
    Outcome          = outcome_name, Subgroup_Var = subgroup_var,
    Subgroup_Level   = model_clust_outliers$subgroup.levels,
    k_out            = model_clust_outliers$k.w,
    k.study_out      = model_clust_outliers$k.study.w,
    Effect_out       = safe_round(model_clust_outliers$TE.random.w, 2),
    CI_Lower_out     = safe_round(model_clust_outliers$lower.random, 2),
    CI_Upper_out     = safe_round(model_clust_outliers$upper.random, 2),
    SE_out           = safe_round(model_clust_outliers$seTE.random.w, 3),
    P.random.w       = safe_round(model_clust_outliers$pval.random.w, 3),
    PI_Lower_out     = safe_round(model_clust_outliers$lower.predict.w, 2),
    PI_Upper_out     = safe_round(model_clust_outliers$upper.predict.w, 2),
    Tau_out          = safe_round(.get_tau(model_clust_outliers, 1), 3),
    Tau2_out         = safe_round(.get_tau2(model_clust_outliers, 1), 3),
    I2_out           = safe_round(model_clust_outliers$I2.w, 3),
    subgr_test_QM    = safe_round(get_scalar(if (!is.null(metareg_sens_output)) metareg_sens_output$QM    else NA), 3),
    subgr_test_QMdf  = safe_round(get_scalar(if (!is.null(metareg_sens_output)) metareg_sens_output$QMdf  else NA), 3),
    subgr_test_QMp   = safe_round(get_scalar(if (!is.null(metareg_sens_output)) metareg_sens_output$QMp   else NA), 3),
    subgr_test_QE    = safe_round(get_scalar(if (!is.null(metareg_sens_output)) metareg_sens_output$QE    else NA), 3),
    subgr_test_QEp   = safe_round(get_scalar(if (!is.null(metareg_sens_output)) metareg_sens_output$QEp   else NA), 3)
  )

  # Merge original and sensitivity (outliers-removed) results side by side
  meta_outliers_subgroup <- merge(
    meta_summary_original, meta_subgroup_outliers,
    by = c("Outcome", "Subgroup_Var", "Subgroup_Level"), all = TRUE
  )

  return(meta_outliers_subgroup)
}


# =============================================================================
# 10. INCLUSION FILTER FUNCTIONS FOR ANALYSIS LOOPS ####
# =============================================================================

# For the overall (pooled) analysis, each phased trial contributes one row:
#   - Benassi-Evans 2009: weight-loss phase only (no 0-12 full-study row exists)
#   - Poddar 2013: the full-study 0-12 month row (avoids double-counting with
#     the separate phase rows used in energy-balance subgroup analyses)

restrict_main_phases <- function(df,
                                 citations = c("Benassi-Evans 2009", "Poddar 2013")) {
  df %>%
    dplyr::filter(
      !(citation %in% citations) |
        (citation == "Benassi-Evans 2009" & energy_balance_subjects == "Weight loss") |
        (citation == "Poddar 2013" & grepl("loss \\+ maintenance", tolower(sub_group1)))
    )
}

# Drop citation-specific exclusions by outcome (e.g., Wells 2003 for body weight)
exclude_citations_by_outcome <- function(df, outcome_name,
                                         exclusions   = list("Body weight" = c("Wells 2003")),
                                         citation_col = "citation") {
  if (outcome_name %in% names(exclusions)) {
    df <- dplyr::filter(df, !.data[[citation_col]] %in% exclusions[[outcome_name]])
  }
  df
}

# Leidy 2007 and Campbell 2010 are from the same RCT.
# For outcomes where both appear, drop Campbell 2010 from all subgroup analyses
# except age_status, where Leidy 2007 is dropped instead (because Campbell 2010
# is the report relevant to the older-adult stratum).
outcomes_with_both <- names(data_subsets)[vapply(names(data_subsets), function(nm) {
  df <- data_subsets[[nm]]
  any(df$citation == "Leidy 2007") && any(df$citation == "Campbell 2010")
}, logical(1))]

exclude_when_cooccurs <- function(df, outcome_name, outcomes_trigger,
                                  drop_citation = "Campbell 2010",
                                  citation_col  = "citation") {
  if (outcome_name %in% outcomes_trigger) {
    dplyr::filter(df, .data[[citation_col]] != drop_citation)
  } else {
    df
  }
}

filter_leidy_campbell <- function(df, outcome_name, subgroup_var) {
  if (!(outcome_name %in% outcomes_with_both)) return(df)
  if (subgroup_var == "age_status") {
    df <- df %>% dplyr::filter(citation != "Leidy 2007")
  } else {
    df <- df %>% dplyr::filter(citation != "Campbell 2010")
  }
  df
}

# Master function: apply all inclusion filters in sequence (used in every loop)
apply_main_inclusion_filters <- function(df, outcome_name) {
  df %>%
    restrict_main_phases() %>%
    exclude_citations_by_outcome(outcome_name = outcome_name) %>%
    exclude_when_cooccurs(outcome_name    = outcome_name,
                          outcomes_trigger = outcomes_with_both,
                          drop_citation   = "Campbell 2010")
}


# =============================================================================
# 11. SUBGROUP PHASE-SELECTION FUNCTIONS ####
# =============================================================================

# For energy-balance subgroup variables, Poddar 2013 and Benassi-Evans 2009
# contribute BOTH their phase rows (so each phase lands in the correct subgroup
# level). For all other subgroup variables, they contribute only one row each
# (consistent with the overall analysis).

eb_subgroup_vars <- c("energy_balance_grp", "energy_balance_subjects",
                      "actual_EB_grp2", "actual_EB_grp", "actual_EB_subjects")

restrict_WL_for_subgroups <- function(df, subgroup_var,
                                      citations = c("Benassi-Evans 2009", "Poddar 2013"),
                                      eb_var    = "sub_group1") {

  if (subgroup_var %in% eb_subgroup_vars) {
    # Keep both phase rows; drop only the full-study Poddar row (would double-count)
    return(df %>%
             dplyr::filter(!(citation == "Poddar 2013" &
                               grepl("loss \\+ maintenance", tolower(sub_group1)))))
  }

  if (!eb_var %in% names(df)) return(df)

  # For all other subgroup variables: one row per phased trial
  df %>%
    dplyr::mutate(
      .EB             = tolower(gsub("\\s+", " ", trimws(.data[[eb_var]]))),
      .is_WL          = (grepl("weight[- ]*loss", .EB) | grepl("\\bwl\\b", .EB)) &
        !grepl("maintenance", .EB),
      .is_poddar_full = citation == "Poddar 2013" & grepl("loss \\+ maintenance", .EB)
    ) %>%
    dplyr::filter(
      (citation == "Poddar 2013"        & .is_poddar_full) |
        (citation == "Benassi-Evans 2009" & .is_WL) |
        !(citation %in% citations)
    ) %>%
    dplyr::select(-.EB, -.is_WL, -.is_poddar_full)
}


# =============================================================================
# 12. MAIN ANALYSIS LOOP (OVERALL POOLED ESTIMATES) ####
# =============================================================================

main_results_list <- vector("list", length = nrow(data_subsets_labels))
meta_models_list  <- vector("list", length = nrow(data_subsets_labels))

for (i in seq_len(nrow(data_subsets_labels))) {
  outcome_i <- data_subsets_labels$measure_composite[i]

  df_i <- apply_main_inclusion_filters(data_subsets[[i]], outcome_i)
  dbg_log(outcome_i, df_i)

  res <- run_metagen(
    data              = df_i,
    outcome_name      = outcome_i,
    effect_type       = data_subsets_labels$effect_type[i],
    te_var            = data_subsets_labels$te_var[i],
    se_var            = data_subsets_labels$se_var[i],
    xlim_vals         = NULL,
    smlab_text        = outcome_i,
    file_suffix       = "Main_forest",
    baujat            = TRUE,
    output_dir        = output_mainfor_dir,
    output_dir_bauj   = output_baujat_sens_dir,
    include_extra_col = data_subsets_labels$include_extra_col[i]
  )

  if (is.null(res)) next
  main_results_list[[i]] <- res$summary
  meta_models_list[[i]]  <- res$model
}


# =============================================================================
# 13. SENSITIVITY ANALYSIS: EXCLUDE HIGH RISK-OF-BIAS STUDIES ####
# =============================================================================

# Re-run every overall analysis after dropping studies rated "High" overall
# risk of bias (RoB 2.0), to assess their influence on pooled estimates.

high_rob_studies <- data_MA %>%
  dplyr::filter(overall_ro_b == "High") %>%
  dplyr::distinct(citation) %>%
  dplyr::pull(citation)

message("Sensitivity analysis - excluding High-RoB studies: ",
        paste(high_rob_studies, collapse = ", "))

output_sens_rob_dir <- file.path(output_dir, "Sensitivity - High RoB excluded")
dir.create(output_sens_rob_dir, showWarnings = FALSE, recursive = TRUE)

sens_rob_results_list <- vector("list", length = nrow(data_subsets_labels))

for (i in seq_len(nrow(data_subsets_labels))) {
  outcome_i <- data_subsets_labels$measure_composite[i]

  df_i <- apply_main_inclusion_filters(data_subsets[[i]], outcome_i) %>%
    dplyr::filter(!citation %in% high_rob_studies)

  dbg_log(paste0(outcome_i, " [High-RoB excluded]"), df_i)
  if (nrow(df_i) < 2) next

  res <- run_metagen(
    data              = df_i,
    outcome_name      = outcome_i,
    effect_type       = data_subsets_labels$effect_type[i],
    te_var            = data_subsets_labels$te_var[i],
    se_var            = data_subsets_labels$se_var[i],
    xlim_vals         = NULL,
    smlab_text        = outcome_i,
    file_suffix       = "Sens_HighRoB_excluded",
    baujat            = FALSE,
    output_dir        = output_sens_rob_dir,
    include_extra_col = data_subsets_labels$include_extra_col[i]
  )

  if (is.null(res)) next
  sens_rob_results_list[[i]] <- res$summary
}

summary_sens_rob <- dplyr::bind_rows(sens_rob_results_list)

# Side-by-side comparison: main vs High-RoB-excluded
keep_cols <- c("Outcome", "k", "k.study", "Effect", "CI_Lower", "CI_Upper", "I2", "P.random")
rob_sensitivity_comparison <- dplyr::bind_rows(main_results_list) %>%
  dplyr::select(dplyr::any_of(keep_cols)) %>%
  dplyr::rename_with(~ paste0(.x, "_main"), -Outcome) %>%
  dplyr::left_join(
    summary_sens_rob %>%
      dplyr::select(dplyr::any_of(keep_cols)) %>%
      dplyr::rename_with(~ paste0(.x, "_sens"), -Outcome),
    by = "Outcome"
  ) %>%
  dplyr::mutate(
    studies_dropped = k_main - k_sens,
    Effect_diff     = round(Effect_sens - Effect_main, 3)
  ) %>%
  dplyr::arrange(Outcome)

writexl::write_xlsx(
  list(
    Comparison_main_vs_sens = rob_sensitivity_comparison,
    Sensitivity_estimates   = summary_sens_rob,
    High_RoB_studies        = tibble::tibble(citation = high_rob_studies)
  ),
  file.path(output_mainexcel_dir, "Sens_HighRoB_excluded.xlsx")
)

# Overlay forest plot: pooled estimate "All studies" vs "High-RoB excluded"
rob_sens_plot_df <- dplyr::bind_rows(
  rob_sensitivity_comparison %>%
    dplyr::transmute(Outcome, Analysis = "All studies",
                     Effect = Effect_main, lo = CI_Lower_main, hi = CI_Upper_main, k = k_main),
  rob_sensitivity_comparison %>%
    dplyr::transmute(Outcome, Analysis = "High-RoB excluded",
                     Effect = Effect_sens, lo = CI_Lower_sens, hi = CI_Upper_sens, k = k_sens)
) %>%
  dplyr::filter(!is.na(Effect)) %>%
  dplyr::mutate(Analysis = factor(Analysis,
                                  levels = c("High-RoB excluded", "All studies")))

rob_sens_overlay <- ggplot2::ggplot(
  rob_sens_plot_df, ggplot2::aes(x = Effect, y = Analysis, colour = Analysis)) +
  ggplot2::geom_vline(xintercept = 0, linetype = "dashed", colour = "grey60") +
  ggplot2::geom_pointrange(ggplot2::aes(xmin = lo, xmax = hi),
                           linewidth = 0.6, fatten = 3) +
  ggplot2::geom_text(ggplot2::aes(label = paste0("k=", k)),
                     vjust = -1, size = 2.8, show.legend = FALSE) +
  ggplot2::facet_wrap(~ Outcome, scales = "free_x", ncol = 3) +
  ggplot2::scale_colour_manual(values = c("All studies"       = "#2C3E50",
                                          "High-RoB excluded" = "#C0392B")) +
  ggplot2::labs(
    x       = "Pooled effect (95% CI)", y = NULL, colour = NULL,
    title   = "Sensitivity analysis: all studies vs High risk-of-bias excluded",
    caption = "Dashed line = no effect. k = number of studies pooled."
  ) +
  ggplot2::theme_bw(base_size = 11) +
  ggplot2::theme(
    legend.position  = "top",
    axis.text.y      = ggplot2::element_blank(),
    axis.ticks.y     = ggplot2::element_blank(),
    panel.grid.minor = ggplot2::element_blank(),
    strip.text       = ggplot2::element_text(face = "bold", size = 8.5)
  )

ggplot2::ggsave(file.path(output_sens_rob_dir, "Sens_overlay_main_vs_HighRoB.png"),
                rob_sens_overlay, width = 11, height = 9, dpi = 300, bg = "white")
ggplot2::ggsave(file.path(output_sens_rob_dir, "Sens_overlay_main_vs_HighRoB.pdf"),
                rob_sens_overlay, width = 11, height = 9)

message("RoB sensitivity complete. Outputs in: ", output_sens_rob_dir)


# =============================================================================
# 14. INFLUENCE DIAGNOSTICS (LEAVE-ONE-OUT + COOK'S DISTANCE) ####
# =============================================================================

# For each outcome, we run two complementary influence diagnostics:
#   (1) meta::metainf() - leave-one-out (LOO) analysis: re-estimates the
#       pooled effect omitting each study in turn.
#   (2) metafor::rma.uni() + influence() - Cook's distance, hat values, and
#       DFFITS. Studies exceeding the Cook's D threshold (4 / (k - p - 1))
#       OR whose omission flips the direction of the effect OR whose LOO
#       estimate falls outside the full-model CI are flagged as influential.

influence_results_list <- list()
k_inf <- 1

for (i in seq_along(meta_models_list)) {

  model_i   <- meta_models_list[[i]]
  outcome_i <- data_subsets_labels$measure_composite[i]
  if (is.null(model_i)) next

  # Leave-one-out analysis
  loo <- try(meta::metainf(model_i, pooled = "random"), silent = TRUE)
  if (inherits(loo, "try-error") || is.null(loo)) next

  loo_tbl <- tibble::tibble(
    Analysis_Type    = "Influence",
    Outcome          = outcome_i,
    Subgroup_Var     = "LOO",
    Subgroup_Level   = loo$studlab,
    Clustered        = NA,
    k                = loo$k,
    k.study          = loo$k.study,
    n_total          = NA_integer_,
    SM               = as.character(get_scalar(loo$sm)),
    Effect           = loo$TE,
    CI_Lower         = loo$lower,
    CI_Upper         = loo$upper,
    SE               = loo$seTE,
    P.random         = loo$pval,
    PI_Lower         = NA_real_, PI_Upper = NA_real_,
    Tau_betw         = NA_real_, Tau_withn = NA_real_,
    Tau2_betw        = NA_real_, Tau2_withn = NA_real_,
    I2               = NA_real_, rho = NA_real_,
    between_subgr_Q  = NA_real_, between_subgr_df = NA_real_,
    between_subgr_p  = NA_real_, within_subgr_QE  = NA_real_,
    within_subgr_QEp = NA_real_,
    method.random    = NA_character_, method.random.ci = NA_character_,
    method.predict   = NA_character_, method.tau      = NA_character_,
    method.tau.ci    = NA_character_, method.bias     = NA_character_,
    method.ci        = NA_character_, method.mean     = NA_character_,
    LOO_Tau2         = loo$tau2,
    LOO_I2           = loo$I2
  )

  # Store full-model estimates for delta calculations
  full_TE <- if (length(model_i$TE.random) > 0) model_i$TE.random else NA_real_
  full_lo <- if (length(model_i$lower.random) > 0) model_i$lower.random else NA_real_
  full_hi <- if (length(model_i$upper.random) > 0) model_i$upper.random else NA_real_

  loo_tbl <- loo_tbl %>%
    dplyr::mutate(
      Full_Effect   = full_TE,
      Full_CI_Lower = full_lo,
      Full_CI_Upper = full_hi,
      Delta_Effect  = Effect - full_TE,
      citation      = sub("^Omitting\\s+", "", Subgroup_Level),
      Subgroup_Level = citation
    )

  # Cook's distance via metafor::rma.uni()
  df_i <- apply_main_inclusion_filters(data_subsets[[i]], outcome_i)
  te_i <- data_subsets_labels$te_var[i]
  se_i <- data_subsets_labels$se_var[i]

  df_i <- df_i %>%
    dplyr::filter(is.finite(.data[[te_i]]), is.finite(.data[[se_i]])) %>%
    dplyr::mutate(citation = as.character(citation))

  mf <- try(
    metafor::rma.uni(
      yi     = df_i[[te_i]],
      sei    = df_i[[se_i]],
      method = "REML",
      slab   = df_i$citation
    ),
    silent = TRUE
  )

  if (inherits(mf, "try-error") || is.null(mf)) {
    loo_tbl <- loo_tbl %>%
      dplyr::mutate(
        CooksD = NA_real_, Hat = NA_real_, DFFITS = NA_real_,
        CooksD_Thresh = NA_real_, Flag_CooksD = NA
      )
  } else {
    k_mf <- mf$k
    p_mf <- mf$p

    inf_obj <- try(metafor:::influence.rma.uni(mf), silent = TRUE)

    if (inherits(inf_obj, "try-error") || is.null(inf_obj) || is.null(inf_obj$inf)) {
      message("Influence diagnostics failed for: ", outcome_i)
      CooksD <- rep(NA_real_, k_mf)
      Hat    <- rep(NA_real_, k_mf)
      DFFITS <- rep(NA_real_, k_mf)
    } else {
      CooksD <- as.numeric(inf_obj$inf$cook.d)
      Hat    <- as.numeric(inf_obj$inf$hat)
      DFFITS <- as.numeric(inf_obj$inf$dffits)
    }

    denom        <- (k_mf - p_mf - 1)
    cooks_thresh <- if (is.finite(denom) && denom > 0) 4 / denom else NA_real_

    cooks_tbl <- tibble::tibble(
      citation      = trimws(as.character(mf$slab)),
      CooksD        = CooksD, Hat = Hat, DFFITS = DFFITS,
      CooksD_Thresh = cooks_thresh,
      Flag_CooksD   = is.finite(cooks_thresh) & is.finite(CooksD) & (CooksD > cooks_thresh)
    )

    loo_tbl <- loo_tbl %>%
      dplyr::mutate(citation = trimws(citation)) %>%
      dplyr::left_join(cooks_tbl, by = "citation")
  }

  loo_tbl <- loo_tbl %>%
    dplyr::mutate(
      Flag_FlipDirection = is.finite(full_TE) & is.finite(Effect) &
        (sign(full_TE) != sign(Effect)),
      Flag_OutsideFullCI = is.finite(Effect) & (Effect < full_lo | Effect > full_hi),
      Influential        = dplyr::coalesce(Flag_CooksD, FALSE) |
        dplyr::coalesce(Flag_OutsideFullCI, FALSE) |
        dplyr::coalesce(Flag_FlipDirection, FALSE)
    )

  influence_results_list[[k_inf]] <- loo_tbl
  k_inf <- k_inf + 1
}

summary_influence_overall <- if (length(influence_results_list))
  dplyr::bind_rows(influence_results_list) else tibble::tibble()


# =============================================================================
# 15. OUTLIER ANALYSIS (OVERALL) ####
# =============================================================================

full_outlier_list <- list()
k_out <- 1

for (i in seq_len(nrow(data_subsets_labels))) {
  outcome_i <- data_subsets_labels$measure_composite[i]
  df_i      <- apply_main_inclusion_filters(data_subsets[[i]], outcome_i)
  te_i      <- data_subsets_labels$te_var[i]
  se_i      <- data_subsets_labels$se_var[i]

  k_i <- sum(is.finite(df_i[[te_i]]) & is.finite(df_i[[se_i]]))
  if (k_i < 5) next

  res <- try(
    full_outlier_analysis(
      data         = df_i,
      outcome_name = outcome_i,
      effect_type  = data_subsets_labels$effect_type[i],
      te_var       = te_i,
      se_var       = se_i,
      smlab_text   = outcome_i,
      file_suffix  = "OutliersRemoved",
      output_dir   = output_mainfor_sens_dir
    ),
    silent = TRUE
  )

  if (inherits(res, "try-error")) {
    message("Outlier analysis failed for ", outcome_i, ": ", as.character(res))
    next
  }
  if (is.null(res)) {
    message("Outlier analysis returned NULL for ", outcome_i)
    next
  }

  full_outlier_list[[k_out]] <- res
  k_out <- k_out + 1
}


# =============================================================================
# 16. SUBGROUP ANALYSIS LOOP ####
# =============================================================================

# Subgroup variables are listed in order of their appearance in the manuscript.
# Energy-balance variables are handled separately (see restrict_WL_for_subgroups).

subgroup_vars <- c(
  "meat_type", "sex3", "obesity", "anyexercise", "age_status", "protein_matched",
  "energy_balance_grp", "energy_balance_subjects", "lean",
  "duration", "region", "lov", "bckgrd_redmeat",
  "actual_EB_grp2", "actual_EB_grp", "actual_EB_subjects"
)

message("Starting subgroup analysis loop...")

subgroup_results_list <- list()
k_sg <- 1

for (j in seq_along(subgroup_vars)) {
  for (i in seq_len(nrow(data_subsets_labels))) {

    outcome_i <- data_subsets_labels$measure_composite[i]
    sg        <- subgroup_vars[j]

    df_i <- data_subsets[[i]] %>%
      restrict_WL_for_subgroups(subgroup_var = sg) %>%
      exclude_citations_by_outcome(outcome_name = outcome_i) %>%
      filter_leidy_campbell(outcome_name = outcome_i, subgroup_var = sg)

    res <- try(
      run_meta_subgroup_analysis(
        data         = df_i,
        outcome_name = outcome_i,
        effect_type  = data_subsets_labels$effect_type[i],
        subgroup_var = sg,
        te_var       = data_subsets_labels$te_var[i],
        se_var       = data_subsets_labels$se_var[i],
        xlim_vals    = NULL,
        smlab_text   = outcome_i,
        file_suffix  = "subgroup",
        output_dir   = output_subfor_dir
      ),
      silent = TRUE
    )

    if (inherits(res, "try-error")) {
      message("ERROR: ", outcome_i, " | ", sg, " | ", as.character(res))
      next
    }
    if (is.null(res) || is.null(res$summary)) next

    subgroup_results_list[[k_sg]] <- res$summary
    k_sg <- k_sg + 1
  }
}


# =============================================================================
# 17. SUBGROUP OUTLIER ANALYSIS LOOP ####
# =============================================================================

subgroup_outlier_list <- list()
k_so <- 1

for (j in seq_along(subgroup_vars)) {
  sg <- subgroup_vars[j]

  for (i in seq_len(nrow(data_subsets_labels))) {
    outcome_i <- data_subsets_labels$measure_composite[i]

    df_i <- data_subsets[[i]] %>%
      restrict_WL_for_subgroups(subgroup_var = sg) %>%
      exclude_citations_by_outcome(outcome_name = outcome_i) %>%
      filter_leidy_campbell(outcome_name = outcome_i, subgroup_var = sg)

    te_i <- data_subsets_labels$te_var[i]
    se_i <- data_subsets_labels$se_var[i]
    k_i  <- sum(is.finite(df_i[[te_i]]) & is.finite(df_i[[se_i]]))
    if (k_i < 5) next

    res <- tryCatch(
      subgroup_outlier_analysis(
        data         = df_i,
        outcome_name = outcome_i,
        effect_type  = data_subsets_labels$effect_type[i],
        subgroup_var = sg,
        te_var       = te_i,
        se_var       = se_i,
        xlim_vals    = NULL,
        smlab_text   = outcome_i,
        file_suffix  = "Subgroup_OutliersRemoved",
        output_dir   = output_subfor_sens_dir
      ),
      error = function(e) {
        message("subgroup_outlier_analysis failed: ", conditionMessage(e))
        NULL
      }
    )

    if (!is.null(res)) {
      subgroup_outlier_list[[k_so]] <- res
      k_so <- k_so + 1
    }
  }
}


# =============================================================================
# 18. PUBLICATION BIAS (FUNNEL PLOTS, EGGER, BEGG) ####
# =============================================================================

# run_funnels_bias_export() produces a contour-enhanced funnel plot for each
# outcome with k >= k_min studies, runs Egger's linear regression test and
# (optionally) Begg's rank correlation test, and exports results to Excel.

run_funnels_bias_export <- function(main_results_list,
                                    output_dir,
                                    file_suffix  = "Funnel",
                                    contour      = TRUE,
                                    do_begg      = TRUE,
                                    add_trimfill = FALSE,
                                    plot_dim     = c(8, 6),
                                    trim_png     = TRUE,
                                    excel_name   = "Funnel_Egger_Begg_Summary.xlsx",
                                    k_min        = 6) {

  for (pkg in c("meta", "tibble", "dplyr", "purrr", "writexl")) {
    if (!requireNamespace(pkg, quietly = TRUE))
      stop("Package '", pkg, "' is required.")
  }
  has_magick  <- requireNamespace("magick",  quietly = TRUE)
  has_metafor <- requireNamespace("metafor", quietly = TRUE)

  sround  <- function(x, d) { o <- suppressWarnings(as.numeric(x)); ifelse(is.finite(o), round(o, d), NA_real_) }
  get1num <- function(x) { x <- suppressWarnings(as.numeric(x)); if (length(x) == 0) NA_real_ else x[1] }

  if (is.null(output_dir)) stop("output_dir must be specified.")
  if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

  grDevices::graphics.off()
  bias_rows <- list()

  for (i in seq_along(main_results_list)) {
    res_i   <- main_results_list[[i]]
    if (is.null(res_i)) next
    model_i <- try(attr(res_i, "model"), silent = TRUE)
    if (inherits(model_i, "try-error") || is.null(model_i)) next

    outcome_i <- try(as.character(res_i$Outcome[1]), silent = TRUE)
    if (inherits(outcome_i, "try-error") || is.na(outcome_i))
      outcome_i <- paste0("Outcome_", i)

    k_i <- model_i$k
    if (is.null(k_i) || k_i < k_min) {
      message("Skipping funnel/bias for ", outcome_i, " (k=", k_i, " < ", k_min, ")")
      next
    }

    message("Running funnel + bias tests for: ", outcome_i, " (k=", k_i, ")")
    png_file <- file.path(output_dir, paste0(outcome_i, "_", file_suffix, ".png"))

    grDevices::png(filename = png_file, width = plot_dim[1], height = plot_dim[2],
                   units = "in", res = 300)
    op <- par(mar = c(4, 4, 3, 2))

    tryCatch({
      if (isTRUE(contour)) {
        meta::funnel(model_i, contour = c(0.90, 0.95, 0.99), backtransf = FALSE,
                     pch = 21, col = "black", bg = "white",
                     xlab = paste0(model_i$sm, " (effect)"), ylab = "Standard Error")
      } else {
        meta::funnel(model_i, backtransf = FALSE,
                     pch = 21, col = "black", bg = "white",
                     xlab = paste0(model_i$sm, " (effect)"), ylab = "Standard Error")
      }
      if (isTRUE(add_trimfill) && k_i >= 4) {
        tf <- try(meta::trimfill(model_i), silent = TRUE)
        if (!inherits(tf, "try-error")) points(tf$TE, tf$seTE, pch = 21, col = "grey40")
      }
    }, finally = {
      try(grDevices::dev.off(), silent = TRUE)
      try(par(op),              silent = TRUE)
    })

    if (isTRUE(trim_png) && has_magick) {
      img <- magick::image_read(png_file)
      img <- magick::image_trim(img)
      img <- magick::image_border(img, color = "white", geometry = "20x20")
      magick::image_write(img, path = png_file)
    }

    # Egger's test (linear regression of standardised effect on precision)
    eg_int <- eg_se <- eg_p <- NA_real_
    eg <- try(meta::metabias(model_i, method.bias = "linreg", plotit = FALSE), silent = TRUE)
    if (!inherits(eg, "try-error")) {
      if (!is.null(eg$bias))    eg_int <- as.numeric(eg$bias)
      if (!is.null(eg$se.bias)) eg_se  <- as.numeric(eg$se.bias)
      if (!is.null(eg$p.value)) eg_p   <- as.numeric(eg$p.value)
    }

    # Fallback: manual Egger's regression if metabias() could not compute it
    if (any(is.na(c(eg_int, eg_se, eg_p)))) {
      yi  <- as.numeric(model_i$TE)
      sei <- as.numeric(model_i$seTE)
      ok  <- is.finite(yi) & is.finite(sei) & sei > 0
      if (sum(ok) >= k_min) {
        df  <- data.frame(SND = yi[ok] / sei[ok], Precision = 1 / sei[ok])
        fit <- try(lm(SND ~ Precision, data = df), silent = TRUE)
        if (!inherits(fit, "try-error")) {
          s      <- summary(fit)
          eg_int <- coef(s)[1, "Estimate"]
          eg_se  <- coef(s)[1, "Std. Error"]
          eg_p   <- coef(s)[1, "Pr(>|t|)"]
        }
      }
    }

    # Begg's rank correlation test
    begg_tau <- begg_p <- NA_real_
    if (isTRUE(do_begg)) {
      bg <- try(meta::metabias(model_i, method.bias = "rank", plotit = FALSE), silent = TRUE)
      if (!inherits(bg, "try-error")) {
        if (!is.null(bg$estimate)) begg_tau <- get1num(bg$estimate)
        if (!is.null(bg$p.value))  begg_p   <- get1num(bg$p.value)
      }
      # Fallback via metafor::ranktest()
      if ((!is.finite(get1num(begg_tau)) || !is.finite(get1num(begg_p))) && has_metafor) {
        yi <- as.numeric(model_i$TE)
        vi <- as.numeric(model_i$seTE^2)
        ok <- is.finite(yi) & is.finite(vi) & vi > 0
        if (sum(ok) >= k_min) {
          rt2 <- try(metafor::ranktest(yi[ok], vi[ok]), silent = TRUE)
          if (!inherits(rt2, "try-error")) {
            if (!is.null(rt2$tau))  begg_tau <- get1num(rt2$tau)
            if (!is.null(rt2$pval)) begg_p   <- get1num(rt2$pval)
          }
        }
      }
    }

    bias_rows[[length(bias_rows) + 1]] <- tibble::tibble(
      Outcome         = outcome_i,
      k               = k_i,
      SM              = as.character(model_i$sm),
      Egger_intercept = sround(eg_int, 3),
      Egger_SE        = sround(eg_se, 3),
      Egger_p         = sround(eg_p, 3),
      Begg_tau        = sround(begg_tau, 3),
      Begg_p          = sround(begg_p, 3),
      method.tau      = as.character(model_i$method.tau),
      method.random   = as.character(model_i$method.random)
    )
  }

  bias_summary <- dplyr::bind_rows(purrr::compact(bias_rows)) %>%
    dplyr::arrange(Outcome)

  if (nrow(bias_summary) > 0) {
    out_xlsx <- file.path(output_dir, excel_name)
    writexl::write_xlsx(bias_summary, out_xlsx)
    message("Bias summary written to: ", out_xlsx)
  } else {
    message("No eligible outcomes (k >= ", k_min, ") for bias tests.")
  }

  return(bias_summary)
}

bias_summary <- run_funnels_bias_export(
  main_results_list = main_results_list,
  output_dir        = output_funnel_dir,
  contour           = TRUE,
  do_begg           = TRUE,
  add_trimfill      = FALSE,
  k_min             = 6
)


# =============================================================================
# 19. EXPORT ALL NUMERIC RESULTS TO EXCEL ####
# =============================================================================

summary_main_outcomes <- dplyr::bind_rows(main_results_list)

summary_subgroup_outcomes <- dplyr::bind_rows(subgroup_results_list) %>%
  arrange(Outcome, Subgroup_Var, Subgroup_Level) %>%
  distinct(Outcome, Subgroup_Var, Subgroup_Level, .keep_all = TRUE)

# Combine subgroup and LOO/influence results under a shared schema
summary_subgroup_and_influence <- dplyr::bind_rows(
  summary_subgroup_outcomes,
  summary_influence_overall
)

summary_outliers_overall  <- if (length(full_outlier_list))
  dplyr::bind_rows(full_outlier_list) else tibble::tibble()

summary_outliers_subgroup <- if (length(subgroup_outlier_list))
  dplyr::bind_rows(subgroup_outlier_list) else tibble::tibble()

xlsx_path <- file.path(output_mainexcel_dir, "All_Results.xlsx")

writexl::write_xlsx(
  list(
    Overall            = summary_main_outcomes,
    Subgroup_Influence = summary_subgroup_and_influence,
    Outliers_Overall   = summary_outliers_overall,
    Outliers_Subgroups = summary_outliers_subgroup,
    Funnel_bias        = bias_summary,
    RoB_Sensitivity    = rob_sensitivity_comparison
  ),
  path = xlsx_path
)

message("All results written to: ", normalizePath(xlsx_path))


# =============================================================================
# END OF SCRIPT ####
# =============================================================================
