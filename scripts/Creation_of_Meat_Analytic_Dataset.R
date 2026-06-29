# =============================================================================
# Meat Intake and Body Composition: Systematic Review and Meta-Analysis
# Peregoy JA, Fleming RA, Leidy HJ, Fleming SA
# Traverse Science | 2025
#
# This script reads the raw extraction database, cleans and standardises the
# data, calculates within-group change scores and between-group effect sizes
# (MD, SMD via metafor::escalc), and writes the analytic dataset used by
# Analysis_Meat_SRMA.R.
#
# Input:  Meat_Extraction_Database.xlsx (same folder as this script)
#           Sheet "Group Data"   -- group-level outcome measurements
#           Sheet "Review Data"  -- study-level characteristics
#           Sheet "Comparators"  -- intervention vs. comparator group pairings
#
# Output: Meat_Analytic_Dataset.xlsx (written to the same folder)
#
# Key analytic decisions documented in-line:
#   - IQR-to-SD conversion:  SD = (Q3 - Q1) / 1.35
#   - 95% CI-to-SD conversion: SD = sqrt(n) * |UL - LL| / 3.92
#   - Median-to-mean transformation: mean = (Q1 + median + Q3) / 3
#   - Within-subject correlation: r = 0.85 (imputed; see manuscript Methods)
#   - Yamashita 1998 hip circumference SD: delta method (first-order Taylor expansion)
#   - Daly/Formica/Griffin/Mitchell T1 values: reconstructed from reported % change
# =============================================================================


# =============================================================================
# 0. SET WORKING DIRECTORY ####
# =============================================================================

setwd("/Users/jenniferperegoy/Library/CloudStorage/OneDrive-SharedLibraries-TraverseScience,Inc/Internal - Documents/Projects/2024-02 Meat SRMA/Database/Github")


# =============================================================================
# 0. PACKAGES ####
# =============================================================================

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
  "tidyverse", "ggplot2", "readxl", "ggpubr", "gmodels", "grid", "gtable",
  "vcd", "cowplot", "ggExtra", "viridis", "stringr", "meta", "metafor",
  "pwr", "writexl", "dplyr", "furniture", "data.table", "psych", "skimr",
  "gtsummary", "kableExtra", "magick", "webshot2", "corrplot", "Hmisc",
  "purrr", "knitr", "flextable", "janitor"
))


# =============================================================================
# 1. READ DATA ####
# =============================================================================

data_outcomes <- read_excel(
  "data/Meat_Extraction_Database.xlsx",
  sheet     = "Group Data",
  col_names = TRUE
)
data_review <- read_excel(
  "data/Meat_Extraction_Database.xlsx",
  sheet     = "Review Data",
  col_names = TRUE
)
data_comp <- read_excel(
  "data/Meat_Extraction_Database.xlsx",
  sheet     = "Comparators",
  col_names = TRUE
)

# Standardise column names to lower snake_case across all three sheets
data_outcomes <- data_outcomes %>% clean_names()
data_comp     <- data_comp     %>% clean_names()
data_review   <- data_review   %>% clean_names()

# Keep only primary comparisons in the Comparators sheet
data_comp <- data_comp %>% filter(primary_comparison == "Y")


# =============================================================================
# 2. INITIAL CLEANING OF GROUP DATA ####
# =============================================================================

## 2a. Remove rows that cause downstream merging problems ####
# Haub 2002:    time == -3 is a pre-run-in timepoint, not a valid baseline
# Murphy 2014:  % lean mass rows are duplicates in a different unit
# Mitchell 2021: Tables 1 and 4 contain carryover measures, not primary outcomes
# Griffin 2013: "Loss of initial..." rows are redundant % change presentations;
#               subject characteristics rows are not outcomes

data_outcomes <- data_outcomes %>%
  filter(
    !(citation == "Haub 2002"     & time == -3),
    !(citation == "Murphy 2014"   & measure_reported == "% Lean mass"),
    !(citation == "Mitchell 2021" & source %in% c("Table 1", "Table 4")),
    !(citation == "Griffin 2013"  & measure_reported %in%
        c("Loss of initial fat", "Loss of initial weight")),
    !(citation == "Griffin 2013"  & measure_domain == "Subject characteristics")
  )


## 2b. Force numeric types ####

# Group-level outcome variables
outcome_num_cols <- c(
  "time", "n", "value", "se", "sd", "lower", "upper",
  "value_converted", "se_converted", "sd_converted",
  "lower_converted", "upper_converted"
)
data_outcomes[, outcome_num_cols] <- lapply(data_outcomes[, outcome_num_cols],
                                            as.numeric)

# Study-level dietary intake and design variables
review_num_cols <- c(
  "sample_size_randomized", "max_intervention_length_days",
  "intervention_duration_weeks", "mean_baseline_bmi",
  "trt_protein_intake_percent_e", "comp_protein_intake_percent_e",
  "trt_protein_intake_g_d",       "comp_protein_intake_g_d",
  "trt_protein_intake_g_kg_d",    "comp_protein_intake_g_kg_d",
  "trt_carb_intake_percent_e",    "comp_carb_intake_percent_e",
  "trt_carb_intake_g_d",          "comp_carb_intake_g_d",
  "trt_fat_intake_percent_e",     "comp_fat_intake_percent_e",
  "trt_fat_intake_g_d",           "comp_fat_intake_g_d",
  "trt_fiber_intake_g_d",         "comp_fiber_intake_g_d",
  "trt_tei_kcal_d",               "comp_tei_kcal_d",
  "tei_diff_kcal", "protein_difference_percent_e", "protein_difference_g_d",
  "trt_sum_percent_e", "comp_sum_percent_e"
)
data_review[, review_num_cols] <- lapply(data_review[, review_num_cols],
                                         as.numeric)


# =============================================================================
# 3. IMPUTE YAMASHITA 1998 HIP CIRCUMFERENCE SD ####
# =============================================================================

# Yamashita 1998 did not report hip circumference SD directly; it was derived
# from the reported waist circumference and waist-to-hip ratio values using a
# first-order Taylor series expansion (delta method):
#
#   SD_hip = sqrt( SD_wc^2 / WHR^2  +  WC^2 * SD_whr^2 / WHR^4 )
#
# where WC = waist circumference, WHR = waist-to-hip ratio.

id_cols <- c("index", "citation", "group_id", "time")

hip_sd_yamashita <- data_outcomes %>%
  filter(
    citation      == "Yamashita 1998",
    measure_cleaned %in% c("Waist circumference", "Waist-hip ratio")
  ) %>%
  dplyr::select(all_of(id_cols), measure_cleaned, value, sd,
                value_converted, sd_converted) %>%
  pivot_wider(
    names_from  = measure_cleaned,
    values_from = c(value, sd, value_converted, sd_converted),
    names_glue  = "{measure_cleaned}_{.value}"
  ) %>%
  mutate(across(c(contains("_value"), contains("_sd")), as.numeric)) %>%
  mutate(
    sd_hip_calc = sqrt(
      (`Waist circumference_sd`^2) / (`Waist-hip ratio_value`^2) +
        (`Waist circumference_value`^2 * `Waist-hip ratio_sd`^2) /
        (`Waist-hip ratio_value`^4)
    ),
    sd_hip_calc_converted = sqrt(
      (`Waist circumference_sd_converted`^2) /
        (`Waist-hip ratio_value_converted`^2) +
        (`Waist circumference_value_converted`^2 *
           `Waist-hip ratio_sd_converted`^2) /
        (`Waist-hip ratio_value_converted`^4)
    )
  ) %>%
  dplyr::select(all_of(id_cols), sd_hip_calc, sd_hip_calc_converted)

# Merge the imputed SDs back and replace the placeholder zeros
data_outcomes <- data_outcomes %>%
  left_join(hip_sd_yamashita, by = id_cols) %>%
  mutate(
    sd = if_else(
      citation == "Yamashita 1998" & measure_cleaned == "Hip circumference" & sd == 0,
      sd_hip_calc, sd
    ),
    sd_converted = if_else(
      citation == "Yamashita 1998" & measure_cleaned == "Hip circumference" & sd_converted == 0,
      sd_hip_calc_converted, sd_converted
    )
  ) %>%
  dplyr::select(-sd_hip_calc, -sd_hip_calc_converted)


# =============================================================================
# 4. VARIANCE CONVERSIONS ####
# =============================================================================

## 4a. IQR → SD ####
# Formula: SD = (Q3 - Q1) / 1.35  (Wan et al. 2014)
# SE is then recalculated as SD / sqrt(n)

data_outcomes <- data_outcomes %>%
  mutate(
    sd = case_when(range_type == "IQR" ~ (upper - lower) / 1.35, TRUE ~ sd),
    se = case_when(range_type == "IQR" ~ sd / sqrt(n),           TRUE ~ se),
    sd_converted = case_when(
      range_type == "IQR" ~ (upper_converted - lower_converted) / 1.35,
      TRUE ~ sd_converted
    ),
    se_converted = case_when(
      range_type == "IQR" ~ sd_converted / sqrt(n),
      TRUE ~ se_converted
    )
  )

## 4b. Median → mean ####
# Formula: mean ≈ (Q1 + median + Q3) / 3  (Luo et al. 2018)

data_outcomes <- data_outcomes %>%
  mutate(
    value = case_when(
      statistic == "Median" ~ (upper + lower + value) / 3,
      TRUE ~ value
    ),
    value_converted = case_when(
      statistic == "Median" ~ (upper_converted + lower_converted + value_converted) / 3,
      TRUE ~ value_converted
    )
  )

# Update labels to reflect the transformations
data_outcomes$statistic[data_outcomes$statistic == "Median"]  <- "Mean transformed (Median)"
data_outcomes$range_type[data_outcomes$range_type == "IQR"]   <- "SD transformed (IQR)"

## 4c. 95% CI → SD ####
# Formula: SD = sqrt(n) * |UL - LL| / 3.92  (Higgins et al. 2023 Cochrane Handbook)

data_outcomes <- data_outcomes %>%
  mutate(
    sd = case_when(
      range_type == "95% CI" ~ sqrt(n) * abs(upper - lower) / 3.92,
      TRUE ~ sd
    ),
    se = case_when(range_type == "95% CI" ~ sd / sqrt(n), TRUE ~ se),
    sd_converted = case_when(
      range_type == "95% CI" ~ sqrt(n) * abs(upper_converted - lower_converted) / 3.92,
      TRUE ~ sd_converted
    ),
    se_converted = case_when(
      range_type == "95% CI" ~ sd_converted / sqrt(n),
      TRUE ~ se_converted
    )
  )

data_outcomes$range_type[data_outcomes$range_type == "95% CI"] <- "SD transformed (95% CI)"

## 4d. Fill any remaining missing SEs from SD / sqrt(n) ####

data_outcomes <- data_outcomes %>%
  mutate(
    sd_clean = suppressWarnings(as.numeric(gsub(",", "", sd))),
    n_clean  = suppressWarnings(as.numeric(gsub(",", "", n))),
    se = case_when(
      !is.na(se)                                           ~ se,
      is.na(se) & !is.na(sd_clean) & !is.na(n_clean) &
        n_clean > 0                                        ~ sd_clean / sqrt(n_clean),
      TRUE ~ NA_real_
    ),
    sd_conv_clean = suppressWarnings(as.numeric(gsub(",", "", sd_converted))),
    se_converted = case_when(
      !is.na(se_converted)                                              ~ se_converted,
      is.na(se_converted) & !is.na(sd_conv_clean) & !is.na(n_clean) &
        n_clean > 0                                                     ~ sd_conv_clean / sqrt(n_clean),
      TRUE ~ NA_real_
    )
  ) %>%
  dplyr::select(-sd_clean, -n_clean, -sd_conv_clean)


# =============================================================================
# 5. RESHAPE TO WIDE FORMAT (BASELINE + FOLLOW-UP ON SAME ROW) ####
# =============================================================================

# Studies with multiple follow-up timepoints: keep BOTH the midpoint and the
# endpoint so that each phase can contribute a separate row to the analysis.
keep_all_times <- c("Magkos 2022", "Benassi-Evans 2009")

# Remove the secondary index column; not needed downstream
data_outcomes <- data_outcomes %>% dplyr::select(-s_index)

# Keys that define a unique study/subgroup/outcome combination
by_keys <- c(
  "citation", "index", "group_id", "sub_group1", "primary_report",
  "study_type", "study_design", "analytical_population",
  "measure_cleaned", "measure_binned", "adjustments", "comparison",
  "measure_domain", "measure_subdomain", "correlation"
)

## 5a. Baseline row (T0): earliest timepoint per group/outcome ####
min_by_group <- data_outcomes %>%
  filter(comparison == "Summary", measure_domain == "Outcome") %>%
  group_by(across(all_of(by_keys))) %>%
  slice_min(time, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  rename(
    value_t0 = value, se_t0 = se, sd_t0 = sd, unit_t0 = unit,
    lower_t0 = lower, upper_t0 = upper, range_type_t0 = range_type,
    n_t0 = n, statistic_t0 = statistic, time_t0 = time,
    time_unit_t0 = time_unit,
    value_converted_t0 = value_converted, se_converted_t0 = se_converted,
    sd_converted_t0 = sd_converted, unit_converted_t0 = unit_converted,
    lower_converted_t0 = lower_converted, upper_converted_t0 = upper_converted
  )

## 5b. Follow-up row (T1) ####
# Regular studies: keep the endpoint (maximum time) only.
# Phased studies (Magkos 2022, Benassi-Evans 2009): keep the FIRST
# non-baseline timepoint as T1; the endpoint is handled separately as T2.

followups_regular <- data_outcomes %>%
  filter(comparison == "Summary", measure_domain == "Outcome",
         !citation %in% keep_all_times) %>%
  group_by(across(all_of(by_keys))) %>%
  slice_max(time, n = 1, with_ties = FALSE) %>%
  ungroup()

followups_special_first <- data_outcomes %>%
  filter(comparison == "Summary", measure_domain == "Outcome",
         citation %in% keep_all_times, time > 0) %>%
  group_by(across(all_of(by_keys))) %>%
  slice_min(time, n = 1, with_ties = FALSE) %>%
  ungroup()

followups_t1 <- bind_rows(followups_regular, followups_special_first) %>%
  rename(
    value_t1 = value, se_t1 = se, sd_t1 = sd, unit_t1 = unit,
    lower_t1 = lower, upper_t1 = upper, range_type_t1 = range_type,
    n_t1 = n, statistic_t1 = statistic, time_t1 = time,
    time_unit_t1 = time_unit,
    value_converted_t1 = value_converted, se_converted_t1 = se_converted,
    sd_converted_t1 = sd_converted, unit_converted_t1 = unit_converted,
    lower_converted_t1 = lower_converted, upper_converted_t1 = upper_converted
  ) %>%
  filter(time_t1 != 0)

## 5c. Join T0 and T1 (baseline to first follow-up) ####
data_change_baseline <- min_by_group %>%
  left_join(followups_t1, by = c(by_keys, "unit_t0" = "unit_t1"),
            multiple = "all") %>%
  distinct() %>%
  filter(!is.na(time_t1))

## 5d. Phased trials: midpoint (T1) to endpoint (T2) ####
special_all <- data_outcomes %>%
  filter(comparison == "Summary", measure_domain == "Outcome",
         citation %in% keep_all_times, time > 0)

# Endpoint: maximum post-baseline time
special_end <- special_all %>%
  group_by(across(all_of(by_keys))) %>%
  slice_max(time, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  rename(
    value_t1 = value, se_t1 = se, sd_t1 = sd, unit_t1 = unit,
    lower_t1 = lower, upper_t1 = upper, range_type_t1 = range_type,
    n_t1 = n, statistic_t1 = statistic, time_t1 = time,
    time_unit_t1 = time_unit,
    value_converted_t1 = value_converted, se_converted_t1 = se_converted,
    sd_converted_t1 = sd_converted, unit_converted_t1 = unit_converted,
    lower_converted_t1 = lower_converted, upper_converted_t1 = upper_converted
  )

# Midpoint: earliest post-baseline time (used as T0 for the second phase)
special_mid <- special_all %>%
  group_by(across(all_of(by_keys))) %>%
  slice_min(time, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  rename(
    value_t0 = value, se_t0 = se, sd_t0 = sd, unit_t0 = unit,
    lower_t0 = lower, upper_t0 = upper, range_type_t0 = range_type,
    n_t0 = n, statistic_t0 = statistic, time_t0 = time,
    time_unit_t0 = time_unit,
    value_converted_t0 = value_converted, se_converted_t0 = se_converted,
    sd_converted_t0 = sd_converted, unit_converted_t0 = unit_converted,
    lower_converted_t0 = lower_converted, upper_converted_t0 = upper_converted
  )

data_change_mid_to_end <- special_mid %>%
  left_join(special_end, by = c(by_keys, "unit_t0" = "unit_t1"),
            multiple = "all") %>%
  filter(!is.na(time_t1))

## 5e. Combine and label phases ####
data_change_all <- bind_rows(data_change_baseline, data_change_mid_to_end) %>%
  distinct() %>%
  rename(measure_reported = measure_reported.x) %>%
  dplyr::select(-measure_reported.y)

# Label weight-loss and weight-maintenance phases for phased trials
data_change_all <- data_change_all %>%
  mutate(sub_group1 = case_when(
    citation %in% keep_all_times & time_t0 == 0 ~ "Weight loss (T0-T1)",
    citation %in% keep_all_times & time_t0 > 0  ~ "Weight maintenance (T1-T2)",
    TRUE ~ sub_group1
  ))

# Drop rows spanning the full study period (T0=0 to T2=endpoint) for phased trials;
# these are handled as two separate phase rows above
data_change_all <- data_change_all %>%
  filter(
    !(citation == "Magkos 2022"        & time_t0 == 0  & time_t1 == 21),
    !(citation == "Benassi-Evans 2009" & time_t0 == 0  & time_t1 == 52)
  )

# Write specific phase labels
data_change_all <- data_change_all %>%
  mutate(sub_group1 = case_when(
    citation == "Magkos 2022"        & time_t0 == 0  & time_t1 == 8  ~
      "Weight loss (0-8 wks)",
    citation == "Magkos 2022"        & time_t0 == 8  & time_t1 == 21 ~
      "Weight maintenance (8-21 wks)",
    citation == "Benassi-Evans 2009" & time_t0 == 0  & time_t1 == 12 ~
      "Weight loss (0-12 wks)",
    citation == "Benassi-Evans 2009" & time_t0 == 12 & time_t1 == 52 ~
      "Weight maintenance (12-52 wks)",
    TRUE ~ sub_group1
  ))

# Haub 2002 thigh muscle CSA: use the author-reported within-group change
# value (comparison == "Within") rather than the reconstructed T0→T1 value
data_change_all <- data_change_all %>%
  filter(!(citation == "Haub 2002" &
             measure_cleaned == "Thigh muscle cross-sectional area"))


# =============================================================================
# 6. RECONSTRUCT ABSOLUTE T1 VALUES FOR STUDIES REPORTING % CHANGE ####
# =============================================================================

# Daly 2014, Formica 2020, Griffin 2013, and Mitchell 2021 reported within-group
# % changes (or absolute changes from baseline) rather than follow-up means.
# We reconstruct follow-up absolute values so that escalc() can compute MDs.
#
# For absolute change:   T1 = T0 + change
# For % change:          T1 = T0 * (1 + change / 100)
# SDs are scaled accordingly.

data_daly_min <- min_by_group %>%
  filter(
    citation %in% c("Daly 2014", "Formica 2020", "Griffin 2013", "Mitchell 2021"),
    measure_domain == "Outcome",
    !measure_subdomain %in% c("Muscles", "Bones")
  )

data_daly_max <- data_outcomes %>%
  filter(
    measure_domain == "Outcome",
    comparison     == "Within",
    !measure_subdomain %in% c("Muscles", "Bones"),
    (citation == "Daly 2014"     & time == 4)  |
      (citation == "Griffin 2013"  & time == 12) |
      (citation == "Mitchell 2021" & time == 12) |
      (citation == "Formica 2020"  & time == 24)
  ) %>%
  rename(
    value_t1 = value, se_t1 = se, sd_t1 = sd, unit_t1 = unit,
    lower_t1 = lower, upper_t1 = upper, range_type_t1 = range_type,
    n_t1 = n, statistic_t1 = statistic, time_t1 = time,
    time_unit_t1 = time_unit,
    value_converted_t1 = value_converted, se_converted_t1 = se_converted,
    sd_converted_t1 = sd_converted, unit_converted_t1 = unit_converted,
    lower_converted_t1 = lower_converted, upper_converted_t1 = upper_converted
  )

data_daly_change <- data_daly_min %>%
  left_join(data_daly_max,
            by = c("index", "citation", "group_id", "analytical_population",
                   "measure_cleaned", "sub_group1"),
            multiple = "all") %>%
  distinct() %>%
  dplyr::select(-any_of(c(
    "primary_report.y", "study_type.y", "study_design.y", "comparison.y",
    "measure_domain.y", "measure_subdomain.y", "measure_binned.y",
    "measure_reported.y", "correlation.y", "adjustments.y", "source.y"
  ))) %>%
  rename(
    study_type      = study_type.x,
    source          = source.x,
    adjustments     = adjustments.x,
    study_design    = study_design.x,
    comparison      = comparison.x,
    correlation     = correlation.x,
    measure_domain  = measure_domain.x,
    measure_subdomain = measure_subdomain.x,
    measure_binned  = measure_binned.x,
    measure_reported = measure_reported.x,
    primary_report  = primary_report.x
  )

# Reconstruct absolute T1 values from reported changes
data_daly_change <- data_daly_change %>%
  ungroup() %>%
  mutate(
    value_t1_absolute = case_when(
      unit_t0 == "%" ~ value_t0 + value_t1,
      unit_t1 %in% c("%, change-in", "% loss, change-in") ~
        value_t0 * (1 + value_t1 / 100),
      TRUE ~ value_t0 + value_t1
    ),
    value_converted_t1_absolute = case_when(
      unit_t0 == "%" ~ value_converted_t0 + value_converted_t1,
      unit_t1 %in% c("%, change-in", "% loss, change-in") ~
        value_converted_t0 * (1 + value_converted_t1 / 100),
      TRUE ~ value_converted_t0 + value_converted_t1
    ),
    # For % change: scale SD by T0 mean / 100 to convert to original units
    sd_t1_absolute = case_when(
      unit_t0 == "%" ~ sd_t1,
      unit_t1 %in% c("%, change-in", "% loss, change-in") ~
        value_t0 * (sd_t1 / 100),
      TRUE ~ sd_t1
    ),
    sd_converted_t1_absolute = case_when(
      unit_t0 == "%" ~ sd_converted_t1,
      unit_t1 %in% c("%, change-in", "% loss, change-in") ~
        value_converted_t0 * (sd_converted_t1 / 100),
      TRUE ~ sd_converted_t1
    ),
    se_t1_absolute = case_when(
      unit_t0 == "%" ~ se_t1,
      unit_t1 %in% c("%, change-in", "% loss, change-in") ~
        value_t0 * (se_t1 / 100),
      TRUE ~ se_t1
    ),
    se_converted_t1_absolute = case_when(
      unit_t0 == "%" ~ se_converted_t1,
      unit_t1 %in% c("%, change-in", "% loss, change-in") ~
        value_converted_t0 * (se_converted_t1 / 100),
      TRUE ~ se_converted_t1
    ),
    lower_t1_absolute = case_when(
      unit_t0 == "%" ~ value_t0 + lower_t1,
      unit_t1 %in% c("%, change-in", "% loss, change-in") ~
        value_t0 * (1 + lower_t1 / 100),
      TRUE ~ value_t0 + lower_t1
    ),
    lower_converted_t1_absolute = case_when(
      unit_t0 == "%" ~ value_converted_t0 + lower_converted_t1,
      unit_t1 %in% c("%, change-in", "% loss, change-in") ~
        value_converted_t0 * (1 + lower_converted_t1 / 100),
      TRUE ~ value_converted_t0 + lower_converted_t1
    ),
    upper_t1_absolute = case_when(
      unit_t0 == "%" ~ value_t0 + upper_t1,
      unit_t1 %in% c("%, change-in", "% loss, change-in") ~
        value_t0 * (1 + upper_t1 / 100),
      TRUE ~ value_t0 + upper_t1
    ),
    upper_converted_t1_absolute = case_when(
      unit_t0 == "%" ~ value_converted_t0 + upper_converted_t1,
      unit_t1 %in% c("%, change-in", "% loss, change-in") ~
        value_converted_t0 * (1 + upper_converted_t1 / 100),
      TRUE ~ value_converted_t0 + upper_converted_t1
    )
  ) %>%
  # Replace original T1 values and update units to match T0
  mutate(
    value_t1           = value_t1_absolute,
    value_converted_t1 = value_converted_t1_absolute,
    sd_t1              = sd_t1_absolute,
    sd_converted_t1    = sd_converted_t1_absolute,
    se_t1              = se_t1_absolute,
    se_converted_t1    = se_converted_t1_absolute,
    lower_t1           = lower_t1_absolute,
    lower_converted_t1 = lower_converted_t1_absolute,
    upper_t1           = upper_t1_absolute,
    upper_converted_t1 = upper_converted_t1_absolute,
    unit_t1            = unit_t0,
    unit_converted_t1  = unit_converted_t0
  ) %>%
  dplyr::select(-ends_with("_absolute"))

# Replace these citations in the main dataset with the reconstructed values
data_change_all <- data_change_all %>%
  filter(!(citation %in% c("Daly 2014", "Griffin 2013", "Formica 2020", "Mitchell 2021") &
             !measure_subdomain %in% c("Muscles", "Bones"))) %>%
  bind_rows(data_daly_change)

# Set within-subject correlation for all change-score calculations
# r = 0.85 (empirically derived from included outcomes; see Methods)
data_change_all <- data_change_all %>% mutate(correlation = 0.85)


# =============================================================================
# 7. CALCULATE PERCENT CHANGE AND SD OF PERCENT CHANGE ####
# =============================================================================

# % change = (T1 - T0) / T0 * 100
# SD of % change using the propagation-of-error formula:
#   SD_pct = sqrt(SD_T0^2 + SD_T1^2 - 2 * r * SD_T0 * SD_T1) / T0 * 100
# r = 0.90 is used here (conservative choice; gives largest SD_pct)

data_change_all <- data_change_all %>%
  mutate(
    pct_change = ifelse(
      !is.na(value_t0) & !is.na(value_t1) & value_t0 != 0,
      (value_t1 - value_t0) / value_t0 * 100,
      NA_real_
    ),
    pct_change_converted = ifelse(
      !is.na(value_converted_t0) & !is.na(value_converted_t1) &
        value_converted_t0 != 0,
      (value_converted_t1 - value_converted_t0) / value_converted_t0 * 100,
      NA_real_
    ),
    # SD of % change at r = 0.90 (used in the primary analysis)
    sd_pct_change_r90 = ifelse(
      !is.na(sd_t0) & !is.na(sd_t1) & value_t0 != 0,
      sqrt(sd_t0^2 + sd_t1^2 - 2 * 0.90 * sd_t0 * sd_t1) / value_t0 * 100,
      NA_real_
    ),
    sd_pct_change_converted_r90 = ifelse(
      !is.na(sd_converted_t0) & !is.na(sd_converted_t1) & value_converted_t0 != 0,
      sqrt(sd_converted_t0^2 + sd_converted_t1^2 -
             2 * 0.90 * sd_converted_t0 * sd_converted_t1) / value_converted_t0 * 100,
      NA_real_
    )
  )


# =============================================================================
# 8. PAIR INTERVENTION AND CONTROL GROUPS ON THE SAME ROW ####
# =============================================================================

# Create a unique group identifier combining index, group_id, sub_group1, and
# analytical_population to handle multi-arm trials and subgroups unambiguously
data_change_all$ugid <- paste(
  data_change_all$index, ": ",
  data_change_all$group_id, " ",
  data_change_all$sub_group1,
  data_change_all$analytical_population
)

# Variable IDs for matching pre- and post-intervention observations
data_change_all <- data_change_all %>%
  mutate(
    pre_varid  = paste(analytical_population, adjustments, comparison, sub_group1,
                       measure_domain, measure_reported,
                       time_t0, time_unit_t0, statistic_t0, unit_converted_t0),
    post_varid = paste(analytical_population, adjustments, comparison, sub_group1,
                       measure_domain, measure_reported,
                       time_t1, time_unit_t1, statistic_t0, unit_converted_t1)
  )

## 8a. Match intervention groups ####
data_MA <- data_comp %>%
  left_join(
    data_change_all %>% dplyr::select(index, citation, group_id, sub_group1, ugid),
    by = c("citation", "index", "group_id"),
    multiple = "all"
  ) %>%
  distinct() %>%
  dplyr::select(-group_id) %>%
  filter(!is.na(ugid))

## 8b. Match control groups (comparator) ####
data_MA <- data_MA %>%
  left_join(
    data_change_all %>%
      dplyr::select(index, citation, group_id, sub_group1, ugid,
                    measure_cleaned, analytical_population, measure_domain),
    by = c("index", "citation", "sub_group1", "comparator" = "group_id"),
    multiple = "all"
  ) %>%
  distinct() %>%
  rename(ugid_ctrl = ugid.y, ugid_int = ugid.x) %>%
  dplyr::select(-comparator)

## 8c. Merge intervention group data ####
data_MA <- data_MA %>%
  left_join(
    data_change_all,
    by = c("index", "citation", "ugid_int" = "ugid",
           "measure_cleaned", "analytical_population",
           "measure_domain", "sub_group1")
  ) %>%
  distinct() %>%
  mutate(
    value_int_t0 = value_converted_t0, sd_int_t0 = sd_converted_t0,
    se_int_t0    = se_converted_t0,    n_int_t0   = n_t0,
    lower_int_t0 = lower_converted_t0, upper.int_t0 = upper_converted_t0,
    unit_int_t0  = unit_converted_t0,
    value_int_t1 = value_converted_t1, sd_int_t1 = sd_converted_t1,
    se_int_t1    = se_converted_t1,    n_int_t1   = n_t1,
    lower_int_t1 = lower_converted_t1, upper.int_t1 = upper_converted_t1,
    unit_int_t1  = unit_converted_t1,
    pct_change_int    = pct_change_converted,
    sd_pct_change_int = sd_pct_change_converted_r90
  ) %>%
  dplyr::select(!c(
    value_t0, sd_t0, se_t0, n_t0, lower_t0, upper_t0, range_type_t0,
    value_converted_t0, sd_converted_t0, se_converted_t0,
    lower_converted_t0, upper_converted_t0, unit_converted_t0,
    value_t1, sd_t1, se_t1, n_t1, lower_t1, upper_t1, unit_t0,
    range_type_t1, value_converted_t1, sd_converted_t1, se_converted_t1,
    lower_converted_t1, upper_converted_t1, unit_converted_t1,
    source.y, source.x, pct_change_converted, pct_change,
    sd_pct_change_r90, sd_pct_change_converted_r90
  ))

## 8d. Merge control group data ####
data_MA <- data_MA %>%
  left_join(
    data_change_all,
    by = c("index", "citation", "measure_cleaned",
           "ugid_ctrl" = "ugid", "analytical_population",
           "adjustments", "sub_group1"),
    keep = FALSE
  ) %>%
  distinct() %>%
  mutate(
    value_ctrl_t0 = value_converted_t0, sd_ctrl_t0 = sd_converted_t0,
    se_ctrl_t0    = se_converted_t0,    n_ctrl_t0   = n_t0,
    lower_ctrl_t0 = lower_converted_t0, upper.ctrl_t0 = upper_converted_t0,
    unit_ctrl_t0  = unit_converted_t0,
    value_ctrl_t1 = value_converted_t1, sd_ctrl_t1 = sd_converted_t1,
    se_ctrl_t1    = se_converted_t1,    n_ctrl_t1   = n_t1,
    lower_ctrl_t1 = lower_converted_t1, upper.ctrl_t1 = upper_converted_t1,
    unit_ctrl_t1  = unit_converted_t1,
    pct_change_ctrl    = pct_change_converted,
    sd_pct_change_ctrl = sd_pct_change_converted_r90
  ) %>%
  dplyr::select(!c(
    value_t0, sd_t0, se_t0, n_t0, lower_t0, upper_t0, range_type_t0,
    value_converted_t0, sd_converted_t0, se_converted_t0,
    lower_converted_t0, upper_converted_t0, unit_converted_t0,
    value_t1, sd_t1, se_t1, n_t1, lower_t1, upper_t1, range_type_t1,
    value_converted_t1, sd_converted_t1, se_converted_t1,
    lower_converted_t1, upper_converted_t1, unit_converted_t1,
    study_design.y, pct_change_converted, pct_change,
    sd_pct_change_r90, sd_pct_change_converted_r90
  ))

## 8e. Clean up duplicate suffix columns from joins ####
data_MA <- data_MA %>%
  dplyr::select(-c(
    primary_report.y, comparison.y, measure_domain.y, study_type.y,
    study_design.x, correlation.y, measure_subdomain.y, measure_binned.y,
    measure_reported.y, statistic_t0.y, statistic_t1.y,
    pre_varid.y, post_varid.y,
    time_t1.y, time_unit_t1.y, time_t0.y, time_unit_t0.y, source.y
  )) %>%
  rename(
    study_type        = study_type.x,
    measure_domain    = measure_domain.x,
    source            = source.x,
    primary_report    = primary_report.x,
    comparison        = comparison.x,
    correlation       = correlation.x,
    measure_binned    = measure_binned.x,
    measure_subdomain = measure_subdomain.x,
    measure_reported  = measure_reported.x,
    statistic_t0      = statistic_t0.x,
    statistic_t1      = statistic_t1.x,
    time_unit_t0      = time_unit_t0.x,
    time_unit_t1      = time_unit_t1.x,
    time_t0           = time_t0.x,
    time_t1           = time_t1.x,
    pre_varid         = pre_varid.x,
    post_varid        = post_varid.x
  )


# =============================================================================
# 9. CALCULATE WITHIN-GROUP CHANGE SCORES USING escalc() ####
# =============================================================================

# metafor::escalc(measure = "MC") computes the mean change (T1 - T0) and its
# variance, accounting for the within-subject correlation r = 0.85.
# This is done separately for control and intervention groups so that
# their change scores can later be differenced to give the between-group MD.

## 9a. Control group within-group change ####
data_MA <- metafor::escalc(
  measure = "MC",
  data    = data_MA %>% dplyr::filter(comparison == "Summary"),
  append  = TRUE,
  m1i     = value_ctrl_t1,
  m2i     = value_ctrl_t0,
  sd1i    = sd_ctrl_t1,
  sd2i    = sd_ctrl_t0,
  ni      = n_ctrl_t1,
  ri      = correlation
)

# Set yi/vi to NA for non-mean statistics (medians, geometric means, etc.)
# to prevent those from entering pooled models
data_MA <- data_MA %>%
  mutate(
    yi = ifelse(statistic_t0 %in% c("Absolute value", "Mean", "Average",
                                    "Geometric mean", "LSM",
                                    "Mean transformed (Median)"), yi, NA),
    vi = ifelse(statistic_t0 %in% c("Absolute value", "Mean", "Average",
                                    "Geometric mean", "LSM",
                                    "Mean transformed (Median)"), vi, NA)
  )

data_MA <- data_MA %>%
  mutate(
    mc_ctrl     = yi,
    mc_var_ctrl = vi,
    mc_sd_ctrl  = sqrt(sd_ctrl_t0^2 + sd_ctrl_t1^2 -
                         2 * correlation * sd_ctrl_t0 * sd_ctrl_t1),
    mc_se_ctrl  = vi^0.5,
    mc_lc_ctrl  = yi - 1.96 * vi^0.5,
    mc_uc_ctrl  = yi + 1.96 * vi^0.5,
    comparison2 = ifelse(!is.na(mc_ctrl), "Within2", NA),
    unit_ctrl_t1 = if_else(!is.na(mc_ctrl),
                           paste0(unit_ctrl_t1, ", change-in2"),
                           unit_ctrl_t1)
  ) %>%
  filter(comparison == "Summary")

## 9b. Intervention group within-group change ####
data_MA <- metafor::escalc(
  measure = "MC",
  data    = data_MA %>% dplyr::filter(comparison == "Summary"),
  append  = TRUE,
  m1i     = value_int_t1,
  m2i     = value_int_t0,
  sd1i    = sd_int_t1,
  sd2i    = sd_int_t0,
  ni      = n_int_t1,
  ri      = correlation
)

data_MA <- data_MA %>%
  mutate(
    yi = ifelse(statistic_t0 %in% c("Absolute value", "Mean", "Average",
                                    "Geometric mean", "LSM",
                                    "Mean transformed (Median)"), yi, NA),
    vi = ifelse(statistic_t0 %in% c("Absolute value", "Mean", "Average",
                                    "Geometric mean", "LSM",
                                    "Mean transformed (Median)"), vi, NA)
  )

data_MA <- data_MA %>%
  mutate(
    mc_int     = yi,
    mc_var_int = vi,
    mc_sd_int  = sqrt(sd_int_t0^2 + sd_int_t1^2 -
                        2 * correlation * sd_int_t0 * sd_int_t1),
    mc_se_int  = vi^0.5,
    mc_lc_int  = yi - 1.96 * vi^0.5,
    mc_uc_int  = yi + 1.96 * vi^0.5,
    comparison3  = ifelse(!is.na(mc_int), "Within2", NA),
    unit_int_t1  = if_else(!is.na(mc_int),
                           paste0(unit_int_t1, ", change-in2"),
                           unit_int_t1)
  ) %>%
  filter(comparison == "Summary")


# =============================================================================
# 10. RECONSTRUCT COMBINED LONG DATASET WITH WITHIN-GROUP CHANGE ROWS ####
# =============================================================================

# Extract the calculated control-group change rows and reshape to match
# the column structure of data_outcomes so they can be stacked
data_MC_ctrl <- data_MA %>%
  dplyr::select(
    index, citation, study_design, treatment_type, comparator_type,
    primary_comparison, analytical_population, measure_cleaned, adjustments,
    ugid_int, ugid_ctrl, study_type, sub_group1, comparison, correlation,
    measure_domain, measure_subdomain, measure_binned, measure_reported,
    time_t1, time_unit_t1, statistic_t0, unit_ctrl_t1, n_ctrl_t1,
    mc_ctrl, mc_sd_ctrl, mc_se_ctrl, mc_lc_ctrl, mc_uc_ctrl,
    comparison2, group_id.y, pct_change_ctrl, sd_pct_change_ctrl
  ) %>%
  rename(
    time          = time_t1,
    time_unit     = time_unit_t1,
    statistic     = statistic_t0,
    unit          = unit_ctrl_t1,
    value         = mc_ctrl,
    se            = mc_se_ctrl,
    sd            = mc_sd_ctrl,
    lower         = mc_lc_ctrl,
    upper         = mc_uc_ctrl,
    n             = n_ctrl_t1,
    group_id      = group_id.y,
    pct_change    = pct_change_ctrl,
    sd_pct_change = sd_pct_change_ctrl
  )

data_MC_int <- data_MA %>%
  dplyr::select(
    index, citation, study_design, treatment_type, comparator_type,
    primary_comparison, study_type, analytical_population, measure_cleaned,
    adjustments, ugid_int, ugid_ctrl, sub_group1, comparison,
    measure_domain, measure_subdomain, measure_binned, measure_reported,
    correlation, time_t1, time_unit_t1, statistic_t0, unit_int_t1,
    group_id.x, mc_int, n_int_t1, mc_se_int, mc_sd_int,
    mc_lc_int, mc_uc_int, comparison3, pct_change_int, sd_pct_change_int
  ) %>%
  rename(
    time          = time_t1,
    time_unit     = time_unit_t1,
    statistic     = statistic_t0,
    unit          = unit_int_t1,
    value         = mc_int,
    se            = mc_se_int,
    sd            = mc_sd_int,
    lower         = mc_lc_int,
    upper         = mc_uc_int,
    n             = n_int_t1,
    group_id      = group_id.x,
    pct_change    = pct_change_int,
    sd_pct_change = sd_pct_change_int
  )

# Stack: original outcomes + calculated within-group change rows for both arms
data_ctrl         <- bind_rows(data_MC_ctrl, data_outcomes)
data_combined_all <- bind_rows(data_ctrl, data_MC_int)

# Consolidate comparison labels (Within2 rows take precedence over Within and Summary)
data_combined_all <- data_combined_all %>%
  mutate(comparison = coalesce(comparison3, comparison2, comparison)) %>%
  dplyr::select(-c(comparison2, comparison3))

# Propagate converted values to the newly computed Within2 rows
data_combined_all <- data_combined_all %>%
  mutate(
    value_converted  = if_else(comparison == "Within2" & primary_comparison == "Y",
                               value, value_converted),
    unit_converted   = if_else(comparison == "Within2" & primary_comparison == "Y",
                               unit,  unit_converted),
    se_converted     = if_else(comparison == "Within2" & primary_comparison == "Y",
                               se,    se_converted),
    sd_converted     = if_else(comparison == "Within2" & primary_comparison == "Y",
                               sd,    sd_converted),
    lower_converted  = if_else(comparison == "Within2" & primary_comparison == "Y",
                               lower, lower_converted),
    upper_converted  = if_else(comparison == "Within2" & primary_comparison == "Y",
                               upper, upper_converted),
    # Also fill in cases where the converted column is still NA
    value_converted  = if_else(comparison == "Within2" & is.na(value_converted),
                               value, value_converted),
    unit_converted   = if_else(comparison == "Within2" & is.na(unit_converted),
                               unit,  unit_converted),
    se_converted     = if_else(comparison == "Within2" & is.na(se_converted),
                               se,    se_converted),
    sd_converted     = if_else(comparison == "Within2" & is.na(sd_converted),
                               sd,    sd_converted),
    lower_converted  = if_else(comparison == "Within2" & is.na(lower_converted),
                               lower, lower_converted),
    upper_converted  = if_else(comparison == "Within2" & is.na(upper_converted),
                               upper, upper_converted)
  )


# =============================================================================
# 11. CALCULATE % CHANGE FOR STUDIES REPORTING ONLY WITHIN-GROUP CHANGES ####
# =============================================================================

# Daly 2014, Formica 2020, Griffin 2013, Mitchell 2021, Poddar 2013, and Sayer 2017
# reported within-group change values directly. We need % change and its SD to
# feed the percent-change meta-analyses. These are calculated by joining the
# baseline mean and SD onto the within-group change rows.

data_combined_all <- data_combined_all %>% mutate(row_id = row_number())

# Extract baseline values (T0 summary rows)
baseline_df <- data_combined_all %>%
  filter(
    measure_domain == "Outcome",
    time == 0,
    citation %in% c("Mitchell 2021", "Sayer 2017", "Poddar 2013",
                    "Daly 2014", "Formica 2020", "Griffin 2013")
  ) %>%
  transmute(
    citation,
    measure_cleaned,
    sub_group1,
    group_id,
    baseline_value = value_converted,
    baseline_sd    = sd_converted
  )

# Filter to within-group change rows for the same citations, excluding known
# duplicates or problematic rows
change_df <- data_combined_all %>%
  filter(
    comparison     == "Within",
    measure_domain == "Outcome",
    citation %in% c("Mitchell 2021", "Sayer 2017", "Poddar 2013",
                    "Daly 2014", "Griffin 2013", "Formica 2020"),
    !(citation == "Sayer 2017"   & measure_reported == "Weight loss"),
    !(citation == "Mitchell 2021" & source == "Table 4"),
    !(citation == "Griffin 2013"  & measure_reported %in%
        c("Loss of initial fat", "Loss of initial weight")),
    !(citation == "Formica 2020"  & measure_subdomain == "Muscles"),
    !(citation == "Daly 2014"     & measure_subdomain %in% c("Muscles", "Bones"))
  )

# Join baseline to change rows and compute % change and its SD
change_with_pct <- change_df %>%
  left_join(baseline_df, by = c("citation", "measure_cleaned", "group_id")) %>%
  mutate(
    pct_change = ifelse(
      !is.na(baseline_value) & baseline_value != 0 & !is.na(value_converted),
      (value_converted / baseline_value) * 100,
      NA_real_
    ),
    sd_pct_change = ifelse(
      !is.na(baseline_sd) & !is.na(sd_converted) & baseline_value != 0,
      sqrt(baseline_sd^2 + sd_converted^2 -
             2 * 0.90 * baseline_sd * sd_converted) / baseline_value * 100,
      NA_real_
    )
  ) %>%
  filter(!is.na(pct_change)) %>%
  dplyr::select(row_id, pct_change, sd_pct_change)

# Merge back using row_id, prioritising newly calculated values
data_combined_all <- data_combined_all %>%
  left_join(change_with_pct, by = "row_id") %>%
  mutate(
    pct_change    = coalesce(pct_change.y,    pct_change.x),
    sd_pct_change = coalesce(sd_pct_change.y, sd_pct_change.x)
  ) %>%
  dplyr::select(-ends_with(".x"), -ends_with(".y"), -row_id)


# =============================================================================
# 12. PREPARE WITHIN-GROUP DATASET FOR MD/SMD CALCULATION ####
# =============================================================================

# Filter to rows containing within-group change values (both originally
# reported "Within" rows and the computed "Within2" rows from Section 9),
# then pair them by intervention and control group.

data_within <- data_combined_all %>%
  filter(
    comparison %in% c("Within", "Within2"),
    !(citation == "Zhu 2022" & group_id == "Processed meat")
  ) %>%
  dplyr::select(-ugid_int, -ugid_ctrl)

# For Poddar 2013: exclude the full-study Within2 rows -- the phase-specific
# rows (0-6 and 6-12 months) are used instead
data_within <- data_within %>%
  filter(!(citation == "Poddar 2013" & comparison == "Within2"))

# Where both a reported Within and a computed Within2 row exist for the same
# study and outcome, keep only Within2 (the computed version corrected for
# r = 0.85). This ensures a consistent correlation assumption across all studies.
data_within_filtered <- data_within %>%
  group_by(index, measure_cleaned) %>%
  mutate(has_both = any(comparison == "Within") & any(comparison == "Within2")) %>%
  filter(!(has_both & comparison == "Within")) %>%
  ungroup() %>%
  dplyr::select(-has_both)

# Drop Griffin 2013 rows where weight was reported in % (keeps kg rows only)
data_within_filtered <- data_within_filtered %>%
  filter(!(citation == "Griffin 2013" & measure_cleaned == "Body weight" &
             unit == "%, change-in")) %>%
  filter(!(citation == "Griffin 2013" & measure_cleaned == "Body fat" &
             unit == "% loss, change-in"))

# Unique group identifier and variable ID for matching
data_within_filtered$ugid <- paste(
  data_within_filtered$index, ": ",
  data_within_filtered$group_id, " ",
  data_within_filtered$sub_group1,
  data_within_filtered$analytical_population
)

data_within_filtered <- data_within_filtered %>%
  mutate(varid = paste(analytical_population, adjustments, comparison,
                       measure_cleaned, time, time_unit, statistic, unit_converted))

## Pair intervention and control groups ####
data_MD <- data_comp %>%
  left_join(
    data_within_filtered %>% dplyr::select(index, group_id, ugid, sub_group1),
    by = c("index", "group_id"),
    multiple = "all"
  ) %>%
  distinct() %>%
  rename(ugid_int = ugid) %>%
  dplyr::select(-group_id)

data_MD <- data_MD %>%
  left_join(
    data_within_filtered %>% dplyr::select(index, group_id, ugid, sub_group1),
    by = c("index", "comparator" = "group_id", "sub_group1"),
    multiple = "all"
  ) %>%
  distinct() %>%
  rename(ugid_ctrl = ugid) %>%
  dplyr::select(-comparator)

# Merge control group data
data_MD <- data_MD %>%
  left_join(data_within_filtered,
            by = c("index", "citation", "ugid_ctrl" = "ugid", "sub_group1"),
            multiple = "all") %>%
  distinct() %>%
  mutate(
    value_ctrl         = value_converted,
    sd_ctrl            = sd_converted,
    se_ctrl            = se_converted,
    n_ctrl             = n,
    lower_ctrl         = lower_converted,
    upper_ctrl         = upper_converted,
    unit_ctrl          = unit_converted,
    pct_change_ctrl    = pct_change,
    sd_pct_change_ctrl = sd_pct_change,
    group_id_ctrl      = group_id
  ) %>%
  dplyr::select(!c(
    value, sd, se, n, lower, upper, study_design.y, group_id,
    value_converted, sd_converted, se_converted, unit,
    lower_converted, upper_converted, unit_converted,
    treatment_type.y, comparator_type.y, primary_comparison.y,
    pct_change, sd_pct_change
  )) %>%
  rename(
    study_design       = study_design.x,
    treatment_type     = treatment_type.x,
    comparator_type    = comparator_type.x,
    primary_comparison = primary_comparison.x
  )

# Merge intervention group data
data_MD <- data_MD %>%
  left_join(
    data_within_filtered,
    by = c("index", "analytical_population", "adjustments", "time", "time_unit",
           "sub_group1", "citation", "study_type", "measure_cleaned",
           "measure_domain", "measure_subdomain", "measure_binned",
           "measure_reported", "ugid_int" = "ugid")
  ) %>%
  distinct() %>%
  mutate(
    value_int         = value_converted,
    sd_int            = sd_converted,
    se_int            = se_converted,
    n_int             = n,
    lower_int         = lower_converted,
    upper_int         = upper_converted,
    unit_int          = unit_converted,
    pct_change_int    = pct_change,
    group_id_int      = group_id,
    sd_pct_change_int = sd_pct_change
  ) %>%
  dplyr::select(!c(
    value, sd, se, n, lower, upper, value_converted, sd_converted,
    se_converted, lower_converted, upper_converted, unit_converted,
    source.x, source.y, unit,
    primary_comparison.y, comparator_type.y, treatment_type.y,
    correlation.y, range_type.y, statistic.y, primary_report.y,
    comparison.y, study_design.y, pct_change
  )) %>%
  rename(
    treatment_type     = treatment_type.x,
    comparator_type    = comparator_type.x,
    primary_comparison = primary_comparison.x,
    primary_report     = primary_report.x,
    correlation        = correlation.x,
    range_type         = range_type.x,
    statistic          = statistic.x,
    comparison         = comparison.x,
    study_design       = study_design.x
  )


# =============================================================================
# 13. ASSIGN REPORTED % CHANGE VALUES FOR MUSCLE AND BONE OUTCOMES ####
# =============================================================================

# Formica 2020, Daly 2014, and Haub 2002 reported muscle/bone outcomes as
# % change directly. These are used in place of the calculated values.

data_MD <- data_MD %>%
  mutate(
    pct_change_ctrl = case_when(
      citation == "Formica 2020" & measure_subdomain == "Muscles" ~ value_ctrl,
      citation == "Daly 2014"    & measure_subdomain %in% c("Muscles", "Bones") ~ value_ctrl,
      citation == "Haub 2002"    & measure_cleaned == "Thigh muscle cross-sectional area" ~ value_ctrl,
      TRUE ~ pct_change_ctrl
    ),
    pct_change_int = case_when(
      citation == "Formica 2020" & measure_subdomain == "Muscles" ~ value_int,
      citation == "Daly 2014"    & measure_subdomain %in% c("Muscles", "Bones") ~ value_int,
      citation == "Haub 2002"    & measure_cleaned == "Thigh muscle cross-sectional area" ~ value_int,
      TRUE ~ pct_change_int
    ),
    sd_pct_change_ctrl = case_when(
      citation == "Formica 2020" & measure_subdomain == "Muscles" ~ sd_ctrl,
      citation == "Daly 2014"    & measure_subdomain %in% c("Muscles", "Bones") ~ sd_ctrl,
      citation == "Haub 2002"    & measure_cleaned == "Thigh muscle cross-sectional area" ~ sd_ctrl,
      TRUE ~ sd_pct_change_ctrl
    ),
    sd_pct_change_int = case_when(
      citation == "Formica 2020" & measure_subdomain == "Muscles" ~ sd_int,
      citation == "Daly 2014"    & measure_subdomain %in% c("Muscles", "Bones") ~ sd_int,
      citation == "Haub 2002"    & measure_cleaned == "Thigh muscle cross-sectional area" ~ sd_int,
      TRUE ~ sd_pct_change_int
    )
  )


# =============================================================================
# 14. CALCULATE BETWEEN-GROUP EFFECT SIZES (MD, SMD) ####
# =============================================================================

## 14a. Mean difference in absolute change (MD) ####
data_MD2 <- metafor::escalc(
  measure = "MD",
  data    = data_MD,
  append  = TRUE,
  m1i     = value_int,
  m2i     = value_ctrl,
  sd1i    = sd_int,
  sd2i    = sd_ctrl,
  n1i     = n_int,
  n2i     = n_ctrl
)

data_MD2 <- data_MD2 %>%
  mutate(
    yi = ifelse(statistic %in% c("Absolute value", "Mean", "Average",
                                 "Geometric mean", "LSM",
                                 "Mean transformed (Median)"), yi, NA),
    vi = ifelse(statistic %in% c("Absolute value", "Mean", "Average",
                                 "Geometric mean", "LSM",
                                 "Mean transformed (Median)"), vi, NA)
  ) %>%
  mutate(
    MD_bg     = yi,
    MD_var_bg = vi,
    MD_se_bg  = sqrt(vi),
    MD_lc_bg  = yi - 1.96 * vi^0.5,
    MD_uc_bg  = yi + 1.96 * vi^0.5
  ) %>%
  dplyr::select(-yi, -vi)

## 14b. Mean difference in percent change (MD_pct) ####
data_MD2 <- metafor::escalc(
  measure = "MD",
  data    = data_MD2,
  append  = TRUE,
  m1i     = pct_change_int,
  m2i     = pct_change_ctrl,
  sd1i    = sd_pct_change_int,
  sd2i    = sd_pct_change_ctrl,
  n1i     = n_int,
  n2i     = n_ctrl
)

data_MD2 <- data_MD2 %>%
  mutate(
    yi = ifelse(statistic %in% c("Absolute value", "Mean", "Average",
                                 "Geometric mean", "LSM",
                                 "Mean transformed (Median)"), yi, NA),
    vi = ifelse(statistic %in% c("Absolute value", "Mean", "Average",
                                 "Geometric mean", "LSM",
                                 "Mean transformed (Median)"), vi, NA)
  ) %>%
  mutate(
    MD_pct_bg     = yi,
    MD_pct_var_bg = vi,
    MD_pct_se_bg  = sqrt(vi),
    MD_pct_lc_bg  = yi - 1.96 * vi^0.5,
    MD_pct_uc_bg  = yi + 1.96 * vi^0.5
  ) %>%
  dplyr::select(-yi, -vi)

## 14c. Standardised mean difference -- absolute (Hedges' g) ####
data_MD2 <- metafor::escalc(
  measure = "SMD",
  data    = data_MD2,
  m1i     = value_int,
  m2i     = value_ctrl,
  sd1i    = sd_int,
  sd2i    = sd_ctrl,
  n1i     = n_int,
  n2i     = n_ctrl
)

data_MD2 <- data_MD2 %>%
  mutate(
    yi = ifelse(statistic %in% c("Absolute value", "Mean", "Average",
                                 "Geometric mean", "LSM",
                                 "Mean transformed (Median)"), yi, NA),
    vi = ifelse(statistic %in% c("Absolute value", "Mean", "Average",
                                 "Geometric mean", "LSM",
                                 "Mean transformed (Median)"), vi, NA)
  ) %>%
  mutate(
    hedgesg     = yi,
    hedgesg_var = vi,
    hedgesg_se  = vi^0.5,
    hedgesg_lc  = yi - 1.96 * vi^0.5,
    hedgesg_uc  = yi + 1.96 * vi^0.5
  ) %>%
  dplyr::select(-yi, -vi) %>%
  filter(measure_domain == "Outcome")   # drop any Beta-coefficient rows

## 14d. Standardised mean difference -- percent change (Hedges' g_pct) ####
data_MD2 <- metafor::escalc(
  measure = "SMD",
  data    = data_MD2,
  m1i     = pct_change_int,
  m2i     = pct_change_ctrl,
  sd1i    = sd_pct_change_int,
  sd2i    = sd_pct_change_ctrl,
  n1i     = n_int,
  n2i     = n_ctrl
)

data_MD2 <- data_MD2 %>%
  mutate(
    yi = ifelse(statistic %in% c("Absolute value", "Mean", "Average",
                                 "Geometric mean", "LSM",
                                 "Mean transformed (Median)"), yi, NA),
    vi = ifelse(statistic %in% c("Absolute value", "Mean", "Average",
                                 "Geometric mean", "LSM",
                                 "Mean transformed (Median)"), vi, NA)
  ) %>%
  mutate(
    hedgesg_pct     = yi,
    hedgesg_pct_var = vi,
    hedgesg_pct_se  = vi^0.5,
    hedgesg_pct_lc  = yi - 1.96 * vi^0.5,
    hedgesg_pct_uc  = yi + 1.96 * vi^0.5
  ) %>%
  dplyr::select(-yi, -vi) %>%
  filter(measure_domain == "Outcome")


# =============================================================================
# 15. FINAL EXCLUSIONS AND DUPLICATE HANDLING ####
# =============================================================================

# Remove rows with duplicate or ambiguous unit representations
# and non-included citations (Iglay 2009, Canfi 2011, Murphy 2014 excluded at
# eligibility stage; Zhu 2022 is an observational study retained for reference
# but excluded from pooled analyses).
#
# NOTE: Leidy 2007 rows with sub_group1 = NA are the combined-sample effects
# (full enrolled sample, before stratification into Pre-obese and Obese subgroups).
# These rows are RETAINED here because they are the appropriate rows for the main
# pooled analysis. The Pre-obese and Obese subgroup rows are filtered out in
# Analysis_Meat_SRMA.R; the NA rows pass those filters and serve as Leidy 2007's
# contribution to each outcome.

data_MD3 <- data_MD2 %>%
  filter(
    !(citation == "Sayer 2017"  & unit_ctrl == "%, change-in"),
    !(citation == "Li 2016"     & unit_ctrl == "%, change-in2"),
    !(citation == "Murphy 2014"),
    !(citation == "Iglay 2009"),
    !(citation == "Canfi 2011")
  )


# =============================================================================
# 16. AGGREGATE BONE MINERAL DENSITY (Daly 2014) ####
# =============================================================================

# Daly 2014 reported BMD at three sites (femoral neck, lumbar spine, total hip).
# These are aggregated into a single row using inverse-variance weighting for
# the MD, and simple means for all other quantities, to avoid treating the same
# study as three independent observations in the BMD pooled analysis.

bmd_sites <- c(
  "Femoral neck bone mineral density",
  "Lumbar spine bone mineral density",
  "Total hip bone mineral density"
)

bmd_data_filtered <- data_MD3 %>%
  filter(citation == "Daly 2014", measure_cleaned %in% bmd_sites)

bmd_aggregated <- bmd_data_filtered %>%
  summarise(
    # Inverse-variance weighted pooled MD across the three BMD sites
    md_agg = {
      valid <- !is.na(MD_bg) & !is.na(MD_se_bg)
      if (!any(valid)) NA_real_ else {
        w <- 1 / (MD_se_bg[valid]^2)
        sum(w * MD_bg[valid]) / sum(w)
      }
    },
    se_agg = {
      valid <- !is.na(MD_bg) & !is.na(MD_se_bg)
      if (!any(valid)) NA_real_ else sqrt(1 / sum(1 / (MD_se_bg[valid]^2)))
    },
    md_lc_agg = md_agg - 1.96 * se_agg,
    md_uc_agg = md_agg + 1.96 * se_agg,
    # Simple means for display values and percent changes
    value_ctrl_agg      = mean(value_ctrl,      na.rm = TRUE),
    value_int_agg       = mean(value_int,        na.rm = TRUE),
    pct_change_ctrl_agg = mean(pct_change_ctrl,  na.rm = TRUE),
    pct_change_int_agg  = mean(pct_change_int,   na.rm = TRUE),
    sd_pct_change_ctrl_agg = mean(sd_pct_change_ctrl, na.rm = TRUE),
    sd_pct_change_int_agg  = mean(sd_pct_change_int,  na.rm = TRUE),
    # Preserve metadata
    citation              = first(citation),
    treatment_type        = first(treatment_type),
    comparator_type       = first(comparator_type),
    group_id_ctrl         = first(group_id_ctrl),
    group_id_int          = first(group_id_int),
    time                  = first(time),
    time_unit             = first(time_unit),
    comparison            = first(comparison),
    n_int                 = first(n_int),
    n_ctrl                = first(n_ctrl),
    index                 = first(index),
    ugid_int              = first(ugid_int),
    ugid_ctrl             = first(ugid_ctrl),
    sub_group1            = first(sub_group1),
    analytical_population = first(analytical_population),
    adjustments           = first(adjustments),
    study_type            = first(study_type),
    measure_domain        = first(measure_domain),
    measure_subdomain     = first(measure_subdomain),
    measure_binned        = first(measure_binned),
    measure_reported      = first(measure_reported),
    statistic             = first(statistic),
    group_id              = first(group_id),
    primary_report        = first(primary_report),
    range_type            = first(range_type),
    varid.x               = first(varid.x),
    sd_ctrl               = first(sd_ctrl),
    se_ctrl               = first(se_ctrl),
    lower_ctrl            = first(lower_ctrl),
    upper_ctrl            = first(upper_ctrl),
    unit_ctrl             = first(unit_ctrl),
    study_design          = first(study_design),
    varid.y               = first(varid.y),
    sd_int                = first(sd_int),
    se_int                = first(se_int),
    lower_int             = first(lower_int),
    upper_int             = first(upper_int),
    unit_int              = first(unit_int),
    hedgesg               = first(hedgesg),
    hedgesg_var           = first(hedgesg_var),
    hedgesg_se            = first(hedgesg_se),
    hedgesg_lc            = first(hedgesg_lc),
    hedgesg_uc            = first(hedgesg_uc),
    .groups = "drop"
  ) %>%
  mutate(
    measure_cleaned    = "Aggregated BMD",
    MD_bg              = md_agg,
    MD_se_bg           = se_agg,
    MD_lc_bg           = md_lc_agg,
    MD_uc_bg           = md_uc_agg,
    value_ctrl         = value_ctrl_agg,
    value_int          = value_int_agg,
    pct_change_ctrl    = pct_change_ctrl_agg,
    pct_change_int     = pct_change_int_agg,
    sd_pct_change_ctrl = sd_pct_change_ctrl_agg,
    sd_pct_change_int  = sd_pct_change_int_agg
  ) %>%
  dplyr::select(-md_agg, -se_agg, -md_lc_agg, -md_uc_agg,
                -value_ctrl_agg, -value_int_agg,
                -pct_change_ctrl_agg, -pct_change_int_agg,
                -sd_pct_change_ctrl_agg, -sd_pct_change_int_agg)

data_MD3_agg <- bind_rows(data_MD3, bmd_aggregated) %>%
  arrange(index)


# =============================================================================
# 17. MERGE STUDY-LEVEL REVIEW VARIABLES ####
# =============================================================================

# Remove the weight-loss row for Magkos 2022 from Review Data (only the
# weight-maintenance phase is included in the meta-analysis) and the
# weight-loss row for Zhu 2022 (secondary analysis, not in primary MA)
data_review2 <- data_review %>%
  filter(
    !(citation == "Magkos 2022" & intended_energy_balance_of_subjects == "Weight loss"),
    !(citation == "Zhu 2022"    & intended_energy_balance_of_subjects == "Weight loss")
  )

# Create a join key for Poddar 2013 and Benassi-Evans 2009 so that each sub_group1 phase row 
# inherits the correct study-level covariates from the Review Data sheet.
# The full-study Poddar row (0-12 mos) reuses the weight-loss review record;
# it carries study-level covariates and is later relabelled in the analysis script.
data_MD3_agg <- data_MD3_agg %>%
  mutate(energy_balance_key = case_when(
    citation == "Poddar 2013" & grepl("0-12", sub_group1)                                          ~ "Weight loss",
    citation %in% c("Poddar 2013", "Benassi-Evans 2009") & grepl("Weight loss", sub_group1)        ~ "Weight loss",
    citation %in% c("Poddar 2013", "Benassi-Evans 2009") & grepl("Weight maintenance", sub_group1) ~ "Weight maintenance",
    TRUE ~ NA_character_
  ))

data_review2 <- data_review2 %>%
  mutate(energy_balance_key = case_when(
    citation %in% c("Poddar 2013", "Benassi-Evans 2009") ~ intended_energy_balance_of_subjects,
    TRUE ~ NA_character_
  ))

# Ensure index is character in both datasets for a clean join
data_MD3_agg <- data_MD3_agg %>% mutate(index = as.character(index))
data_review2  <- data_review2  %>% mutate(index = as.character(index))

data_MD4 <- data_MD3_agg %>%
  left_join(data_review2,
            by      = c("index", "citation", "energy_balance_key"),
            multiple = "all",
            na_matches = "na") %>%
  filter(comparison != "Between-group difference of the within-group change") %>%
  distinct() %>%
  filter(unit_ctrl == unit_int) %>%
  dplyr::select(!c(study_type.y, study_design.y, primary_report.y,
                   group_id, energy_balance_key)) %>%
  rename(
    study_design   = study_design.x,
    study_type     = study_type.x,
    primary_report = primary_report.x
  )


# =============================================================================
# 18. RECODES AND DERIVED VARIABLES ####
# =============================================================================

# Duration
data_MD4$max_intervention_length_days <- as.numeric(data_MD4$max_intervention_length_days)
data_MD4$duration <- ifelse(data_MD4$max_intervention_length_days < 120,
                            "< 120 days", ">= 120 days")
data_MD4$duration_weeks <- round(data_MD4$max_intervention_length_days / 7, 0)

# Exercise
data_MD4$anyexercise <- ifelse(data_MD4$exercise %in% c("Both", "Cardio", "Resistance"),
                               "Yes", "No")
data_MD4$resistance  <- ifelse(data_MD4$exercise %in% c("Both", "Resistance"),
                               "Yes", "No")

# Sex
data_MD4 <- data_MD4 %>%
  mutate(sex3 = case_when(
    sex %in% c("F", "Majority F") ~ "Female",
    sex == "M"                    ~ "Male",
    sex == "Mixed"                ~ "Mixed",
    TRUE ~ "Other"
  ))

# BMI category
data_MD4 <- data_MD4 %>%
  mutate(obesity = case_when(
    mean_baseline_bmi < 30  ~ "Pre-obese",
    mean_baseline_bmi >= 30 ~ "Obese",
    TRUE ~ "Other"
  ))

# Weight status
data_MD4 <- data_MD4 %>%
  mutate(weight_status = case_when(
    target_population %in% c("OW/OB", "Overweight/obese",
                              "Older adults/senior;Overweight/obese",
                              "OW/OB;Prediabetes") ~ "OW/OB",
    TRUE ~ "General Pop/Mixed"
  ))

# Age status
data_MD4 <- data_MD4 %>%
  mutate(age_status = case_when(
    target_population %in% c(
      "Older adult/senior men", "Older adults/senior;Overweight/obese",
      "Older adults (>=65yrs)", "Older adults/seniors",
      "Postmenopausal women", "Women 60+ in retirement village"
    ) ~ "Seniors",
    TRUE ~ "General Pop/Mixed"
  ))

# Meat leanness
data_MD4 <- data_MD4 %>%
  mutate(lean = case_when(
    meat_exposure %in% c("Lean beef", "Lean Beef", "Lean red meat",
                         "Lean pork", "Lean beef;Poultry;Lean pork") ~ "Lean meat",
    TRUE ~ "Not lean or not specified"
  ))

# Total sample size (n_int for crossover; n_int + n_ctrl for parallel)
data_MD4$n_total <- data_MD4$n_int + data_MD4$n_ctrl
data_MD4 <- data_MD4 %>%
  mutate(n_total = case_when(
    study_design == "Crossover" ~ n_int,
    TRUE ~ n_total
  ))

# Meal control
data_MD4 <- data_MD4 %>%
  mutate(meal_control = case_when(
    meal_control_ad_lib_or_controlled == "Controlled" ~ "Controlled",
    meal_control_ad_lib_or_controlled == "Ad lib"     ~ "Ad libitum",
    TRUE ~ "Unclear"
  ))

# Energy balance of subjects (intended)
data_MD4 <- data_MD4 %>%
  mutate(energy_balance_subjects = case_when(
    intended_energy_balance_of_subjects == "Weight loss"        ~ "Weight loss",
    intended_energy_balance_of_subjects == "Weight maintenance" ~ "Weight maintenance",
    intended_energy_balance_of_subjects == "No intent"          ~ "No intent",
    TRUE ~ "Other"
  ))

# Energy balance between groups (intended)
data_MD4 <- data_MD4 %>%
  mutate(energy_balance_grp = case_when(
    intended_energy_balance_between_groups == "Hypercaloric" ~ "Hypercaloric",
    intended_energy_balance_between_groups == "Isocaloric"   ~ "Isocaloric",
    intended_energy_balance_between_groups == "No intent"    ~ "No intent",
    TRUE ~ "Other"
  ))

# Treatment type
data_MD4 <- data_MD4 %>%
  mutate(treatment = case_when(
    treatment_type %in% c("High Meat", "High protein") ~ "High meat/protein",
    treatment_type == "Meat type"                       ~ "Meat type",
    treatment_type == "Meat"                            ~ "Meat (vs. no meat)",
    TRUE ~ "Other"
  ))

# Geographic region
data_MD4 <- data_MD4 %>%
  mutate(region = case_when(
    population_region == "USA"       ~ "USA",
    population_region == "Australia" ~ "Australia",
    TRUE ~ "Other"
  ))

# Study-specific energy balance reclassifications (phase-level corrections)
data_MD4 <- data_MD4 %>%
  mutate(energy_balance_subjects = case_when(
    citation == "Benassi-Evans 2009" & time == "12"               ~ "Weight loss",
    citation == "Benassi-Evans 2009" & time == "52"               ~ "Weight maintenance",
    citation == "Poddar 2013"        & grepl("0-12", sub_group1)  ~ "Weight loss and maintenance",
    citation == "Poddar 2013"        & grepl("6-12", sub_group1)  ~ "Weight maintenance",
    citation == "Poddar 2013"        & grepl("0-6",  sub_group1)  ~ "Weight loss",
    citation == "Magkos 2022"        & time == "8"                ~ "Weight loss",
    citation == "Magkos 2022"        & time == "21"               ~ "Weight maintenance",
    TRUE ~ energy_balance_subjects
  ))

# Phase labels for Magkos 2022
data_MD4 <- data_MD4 %>%
  mutate(sub_group1 = case_when(
    citation == "Magkos 2022" & time == 8  ~ "Weight loss (0-8 wks)",
    citation == "Magkos 2022" & time == 21 ~ "Weight maintenance (8-21 wks)",
    TRUE ~ sub_group1
  ))

# Protein matching classification (override for two studies not in the Review sheet)
data_MD4 <- data_MD4 %>%
  mutate(protein_matched = case_when(
    citation %in% c("Poddar 2013", "Yamashita 1998") ~ "Protein-matched",
    TRUE ~ protein_matched
  ))


# =============================================================================
# 19. RENAME DIETARY INTAKE COLUMNS TO SHORTER NAMES ####
# =============================================================================

data_MD4 <- data_MD4 %>%
  rename(
    trt_pcte_protein    = trt_protein_intake_percent_e,
    comp_pcte_protein   = comp_protein_intake_percent_e,
    trt_pcte_carb       = trt_carb_intake_percent_e,
    comp_pcte_carb      = comp_carb_intake_percent_e,
    trt_pcte_fat        = trt_fat_intake_percent_e,
    comp_pcte_fat       = comp_fat_intake_percent_e,
    trt_kcal            = trt_tei_kcal_d,
    comp_kcal           = comp_tei_kcal_d,
    trt_g_kg_d_protein  = trt_protein_intake_g_kg_d,
    trt_g_protein       = trt_protein_intake_g_d,
    trt_g_carb          = trt_carb_intake_g_d,
    trt_g_fat           = trt_fat_intake_g_d,
    comp_g_kg_d_protein = comp_protein_intake_g_kg_d,
    comp_g_protein      = comp_protein_intake_g_d,
    comp_g_carb         = comp_carb_intake_g_d,
    comp_g_fat          = comp_fat_intake_g_d,
    trt_g_fiber         = trt_fiber_intake_g_d,
    comp_g_fiber        = comp_fiber_intake_g_d,
    trt_pcte_sum        = trt_sum_percent_e,
    comp_pcte_sum       = comp_sum_percent_e,
    bckgrd_redmeat      = background_diet_included_red_meat,
    lov                 = lacto_ovo_vegetarian_comparator,
    trt_manipulated     = trt_manipulated_dietary_fraction,
    comp_manipulated    = comp_manipulated_dietary_fraction,
    trt_protein_srce    = trt_primary_protein_source,
    comp_protein_srce   = comp_primary_protein_source
  )

# Between-arm macronutrient difference variables (used in meta-regression)
data_MD4 <- data_MD4 %>%
  mutate(
    diff_pcte_protein = trt_pcte_protein - comp_pcte_protein,
    diff_pcte_carb    = trt_pcte_carb    - comp_pcte_carb,
    diff_pcte_fat     = trt_pcte_fat     - comp_pcte_fat,
    diff_tei          = trt_kcal         - comp_kcal
  )


# =============================================================================
# 20. CREATE COMPOSITE OUTCOME VARIABLE ####
# =============================================================================

# Map individual measure names to composite categories used in the analysis.
# Central adiposity pools trunk fat mass, abdominal fat, and VAT.
# Bone mineral density pools the Daly 2014 aggregated row and any total BMD.
# Thigh muscle CSA pools femur and thigh cross-sectional area measurements.
# Thigh muscle density pools femur and thigh density measurements.

data_MD4 <- data_MD4 %>%
  mutate(measure_composite = case_when(
    measure_cleaned %in% c("Trunk fat mass", "Abdominal fat", "Visceral adipose tissue") ~
      "Central adiposity",
    measure_cleaned %in% c("Total bone mineral density", "Aggregated BMD") ~
      "Bone mineral density",
    measure_cleaned %in% c("Femur muscle cross-sectional area",
                           "Thigh muscle cross-sectional area") ~
      "Thigh muscle cross-sectional area",
    measure_cleaned %in% c("Femur muscle density", "Thigh muscle density") ~
      "Thigh muscle density",
    TRUE ~ measure_cleaned
  ))


# =============================================================================
# 21. WRITE ANALYTIC DATASET ####
# =============================================================================

write_xlsx(data_MD4, "data/Meat_Analytic_Dataset.xlsx")

message("Analytic dataset written to: Meat_Analytic_Dataset.xlsx")
message(paste("Rows:", nrow(data_MD4), "| Columns:", ncol(data_MD4)))

# =============================================================================
# END OF SCRIPT
# NOTE: Meat_Analytic_Dataset.xlsx is the sole input for Analysis_Meat_SRMA.R
# =============================================================================
