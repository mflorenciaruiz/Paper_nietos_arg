################################################################################
# Compare Spanish-born population between Argentina Census 1960 and 1970
# Province x sex x age-group analysis, with year-of-arrival sensitivity checks
#
# Inputs:
#   1960: Data Raw/Censo/cuadro5_clean.xlsx
#   1970: Data Raw/Censo/censos_arg.dta
#
# Outputs:
#   Excel workbook with original tables and alternative 1970 samples
#   LaTeX tables for Overleaf
#
# Interpretation:
#   - 1960 comes from aggregate published tables.
#   - 1970 comes from IPUMS-style microdata and uses person weights (perwt).
#   - Spanish-born means born in Spain.
#   - Cohort comparisons shift each 1960 age group forward by ten years.
#   - These comparisons measure cohort attrition, not mortality alone.
#   - Alternative 1970 samples remove immigrants who arrived after 1960.
################################################################################


# ==============================================================================
# 0. Packages
# ==============================================================================

packages <- c(
  "readxl", "haven", "dplyr", "tidyr", "stringr", "janitor",
  "openxlsx", "knitr", "purrr"
)

installed <- rownames(installed.packages())
to_install <- setdiff(packages, installed)

if (length(to_install) > 0) {
  install.packages(to_install, repos = "https://cloud.r-project.org")
}

library(readxl)
library(haven)
library(dplyr)
library(tidyr)
library(stringr)
library(janitor)
library(openxlsx)
library(knitr)
library(purrr)


# ==============================================================================
# 1. Paths
# ==============================================================================

# Change only this line if the project is in another location.
path_pili <- "C:/Users/pilih/Documents/Papers German/Valerie/Paper_nietos_arg"

setwd(path_pili)

censo_dir <- file.path(path_pili, "Data Raw", "Censo")

input_1960 <- file.path(censo_dir, "cuadro5_clean.xlsx")
input_1970 <- file.path(censo_dir, "censos_arg.dta")

out_dir <- file.path(
  path_pili,
  "Output",
  "spanish_population_1960_1970_arrival_sensitivity"
)

tex_dir <- file.path(out_dir, "tex")

dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(tex_dir, recursive = TRUE, showWarnings = FALSE)

cat("\nChecking input files:\n")
cat("1960 file exists: ", file.exists(input_1960), "\n")
cat("1970 file exists: ", file.exists(input_1970), "\n\n")

if (!file.exists(input_1960)) {
  stop("I cannot find the 1960 file here: ", input_1960)
}

if (!file.exists(input_1970)) {
  stop("I cannot find the 1970 file here: ", input_1970)
}


# ==============================================================================
# 2. Helper functions
# ==============================================================================

clean_province_name <- function(x) {
  x <- as.character(x)
  x <- stringr::str_squish(x)

  dplyr::recode(
    x,
    "Buenos Aires province" = "Buenos Aires",
    "Entre Rios" = "Entre Rios",
    "Entre Ríos" = "Entre Rios",
    .default = x
  )
}

clean_sex_1970 <- function(x) {
  x <- as.character(x)

  case_when(
    x %in% c("Male", "Varones", "Varon", "Hombre", "Hombres") ~ "Varones",
    x %in% c("Female", "Mujeres", "Mujer") ~ "Mujeres",
    TRUE ~ NA_character_
  )
}

make_age_group <- function(age_num) {
  case_when(
    is.na(age_num) ~ NA_character_,
    age_num < 0 ~ NA_character_,
    age_num <= 4 ~ "0_4",
    age_num <= 9 ~ "5_9",
    age_num <= 14 ~ "10_14",
    age_num <= 19 ~ "15_19",
    age_num <= 24 ~ "20_24",
    age_num <= 29 ~ "25_29",
    age_num <= 34 ~ "30_34",
    age_num <= 39 ~ "35_39",
    age_num <= 44 ~ "40_44",
    age_num <= 49 ~ "45_49",
    age_num <= 54 ~ "50_54",
    age_num <= 59 ~ "55_59",
    age_num <= 64 ~ "60_64",
    age_num <= 69 ~ "65_69",
    age_num <= 74 ~ "70_74",
    age_num <= 79 ~ "75_79",
    age_num <= 84 ~ "80_84",
    age_num >= 85 & age_num < 999 ~ "85_plus",
    TRUE ~ NA_character_
  )
}

age_group_order <- c(
  "0_4", "5_9", "10_14", "15_19", "20_24", "25_29",
  "30_34", "35_39", "40_44", "45_49", "50_54",
  "55_59", "60_64", "65_69", "70_74", "75_79",
  "80_84", "85_plus"
)

age_label <- function(x) {
  recode(
    x,
    "0_4" = "0-4",
    "5_9" = "5-9",
    "10_14" = "10-14",
    "15_19" = "15-19",
    "20_24" = "20-24",
    "25_29" = "25-29",
    "30_34" = "30-34",
    "35_39" = "35-39",
    "40_44" = "40-44",
    "45_49" = "45-49",
    "50_54" = "50-54",
    "55_59" = "55-59",
    "60_64" = "60-64",
    "65_69" = "65-69",
    "70_74" = "70-74",
    "75_79" = "75-79",
    "80_84" = "80-84",
    "85_plus" = "85+",
    .default = x
  )
}

format_table_for_export <- function(df) {
  df %>%
    mutate(
      across(where(is.numeric), ~ round(.x, 3))
    )
}

write_tex_table <- function(df, filename, caption = NULL) {
  tex_path <- file.path(tex_dir, filename)

  tex <- knitr::kable(
    format_table_for_export(df),
    format = "latex",
    booktabs = TRUE,
    caption = caption,
    longtable = FALSE
  )

  writeLines(tex, tex_path)
}

add_change_vars <- function(df, pop60 = "pop_1960", pop70 = "pop_1970") {
  df %>%
    mutate(
      change_abs = .data[[pop70]] - .data[[pop60]],
      change_pct = if_else(
        .data[[pop60]] > 0,
        100 * change_abs / .data[[pop60]],
        NA_real_
      ),
      ratio_1970_1960 = if_else(
        .data[[pop60]] > 0,
        .data[[pop70]] / .data[[pop60]],
        NA_real_
      ),
      declined = if_else(
        .data[[pop60]] > 0,
        change_abs < 0,
        NA
      )
    )
}

safe_binom_p <- function(x_decline, n) {
  if (is.na(n) || n == 0) return(NA_real_)
  binom.test(x_decline, n, p = 0.5, alternative = "greater")$p.value
}

safe_ttest_p <- function(log_change) {
  log_change <- log_change[is.finite(log_change)]
  if (length(log_change) < 2) return(NA_real_)
  t.test(log_change, mu = 0, alternative = "less")$p.value
}

safe_cor <- function(x, y, method = "pearson") {
  ok <- is.finite(x) & is.finite(y)
  if (sum(ok) < 3) return(NA_real_)
  cor(x[ok], y[ok], method = method)
}

safe_lm_stat <- function(x, y, statistic = c("slope", "r_squared")) {
  statistic <- match.arg(statistic)
  ok <- is.finite(x) & is.finite(y)

  if (sum(ok) < 3 || length(unique(x[ok])) < 2) {
    return(NA_real_)
  }

  model <- lm(y[ok] ~ x[ok])

  if (statistic == "slope") {
    return(unname(coef(model)[2]))
  }

  summary(model)$r.squared
}


# ==============================================================================
# 3. Read and clean 1960 aggregate data
# ==============================================================================

# The first row of the Excel file contains the real variable names.
raw_1960 <- readxl::read_excel(input_1960, skip = 1) %>%
  janitor::clean_names()

# clean_names() adds an x before names beginning with a number.
names(raw_1960) <- stringr::str_replace(
  names(raw_1960),
  "^x(?=[0-9])",
  ""
)

cat("1960 variable names after cleaning:\n")
print(names(raw_1960))

age_cols_1960 <- c(
  "menos_5",
  "5_9", "10_14", "15_19", "20_24", "25_29",
  "30_34", "35_39", "40_44", "45_49", "50_54",
  "55_59", "60_64", "65_69", "70_74", "75_79",
  "80_84", "85_mas"
)

required_1960 <- c(
  "provincia",
  "grupo",
  "sexo",
  "total",
  age_cols_1960
)

missing_1960 <- setdiff(required_1960, names(raw_1960))

if (length(missing_1960) > 0) {
  stop(
    "Missing expected variables in the 1960 Excel file: ",
    paste(missing_1960, collapse = ", ")
  )
}

# These are aggregate/subprovincial geographies and would double count.
drop_geographies_1960 <- c(
  "Gran Buenos Aires",
  "Partidos del Gran Buenos Aires"
)

spanish_1960_wide <- raw_1960 %>%
  mutate(
    provincia = clean_province_name(provincia),
    grupo = str_squish(as.character(grupo)),
    sexo = str_squish(as.character(sexo))
  ) %>%
  filter(grupo == "Espanoles" | grupo == "Españoles") %>%
  filter(!provincia %in% drop_geographies_1960) %>%
  mutate(
    across(c(total, all_of(age_cols_1960)), ~ as.numeric(.x))
  )

if (nrow(spanish_1960_wide) == 0) {
  stop("No rows for Spanish-born population were found in the 1960 file.")
}

spanish_1960_totals <- spanish_1960_wide %>%
  select(provincia, sexo, pop_1960 = total)

spanish_1960_age <- spanish_1960_wide %>%
  select(provincia, sexo, all_of(age_cols_1960)) %>%
  pivot_longer(
    cols = all_of(age_cols_1960),
    names_to = "age_group_raw",
    values_to = "pop_1960"
  ) %>%
  mutate(
    age_group = recode(
      age_group_raw,
      "menos_5" = "0_4",
      "85_mas" = "85_plus",
      .default = age_group_raw
    ),
    age_group = factor(age_group, levels = age_group_order),
    pop_1960 = as.numeric(pop_1960)
  ) %>%
  select(provincia, sexo, age_group, pop_1960)


# ==============================================================================
# 4. Read and clean 1970 microdata
# ==============================================================================

raw_1970 <- haven::read_dta(input_1970)

required_1970 <- c(
  "year",
  "geo1_ar1970",
  "sex",
  "age",
  "bplcountry",
  "yrimm",
  "perwt"
)

missing_1970 <- setdiff(required_1970, names(raw_1970))

if (length(missing_1970) > 0) {
  stop(
    "Missing expected variables in the 1970 Stata file: ",
    paste(missing_1970, collapse = ", ")
  )
}

province_universe_1970 <- raw_1970 %>%
  mutate(
    year_num = as.numeric(haven::zap_labels(year)),
    provincia = clean_province_name(haven::as_factor(geo1_ar1970))
  ) %>%
  filter(year_num == 1970) %>%
  filter(!is.na(provincia), provincia != "Unknown") %>%
  distinct(provincia)

spanish_1970_indiv <- raw_1970 %>%
  mutate(
    year_num = as.numeric(haven::zap_labels(year)),
    province_1970 = clean_province_name(
      haven::as_factor(geo1_ar1970)
    ),
    sex_label = clean_sex_1970(haven::as_factor(sex)),
    age_num = as.numeric(haven::zap_labels(age)),
    bpl_num = as.numeric(haven::zap_labels(bplcountry)),
    bpl_label = as.character(haven::as_factor(bplcountry)),
    yrimm_num = as.numeric(haven::zap_labels(yrimm)),
    weight = as.numeric(perwt),
    is_spanish = bpl_num == 43120 | bpl_label == "Spain",
    yrimm_known = (
      !is.na(yrimm_num) &
        !yrimm_num %in% c(0, 9998, 9999)
    )
  ) %>%
  filter(year_num == 1970) %>%
  filter(is_spanish) %>%
  filter(!is.na(province_1970), province_1970 != "Unknown") %>%
  filter(!is.na(sex_label)) %>%
  filter(!is.na(age_num), age_num < 999) %>%
  filter(!is.na(weight), weight >= 0) %>%
  mutate(
    age_group = make_age_group(age_num),
    age_group = factor(age_group, levels = age_group_order)
  ) %>%
  filter(!is.na(age_group))

if (nrow(spanish_1970_indiv) == 0) {
  stop("No Spanish-born observations were found in the 1970 microdata.")
}


# ==============================================================================
# 5. Alternative 1970 samples
# ==============================================================================

# all_spanish:
#   Original specification. Includes every Spanish-born person observed in 1970.
#
# known_arrival_by_1959:
#   Strict specification. Includes only known arrivals before 1960.
#
# known_arrival_by_1960:
#   Inclusive specification. Also includes arrivals reporting year 1960.
#
# arrival_by_1960_plus_unknown:
#   Upper-bound sensitivity. Includes known arrivals by 1960 and every person
#   whose year of arrival is unknown. This assumes, only for sensitivity, that
#   all unknown arrival years could correspond to people already present by 1960.

samples_1970 <- list(
  all_spanish = spanish_1970_indiv,

  known_arrival_by_1959 = spanish_1970_indiv %>%
    filter(
      yrimm_known,
      yrimm_num <= 1959
    ),

  known_arrival_by_1960 = spanish_1970_indiv %>%
    filter(
      yrimm_known,
      yrimm_num <= 1960
    ),

  arrival_by_1960_plus_unknown = spanish_1970_indiv %>%
    filter(
      (yrimm_known & yrimm_num <= 1960) |
        !yrimm_known
    )
)

scenario_labels <- c(
  all_spanish = "All Spanish-born observed in 1970",
  known_arrival_by_1959 = "Known arrival by 1959",
  known_arrival_by_1960 = "Known arrival by 1960",
  arrival_by_1960_plus_unknown = "Arrival by 1960 plus unknown arrival year"
)

scenario_order <- names(scenario_labels)

scenario_definitions <- tibble(
  scenario = scenario_order,
  label = unname(scenario_labels),
  interpretation = c(
    "Original comparison; includes arrivals after 1960.",
    "Strict preferred restriction; excludes arrivals in 1960 and later.",
    "Inclusive restriction; includes reported arrivals during 1960.",
    "Upper-bound sensitivity; includes unknown arrival years with arrivals by 1960."
  )
)


# ==============================================================================
# 6. Aggregate each 1970 sample
# ==============================================================================

aggregate_1970 <- function(df) {

  totals_by_sex <- df %>%
    group_by(
      provincia = province_1970,
      sexo = sex_label
    ) %>%
    summarise(
      pop_1970 = sum(weight, na.rm = TRUE),
      n_unweighted_1970 = n(),
      .groups = "drop"
    )

  totals_total_sex <- df %>%
    group_by(
      provincia = province_1970
    ) %>%
    summarise(
      pop_1970 = sum(weight, na.rm = TRUE),
      n_unweighted_1970 = n(),
      .groups = "drop"
    ) %>%
    mutate(sexo = "Total")

  totals <- bind_rows(
    totals_by_sex,
    totals_total_sex
  ) %>%
    select(
      provincia,
      sexo,
      pop_1970,
      n_unweighted_1970
    )

  age_by_sex <- df %>%
    group_by(
      provincia = province_1970,
      sexo = sex_label,
      age_group
    ) %>%
    summarise(
      pop_1970 = sum(weight, na.rm = TRUE),
      n_unweighted_1970 = n(),
      .groups = "drop"
    )

  age_total_sex <- df %>%
    group_by(
      provincia = province_1970,
      age_group
    ) %>%
    summarise(
      pop_1970 = sum(weight, na.rm = TRUE),
      n_unweighted_1970 = n(),
      .groups = "drop"
    ) %>%
    mutate(sexo = "Total")

  age <- bind_rows(
    age_by_sex,
    age_total_sex
  ) %>%
    select(
      provincia,
      sexo,
      age_group,
      pop_1970,
      n_unweighted_1970
    )

  list(
    totals = totals,
    age = age
  )
}

aggregates_1970 <- purrr::map(
  samples_1970,
  aggregate_1970
)

# Keep the original objects so the original tables remain directly comparable.
spanish_1970_totals <- aggregates_1970$all_spanish$totals
spanish_1970_age <- aggregates_1970$all_spanish$age


# ==============================================================================
# 7. Province matching check
# ==============================================================================

check_matching_provinces <- full_join(
  spanish_1960_totals %>%
    filter(sexo == "Total") %>%
    distinct(provincia) %>%
    mutate(in_1960 = TRUE),

  province_universe_1970 %>%
    mutate(in_1970 = TRUE),

  by = "provincia"
) %>%
  mutate(
    in_1960 = replace_na(in_1960, FALSE),
    in_1970 = replace_na(in_1970, FALSE),
    matched = in_1960 & in_1970
  ) %>%
  arrange(provincia)

unmatched_provinces <- check_matching_provinces %>%
  filter(!matched)

if (nrow(unmatched_provinces) > 0) {
  print(unmatched_provinces)
  stop(
    "Some province names do not match between 1960 and 1970. ",
    "Fix clean_province_name() before interpreting missing cells as zero."
  )
}


# ==============================================================================
# 8. Original comparisons: all Spanish-born observed in 1970
# ==============================================================================

# ------------------------------------------------------------------------------
# 8.1 Province x sex totals
# ------------------------------------------------------------------------------

comparison_province_sex <- spanish_1960_totals %>%
  full_join(
    spanish_1970_totals,
    by = c("provincia", "sexo")
  ) %>%
  mutate(
    pop_1960 = replace_na(pop_1960, 0),
    pop_1970 = replace_na(pop_1970, 0),
    n_unweighted_1970 = replace_na(n_unweighted_1970, 0L)
  ) %>%
  add_change_vars() %>%
  arrange(
    provincia,
    factor(sexo, levels = c("Total", "Varones", "Mujeres"))
  )

comparison_province_total <- comparison_province_sex %>%
  filter(sexo == "Total") %>%
  arrange(change_pct)

comparison_national_sex <- comparison_province_sex %>%
  group_by(sexo) %>%
  summarise(
    pop_1960 = sum(pop_1960, na.rm = TRUE),
    pop_1970 = sum(pop_1970, na.rm = TRUE),
    n_unweighted_1970 = sum(n_unweighted_1970, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  add_change_vars() %>%
  arrange(factor(sexo, levels = c("Total", "Varones", "Mujeres")))

# ------------------------------------------------------------------------------
# 8.2 Same-age comparison
# ------------------------------------------------------------------------------

# This is descriptive only. It compares different birth cohorts.
comparison_same_age <- spanish_1960_age %>%
  full_join(
    spanish_1970_age,
    by = c("provincia", "sexo", "age_group")
  ) %>%
  mutate(
    pop_1960 = replace_na(pop_1960, 0),
    pop_1970 = replace_na(pop_1970, 0),
    n_unweighted_1970 = replace_na(n_unweighted_1970, 0L)
  ) %>%
  add_change_vars() %>%
  mutate(
    age_group_label = age_label(as.character(age_group))
  ) %>%
  arrange(provincia, sexo, age_group)

comparison_same_age_national <- comparison_same_age %>%
  group_by(sexo, age_group, age_group_label) %>%
  summarise(
    pop_1960 = sum(pop_1960, na.rm = TRUE),
    pop_1970 = sum(pop_1970, na.rm = TRUE),
    n_unweighted_1970 = sum(n_unweighted_1970, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  add_change_vars() %>%
  arrange(
    factor(sexo, levels = c("Total", "Varones", "Mujeres")),
    age_group
  )

# ------------------------------------------------------------------------------
# 8.3 Cohort comparison
# ------------------------------------------------------------------------------

# Approximate ten-year cohort mapping.
# Closed age intervals are retained only when they map cleanly to another closed
# interval. The main comparison excludes ages 75-79, 80-84 and 85+ in 1960.

cohort_map <- tibble(
  age_group_1960 = factor(
    c(
      "0_4", "5_9", "10_14", "15_19", "20_24", "25_29",
      "30_34", "35_39", "40_44", "45_49", "50_54",
      "55_59", "60_64", "65_69", "70_74"
    ),
    levels = age_group_order
  ),
  age_group_1970 = factor(
    c(
      "10_14", "15_19", "20_24", "25_29", "30_34", "35_39",
      "40_44", "45_49", "50_54", "55_59", "60_64",
      "65_69", "70_74", "75_79", "80_84"
    ),
    levels = age_group_order
  )
)

cohort_1960 <- spanish_1960_age %>%
  rename(age_group_1960 = age_group) %>%
  inner_join(cohort_map, by = "age_group_1960")

build_cohort_comparison <- function(data_1970_age, scenario_name) {

  cohort_1970_scenario <- data_1970_age %>%
    rename(age_group_1970 = age_group)

  cohort_1960 %>%
    left_join(
      cohort_1970_scenario,
      by = c(
        "provincia",
        "sexo",
        "age_group_1970"
      )
    ) %>%
    mutate(
      pop_1970 = replace_na(pop_1970, 0),
      n_unweighted_1970 = replace_na(n_unweighted_1970, 0L)
    ) %>%
    add_change_vars() %>%
    mutate(
      scenario = scenario_name,
      scenario_label = unname(scenario_labels[scenario_name]),
      cohort_1960_label = age_label(as.character(age_group_1960)),
      cohort_1970_label = age_label(as.character(age_group_1970)),
      cohort_label = paste0(
        cohort_1960_label,
        " in 1960 -> ",
        cohort_1970_label,
        " in 1970"
      )
    ) %>%
    arrange(provincia, sexo, age_group_1960)
}

comparison_cohort_sensitivity <- purrr::imap_dfr(
  aggregates_1970,
  ~ build_cohort_comparison(
    data_1970_age = .x$age,
    scenario_name = .y
  )
) %>%
  mutate(
    scenario = factor(scenario, levels = scenario_order),
    scenario_label = factor(
      scenario_label,
      levels = unname(scenario_labels)
    )
  )

comparison_cohort <- comparison_cohort_sensitivity %>%
  filter(scenario == "all_spanish") %>%
  select(-scenario, -scenario_label)

comparison_cohort_national_sensitivity <- comparison_cohort_sensitivity %>%
  group_by(
    scenario,
    scenario_label,
    sexo,
    age_group_1960,
    age_group_1970,
    cohort_label
  ) %>%
  summarise(
    pop_1960 = sum(pop_1960, na.rm = TRUE),
    pop_1970 = sum(pop_1970, na.rm = TRUE),
    n_unweighted_1970 = sum(n_unweighted_1970, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  add_change_vars() %>%
  arrange(
    scenario,
    factor(sexo, levels = c("Total", "Varones", "Mujeres")),
    age_group_1960
  )

comparison_cohort_national <- comparison_cohort_national_sensitivity %>%
  filter(scenario == "all_spanish") %>%
  select(-scenario, -scenario_label)


# ==============================================================================
# 9. Alternative total-population comparisons by scenario
# ==============================================================================

build_province_sex_comparison <- function(data_1970_totals, scenario_name) {
  spanish_1960_totals %>%
    full_join(
      data_1970_totals,
      by = c("provincia", "sexo")
    ) %>%
    mutate(
      pop_1960 = replace_na(pop_1960, 0),
      pop_1970 = replace_na(pop_1970, 0),
      n_unweighted_1970 = replace_na(n_unweighted_1970, 0L),
      scenario = scenario_name,
      scenario_label = unname(scenario_labels[scenario_name])
    ) %>%
    add_change_vars() %>%
    arrange(
      provincia,
      factor(sexo, levels = c("Total", "Varones", "Mujeres"))
    )
}

comparison_province_sensitivity <- purrr::imap_dfr(
  aggregates_1970,
  ~ build_province_sex_comparison(
    data_1970_totals = .x$totals,
    scenario_name = .y
  )
) %>%
  mutate(
    scenario = factor(scenario, levels = scenario_order),
    scenario_label = factor(
      scenario_label,
      levels = unname(scenario_labels)
    )
  )

comparison_national_sensitivity <- comparison_province_sensitivity %>%
  group_by(
    scenario,
    scenario_label,
    sexo
  ) %>%
  summarise(
    pop_1960 = sum(pop_1960, na.rm = TRUE),
    pop_1970 = sum(pop_1970, na.rm = TRUE),
    n_unweighted_1970 = sum(n_unweighted_1970, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  add_change_vars() %>%
  arrange(
    scenario,
    factor(sexo, levels = c("Total", "Varones", "Mujeres"))
  )


# ==============================================================================
# 10. Arrival-year diagnostics
# ==============================================================================

all_weighted_1970 <- sum(spanish_1970_indiv$weight, na.rm = TRUE)
all_unweighted_1970 <- nrow(spanish_1970_indiv)

arrival_year_diagnostics <- bind_rows(
  tibble(
    category = "All Spanish-born observed in 1970",
    weighted_population = all_weighted_1970,
    unweighted_n = all_unweighted_1970
  ),
  tibble(
    category = "Known arrival by 1959",
    weighted_population = sum(
      spanish_1970_indiv$weight[
        spanish_1970_indiv$yrimm_known &
          spanish_1970_indiv$yrimm_num <= 1959
      ],
      na.rm = TRUE
    ),
    unweighted_n = sum(
      spanish_1970_indiv$yrimm_known &
        spanish_1970_indiv$yrimm_num <= 1959,
      na.rm = TRUE
    )
  ),
  tibble(
    category = "Known arrival in 1960",
    weighted_population = sum(
      spanish_1970_indiv$weight[
        spanish_1970_indiv$yrimm_known &
          spanish_1970_indiv$yrimm_num == 1960
      ],
      na.rm = TRUE
    ),
    unweighted_n = sum(
      spanish_1970_indiv$yrimm_known &
        spanish_1970_indiv$yrimm_num == 1960,
      na.rm = TRUE
    )
  ),
  tibble(
    category = "Known arrival after 1960",
    weighted_population = sum(
      spanish_1970_indiv$weight[
        spanish_1970_indiv$yrimm_known &
          spanish_1970_indiv$yrimm_num > 1960
      ],
      na.rm = TRUE
    ),
    unweighted_n = sum(
      spanish_1970_indiv$yrimm_known &
        spanish_1970_indiv$yrimm_num > 1960,
      na.rm = TRUE
    )
  ),
  tibble(
    category = "Unknown or invalid arrival year",
    weighted_population = sum(
      spanish_1970_indiv$weight[
        !spanish_1970_indiv$yrimm_known
      ],
      na.rm = TRUE
    ),
    unweighted_n = sum(
      !spanish_1970_indiv$yrimm_known,
      na.rm = TRUE
    )
  )
) %>%
  mutate(
    percent_of_all_weighted = 100 * weighted_population / all_weighted_1970,
    percent_of_all_unweighted = 100 * unweighted_n / all_unweighted_1970
  )

arrival_diagnostics_by_province <- spanish_1970_indiv %>%
  group_by(provincia = province_1970) %>%
  summarise(
    weighted_all = sum(weight, na.rm = TRUE),
    weighted_known_by_1959 = sum(
      weight[yrimm_known & yrimm_num <= 1959],
      na.rm = TRUE
    ),
    weighted_known_in_1960 = sum(
      weight[yrimm_known & yrimm_num == 1960],
      na.rm = TRUE
    ),
    weighted_known_after_1960 = sum(
      weight[yrimm_known & yrimm_num > 1960],
      na.rm = TRUE
    ),
    weighted_unknown = sum(
      weight[!yrimm_known],
      na.rm = TRUE
    ),
    unweighted_all = n(),
    .groups = "drop"
  ) %>%
  mutate(
    share_known_after_1960 = if_else(
      weighted_all > 0,
      100 * weighted_known_after_1960 / weighted_all,
      NA_real_
    ),
    share_unknown = if_else(
      weighted_all > 0,
      100 * weighted_unknown / weighted_all,
      NA_real_
    )
  ) %>%
  arrange(desc(share_known_after_1960))

scenario_sample_sizes <- purrr::imap_dfr(
  samples_1970,
  ~ tibble(
    scenario = .y,
    scenario_label = unname(scenario_labels[.y]),
    weighted_population = sum(.x$weight, na.rm = TRUE),
    unweighted_n = nrow(.x)
  )
) %>%
  mutate(
    percent_of_all_weighted = 100 * weighted_population / all_weighted_1970,
    percent_of_all_unweighted = 100 * unweighted_n / all_unweighted_1970,
    scenario = factor(scenario, levels = scenario_order),
    scenario_label = factor(
      scenario_label,
      levels = unname(scenario_labels)
    )
  ) %>%
  arrange(scenario)


# ==============================================================================
# 11. Descriptive tests
# ==============================================================================

# These are descriptive summaries across provinces/cells, not causal tests.

test_one_dataset <- function(df, label, scenario_name = NA_character_) {
  df2 <- df %>%
    filter(
      is.finite(pop_1960),
      is.finite(pop_1970),
      pop_1960 > 0
    ) %>%
    mutate(
      log_change = log(pop_1970 + 1) - log(pop_1960 + 1)
    )

  n_units <- nrow(df2)
  n_decline <- sum(df2$declined, na.rm = TRUE)

  tibble(
    scenario = scenario_name,
    analysis = label,
    n_units = n_units,
    n_decline = n_decline,
    share_decline = if_else(n_units > 0, n_decline / n_units, NA_real_),
    total_pop_1960 = sum(df2$pop_1960, na.rm = TRUE),
    total_pop_1970 = sum(df2$pop_1970, na.rm = TRUE),
    total_change_abs = total_pop_1970 - total_pop_1960,
    total_change_pct = if_else(
      total_pop_1960 > 0,
      100 * total_change_abs / total_pop_1960,
      NA_real_
    ),
    mean_change_abs = mean(df2$change_abs, na.rm = TRUE),
    median_change_abs = median(df2$change_abs, na.rm = TRUE),
    mean_change_pct = mean(df2$change_pct, na.rm = TRUE),
    median_change_pct = median(df2$change_pct, na.rm = TRUE),
    sign_test_p_value_decline = safe_binom_p(n_decline, n_units),
    t_test_p_value_log_decline = safe_ttest_p(df2$log_change)
  )
}

tests_summary <- bind_rows(
  test_one_dataset(
    comparison_province_total,
    "Province totals, sex = Total"
  ),
  test_one_dataset(
    comparison_province_sex %>%
      filter(sexo %in% c("Varones", "Mujeres")),
    "Province x sex, excluding sex = Total"
  ),
  test_one_dataset(
    comparison_same_age %>%
      filter(sexo == "Total"),
    "Province x same age group, sex = Total"
  ),
  test_one_dataset(
    comparison_cohort %>%
      filter(sexo == "Total"),
    "Province x cohort age group, sex = Total"
  )
)

tests_cohort_sensitivity <- comparison_cohort_sensitivity %>%
  filter(sexo == "Total") %>%
  group_split(scenario, scenario_label) %>%
  purrr::map_dfr(
    function(df) {
      scenario_name <- as.character(df$scenario[1])
      scenario_label_value <- as.character(df$scenario_label[1])

      test_one_dataset(
        df,
        label = "Province x cohort age group, sex = Total",
        scenario_name = scenario_name
      ) %>%
        mutate(
          scenario_label = scenario_label_value,
          .after = scenario
        )
    }
  ) %>%
  mutate(
    scenario = factor(scenario, levels = scenario_order),
    scenario_label = factor(
      scenario_label,
      levels = unname(scenario_labels)
    )
  ) %>%
  arrange(scenario)


# ==============================================================================
# 12. Spatial stability of province-level Spanish-born stocks
# ==============================================================================

# This uses counts, not percentages of provincial population, because the 1960
# total-population denominators are not part of this input file.

spatial_stability <- comparison_province_sensitivity %>%
  filter(sexo == "Total") %>%
  group_by(scenario, scenario_label) %>%
  summarise(
    n_provinces = n(),
    pearson_correlation = safe_cor(
      pop_1960,
      pop_1970,
      method = "pearson"
    ),
    spearman_correlation = safe_cor(
      pop_1960,
      pop_1970,
      method = "spearman"
    ),
    regression_slope = safe_lm_stat(
      pop_1960,
      pop_1970,
      statistic = "slope"
    ),
    regression_r_squared = safe_lm_stat(
      pop_1960,
      pop_1970,
      statistic = "r_squared"
    ),
    .groups = "drop"
  ) %>%
  mutate(
    scenario = factor(scenario, levels = scenario_order),
    scenario_label = factor(
      scenario_label,
      levels = unname(scenario_labels)
    )
  ) %>%
  arrange(scenario)


# ==============================================================================
# 13. Data checks
# ==============================================================================

check_1960_total_vs_age_sum <- spanish_1960_wide %>%
  mutate(
    age_sum_known = rowSums(
      across(all_of(age_cols_1960)),
      na.rm = TRUE
    ),
    diff_total_minus_known_age_sum = total - age_sum_known
  ) %>%
  select(
    provincia,
    sexo,
    total,
    age_sum_known,
    diff_total_minus_known_age_sum
  ) %>%
  arrange(desc(abs(diff_total_minus_known_age_sum)))

check_1970_provinces <- spanish_1970_totals %>%
  filter(sexo == "Total") %>%
  arrange(provincia)


# ==============================================================================
# 14. Tables for paper / Overleaf
# ==============================================================================

# Original tables, unchanged in meaning.

table_1_national_sex <- comparison_national_sex %>%
  transmute(
    Sex = sexo,
    `Spanish-born population, 1960` = pop_1960,
    `Spanish-born population, 1970` = pop_1970,
    `Absolute change` = change_abs,
    `% change` = change_pct,
    `1970 / 1960` = ratio_1970_1960,
    `Unweighted N, 1970` = n_unweighted_1970
  )

table_2_province_total <- comparison_province_total %>%
  transmute(
    Province = provincia,
    `Spanish-born population, 1960` = pop_1960,
    `Spanish-born population, 1970` = pop_1970,
    `Absolute change` = change_abs,
    `% change` = change_pct,
    `1970 / 1960` = ratio_1970_1960,
    Declined = declined,
    `Unweighted N, 1970` = n_unweighted_1970
  ) %>%
  arrange(`% change`)

table_3_province_sex <- comparison_province_sex %>%
  transmute(
    Province = provincia,
    Sex = sexo,
    `Spanish-born population, 1960` = pop_1960,
    `Spanish-born population, 1970` = pop_1970,
    `Absolute change` = change_abs,
    `% change` = change_pct,
    `1970 / 1960` = ratio_1970_1960,
    Declined = declined,
    `Unweighted N, 1970` = n_unweighted_1970
  ) %>%
  arrange(
    Province,
    factor(Sex, levels = c("Total", "Varones", "Mujeres"))
  )

table_4_same_age_national <- comparison_same_age_national %>%
  filter(sexo == "Total") %>%
  transmute(
    `Age group` = age_group_label,
    `Spanish-born population, 1960` = pop_1960,
    `Spanish-born population, 1970` = pop_1970,
    `Absolute change` = change_abs,
    `% change` = change_pct,
    `1970 / 1960` = ratio_1970_1960,
    Declined = declined,
    `Unweighted N, 1970` = n_unweighted_1970
  )

table_5_cohort_national <- comparison_cohort_national %>%
  filter(sexo == "Total") %>%
  transmute(
    Cohort = cohort_label,
    `Spanish-born population, 1960` = pop_1960,
    `Spanish-born population, 1970` = pop_1970,
    `Absolute change` = change_abs,
    `% change` = change_pct,
    `1970 / 1960` = ratio_1970_1960,
    Declined = declined,
    `Unweighted N, 1970` = n_unweighted_1970
  )

table_6_tests <- tests_summary %>%
  transmute(
    Analysis = analysis,
    `N units` = n_units,
    `N declined` = n_decline,
    `Share declined` = share_decline,
    `Total 1960` = total_pop_1960,
    `Total 1970` = total_pop_1970,
    `Total absolute change` = total_change_abs,
    `Total % change` = total_change_pct,
    `Sign test p-value` = sign_test_p_value_decline,
    `Paired t-test p-value, log decline` = t_test_p_value_log_decline
  )

# New sensitivity tables.

table_7_cohort_sensitivity_long <- comparison_cohort_national_sensitivity %>%
  filter(sexo == "Total") %>%
  transmute(
    Cohort = cohort_label,
    `1970 sample` = as.character(scenario_label),
    `Spanish-born population, 1960` = pop_1960,
    `Spanish-born population, 1970` = pop_1970,
    `Absolute change` = change_abs,
    `% change` = change_pct,
    `1970 / 1960` = ratio_1970_1960,
    `Unweighted N, 1970` = n_unweighted_1970
  )

table_7_cohort_sensitivity_wide <- comparison_cohort_national_sensitivity %>%
  filter(sexo == "Total") %>%
  mutate(
    scenario_short = recode(
      as.character(scenario),
      all_spanish = "all",
      known_arrival_by_1959 = "pre1959",
      known_arrival_by_1960 = "by1960",
      arrival_by_1960_plus_unknown = "by1960_plus_unknown"
    )
  ) %>%
  select(
    cohort_label,
    age_group_1960,
    scenario_short,
    pop_1960,
    pop_1970,
    change_pct,
    ratio_1970_1960,
    n_unweighted_1970
  ) %>%
  pivot_wider(
    names_from = scenario_short,
    values_from = c(
      pop_1970,
      change_pct,
      ratio_1970_1960,
      n_unweighted_1970
    ),
    names_glue = "{.value}_{scenario_short}"
  ) %>%
  arrange(age_group_1960) %>%
  select(-age_group_1960) %>%
  rename(Cohort = cohort_label)

table_8_national_sensitivity <- comparison_national_sensitivity %>%
  transmute(
    `1970 sample` = as.character(scenario_label),
    Sex = sexo,
    `Spanish-born population, 1960` = pop_1960,
    `Spanish-born population, 1970` = pop_1970,
    `Absolute change` = change_abs,
    `% change` = change_pct,
    `1970 / 1960` = ratio_1970_1960,
    `Unweighted N, 1970` = n_unweighted_1970
  )

table_9_arrival_diagnostics <- arrival_year_diagnostics %>%
  transmute(
    Category = category,
    `Weighted population` = weighted_population,
    `Unweighted N` = unweighted_n,
    `% of all, weighted` = percent_of_all_weighted,
    `% of all, unweighted` = percent_of_all_unweighted
  )

table_10_sensitivity_tests <- tests_cohort_sensitivity %>%
  transmute(
    `1970 sample` = as.character(scenario_label),
    `N province-cohort cells` = n_units,
    `N declined` = n_decline,
    `Share declined` = share_decline,
    `Total 1960` = total_pop_1960,
    `Total 1970` = total_pop_1970,
    `Total % change` = total_change_pct,
    `Median cell % change` = median_change_pct,
    `Sign test p-value` = sign_test_p_value_decline,
    `Paired t-test p-value, log decline` = t_test_p_value_log_decline
  )

table_11_spatial_stability <- spatial_stability %>%
  transmute(
    `1970 sample` = as.character(scenario_label),
    `N provinces` = n_provinces,
    `Pearson correlation` = pearson_correlation,
    `Spearman correlation` = spearman_correlation,
    `Regression slope` = regression_slope,
    `Regression R-squared` = regression_r_squared
  )


# ==============================================================================
# 15. Export Excel workbook
# ==============================================================================

xlsx_out <- file.path(
  out_dir,
  "spanish_population_1960_1970_arrival_sensitivity.xlsx"
)

wb <- createWorkbook()

sheets <- list(
  "T1_national_sex" = table_1_national_sex,
  "T2_province_total" = table_2_province_total,
  "T3_province_sex" = table_3_province_sex,
  "T4_same_age_national" = table_4_same_age_national,
  "T5_cohort_national" = table_5_cohort_national,
  "T6_tests" = table_6_tests,
  "T7_cohort_sens_long" = table_7_cohort_sensitivity_long,
  "T7_cohort_sens_wide" = table_7_cohort_sensitivity_wide,
  "T8_national_sens" = table_8_national_sensitivity,
  "T9_arrival_diag" = table_9_arrival_diagnostics,
  "T10_sens_tests" = table_10_sensitivity_tests,
  "T11_spatial_stability" = table_11_spatial_stability,
  "scenario_definitions" = scenario_definitions,
  "scenario_sample_sizes" = scenario_sample_sizes,
  "arrival_diag_province" = arrival_diagnostics_by_province,
  "full_province_sex" = comparison_province_sex,
  "full_same_age" = comparison_same_age,
  "full_cohort" = comparison_cohort,
  "full_province_sens" = comparison_province_sensitivity,
  "full_cohort_sens" = comparison_cohort_sensitivity,
  "check_1960_age_sum" = check_1960_total_vs_age_sum,
  "check_1970_provinces" = check_1970_provinces,
  "check_matching_provinces" = check_matching_provinces
)

for (sheet_name in names(sheets)) {
  addWorksheet(wb, sheet_name)
  writeData(wb, sheet_name, sheets[[sheet_name]])
  freezePane(wb, sheet_name, firstRow = TRUE)
  setColWidths(
    wb,
    sheet_name,
    cols = seq_len(ncol(sheets[[sheet_name]])),
    widths = "auto"
  )
}

saveWorkbook(wb, xlsx_out, overwrite = TRUE)

cat("\nExcel workbook exported to:\n")
cat(xlsx_out, "\n")


# ==============================================================================
# 16. Export LaTeX tables for Overleaf
# ==============================================================================

write_tex_table(
  table_1_national_sex,
  "table_1_national_sex.tex",
  caption = "Spanish-born population in Argentina by sex, 1960 and 1970"
)

write_tex_table(
  table_2_province_total,
  "table_2_province_total.tex",
  caption = "Spanish-born population in Argentina by province, 1960 and 1970"
)

write_tex_table(
  table_3_province_sex,
  "table_3_province_sex.tex",
  caption = "Spanish-born population in Argentina by province and sex, 1960 and 1970"
)

write_tex_table(
  table_4_same_age_national,
  "table_4_same_age_national.tex",
  caption = "Spanish-born population by age group, same-age comparison, 1960 and 1970"
)

write_tex_table(
  table_5_cohort_national,
  "table_5_cohort_national.tex",
  caption = "Spanish-born population by approximate birth cohort, 1960 to 1970"
)

write_tex_table(
  table_6_tests,
  "table_6_tests.tex",
  caption = "Descriptive tests of decline in Spanish-born population, 1960 to 1970"
)

write_tex_table(
  table_7_cohort_sensitivity_long,
  "table_7_cohort_arrival_sensitivity.tex",
  caption = paste0(
    "Spanish-born population by cohort under alternative restrictions ",
    "on year of arrival in the 1970 census"
  )
)

write_tex_table(
  table_8_national_sensitivity,
  "table_8_national_arrival_sensitivity.tex",
  caption = paste0(
    "National Spanish-born population under alternative restrictions ",
    "on year of arrival in the 1970 census"
  )
)

write_tex_table(
  table_9_arrival_diagnostics,
  "table_9_arrival_year_diagnostics.tex",
  caption = paste0(
    "Distribution of year-of-arrival information among Spanish-born ",
    "residents observed in the 1970 census"
  )
)

write_tex_table(
  table_10_sensitivity_tests,
  "table_10_cohort_sensitivity_tests.tex",
  caption = paste0(
    "Descriptive cohort-attrition summaries under alternative ",
    "year-of-arrival restrictions"
  )
)

write_tex_table(
  table_11_spatial_stability,
  "table_11_spatial_stability.tex",
  caption = paste0(
    "Province-level stability of Spanish-born population stocks, ",
    "1960 to 1970"
  )
)

cat("\nLaTeX tables exported to:\n")
cat(tex_dir, "\n")


# ==============================================================================
# 17. Console summary
# ==============================================================================

cat("\n============================================================\n")
cat("Original national result: all Spanish-born observed in 1970\n")
cat("============================================================\n")
print(table_1_national_sex)

cat("\n============================================================\n")
cat("Arrival-year diagnostics\n")
cat("============================================================\n")
print(table_9_arrival_diagnostics)

cat("\n============================================================\n")
cat("National sensitivity results\n")
cat("============================================================\n")
print(table_8_national_sensitivity)

cat("\n============================================================\n")
cat("Cohort sensitivity tests\n")
cat("============================================================\n")
print(table_10_sensitivity_tests)

cat("\nDone.\n")
