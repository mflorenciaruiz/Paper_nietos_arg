################################################################################
# Compare Spanish-born population between Argentina Census 1960 and 1970
# Province x sex x age-group analysis
#
# Inputs:
#   1960: cuadro5_clean.xlsx
#   1970: censos_arg.dta
#
# Outputs:
#   Excel workbook with all tables
#   LaTeX tables for Overleaf
#
# Notes:
#   - 1960 comes from aggregate tables.
#   - 1970 comes from IPUMS-style microdata and uses person weights (perwt).
#   - "Spanish population" is defined as people born in Spain.
#   - The safest age comparison is by cohort:
#       age group in 1960 is compared to the same cohort 10 years older in 1970.
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
  install.packages(to_install)
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

# The data are in Data Raw/Censo.
censo_dir <- file.path(path_pili, "Data Raw", "Censo")

input_1960 <- file.path(censo_dir, "cuadro5_clean.xlsx")
input_1970 <- file.path(censo_dir, "censos_arg.dta")

out_dir <- file.path(path_pili, "Output", "spanish_population_1960_1970")
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
  
  x <- ifelse(x == "Buenos Aires province", "Buenos Aires", x)
  x <- ifelse(x == "Entre Ríos", "Entre Rios", x)
  x <- ifelse(x == "Tierra del Fuego, Antártida e Islas del Atlántico Sur",
              "Tierra del Fuego", x)
  x <- ifelse(x == "Tierra del Fuego, Antartida e Islas del Atlantico Sur",
              "Tierra del Fuego", x)
  
  return(x)
}

clean_sex_1970 <- function(x) {
  x <- as.character(x)
  
  case_when(
    x %in% c("Male", "Varones", "Varon", "Varón", "Hombre", "Hombres") ~ "Varones",
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
      change_pct = 100 * change_abs / .data[[pop60]],
      ratio_1970_1960 = .data[[pop70]] / .data[[pop60]],
      declined = change_abs < 0
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


# ==============================================================================
# 3. Read and clean 1960 aggregate data
# ==============================================================================

# The first row of the Excel file contains the real variable names, so skip = 1.
raw_1960 <- readxl::read_excel(input_1960, skip = 1) %>%
  janitor::clean_names()

# janitor::clean_names() adds an "x" before variable names that start with a number:
# e.g. 5_9 becomes x5_9. This line removes that leading x only in age columns.
names(raw_1960) <- stringr::str_replace(names(raw_1960), "^x(?=[0-9])", "")

cat("1960 variable names after cleaning:\n")
print(names(raw_1960))

age_cols_1960 <- c(
  "menos_5",
  "5_9", "10_14", "15_19", "20_24", "25_29",
  "30_34", "35_39", "40_44", "45_49", "50_54",
  "55_59", "60_64", "65_69", "70_74", "75_79",
  "80_84", "85_mas"
)

required_1960 <- c("provincia", "grupo", "sexo", "total", age_cols_1960)

missing_1960 <- setdiff(required_1960, names(raw_1960))

if (length(missing_1960) > 0) {
  stop(
    "Missing expected variables in the 1960 Excel file: ",
    paste(missing_1960, collapse = ", ")
  )
}

# Keep only Spanish-born population and real provinces.
# Drop Gran Buenos Aires and Partidos del Gran Buenos Aires because those are
# aggregate/subprovincial geographies and would generate double counting.
drop_geographies_1960 <- c("Gran Buenos Aires", "Partidos del Gran Buenos Aires")

spanish_1960_wide <- raw_1960 %>%
  mutate(
    provincia = clean_province_name(provincia),
    grupo = str_squish(as.character(grupo)),
    sexo = str_squish(as.character(sexo))
  ) %>%
  filter(grupo == "Españoles") %>%
  filter(!provincia %in% drop_geographies_1960) %>%
  mutate(
    across(c(total, all_of(age_cols_1960)), ~ as.numeric(.x))
  )

# 1960: total by province and sex from the total column.
spanish_1960_totals <- spanish_1960_wide %>%
  select(provincia, sexo, pop_1960 = total)

# 1960: province x sex x age group.
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

required_1970 <- c("year", "geo1_ar1970", "sex", "age", "bplcountry", "perwt")
missing_1970 <- setdiff(required_1970, names(raw_1970))

if (length(missing_1970) > 0) {
  stop(
    "Missing expected variables in the 1970 Stata file: ",
    paste(missing_1970, collapse = ", ")
  )
}

spanish_1970_indiv <- raw_1970 %>%
  mutate(
    year_num = as.numeric(haven::zap_labels(year)),
    province_1970 = clean_province_name(haven::as_factor(geo1_ar1970)),
    sex_label = clean_sex_1970(haven::as_factor(sex)),
    age_num = as.numeric(haven::zap_labels(age)),
    bpl_num = as.numeric(haven::zap_labels(bplcountry)),
    bpl_label = as.character(haven::as_factor(bplcountry)),
    weight = as.numeric(perwt),
    is_spanish = bpl_num == 43120 | bpl_label == "Spain"
  ) %>%
  filter(year_num == 1970) %>%
  filter(is_spanish) %>%
  filter(!is.na(province_1970), province_1970 != "Unknown") %>%
  filter(!is.na(sex_label)) %>%
  filter(!is.na(age_num), age_num < 999) %>%
  mutate(
    age_group = make_age_group(age_num),
    age_group = factor(age_group, levels = age_group_order)
  ) %>%
  filter(!is.na(age_group))

# 1970: totals by province and sex, plus Total sex.
spanish_1970_totals_by_sex <- spanish_1970_indiv %>%
  group_by(provincia = province_1970, sexo = sex_label) %>%
  summarise(pop_1970 = sum(weight, na.rm = TRUE), .groups = "drop")

spanish_1970_totals_total_sex <- spanish_1970_indiv %>%
  group_by(provincia = province_1970) %>%
  summarise(pop_1970 = sum(weight, na.rm = TRUE), .groups = "drop") %>%
  mutate(sexo = "Total")

spanish_1970_totals <- bind_rows(
  spanish_1970_totals_by_sex,
  spanish_1970_totals_total_sex
) %>%
  select(provincia, sexo, pop_1970)

# 1970: province x sex x age group, plus Total sex.
spanish_1970_age_by_sex <- spanish_1970_indiv %>%
  group_by(provincia = province_1970, sexo = sex_label, age_group) %>%
  summarise(pop_1970 = sum(weight, na.rm = TRUE), .groups = "drop")

spanish_1970_age_total_sex <- spanish_1970_indiv %>%
  group_by(provincia = province_1970, age_group) %>%
  summarise(pop_1970 = sum(weight, na.rm = TRUE), .groups = "drop") %>%
  mutate(sexo = "Total")

spanish_1970_age <- bind_rows(
  spanish_1970_age_by_sex,
  spanish_1970_age_total_sex
) %>%
  select(provincia, sexo, age_group, pop_1970)


# ==============================================================================
# 5. Main comparisons
# ==============================================================================

# ------------------------------------------------------------------------------
# 5.1 Province x sex totals
# ------------------------------------------------------------------------------

comparison_province_sex <- spanish_1960_totals %>%
  full_join(spanish_1970_totals, by = c("provincia", "sexo")) %>%
  mutate(
    pop_1960 = replace_na(pop_1960, 0),
    pop_1970 = replace_na(pop_1970, 0)
  ) %>%
  add_change_vars() %>%
  arrange(provincia, factor(sexo, levels = c("Total", "Varones", "Mujeres")))

comparison_province_total <- comparison_province_sex %>%
  filter(sexo == "Total") %>%
  arrange(change_pct)

comparison_national_sex <- comparison_province_sex %>%
  group_by(sexo) %>%
  summarise(
    pop_1960 = sum(pop_1960, na.rm = TRUE),
    pop_1970 = sum(pop_1970, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  add_change_vars() %>%
  arrange(factor(sexo, levels = c("Total", "Varones", "Mujeres")))

# ------------------------------------------------------------------------------
# 5.2 Same-age comparison
# ------------------------------------------------------------------------------

comparison_same_age <- spanish_1960_age %>%
  full_join(spanish_1970_age, by = c("provincia", "sexo", "age_group")) %>%
  mutate(
    pop_1960 = replace_na(pop_1960, 0),
    pop_1970 = replace_na(pop_1970, 0)
  ) %>%
  add_change_vars() %>%
  mutate(age_group_label = age_label(as.character(age_group))) %>%
  arrange(provincia, sexo, age_group)

comparison_same_age_national <- comparison_same_age %>%
  group_by(sexo, age_group, age_group_label) %>%
  summarise(
    pop_1960 = sum(pop_1960, na.rm = TRUE),
    pop_1970 = sum(pop_1970, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  add_change_vars() %>%
  arrange(factor(sexo, levels = c("Total", "Varones", "Mujeres")), age_group)

# ------------------------------------------------------------------------------
# 5.3 Cohort comparison
# ------------------------------------------------------------------------------

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

cohort_1970 <- spanish_1970_age %>%
  rename(age_group_1970 = age_group)

comparison_cohort <- cohort_1960 %>%
  left_join(
    cohort_1970,
    by = c("provincia", "sexo", "age_group_1970")
  ) %>%
  mutate(
    pop_1970 = replace_na(pop_1970, 0)
  ) %>%
  add_change_vars() %>%
  mutate(
    cohort_1960_label = age_label(as.character(age_group_1960)),
    cohort_1970_label = age_label(as.character(age_group_1970)),
    cohort_label = paste0(cohort_1960_label, " in 1960 -> ", cohort_1970_label, " in 1970")
  ) %>%
  arrange(provincia, sexo, age_group_1960)

comparison_cohort_national <- comparison_cohort %>%
  group_by(sexo, age_group_1960, age_group_1970, cohort_label) %>%
  summarise(
    pop_1960 = sum(pop_1960, na.rm = TRUE),
    pop_1970 = sum(pop_1970, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  add_change_vars() %>%
  arrange(factor(sexo, levels = c("Total", "Varones", "Mujeres")), age_group_1960)


# ==============================================================================
# 6. Descriptive tests
# ==============================================================================

test_one_dataset <- function(df, label) {
  df2 <- df %>%
    filter(is.finite(pop_1960), is.finite(pop_1970), pop_1960 > 0) %>%
    mutate(
      log_change = log(pop_1970 + 1) - log(pop_1960 + 1)
    )
  
  n_units <- nrow(df2)
  n_decline <- sum(df2$declined, na.rm = TRUE)
  
  tibble(
    analysis = label,
    n_units = n_units,
    n_decline = n_decline,
    share_decline = n_decline / n_units,
    total_pop_1960 = sum(df2$pop_1960, na.rm = TRUE),
    total_pop_1970 = sum(df2$pop_1970, na.rm = TRUE),
    total_change_abs = total_pop_1970 - total_pop_1960,
    total_change_pct = 100 * total_change_abs / total_pop_1960,
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
    comparison_province_sex %>% filter(sexo %in% c("Varones", "Mujeres")),
    "Province x sex, excluding sex = Total"
  ),
  test_one_dataset(
    comparison_same_age %>% filter(sexo == "Total"),
    "Province x same age group, sex = Total"
  ),
  test_one_dataset(
    comparison_cohort %>% filter(sexo == "Total"),
    "Province x cohort age group, sex = Total"
  )
)


# ==============================================================================
# 7. Data checks
# ==============================================================================

check_1960_total_vs_age_sum <- spanish_1960_wide %>%
  mutate(
    age_sum_known = rowSums(across(all_of(age_cols_1960)), na.rm = TRUE),
    diff_total_minus_known_age_sum = total - age_sum_known
  ) %>%
  select(provincia, sexo, total, age_sum_known, diff_total_minus_known_age_sum) %>%
  arrange(desc(abs(diff_total_minus_known_age_sum)))

check_1970_provinces <- spanish_1970_totals %>%
  filter(sexo == "Total") %>%
  arrange(provincia)

check_matching_provinces <- full_join(
  spanish_1960_totals %>%
    filter(sexo == "Total") %>%
    distinct(provincia) %>%
    mutate(in_1960 = TRUE),
  spanish_1970_totals %>%
    filter(sexo == "Total") %>%
    distinct(provincia) %>%
    mutate(in_1970 = TRUE),
  by = "provincia"
) %>%
  mutate(
    in_1960 = replace_na(in_1960, FALSE),
    in_1970 = replace_na(in_1970, FALSE)
  ) %>%
  arrange(provincia)


# ==============================================================================
# 8. Tables for paper / Overleaf
# ==============================================================================

table_1_national_sex <- comparison_national_sex %>%
  transmute(
    Sex = sexo,
    `Spanish-born population, 1960` = pop_1960,
    `Spanish-born population, 1970` = pop_1970,
    `Absolute change` = change_abs,
    `% change` = change_pct,
    `1970 / 1960` = ratio_1970_1960
  )

table_2_province_total <- comparison_province_total %>%
  transmute(
    Province = provincia,
    `Spanish-born population, 1960` = pop_1960,
    `Spanish-born population, 1970` = pop_1970,
    `Absolute change` = change_abs,
    `% change` = change_pct,
    `Declined` = declined
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
    `Declined` = declined
  ) %>%
  arrange(Province, factor(Sex, levels = c("Total", "Varones", "Mujeres")))

table_4_same_age_national <- comparison_same_age_national %>%
  filter(sexo == "Total") %>%
  transmute(
    `Age group` = age_group_label,
    `Spanish-born population, 1960` = pop_1960,
    `Spanish-born population, 1970` = pop_1970,
    `Absolute change` = change_abs,
    `% change` = change_pct,
    `Declined` = declined
  )

table_5_cohort_national <- comparison_cohort_national %>%
  filter(sexo == "Total") %>%
  transmute(
    Cohort = cohort_label,
    `Spanish-born population, 1960` = pop_1960,
    `Spanish-born population, 1970` = pop_1970,
    `Absolute change` = change_abs,
    `% change` = change_pct,
    `Declined` = declined
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


# ==============================================================================
# 9. Export Excel workbook
# ==============================================================================

xlsx_out <- file.path(out_dir, "spanish_population_1960_1970_comparison.xlsx")

wb <- createWorkbook()

sheets <- list(
  "T1_national_sex" = table_1_national_sex,
  "T2_province_total" = table_2_province_total,
  "T3_province_sex" = table_3_province_sex,
  "T4_same_age_national" = table_4_same_age_national,
  "T5_cohort_national" = table_5_cohort_national,
  "T6_tests" = table_6_tests,
  "full_province_sex" = comparison_province_sex,
  "full_same_age" = comparison_same_age,
  "full_cohort" = comparison_cohort,
  "check_1960_age_sum" = check_1960_total_vs_age_sum,
  "check_1970_provinces" = check_1970_provinces,
  "check_matching_provinces" = check_matching_provinces
)

for (sheet_name in names(sheets)) {
  addWorksheet(wb, sheet_name)
  writeData(wb, sheet_name, sheets[[sheet_name]])
  freezePane(wb, sheet_name, firstRow = TRUE)
  setColWidths(wb, sheet_name, cols = 1:ncol(sheets[[sheet_name]]), widths = "auto")
}

saveWorkbook(wb, xlsx_out, overwrite = TRUE)

cat("\nExcel workbook exported to:\n")
cat(xlsx_out, "\n")


# ==============================================================================
# 10. Export LaTeX tables for Overleaf
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
  caption = "Spanish-born population by cohort, 1960 to 1970"
)

write_tex_table(
  table_6_tests,
  "table_6_tests.tex",
  caption = "Descriptive tests of decline in Spanish-born population, 1960 to 1970"
)

cat("\nLaTeX tables exported to:\n")
cat(tex_dir, "\n")


# ==============================================================================
# 11. Console summary
# ==============================================================================

cat("\n============================================================\n")
cat("Main national result\n")
cat("============================================================\n")
print(table_1_national_sex)

cat("\n============================================================\n")
cat("Descriptive tests\n")
cat("============================================================\n")
print(table_6_tests)

# ==============================================================================
# Extra Overleaf tables requested
# ==============================================================================

# ------------------------------------------------------------------------------
# 1. T3 modified: province-level % change by total, women, and men
# ------------------------------------------------------------------------------

table_3_pct_change_wide <- comparison_province_sex %>%
  mutate(
    sexo = dplyr::recode(
      sexo,
      "Total" = "Total",
      "Mujeres" = "Women",
      "Varones" = "Men",
      .default = sexo
    )
  ) %>%
  select(
    Province = provincia,
    Sex = sexo,
    pct_change = change_pct
  ) %>%
  tidyr::pivot_wider(
    names_from = Sex,
    values_from = pct_change
  ) %>%
  transmute(
    Province,
    `% change, province total` = Total,
    `% change, women` = Women,
    `% change, men` = Men
  ) %>%
  arrange(`% change, province total`)

# Optional: round for display
table_3_pct_change_wide_export <- table_3_pct_change_wide %>%
  mutate(
    across(where(is.numeric), ~ round(.x, 2))
  )

# Export to LaTeX
write_tex_table(
  table_3_pct_change_wide_export,
  "table_3_pct_change_by_sex_wide.tex",
  caption = "Percentage change in the Spanish-born population by province and sex, 1960--1970"
)


# ------------------------------------------------------------------------------
# 2. T5 cohort national table
# ------------------------------------------------------------------------------

table_5_cohort_national_export <- table_5_cohort_national %>%
  mutate(
    across(where(is.numeric), ~ round(.x, 2))
  )

write_tex_table(
  table_5_cohort_national_export,
  "table_5_cohort_national.tex",
  caption = "Spanish-born population by cohort, 1960--1970"
)

table_5_cohort_pct_change <- table_5_cohort_national %>%
  transmute(
    Cohort = Cohort,
    `% change` = round(`% change`, 2)
  ) %>%
  arrange(`% change`)

write_tex_table(
  table_5_cohort_pct_change,
  "table_5_cohort_pct_change_only.tex",
  caption = "Percentage change in the Spanish-born population by cohort, 1960--1970"
)

cat("\nDone.\n")
