# ---------------------------------------------------------------------------- #
#     LAPOP: diagnóstico de coeficientes grandes en modelos Logit
# ---------------------------------------------------------------------------- #
#
# Este script no estima modelos principales.
# Lee:
#   - la base LAPOP
#   - los modelos Logit guardados como RDS
#   - las tablas Excel de resultados Logit
#
# Objetivo:
#   Detectar coeficientes Logit grandes y explorar posibles causas:
#     1. odds ratios extremos
#     2. colinealidad
#     3. separación o poca variación de Y por municipio-año
#     4. escala/outliers de X y shares
#     5. escala de interacciones
#
# ---------------------------------------------------------------------------- #

library(dplyr)
library(readr)
library(readxl)
library(stringr)
library(purrr)
library(tibble)
library(openxlsx)
library(fixest)

# ---------------------------------------------------------------------------- #
# 0. Paths y carga
# ---------------------------------------------------------------------------- #

path_pili <- "C:\\Users\\pilih\\Documents\\Papers German\\Valerie\\Paper_nietos_arg"
setwd(path_pili)

dir.create("Output/diagnostics", showWarnings = FALSE, recursive = TRUE)

lapop <- read_csv("Data Out/lapop_data_merge.csv", show_col_types = FALSE)

lapop <- lapop %>%
  mutate(
    year_num = as.numeric(year),
    post = if_else(year_num >= 2023, 1, 0),
    mun_code = as.factor(mun_code),
    year = as.factor(year_num)
  )

# ---------------------------------------------------------------------------- #
# 0.1 Construir variables municipales pre-2023
# ---------------------------------------------------------------------------- #

wmean <- function(x, w) {
  ok <- !is.na(x) & !is.na(w)
  
  if (sum(ok) == 0) {
    return(NA_real_)
  }
  
  weighted.mean(x[ok], w = w[ok], na.rm = TRUE)
}

safe_wmean <- function(data, var) {
  if (!var %in% names(data)) {
    return(NA_real_)
  }
  
  x <- data[[var]]
  
  if (!is.numeric(x) & !is.integer(x) & !is.logical(x)) {
    return(NA_real_)
  }
  
  wmean(as.numeric(x), data$wt)
}

pre_period_all <- lapop %>%
  filter(year_num < 2023)

if (nrow(pre_period_all) == 0) {
  stop("No hay observaciones pre-2023 para calcular controles municipales.")
}

mun_pre_all_controls <- pre_period_all %>%
  group_by(mun_code) %>%
  group_modify(
    ~ tibble(
      mun_pre_all_mean_edad = safe_wmean(.x, "edad"),
      mun_pre_all_share_hombre = safe_wmean(.x, "hombre"),
      mun_pre_all_share_rural = safe_wmean(.x, "rural"),
      mun_pre_all_share_desempleado = safe_wmean(.x, "desempleado"),
      mun_pre_all_share_en_pareja = safe_wmean(.x, "en_pareja"),
      mun_pre_all_mean_educ = safe_wmean(.x, "anios_educ"),
      mun_pre_all_mean_izq_der = safe_wmean(.x, "izq_der"),
      mun_pre_all_share_interes_pol_mucho = safe_wmean(.x, "interes_pol_mucho"),
      mun_pre_all_share_voto_blanco_nulo = safe_wmean(.x, "voto_blanco_nulo")
    )
  ) %>%
  ungroup()

lapop <- lapop %>%
  left_join(mun_pre_all_controls, by = "mun_code")

# ---------------------------------------------------------------------------- #
# 1. Leer modelos Logit guardados
# ---------------------------------------------------------------------------- #

models_logit_post_x_no_controls <- readRDS(
  "Output/models/models_logit_post_x_no_controls.rds"
)

models_logit_post_x_with_controls <- readRDS(
  "Output/models/models_logit_post_x_with_controls.rds"
)

models_logit_triple_no_controls <- readRDS(
  "Output/models/models_logit_triple_no_controls.rds"
)

models_logit_triple_with_controls <- readRDS(
  "Output/models/models_logit_triple_with_controls.rds"
)

# ---------------------------------------------------------------------------- #
# 2. Función para detectar coeficientes grandes
# ---------------------------------------------------------------------------- #

diagnose_big_logit_coefs <- function(models,
                                     model_family,
                                     controls_label,
                                     threshold_coef = 5,
                                     threshold_or = 100) {
  
  imap_dfr(models, function(model, model_name) {
    
    b <- coef(model)
    
    if (length(b) == 0) {
      return(
        tibble(
          model_family = model_family,
          controls = controls_label,
          model = model_name,
          term = NA_character_,
          coef_logit = NA_real_,
          odds_ratio = NA_real_,
          abs_coef = NA_real_,
          big_coef = NA,
          big_or = NA
        )
      )
    }
    
    tibble(
      model_family = model_family,
      controls = controls_label,
      model = model_name,
      term = names(b),
      coef_logit = as.numeric(b),
      odds_ratio = exp(as.numeric(b)),
      abs_coef = abs(as.numeric(b)),
      big_coef = abs_coef > threshold_coef,
      big_or = odds_ratio > threshold_or | odds_ratio < 1 / threshold_or
    )
  }) %>%
    arrange(desc(abs_coef))
}

# ---------------------------------------------------------------------------- #
# 3. Armar tabla general de coeficientes Logit
# ---------------------------------------------------------------------------- #

big_logit_all <- bind_rows(
  diagnose_big_logit_coefs(
    models_logit_post_x_no_controls,
    model_family = "post_x",
    controls_label = "No"
  ),
  diagnose_big_logit_coefs(
    models_logit_post_x_with_controls,
    model_family = "post_x",
    controls_label = "Yes"
  ),
  
  diagnose_big_logit_coefs(
    models_logit_triple_no_controls,
    model_family = "triple",
    controls_label = "No"
  ),
  diagnose_big_logit_coefs(
    models_logit_triple_with_controls,
    model_family = "triple",
    controls_label = "Yes"
  )
)

write.xlsx(
  big_logit_all,
  "Output/diagnostics/logit_all_coefficients_diagnostic.xlsx",
  overwrite = TRUE
)

big_logit_problematic <- big_logit_all %>%
  filter(big_coef | big_or)

write.xlsx(
  big_logit_problematic,
  "Output/diagnostics/logit_problematic_coefficients.xlsx",
  overwrite = TRUE
)

print(big_logit_problematic)

# ---------------------------------------------------------------------------- #
# 4. Diagnóstico de colinealidad
# ---------------------------------------------------------------------------- #

extract_collinearity <- function(models,
                                 model_family,
                                 controls_label) {
  
  imap_dfr(models, function(model, model_name) {
    
    collin_vars <- model$collin.var
    
    if (is.null(collin_vars) || length(collin_vars) == 0) {
      return(
        tibble(
          model_family = model_family,
          controls = controls_label,
          model = model_name,
          collinear_var = NA_character_
        )
      )
    }
    
    tibble(
      model_family = model_family,
      controls = controls_label,
      model = model_name,
      collinear_var = collin_vars
    )
  })
}

collinearity_all <- bind_rows(
  extract_collinearity(
    models_logit_post_x_no_controls,
    "post_x",
    "No"
  ),
  extract_collinearity(
    models_logit_post_x_with_controls,
    "post_x",
    "Yes"
  ),
  extract_collinearity(
    models_logit_triple_no_controls,
    "triple",
    "No"
  ),
  extract_collinearity(
    models_logit_triple_with_controls,
    "triple",
    "Yes"
  )
)

write.xlsx(
  collinearity_all,
  "Output/diagnostics/logit_collinearity_diagnostic.xlsx",
  overwrite = TRUE
)

# ---------------------------------------------------------------------------- #
# 5. Diagnóstico general de separación por municipio-año
# ---------------------------------------------------------------------------- #

separation_by_mun_year <- lapop %>%
  group_by(mun_code, year) %>%
  summarise(
    n = n(),
    y_min = min(intencion_migrar, na.rm = TRUE),
    y_max = max(intencion_migrar, na.rm = TRUE),
    mean_y = weighted.mean(intencion_migrar, wt, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    all_zero = y_max == 0,
    all_one = y_min == 1,
    no_variation_y = y_min == y_max
  )

separation_summary <- separation_by_mun_year %>%
  summarise(
    n_cells = n(),
    cells_no_variation_y = sum(no_variation_y, na.rm = TRUE),
    share_cells_no_variation_y = mean(no_variation_y, na.rm = TRUE),
    cells_all_zero = sum(all_zero, na.rm = TRUE),
    cells_all_one = sum(all_one, na.rm = TRUE)
  )

write.xlsx(
  separation_by_mun_year,
  "Output/diagnostics/separation_by_mun_year.xlsx",
  overwrite = TRUE
)

write.xlsx(
  separation_summary,
  "Output/diagnostics/separation_summary.xlsx",
  overwrite = TRUE
)

print(separation_summary)

# ---------------------------------------------------------------------------- #
# 6. Diagnóstico por año
# ---------------------------------------------------------------------------- #

diagnostic_by_year <- lapop %>%
  group_by(year) %>%
  summarise(
    n = n(),
    mean_y = weighted.mean(intencion_migrar, wt, na.rm = TRUE),
    share_y_1 = mean(intencion_migrar == 1, na.rm = TRUE),
    share_y_0 = mean(intencion_migrar == 0, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(year)

write.xlsx(
  diagnostic_by_year,
  "Output/diagnostics/diagnostic_by_year.xlsx",
  overwrite = TRUE
)

print(diagnostic_by_year)

# ---------------------------------------------------------------------------- #
# 7. Extraer variables X problemáticas
# ---------------------------------------------------------------------------- #

problem_x <- big_logit_problematic %>%
  mutate(
    x = str_extract(term, "mun_pre_all_[a-zA-Z0-9_]+")
  ) %>%
  filter(!is.na(x)) %>%
  distinct(x) %>%
  pull(x)

print(problem_x)

# ---------------------------------------------------------------------------- #
# 8. Diagnóstico de escala para X, shares e interacciones
# ---------------------------------------------------------------------------- #

diagnose_one_x_scale <- function(data,
                                 x,
                                 exposure_1 = "share_1936_1955",
                                 exposure_2 = "share_1956_1978") {
  
  data %>%
    mutate(
      interaction_1 = .data[[x]] * .data[[exposure_1]],
      interaction_2 = .data[[x]] * .data[[exposure_2]],
      post_interaction_1 = post * .data[[x]] * .data[[exposure_1]],
      post_interaction_2 = post * .data[[x]] * .data[[exposure_2]]
    ) %>%
    summarise(
      variable = x,
      
      x_n = sum(!is.na(.data[[x]])),
      x_mean = mean(.data[[x]], na.rm = TRUE),
      x_sd = sd(.data[[x]], na.rm = TRUE),
      x_min = min(.data[[x]], na.rm = TRUE),
      x_p1 = quantile(.data[[x]], 0.01, na.rm = TRUE),
      x_p5 = quantile(.data[[x]], 0.05, na.rm = TRUE),
      x_p50 = quantile(.data[[x]], 0.50, na.rm = TRUE),
      x_p95 = quantile(.data[[x]], 0.95, na.rm = TRUE),
      x_p99 = quantile(.data[[x]], 0.99, na.rm = TRUE),
      x_max = max(.data[[x]], na.rm = TRUE),
      
      share1_mean = mean(.data[[exposure_1]], na.rm = TRUE),
      share1_sd = sd(.data[[exposure_1]], na.rm = TRUE),
      share1_min = min(.data[[exposure_1]], na.rm = TRUE),
      share1_p99 = quantile(.data[[exposure_1]], 0.99, na.rm = TRUE),
      share1_max = max(.data[[exposure_1]], na.rm = TRUE),
      
      share2_mean = mean(.data[[exposure_2]], na.rm = TRUE),
      share2_sd = sd(.data[[exposure_2]], na.rm = TRUE),
      share2_min = min(.data[[exposure_2]], na.rm = TRUE),
      share2_p99 = quantile(.data[[exposure_2]], 0.99, na.rm = TRUE),
      share2_max = max(.data[[exposure_2]], na.rm = TRUE),
      
      interaction1_sd = sd(interaction_1, na.rm = TRUE),
      interaction1_min = min(interaction_1, na.rm = TRUE),
      interaction1_p99 = quantile(interaction_1, 0.99, na.rm = TRUE),
      interaction1_max = max(interaction_1, na.rm = TRUE),
      
      interaction2_sd = sd(interaction_2, na.rm = TRUE),
      interaction2_min = min(interaction_2, na.rm = TRUE),
      interaction2_p99 = quantile(interaction_2, 0.99, na.rm = TRUE),
      interaction2_max = max(interaction_2, na.rm = TRUE),
      
      post_interaction1_sd = sd(post_interaction_1, na.rm = TRUE),
      post_interaction1_min = min(post_interaction_1, na.rm = TRUE),
      post_interaction1_p99 = quantile(post_interaction_1, 0.99, na.rm = TRUE),
      post_interaction1_max = max(post_interaction_1, na.rm = TRUE),
      
      post_interaction2_sd = sd(post_interaction_2, na.rm = TRUE),
      post_interaction2_min = min(post_interaction_2, na.rm = TRUE),
      post_interaction2_p99 = quantile(post_interaction_2, 0.99, na.rm = TRUE),
      post_interaction2_max = max(post_interaction_2, na.rm = TRUE)
    )
}

if (length(problem_x) > 0) {
  
  scale_diagnostics_problem_x <- map_dfr(
    problem_x,
    ~ diagnose_one_x_scale(
      data = lapop,
      x = .x
    )
  )
  
} else {
  
  scale_diagnostics_problem_x <- tibble()
}

write.xlsx(
  scale_diagnostics_problem_x,
  "Output/diagnostics/scale_diagnostics_problem_x.xlsx",
  overwrite = TRUE
)

print(scale_diagnostics_problem_x)

# ---------------------------------------------------------------------------- #
# 9. Guardar resumen en texto
# ---------------------------------------------------------------------------- #

sink("Output/diagnostics/logit_diagnostic_summary.txt")

cat("Diagnóstico de coeficientes Logit grandes\n")
cat("========================================\n\n")

cat("1. Coeficientes problemáticos\n")
cat("-----------------------------\n")
print(big_logit_problematic)

cat("\n\n2. Resumen de separación por municipio-año\n")
cat("------------------------------------------\n")
print(separation_summary)

cat("\n\n3. Diagnóstico por año\n")
cat("----------------------\n")
print(diagnostic_by_year)

cat("\n\n4. Variables eliminadas por colinealidad\n")
cat("----------------------------------------\n")
print(collinearity_all %>% filter(!is.na(collinear_var)))

cat("\n\n5. Variables X problemáticas detectadas\n")
cat("---------------------------------------\n")
print(problem_x)

cat("\n\n6. Diagnóstico de escala de X problemáticas\n")
cat("------------------------------------------\n")
print(scale_diagnostics_problem_x)

sink()

cat("\nDiagnóstico terminado correctamente.\n")
cat("Resultados guardados en: Output/diagnostics/\n")