# ---------------------------------------------------------------------------- #
#     LAPOP: intención de migrar y características individuales
# ---------------------------------------------------------------------------- #
#
# Objetivo:
#   Estimar la asociación entre características individuales e intención
#   de migrar, controlando por efectos fijos de año y municipio.
#
# Se estiman tres especificaciones:
#
#   1. Full:
#      edad + hombre + rural + desempleado + en_pareja +
#      secundaria_completa_o_mas + izq_der + interes_pol_mucho +
#      voto_blanco_nulo
#
#   2. No blank/null vote:
#      Full sin voto_blanco_nulo
#
#   3. No ideology:
#      Full sin izq_der
#
# Modelos:
#   LPM:
#     intencion_migrar ~ características individuales | year + mun_code
#
#   Logit:
#     intencion_migrar ~ características individuales | year + mun_code
#
# No se incluyen:
#   - post
#   - share_1936_1955
#   - share_1956_1978
#   - promedios municipales pre-2023
#
# La tabla reporta:
#   - LPM: coeficientes con errores estándar
#   - Logit: signo y significatividad
#   - N: observaciones LPM / observaciones Logit
#
# ---------------------------------------------------------------------------- #

library(dplyr)
library(readr)
library(stringr)
library(purrr)
library(tidyr)
library(fixest)
library(openxlsx)

# ---------------------------------------------------------------------------- #
# 0. Paths y carga
# ---------------------------------------------------------------------------- #

path_pili <- "C:\\Users\\pilih\\Documents\\Papers German\\Valerie\\Paper_nietos_arg"
setwd(path_pili)

dir.create("Output", showWarnings = FALSE)
dir.create("Output/tex", showWarnings = FALSE, recursive = TRUE)
dir.create("Output/models", showWarnings = FALSE, recursive = TRUE)

lapop <- read_csv("Data Out/lapop_data_merge.csv", show_col_types = FALSE)

# ---------------------------------------------------------------------------- #
# 1. Preparar base
# ---------------------------------------------------------------------------- #

lapop <- lapop %>%
  mutate(
    year_num = as.numeric(year),
    mun_code = as.factor(mun_code),
    year = as.factor(year_num)
  )

# ---------------------------------------------------------------------------- #
# 2. Funciones auxiliares
# ---------------------------------------------------------------------------- #

rhs_join <- function(...) {
  parts <- c(...)
  parts <- parts[!is.na(parts)]
  parts <- parts[str_trim(parts) != ""]
  
  if (length(parts) == 0) {
    return("")
  }
  
  paste(parts, collapse = " + ")
}

stars <- function(p) {
  case_when(
    is.na(p) ~ "",
    p < 0.01 ~ "***",
    p < 0.05 ~ "**",
    p < 0.10 ~ "*",
    TRUE ~ ""
  )
}

format_coef <- function(estimate, se, p) {
  if (is.na(estimate)) {
    return("")
  }
  
  paste0(
    sprintf("%.3f", estimate),
    stars(p),
    " (",
    sprintf("%.3f", se),
    ")"
  )
}

format_logit_sign <- function(estimate, p) {
  if (is.na(estimate)) {
    return("")
  }
  
  sign_char <- case_when(
    estimate > 0 ~ "+",
    estimate < 0 ~ "-",
    TRUE ~ "0"
  )
  
  paste0(
    sign_char,
    stars(p)
  )
}

latex_escape <- function(x) {
  x %>%
    as.character() %>%
    str_replace_all("_", "\\\\_") %>%
    str_replace_all("%", "\\\\%")
}

# ---------------------------------------------------------------------------- #
# 3. Etiquetas limpias para variables individuales
# ---------------------------------------------------------------------------- #

variable_labels <- c(
  "edad" = "Age",
  "hombre" = "Male",
  "rural" = "Rural",
  "desempleado" = "Unemployed",
  "en_pareja" = "Partnered",
  "secundaria_completa_o_mas" = "High School or more",
  "izq_der" = "Left-right ideology",
  "interes_pol_mucho" = "Very interested in politics",
  "voto_blanco_nulo" = "Blank/null vote"
)

label_variable <- function(x) {
  ifelse(
    x %in% names(variable_labels),
    variable_labels[x],
    x
  )
}

# ---------------------------------------------------------------------------- #
# 4. Definir características individuales por especificación
# ---------------------------------------------------------------------------- #

individual_specs <- list(
  full = c(
    "edad",
    "hombre",
    "rural",
    "desempleado",
    "en_pareja",
    "secundaria_completa_o_mas",
    "izq_der",
    "interes_pol_mucho",
    "voto_blanco_nulo"
  ),
  
  no_blank_null_vote = c(
    "edad",
    "hombre",
    "rural",
    "desempleado",
    "en_pareja",
    "secundaria_completa_o_mas",
    "izq_der",
    "interes_pol_mucho"
  ),
  
  no_ideology = c(
    "edad",
    "hombre",
    "rural",
    "desempleado",
    "en_pareja",
    "secundaria_completa_o_mas",
    "interes_pol_mucho",
    "voto_blanco_nulo"
  )
)

individual_specs <- map(
  individual_specs,
  ~ .x[
    .x %in% names(lapop) &
      map_lgl(.x, ~ any(!is.na(lapop[[.x]])))
  ]
)

cat("Variables individuales usadas por especificación:\n")
print(individual_specs)

if (any(map_int(individual_specs, length) == 0)) {
  stop("Alguna especificación quedó sin variables disponibles para estimar el modelo.")
}

all_individual_vars <- unique(unlist(individual_specs))

# ---------------------------------------------------------------------------- #
# 4.1 Diagnóstico de missing values en variables usadas
# ---------------------------------------------------------------------------- #

vars_model_all <- c(
  "intencion_migrar",
  "wt",
  "mun_code",
  "year",
  all_individual_vars
)

vars_model_all <- vars_model_all[
  vars_model_all %in% names(lapop)
]

missing_diagnostics <- tibble(
  variable = vars_model_all
) %>%
  mutate(
    n_total = nrow(lapop),
    n_missing = map_int(variable, ~ sum(is.na(lapop[[.x]]))),
    n_non_missing = n_total - n_missing,
    share_missing = n_missing / n_total
  ) %>%
  arrange(desc(n_missing))

print(missing_diagnostics)

complete_cases_by_spec <- imap_dfr(
  individual_specs,
  function(vars, spec_name) {
    
    vars_spec <- c(
      "intencion_migrar",
      "wt",
      "mun_code",
      "year",
      vars
    )
    
    vars_spec <- vars_spec[
      vars_spec %in% names(lapop)
    ]
    
    tibble(
      spec = spec_name,
      n_total = nrow(lapop),
      n_complete_model_vars = sum(complete.cases(lapop[, vars_spec])),
      n_lost_model_vars = n_total - n_complete_model_vars,
      share_lost_model_vars = n_lost_model_vars / n_total
    )
  }
)

print(complete_cases_by_spec)

write.xlsx(
  missing_diagnostics,
  "Output/individual_characteristics_missing_diagnostics.xlsx",
  overwrite = TRUE
)

write.xlsx(
  complete_cases_by_spec,
  "Output/individual_characteristics_complete_cases_by_spec.xlsx",
  overwrite = TRUE
)

# ---------------------------------------------------------------------------- #
# 5. Función para construir fórmula por especificación
# ---------------------------------------------------------------------------- #

make_individual_formula <- function(vars) {
  
  rhs_individual <- paste(
    vars,
    collapse = " + "
  )
  
  as.formula(paste0(
    "intencion_migrar ~ ",
    rhs_individual,
    " | year + mun_code"
  ))
}

# ---------------------------------------------------------------------------- #
# 6. Funciones para estimar LPM y Logit
# ---------------------------------------------------------------------------- #

run_individual_lpm <- function(vars, spec_name, data = lapop) {
  
  fml <- make_individual_formula(vars)
  
  cat("\nEstimando LPM con características individuales. Spec:", spec_name, "\n")
  cat(deparse(fml), "\n\n")
  
  feols(
    fml,
    data = data,
    weights = ~ wt,
    cluster = ~ mun_code
  )
}

run_individual_logit <- function(vars, spec_name, data = lapop) {
  
  fml <- make_individual_formula(vars)
  
  cat("\nEstimando Logit con características individuales. Spec:", spec_name, "\n")
  cat(deparse(fml), "\n\n")
  
  feglm(
    fml,
    data = data,
    family = binomial(link = "logit"),
    weights = ~ wt,
    cluster = ~ mun_code
  )
}

# ---------------------------------------------------------------------------- #
# 7. Estimar LPM y Logit por especificación
# ---------------------------------------------------------------------------- #

models_individual_lpm <- imap(
  individual_specs,
  ~ run_individual_lpm(
    vars = .x,
    spec_name = .y
  )
)

models_individual_logit <- imap(
  individual_specs,
  ~ run_individual_logit(
    vars = .x,
    spec_name = .y
  )
)

saveRDS(
  models_individual_lpm,
  "Output/models/models_individual_lpm_specs.rds"
)

saveRDS(
  models_individual_logit,
  "Output/models/models_individual_logit_specs.rds"
)

# ---------------------------------------------------------------------------- #
# 8. Función para extraer coeficientes
# ---------------------------------------------------------------------------- #

extract_model_coefs <- function(model,
                                vars,
                                spec_name,
                                model_type = c("lpm", "logit")) {
  
  model_type <- match.arg(model_type)
  
  ct <- as.data.frame(coeftable(model))
  ct$term <- rownames(ct)
  
  estimate_col <- names(ct)[str_detect(names(ct), "^Estimate$")]
  se_col <- names(ct)[str_detect(names(ct), "Std\\. Error")]
  p_col <- names(ct)[str_detect(names(ct), "^Pr\\(")]
  
  if (length(estimate_col) == 0) estimate_col <- names(ct)[1]
  if (length(se_col) == 0) se_col <- names(ct)[2]
  
  if (length(p_col) == 0) {
    ct$p_value <- NA_real_
  } else {
    ct$p_value <- ct[[p_col[1]]]
  }
  
  ct <- ct %>%
    mutate(
      estimate = .data[[estimate_col[1]]],
      se = .data[[se_col[1]]]
    ) %>%
    filter(term %in% vars)
  
  if (model_type == "lpm") {
    
    out <- ct %>%
      transmute(
        spec = spec_name,
        variable = term,
        lpm = pmap_chr(
          list(estimate, se, p_value),
          ~ format_coef(..1, ..2, ..3)
        )
      )
    
  } else {
    
    out <- ct %>%
      transmute(
        spec = spec_name,
        variable = term,
        logit_sign = pmap_chr(
          list(estimate, p_value),
          ~ format_logit_sign(..1, ..2)
        )
      )
  }
  
  out
}

# ---------------------------------------------------------------------------- #
# 9. Armar tabla combinada LPM + Logit por especificación
# ---------------------------------------------------------------------------- #

individual_lpm_table <- imap_dfr(
  models_individual_lpm,
  function(model, spec_name) {
    extract_model_coefs(
      model = model,
      vars = individual_specs[[spec_name]],
      spec_name = spec_name,
      model_type = "lpm"
    )
  }
)

individual_logit_table <- imap_dfr(
  models_individual_logit,
  function(model, spec_name) {
    extract_model_coefs(
      model = model,
      vars = individual_specs[[spec_name]],
      spec_name = spec_name,
      model_type = "logit"
    )
  }
)

individual_nobs_table <- tibble(
  spec = names(individual_specs),
  nobs_lpm = map_int(models_individual_lpm, nobs),
  nobs_logit = map_int(models_individual_logit, nobs)
)

individual_table <- individual_lpm_table %>%
  full_join(
    individual_logit_table,
    by = c("spec", "variable")
  ) %>%
  left_join(
    individual_nobs_table,
    by = "spec"
  ) %>%
  arrange(
    match(variable, all_individual_vars),
    match(spec, names(individual_specs))
  )

print(individual_table)

# ---------------------------------------------------------------------------- #
# 10. Guardar tabla en Excel
# ---------------------------------------------------------------------------- #

write.xlsx(
  individual_table,
  "Output/individual_characteristics_lpm_logit_specs.xlsx",
  overwrite = TRUE
)

# ---------------------------------------------------------------------------- #
# 11. Guardar tabla en LaTeX simple para Overleaf
# ---------------------------------------------------------------------------- #

spec_labels <- c(
  "full" = "Full",
  "no_blank_null_vote" = "No blank/null vote",
  "no_ideology" = "No ideology"
)

# Preparar tabla wide con LPM y Logit en columnas separadas

individual_table_wide <- individual_table %>%
  mutate(
    variable = factor(variable, levels = all_individual_vars),
    spec = factor(spec, levels = names(individual_specs))
  ) %>%
  select(variable, spec, lpm, logit_sign) %>%
  pivot_wider(
    names_from = spec,
    values_from = c(lpm, logit_sign),
    names_glue = "{spec}_{.value}"
  ) %>%
  arrange(variable)

# Orden deseado de columnas: para cada spec, LPM y Logit juntos

ordered_cols <- c(
  "variable",
  unlist(
    map(
      names(individual_specs),
      ~ c(
        paste0(.x, "_lpm"),
        paste0(.x, "_logit_sign")
      )
    )
  )
)

individual_table_wide <- individual_table_wide %>%
  select(all_of(ordered_cols))

individual_table_latex <- individual_table_wide %>%
  mutate(
    variable = as.character(variable),
    variable = label_variable(variable),
    variable = latex_escape(variable),
    across(
      -variable,
      ~ latex_escape(ifelse(is.na(.x), "", .x))
    )
  )

# N para cada especificación y modelo

nobs_row <- individual_nobs_table %>%
  mutate(
    nobs_lpm = as.character(nobs_lpm),
    nobs_logit = as.character(nobs_logit)
  )

nobs_values <- unlist(
  map(
    names(individual_specs),
    function(s) {
      row_s <- nobs_row %>%
        filter(spec == s)
      
      c(
        row_s$nobs_lpm,
        row_s$nobs_logit
      )
    }
  )
)

# Encabezados LaTeX

latex_lines <- c(
  "\\begin{table}[!htbp]",
  "\\centering",
  "\\caption{Individual characteristics and migration intention}",
  "\\label{tab:individual_characteristics_specs}",
  "\\small",
  paste0(
    "\\begin{tabular}{l",
    paste(rep("c", length(individual_specs) * 2), collapse = ""),
    "}"
  ),
  "\\hline",
  paste0(
    " & ",
    paste(
      paste0(
        "\\multicolumn{2}{c}{",
        spec_labels[names(individual_specs)],
        "}"
      ),
      collapse = " & "
    ),
    " \\\\"
  ),
  paste0(
    "Variable & ",
    paste(
      rep(c("LPM", "Logit"), times = length(individual_specs)),
      collapse = " & "
    ),
    " \\\\"
  ),
  "\\hline"
)

# Filas de coeficientes

for (i in seq_len(nrow(individual_table_latex))) {
  
  row_values <- individual_table_latex[i, ordered_cols[-1]] %>%
    as.character()
  
  latex_lines <- c(
    latex_lines,
    paste0(
      individual_table_latex$variable[i],
      " & ",
      paste(row_values, collapse = " & "),
      " \\\\"
    )
  )
}

# Fila de observaciones y notas

latex_lines <- c(
  latex_lines,
  "\\hline",
  paste0(
    "Observations & ",
    paste(nobs_values, collapse = " & "),
    " \\\\"
  ),
  "\\hline",
  paste0(
    "\\multicolumn{",
    length(individual_specs) * 2 + 1,
    "}{l}{\\footnotesize Notes: The dependent variable is migration intention.} \\\\"
  ),
  paste0(
    "\\multicolumn{",
    length(individual_specs) * 2 + 1,
    "}{l}{\\footnotesize All models include year and municipality fixed effects.} \\\\"
  ),
  paste0(
    "\\multicolumn{",
    length(individual_specs) * 2 + 1,
    "}{l}{\\footnotesize Standard errors are clustered at the municipality level.} \\\\"
  ),
  paste0(
    "\\multicolumn{",
    length(individual_specs) * 2 + 1,
    "}{l}{\\footnotesize LPM columns report coefficients with clustered standard errors in parentheses.} \\\\"
  ),
  paste0(
    "\\multicolumn{",
    length(individual_specs) * 2 + 1,
    "}{l}{\\footnotesize Logit columns report the sign and statistical significance of the corresponding coefficient.} \\\\"
  ),
  paste0(
    "\\multicolumn{",
    length(individual_specs) * 2 + 1,
    "}{l}{\\footnotesize * p$<$0.10, ** p$<$0.05, *** p$<$0.01.} \\\\"
  ),
  "\\end{tabular}",
  "\\end{table}"
)

writeLines(
  latex_lines,
  "Output/tex/individual_characteristics_lpm_logit_specs.tex"
)

# ---------------------------------------------------------------------------- #
# 12. Mensaje final
# ---------------------------------------------------------------------------- #

cat("\nCódigo terminado correctamente.\n")
cat("Modelos LPM guardados en: Output/models/models_individual_lpm_specs.rds\n")
cat("Modelos Logit guardados en: Output/models/models_individual_logit_specs.rds\n")
cat("Tabla Excel guardada en: Output/individual_characteristics_lpm_logit_specs.xlsx\n")
cat("Tabla LaTeX guardada en: Output/tex/individual_characteristics_lpm_logit_specs.tex\n")
cat("Diagnóstico de missing guardado en:\n")
cat("  Output/individual_characteristics_missing_diagnostics.xlsx\n")
cat("  Output/individual_characteristics_complete_cases_by_spec.xlsx\n")