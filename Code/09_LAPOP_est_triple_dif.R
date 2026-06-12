# ---------------------------------------------------------------------------- #
#     LAPOP: triple diferencias por características municipales pre-2023
# ---------------------------------------------------------------------------- #

library(dplyr)
library(readr)
library(stringr)
library(purrr)
library(fixest)
library(openxlsx)

# ---------------------------------------------------------------------------- #
# 0. Paths y carga
# ---------------------------------------------------------------------------- #

path_pili <- "C:\\Users\\pilih\\Documents\\Papers German\\Valerie\\Paper_nietos_arg"
setwd(path_pili)

dir.create("Output", showWarnings = FALSE)
dir.create("Output/tex", showWarnings = FALSE)

lapop <- read_csv("Data Out/lapop_data_merge.csv", show_col_types = FALSE)

# ---------------------------------------------------------------------------- #
# 1. Preparar base
# ---------------------------------------------------------------------------- #

lapop <- lapop %>%
  mutate(
    year_num = as.numeric(year),
    post = if_else(year_num >= 2023, 1, 0)
  )

# ---------------------------------------------------------------------------- #
# 2. Funciones auxiliares
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

latex_escape <- function(x) {
  x %>%
    str_replace_all("_", "\\\\_") %>%
    str_replace_all("%", "\\\\%")
}

# ---------------------------------------------------------------------------- #
# Etiquetas limpias para variables en todas las tablas
# ---------------------------------------------------------------------------- #

variable_labels <- c(
  "mun_pre_all_mean_edad" = "Mean age",
  "mun_pre_all_share_hombre" = "Share male",
  "mun_pre_all_share_rural" = "Share rural",
  "mun_pre_all_share_desempleado" = "Share unemployed",
  "mun_pre_all_share_en_pareja" = "Share partnered",
  "mun_pre_all_mean_educ" = "Mean years of education",
  "mun_pre_all_mean_izq_der" = "Mean left-right ideology",
  "mun_pre_all_share_interes_pol_mucho" = "Share very interested in politics",
  "mun_pre_all_share_voto_blanco_nulo" = "Share blank/null vote"
)

label_variable <- function(x) {
  ifelse(
    x %in% names(variable_labels),
    variable_labels[x],
    x
  )
}

# ---------------------------------------------------------------------------- #
# 3. Construir variables municipales pre-2023
# ---------------------------------------------------------------------------- #

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
  left_join(mun_pre_all_controls, by = "mun_code") %>%
  mutate(
    mun_code = as.factor(mun_code),
    year = as.factor(year_num)
  )

# ---------------------------------------------------------------------------- #
# 4. Definir variables de heterogeneidad
# ---------------------------------------------------------------------------- #

triple_vars_core <- c(
  "mun_pre_all_mean_edad",
  "mun_pre_all_share_hombre",
  "mun_pre_all_share_rural",
  "mun_pre_all_share_desempleado",
  "mun_pre_all_share_en_pareja",
  "mun_pre_all_mean_educ"
)

triple_vars_politics <- c(
  "mun_pre_all_mean_izq_der",
  "mun_pre_all_share_interes_pol_mucho",
  "mun_pre_all_share_voto_blanco_nulo"
)

triple_vars <- unique(c(
  triple_vars_core,
  triple_vars_politics
))

triple_vars <- triple_vars[
  triple_vars %in% names(lapop) &
    map_lgl(triple_vars, ~ any(!is.na(lapop[[.x]])))
]

cat("Variables usadas para triples:\n")
print(triple_vars)

# ---------------------------------------------------------------------------- #
# 5. Función para correr triple diferencias
# ---------------------------------------------------------------------------- #
#
# with_controls = FALSE:
#   post:share + post:x + post:x:share | year + mun_code
#
# with_controls = TRUE:
#   post:share + post:x + post:x:share + post:otras_variables_pre
#   + edad + hombre | year + mun_code
#
# Los controles adicionales no se muestran en la tabla final.
#
# ---------------------------------------------------------------------------- #

run_triple_lpm <- function(x,
                           with_controls = FALSE,
                           data = lapop,
                           exposure_1 = "share_1936_1955",
                           exposure_2 = "share_1956_1978",
                           controls_individual = c("edad", "hombre"),
                           mun_pre_controls = triple_vars) {
  
  rhs_triple <- paste0(
    "post:", exposure_1,
    " + post:", exposure_2,
    " + post:", x,
    " + post:", x, ":", exposure_1,
    " + post:", x, ":", exposure_2
  )
  
  if (with_controls) {
    
    other_mun_pre_vars <- setdiff(mun_pre_controls, x)
    
    rhs_other_mun_pre <- paste0(
      "post:",
      other_mun_pre_vars,
      collapse = " + "
    )
    
    controls_individual <- controls_individual[
      controls_individual %in% names(data)
    ]
    
    rhs_individual <- paste(
      controls_individual,
      collapse = " + "
    )
    
    rhs <- rhs_join(
      rhs_triple,
      rhs_other_mun_pre,
      rhs_individual
    )
    
  } else {
    
    rhs <- rhs_triple
  }
  
  fml <- as.formula(paste0(
    "intencion_migrar ~ ",
    rhs,
    " | year + mun_code"
  ))
  
  cat("\nEstimando modelo triple para:", x, "\n")
  cat("Controles adicionales:", ifelse(with_controls, "Sí", "No"), "\n")
  cat(deparse(fml), "\n\n")
  
  feols(
    fml,
    data = data,
    weights = ~ wt,
    cluster = ~ mun_code
  )
}

# ---------------------------------------------------------------------------- #
# 6. Estimar triples sin controles y con controles
# ---------------------------------------------------------------------------- #

models_triple_no_controls <- map(
  triple_vars,
  ~ run_triple_lpm(
    x = .x,
    with_controls = FALSE
  )
)

names(models_triple_no_controls) <- triple_vars

models_triple_with_controls <- map(
  triple_vars,
  ~ run_triple_lpm(
    x = .x,
    with_controls = TRUE
  )
)

names(models_triple_with_controls) <- triple_vars

# ---------------------------------------------------------------------------- #
# 6.1 Guardar modelos LPM triple
# ---------------------------------------------------------------------------- #

dir.create("Output/models", showWarnings = FALSE, recursive = TRUE)

saveRDS(
  models_triple_no_controls,
  "Output/models/models_triple_no_controls.rds"
)

saveRDS(
  models_triple_with_controls,
  "Output/models/models_triple_with_controls.rds"
)

# ---------------------------------------------------------------------------- #
# 7. Extraer solo coeficientes de triple interacción
# ---------------------------------------------------------------------------- #

extract_triple_coefs <- function(model, x, controls_label) {
  
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
    )
  
  term_1936 <- ct %>%
    filter(
      str_detect(term, "post") &
        str_detect(term, fixed(x)) &
        str_detect(term, "share_1936_1955")
    )
  
  term_1956 <- ct %>%
    filter(
      str_detect(term, "post") &
        str_detect(term, fixed(x)) &
        str_detect(term, "share_1956_1978")
    )
  
  tibble(
    variable = x,
    controls = controls_label,
    coef_1936_1955 = if (nrow(term_1936) > 0) {
      format_coef(term_1936$estimate[1], term_1936$se[1], term_1936$p_value[1])
    } else {
      ""
    },
    coef_1956_1978 = if (nrow(term_1956) > 0) {
      format_coef(term_1956$estimate[1], term_1956$se[1], term_1956$p_value[1])
    } else {
      ""
    },
    nobs = nobs(model)
  )
}

triple_table_no_controls <- map_dfr(
  names(models_triple_no_controls),
  function(x) {
    extract_triple_coefs(
      model = models_triple_no_controls[[x]],
      x = x,
      controls_label = "No"
    )
  }
)

triple_table_with_controls <- map_dfr(
  names(models_triple_with_controls),
  function(x) {
    extract_triple_coefs(
      model = models_triple_with_controls[[x]],
      x = x,
      controls_label = "Yes"
    )
  }
)

triple_table <- bind_rows(
  triple_table_no_controls,
  triple_table_with_controls
) %>%
  arrange(variable, controls)

print(triple_table)

# ---------------------------------------------------------------------------- #
# 8. Guardar tabla en Excel
# ---------------------------------------------------------------------------- #

write.xlsx(
  triple_table,
  "Output/triple_differences_key_coefficients.xlsx",
  overwrite = TRUE
)

# ---------------------------------------------------------------------------- #
# 9. Guardar tabla en LaTeX simple para Overleaf
# ---------------------------------------------------------------------------- #

triple_table_latex <- triple_table %>%
  mutate(
    variable = label_variable(variable),
    variable = latex_escape(variable),
    controls = latex_escape(controls),
    coef_1936_1955 = latex_escape(coef_1936_1955),
    coef_1956_1978 = latex_escape(coef_1956_1978)
  )

latex_lines <- c(
  "\\begin{table}[!htbp]",
  "\\centering",
  "\\caption{Triple differences by pre-2023 municipal characteristics}",
  "\\label{tab:triple_differences_key}",
  "\\small",
  "\\begin{tabular}{llccr}",
  "\\hline",
  "Variable & Controls & $Post \\times X \\times Share_{1936-1955}$ & $Post \\times X \\times Share_{1956-1978}$ & N \\\\",
  "\\hline"
)

for (i in seq_len(nrow(triple_table_latex))) {
  latex_lines <- c(
    latex_lines,
    paste0(
      triple_table_latex$variable[i],
      " & ",
      triple_table_latex$controls[i],
      " & ",
      triple_table_latex$coef_1936_1955[i],
      " & ",
      triple_table_latex$coef_1956_1978[i],
      " & ",
      triple_table_latex$nobs[i],
      " \\\\"
    )
  )
}

latex_lines <- c(
  latex_lines,
  "\\hline",
  "\\multicolumn{5}{l}{\\footnotesize Notes: The dependent variable is migration intention.} \\\\",
  "\\multicolumn{5}{l}{\\footnotesize Each row reports a separate triple-difference specification.} \\\\",
  "\\multicolumn{5}{l}{\\footnotesize The reported coefficients correspond to $Post \\times X \\times Share_{1936-1955}$ and $Post \\times X \\times Share_{1956-1978}$.} \\\\",
  "\\multicolumn{5}{l}{\\footnotesize All models include year and municipality fixed effects.} \\\\",
  "\\multicolumn{5}{l}{\\footnotesize In rows with Controls = Yes, controls include age, male, and $Post$ interacted with the remaining pre-2023 municipal characteristics.} \\\\",
  "\\multicolumn{5}{l}{\\footnotesize The remaining pre-2023 municipal characteristics exclude the heterogeneity variable $X$ used in that row.} \\\\",
  "\\multicolumn{5}{l}{\\footnotesize Survey weights are used. Standard errors are clustered at the municipality level. Standard errors are in parentheses.} \\\\",
  "\\multicolumn{5}{l}{\\footnotesize * p$<$0.10, ** p$<$0.05, *** p$<$0.01.} \\\\",
  "\\end{tabular}",
  "\\end{table}"
)

writeLines(
  latex_lines,
  "Output/tex/triple_differences_key_coefficients.tex"
)

cat("\nCódigo terminado correctamente.\n")
cat("Tabla Excel guardada en: Output/triple_differences_key_coefficients.xlsx\n")
cat("Tabla LaTeX guardada en: Output/tex/triple_differences_key_coefficients.tex\n")