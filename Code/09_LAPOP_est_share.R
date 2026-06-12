# ---------------------------------------------------------------------------- #
#   LAPOP: interacciones entre share español y características municipales pre
# ---------------------------------------------------------------------------- #
#
# Objetivo:
#   Estimar si la asociación entre exposición histórica española e intención
#   de migrar varía según características municipales pre-2023.
#
# Modelo sin controles:
#
#   intencion_migrar ~
#     share_1936_1955 +
#     share_1956_1978 +
#     X +
#     X:share_1936_1955 +
#     X:share_1956_1978
#     | year
#
# Modelo con controles:
#
#   intencion_migrar ~
#     share_1936_1955 +
#     share_1956_1978 +
#     X +
#     X:share_1936_1955 +
#     X:share_1956_1978 +
#     edad + hombre +
#     otras variables municipales pre-2023
#     | year
#
# Coeficientes de interés:
#   X:share_1936_1955
#   X:share_1956_1978
#
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
# 4. Definir variables que se interactúan con share español
# ---------------------------------------------------------------------------- #

interaction_vars_core <- c(
  "mun_pre_all_mean_edad",
  "mun_pre_all_share_hombre",
  "mun_pre_all_share_rural",
  "mun_pre_all_share_desempleado",
  "mun_pre_all_share_en_pareja",
  "mun_pre_all_mean_educ"
)

interaction_vars_politics <- c(
  "mun_pre_all_mean_izq_der",
  "mun_pre_all_share_interes_pol_mucho",
  "mun_pre_all_share_voto_blanco_nulo"
)

interaction_vars <- unique(c(
  interaction_vars_core,
  interaction_vars_politics
))

interaction_vars <- interaction_vars[
  interaction_vars %in% names(lapop) &
    map_lgl(interaction_vars, ~ any(!is.na(lapop[[.x]])))
]

cat("Variables usadas para interacciones con share español:\n")
print(interaction_vars)

# ---------------------------------------------------------------------------- #
# 5. Función para correr modelos X x share
# ---------------------------------------------------------------------------- #
#
# with_controls = FALSE:
#   share + X + X:share | year
#
# with_controls = TRUE:
#   share + X + X:share + otras variables pre + edad + hombre | year
#
# Nota:
#   No usamos FE de municipio porque share y X son variables fijas por municipio.
#
# ---------------------------------------------------------------------------- #

run_share_interaction_lpm <- function(x,
                                      with_controls = FALSE,
                                      data = lapop,
                                      exposure_1 = "share_1936_1955",
                                      exposure_2 = "share_1956_1978",
                                      controls_individual = c("edad", "hombre"),
                                      mun_pre_controls = interaction_vars) {
  
  rhs_interaction <- paste0(
    exposure_1,
    " + ",
    exposure_2,
    " + ",
    x,
    " + ",
    x, ":", exposure_1,
    " + ",
    x, ":", exposure_2
  )
  
  if (with_controls) {
    
    other_mun_pre_vars <- setdiff(mun_pre_controls, x)
    
    rhs_other_mun_pre <- paste(
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
      rhs_interaction,
      rhs_other_mun_pre,
      rhs_individual
    )
    
  } else {
    
    rhs <- rhs_interaction
  }
  
  fml <- as.formula(paste0(
    "intencion_migrar ~ ",
    rhs,
    " | year"
  ))
  
  cat("\nEstimando interacción share español para:", x, "\n")
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
# 6. Estimar modelos sin controles y con controles
# ---------------------------------------------------------------------------- #

models_share_interaction_no_controls <- map(
  interaction_vars,
  ~ run_share_interaction_lpm(
    x = .x,
    with_controls = FALSE
  )
)

names(models_share_interaction_no_controls) <- interaction_vars

models_share_interaction_with_controls <- map(
  interaction_vars,
  ~ run_share_interaction_lpm(
    x = .x,
    with_controls = TRUE
  )
)

names(models_share_interaction_with_controls) <- interaction_vars

# ---------------------------------------------------------------------------- #
# 7. Extraer solo coeficientes de interacción X x share
# ---------------------------------------------------------------------------- #

extract_share_interaction_coefs <- function(model, x, controls_label) {
  
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
      str_detect(term, fixed(x)) &
        str_detect(term, "share_1936_1955") &
        !str_detect(term, "post")
    )
  
  term_1956 <- ct %>%
    filter(
      str_detect(term, fixed(x)) &
        str_detect(term, "share_1956_1978") &
        !str_detect(term, "post")
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

share_interaction_table_no_controls <- map_dfr(
  names(models_share_interaction_no_controls),
  function(x) {
    extract_share_interaction_coefs(
      model = models_share_interaction_no_controls[[x]],
      x = x,
      controls_label = "No"
    )
  }
)

share_interaction_table_with_controls <- map_dfr(
  names(models_share_interaction_with_controls),
  function(x) {
    extract_share_interaction_coefs(
      model = models_share_interaction_with_controls[[x]],
      x = x,
      controls_label = "Yes"
    )
  }
)

share_interaction_table <- bind_rows(
  share_interaction_table_no_controls,
  share_interaction_table_with_controls
) %>%
  arrange(variable, controls)

print(share_interaction_table)

# ---------------------------------------------------------------------------- #
# 8. Guardar tabla en Excel
# ---------------------------------------------------------------------------- #

write.xlsx(
  share_interaction_table,
  "Output/share_interactions_key_coefficients.xlsx",
  overwrite = TRUE
)

# ---------------------------------------------------------------------------- #
# 9. Guardar tabla en LaTeX simple para Overleaf
# ---------------------------------------------------------------------------- #

share_interaction_table_latex <- share_interaction_table %>%
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
  "\\caption{Interactions between Spanish exposure and pre-2023 municipal characteristics}",
  "\\label{tab:share_interactions_key}",
  "\\small",
  "\\begin{tabular}{llccr}",
  "\\hline",
  "Variable & Controls & $X \\times Share_{1936-1955}$ & $X \\times Share_{1956-1978}$ & N \\\\",
  "\\hline"
)

for (i in seq_len(nrow(share_interaction_table_latex))) {
  latex_lines <- c(
    latex_lines,
    paste0(
      share_interaction_table_latex$variable[i],
      " & ",
      share_interaction_table_latex$controls[i],
      " & ",
      share_interaction_table_latex$coef_1936_1955[i],
      " & ",
      share_interaction_table_latex$coef_1956_1978[i],
      " & ",
      share_interaction_table_latex$nobs[i],
      " \\\\"
    )
  )
}

latex_lines <- c(
  latex_lines,
  "\\hline",
  "\\multicolumn{5}{l}{\\footnotesize Notes: The dependent variable is migration intention.} \\\\",
  "\\multicolumn{5}{l}{\\footnotesize Each row reports a separate interaction specification.} \\\\",
  "\\multicolumn{5}{l}{\\footnotesize The reported coefficients correspond to $X \\times Share_{1936-1955}$ and $X \\times Share_{1956-1978}$.} \\\\",
  "\\multicolumn{5}{l}{\\footnotesize All models include year fixed effects. Municipality fixed effects are not included because $X$ and Spanish exposure are time-invariant at the municipality level.} \\\\",
  "\\multicolumn{5}{l}{\\footnotesize In rows with Controls = Yes, controls include age, male, and the remaining pre-2023 municipal characteristics.} \\\\",
  "\\multicolumn{5}{l}{\\footnotesize The remaining pre-2023 municipal characteristics exclude the heterogeneity variable $X$ used in that row.} \\\\",
  "\\multicolumn{5}{l}{\\footnotesize Survey weights are used. Standard errors are clustered at the municipality level. Standard errors are in parentheses.} \\\\",
  "\\multicolumn{5}{l}{\\footnotesize * p$<$0.10, ** p$<$0.05, *** p$<$0.01.} \\\\",
  "\\end{tabular}",
  "\\end{table}"
)

writeLines(
  latex_lines,
  "Output/tex/share_interactions_key_coefficients.tex"
)

cat("\nCódigo terminado correctamente.\n")
cat("Tabla Excel guardada en: Output/share_interactions_key_coefficients.xlsx\n")
cat("Tabla LaTeX guardada en: Output/tex/share_interactions_key_coefficients.tex\n")