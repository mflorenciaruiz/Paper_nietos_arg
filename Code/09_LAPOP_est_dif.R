# ---------------------------------------------------------------------------- #
#     LAPOP: LPM post x características municipales pre-2023
# ---------------------------------------------------------------------------- #
#
# Objetivo:
#   Estimar si el cambio en 2023 en intención de migrar es distinto según
#   características municipales pre-2023.
#
# Modelo sin controles:
#
#   intencion_migrar ~
#     X +
#     post:X
#     | year
#
# Modelo con controles:
#
#   intencion_migrar ~
#     X +
#     post:X +
#     edad + hombre +
#     otras variables municipales pre-2023
#     | year
#
# En la versión con controles, las otras variables municipales pre-2023
# entran en niveles, sin interactuarse con post.
#
# Coeficiente de interés:
#   post:X
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
# 4. Definir variables X
# ---------------------------------------------------------------------------- #

post_x_vars_core <- c(
  "mun_pre_all_mean_edad",
  "mun_pre_all_share_hombre",
  "mun_pre_all_share_rural",
  "mun_pre_all_share_desempleado",
  "mun_pre_all_share_en_pareja",
  "mun_pre_all_mean_educ"
)

post_x_vars_politics <- c(
  "mun_pre_all_mean_izq_der",
  "mun_pre_all_share_interes_pol_mucho",
  "mun_pre_all_share_voto_blanco_nulo"
)

post_x_vars <- unique(c(
  post_x_vars_core,
  post_x_vars_politics
))

post_x_vars <- post_x_vars[
  post_x_vars %in% names(lapop) &
    map_lgl(post_x_vars, ~ any(!is.na(lapop[[.x]])))
]

cat("Variables usadas para post x X:\n")
print(post_x_vars)

# ---------------------------------------------------------------------------- #
# 5. Función para correr modelos post x X
# ---------------------------------------------------------------------------- #
#
# with_controls = FALSE:
#   X + post:X | year
#
# with_controls = TRUE:
#   X + post:X + otras variables pre + edad + hombre | year
#
# Las otras variables pre-2023 entran en niveles, sin interactuar.
# No usamos FE de municipio para que esas variables se puedan estimar.
#
# ---------------------------------------------------------------------------- #

run_post_x_lpm <- function(x,
                           with_controls = FALSE,
                           data = lapop,
                           controls_individual = c("edad", "hombre"),
                           mun_pre_controls = post_x_vars) {
  
  rhs_main <- paste0(
    x,
    " + post:", x
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
      rhs_main,
      rhs_other_mun_pre,
      rhs_individual
    )
    
  } else {
    
    rhs <- rhs_main
  }
  
  fml <- as.formula(paste0(
    "intencion_migrar ~ ",
    rhs,
    " | year"
  ))
  
  cat("\nEstimando LPM post x X para:", x, "\n")
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

models_post_x_no_controls <- map(
  post_x_vars,
  ~ run_post_x_lpm(
    x = .x,
    with_controls = FALSE
  )
)

names(models_post_x_no_controls) <- post_x_vars

models_post_x_with_controls <- map(
  post_x_vars,
  ~ run_post_x_lpm(
    x = .x,
    with_controls = TRUE
  )
)

names(models_post_x_with_controls) <- post_x_vars

# ---------------------------------------------------------------------------- #
# 7. Extraer solo coeficientes post:X
# ---------------------------------------------------------------------------- #

extract_post_x_coefs <- function(model, x, controls_label) {
  
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
  
  term_post_x <- ct %>%
    filter(
      term == paste0("post:", x) |
        term == paste0(x, ":post")
    )
  
  tibble(
    variable = x,
    controls = controls_label,
    coef_post_x = if (nrow(term_post_x) > 0) {
      format_coef(term_post_x$estimate[1], term_post_x$se[1], term_post_x$p_value[1])
    } else {
      ""
    },
    nobs = nobs(model)
  )
}

post_x_table_no_controls <- map_dfr(
  names(models_post_x_no_controls),
  function(x) {
    extract_post_x_coefs(
      model = models_post_x_no_controls[[x]],
      x = x,
      controls_label = "No"
    )
  }
)

post_x_table_with_controls <- map_dfr(
  names(models_post_x_with_controls),
  function(x) {
    extract_post_x_coefs(
      model = models_post_x_with_controls[[x]],
      x = x,
      controls_label = "Yes"
    )
  }
)

post_x_table <- bind_rows(
  post_x_table_no_controls,
  post_x_table_with_controls
) %>%
  arrange(variable, controls)

print(post_x_table)

# ---------------------------------------------------------------------------- #
# 8. Guardar tabla en Excel
# ---------------------------------------------------------------------------- #

write.xlsx(
  post_x_table,
  "Output/post_x_key_coefficients.xlsx",
  overwrite = TRUE
)

# ---------------------------------------------------------------------------- #
# 9. Guardar tabla en LaTeX simple para Overleaf
# ---------------------------------------------------------------------------- #

post_x_table_latex <- post_x_table %>%
  mutate(
    variable = label_variable(variable),
    variable = latex_escape(variable),
    controls = latex_escape(controls),
    coef_post_x = latex_escape(coef_post_x)
  )

latex_lines <- c(
  "\\begin{table}[!htbp]",
  "\\centering",
  "\\caption{Post-2023 changes by pre-2023 municipal characteristics}",
  "\\label{tab:post_x_key}",
  "\\small",
  "\\begin{tabular}{lccr}",
  "\\hline",
  "Variable & Controls & $Post \\times X$ & N \\\\",
  "\\hline"
)

for (i in seq_len(nrow(post_x_table_latex))) {
  latex_lines <- c(
    latex_lines,
    paste0(
      post_x_table_latex$variable[i],
      " & ",
      post_x_table_latex$controls[i],
      " & ",
      post_x_table_latex$coef_post_x[i],
      " & ",
      post_x_table_latex$nobs[i],
      " \\\\"
    )
  )
}

latex_lines <- c(
  latex_lines,
  "\\hline",
  "\\multicolumn{4}{l}{\\footnotesize Notes: The dependent variable is migration intention.} \\\\",
  "\\multicolumn{4}{l}{\\footnotesize Each row reports a separate specification interacting $Post$ with one pre-2023 municipal characteristic $X$.} \\\\",
  "\\multicolumn{4}{l}{\\footnotesize All models include year fixed effects. Municipality fixed effects are not included because controls enter in levels.} \\\\",
  "\\multicolumn{4}{l}{\\footnotesize In rows with Controls = Yes, controls include age, male, and the remaining pre-2023 municipal characteristics in levels.} \\\\",
  "\\multicolumn{4}{l}{\\footnotesize The remaining pre-2023 municipal characteristics exclude the heterogeneity variable $X$ used in that row.} \\\\",
  "\\multicolumn{4}{l}{\\footnotesize Survey weights are used. Standard errors are clustered at the municipality level. Standard errors are in parentheses.} \\\\",
  "\\multicolumn{4}{l}{\\footnotesize * p$<$0.10, ** p$<$0.05, *** p$<$0.01.} \\\\",
  "\\end{tabular}",
  "\\end{table}"
)

writeLines(
  latex_lines,
  "Output/tex/post_x_key_coefficients.tex"
)

cat("\nCódigo terminado correctamente.\n")
cat("Tabla Excel guardada en: Output/post_x_key_coefficients.xlsx\n")
cat("Tabla LaTeX guardada en: Output/tex/post_x_key_coefficients.tex\n")