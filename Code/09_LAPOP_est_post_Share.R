# ---------------------------------------------------------------------------- #
#     LAPOP: post x share español sobre intención de migrar
# ---------------------------------------------------------------------------- #
#
# Objetivo:
#   Estimar si el cambio post-2023 en intención de migrar es distinto según
#   exposición histórica española del municipio.
#
# Modelo LPM:
#
#   intencion_migrar ~
#     post:share_1936_1955 +
#     post:share_1956_1978
#     | year + mun_code
#
# Modelo Logit:
#
#   intencion_migrar ~
#     post:share_1936_1955 +
#     post:share_1956_1978
#     | year + mun_code
#
# También se estima una versión con controles individuales:
#   edad + hombre
#
# Notas:
#   - post en niveles queda absorbido por efectos fijos de año.
#   - share_1936_1955 y share_1956_1978 en niveles quedan absorbidos por
#     efectos fijos de municipio.
#   - Los coeficientes de interés son:
#       post:share_1936_1955
#       post:share_1956_1978
#
# La tabla reporta:
#   - LPM: coeficiente con error estándar clusterizado
#   - Logit: signo y significatividad del coeficiente Logit
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
dir.create("Output/tex", showWarnings = FALSE, recursive = TRUE)
dir.create("Output/models", showWarnings = FALSE, recursive = TRUE)

lapop <- read_csv("Data Out/lapop_data_merge.csv", show_col_types = FALSE)

# ---------------------------------------------------------------------------- #
# 1. Preparar base
# ---------------------------------------------------------------------------- #

lapop <- lapop %>%
  mutate(
    year_num = as.numeric(year),
    post = if_else(year_num >= 2023, 1, 0),
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
# 3. Etiquetas
# ---------------------------------------------------------------------------- #

term_labels <- c(
  "post:share_1936_1955" = "$Post \\times Share_{1936-1955}$",
  "share_1936_1955:post" = "$Post \\times Share_{1936-1955}$",
  "post:share_1956_1978" = "$Post \\times Share_{1956-1978}$",
  "share_1956_1978:post" = "$Post \\times Share_{1956-1978}$"
)

label_term <- function(x) {
  ifelse(
    x %in% names(term_labels),
    term_labels[x],
    x
  )
}

# ---------------------------------------------------------------------------- #
# 4. Chequeos básicos
# ---------------------------------------------------------------------------- #

required_vars <- c(
  "intencion_migrar",
  "post",
  "share_1936_1955",
  "share_1956_1978",
  "year",
  "mun_code",
  "wt"
)

missing_vars <- setdiff(required_vars, names(lapop))

if (length(missing_vars) > 0) {
  stop(
    paste0(
      "Faltan estas variables en lapop_data_merge.csv: ",
      paste(missing_vars, collapse = ", ")
    )
  )
}

cat("Resumen de shares españoles:\n")
print(
  lapop %>%
    summarise(
      n_share_1936_1955 = sum(!is.na(share_1936_1955)),
      mean_share_1936_1955 = mean(share_1936_1955, na.rm = TRUE),
      sd_share_1936_1955 = sd(share_1936_1955, na.rm = TRUE),
      n_share_1956_1978 = sum(!is.na(share_1956_1978)),
      mean_share_1956_1978 = mean(share_1956_1978, na.rm = TRUE),
      sd_share_1956_1978 = sd(share_1956_1978, na.rm = TRUE)
    )
)

# ---------------------------------------------------------------------------- #
# 5. Definir especificaciones
# ---------------------------------------------------------------------------- #

post_share_terms <- c(
  "post:share_1936_1955",
  "post:share_1956_1978"
)

controls_individual <- c(
  "edad",
  "hombre"
)

controls_individual <- controls_individual[
  controls_individual %in% names(lapop)
]

post_share_specs <- list(
  no_controls = list(
    controls_label = "No",
    controls = character(0)
  ),
  with_controls = list(
    controls_label = "Yes",
    controls = controls_individual
  )
)

make_post_share_formula <- function(controls = character(0)) {
  
  rhs_main <- paste(
    post_share_terms,
    collapse = " + "
  )
  
  rhs_controls <- paste(
    controls,
    collapse = " + "
  )
  
  rhs <- rhs_join(
    rhs_main,
    rhs_controls
  )
  
  as.formula(paste0(
    "intencion_migrar ~ ",
    rhs,
    " | year + mun_code"
  ))
}

# ---------------------------------------------------------------------------- #
# 6. Estimar LPM y Logit
# ---------------------------------------------------------------------------- #

run_lpm <- function(spec_name, spec) {
  
  fml <- make_post_share_formula(
    controls = spec$controls
  )
  
  cat("\nEstimando LPM post x share. Spec:", spec_name, "\n")
  cat("Controles:", spec$controls_label, "\n")
  cat(deparse(fml), "\n\n")
  
  feols(
    fml,
    data = lapop,
    weights = ~ wt,
    cluster = ~ mun_code
  )
}

run_logit <- function(spec_name, spec) {
  
  fml <- make_post_share_formula(
    controls = spec$controls
  )
  
  cat("\nEstimando Logit post x share. Spec:", spec_name, "\n")
  cat("Controles:", spec$controls_label, "\n")
  cat(deparse(fml), "\n\n")
  
  feglm(
    fml,
    data = lapop,
    family = binomial(link = "logit"),
    weights = ~ wt,
    cluster = ~ mun_code
  )
}

models_post_share_lpm <- imap(
  post_share_specs,
  ~ run_lpm(
    spec_name = .y,
    spec = .x
  )
)

models_post_share_logit <- imap(
  post_share_specs,
  ~ run_logit(
    spec_name = .y,
    spec = .x
  )
)

saveRDS(
  models_post_share_lpm,
  "Output/models/models_post_share_lpm.rds"
)

saveRDS(
  models_post_share_logit,
  "Output/models/models_post_share_logit.rds"
)

# ---------------------------------------------------------------------------- #
# 7. Funciones para extraer coeficientes
# ---------------------------------------------------------------------------- #

get_clean_coeftable <- function(model) {
  
  ct <- as.data.frame(coeftable(model))
  ct$term <- rownames(ct)
  
  estimate_col <- names(ct)[str_detect(names(ct), "^Estimate$")]
  se_col <- names(ct)[str_detect(names(ct), "Std\\. Error")]
  p_col <- names(ct)[str_detect(names(ct), "^Pr\\(")]
  
  if (length(estimate_col) == 0) {
    estimate_col <- names(ct)[1]
  }
  
  if (length(se_col) == 0) {
    se_col <- names(ct)[2]
  }
  
  if (length(p_col) == 0) {
    ct$p_value <- NA_real_
  } else {
    ct$p_value <- ct[[p_col[1]]]
  }
  
  ct %>%
    mutate(
      estimate = .data[[estimate_col[1]]],
      se = .data[[se_col[1]]]
    ) %>%
    select(term, estimate, se, p_value)
}

term_matches_parts <- function(term, parts) {
  term_parts <- str_split(term, ":", simplify = FALSE)[[1]]
  setequal(term_parts, parts)
}

extract_one_term <- function(model,
                             parts,
                             model_type = c("lpm", "logit")) {
  
  model_type <- match.arg(model_type)
  
  ct <- get_clean_coeftable(model)
  
  term_row <- ct %>%
    filter(
      map_lgl(term, ~ term_matches_parts(.x, parts))
    )
  
  if (nrow(term_row) == 0) {
    return("")
  }
  
  if (model_type == "lpm") {
    
    format_coef(
      estimate = term_row$estimate[1],
      se = term_row$se[1],
      p = term_row$p_value[1]
    )
    
  } else {
    
    format_logit_sign(
      estimate = term_row$estimate[1],
      p = term_row$p_value[1]
    )
  }
}

extract_post_share_table_one_spec <- function(spec_name) {
  
  lpm_model <- models_post_share_lpm[[spec_name]]
  logit_model <- models_post_share_logit[[spec_name]]
  
  tibble(
    spec = spec_name,
    controls = post_share_specs[[spec_name]]$controls_label,
    term_id = c(
      "post_share_1936_1955",
      "post_share_1956_1978"
    ),
    term_label = c(
      "$Post \\times Share_{1936-1955}$",
      "$Post \\times Share_{1956-1978}$"
    ),
    parts = list(
      c("post", "share_1936_1955"),
      c("post", "share_1956_1978")
    )
  ) %>%
    mutate(
      lpm = map_chr(
        parts,
        ~ extract_one_term(
          model = lpm_model,
          parts = .x,
          model_type = "lpm"
        )
      ),
      logit_sign = map_chr(
        parts,
        ~ extract_one_term(
          model = logit_model,
          parts = .x,
          model_type = "logit"
        )
      ),
      n_lpm = nobs(lpm_model),
      n_logit = nobs(logit_model)
    ) %>%
    select(
      spec,
      controls,
      term_id,
      term_label,
      lpm,
      logit_sign,
      n_lpm,
      n_logit
    )
}

post_share_table <- map_dfr(
  names(post_share_specs),
  extract_post_share_table_one_spec
)

post_share_table <- post_share_table %>%
  mutate(
    spec = factor(
      spec,
      levels = names(post_share_specs)
    ),
    term_id = factor(
      term_id,
      levels = c(
        "post_share_1936_1955",
        "post_share_1956_1978"
      )
    )
  ) %>%
  arrange(term_id, spec) %>%
  mutate(
    spec = as.character(spec),
    term_id = as.character(term_id)
  )

print(post_share_table)

# ---------------------------------------------------------------------------- #
# 8. Guardar tabla en Excel
# ---------------------------------------------------------------------------- #

write.xlsx(
  post_share_table,
  "Output/post_share_lpm_logit_key_coefficients.xlsx",
  overwrite = TRUE
)

# ---------------------------------------------------------------------------- #
# 9. Guardar tabla LaTeX para Overleaf
# ---------------------------------------------------------------------------- #

post_share_wide <- post_share_table %>%
  select(
    term_id,
    term_label,
    spec,
    lpm,
    logit_sign,
    n_lpm,
    n_logit
  ) %>%
  tidyr::pivot_wider(
    names_from = spec,
    values_from = c(lpm, logit_sign, n_lpm, n_logit),
    names_glue = "{spec}_{.value}"
  )

post_share_latex <- post_share_wide %>%
  mutate(
    term_label = as.character(term_label),
    no_controls_lpm = latex_escape(no_controls_lpm),
    no_controls_logit_sign = latex_escape(no_controls_logit_sign),
    with_controls_lpm = latex_escape(with_controls_lpm),
    with_controls_logit_sign = latex_escape(with_controls_logit_sign)
  )

n_no <- unique(post_share_table$n_lpm[post_share_table$spec == "no_controls"])
n_yes <- unique(post_share_table$n_lpm[post_share_table$spec == "with_controls"])

latex_lines <- c(
  "\\begin{table}[!htbp]",
  "\\centering",
  "\\caption{Post-2023 changes by Spanish historical exposure}",
  "\\label{tab:post_share_lpm_logit}",
  "\\small",
  "\\begin{tabular}{lcccccc}",
  "\\hline",
  " & \\multicolumn{2}{c}{No controls} & \\multicolumn{2}{c}{Controls} & \\multicolumn{2}{c}{N} \\\\",
  "Term & LPM & Logit & LPM & Logit & No controls & Controls \\\\",
  "\\hline"
)

for (i in seq_len(nrow(post_share_latex))) {
  
  latex_lines <- c(
    latex_lines,
    paste0(
      post_share_latex$term_label[i],
      " & ",
      post_share_latex$no_controls_lpm[i],
      " & ",
      post_share_latex$no_controls_logit_sign[i],
      " & ",
      post_share_latex$with_controls_lpm[i],
      " & ",
      post_share_latex$with_controls_logit_sign[i],
      " & ",
      n_no[1],
      " & ",
      n_yes[1],
      " \\\\"
    )
  )
}

latex_lines <- c(
  latex_lines,
  "\\hline",
  "\\multicolumn{7}{l}{\\footnotesize Notes: The dependent variable is migration intention.} \\\\",
  "\\multicolumn{7}{l}{\\footnotesize LPM columns report coefficients with clustered standard errors in parentheses.} \\\\",
  "\\multicolumn{7}{l}{\\footnotesize Logit columns report the sign and statistical significance of the corresponding Logit coefficient.} \\\\",
  "\\multicolumn{7}{l}{\\footnotesize All models include year and municipality fixed effects.} \\\\",
  "\\multicolumn{7}{l}{\\footnotesize Controls include age and male.} \\\\",
  "\\multicolumn{7}{l}{\\footnotesize Standard errors are clustered at the municipality level.} \\\\",
  "\\multicolumn{7}{l}{\\footnotesize * p$<$0.10, ** p$<$0.05, *** p$<$0.01.} \\\\",
  "\\end{tabular}",
  "\\end{table}"
)

writeLines(
  latex_lines,
  "Output/tex/post_share_lpm_logit_key_coefficients.tex"
)

# ---------------------------------------------------------------------------- #
# 10. Mensaje final
# ---------------------------------------------------------------------------- #

cat("\nCódigo terminado correctamente.\n")
cat("Modelos LPM guardados en: Output/models/models_post_share_lpm.rds\n")
cat("Modelos Logit guardados en: Output/models/models_post_share_logit.rds\n")
cat("Tabla Excel guardada en: Output/post_share_lpm_logit_key_coefficients.xlsx\n")
cat("Tabla LaTeX guardada en: Output/tex/post_share_lpm_logit_key_coefficients.tex\n")