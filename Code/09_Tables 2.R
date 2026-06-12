# ---------------------------------------------------------------------------- #
#     LAPOP: tabla con todas las interacciones de modelos triple
# ---------------------------------------------------------------------------- #
#
# Este script:
#   - Lee modelos LPM triple y Logit triple ya estimados.
#   - Extrae todas las interacciones relevantes:
#
#       Post x Share 1936-1955
#       Post x Share 1956-1978
#       Post x X
#       Post x X x Share 1936-1955
#       Post x X x Share 1956-1978
#
#   - Arma una tabla larga para Overleaf:
#
#       Municipal characteristic | Term | LPM No | Logit No | LPM Yes | Logit Yes | N No | N Yes
#
# ---------------------------------------------------------------------------- #

library(dplyr)
library(stringr)
library(purrr)
library(tidyr)
library(fixest)
library(openxlsx)

# ---------------------------------------------------------------------------- #
# 0. Paths
# ---------------------------------------------------------------------------- #

path_pili <- "C:\\Users\\pilih\\Documents\\Papers German\\Valerie\\Paper_nietos_arg"
setwd(path_pili)

dir.create("Output", showWarnings = FALSE)
dir.create("Output/tex", showWarnings = FALSE, recursive = TRUE)

# ---------------------------------------------------------------------------- #
# 1. Cargar modelos
# ---------------------------------------------------------------------------- #

models_triple_no_controls <- readRDS(
  "Output/models/models_triple_no_controls.rds"
)

models_triple_with_controls <- readRDS(
  "Output/models/models_triple_with_controls.rds"
)

models_logit_triple_no_controls <- readRDS(
  "Output/models/models_logit_triple_no_controls.rds"
)

models_logit_triple_with_controls <- readRDS(
  "Output/models/models_logit_triple_with_controls.rds"
)

# ---------------------------------------------------------------------------- #
# 2. Funciones auxiliares
# ---------------------------------------------------------------------------- #

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

# Esta función matchea términos aunque fixest cambie el orden.
# Por ejemplo:
#   post:x:share_1936_1955
#   share_1936_1955:post:x
# se reconocen como el mismo término.

term_matches_parts <- function(term, parts) {
  term_parts <- str_split(term, ":", simplify = FALSE)[[1]]
  setequal(term_parts, parts)
}

# ---------------------------------------------------------------------------- #
# 3. Extraer coeftable de forma robusta
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

# ---------------------------------------------------------------------------- #
# 4. Definir términos a extraer para cada X
# ---------------------------------------------------------------------------- #

make_terms_for_x <- function(x,
                             exposure_1 = "share_1936_1955",
                             exposure_2 = "share_1956_1978") {
  
  tibble(
    term_id = c(
      "post_share_1936_1955",
      "post_share_1956_1978",
      "post_x",
      "post_x_share_1936_1955",
      "post_x_share_1956_1978"
    ),
    term_label = c(
      "$Post \\times Share_{1936-1955}$",
      "$Post \\times Share_{1956-1978}$",
      "$Post \\times X$",
      "$Post \\times X \\times Share_{1936-1955}$",
      "$Post \\times X \\times Share_{1956-1978}$"
    ),
    parts = list(
      c("post", exposure_1),
      c("post", exposure_2),
      c("post", x),
      c("post", x, exposure_1),
      c("post", x, exposure_2)
    )
  )
}

# ---------------------------------------------------------------------------- #
# 5. Extraer todas las interacciones por variable X
# ---------------------------------------------------------------------------- #

triple_vars <- names(models_triple_no_controls)

extract_all_interactions_for_x <- function(x) {
  
  terms_x <- make_terms_for_x(x)
  
  lpm_no_model <- models_triple_no_controls[[x]]
  lpm_yes_model <- models_triple_with_controls[[x]]
  
  logit_no_model <- models_logit_triple_no_controls[[x]]
  logit_yes_model <- models_logit_triple_with_controls[[x]]
  
  terms_x %>%
    mutate(
      variable = x,
      lpm_no = map_chr(
        parts,
        ~ extract_one_term(
          model = lpm_no_model,
          parts = .x,
          model_type = "lpm"
        )
      ),
      logit_no = map_chr(
        parts,
        ~ extract_one_term(
          model = logit_no_model,
          parts = .x,
          model_type = "logit"
        )
      ),
      lpm_yes = map_chr(
        parts,
        ~ extract_one_term(
          model = lpm_yes_model,
          parts = .x,
          model_type = "lpm"
        )
      ),
      logit_yes = map_chr(
        parts,
        ~ extract_one_term(
          model = logit_yes_model,
          parts = .x,
          model_type = "logit"
        )
      ),
      n_no = nobs(lpm_no_model),
      n_yes = nobs(lpm_yes_model)
    ) %>%
    select(
      variable,
      term_id,
      term_label,
      lpm_no,
      logit_no,
      lpm_yes,
      logit_yes,
      n_no,
      n_yes
    )
}

triple_all_interactions_table <- map_dfr(
  triple_vars,
  extract_all_interactions_for_x
)

triple_all_interactions_table <- triple_all_interactions_table %>%
  mutate(
    variable = factor(variable, levels = triple_vars),
    term_id = factor(
      term_id,
      levels = c(
        "post_share_1936_1955",
        "post_share_1956_1978",
        "post_x",
        "post_x_share_1936_1955",
        "post_x_share_1956_1978"
      )
    )
  ) %>%
  arrange(variable, term_id) %>%
  mutate(
    variable = as.character(variable),
    term_id = as.character(term_id)
  )

print(triple_all_interactions_table)

write.xlsx(
  triple_all_interactions_table,
  "Output/triple_all_interactions_combined.xlsx",
  overwrite = TRUE
)

# ---------------------------------------------------------------------------- #
# 6. Preparar versión LaTeX
# ---------------------------------------------------------------------------- #

triple_all_interactions_latex <- triple_all_interactions_table %>%
  mutate(
    variable_label = label_variable(variable),
    variable_label = latex_escape(variable_label),
    term_label = as.character(term_label),
    lpm_no = latex_escape(lpm_no),
    logit_no = latex_escape(logit_no),
    lpm_yes = latex_escape(lpm_yes),
    logit_yes = latex_escape(logit_yes),
    n_no = latex_escape(n_no),
    n_yes = latex_escape(n_yes)
  )

# Para no repetir el nombre de la variable en cada fila:
# se muestra solo en la primera fila de cada bloque de X.

triple_all_interactions_latex <- triple_all_interactions_latex %>%
  group_by(variable) %>%
  mutate(
    variable_print = if_else(
      row_number() == 1,
      variable_label,
      ""
    )
  ) %>%
  ungroup()

# ---------------------------------------------------------------------------- #
# 7. Exportar tabla LaTeX
# ---------------------------------------------------------------------------- #

latex_lines <- c(
  "\\begin{table}[!htbp]",
  "\\centering",
  "\\caption{Triple-difference models: all interaction terms}",
  "\\label{tab:triple_all_interactions}",
  "\\scriptsize",
  "\\begin{tabular}{llcccccc}",
  "\\hline",
  "Municipal characteristic & Term & LPM No & Logit No & LPM Yes & Logit Yes & N No & N Yes \\\\",
  "\\hline"
)

for (i in seq_len(nrow(triple_all_interactions_latex))) {
  
  latex_lines <- c(
    latex_lines,
    paste0(
      triple_all_interactions_latex$variable_print[i],
      " & ",
      triple_all_interactions_latex$term_label[i],
      " & ",
      triple_all_interactions_latex$lpm_no[i],
      " & ",
      triple_all_interactions_latex$logit_no[i],
      " & ",
      triple_all_interactions_latex$lpm_yes[i],
      " & ",
      triple_all_interactions_latex$logit_yes[i],
      " & ",
      triple_all_interactions_latex$n_no[i],
      " & ",
      triple_all_interactions_latex$n_yes[i],
      " \\\\"
    )
  )
  
  # Línea separadora después de cada bloque de cinco términos
  if (i %% 5 == 0 && i < nrow(triple_all_interactions_latex)) {
    latex_lines <- c(
      latex_lines,
      "\\addlinespace"
    )
  }
}

latex_lines <- c(
  latex_lines,
  "\\hline",
  "\\multicolumn{8}{l}{\\footnotesize Notes: The dependent variable is migration intention.} \\\\",
  "\\multicolumn{8}{l}{\\footnotesize Each block reports coefficients from a separate triple-difference specification for municipal characteristic $X$.} \\\\",
  "\\multicolumn{8}{l}{\\footnotesize LPM columns report coefficients with clustered standard errors in parentheses.} \\\\",
  "\\multicolumn{8}{l}{\\footnotesize Logit columns report the sign and statistical significance of the corresponding Logit coefficient.} \\\\",
  "\\multicolumn{8}{l}{\\footnotesize All models include year and municipality fixed effects.} \\\\",
  "\\multicolumn{8}{l}{\\footnotesize Controls Yes includes age, male, and $Post$ interacted with the remaining pre-2023 municipal characteristics.} \\\\",
  "\\multicolumn{8}{l}{\\footnotesize $N$ reports the number of observations in the corresponding LPM specification. Standard errors are clustered at the municipality level.} \\\\",
  "\\multicolumn{8}{l}{\\footnotesize Lower-order interaction terms are conditional on the other interacted variables being equal to zero.} \\\\",
  "\\multicolumn{8}{l}{\\footnotesize * p$<$0.10, ** p$<$0.05, *** p$<$0.01.} \\\\",
  "\\end{tabular}",
  "\\end{table}"
)

writeLines(
  latex_lines,
  "Output/tex/triple_all_interactions_combined.tex"
)

# ---------------------------------------------------------------------------- #
# 8. Mensaje final
# ---------------------------------------------------------------------------- #

cat("\nCódigo terminado correctamente.\n")
cat("Tabla Excel guardada en: Output/triple_all_interactions_combined.xlsx\n")
cat("Tabla LaTeX guardada en: Output/tex/triple_all_interactions_combined.tex\n")