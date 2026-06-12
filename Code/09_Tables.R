# ---------------------------------------------------------------------------- #
#     LAPOP: combinar tablas LPM y Logit para Overleaf
# ---------------------------------------------------------------------------- #
#
# Este script no estima modelos.
# Lee las tablas ya generadas por los scripts LPM y Logit,
# convierte los resultados Logit a signo + significatividad,
# y exporta tres tablas combinadas para Overleaf:
#
#   1. post x X
#   2. X x share español
#   3. post x X x share español
#
# ---------------------------------------------------------------------------- #

library(dplyr)
library(readxl)
library(readr)
library(stringr)
library(openxlsx)

# ---------------------------------------------------------------------------- #
# 0. Paths
# ---------------------------------------------------------------------------- #

path_pili <- "C:\\Users\\pilih\\Documents\\Papers German\\Valerie\\Paper_nietos_arg"
setwd(path_pili)

dir.create("Output", showWarnings = FALSE)
dir.create("Output/tex", showWarnings = FALSE, recursive = TRUE)

# ---------------------------------------------------------------------------- #
# 1. Funciones auxiliares
# ---------------------------------------------------------------------------- #

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

# Convierte una celda Logit tipo "1.245** (0.230)" en "+**"
# y una celda tipo "0.812* (0.101)" en "-*".
# Si OR > 1 => signo positivo.
# Si OR < 1 => signo negativo.
# Las estrellas se conservan.
format_logit_sign_from_or <- function(x) {
  
  if (is.na(x) || str_trim(as.character(x)) == "") {
    return("")
  }
  
  x_chr <- as.character(x)
  
  stars <- str_extract(x_chr, "\\*+")
  if (is.na(stars)) {
    stars <- ""
  }
  
  or_value <- parse_number(x_chr)
  
  if (is.na(or_value)) {
    return("")
  }
  
  sign_char <- case_when(
    or_value > 1 ~ "+",
    or_value < 1 ~ "-",
    TRUE ~ "0"
  )
  
  paste0(sign_char, stars)
}

# ---------------------------------------------------------------------------- #
# 2. Leer tablas existentes
# ---------------------------------------------------------------------------- #

post_x_lpm <- read_excel(
  "Output/post_x_key_coefficients.xlsx"
)

post_x_logit <- read_excel(
  "Output/logit_post_x_key_coefficients.xlsx"
)

share_lpm <- read_excel(
  "Output/share_interactions_key_coefficients.xlsx"
)

share_logit <- read_excel(
  "Output/logit_share_interactions_key_coefficients.xlsx"
)

triple_lpm <- read_excel(
  "Output/triple_differences_key_coefficients.xlsx"
)

triple_logit <- read_excel(
  "Output/logit_triple_differences_key_coefficients.xlsx"
)

# ---------------------------------------------------------------------------- #
# 3. Tabla combinada 1: post x X
# ---------------------------------------------------------------------------- #

post_x_logit_sign <- post_x_logit %>%
  mutate(
    logit_post_x = sapply(or_post_x, format_logit_sign_from_or)
  ) %>%
  select(
    variable,
    controls,
    logit_post_x
  )

post_x_combined <- post_x_lpm %>%
  left_join(
    post_x_logit_sign,
    by = c("variable", "controls")
  ) %>%
  select(
    variable,
    controls,
    coef_post_x,
    logit_post_x,
    nobs
  )

write.xlsx(
  post_x_combined,
  "Output/post_x_combined_key_coefficients.xlsx",
  overwrite = TRUE
)

# ---------------------------------------------------------------------------- #
# 4. Tabla combinada 2: X x share español
# ---------------------------------------------------------------------------- #

share_logit_sign <- share_logit %>%
  mutate(
    logit_1936_1955 = sapply(or_1936_1955, format_logit_sign_from_or),
    logit_1956_1978 = sapply(or_1956_1978, format_logit_sign_from_or)
  ) %>%
  select(
    variable,
    controls,
    logit_1936_1955,
    logit_1956_1978
  )

share_combined <- share_lpm %>%
  left_join(
    share_logit_sign,
    by = c("variable", "controls")
  ) %>%
  select(
    variable,
    controls,
    coef_1936_1955,
    logit_1936_1955,
    coef_1956_1978,
    logit_1956_1978,
    nobs
  )

write.xlsx(
  share_combined,
  "Output/share_interactions_combined_key_coefficients.xlsx",
  overwrite = TRUE
)

# ---------------------------------------------------------------------------- #
# 5. Tabla combinada 3: post x X x share español
# ---------------------------------------------------------------------------- #

triple_logit_sign <- triple_logit %>%
  mutate(
    logit_1936_1955 = sapply(or_1936_1955, format_logit_sign_from_or),
    logit_1956_1978 = sapply(or_1956_1978, format_logit_sign_from_or)
  ) %>%
  select(
    variable,
    controls,
    logit_1936_1955,
    logit_1956_1978
  )

triple_combined <- triple_lpm %>%
  left_join(
    triple_logit_sign,
    by = c("variable", "controls")
  ) %>%
  select(
    variable,
    controls,
    coef_1936_1955,
    logit_1936_1955,
    coef_1956_1978,
    logit_1956_1978,
    nobs
  )

write.xlsx(
  triple_combined,
  "Output/triple_differences_combined_key_coefficients.xlsx",
  overwrite = TRUE
)

# ---------------------------------------------------------------------------- #
# 6. Exportar tabla combinada 1 a LaTeX: post x X
# ---------------------------------------------------------------------------- #

post_x_combined_latex <- post_x_combined %>%
  mutate(
    variable = label_variable(variable),
    variable = latex_escape(variable),
    controls = latex_escape(controls),
    coef_post_x = latex_escape(coef_post_x),
    logit_post_x = latex_escape(logit_post_x)
  )

latex_lines_post_x <- c(
  "\\begin{table}[!htbp]",
  "\\centering",
  "\\caption{Post-2023 changes by pre-2023 municipal characteristics}",
  "\\label{tab:post_x_combined_key}",
  "\\small",
  "\\begin{tabular}{lcccr}",
  "\\hline",
  "Variable & Controls & LPM: $Post \\times X$ & Logit sign & N \\\\",
  "\\hline"
)

for (i in seq_len(nrow(post_x_combined_latex))) {
  latex_lines_post_x <- c(
    latex_lines_post_x,
    paste0(
      post_x_combined_latex$variable[i],
      " & ",
      post_x_combined_latex$controls[i],
      " & ",
      post_x_combined_latex$coef_post_x[i],
      " & ",
      post_x_combined_latex$logit_post_x[i],
      " & ",
      post_x_combined_latex$nobs[i],
      " \\\\"
    )
  )
}

latex_lines_post_x <- c(
  latex_lines_post_x,
  "\\hline",
  "\\multicolumn{5}{l}{\\footnotesize Notes: The dependent variable is migration intention.} \\\\",
  "\\multicolumn{5}{l}{\\footnotesize Each row reports a separate specification interacting $Post$ with one pre-2023 municipal characteristic $X$.} \\\\",
  "\\multicolumn{5}{l}{\\footnotesize LPM columns report coefficients with clustered standard errors in parentheses.} \\\\",
  "\\multicolumn{5}{l}{\\footnotesize Logit columns report the sign and statistical significance of the corresponding Logit coefficient.} \\\\",
  "\\multicolumn{5}{l}{\\footnotesize All models include year and municipality fixed effects.} \\\\",
  "\\multicolumn{5}{l}{\\footnotesize In rows with Controls = Yes, controls include age and male.} \\\\",
  "\\multicolumn{5}{l}{\\footnotesize Standard errors are clustered at the municipality level.} \\\\",
  "\\multicolumn{5}{l}{\\footnotesize * p$<$0.10, ** p$<$0.05, *** p$<$0.01.} \\\\",
  "\\end{tabular}",
  "\\end{table}"
)

writeLines(
  latex_lines_post_x,
  "Output/tex/post_x_combined_key_coefficients.tex"
)

# ---------------------------------------------------------------------------- #
# 7. Exportar tabla combinada 2 a LaTeX: X x share español
# ---------------------------------------------------------------------------- #

share_combined_latex <- share_combined %>%
  mutate(
    variable = label_variable(variable),
    variable = latex_escape(variable),
    controls = latex_escape(controls),
    coef_1936_1955 = latex_escape(coef_1936_1955),
    logit_1936_1955 = latex_escape(logit_1936_1955),
    coef_1956_1978 = latex_escape(coef_1956_1978),
    logit_1956_1978 = latex_escape(logit_1956_1978)
  )

latex_lines_share <- c(
  "\\begin{table}[!htbp]",
  "\\centering",
  "\\caption{Interactions between Spanish exposure and pre-2023 municipal characteristics}",
  "\\label{tab:share_interactions_combined_key}",
  "\\small",
  "\\begin{tabular}{lcccccr}",
  "\\hline",
  "Variable & Controls & LPM: $X \\times Share_{1936-1955}$ & Logit sign & LPM: $X \\times Share_{1956-1978}$ & Logit sign & N \\\\",
  "\\hline"
)

for (i in seq_len(nrow(share_combined_latex))) {
  latex_lines_share <- c(
    latex_lines_share,
    paste0(
      share_combined_latex$variable[i],
      " & ",
      share_combined_latex$controls[i],
      " & ",
      share_combined_latex$coef_1936_1955[i],
      " & ",
      share_combined_latex$logit_1936_1955[i],
      " & ",
      share_combined_latex$coef_1956_1978[i],
      " & ",
      share_combined_latex$logit_1956_1978[i],
      " & ",
      share_combined_latex$nobs[i],
      " \\\\"
    )
  )
}

latex_lines_share <- c(
  latex_lines_share,
  "\\hline",
  "\\multicolumn{7}{l}{\\footnotesize Notes: The dependent variable is migration intention.} \\\\",
  "\\multicolumn{7}{l}{\\footnotesize Each row reports a separate specification interacting Spanish exposure with one pre-2023 municipal characteristic $X$.} \\\\",
  "\\multicolumn{7}{l}{\\footnotesize LPM columns report coefficients with clustered standard errors in parentheses.} \\\\",
  "\\multicolumn{7}{l}{\\footnotesize Logit columns report the sign and statistical significance of the corresponding Logit coefficient.} \\\\",
  "\\multicolumn{7}{l}{\\footnotesize All models include year fixed effects. Municipality fixed effects are not included because $X$ and Spanish exposure are time-invariant.} \\\\",
  "\\multicolumn{7}{l}{\\footnotesize In rows with Controls = Yes, controls include age, male, and the remaining pre-2023 municipal characteristics in levels.} \\\\",
  "\\multicolumn{7}{l}{\\footnotesize Standard errors are clustered at the municipality level.} \\\\",
  "\\multicolumn{7}{l}{\\footnotesize * p$<$0.10, ** p$<$0.05, *** p$<$0.01.} \\\\",
  "\\end{tabular}",
  "\\end{table}"
)

writeLines(
  latex_lines_share,
  "Output/tex/share_interactions_combined_key_coefficients.tex"
)

# ---------------------------------------------------------------------------- #
# 8. Exportar tabla combinada 3 a LaTeX: triple diferencias
# ---------------------------------------------------------------------------- #

triple_combined_latex <- triple_combined %>%
  mutate(
    variable = label_variable(variable),
    variable = latex_escape(variable),
    controls = latex_escape(controls),
    coef_1936_1955 = latex_escape(coef_1936_1955),
    logit_1936_1955 = latex_escape(logit_1936_1955),
    coef_1956_1978 = latex_escape(coef_1956_1978),
    logit_1956_1978 = latex_escape(logit_1956_1978)
  )

latex_lines_triple <- c(
  "\\begin{table}[!htbp]",
  "\\centering",
  "\\caption{Triple differences by pre-2023 municipal characteristics}",
  "\\label{tab:triple_differences_combined_key}",
  "\\small",
  "\\begin{tabular}{lcccccr}",
  "\\hline",
  "Variable & Controls & LPM: $Post \\times X \\times Share_{1936-1955}$ & Logit sign & LPM: $Post \\times X \\times Share_{1956-1978}$ & Logit sign & N \\\\",
  "\\hline"
)

for (i in seq_len(nrow(triple_combined_latex))) {
  latex_lines_triple <- c(
    latex_lines_triple,
    paste0(
      triple_combined_latex$variable[i],
      " & ",
      triple_combined_latex$controls[i],
      " & ",
      triple_combined_latex$coef_1936_1955[i],
      " & ",
      triple_combined_latex$logit_1936_1955[i],
      " & ",
      triple_combined_latex$coef_1956_1978[i],
      " & ",
      triple_combined_latex$logit_1956_1978[i],
      " & ",
      triple_combined_latex$nobs[i],
      " \\\\"
    )
  )
}

latex_lines_triple <- c(
  latex_lines_triple,
  "\\hline",
  "\\multicolumn{7}{l}{\\footnotesize Notes: The dependent variable is migration intention.} \\\\",
  "\\multicolumn{7}{l}{\\footnotesize Each row reports a separate triple-difference specification.} \\\\",
  "\\multicolumn{7}{l}{\\footnotesize LPM columns report coefficients with clustered standard errors in parentheses.} \\\\",
  "\\multicolumn{7}{l}{\\footnotesize Logit columns report the sign and statistical significance of the corresponding Logit coefficient.} \\\\",
  "\\multicolumn{7}{l}{\\footnotesize All models include year and municipality fixed effects.} \\\\",
  "\\multicolumn{7}{l}{\\footnotesize In rows with Controls = Yes, controls include age, male, and $Post$ interacted with the remaining pre-2023 municipal characteristics.} \\\\",
  "\\multicolumn{7}{l}{\\footnotesize Standard errors are clustered at the municipality level.} \\\\",
  "\\multicolumn{7}{l}{\\footnotesize * p$<$0.10, ** p$<$0.05, *** p$<$0.01.} \\\\",
  "\\end{tabular}",
  "\\end{table}"
)

writeLines(
  latex_lines_triple,
  "Output/tex/triple_differences_combined_key_coefficients.tex"
)

# ---------------------------------------------------------------------------- #
# 9. Mensaje final
# ---------------------------------------------------------------------------- #

cat("\nCódigo terminado correctamente.\n")
cat("Tablas combinadas guardadas en:\n")
cat("  Output/tex/post_x_combined_key_coefficients.tex\n")
cat("  Output/tex/share_interactions_combined_key_coefficients.tex\n")
cat("  Output/tex/triple_differences_combined_key_coefficients.tex\n")