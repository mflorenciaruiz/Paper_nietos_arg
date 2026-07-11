# ---------------------------------------------------------------------------- #
#                Formatear talas de Logit con efectos marginales
# ---------------------------------------------------------------------------- #
#
# Este script toma el output de los modelos logit corridos en stata, formatea
# y exporta las tablas de:
#   - Intención de migrar con EB
#   - Eefectos heterogéneos de la intención de migrar
#   - P valores de test de igualdad de coeficientes entre terciles 
# ---------------------------------------------------------------------------- #

# ---------------------------------------------------------------------------- #
# 1. Setting
# ---------------------------------------------------------------------------- #
library(readxl)
library(dplyr)
library(tidyverse)
library(knitr)
library(kableExtra)

path <- "/Users/florenciaruiz/BID 2/Paper Valerie/Nietos/Argentina/Paper_nietos_arg"
setwd(path)

results_stata <- read_excel("Output/EB/logit_eb_results.xlsx")
results_stata_36 <- read_excel("Output/EB/logit_eb_results_event_36.xlsx")
results_stata_56 <- read_excel("Output/EB/logit_eb_results_event_56.xlsx")
heterog <- read_excel("Output/heterog_terciles_migration.xlsx")
pairwise <- read_excel("Output/pairwise_terciles_migration.xlsx")
pairwise_lapop <- read_excel("Output/pairwise_terciles_migration_lapop.xlsx")

# ---------------------------------------------------------------------------- #
# 2. Tablas para intención de migrar con EB (ATT)
# ---------------------------------------------------------------------------- #
{
# 1) Definir el orden explicito de las 12 columnas
column_order <- tribble(
  ~weight_var,   ~controls, ~column,
  "w_m_1936_1",  "No",      1,
  "w_m_1936_1",  "Yes",     2,
  "w_m_1956_1",  "No",      3,
  "w_m_1956_1",  "Yes",     4,
  "w_m_1936_4",  "No",      5,
  "w_m_1936_4",  "Yes",     6,
  "w_m_1956_4",  "No",      7,
  "w_m_1956_4",  "Yes",     8,
  "w_m_1936_5",  "No",      9,
  "w_m_1936_5",  "Yes",    10,
  "w_m_1956_5",  "No",     11,
  "w_m_1956_5",  "Yes",    12
)

# 2) Unir el orden al dataframe de resultados
results_ordered <- results_stata %>%
  inner_join(column_order, by = c("weight_var", "controls")) %>%
  arrange(column)

# 3) Convertir a formato long (una fila por termino x columna)
eb_ames <- bind_rows(
  results_ordered %>%
    transmute(
      column       = column,
      term_label   = "Spanish share 1936-1955 $\\times$ Post",
      ame_reported = ame_36,
      se_reported  = se_36,
      p_value      = p_36,
      nobs         = nobs,
      pseudo_r2    = pseudo_r2
    ),
  results_ordered %>%
    transmute(
      column       = column,
      term_label   = "Spanish share 1956-1978 $\\times$ Post",
      ame_reported = ame_56 ,
      se_reported  = se_56 ,
      p_value      = p_56,
      nobs         = nobs,
      pseudo_r2    = pseudo_r2
    )
) %>%
  arrange(column, term_label)

# 4) Exportacion LaTeX 
n_models <- 12
column_headers <- paste0("(", 1:n_models, ")")

term_labels <- c(
  "Spanish share 1936-1955 $\\times$ Post",
  "Spanish share 1956-1978 $\\times$ Post"
)

# Header de la tabla
latex_lines <- c(
  "\\begin{table}[!h]",
  "\\centering",
  "\\renewcommand{\\arraystretch}{1.25}",
  "\\setlength{\\tabcolsep}{3pt}",
  "\\caption{Effect on Migration Intention Under Entropy Balancing Weights}",
  paste0("\\begin{tabular}{l", strrep("c", n_models), "}"),
  "\\hline",
  paste0(" & ", paste(column_headers, collapse = " & "), " \\\\"),
  "\\hline"
)

# Para cada termino: fila con AME + estrellas, y fila con SE en parentesis
for (term_lab in term_labels) {
  row_data <- eb_ames %>%
    filter(term_label == term_lab) %>%
    arrange(column)   # importante: ordenar por column, no por weight
  
  ame_cells <- sprintf("%.3f%s",
                       row_data$ame_reported,
                       stars(row_data$p_value))
  se_cells  <- sprintf("(%.3f)", row_data$se_reported)
  
  latex_lines <- c(
    latex_lines,
    paste0(term_lab, " & ", paste(ame_cells, collapse = " & "), " \\\\"),
    paste0(" & ",          paste(se_cells,  collapse = " & "), " \\\\")
  )
}

# Espacio y filas de estadisticas
latex_lines <- c(latex_lines, "\\addlinespace")

obs_row_data <- eb_ames %>%
  filter(term_label == term_labels[1]) %>%
  arrange(column)

obs_row <- paste0("Observations & ",
                  paste(obs_row_data$nobs, collapse = " & "),
                  " \\\\")

r2_row <- paste0("Pseudo R$^2$ & ",
                 paste(sprintf("%.3f", obs_row_data$pseudo_r2), collapse = " & "),
                 " \\\\")

# Fila de controles individuales (No/Yes segun corresponde)
controls_row_data <- results_ordered %>% arrange(column)
controls_row <- paste0("Controls & ",
                       paste(controls_row_data$controls, collapse = " & "),
                       " \\\\")

muni_fe_row <- paste0("Municipality FE & ",
                      paste(rep("Yes", n_models), collapse = " & "), " \\\\")
year_fe_row <- paste0("Year FE & ",
                      paste(rep("Yes", n_models), collapse = " & "), " \\\\")

latex_lines <- c(
  latex_lines,
  obs_row,
  r2_row,
  controls_row,
  muni_fe_row,
  year_fe_row,
  "\\hline",
  "\\end{tabular}",
  "\\vspace{0.4em}",
  "\\captionsetup{justification=justified, singlelinecheck=false}",
  paste0(
    "\\caption*{\\footnotesize Notes: The dependent variable is migration ",
    "intention. All columns report average marginal effects from a logit specification.",
    "The estimates are expressed as the change in the probability of reporting an",
    "intention to migrate associated with a one-unit increase in the corresponding regressor,",
    "where one unit represents a one-percentage-point increase in the municipal share of the",
    "Spanish-born population. Columns use different sets of ",
    "entropy balancing weights. Columns (1), (2), (5), (6), (9) and (10) use ",
    "entropy balancing weights computed with treatment defined as the share of ",
    "Spaniards in the first migration window, 1936--1955. Columns (3), (4), ",
    "(7), (8), (11) and (12) use entropy balancing weights computed with ",
    "treatment defined as the share of Spaniards in the second migration ",
    "window, 1956--1978. Columns (1)--(4) use entropy balancing weights ",
    "computed to balance the pre-treatment values of the outcome (2011, 2013, ",
    "2015, 2017, 2019, and 2021). Columns (5)--(8) balance on the pre-treatment ",
    "values of the outcome and additionally on mean age in 2010. Columns ",
    "(9)--(12) balance on the pre-treatment values of the outcome and ",
    "additionally on average years of education in 2010. Columns (2), (4), ",
    "(6), (8), (10), and (12) include controls for age and a male indicator. ",
    "Standard errors clustered at the municipality level in parentheses (56 clusters). ",
    "Pseudo R$^2$ is McFadden's. * $p<0.10$, ** $p<0.05$, *** $p<0.01$.}"
  ),
  "\\end{table}"
)


write_latex_lines(latex_lines, "Output/EB/eb_m_att_logit_margins.tex")
}
# ---------------------------------------------------------------------------- #
# 3. Tablas para intención de migrar con EB (Event study 1936)
# ---------------------------------------------------------------------------- #
{
# 1) Definir el orden explicito de las 8 columnas
column_order <- tribble(
  ~weight_var,   ~controls, ~column,
  "no_w"      , "No",       1,
  "no_w"      , "Yes",      2,
  "w_m_1936_1",  "No",      3,
  "w_m_1936_1",  "Yes",     4,
  "w_m_1936_4",  "No",      5,
  "w_m_1936_4",  "Yes",     6,
  "w_m_1936_5",  "No",      7,
  "w_m_1936_5",  "Yes",     8
)

# 2) Unir el orden al dataframe de resultados
results_ordered <- results_stata_36 %>%
  inner_join(column_order, by = c("weight_var", "controls")) %>%
  arrange(column)

# 3) Convertir a formato long (una fila por termino x columna)
eb_ames <- bind_rows(
  results_ordered %>%
    transmute(
      column       = column,
      term_label   = "Spanish share 1936-1955 $\\times$ 2012",
      ame_reported = ame_36_12,
      se_reported  = se_36_12,
      p_value      = p_36_12,
      nobs         = nobs,
      pseudo_r2    = pseudo_r2
    ),
  results_ordered %>%
    transmute(
      column       = column,
      term_label   = "Spanish share 1936-1955 $\\times$ 2014",
      ame_reported = ame_36_14 ,
      se_reported  = se_36_14 ,
      p_value      = p_36_14,
      nobs         = nobs,
      pseudo_r2    = pseudo_r2
    ), 
  results_ordered %>%
    transmute(
      column       = column,
      term_label   = "Spanish share 1936-1955 $\\times$ 2017",
      ame_reported = ame_36_17 ,
      se_reported  = se_36_17,
      p_value      = p_36_17,
      nobs         = nobs,
      pseudo_r2    = pseudo_r2
    ), 
  results_ordered %>%
    transmute(
      column       = column,
      term_label   = "Spanish share 1936-1955 $\\times$ 2023",
      ame_reported = ame_36_23 ,
      se_reported  = se_36_23,
      p_value      = p_36_23,
      nobs         = nobs,
      pseudo_r2    = pseudo_r2
    )
) %>%
  arrange(column, term_label)

# 4) Exportacion LaTeX 
n_models <- 8
column_headers <- paste0("(", 1:n_models, ")")

term_labels <- c(
  "Spanish share 1936-1955 $\\times$ 2012",
  "Spanish share 1936-1955 $\\times$ 2014",
  "Spanish share 1936-1955 $\\times$ 2017",
  "Spanish share 1936-1955 $\\times$ 2023"
)

# Header de la tabla
latex_lines <- c(
  "\\begin{table}[!h]",
  "\\centering",
  "\\renewcommand{\\arraystretch}{1.25}",
  "\\setlength{\\tabcolsep}{3pt}",
  "\\caption{Event Study Estimates on Intention to Migrate Under Entropy Balancing Weights -- Spanish Share 1936-1955}",
  paste0("\\begin{tabular}{l", strrep("c", n_models), "}"),
  "\\hline",
  paste0(" & ", paste(column_headers, collapse = " & "), " \\\\"),
  "\\hline"
)

# Para cada termino: fila con AME + estrellas, y fila con SE en parentesis
for (term_lab in term_labels) {
  row_data <- eb_ames %>%
    filter(term_label == term_lab) %>%
    arrange(column)   # importante: ordenar por column, no por weight
  
  ame_cells <- sprintf("%.3f%s",
                       row_data$ame_reported,
                       stars(row_data$p_value))
  se_cells  <- sprintf("(%.3f)", row_data$se_reported)
  
  latex_lines <- c(
    latex_lines,
    paste0(term_lab, " & ", paste(ame_cells, collapse = " & "), " \\\\"),
    paste0(" & ",          paste(se_cells,  collapse = " & "), " \\\\")
  )
}

# Espacio y filas de estadisticas
latex_lines <- c(latex_lines, "\\addlinespace")

obs_row_data <- eb_ames %>%
  filter(term_label == term_labels[1]) %>%
  arrange(column)

obs_row <- paste0("Observations & ",
                  paste(obs_row_data$nobs, collapse = " & "),
                  " \\\\")

r2_row <- paste0("Pseudo R$^2$ & ",
                 paste(sprintf("%.3f", obs_row_data$pseudo_r2), collapse = " & "),
                 " \\\\")

# Fila de controles individuales (No/Yes segun corresponde)
controls_row_data <- results_ordered %>% arrange(column)
controls_row <- paste0("Controls & ",
                       paste(controls_row_data$controls, collapse = " & "),
                       " \\\\")

muni_fe_row <- paste0("Municipality FE & ",
                      paste(rep("Yes", n_models), collapse = " & "), " \\\\")
year_fe_row <- paste0("Year FE & ",
                      paste(rep("Yes", n_models), collapse = " & "), " \\\\")
eb_weights <- paste0("EB weights & ",
                      paste(rep("No", 2), collapse = " & ") ," & ", paste(rep("Yes", n_models-2), collapse = " & ") , " \\\\")

latex_lines <- c(
  latex_lines,
  obs_row,
  r2_row,
  controls_row,
  muni_fe_row,
  year_fe_row,
  eb_weights,
  "\\hline",
  "\\end{tabular}",
  "\\vspace{0.4em}",
  "\\captionsetup{justification=justified, singlelinecheck=false}",
  paste0(
    "\\caption*{\\footnotesize Notes: The dependent variable is migration ",
    "intention. All columns report average marginal effects from a logit specification.",
    "The estimates are expressed as the change in the probability of reporting an",
    "intention to migrate associated with a one-unit increase in the corresponding regressor,",
    "where one unit represents a one-percentage-point increase in the municipal share of the",
    "Spanish-born population. Columns use different sets of ",
    "entropy balancing weights. Columns (1) and (2) are estimated without entropy balancing weights;",
    "columns (3)-(8) use entropy balancing weights computed with treatment defined as the share of",
    "Spaniards in the first migration window, 1936–1955. Columns (3) and (4) use entropy balancing",
    "weights computed to balance the pre-treatment values of the outcome (2011, 2013, 2015, 2017, 2019, and 2021).",
    "Columns (5) and (6) balance on the pre-treatment values of the outcome and additionally on population density and",
    "mean age in 2010. Columns (7) and (8) balance on the pre-treatment values of the outcome and additionally",
    "on population density and average years of education in 2010. Columns (2), (4), (6), and (8) include",
    "controls for age and a male indicator. ",
    "Standard errors clustered at the municipality level in parentheses (56 clusters). ",
    "Pseudo R$^2$ is McFadden's. * $p<0.10$, ** $p<0.05$, *** $p<0.01$.}"
  ),
  "\\end{table}"
)

write_latex_lines(latex_lines, "Output/EB/eb_m_36_logit_margins.tex")
}
# ---------------------------------------------------------------------------- #
# 4. Tablas para intención de migrar con EB (Event study 1956)
# ---------------------------------------------------------------------------- #
{
# 1) Definir el orden explicito de las 8 columnas
column_order <- tribble(
  ~weight_var,   ~controls, ~column,
  "no_w"      , "No",       1,
  "no_w"      , "Yes",      2,
  "w_m_1956_1",  "No",      3,
  "w_m_1956_1",  "Yes",     4,
  "w_m_1956_4",  "No",      5,
  "w_m_1956_4",  "Yes",     6,
  "w_m_1956_5",  "No",      7,
  "w_m_1956_5",  "Yes",     8
)

# 2) Unir el orden al dataframe de resultados
results_ordered <- results_stata_56 %>%
  inner_join(column_order, by = c("weight_var", "controls")) %>%
  arrange(column)

# 3) Convertir a formato long (una fila por termino x columna)
eb_ames <- bind_rows(
  results_ordered %>%
    transmute(
      column       = column,
      term_label   = "Spanish share 1956-1978 $\\times$ 2012",
      ame_reported = ame_56_12,
      se_reported  = se_56_12,
      p_value      = p_56_12,
      nobs         = nobs,
      pseudo_r2    = pseudo_r2
    ),
  results_ordered %>%
    transmute(
      column       = column,
      term_label   = "Spanish share 1956-1978 $\\times$ 2014",
      ame_reported = ame_56_14 ,
      se_reported  = se_56_14 ,
      p_value      = p_56_14,
      nobs         = nobs,
      pseudo_r2    = pseudo_r2
    ), 
  results_ordered %>%
    transmute(
      column       = column,
      term_label   = "Spanish share 1956-1978 $\\times$ 2017",
      ame_reported = ame_56_17 ,
      se_reported  = se_56_17,
      p_value      = p_56_17,
      nobs         = nobs,
      pseudo_r2    = pseudo_r2
    ), 
  results_ordered %>%
    transmute(
      column       = column,
      term_label   = "Spanish share 1956-1978 $\\times$ 2023",
      ame_reported = ame_56_23 ,
      se_reported  = se_56_23,
      p_value      = p_56_23,
      nobs         = nobs,
      pseudo_r2    = pseudo_r2
    )
) %>%
  arrange(column, term_label)

# 4) Exportacion LaTeX 
n_models <- 8
column_headers <- paste0("(", 1:n_models, ")")

term_labels <- c(
  "Spanish share 1956-1978 $\\times$ 2012",
  "Spanish share 1956-1978 $\\times$ 2014",
  "Spanish share 1956-1978 $\\times$ 2017",
  "Spanish share 1956-1978 $\\times$ 2023"
)

# Header de la tabla
latex_lines <- c(
  "\\begin{table}[!h]",
  "\\centering",
  "\\renewcommand{\\arraystretch}{1.25}",
  "\\setlength{\\tabcolsep}{3pt}",
  "\\caption{Event Study Estimates on Intention to Migrate Under Entropy Balancing Weights -- Spanish Share 1976-1978}",
  paste0("\\begin{tabular}{l", strrep("c", n_models), "}"),
  "\\hline",
  paste0(" & ", paste(column_headers, collapse = " & "), " \\\\"),
  "\\hline"
)

# Para cada termino: fila con AME + estrellas, y fila con SE en parentesis
for (term_lab in term_labels) {
  row_data <- eb_ames %>%
    filter(term_label == term_lab) %>%
    arrange(column)   # importante: ordenar por column, no por weight
  
  ame_cells <- sprintf("%.3f%s",
                       row_data$ame_reported,
                       stars(row_data$p_value))
  se_cells  <- sprintf("(%.3f)", row_data$se_reported)
  
  latex_lines <- c(
    latex_lines,
    paste0(term_lab, " & ", paste(ame_cells, collapse = " & "), " \\\\"),
    paste0(" & ",          paste(se_cells,  collapse = " & "), " \\\\")
  )
}

# Espacio y filas de estadisticas
latex_lines <- c(latex_lines, "\\addlinespace")

obs_row_data <- eb_ames %>%
  filter(term_label == term_labels[1]) %>%
  arrange(column)

obs_row <- paste0("Observations & ",
                  paste(obs_row_data$nobs, collapse = " & "),
                  " \\\\")

r2_row <- paste0("Pseudo R$^2$ & ",
                 paste(sprintf("%.3f", obs_row_data$pseudo_r2), collapse = " & "),
                 " \\\\")

# Fila de controles individuales (No/Yes segun corresponde)
controls_row_data <- results_ordered %>% arrange(column)
controls_row <- paste0("Controls & ",
                       paste(controls_row_data$controls, collapse = " & "),
                       " \\\\")

muni_fe_row <- paste0("Municipality FE & ",
                      paste(rep("Yes", n_models), collapse = " & "), " \\\\")
year_fe_row <- paste0("Year FE & ",
                      paste(rep("Yes", n_models), collapse = " & "), " \\\\")
eb_weights <- paste0("EB weights & ",
                     paste(rep("No", 2), collapse = " & ") ," & ", paste(rep("Yes", n_models-2), collapse = " & ") , " \\\\")

latex_lines <- c(
  latex_lines,
  obs_row,
  r2_row,
  controls_row,
  muni_fe_row,
  year_fe_row,
  eb_weights,
  "\\hline",
  "\\end{tabular}",
  "\\vspace{0.4em}",
  "\\captionsetup{justification=justified, singlelinecheck=false}",
  paste0(
    "\\caption*{\\footnotesize Notes: The dependent variable is migration ",
    "intention. All columns report average marginal effects from a logit specification.",
    "The estimates are expressed as the change in the probability of reporting an",
    "intention to migrate associated with a one-unit increase in the corresponding regressor,",
    "where one unit represents a one-percentage-point increase in the municipal share of the",
    "Spanish-born population. Columns use different sets of ",
    "entropy balancing weights. Columns (1) and (2) are estimated without entropy balancing weights;",
    "columns (3)-(8) use entropy balancing weights computed with treatment defined as the share of",
    "Spaniards in the second migration window, 1956–1978. Columns (3) and (4) use entropy balancing",
    "weights computed to balance the pre-treatment values of the outcome (2011, 2013, 2015, 2017, 2019, and 2021).",
    "Columns (5) and (6) balance on the pre-treatment values of the outcome and additionally on population density and",
    "mean age in 2010. Columns (7) and (8) balance on the pre-treatment values of the outcome and additionally",
    "on population density and average years of education in 2010. Columns (2), (4), (6), and (8) include",
    "controls for age and a male indicator. ",
    "Standard errors clustered at the municipality level in parentheses (56 clusters). ",
    "Pseudo R$^2$ is McFadden's. * $p<0.10$, ** $p<0.05$, *** $p<0.01$.}"
  ),
  "\\end{table}"
)

write_latex_lines(latex_lines, "Output/EB/eb_m_56_logit_margins.tex")
}
# ---------------------------------------------------------------------------- #
# 5. Tablas efectos heterogéneos
# ---------------------------------------------------------------------------- #
{
# ==========================================
# Etiquetas y agrupamiento en paneles
# ==========================================
tercile_labels <- tribble(
  ~tercile_var,        ~label,                    ~panel,
  "t_density_2010",    "Pop.\\ density",          "A",
  "t_fem_2010",        "Female pop.",             "A",
  "t_mean_schyr_2010", "Mean years educ.",        "B",
  "t_med_dage_2010",   "Median age",              "B",
  "t_pea_2010",        "Share in labor force",    "C",
  "t_unemp_2010",      "Share unemployed",        "C",
  "t_izam_pre_avg",    "Left vote share",         "D",
  "t_alt_pre_avg",     "Ideological alternation", "D"
)

heterog_labeled <- heterog %>% left_join(tercile_labels, by = "tercile_var")

# ==========================================
# Helpers
# ==========================================
stars <- function(p) case_when(
  p < 0.01 ~ "***",
  p < 0.05 ~ "**",
  p < 0.10 ~ "*",
  TRUE     ~ ""
)
fmt_ame <- function(b, p, d = 3) sprintf(paste0("%.", d, "f%s"), b, stars(p))
fmt_se  <- function(s, d = 3)    sprintf(paste0("(%.", d, "f)"), s)
fmt_p   <- function(x, d = 3)    sprintf(paste0("%.", d, "f"), x)
fmt_ub  <- function(x, d = 3)    sprintf(paste0("$%.", d, "f$"), x)
fmt_n   <- function(x)           format(x, big.mark = ",", trim = TRUE)

# ==========================================
# Constructor del panel (mismo formato que make_panel_tabular)
# ==========================================
make_panel_tabular_from_stata <- function(df_panel, group_names, digits = 3) {
  stopifnot(length(group_names) == 2, length(unique(df_panel$label)) == 2)
  
  # Orden estable: primero la variable 1, luego la variable 2; dentro de cada,
  # T1 -> T2 -> T3. Esto matchea el orden de columnas del panel.
  df_panel <- df_panel %>%
    mutate(label_order = match(label, group_names)) %>%
    arrange(label_order, tercile)
  
  # Construir los 6 valores (T1_v1, T2_v1, T3_v1, T1_v2, T2_v2, T3_v2) por fila
  cells_ame_36 <- fmt_ame(df_panel$ame_36, df_panel$p_36, digits)
  cells_se_36  <- fmt_se(df_panel$se_36, digits)
  cells_ame_56 <- fmt_ame(df_panel$ame_56, df_panel$p_56, digits)
  cells_se_56  <- fmt_se(df_panel$se_56, digits)
  cells_p_eq   <- fmt_p(df_panel$p_equal_cohorts, 3)
  cells_ub     <- fmt_ub(df_panel$tercile_ub, digits)
  cells_n      <- fmt_n(df_panel$nobs)
  
  make_row <- function(label, vals) {
    sprintf("%s & %s \\\\", label, paste(vals, collapse = " & "))
  }
  
  # Encabezado agrupado (dos variables por panel, 3 terciles cada una)
  header_top <- sprintf("& \\multicolumn{3}{c}{%s} & \\multicolumn{3}{c}{%s} \\\\",
                        group_names[1], group_names[2])
  cmid       <- "\\cmidrule(l){2-4} \\cmidrule(l){5-7}"
  header_sub <- "& T1 & T2 & T3 & T1 & T2 & T3 \\\\"
  
  # Filas de contenido (mismo orden que make_panel_tabular)
  row_36    <- make_row("Spanish share 1936-1955$\\times$Post", cells_ame_36)
  row_36_se <- make_row("",                                     cells_se_36)
  row_56    <- make_row("Spanish share 1956-1978$\\times$Post", cells_ame_56)
  row_56_se <- make_row(" ",                                    cells_se_56)
  row_p_eq  <- make_row("$p$-value ($\\beta_{36{-}55} = \\beta_{56{-}78}$)", cells_p_eq)
  row_ub    <- make_row("Tercile upper bound",                  cells_ub)
  row_n     <- make_row("Observations",                         cells_n)
  
  # Ensamblado
  c(
    "\\begin{tabular*}{\\textwidth}{l@{\\extracolsep{\\fill}}cccccc}",
    "\\hline",
    header_top,
    cmid,
    header_sub,
    "\\hline",
    row_36,
    row_36_se,
    row_56,
    row_56_se,
    "\\addlinespace",
    row_n,
    row_p_eq,
    row_ub,
    "\\hline",
    "\\end{tabular*}"
  )
}

# ==========================================
# Construir los 4 paneles
# ==========================================
panel_A <- make_panel_tabular_from_stata(
  df_panel    = heterog_labeled %>% filter(panel == "A"),
  group_names = c("Pop.\\ density", "Female pop.")
)

panel_B <- make_panel_tabular_from_stata(
  df_panel    = heterog_labeled %>% filter(panel == "B"),
  group_names = c("Mean years educ.", "Median age")
)
# Sacar la hline de arriba del panel B
first_hline_B <- grep("^\\\\hline$", panel_B)[1]
if (!is.na(first_hline_B)) panel_B <- panel_B[-first_hline_B]

panel_C <- make_panel_tabular_from_stata(
  df_panel    = heterog_labeled %>% filter(panel == "C"),
  group_names = c("Share in labor force", "Share unemployed")
)
first_hline_C <- grep("^\\\\hline$", panel_C)[1]
if (!is.na(first_hline_C)) panel_C <- panel_C[-first_hline_C]

panel_D <- make_panel_tabular_from_stata(
  df_panel    = heterog_labeled %>% filter(panel == "D"),
  group_names = c("Left vote share", "Ideological alternation")
)
first_hline_D <- grep("^\\\\hline$", panel_D)[1]
if (!is.na(first_hline_D)) panel_D <- panel_D[-first_hline_D]

# ==========================================
# Nota con los joint tests (leyendo desde el Excel)
# ==========================================
joint_tests_text <- heterog_labeled %>%
  distinct(label, panel, p_joint_36, p_joint_56) %>%
  arrange(panel, match(label, tercile_labels$label)) %>%
  mutate(entry = sprintf("%s (%.3f / %.3f)", label, p_joint_36, p_joint_56)) %>%
  pull(entry) %>%
  paste(collapse = "; ")

nota_completa <- paste0(
  "\\caption*{\\footnotesize Notes: The dependent variable is migration ",
    "intention. Each column reports subsample estimates for the tercile of the ",
    "specified municipal-level variable. Terciles are defined using the full ",
    "sample of 312 municipalities, and the corresponding tercile upper bounds ",
    "shown at the bottom of each panel are also calculated from this full ",
    "municipal sample. All specifications include municipality and year ",
    "fixed effects and individual controls for age and a male indicator. The reported coefficients are ",
    "average marginal effects from a logit specification, expressed as ",
    "the change in the probability of reporting migration intention per ",
    "one-unit increase in the corresponding regressor, where one unit ",
    "corresponds to one percentage point of the municipal Spanish-born ",
    "population share. Standard errors are clustered at the municipality level ",
    "and reported in parentheses (56 clusters). The row labeled ",
    "$p$-value: $\\beta^{1936-1955}=\\beta^{1956-1978}$ reports the p-value from ",
    "a two-sided Wald test of equality between the two cohort-specific ",
    "coefficients within each tercile. Joint Wald tests of the null hypothesis ",
    "that the coefficient on Spanish share $\\times$ Post is equal across the ",
    "three terciles ",
    "($H_0: \\beta_{T_1} = \\beta_{T_2} = \\beta_{T_3}$), computed under the ",
    "independence of the tercile subsamples and following a $\\chi^2$ ",
    "distribution with 2 degrees of freedom, yield the following p-values, ",
    "reported as ($\\beta^{1936-1955}$ / $\\beta^{1956-1978}$) for each ",
    "variable: ", joint_tests_text, ". ",
    "* $p<0.10$, ** $p<0.05$, *** $p<0.01$.}"
)

# =================
# Ensamblado final 
# =================
final <- c(
  "\\begin{table}[!h]",
  "\\centering",
  "\\renewcommand{\\arraystretch}{1.10}",
  "\\setlength{\\tabcolsep}{6pt}",
  "\\captionsetup{justification=centering}",
  "\\caption{Heterogeneous Effects on Migration Intention}",
  panel_A,
  panel_B,
  panel_C,
  panel_D,
  "\\addvspace{0.2em}",
  "\\captionsetup{font=footnotesize, justification=justified, singlelinecheck=false}",
  nota_completa,
  "\\end{table}"
)

writeLines(final, "Output/migration_subsamples.tex")
}
# ---------------------------------------------------------------------------- #
# 6. Tabla de p valores de tests de igualdad de efectos entre terciles
# ---------------------------------------------------------------------------- #

# ==========================================
# Uno la data de pairwise
# ==========================================

pairwise <- rbind(pairwise, pairwise_lapop)

# ==========================================
# Etiquetas y agrupamiento en paneles (mismo que la tabla principal)
# ==========================================
tercile_labels <- tribble(
  ~tercile_var,         ~label,                    ~panel,
  "t_density_2010",     "Pop. density",            "A",
  "t_fem_2010",         "Female pop.",             "A",
  "t_mean_schyr_2010",  "Mean years educ.",        "B",
  "t_med_dage_2010",    "Median age",              "B",
  "t_pea_2010",         "Share in labor force",    "C",
  "t_unemp_2010",       "Share unemployed",        "C",
  "t_izam_pre_avg",     "Left vote share",         "D",
  "t_alt_pre_avg",      "Ideological alternation", "D",
  "t_interes_pol_mucho","Share very interested in politics",   "E",
  "t_en_pareja",        "Share partnered",             "E",
)

pairwise_labeled <- pairwise %>% left_join(tercile_labels, by = "tercile_var")

# ==========================================
# Constructor del panel de p-values (desde Excel)
# ==========================================
make_pvalue_panel_from_stata <- function(df_panel, group_names, group_sizes = c(3, 3)) {
  stopifnot(length(group_names) == 2, nrow(df_panel) == 2)
  
  # Orden estable segun group_names
  df_panel <- df_panel %>%
    mutate(label_order = match(label, group_names)) %>%
    arrange(label_order)
  
  n_data_cols <- sum(group_sizes)  # 6 columnas de datos
  
  # 1) Construir la matriz de p-values (2 filas: cohorte 36, cohorte 56; 6 cols)
  p_matrix <- matrix("", nrow = 2, ncol = n_data_cols)
  
  for (v in seq_len(nrow(df_panel))) {
    row <- df_panel[v, ]
    col_start <- (v - 1) * 3 + 1
    
    p_matrix[1, col_start:(col_start + 2)] <- sprintf(
      "%.3f", c(row$p_36_T1T2, row$p_36_T1T3, row$p_36_T2T3))
    p_matrix[2, col_start:(col_start + 2)] <- sprintf(
      "%.3f", c(row$p_56_T1T2, row$p_56_T1T3, row$p_56_T2T3))
  }
  
  # 2) Construir el LaTeX del panel (mismo formato que make_pvalue_panel)
  lines <- character()
  col_spec <- paste0("l@{\\extracolsep{\\fill}}",
                     paste(rep("c", n_data_cols), collapse = ""))
  lines <- c(lines, paste0("\\begin{tabular*}{\\textwidth}{", col_spec, "}"))
  lines <- c(lines, "\\hline")
  
  # Header agrupado
  mc_parts <- paste0("\\multicolumn{", group_sizes, "}{c}{", group_names, "}")
  lines <- c(lines, paste0(" & ", paste(mc_parts, collapse = " & "), " \\\\"))
  
  # Cmidrules
  cmids <- c(); pos <- 2
  for (gs in group_sizes) {
    cmids <- c(cmids, paste0("\\cmidrule(l){", pos, "-", pos + gs - 1, "}"))
    pos <- pos + gs
  }
  lines <- c(lines, paste(cmids, collapse = " "))
  
  # Sub-headers
  sub_headers <- rep(c("$T_1 = T_2$", "$T_1 = T_3$", "$T_2 = T_3$"), 
                     length(group_names))
  lines <- c(lines, paste0(" & ", paste(sub_headers, collapse = " & "), " \\\\"))
  lines <- c(lines, "\\hline")
  
  # Filas de p-values
  row_36 <- paste0("Spanish share 1936-1955$\\times$Post & ",
                   paste(p_matrix[1, ], collapse = " & "), " \\\\")
  row_56 <- paste0("Spanish share 1956-1978$\\times$Post & ",
                   paste(p_matrix[2, ], collapse = " & "), " \\\\")
  lines <- c(lines, row_36, row_56)
  
  lines <- c(lines, "\\hline")
  lines <- c(lines, "\\end{tabular*}")
  
  lines
}

# ==========================================
# Construir los 5 paneles
# ==========================================
panel_A_pv <- make_pvalue_panel_from_stata(
  df_panel    = pairwise_labeled %>% filter(panel == "A"),
  group_names = c("Pop. density", "Female pop.")
)

panel_B_pv <- make_pvalue_panel_from_stata(
  df_panel    = pairwise_labeled %>% filter(panel == "B"),
  group_names = c("Mean years educ.", "Median age")
)
first_hline_B <- grep("^\\\\hline$", panel_B_pv)[1]
if (!is.na(first_hline_B)) panel_B_pv <- panel_B_pv[-first_hline_B]

panel_C_pv <- make_pvalue_panel_from_stata(
  df_panel    = pairwise_labeled %>% filter(panel == "C"),
  group_names = c("Share in labor force", "Share unemployed")
)
first_hline_C <- grep("^\\\\hline$", panel_C_pv)[1]
if (!is.na(first_hline_C)) panel_C_pv <- panel_C_pv[-first_hline_C]

panel_D_pv <- make_pvalue_panel_from_stata(
  df_panel    = pairwise_labeled %>% filter(panel == "D"),
  group_names = c("Left vote share", "Ideological alternation")
)
first_hline_D <- grep("^\\\\hline$", panel_D_pv)[1]
if (!is.na(first_hline_D)) panel_D_pv <- panel_D_pv[-first_hline_D]

panel_E_pv <- make_pvalue_panel_from_stata(
  df_panel    = pairwise_labeled %>% filter(panel == "E"),
  group_names = c("Share very interested in politics", "Share partnered")
)
first_hline_E <- grep("^\\\\hline$", panel_E_pv)[1]
if (!is.na(first_hline_E)) panel_E_pv <- panel_E_pv[-first_hline_E]

# ==========================================
# Ensamblado final (identico al del codigo 03)
# ==========================================
final <- c(
  "\\begin{table}[!h]",
  "\\centering",
  "\\renewcommand{\\arraystretch}{1.15}",
  "\\setlength{\\tabcolsep}{4pt}",
  "\\caption{Pairwise Tests of Coefficient Equality Across Terciles --- Migration Intention}",
  panel_A_pv,
  panel_B_pv,
  panel_C_pv,
  panel_D_pv,
  panel_E_pv,
  "\\vspace{0.3em}",
  "\\captionsetup{justification=justified, singlelinecheck=false}",
  "\\caption*{",
  paste0(
    "\\footnotesize Notes: This table reports p-values from tests of equality of ",
    "the average marginal effect of the Spanish share $\\times$ Post interaction across ",
    "pairs of terciles of each municipal characteristic. Each column corresponds to a ",
    "pairwise comparison ($T_1 = T_2$, $T_1 = T_3$, or $T_2 = T_3$). Tests are computed ",
    "as z-statistics on the difference between the two average marginal effects estimated ",
    "on independent subsamples. The dependent variable is migration intention. ",
    "* $p<0.10$, ** $p<0.05$, *** $p<0.01$.}"
  ),
  "\\end{table}"
)

writeLines(final, "Output/migration_tercile_pvalues.tex")
