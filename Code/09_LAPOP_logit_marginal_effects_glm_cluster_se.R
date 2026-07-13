# ---------------------------------------------------------------------------- #
#     LAPOP: Logit con efectos marginales promedio para todas las tablas
# ---------------------------------------------------------------------------- #
#
# CHEQUEADO QUE LOS VALORES DE ESTA SON IGUALES QUE CALCULANDO LOS FE DE LA OTRA FORMA, ESTÁ OK
#
# Este script replica la logica de los scripts Logit, pero reemplaza:
#   - odds ratios
#   - signos del coeficiente Logit
#   - coeficientes en log-odds
#
# por efectos marginales promedio de modelos Logit, calculados con:
#   marginaleffects::avg_slopes()
#
# Salidas principales:
#   Output/logit_marginal_effects_glm_cluster_se.xlsx
#   Output/tex/logit_post_share_mfx_glm_cluster_se.tex
#   Output/tex/logit_post_x_mfx_glm_cluster_se.tex
#   Output/tex/logit_triple_mfx_glm_cluster_se.tex
#
# Interpretacion de las tablas:
#   - Por default, los efectos se reportan en puntos porcentuales.
#   - Un valor de 2.500 significa +2.5 p.p. en la probabilidad predicha de
#     intencion_migrar = 1.
#
# Nota importante:
#   Para las interacciones, creo variables explicitas:
#     me_post__share_1936_1955 = post * share_1936_1955
#   y calculo avg_slopes() sobre esa variable. Esto permite reportar el efecto
#   marginal promedio del termino de interes del Logit en escala de probabilidad.
#
# ---------------------------------------------------------------------------- #

# ---------------------------------------------------------------------------- #
# 0. Paquetes
# ---------------------------------------------------------------------------- #

packages <- c(
  "dplyr", "readr", "stringr", "purrr", "tidyr",
  "fixest", "openxlsx", "haven", "marginaleffects", "sandwich"
)

installed <- rownames(installed.packages())
to_install <- setdiff(packages, installed)
if (length(to_install) > 0) install.packages(to_install)

library(dplyr)
library(readr)
library(stringr)
library(purrr)
library(tidyr)
library(fixest)
library(openxlsx)
library(haven)
library(marginaleffects)
library(sandwich)
library(readxl)

# ---------------------------------------------------------------------------- #
# 1. Paths y parametros
# ---------------------------------------------------------------------------- #

#path <- "C:/Users/pilih/Documents/Papers German/Valerie/Paper_nietos_arg"
path <- "/Users/florenciaruiz/BID 2/Paper Valerie/Nietos/Argentina/Paper_nietos_arg"
setwd(path)

dir.create("Output", showWarnings = FALSE)
dir.create("Output/tex", showWarnings = FALSE, recursive = TRUE)
dir.create("Output/models", showWarnings = FALSE, recursive = TRUE)

# Mantengo la logica de tus scripts originales: post = 1 para year >= 2022.
# Si queres que sea estrictamente 2023, cambia esta linea a 2023.
post_cutoff <- 2022

# TRUE: reporta efectos marginales en puntos porcentuales.
# FALSE: reporta efectos marginales en probabilidades, entre -1 y 1.
report_in_percentage_points <- TRUE
scale_factor <- ifelse(report_in_percentage_points, 100, 1)
scale_label <- ifelse(report_in_percentage_points, "p.p.", "probability")

lapop <- read_csv("Data Out/lapop_data_merge.csv", show_col_types = FALSE)

censo_2010_density <- read_dta("Data Int/censo_2010_arg_mun.dta") %>%
  select(mun_code, popdensgeo2)

# ---------------------------------------------------------------------------- #
# 2. Preparar base
# ---------------------------------------------------------------------------- #

lapop <- lapop %>%
  mutate(
    year_num = as.numeric(year),
    post = if_else(year_num >= post_cutoff, 1, 0),
    mun_code = as.factor(mun_code),
    year = as.factor(year_num)
  ) %>%
  left_join(censo_2010_density, by = "mun_code") %>%
  mutate(
    popdensgeo2 = log(popdensgeo2)
  )

# ---------------------------------------------------------------------------- #
# 3. Construir controles municipales pre-post_cutoff
# ---------------------------------------------------------------------------- #

wmean <- function(x, w) {
  ok <- !is.na(x) & !is.na(w)
  if (sum(ok) == 0) return(NA_real_)
  weighted.mean(x[ok], w = w[ok], na.rm = TRUE)
}

safe_wmean <- function(data, var) {
  if (!var %in% names(data)) return(NA_real_)
  x <- data[[var]]
  if (!is.numeric(x) & !is.integer(x) & !is.logical(x)) return(NA_real_)
  wmean(as.numeric(x), data$wt)
}

pre_period_all <- lapop %>%
  filter(year_num < post_cutoff)

if (nrow(pre_period_all) == 0) {
  stop("No hay observaciones pre para calcular controles municipales. Revisar post_cutoff.")
}

mun_pre_all_controls <- pre_period_all %>%
  group_by(mun_code) %>%
  group_modify(
    ~ tibble(
      mun_pre_all_mean_edad = safe_wmean(.x, "edad"),
      mun_pre_all_share_hombre = safe_wmean(.x, "hombre"),
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
# 4. Funciones auxiliares
# ---------------------------------------------------------------------------- #

rhs_join <- function(...) {
  parts <- c(...)
  parts <- parts[!is.na(parts)]
  parts <- parts[str_trim(parts) != ""]
  if (length(parts) == 0) return("")
  paste(parts, collapse = " + ")
}

add_fe_rhs <- function(rhs) {
  # For glm(), fixed effects are included explicitly as factor variables.
  # year and mun_code are already factors in the prepared data.
  rhs_join(rhs, "year", "mun_code")
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

format_ame <- function(estimate, se, p) {
  if (is.na(estimate)) return("")
  paste0(
    sprintf("%.3f", estimate * scale_factor),
    stars(p),
    " (",
    sprintf("%.3f", se * scale_factor),
    ")"
  )
}

# Helpers de formateo (para el LaTeX de la tabla de efectos heterogenos) 
# A diferencia de format_ame, separan el se y beta para ponerlos en filas distintas
fmt_ame <- function(b, p, d = 3) sprintf(paste0("%.", d, "f%s"), b, stars(p))
fmt_se  <- function(s, d = 3)    sprintf(paste0("(%.", d, "f)"), s)
fmt_p   <- function(x, d = 3)    sprintf(paste0("%.", d, "f"), x)
fmt_ub  <- function(x, d = 3)    sprintf(paste0("$%.", d, "f$"), x)
fmt_n   <- function(x)           format(x, big.mark = ",", trim = TRUE)

latex_escape <- function(x) {
  x %>%
    as.character() %>%
    str_replace_all("_", "\\\\_") %>%
    str_replace_all("%", "\\\\%")
}

make_interaction_name <- function(...) {
  paste0("me_", paste(make.names(c(...)), collapse = "__"))
}

add_interaction <- function(data, newvar, vars) {
  missing_vars <- setdiff(vars, names(data))
  if (length(missing_vars) > 0) {
    stop(
      paste0(
        "No se puede crear ", newvar, ". Faltan: ",
        paste(missing_vars, collapse = ", ")
      )
    )
  }
  data[[newvar]] <- Reduce(`*`, lapply(vars, function(v) data[[v]]))
  data
}

get_p_value <- function(df) {
  if ("p.value" %in% names(df)) return(df[["p.value"]])
  if ("p_value" %in% names(df)) return(df[["p_value"]])
  if ("Pr(>|z|)" %in% names(df)) return(df[["Pr(>|z|)"]])
  rep(NA_real_, nrow(df))
}

get_std_error <- function(df) {
  if ("std.error" %in% names(df)) return(df[["std.error"]])
  if ("std_error" %in% names(df)) return(df[["std_error"]])
  if ("Std. Error" %in% names(df)) return(df[["Std. Error"]])
  rep(NA_real_, nrow(df))
}

extract_ame <- function(model, variable, label, controls_label) {
  # Version B: glm() with explicit factor fixed effects.
  # This allows marginaleffects to compute clustered standard errors.
  ame <- marginaleffects::avg_slopes(
    model,
    variables = variable,
    vcov = ~ mun_code
  )

  ame <- as.data.frame(ame)

  estimate <- ame$estimate[1]
  se <- get_std_error(ame)[1]
  p_value <- get_p_value(ame)[1]

  tibble(
    term_id = variable,
    term_label = label,
    controls = controls_label,
    ame = estimate,
    se = se,
    p_value = p_value,
    ame_reported = estimate * scale_factor,
    se_reported = se * scale_factor,
    ame_formatted = format_ame(estimate, se, p_value),
    nobs = nobs(model)
  )
}

write_latex_lines <- function(lines, file) {
  writeLines(lines, con = file)
  cat("Tabla LaTeX guardada en:", file, "\n")
}

# ---------------------------------------------------------------------------- #
# 5. Variables y etiquetas
# ---------------------------------------------------------------------------- #

exposure_1 <- "share_1936_1955"
exposure_2 <- "share_1956_1978"

post_x_vars_core <- c(
  "mun_pre_all_mean_edad",
  "mun_pre_all_share_hombre",
  "popdensgeo2",
  "mun_pre_all_share_desempleado",
  "mun_pre_all_share_en_pareja",
  "mun_pre_all_mean_educ"
)

post_x_vars_politics <- c(
  "mun_pre_all_mean_izq_der",
  "mun_pre_all_share_interes_pol_mucho",
  "mun_pre_all_share_voto_blanco_nulo"
)

post_x_vars <- unique(c(post_x_vars_core, post_x_vars_politics))
post_x_vars <- post_x_vars[
  post_x_vars %in% names(lapop) &
    map_lgl(post_x_vars, ~ any(!is.na(lapop[[.x]])))
]

controls_individual <- c("edad", "hombre")
controls_individual <- controls_individual[controls_individual %in% names(lapop)]

variable_labels <- c(
  "mun_pre_all_mean_edad" = "Mean age",
  "mun_pre_all_share_hombre" = "Share male",
  "popdensgeo2" = "Log population density",
  "mun_pre_all_share_desempleado" = "Share unemployed",
  "mun_pre_all_share_en_pareja" = "Share partnered",
  "mun_pre_all_mean_educ" = "Mean years of education",
  "mun_pre_all_mean_izq_der" = "Mean left-right ideology",
  "mun_pre_all_share_interes_pol_mucho" = "Share very interested in politics",
  "mun_pre_all_share_voto_blanco_nulo" = "Share blank/null vote",
  "share_1936_1955" = "Share 1936-1955",
  "share_1956_1978" = "Share 1956-1978"
)

label_variable <- function(x) {
  ifelse(x %in% names(variable_labels), variable_labels[x], x)
}

# ---------------------------------------------------------------------------- #
# 6. Crear interacciones explicitas
# ---------------------------------------------------------------------------- #

int_post_share_1 <- make_interaction_name("post", exposure_1)
int_post_share_2 <- make_interaction_name("post", exposure_2)

lapop <- add_interaction(lapop, int_post_share_1, c("post", exposure_1))
lapop <- add_interaction(lapop, int_post_share_2, c("post", exposure_2))

int_post_x_names <- setNames(
  map_chr(post_x_vars, ~ make_interaction_name("post", .x)),
  post_x_vars
)

for (x in post_x_vars) {
  lapop <- add_interaction(lapop, int_post_x_names[[x]], c("post", x))
}

int_triple_1_names <- setNames(
  map_chr(post_x_vars, ~ make_interaction_name("post", .x, exposure_1)),
  post_x_vars
)

int_triple_2_names <- setNames(
  map_chr(post_x_vars, ~ make_interaction_name("post", .x, exposure_2)),
  post_x_vars
)

for (x in post_x_vars) {
  lapop <- add_interaction(lapop, int_triple_1_names[[x]], c("post", x, exposure_1))
  lapop <- add_interaction(lapop, int_triple_2_names[[x]], c("post", x, exposure_2))
}

# ---------------------------------------------------------------------------- #
# 7. Modelo 1: Logit post x shares
# ---------------------------------------------------------------------------- #

post_share_specs <- list(
  no_controls = list(controls_label = "No", controls = character(0)),
  with_controls = list(controls_label = "Yes", controls = controls_individual)
)

make_post_share_formula <- function(controls = character(0)) {
  rhs <- rhs_join(
    paste(c(int_post_share_1, int_post_share_2), collapse = " + "),
    paste(controls, collapse = " + ")
  )

  as.formula(paste0("intencion_migrar ~ ", add_fe_rhs(rhs)))
}

run_post_share_logit <- function(spec_name, spec) {
  fml <- make_post_share_formula(spec$controls)

  cat("\nEstimando Logit post x share. Spec:", spec_name, "\n")
  cat(deparse(fml), "\n\n")

  glm(
    fml,
    data = lapop,
    family = binomial(link = "logit"),
    weights = wt
  )
}

models_post_share_logit_me <- imap(
  post_share_specs,
  ~ run_post_share_logit(spec_name = .y, spec = .x)
)

saveRDS(models_post_share_logit_me, "Output/models/models_post_share_logit_marginal_effects.rds")

logit_post_share_me_long <- imap_dfr(
  models_post_share_logit_me,
  function(model, spec_name) {
    controls_label <- post_share_specs[[spec_name]]$controls_label

    bind_rows(
      extract_ame(
        model = model,
        variable = int_post_share_1,
        label = "$Post \\times Share_{1936-1955}$",
        controls_label = controls_label
      ),
      extract_ame(
        model = model,
        variable = int_post_share_2,
        label = "$Post \\times Share_{1956-1978}$",
        controls_label = controls_label
      )
    ) %>%
      mutate(spec = spec_name, .before = 1)
  }
)

logit_post_share_me_wide <- logit_post_share_me_long %>%
  select(term_id, term_label, spec, ame_formatted, nobs) %>%
  pivot_wider(
    names_from = spec,
    values_from = c(ame_formatted, nobs),
    names_glue = "{spec}_{.value}"
  ) %>%
  select(
    term_id,
    term_label,
    no_controls_ame = no_controls_ame_formatted,
    with_controls_ame = with_controls_ame_formatted,
    no_controls_n = no_controls_nobs,
    with_controls_n = with_controls_nobs
  )

# ---------------------------------------------------------------------------- #
# 8. Modelo 2: Logit post x caracteristicas municipales
# ---------------------------------------------------------------------------- #

run_post_x_logit <- function(x,
                             with_controls = FALSE,
                             data = lapop,
                             controls_individual = c("edad", "hombre")) {
  int_x <- int_post_x_names[[x]]

  rhs_main <- paste0(x, " + ", int_x)

  if (with_controls) {
    controls_individual <- controls_individual[controls_individual %in% names(data)]
    rhs <- rhs_join(rhs_main, paste(controls_individual, collapse = " + "))
  } else {
    rhs <- rhs_main
  }

  fml <- as.formula(paste0("intencion_migrar ~ ", add_fe_rhs(rhs)))

  cat("\nEstimando Logit post x X para:", x, "\n")
  cat("Controles individuales:", ifelse(with_controls, "Si", "No"), "\n")
  cat(deparse(fml), "\n\n")

  glm(
    fml,
    data = data,
    family = binomial(link = "logit"),
    weights = wt
  )
}

models_logit_post_x_me_no_controls <- map(
  post_x_vars,
  ~ run_post_x_logit(x = .x, with_controls = FALSE)
)
names(models_logit_post_x_me_no_controls) <- post_x_vars

models_logit_post_x_me_with_controls <- map(
  post_x_vars,
  ~ run_post_x_logit(x = .x, with_controls = TRUE)
)
names(models_logit_post_x_me_with_controls) <- post_x_vars

saveRDS(models_logit_post_x_me_no_controls, "Output/models/models_logit_post_x_marginal_effects_no_controls.rds")
saveRDS(models_logit_post_x_me_with_controls, "Output/models/models_logit_post_x_marginal_effects_with_controls.rds")

logit_post_x_me_long <- bind_rows(
  imap_dfr(
    models_logit_post_x_me_no_controls,
    function(model, x) {
      extract_ame(
        model = model,
        variable = int_post_x_names[[x]],
        label = label_variable(x),
        controls_label = "No"
      ) %>%
        mutate(variable = x, .before = 1)
    }
  ),
  imap_dfr(
    models_logit_post_x_me_with_controls,
    function(model, x) {
      extract_ame(
        model = model,
        variable = int_post_x_names[[x]],
        label = label_variable(x),
        controls_label = "Yes"
      ) %>%
        mutate(variable = x, .before = 1)
    }
  )
)

logit_post_x_me_wide <- logit_post_x_me_long %>%
  select(variable, term_label, controls, ame_formatted, nobs) %>%
  mutate(controls = if_else(controls == "No", "no_controls", "with_controls")) %>%
  pivot_wider(
    names_from = controls,
    values_from = c(ame_formatted, nobs),
    names_glue = "{controls}_{.value}"
  ) %>%
  transmute(
    variable,
    term_label,
    no_controls_ame = no_controls_ame_formatted,
    with_controls_ame = with_controls_ame_formatted,
    no_controls_n = no_controls_nobs,
    with_controls_n = with_controls_nobs
  )

# ---------------------------------------------------------------------------- #
# 9. Modelo 3: Logit triple diferencias
# ---------------------------------------------------------------------------- #

run_triple_logit <- function(x,
                             with_controls = FALSE,
                             data = lapop,
                             controls_individual = c("edad", "hombre"),
                             mun_pre_controls = post_x_vars) {
  int_post_x <- int_post_x_names[[x]]
  int_triple_1 <- int_triple_1_names[[x]]
  int_triple_2 <- int_triple_2_names[[x]]

  rhs_triple <- paste(
    c(
      int_post_share_1,
      int_post_share_2,
      int_post_x,
      int_triple_1,
      int_triple_2
    ),
    collapse = " + "
  )

  if (with_controls) {
    other_mun_pre_vars <- setdiff(mun_pre_controls, x)

    rhs_other_mun_pre <- paste(
      int_post_x_names[other_mun_pre_vars],
      collapse = " + "
    )

    controls_individual <- controls_individual[controls_individual %in% names(data)]

    rhs <- rhs_join(
      rhs_triple,
      rhs_other_mun_pre,
      paste(controls_individual, collapse = " + ")
    )
  } else {
    rhs <- rhs_triple
  }

  fml <- as.formula(paste0("intencion_migrar ~ ", add_fe_rhs(rhs)))

  cat("\nEstimando Logit triple para:", x, "\n")
  cat("Controles adicionales:", ifelse(with_controls, "Si", "No"), "\n")
  cat(deparse(fml), "\n\n")

  glm(
    fml,
    data = data,
    family = binomial(link = "logit"),
    weights = wt
  )
}

models_logit_triple_me_no_controls <- map(
  post_x_vars,
  ~ run_triple_logit(x = .x, with_controls = FALSE)
)
names(models_logit_triple_me_no_controls) <- post_x_vars

models_logit_triple_me_with_controls <- map(
  post_x_vars,
  ~ run_triple_logit(x = .x, with_controls = TRUE)
)
names(models_logit_triple_me_with_controls) <- post_x_vars

saveRDS(models_logit_triple_me_no_controls, "Output/models/models_logit_triple_marginal_effects_no_controls.rds")
saveRDS(models_logit_triple_me_with_controls, "Output/models/models_logit_triple_marginal_effects_with_controls.rds")

extract_triple_ames <- function(model, x, controls_label) {
  bind_rows(
    extract_ame(
      model = model,
      variable = int_triple_1_names[[x]],
      label = "$Post \\times X \\times Share_{1936-1955}$",
      controls_label = controls_label
    ),
    extract_ame(
      model = model,
      variable = int_triple_2_names[[x]],
      label = "$Post \\times X \\times Share_{1956-1978}$",
      controls_label = controls_label
    )
  ) %>%
    mutate(variable = x, variable_label = label_variable(x), .before = 1)
}

logit_triple_me_long <- bind_rows(
  imap_dfr(
    models_logit_triple_me_no_controls,
    ~ extract_triple_ames(model = .x, x = .y, controls_label = "No")
  ),
  imap_dfr(
    models_logit_triple_me_with_controls,
    ~ extract_triple_ames(model = .x, x = .y, controls_label = "Yes")
  )
)

logit_triple_me_wide <- logit_triple_me_long %>%
  mutate(
    controls = if_else(controls == "No", "no_controls", "with_controls"),
    share_group = case_when(
      term_id %in% unname(int_triple_1_names) ~ "share_1936_1955",
      term_id %in% unname(int_triple_2_names) ~ "share_1956_1978",
      TRUE ~ term_id
    )
  ) %>%
  select(variable, variable_label, controls, share_group, ame_formatted, nobs) %>%
  pivot_wider(
    names_from = c(controls, share_group),
    values_from = c(ame_formatted, nobs),
    names_glue = "{controls}_{share_group}_{.value}"
  ) %>%
  transmute(
    variable,
    variable_label,
    no_controls_1936_1955 = no_controls_share_1936_1955_ame_formatted,
    no_controls_1956_1978 = no_controls_share_1956_1978_ame_formatted,
    with_controls_1936_1955 = with_controls_share_1936_1955_ame_formatted,
    with_controls_1956_1978 = with_controls_share_1956_1978_ame_formatted,
    no_controls_n = no_controls_share_1936_1955_nobs,
    with_controls_n = with_controls_share_1936_1955_nobs
  )

# ---------------------------------------------------------------------------- #
# 10. Exportar Excel
# ---------------------------------------------------------------------------- #

wb <- createWorkbook()

sheets <- list(
  "post_share_long" = logit_post_share_me_long,
  "post_share_wide" = logit_post_share_me_wide,
  "post_x_long" = logit_post_x_me_long,
  "post_x_wide" = logit_post_x_me_wide,
  "triple_long" = logit_triple_me_long,
  "triple_wide" = logit_triple_me_wide
)

for (s in names(sheets)) {
  addWorksheet(wb, s)
  writeData(wb, s, sheets[[s]])
  freezePane(wb, s, firstRow = TRUE)
  setColWidths(wb, s, cols = 1:ncol(sheets[[s]]), widths = "auto")
}

saveWorkbook(wb, "Output/logit_marginal_effects_glm_cluster_se.xlsx", overwrite = TRUE)

# ---------------------------------------------------------------------------- #
# 11. Exportar LaTeX para Overleaf
# ---------------------------------------------------------------------------- #

row_end <- "\\\\"

# 11.1 Post x shares
post_share_tex <- logit_post_share_me_wide %>%
  mutate(
    term_label = as.character(term_label),
    no_controls_ame = latex_escape(no_controls_ame),
    with_controls_ame = latex_escape(with_controls_ame)
  )

latex_lines <- c(
  "\\begin{table}[!htbp]",
  "\\centering",
  "\\caption{Logit average marginal effects: post-period Spanish historical exposure}",
  "\\label{tab:logit_post_share_mfx}",
  "\\small",
  "\\begin{tabular}{lcccc}",
  "\\hline",
  paste0("Term & No controls & Controls & N no controls & N controls ", row_end),
  "\\hline"
)

for (i in seq_len(nrow(post_share_tex))) {
  latex_lines <- c(
    latex_lines,
    paste0(
      post_share_tex$term_label[i], " & ",
      post_share_tex$no_controls_ame[i], " & ",
      post_share_tex$with_controls_ame[i], " & ",
      post_share_tex$no_controls_n[i], " & ",
      post_share_tex$with_controls_n[i], " ", row_end
    )
  )
}

latex_lines <- c(
  latex_lines,
  "\\hline",
  paste0("\\multicolumn{5}{l}{\\footnotesize Notes: The dependent variable is migration intention.} ", row_end),
  paste0("\\multicolumn{5}{l}{\\footnotesize Logit entries report average marginal effects in ", scale_label, ".} ", row_end),
  paste0("\\multicolumn{5}{l}{\\footnotesize All models include year and municipality fixed effects and use survey weights.} ", row_end),
  paste0("\\multicolumn{5}{l}{\\footnotesize Controls include age and male. Standard errors clustered at municipality level are in parentheses.} ", row_end),
  paste0("\\multicolumn{5}{l}{\\footnotesize * p$<$0.10, ** p$<$0.05, *** p$<$0.01.} ", row_end),
  "\\end{tabular}",
  "\\end{table}"
)

write_latex_lines(latex_lines, "Output/tex/logit_post_share_mfx_glm_cluster_se.tex")

# 11.2 Post x X
post_x_tex <- logit_post_x_me_wide %>%
  mutate(
    term_label = latex_escape(term_label),
    no_controls_ame = latex_escape(no_controls_ame),
    with_controls_ame = latex_escape(with_controls_ame)
  )

latex_lines <- c(
  "\\begin{table}[!htbp]",
  "\\centering",
  "\\caption{Logit average marginal effects: post-period municipal characteristics}",
  "\\label{tab:logit_post_x_mfx}",
  "\\small",
  "\\begin{tabular}{lcccc}",
  "\\hline",
  paste0("Variable & No controls & Controls & N no controls & N controls ", row_end),
  "\\hline"
)

for (i in seq_len(nrow(post_x_tex))) {
  latex_lines <- c(
    latex_lines,
    paste0(
      post_x_tex$term_label[i], " & ",
      post_x_tex$no_controls_ame[i], " & ",
      post_x_tex$with_controls_ame[i], " & ",
      post_x_tex$no_controls_n[i], " & ",
      post_x_tex$with_controls_n[i], " ", row_end
    )
  )
}

latex_lines <- c(
  latex_lines,
  "\\hline",
  paste0("\\multicolumn{5}{l}{\\footnotesize Notes: Each row reports a separate Logit model interacting $Post$ with one characteristic $X$.} ", row_end),
  paste0("\\multicolumn{5}{l}{\\footnotesize Entries report average marginal effects in ", scale_label, ".} ", row_end),
  paste0("\\multicolumn{5}{l}{\\footnotesize All models include year and municipality fixed effects and use survey weights.} ", row_end),
  paste0("\\multicolumn{5}{l}{\\footnotesize Controls include age and male. Standard errors clustered at municipality level are in parentheses.} ", row_end),
  paste0("\\multicolumn{5}{l}{\\footnotesize * p$<$0.10, ** p$<$0.05, *** p$<$0.01.} ", row_end),
  "\\end{tabular}",
  "\\end{table}"
)

write_latex_lines(latex_lines, "Output/tex/logit_post_x_mfx_glm_cluster_se.tex")

# 11.3 Triple differences
triple_tex <- logit_triple_me_wide %>%
  mutate(
    variable_label = latex_escape(variable_label),
    across(
      c(
        no_controls_1936_1955,
        no_controls_1956_1978,
        with_controls_1936_1955,
        with_controls_1956_1978
      ),
      latex_escape
    )
  )

latex_lines <- c(
  "\\begin{table}[!htbp]",
  "\\centering",
  "\\caption{Logit average marginal effects: triple differences}",
  "\\label{tab:logit_triple_mfx}",
  "\\small",
  "\\begin{tabular}{lcccccc}",
  "\\hline",
  paste0(" & \\multicolumn{2}{c}{No controls} & \\multicolumn{2}{c}{Controls} & \\multicolumn{2}{c}{N} ", row_end),
  paste0("Variable & $1936$-$1955$ & $1956$-$1978$ & $1936$-$1955$ & $1956$-$1978$ & No controls & Controls ", row_end),
  "\\hline"
)

for (i in seq_len(nrow(triple_tex))) {
  latex_lines <- c(
    latex_lines,
    paste0(
      triple_tex$variable_label[i], " & ",
      triple_tex$no_controls_1936_1955[i], " & ",
      triple_tex$no_controls_1956_1978[i], " & ",
      triple_tex$with_controls_1936_1955[i], " & ",
      triple_tex$with_controls_1956_1978[i], " & ",
      triple_tex$no_controls_n[i], " & ",
      triple_tex$with_controls_n[i], " ", row_end
    )
  )
}

latex_lines <- c(
  latex_lines,
  "\\hline",
  paste0("\\multicolumn{7}{l}{\\footnotesize Notes: Each row reports a separate Logit triple-difference model.} ", row_end),
  paste0("\\multicolumn{7}{l}{\\footnotesize Entries report average marginal effects in ", scale_label, " for $Post \\times X \\times Share$.} ", row_end),
  paste0("\\multicolumn{7}{l}{\\footnotesize All models include year and municipality fixed effects and use survey weights.} ", row_end),
  paste0("\\multicolumn{7}{l}{\\footnotesize Controls include age, male, and $Post$ interacted with the remaining municipal characteristics.} ", row_end),
  paste0("\\multicolumn{7}{l}{\\footnotesize Standard errors clustered at municipality level are in parentheses.} ", row_end),
  paste0("\\multicolumn{7}{l}{\\footnotesize * p$<$0.10, ** p$<$0.05, *** p$<$0.01.} ", row_end),
  "\\end{tabular}",
  "\\end{table}"
)

write_latex_lines(latex_lines, "Output/tex/logit_triple_mfx_glm_cluster_se.tex")

# ---------------------------------------------------------------------------- #
# 12. Modelo 1 heterogeneo por terciles de variables municipales LAPOP
# ---------------------------------------------------------------------------- #

# Variables de tercil: nombre_categorica -> nombre_continua
lapop_tercile_vars <- c(
  "t_interes_pol_mucho" = "mun_pre_all_share_interes_pol_mucho",
  "t_en_pareja"         = "mun_pre_all_share_en_pareja"
)

# Crear los terciles a nivel municipal (un valor por municipio y despues merge)
mun_tercile_data <- lapop %>%
  distinct(mun_code,
           mun_pre_all_share_interes_pol_mucho,
           mun_pre_all_share_en_pareja) %>%
  mutate(
    t_interes_pol_mucho = ntile(mun_pre_all_share_interes_pol_mucho, 3),
    t_en_pareja         = ntile(mun_pre_all_share_en_pareja, 3)
  ) %>%
  select(mun_code, t_interes_pol_mucho, t_en_pareja)

lapop <- lapop %>%
  left_join(mun_tercile_data, by = "mun_code")

# Helper: z-test entre coeficientes de submuestras independientes
pairwise_test <- function(b1, b2, v1, v2) {
  z <- (b2 - b1) / sqrt(v1 + v2)
  2 * pnorm(-abs(z))
}

results_list  <- list()
pairwise_list <- list()

for (tv_idx in seq_along(lapop_tercile_vars)) {
  tv <- names(lapop_tercile_vars)[tv_idx]
  sv <- unname(lapop_tercile_vars[tv_idx])
  
  # Matrices acumuladoras para joint y pairwise tests
  betas_36 <- numeric(3); betas_56 <- numeric(3)
  vars_36  <- matrix(0, 3, 3); vars_56  <- matrix(0, 3, 3)
  
  tv_rows <- list()
  
  for (t in 1:3) {
    data_sub <- lapop %>% filter(.data[[tv]] == t)
    
    cat("\n--- Estimando ", tv, " tercil ", t,
        " (N=", nrow(data_sub), ") ---\n", sep = "")
    
    model <- glm(
      make_post_share_formula(controls_individual),
      data    = data_sub,
      family  = binomial(link = "logit"),
      weights = wt
    )
    
    # AMEs de cada cohorte
    ame_36_res <- extract_ame(model, int_post_share_1,
                              "$Post \\times Share_{1936-1955}$", "Yes")
    ame_56_res <- extract_ame(model, int_post_share_2,
                              "$Post \\times Share_{1956-1978}$", "Yes")
    
    # Test AME_36 = AME_56 dentro del tercil (via marginaleffects::avg_slopes)
    hyp <- marginaleffects::avg_slopes(
      model,
      variables  = c(int_post_share_1, int_post_share_2),
      vcov       = ~ mun_code,
      hypothesis = "b1 - b2 = 0"
    ) %>% as.data.frame()
    p_equal <- get_p_value(hyp)[1]
    
    # Upper bound del tercil (max de la variable continua en la submuestra)
    ub <- max(data_sub[[sv]], na.rm = TRUE)
    
    # Almacenar
    betas_36[t]   <- ame_36_res$ame
    betas_56[t]   <- ame_56_res$ame
    vars_36[t, t] <- ame_36_res$se^2
    vars_56[t, t] <- ame_56_res$se^2
    
    tv_rows[[t]] <- tibble(
      tercile_var     = tv,
      tercile         = t,
      ame_36          = ame_36_res$ame,
      se_36           = ame_36_res$se,
      p_36            = ame_36_res$p_value,
      ame_56          = ame_56_res$ame,
      se_56           = ame_56_res$se,
      p_56            = ame_56_res$p_value,
      nobs            = ame_36_res$nobs,
      pseudo_r2       = 1 - model$deviance / model$null.deviance,
      p_equal_cohorts = p_equal,
      tercile_ub      = ub
    )
  }
  
  # Joint test H0: b_T1 = b_T2 = b_T3
  R <- matrix(c(1, -1, 0,
                0,  1, -1), nrow = 2, byrow = TRUE)
  
  Rb_36 <- R %*% betas_36
  RVR_36 <- R %*% vars_36 %*% t(R)
  W_36  <- as.numeric(t(Rb_36) %*% solve(RVR_36) %*% Rb_36)
  p_joint_36 <- pchisq(W_36, df = 2, lower.tail = FALSE)
  
  Rb_56 <- R %*% betas_56
  RVR_56 <- R %*% vars_56 %*% t(R)
  W_56  <- as.numeric(t(Rb_56) %*% solve(RVR_56) %*% Rb_56)
  p_joint_56 <- pchisq(W_56, df = 2, lower.tail = FALSE)
  
  tv_rows <- lapply(tv_rows, function(r) {
    r %>% mutate(p_joint_36 = p_joint_36, p_joint_56 = p_joint_56)
  })
  results_list <- c(results_list, tv_rows)
  
  # Pairwise tests
  pairwise_list[[length(pairwise_list) + 1]] <- tibble(
    tercile_var = tv,
    p_36_T1T2 = pairwise_test(betas_36[1], betas_36[2],
                              vars_36[1,1], vars_36[2,2]),
    p_36_T1T3 = pairwise_test(betas_36[1], betas_36[3],
                              vars_36[1,1], vars_36[3,3]),
    p_36_T2T3 = pairwise_test(betas_36[2], betas_36[3],
                              vars_36[2,2], vars_36[3,3]),
    p_56_T1T2 = pairwise_test(betas_56[1], betas_56[2],
                              vars_56[1,1], vars_56[2,2]),
    p_56_T1T3 = pairwise_test(betas_56[1], betas_56[3],
                              vars_56[1,1], vars_56[3,3]),
    p_56_T2T3 = pairwise_test(betas_56[2], betas_56[3],
                              vars_56[2,2], vars_56[3,3])
  )
}

heterog_results <- bind_rows(results_list) %>%
  select(tercile_var, tercile,
         ame_36, se_36, p_36,
         ame_56, se_56, p_56,
         p_equal_cohorts,
         nobs, pseudo_r2,
         tercile_ub,
         p_joint_36, p_joint_56)

pairwise_results <- bind_rows(pairwise_list)

# ---------------------------------------------------------------------------- #
# 13. Exportar Excels de test de igualdad para usar en el codigo 10
# ---------------------------------------------------------------------------- #

wb_h <- createWorkbook()
addWorksheet(wb_h, "Sheet1")
writeData(wb_h, "Sheet1", heterog_results)
freezePane(wb_h, "Sheet1", firstRow = TRUE)
setColWidths(wb_h, "Sheet1", cols = 1:ncol(heterog_results), widths = "auto")
saveWorkbook(wb_h, "Output/heterog_terciles_migration_lapop.xlsx", overwrite = TRUE)

wb_p <- createWorkbook()
addWorksheet(wb_p, "Sheet1")
writeData(wb_p, "Sheet1", pairwise_results)
freezePane(wb_p, "Sheet1", firstRow = TRUE)
setColWidths(wb_p, "Sheet1", cols = 1:ncol(pairwise_results), widths = "auto")
saveWorkbook(wb_p, "Output/pairwise_terciles_migration_lapop.xlsx", overwrite = TRUE)

# ---------------------------------------------------------------------------- #
# 14. Tabla de efectos heterogenos (variables de lapop)
# ---------------------------------------------------------------------------- #

# --- Version single-var (3 columnas por panel) ---
make_panel_tabular_from_stata_single <- function(df_panel, group_name, digits = 3) {
  stopifnot(nrow(df_panel) == 3)  # 3 filas (una por tercil)
  
  df_panel <- df_panel %>% arrange(tercile)
  
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
  
  header_top <- sprintf("& \\multicolumn{3}{c}{%s} \\\\", group_name)
  cmid       <- "\\cmidrule(l){2-4}"
  header_sub <- "& T1 & T2 & T3 \\\\"
  
  c(
    "\\begin{tabular*}{\\textwidth}{l@{\\extracolsep{\\fill}}ccc}",
    "\\hline",
    header_top,
    cmid,
    header_sub,
    "\\hline",
    make_row("Spanish share 1936-1955$\\times$Post", cells_ame_36),
    make_row("",                                     cells_se_36),
    make_row("Spanish share 1956-1978$\\times$Post", cells_ame_56),
    make_row(" ",                                    cells_se_56),
    "\\addlinespace",
    make_row("Observations",                         cells_n),
    make_row("$p$-value ($\\beta_{36{-}55} = \\beta_{56{-}78}$)", cells_p_eq),
    make_row("Tercile upper bound",                  cells_ub),
    "\\hline",
    "\\end{tabular*}"
  )
}

# --- Cargar el Excel de LAPOP y construir los 2 paneles ---
heterog_lapop <- read_excel("Output/heterog_terciles_migration_lapop.xlsx")

lapop_labels <- tribble(
  ~tercile_var,          ~label,
  "t_interes_pol_mucho", "Share very interested in politics",
  "t_en_pareja",         "Share partnered"
)

heterog_lapop_labeled <- heterog_lapop %>% left_join(lapop_labels, by = "tercile_var")

panel_A_lapop <- make_panel_tabular_from_stata_single(
  df_panel   = heterog_lapop_labeled %>% filter(tercile_var == "t_interes_pol_mucho"),
  group_name = "Share very interested in politics"
)

panel_B_lapop <- make_panel_tabular_from_stata_single(
  df_panel   = heterog_lapop_labeled %>% filter(tercile_var == "t_en_pareja"),
  group_name = "Share partnered"
)
# Sacar hline superior del panel B
first_hline_B <- grep("^\\\\hline$", panel_B_lapop)[1]
if (!is.na(first_hline_B)) panel_B_lapop <- panel_B_lapop[-first_hline_B]

# --- Nota con joint tests ---
joint_tests_text_lapop <- heterog_lapop_labeled %>%
  distinct(label, p_joint_36, p_joint_56) %>%
  mutate(entry = sprintf("%s (%.3f / %.3f)", label, p_joint_36, p_joint_56)) %>%
  pull(entry) %>%
  paste(collapse = "; ")

nota_lapop <- paste0(
  "\\caption*{\\footnotesize Notes: The dependent variable is migration ",
  "intention. Each column reports subsample estimates by municipality-level " ,
  "tercile of the specified variable. Terciles are defined across municipalities " ,
  "using the pre-treatment average of each municipality-level aggregate over ",
  "the 2012, 2014, 2017, and 2019 survey waves. Tercile upper bounds are shown at the ",
  "bottom of each panel. All specifications include municipality and year ",
  "fixed effects and individual controls for age and a male indicator. ",
  "The reported coefficients are average marginal effects from a logit ",
  "specification. Standard errors clustered at the municipality level in ",
  "parentheses (56 clusters). The row labeled ",
  "$p$-value: $\\beta^{1936-1955}=\\beta^{1956-1978}$ reports the p-value from ",
  "a two-sided Wald test of equality between the two cohort-specific ",
  "coefficients within each tercile. Joint Wald tests of the null hypothesis that the coefficient ",
  "on Spanish share $\\times$ Post is equal across the three terciles ",
  "($H_0: \\beta_{T_1} = \\beta_{T_2} = \\beta_{T_3}$), computed under the ",
  "independence of the tercile subsamples and following a $\\chi^2$ ",
  "distribution with 2 degrees of freedom, yield the following p-values, ",
  "reported as ($\\beta^{1936-1955}$ / $\\beta^{1956-1978}$) for each ",
  "variable: ", joint_tests_text_lapop, ". ",
  "* $p<0.10$, ** $p<0.05$, *** $p<0.01$.}"
)

final_lapop <- c(
  "\\begin{table}[!h]",
  "\\centering",
  "\\renewcommand{\\arraystretch}{1.15}",
  "\\setlength{\\tabcolsep}{6pt}",
  "\\captionsetup{justification=centering}",
  "\\caption{Heterogeneous Effects on Migration Intention --- LAPOP characteristics}",
  panel_A_lapop,
  panel_B_lapop,
  "\\addvspace{0.3em}",
  "\\captionsetup{font=footnotesize, justification=justified, singlelinecheck=false}",
  nota_lapop,
  "\\end{table}"
)

writeLines(final_lapop, "Output/migration_subsamples_lapop.tex")
