# ---------------------------------------------------------------------------- #
#     LAPOP: Logit con efectos marginales promedio para todas las tablas
# ---------------------------------------------------------------------------- #
#
# ESTE SCRIPT SIRVE DE CHEQUEO, PORQUE NO SE PODÍAN CORRER LOS MARGINALES DE LA FORMA EN QUE ESTABAN
# LAS REGRESIONES, PERO DIO IGUAL QUE EN STATA Y QUE EN LA OTRA FORMA DE ESTIMARLOS
# 
# Este script replica la logica de tus scripts Logit, pero reemplaza:
#   - odds ratios
#   - signos del coeficiente Logit
#   - coeficientes en log-odds
#
# por efectos marginales promedio de modelos Logit, calculados con:
#   marginaleffects::avg_slopes()
#
# Salidas principales:
#   Output/logit_marginal_effects_fixest_no_se.xlsx
#   Output/tex/logit_post_share_mfx_fixest_no_se.tex
#   Output/tex/logit_post_x_mfx_fixest_no_se.tex
#   Output/tex/logit_triple_mfx_fixest_no_se.tex
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
  "fixest", "openxlsx", "haven", "marginaleffects"
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

# ---------------------------------------------------------------------------- #
# 1. Paths y parametros
# ---------------------------------------------------------------------------- #

path_pili <- "C:/Users/pilih/Documents/Papers German/Valerie/Paper_nietos_arg"
setwd(path_pili)

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

stars <- function(p) {
  case_when(
    is.na(p) ~ "",
    p < 0.01 ~ "***",
    p < 0.05 ~ "**",
    p < 0.10 ~ "*",
    TRUE ~ ""
  )
}

format_ame <- function(estimate, se = NA_real_, p = NA_real_) {
  if (is.na(estimate)) return("")

  # In the fixest/feglm version, marginaleffects cannot compute uncertainty
  # for fixed-effect parameters. Therefore, this version reports point
  # estimates only, using vcov = FALSE below.
  if (is.na(se)) {
    return(sprintf("%.3f", estimate * scale_factor))
  }

  paste0(
    sprintf("%.3f", estimate * scale_factor),
    stars(p),
    " (",
    sprintf("%.3f", se * scale_factor),
    ")"
  )
}

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
  # Version A: keep fixest::feglm() with absorbed fixed effects.
  # marginaleffects currently cannot compute valid standard errors for
  # avg_slopes() with feglm fixed-effect uncertainty. Therefore, use
  # vcov = FALSE and report point estimates only.
  ame <- marginaleffects::avg_slopes(
    model,
    variables = variable,
    vcov = FALSE
  )

  ame <- as.data.frame(ame)

  estimate <- ame$estimate[1]
  se <- NA_real_
  p_value <- NA_real_

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

  as.formula(paste0("intencion_migrar ~ ", rhs, " | year + mun_code"))
}

run_post_share_logit <- function(spec_name, spec) {
  fml <- make_post_share_formula(spec$controls)

  cat("\nEstimando Logit post x share. Spec:", spec_name, "\n")
  cat(deparse(fml), "\n\n")

  feglm(
    fml,
    data = lapop,
    family = binomial(link = "logit"),
    weights = ~ wt,
    cluster = ~ mun_code
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

  fml <- as.formula(paste0("intencion_migrar ~ ", rhs, " | year + mun_code"))

  cat("\nEstimando Logit post x X para:", x, "\n")
  cat("Controles individuales:", ifelse(with_controls, "Si", "No"), "\n")
  cat(deparse(fml), "\n\n")

  feglm(
    fml,
    data = data,
    family = binomial(link = "logit"),
    weights = ~ wt,
    cluster = ~ mun_code
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

  fml <- as.formula(paste0("intencion_migrar ~ ", rhs, " | year + mun_code"))

  cat("\nEstimando Logit triple para:", x, "\n")
  cat("Controles adicionales:", ifelse(with_controls, "Si", "No"), "\n")
  cat(deparse(fml), "\n\n")

  feglm(
    fml,
    data = data,
    family = binomial(link = "logit"),
    weights = ~ wt,
    cluster = ~ mun_code
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

saveWorkbook(wb, "Output/logit_marginal_effects_fixest_no_se.xlsx", overwrite = TRUE)

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
  paste0("\\multicolumn{5}{l}{\\footnotesize Controls include age and male. This fixest version reports point estimates only because standard errors are not available for marginal effects with absorbed fixed effects.} ", row_end),
  paste0("\\multicolumn{5}{l}{\\footnotesize No standard errors or significance stars are reported in this version.} ", row_end),
  "\\end{tabular}",
  "\\end{table}"
)

write_latex_lines(latex_lines, "Output/tex/logit_post_share_mfx_fixest_no_se.tex")

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
  paste0("\\multicolumn{5}{l}{\\footnotesize Controls include age and male. This fixest version reports point estimates only because standard errors are not available for marginal effects with absorbed fixed effects.} ", row_end),
  paste0("\\multicolumn{5}{l}{\\footnotesize No standard errors or significance stars are reported in this version.} ", row_end),
  "\\end{tabular}",
  "\\end{table}"
)

write_latex_lines(latex_lines, "Output/tex/logit_post_x_mfx_fixest_no_se.tex")

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
  paste0("\\multicolumn{7}{l}{\\footnotesize This fixest version reports point estimates only because standard errors are not available for marginal effects with absorbed fixed effects.} ", row_end),
  paste0("\\multicolumn{7}{l}{\\footnotesize No standard errors or significance stars are reported in this version.} ", row_end),
  "\\end{tabular}",
  "\\end{table}"
)

write_latex_lines(latex_lines, "Output/tex/logit_triple_mfx_fixest_no_se.tex")

# ---------------------------------------------------------------------------- #
# 12. Mensaje final
# ---------------------------------------------------------------------------- #

cat("\nListo. Outputs principales:\n")
cat("- Output/logit_marginal_effects_fixest_no_se.xlsx\n")
cat("- Output/tex/logit_post_share_mfx_fixest_no_se.tex\n")
cat("- Output/tex/logit_post_x_mfx_fixest_no_se.tex\n")
cat("- Output/tex/logit_triple_mfx_fixest_no_se.tex\n")
cat("\nLos efectos estan reportados en: ", scale_label, "\n", sep = "")
