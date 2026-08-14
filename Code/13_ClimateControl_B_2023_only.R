# ==============================================================================
# 11_ERA5_Drought_SPEI12_2023_only.R
#
# OBJETIVO
#   Estimar las mismas seis especificaciones del analisis principal, pero
#   controlando unicamente por la sequia preelectoral de 2023.
#
# DEFINICION DEL CONTROL
#   - PASO 2023: SPEI-6 de julio de 2023 (aprox. febrero-julio de 2023).
#   - Generales 2023: SPEI-6 de septiembre de 2023
#     (aprox. abril-septiembre de 2023).
#   - Para todas las elecciones de otros anios, el control vale cero.
#
# Esto equivale a incluir:
#   drought_spei12_pre_election_pct * 1(anio == 2023)
#
# REQUISITO
#   Haber corrido previamente:
#   10_ERA5_Drought_SPEI12_from_NC_Folder.R
#
# El script usa como input:
#   Output/Argentina/Drought_ERA5_SPEI12/
#   dip_nac_mun_with_era5_spei12.csv
# ==============================================================================


# ------------------------------------------------------------------------------
# 0. Paquetes
# ------------------------------------------------------------------------------

packages <- c(
  "dplyr",
  "tidyr",
  "stringr",
  "readr",
  "purrr",
  "fixest",
  "modelsummary",
  "tibble",
  "broom"
)

missing_packages <- packages[
  !vapply(packages, requireNamespace, logical(1), quietly = TRUE)
]

if (length(missing_packages) > 0) {
  install.packages(missing_packages)
}

invisible(
  lapply(packages, library, character.only = TRUE)
)

options(scipen = 999)


# ------------------------------------------------------------------------------
# 1. Paths
# ------------------------------------------------------------------------------

path_pili <- "C:/Users/pilih/Documents/Papers German/Valerie/Paper_nietos_arg"
setwd(path_pili)

archivo_panel_spei12 <- file.path(
  "Output",
  "Argentina",
  "Drought_ERA5_SPEI12",
  "dip_nac_mun_with_era5_spei12.csv"
)

carpeta_salida <- file.path(
  "Output",
  "Argentina",
  "Drought_ERA5_SPEI12_2023_only"
)

dir.create(
  carpeta_salida,
  recursive = TRUE,
  showWarnings = FALSE
)

if (!file.exists(archivo_panel_spei12)) {
  stop(
    paste0(
      "No se encontro el panel con SPEI-6:\n",
      archivo_panel_spei12,
      "\n\nPrimero corre 10_ERA5_Drought_SPEI12_from_NC_Folder.R."
    )
  )
}


# ------------------------------------------------------------------------------
# 2. Leer el panel y construir controles exclusivos de 2023
# ------------------------------------------------------------------------------

data_all <- readr::read_csv(
  archivo_panel_spei12,
  show_col_types = FALSE
) %>%
  mutate(
    mun_code = stringr::str_pad(
      as.character(mun_code),
      width = 6,
      side = "left",
      pad = "0"
    ),
    anio = as.integer(anio),
    tipo_eleccion = stringr::str_to_upper(
      stringr::str_trim(tipo_eleccion)
    ),
    post = as.integer(anio > 2021),

    # Indicador que activa el control solo en 2023.
    election_2023 = as.integer(anio == 2023),

    # Medida principal: porcentaje del area municipal con SPEI-6 < -1.5.
    drought_spei12_2023_pct_15 = dplyr::if_else(
      anio == 2023,
      drought_spei12_pre_election_pct_15,
      0
    ),

    # Robustez: porcentaje del area municipal con SPEI-6 < -2.
    drought_spei12_2023_pct_20 = dplyr::if_else(
      anio == 2023,
      drought_spei12_pre_election_pct_20,
      0
    ),

    # Robustez continua: SPEI-6 municipal promedio, activo solo en 2023.
    spei12_2023_mean = dplyr::if_else(
      anio == 2023,
      spei12_pre_election_mean,
      0
    )
  )

# Verificar que el panel mantiene la estructura esperada.
if (nrow(data_all) != 4680) {
  warning(
    paste0(
      "El panel tiene ",
      nrow(data_all),
      " observaciones, no 4680. Revisar antes de interpretar."
    )
  )
}

# El clima solo debe ser no cero en 2023.
if (any(data_all$anio != 2023 & data_all$drought_spei12_2023_pct_15 != 0,
        na.rm = TRUE)) {
  stop("El control de 2023 no quedo en cero fuera de 2023.")
}

# No puede haber datos faltantes en las elecciones de 2023.
missing_2023 <- data_all %>%
  filter(
    anio == 2023,
    is.na(drought_spei12_2023_pct_15)
  ) %>%
  distinct(mun_code, anio, tipo_eleccion)

readr::write_csv(
  missing_2023,
  file.path(carpeta_salida, "missing_climate_2023.csv")
)

if (nrow(missing_2023) > 0) {
  stop(
    paste0(
      "Hay ", nrow(missing_2023),
      " filas de 2023 sin clima. Revisar missing_climate_2023.csv."
    )
  )
}

# Diagnostico de la variable usada.
resumen_clima_2023 <- data_all %>%
  filter(anio == 2023) %>%
  group_by(tipo_eleccion) %>%
  summarise(
    municipios = n_distinct(mun_code),
    mean_pct_15 = mean(drought_spei12_2023_pct_15, na.rm = TRUE),
    sd_pct_15 = sd(drought_spei12_2023_pct_15, na.rm = TRUE),
    min_pct_15 = min(drought_spei12_2023_pct_15, na.rm = TRUE),
    max_pct_15 = max(drought_spei12_2023_pct_15, na.rm = TRUE),
    mean_pct_20 = mean(drought_spei12_2023_pct_20, na.rm = TRUE),
    mean_spei12 = mean(spei12_2023_mean, na.rm = TRUE),
    .groups = "drop"
  )

print(resumen_clima_2023, n = Inf)

readr::write_csv(
  resumen_clima_2023,
  file.path(carpeta_salida, "summary_drought_2023.csv")
)

# Guardar el panel con los nuevos controles.
readr::write_csv(
  data_all,
  file.path(carpeta_salida, "dip_nac_mun_with_spei12_2023_control.csv")
)

data_gen <- data_all %>%
  filter(tipo_eleccion == "GENERALES")

cat("\nObservaciones panel completo:", nrow(data_all), "\n")
cat("Municipios:", dplyr::n_distinct(data_all$mun_code), "\n")
cat("Observaciones de 2023:", sum(data_all$anio == 2023), "\n")


# ------------------------------------------------------------------------------
# 3. Funcion para estimar las seis especificaciones
# ------------------------------------------------------------------------------

estimar_seis_modelos <- function(control_var = NULL) {

  crear_formula <- function(outcome, lag_var = NULL, solo_generales = FALSE) {

    rhs <- c(
      "share_1936_1955:post",
      "share_1956_1978:post",
      lag_var,
      control_var
    )

    rhs <- rhs[!is.na(rhs) & nzchar(rhs)]

    efectos_fijos <- if (solo_generales) {
      "mun_code + anio"
    } else {
      "mun_code + anio + tipo_eleccion"
    }

    stats::as.formula(
      paste0(
        outcome,
        " ~ ",
        paste(rhs, collapse = " + "),
        " | ",
        efectos_fijos
      )
    )
  }

  b1 <- fixest::feols(
    crear_formula("porcentaje_blanco"),
    data = data_all,
    vcov = ~mun_code
  )

  b2 <- fixest::feols(
    crear_formula(
      "porcentaje_blanco",
      solo_generales = TRUE
    ),
    data = data_gen,
    vcov = ~mun_code
  )

  b3 <- fixest::feols(
    crear_formula(
      "porcentaje_blanco",
      lag_var = "porcentaje_blanco_l"
    ),
    data = data_all,
    vcov = ~mun_code
  )

  t1 <- fixest::feols(
    crear_formula("participacion"),
    data = data_all,
    vcov = ~mun_code
  )

  t2 <- fixest::feols(
    crear_formula(
      "participacion",
      solo_generales = TRUE
    ),
    data = data_gen,
    vcov = ~mun_code
  )

  t3 <- fixest::feols(
    crear_formula(
      "participacion",
      lag_var = "participacion_l"
    ),
    data = data_all,
    vcov = ~mun_code
  )

  list(
    "(1)" = b1,
    "(2)" = b2,
    "(3)" = b3,
    "(4)" = t1,
    "(5)" = t2,
    "(6)" = t3
  )
}


# ------------------------------------------------------------------------------
# 4. Estimar baseline y las tres versiones del control de 2023
# ------------------------------------------------------------------------------

models_baseline <- estimar_seis_modelos()

models_2023_15 <- estimar_seis_modelos(
  "drought_spei12_2023_pct_15"
)

models_2023_20 <- estimar_seis_modelos(
  "drought_spei12_2023_pct_20"
)

models_2023_continuo <- estimar_seis_modelos(
  "spei12_2023_mean"
)


# ------------------------------------------------------------------------------
# 5. Test de igualdad entre las dos ventanas espanolas
# ------------------------------------------------------------------------------

get_p_equal <- function(model) {

  betas <- stats::coef(model)
  V <- stats::vcov(model)

  name1 <- grep("1936_1955", names(betas), value = TRUE)
  name2 <- grep("1956_1978", names(betas), value = TRUE)

  if (length(name1) != 1 || length(name2) != 1) {
    return(NA_real_)
  }

  difference <- betas[name1] - betas[name2]

  se_difference <- sqrt(
    V[name1, name1] +
      V[name2, name2] -
      2 * V[name1, name2]
  )

  t_stat <- difference / se_difference
  2 * stats::pnorm(-abs(t_stat))
}


# ------------------------------------------------------------------------------
# 6. Funcion para exportar cada tabla LaTeX
# ------------------------------------------------------------------------------

exportar_tabla <- function(
    models,
    control_var,
    control_label,
    caption,
    nota_control,
    nombre_archivo
) {

  p_values <- vapply(models, get_p_equal, numeric(1))

  p_strings <- ifelse(
    is.na(p_values),
    "",
    sprintf("%.3f", p_values)
  )

  coef_map <- c(
    "share_1936_1955:post" =
      "Spanish share 1936-1955$\\times$Post",
    "post:share_1936_1955" =
      "Spanish share 1936-1955$\\times$Post",
    "share_1956_1978:post" =
      "Spanish share 1956-1978$\\times$Post",
    "post:share_1956_1978" =
      "Spanish share 1956-1978$\\times$Post",
    "porcentaje_blanco_l" =
      "Lagged share of blank votes",
    "participacion_l" =
      "Lagged voter turnout"
  )

  coef_map <- c(
    coef_map,
    stats::setNames(control_label, control_var)
  )

  gof_map <- tibble::tribble(
    ~raw,        ~clean,         ~fmt,
    "nobs",      "Observations", 0,
    "r.squared", "R$^2$",        3
  )

  add_rows <- tibble::tibble(
    term = c(
      "$p$-value ($\\beta_{36{-}55} = \\beta_{56{-}78}$)",
      "Municipality FE",
      "Time FE",
      "Election type FE",
      "General elections only",
      "2023 drought control"
    ),
    m1 = c(p_strings[1], "Yes", "Yes", "Yes", "No",  "Yes"),
    m2 = c(p_strings[2], "Yes", "Yes", "No",  "Yes", "Yes"),
    m3 = c(p_strings[3], "Yes", "Yes", "Yes", "No",  "Yes"),
    m4 = c(p_strings[4], "Yes", "Yes", "Yes", "No",  "Yes"),
    m5 = c(p_strings[5], "Yes", "Yes", "No",  "Yes", "Yes"),
    m6 = c(p_strings[6], "Yes", "Yes", "Yes", "No",  "Yes")
  )

  names(add_rows) <- c(
    "term", "(1)", "(2)", "(3)", "(4)", "(5)", "(6)"
  )

  tex <- modelsummary::modelsummary(
    models,
    output = "latex",
    coef_map = coef_map,
    gof_map = gof_map,
    estimate = "{estimate}{stars}",
    statistic = "({std.error})",
    stars = c("*" = 0.10, "**" = 0.05, "***" = 0.01),
    add_rows = add_rows,
    escape = FALSE
  )

  lines <- strsplit(as.character(tex), "\n")[[1]]

  beg_table <- grep("\\\\begin\\{table\\}", lines)

  if (length(beg_table) >= 1) {
    lines[beg_table[1]] <- "\\begin{table}[!h]"
    header <- c(
      paste0("\\caption{", caption, "}"),
      "\\renewcommand{\\arraystretch}{1.25}",
      "\\setlength{\\tabcolsep}{6pt}"
    )
    lines <- c(
      lines[1:beg_table[1]],
      header,
      lines[(beg_table[1] + 1):length(lines)]
    )
  }

  top_idx <- grep("\\\\toprule", lines)

  if (length(top_idx) >= 1) {
    multicol <- paste0(
      " & \\multicolumn{3}{c}{Share of blank votes}",
      " & \\multicolumn{3}{c}{Voter turnout} \\\\"
    )
    cmidrule <- "\\cmidrule(l){2-4} \\cmidrule(l){5-7}"
    lines <- c(
      lines[1:top_idx[1]],
      multicol,
      cmidrule,
      lines[(top_idx[1] + 1):length(lines)]
    )
  }

  lines <- gsub("\\\\toprule", "\\\\hline", lines)
  lines <- gsub("\\\\bottomrule", "\\\\hline", lines)

  midrules <- grep("\\\\midrule", lines)

  if (length(midrules) >= 1) {
    lines[midrules[1]] <- gsub(
      "\\\\midrule",
      "\\\\hline",
      lines[midrules[1]]
    )
  }

  if (length(midrules) >= 2) {
    lines[midrules[-1]] <- ""
  }

  observations_line <- grep("^Observations", lines)

  if (length(observations_line) >= 1) {
    lines <- c(
      lines[1:(observations_line[1] - 1)],
      "\\addlinespace",
      lines[observations_line[1]:length(lines)]
    )
  }

  end_tabular <- grep("\\\\end\\{tabular\\}", lines)

  if (length(end_tabular) >= 1) {
    note <- c(
      "\\vspace{0.3em}",
      "\\captionsetup{justification=justified, singlelinecheck=false}",
      paste0(
        "\\caption*{\\footnotesize Notes: ",
        "The dependent variable in columns (1)--(3) is the share of blank votes over total voters. ",
        "In columns (4)--(6), the dependent variable is voter turnout. ",
        nota_control,
        " The climate control is equal to zero in all election years other than 2023. ",
        "Columns (2) and (5) use general elections only. ",
        "Columns (3) and (6) include the lagged dependent variable, calculated within municipality and election type. ",
        "All specifications include municipality and year fixed effects; pooled specifications also include election-type fixed effects. ",
        "Standard errors clustered at the municipality level in parentheses (312 clusters). ",
        "* $p<0.10$, ** $p<0.05$, *** $p<0.01$.}"
      )
    )

    lines <- c(
      lines[1:end_tabular[1]],
      note,
      lines[(end_tabular[1] + 1):length(lines)]
    )
  }

  archivo_salida <- file.path(carpeta_salida, nombre_archivo)
  writeLines(lines, archivo_salida)
  cat("\nTabla guardada en:\n", archivo_salida, "\n")
}


# ------------------------------------------------------------------------------
# 7. Exportar las tres tablas de 2023
# ------------------------------------------------------------------------------

exportar_tabla(
  models = models_2023_15,
  control_var = "drought_spei12_2023_pct_15",
  control_label = "2023 pre-election drought exposure",
  caption = paste0(
    "Effects on Blank Votes and Voter Turnout, ",
    "Controlling for 2023 Pre-Election Drought"
  ),
  nota_control = paste0(
    "For 2023 observations, drought exposure is the percentage of municipal area ",
    "with SPEI-6 below -1.5 in the calendar month immediately preceding each election. ",
    "The July 2023 SPEI-6 is used for the PASO and the September 2023 SPEI-6 for the general election. ",
    "Each SPEI-6 value summarizes the six-month climatic balance ending in that month."
  ),
  nombre_archivo =
    "att_blankvotes_turnout_drought_era5_spei12_2023_only.tex"
)

exportar_tabla(
  models = models_2023_20,
  control_var = "drought_spei12_2023_pct_20",
  control_label = "2023 severe drought exposure",
  caption = paste0(
    "Effects on Blank Votes and Voter Turnout, ",
    "Controlling for Severe 2023 Pre-Election Drought"
  ),
  nota_control = paste0(
    "For 2023 observations, drought exposure is the percentage of municipal area ",
    "with SPEI-6 below -2 in the calendar month immediately preceding each election. ",
    "The July 2023 SPEI-6 is used for the PASO and the September 2023 SPEI-6 for the general election."
  ),
  nombre_archivo =
    "att_blankvotes_turnout_drought_era5_spei12_2023_only_threshold20.tex"
)

exportar_tabla(
  models = models_2023_continuo,
  control_var = "spei12_2023_mean",
  control_label = "Mean 2023 pre-election SPEI-6",
  caption = paste0(
    "Effects on Blank Votes and Voter Turnout, ",
    "Controlling for Continuous 2023 Pre-Election SPEI-6"
  ),
  nota_control = paste0(
    "For 2023 observations, the climate control is the area-weighted municipal mean ",
    "of SPEI-6 in the calendar month immediately preceding each election. ",
    "Lower values indicate drier conditions."
  ),
  nombre_archivo =
    "att_blankvotes_turnout_drought_era5_spei12_2023_only_continuous.tex"
)


# ------------------------------------------------------------------------------
# 8. Comparar los coeficientes principales
# ------------------------------------------------------------------------------

extraer_coeficientes_principales <- function(models, version) {
  purrr::imap_dfr(
    models,
    function(modelo, numero_modelo) {
      broom::tidy(modelo) %>%
        filter(
          stringr::str_detect(
            term,
            "1936_1955|1956_1978"
          )
        ) %>%
        transmute(
          model = numero_modelo,
          version = version,
          term,
          estimate,
          std.error,
          p.value
        )
    }
  )
}

comparison_2023 <- dplyr::bind_rows(
  extraer_coeficientes_principales(
    models_baseline,
    "Baseline"
  ),
  extraer_coeficientes_principales(
    models_2023_15,
    "2023 SPEI-6 threshold -1.5"
  ),
  extraer_coeficientes_principales(
    models_2023_20,
    "2023 SPEI-6 threshold -2"
  ),
  extraer_coeficientes_principales(
    models_2023_continuo,
    "2023 continuous SPEI-6"
  )
) %>%
  arrange(model, term, version)

readr::write_csv(
  comparison_2023,
  file.path(
    carpeta_salida,
    "key_coefficient_comparison_era5_spei12_2023_only.csv"
  )
)

# Comparacion adicional en formato ancho, con cambio porcentual contra baseline.
comparison_2023_wide <- comparison_2023 %>%
  select(model, term, version, estimate, std.error, p.value) %>%
  tidyr::pivot_wider(
    names_from = version,
    values_from = c(estimate, std.error, p.value)
  )

readr::write_csv(
  comparison_2023_wide,
  file.path(
    carpeta_salida,
    "key_coefficient_comparison_era5_spei12_2023_only_wide.csv"
  )
)

cat(
  "\nProceso terminado. Resultados guardados en:\n",
  normalizePath(
    carpeta_salida,
    winslash = "/",
    mustWork = FALSE
  ),
  "\n"
)
