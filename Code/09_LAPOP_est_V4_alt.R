# ---------------------------------------------------------------------------- #
#                                 CÓDIGO 9
#          Estimaciones LAPOP: quiénes tienen más probabilidad de migrar
#          Versión con controles municipales pre-2023
# ---------------------------------------------------------------------------- #

library(dplyr)
library(tidyr)
library(readr)
library(stringr)
library(purrr)
library(fixest)
library(modelsummary)
library(openxlsx)

# ---------------------------------------------------------------------------- #
# 0. Paths y carga de datos
# ---------------------------------------------------------------------------- #

path_pili <- "C:\\Users\\pilih\\Documents\\Papers German\\Valerie\\Paper_nietos_arg"
setwd(path_pili)

dir.create("Output", showWarnings = FALSE)

lapop <- read_csv("Data Out/lapop_data_merge.csv", show_col_types = FALSE)

# ---------------------------------------------------------------------------- #
# 1. Preparar LAPOP
# ---------------------------------------------------------------------------- #
# Importante:
# Mantengo year_num como numérico antes de convertir year a factor.

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

run_lpm <- function(rhs,
                    data = lapop,
                    y = "intencion_migrar",
                    fe = "year + mun_code",
                    cluster = "mun_code") {
  
  fml <- as.formula(paste0(
    y, " ~ ", rhs, " | ", fe
  ))
  
  feols(
    fml,
    data = data,
    weights = ~ wt,
    cluster = as.formula(paste0("~", cluster))
  )
}

run_logit <- function(rhs,
                      data = lapop,
                      y = "intencion_migrar",
                      fe = "year + mun_code",
                      cluster = "mun_code") {
  
  fml <- as.formula(paste0(
    y, " ~ ", rhs, " | ", fe
  ))
  
  feglm(
    fml,
    data = data,
    family = binomial(link = "logit"),
    weights = ~ wt,
    cluster = as.formula(paste0("~", cluster))
  )
}

# ---------------------------------------------------------------------------- #
# 3. Chequeo variables clave
# ---------------------------------------------------------------------------- #

vars_needed <- c(
  "intencion_migrar",
  "edad",
  "hombre",
  "rural",
  "nivel_educ7",
  "sit_lab_mig",
  "desempleado",
  "ocupado",
  "estudiante",
  "en_pareja",
  "etnia_minoritaria",
  "share_1936_1955",
  "share_1956_1978",
  "wt",
  "mun_code",
  "year_num",
  "post"
)

missing_vars <- setdiff(vars_needed, names(lapop))

if (length(missing_vars) > 0) {
  stop(paste(
    "Faltan estas variables en lapop:",
    paste(missing_vars, collapse = ", ")
  ))
}

# ---------------------------------------------------------------------------- #
# 4. Crear controles municipales pre-2023
# ---------------------------------------------------------------------------- #
# Hay dos versiones:
#
# A. Principal:
#    Promedio municipal usando todos los años anteriores a 2023.
#
# B. Robustez:
#    Promedio municipal usando el último año pre-2023 disponible por municipio.
#
# Estos controles entran como post:mun_pre_* porque con FE de municipio
# los niveles municipales fijos se absorben.

pre_period_all <- lapop %>%
  filter(year_num < 2023)

if (nrow(pre_period_all) == 0) {
  stop("No hay observaciones pre-2023 para calcular controles municipales.")
}

# ------------------------- #
# A. Todos los años pre-2023
# ------------------------- #

mun_pre_all_controls <- pre_period_all %>%
  group_by(mun_code) %>%
  summarise(
    mun_pre_all_mean_edad = wmean(edad, wt),
    mun_pre_all_share_hombre = wmean(hombre, wt),
    mun_pre_all_share_rural = wmean(rural, wt),
    mun_pre_all_share_desempleado = wmean(desempleado, wt),
    mun_pre_all_share_ocupado = wmean(ocupado, wt),
    mun_pre_all_share_estudiante = wmean(estudiante, wt),
    mun_pre_all_share_en_pareja = wmean(en_pareja, wt),
    mun_pre_all_share_etnia_minoritaria = wmean(etnia_minoritaria, wt),
    
    mun_pre_all_share_secundaria_completa_o_mas =
      if ("secundaria_completa_o_mas" %in% names(pre_period_all)) {
        wmean(secundaria_completa_o_mas, wt)
      } else {
        NA_real_
      },
    
    mun_pre_all_share_superior_incompleta_o_mas =
      if ("superior_incompleta_o_mas" %in% names(pre_period_all)) {
        wmean(superior_incompleta_o_mas, wt)
      } else {
        NA_real_
      },
    
    mun_pre_all_share_superior_completa =
      if ("superior_completa" %in% names(pre_period_all)) {
        wmean(superior_completa, wt)
      } else {
        NA_real_
      },
    
    mun_pre_all_share_blanco =
      if ("blanco" %in% names(pre_period_all)) {
        wmean(blanco, wt)
      } else {
        NA_real_
      },
    
    .groups = "drop"
  )

# ------------------------- #
# B. Último año pre-2023 disponible por municipio
# ------------------------- #

pre_period_last_by_mun <- pre_period_all %>%
  group_by(mun_code) %>%
  filter(year_num == max(year_num, na.rm = TRUE)) %>%
  ungroup()

mun_pre_last_controls <- pre_period_last_by_mun %>%
  group_by(mun_code) %>%
  summarise(
    mun_pre_last_year_used = max(year_num, na.rm = TRUE),
    mun_pre_last_mean_edad = wmean(edad, wt),
    mun_pre_last_share_hombre = wmean(hombre, wt),
    mun_pre_last_share_rural = wmean(rural, wt),
    mun_pre_last_share_desempleado = wmean(desempleado, wt),
    mun_pre_last_share_ocupado = wmean(ocupado, wt),
    mun_pre_last_share_estudiante = wmean(estudiante, wt),
    mun_pre_last_share_en_pareja = wmean(en_pareja, wt),
    mun_pre_last_share_etnia_minoritaria = wmean(etnia_minoritaria, wt),
    
    mun_pre_last_share_secundaria_completa_o_mas =
      if ("secundaria_completa_o_mas" %in% names(pre_period_last_by_mun)) {
        wmean(secundaria_completa_o_mas, wt)
      } else {
        NA_real_
      },
    
    mun_pre_last_share_superior_incompleta_o_mas =
      if ("superior_incompleta_o_mas" %in% names(pre_period_last_by_mun)) {
        wmean(superior_incompleta_o_mas, wt)
      } else {
        NA_real_
      },
    
    mun_pre_last_share_superior_completa =
      if ("superior_completa" %in% names(pre_period_last_by_mun)) {
        wmean(superior_completa, wt)
      } else {
        NA_real_
      },
    
    mun_pre_last_share_blanco =
      if ("blanco" %in% names(pre_period_last_by_mun)) {
        wmean(blanco, wt)
      } else {
        NA_real_
      },
    
    .groups = "drop"
  )

lapop <- lapop %>%
  left_join(mun_pre_all_controls, by = "mun_code") %>%
  left_join(mun_pre_last_controls, by = "mun_code")

# ---------------------------------------------------------------------------- #
# 5. Convertir variables categóricas y FE
# ---------------------------------------------------------------------------- #

if ("etnia_arg" %in% names(lapop)) {
  lapop <- lapop %>%
    mutate(etnia_arg = as.factor(etnia_arg))
}

if ("estado_civil" %in% names(lapop)) {
  lapop <- lapop %>%
    mutate(estado_civil = as.factor(estado_civil))
}

lapop <- lapop %>%
  mutate(
    nivel_educ7 = as.factor(nivel_educ7),
    sit_lab_mig = as.factor(sit_lab_mig),
    mun_code = as.factor(mun_code),
    year = as.factor(year_num)
  )

# ---------------------------------------------------------------------------- #
# 6. Chequeos básicos
# ---------------------------------------------------------------------------- #

cat("Observaciones:", nrow(lapop), "\n")
cat("Municipios:", n_distinct(lapop$mun_code), "\n")
cat("Años:", paste(sort(unique(lapop$year)), collapse = ", "), "\n")
cat("Media intención migrar:", mean(lapop$intencion_migrar, na.rm = TRUE), "\n")

table(lapop$intencion_migrar, useNA = "ifany")
table(lapop$post, lapop$year, useNA = "ifany")
table(lapop$year, lapop$nivel_educ7, useNA = "ifany")
table(lapop$year, lapop$sit_lab_mig, useNA = "ifany")
table(lapop$year, lapop$rural, useNA = "ifany")
table(lapop$year, lapop$en_pareja, useNA = "ifany")

if ("etnia_arg" %in% names(lapop)) {
  table(lapop$year, lapop$etnia_arg, useNA = "ifany")
}

# ---------------------------------------------------------------------------- #
# 7. Bloques de controles
# ---------------------------------------------------------------------------- #

# Controles individuales predeterminados.
# Estos son los más defendibles como controles contemporáneos.

if ("blanco" %in% names(lapop)) {
  controls_predetermined_full <- paste(
    "edad",
    "hombre",
    "blanco",
    sep = " + "
  )
} else {
  controls_predetermined_full <- paste(
    "edad",
    "hombre",
    "etnia_minoritaria",
    sep = " + "
  )
}

basic_predetermined_vars <- c(
  "edad",
  "hombre",
  if ("blanco" %in% names(lapop)) "blanco" else "etnia_minoritaria"
)

basic_predetermined_vars <- intersect(basic_predetermined_vars, names(lapop))

# Controles municipales pre-2023: todos los años previos.
# Especificación principal.

mun_pre_all_control_vars <- c(
  "mun_pre_all_mean_edad",
  "mun_pre_all_share_hombre",
  "mun_pre_all_share_rural",
  "mun_pre_all_share_desempleado",
  "mun_pre_all_share_ocupado",
  "mun_pre_all_share_estudiante",
  "mun_pre_all_share_en_pareja",
  "mun_pre_all_share_etnia_minoritaria",
  "mun_pre_all_share_secundaria_completa_o_mas",
  "mun_pre_all_share_superior_incompleta_o_mas",
  "mun_pre_all_share_superior_completa"
)

if ("blanco" %in% names(lapop)) {
  mun_pre_all_control_vars <- c(
    mun_pre_all_control_vars,
    "mun_pre_all_share_blanco"
  )
}

mun_pre_all_control_vars <- intersect(mun_pre_all_control_vars, names(lapop))

controls_mun_pre_all <- paste(
  paste0("post:", mun_pre_all_control_vars),
  collapse = " + "
)

# Controles municipales pre-2023: último año disponible por municipio.
# Robustez.

mun_pre_last_control_vars <- c(
  "mun_pre_last_mean_edad",
  "mun_pre_last_share_hombre",
  "mun_pre_last_share_rural",
  "mun_pre_last_share_desempleado",
  "mun_pre_last_share_ocupado",
  "mun_pre_last_share_estudiante",
  "mun_pre_last_share_en_pareja",
  "mun_pre_last_share_etnia_minoritaria",
  "mun_pre_last_share_secundaria_completa_o_mas",
  "mun_pre_last_share_superior_incompleta_o_mas",
  "mun_pre_last_share_superior_completa"
)

if ("blanco" %in% names(lapop)) {
  mun_pre_last_control_vars <- c(
    mun_pre_last_control_vars,
    "mun_pre_last_share_blanco"
  )
}

mun_pre_last_control_vars <- intersect(mun_pre_last_control_vars, names(lapop))

controls_mun_pre_last <- paste(
  paste0("post:", mun_pre_last_control_vars),
  collapse = " + "
)

# Controles contemporáneos.
# Solo robustez, no main specification.

controls_contemporaneous_robust <- paste(
  "edad",
  "hombre",
  "i(nivel_educ7)",
  "rural",
  "i(sit_lab_mig)",
  "en_pareja",
  sep = " + "
)

controls_contemporaneous_full_robust <- paste(
  controls_contemporaneous_robust,
  "etnia_minoritaria",
  sep = " + "
)

# ---------------------------------------------------------------------------- #
# 8. Descriptivos
# ---------------------------------------------------------------------------- #

desc_mig <- lapop %>%
  group_by(intencion_migrar) %>%
  summarise(
    n = n(),
    edad_prom = wmean(edad, wt),
    share_hombre = wmean(hombre, wt),
    share_rural = wmean(rural, wt),
    share_desempleado = wmean(desempleado, wt),
    share_ocupado = wmean(ocupado, wt),
    share_estudiante = wmean(estudiante, wt),
    share_en_pareja = wmean(en_pareja, wt),
    share_etnia_minoritaria = wmean(etnia_minoritaria, wt),
    share_secundaria_completa_o_mas =
      if ("secundaria_completa_o_mas" %in% names(lapop)) {
        wmean(secundaria_completa_o_mas, wt)
      } else {
        NA_real_
      },
    share_superior_incompleta_o_mas =
      if ("superior_incompleta_o_mas" %in% names(lapop)) {
        wmean(superior_incompleta_o_mas, wt)
      } else {
        NA_real_
      },
    share_superior_completa =
      if ("superior_completa" %in% names(lapop)) {
        wmean(superior_completa, wt)
      } else {
        NA_real_
      },
    .groups = "drop"
  )

desc_year <- lapop %>%
  group_by(year, post) %>%
  summarise(
    n = n(),
    mean_mig = wmean(intencion_migrar, wt),
    mean_edad = wmean(edad, wt),
    share_hombre = wmean(hombre, wt),
    share_rural = wmean(rural, wt),
    share_desempleado = wmean(desempleado, wt),
    share_ocupado = wmean(ocupado, wt),
    share_estudiante = wmean(estudiante, wt),
    share_en_pareja = wmean(en_pareja, wt),
    share_etnia_minoritaria = wmean(etnia_minoritaria, wt),
    share_secundaria_completa_o_mas =
      if ("secundaria_completa_o_mas" %in% names(lapop)) {
        wmean(secundaria_completa_o_mas, wt)
      } else {
        NA_real_
      },
    share_superior_incompleta_o_mas =
      if ("superior_incompleta_o_mas" %in% names(lapop)) {
        wmean(superior_incompleta_o_mas, wt)
      } else {
        NA_real_
      },
    share_superior_completa =
      if ("superior_completa" %in% names(lapop)) {
        wmean(superior_completa, wt)
      } else {
        NA_real_
      },
    .groups = "drop"
  )

write.xlsx(desc_mig, "Output/lapop_desc_migrants_weighted.xlsx", overwrite = TRUE)
write.xlsx(desc_year, "Output/lapop_desc_by_year_weighted.xlsx", overwrite = TRUE)

# ---------------------------------------------------------------------------- #
# 9. LPM: selección individual promedio
# ---------------------------------------------------------------------------- #
# Pregunta descriptiva:
# ¿Quiénes tienen mayor probabilidad de querer migrar?

m_lpm_sel_1 <- run_lpm(
  rhs = "edad + hombre + rural"
)

m_lpm_sel_2 <- run_lpm(
  rhs = "edad + hombre + rural + i(nivel_educ7)"
)

m_lpm_sel_3 <- run_lpm(
  rhs = "edad + hombre + rural + i(nivel_educ7) + i(sit_lab_mig) + en_pareja"
)

m_lpm_sel_4 <- run_lpm(
  rhs = "edad + hombre + rural + i(nivel_educ7) + i(sit_lab_mig) + en_pareja + etnia_minoritaria"
)

models_lpm_selection <- list(
  "Basic" = m_lpm_sel_1,
  "Education" = m_lpm_sel_2,
  "Socioeconomic" = m_lpm_sel_3,
  "Full" = m_lpm_sel_4
)

# ---------------------------------------------------------------------------- #
# 10. LPM: cambios en selección en 2023
# ---------------------------------------------------------------------------- #
# Main:
# Solo variables predeterminadas.
#
# Robustez:
# Variables que pueden variar en el tiempo.

post_selection_vars_main <- c(
  "edad",
  "hombre",
  "blanco",
  "etnia_minoritaria"
)

post_selection_vars_main <- intersect(post_selection_vars_main, names(lapop))

post_selection_vars_robust <- c(
  "rural",
  "superior_incompleta_o_mas",
  "desempleado",
  "ocupado",
  "estudiante",
  "en_pareja",
  "interes_pol_mucho",
  "voto_blanco_nulo"
)

post_selection_vars_robust <- intersect(post_selection_vars_robust, names(lapop))

run_post_selection_lpm <- function(x,
                                   data = lapop,
                                   control_vars = basic_predetermined_vars) {
  
  controls_x <- setdiff(control_vars, x)
  
  controls_rhs <- if (length(controls_x) > 0) {
    paste(controls_x, collapse = " + ")
  } else {
    NULL
  }
  
  rhs_main <- paste0(
    "post:", x,
    " + ",
    x
  )
  
  rhs <- if (!is.null(controls_rhs)) {
    paste(rhs_main, controls_rhs, sep = " + ")
  } else {
    rhs_main
  }
  
  run_lpm(rhs = rhs, data = data)
}

models_lpm_post_selection_main <- map(
  post_selection_vars_main,
  ~ run_post_selection_lpm(.x)
)

names(models_lpm_post_selection_main) <- post_selection_vars_main

models_lpm_post_selection_robust <- map(
  post_selection_vars_robust,
  ~ run_post_selection_lpm(.x)
)

names(models_lpm_post_selection_robust) <- post_selection_vars_robust

# ---------------------------------------------------------------------------- #
# 11. LPM: exposición española municipal
# ---------------------------------------------------------------------------- #

# 11.1 Asociación cross-sectional
# Sin FE de municipio porque los shares municipales se absorberían.

m_lpm_exp_cross <- run_lpm(
  rhs = "share_1936_1955 + share_1956_1978",
  fe = "year"
)

m_lpm_exp_cross_predetermined <- run_lpm(
  rhs = paste0(
    "share_1936_1955 + share_1956_1978 + ",
    controls_predetermined_full
  ),
  fe = "year"
)

m_lpm_exp_cross_robust_contemporaneous <- run_lpm(
  rhs = paste0(
    "share_1936_1955 + share_1956_1978 + ",
    controls_contemporaneous_robust
  ),
  fe = "year"
)

# 11.2 Cambio diferencial post-2023 por exposición municipal
# Coeficientes de interés:
# post:share_1936_1955
# post:share_1956_1978

m_lpm_exp_did <- run_lpm(
  rhs = "post:share_1936_1955 + post:share_1956_1978"
)

m_lpm_exp_did_predetermined <- run_lpm(
  rhs = paste0(
    "post:share_1936_1955 + post:share_1956_1978 + ",
    controls_predetermined_full
  )
)

m_lpm_exp_did_mun_pre_all <- run_lpm(
  rhs = paste0(
    "post:share_1936_1955 + post:share_1956_1978 + ",
    controls_mun_pre_all
  )
)

m_lpm_exp_did_predetermined_mun_pre_all <- run_lpm(
  rhs = paste0(
    "post:share_1936_1955 + post:share_1956_1978 + ",
    controls_predetermined_full,
    " + ",
    controls_mun_pre_all
  )
)

# Robustez: último año pre-2023 disponible por municipio.

m_lpm_exp_did_mun_pre_last <- run_lpm(
  rhs = paste0(
    "post:share_1936_1955 + post:share_1956_1978 + ",
    controls_mun_pre_last
  )
)

m_lpm_exp_did_predetermined_mun_pre_last <- run_lpm(
  rhs = paste0(
    "post:share_1936_1955 + post:share_1956_1978 + ",
    controls_predetermined_full,
    " + ",
    controls_mun_pre_last
  )
)

# Robustez: controles contemporáneos.

m_lpm_exp_did_robust_contemporaneous <- run_lpm(
  rhs = paste0(
    "post:share_1936_1955 + post:share_1956_1978 + ",
    controls_contemporaneous_robust
  )
)

m_lpm_exp_did_robust_contemporaneous_full <- run_lpm(
  rhs = paste0(
    "post:share_1936_1955 + post:share_1956_1978 + ",
    controls_contemporaneous_full_robust
  )
)

models_lpm_exposure <- list(
  "Exposure, year FE" = m_lpm_exp_cross,
  "Exposure + predetermined, year FE" = m_lpm_exp_cross_predetermined,
  "Exposure + contemporary controls, year FE" = m_lpm_exp_cross_robust_contemporaneous,
  "Post x exposure" = m_lpm_exp_did,
  "Post x exposure + predetermined" = m_lpm_exp_did_predetermined,
  "Post x exposure + municipal pre controls, all pre years" = m_lpm_exp_did_mun_pre_all,
  "Post x exposure + predetermined + municipal pre controls, all pre years" =
    m_lpm_exp_did_predetermined_mun_pre_all,
  "Post x exposure + municipal pre controls, last pre year" =
    m_lpm_exp_did_mun_pre_last,
  "Post x exposure + predetermined + municipal pre controls, last pre year" =
    m_lpm_exp_did_predetermined_mun_pre_last,
  "Post x exposure + contemporary controls" =
    m_lpm_exp_did_robust_contemporaneous,
  "Post x exposure + full contemporary controls" =
    m_lpm_exp_did_robust_contemporaneous_full
)

# ---------------------------------------------------------------------------- #
# 12. LPM: política individual
# ---------------------------------------------------------------------------- #
# Estas variables pueden estar afectadas por el contexto político.
# Las interpreto como descriptivas, no como controles principales.

models_lpm_politics <- list()

if ("izq_der" %in% names(lapop)) {
  models_lpm_politics[["Ideology"]] <- run_lpm(
    rhs = paste0("izq_der + ", controls_predetermined_full)
  )
}

if ("identifica_partido" %in% names(lapop)) {
  models_lpm_politics[["Party ID"]] <- run_lpm(
    rhs = paste0("identifica_partido + ", controls_predetermined_full)
  )
}

if ("voto_anterior" %in% names(lapop)) {
  models_lpm_politics[["Previous vote"]] <- run_lpm(
    rhs = paste0("voto_anterior + ", controls_predetermined_full)
  )
}

if ("interes_pol_mucho" %in% names(lapop)) {
  models_lpm_politics[["High political interest"]] <- run_lpm(
    rhs = paste0("interes_pol_mucho + ", controls_predetermined_full)
  )
}

if ("voto_blanco_nulo" %in% names(lapop)) {
  models_lpm_politics[["Blank/null vote"]] <- run_lpm(
    rhs = paste0("voto_blanco_nulo + ", controls_predetermined_full)
  )
}

# ---------------------------------------------------------------------------- #
# 13. LOGIT: selección individual promedio
# ---------------------------------------------------------------------------- #

m_logit_sel_1 <- run_logit(
  rhs = "edad + hombre + rural"
)

m_logit_sel_2 <- run_logit(
  rhs = "edad + hombre + rural + i(nivel_educ7)"
)

m_logit_sel_3 <- run_logit(
  rhs = "edad + hombre + rural + i(nivel_educ7) + i(sit_lab_mig) + en_pareja"
)

m_logit_sel_4 <- run_logit(
  rhs = "edad + hombre + rural + i(nivel_educ7) + i(sit_lab_mig) + en_pareja + etnia_minoritaria"
)

models_logit_selection <- list(
  "Basic" = m_logit_sel_1,
  "Education" = m_logit_sel_2,
  "Socioeconomic" = m_logit_sel_3,
  "Full" = m_logit_sel_4
)

# ---------------------------------------------------------------------------- #
# 14. LOGIT: cambios en selección en 2023
# ---------------------------------------------------------------------------- #

run_post_selection_logit <- function(x,
                                     data = lapop,
                                     control_vars = basic_predetermined_vars) {
  
  controls_x <- setdiff(control_vars, x)
  
  controls_rhs <- if (length(controls_x) > 0) {
    paste(controls_x, collapse = " + ")
  } else {
    NULL
  }
  
  rhs_main <- paste0(
    "post:", x,
    " + ",
    x
  )
  
  rhs <- if (!is.null(controls_rhs)) {
    paste(rhs_main, controls_rhs, sep = " + ")
  } else {
    rhs_main
  }
  
  run_logit(rhs = rhs, data = data)
}

models_logit_post_selection_main <- map(
  post_selection_vars_main,
  ~ run_post_selection_logit(.x)
)

names(models_logit_post_selection_main) <- post_selection_vars_main

models_logit_post_selection_robust <- map(
  post_selection_vars_robust,
  ~ run_post_selection_logit(.x)
)

names(models_logit_post_selection_robust) <- post_selection_vars_robust

# ---------------------------------------------------------------------------- #
# 15. LOGIT: exposición española municipal
# ---------------------------------------------------------------------------- #

m_logit_exp_cross <- run_logit(
  rhs = "share_1936_1955 + share_1956_1978",
  fe = "year"
)

m_logit_exp_cross_predetermined <- run_logit(
  rhs = paste0(
    "share_1936_1955 + share_1956_1978 + ",
    controls_predetermined_full
  ),
  fe = "year"
)

m_logit_exp_cross_robust_contemporaneous <- run_logit(
  rhs = paste0(
    "share_1936_1955 + share_1956_1978 + ",
    controls_contemporaneous_robust
  ),
  fe = "year"
)

m_logit_exp_did <- run_logit(
  rhs = "post:share_1936_1955 + post:share_1956_1978"
)

m_logit_exp_did_predetermined <- run_logit(
  rhs = paste0(
    "post:share_1936_1955 + post:share_1956_1978 + ",
    controls_predetermined_full
  )
)

m_logit_exp_did_mun_pre_all <- run_logit(
  rhs = paste0(
    "post:share_1936_1955 + post:share_1956_1978 + ",
    controls_mun_pre_all
  )
)

m_logit_exp_did_predetermined_mun_pre_all <- run_logit(
  rhs = paste0(
    "post:share_1936_1955 + post:share_1956_1978 + ",
    controls_predetermined_full,
    " + ",
    controls_mun_pre_all
  )
)

m_logit_exp_did_mun_pre_last <- run_logit(
  rhs = paste0(
    "post:share_1936_1955 + post:share_1956_1978 + ",
    controls_mun_pre_last
  )
)

m_logit_exp_did_predetermined_mun_pre_last <- run_logit(
  rhs = paste0(
    "post:share_1936_1955 + post:share_1956_1978 + ",
    controls_predetermined_full,
    " + ",
    controls_mun_pre_last
  )
)

m_logit_exp_did_robust_contemporaneous <- run_logit(
  rhs = paste0(
    "post:share_1936_1955 + post:share_1956_1978 + ",
    controls_contemporaneous_robust
  )
)

m_logit_exp_did_robust_contemporaneous_full <- run_logit(
  rhs = paste0(
    "post:share_1936_1955 + post:share_1956_1978 + ",
    controls_contemporaneous_full_robust
  )
)

models_logit_exposure <- list(
  "Exposure, year FE" = m_logit_exp_cross,
  "Exposure + predetermined, year FE" = m_logit_exp_cross_predetermined,
  "Exposure + contemporary controls, year FE" = m_logit_exp_cross_robust_contemporaneous,
  "Post x exposure" = m_logit_exp_did,
  "Post x exposure + predetermined" = m_logit_exp_did_predetermined,
  "Post x exposure + municipal pre controls, all pre years" =
    m_logit_exp_did_mun_pre_all,
  "Post x exposure + predetermined + municipal pre controls, all pre years" =
    m_logit_exp_did_predetermined_mun_pre_all,
  "Post x exposure + municipal pre controls, last pre year" =
    m_logit_exp_did_mun_pre_last,
  "Post x exposure + predetermined + municipal pre controls, last pre year" =
    m_logit_exp_did_predetermined_mun_pre_last,
  "Post x exposure + contemporary controls" =
    m_logit_exp_did_robust_contemporaneous,
  "Post x exposure + full contemporary controls" =
    m_logit_exp_did_robust_contemporaneous_full
)

# ---------------------------------------------------------------------------- #
# 16. LOGIT: política individual
# ---------------------------------------------------------------------------- #

models_logit_politics <- list()

if ("izq_der" %in% names(lapop)) {
  models_logit_politics[["Ideology"]] <- run_logit(
    rhs = paste0("izq_der + ", controls_predetermined_full)
  )
}

if ("identifica_partido" %in% names(lapop)) {
  models_logit_politics[["Party ID"]] <- run_logit(
    rhs = paste0("identifica_partido + ", controls_predetermined_full)
  )
}

if ("voto_anterior" %in% names(lapop)) {
  models_logit_politics[["Previous vote"]] <- run_logit(
    rhs = paste0("voto_anterior + ", controls_predetermined_full)
  )
}

if ("interes_pol_mucho" %in% names(lapop)) {
  models_logit_politics[["High political interest"]] <- run_logit(
    rhs = paste0("interes_pol_mucho + ", controls_predetermined_full)
  )
}

if ("voto_blanco_nulo" %in% names(lapop)) {
  models_logit_politics[["Blank/null vote"]] <- run_logit(
    rhs = paste0("voto_blanco_nulo + ", controls_predetermined_full)
  )
}

# ---------------------------------------------------------------------------- #
# 17. Triple diferencia: post x variable x exposición municipal
# ---------------------------------------------------------------------------- #
# Main:
# Solo variables predeterminadas.
#
# Robustez:
# Incluye variables que pueden cambiar en el tiempo.

triple_vars_main <- c(
  "blanco",
  "edad",
  "hombre",
  "etnia_minoritaria"
)

triple_vars_main <- intersect(triple_vars_main, names(lapop))

triple_vars_robust <- c(
  triple_vars_main,
  "rural",
  "superior_incompleta_o_mas",
  "desempleado",
  "ocupado",
  "estudiante",
  "en_pareja",
  "interes_pol_mucho",
  "voto_blanco_nulo"
)

triple_vars_robust <- unique(intersect(triple_vars_robust, names(lapop)))

run_triple_by_var_lpm <- function(x,
                                  data = lapop,
                                  exposure_1 = "share_1936_1955",
                                  exposure_2 = "share_1956_1978",
                                  control_vars = basic_predetermined_vars) {
  
  controls_x <- setdiff(control_vars, x)
  
  controls_rhs <- if (length(controls_x) > 0) {
    paste(controls_x, collapse = " + ")
  } else {
    NULL
  }
  
  rhs_main <- paste0(
    "post:", exposure_1,
    " + post:", exposure_2,
    
    " + post:", x,
    " + ", x, ":", exposure_1,
    " + ", x, ":", exposure_2,
    
    " + post:", x, ":", exposure_1,
    " + post:", x, ":", exposure_2
  )
  
  rhs <- if (!is.null(controls_rhs)) {
    paste(rhs_main, controls_rhs, sep = " + ")
  } else {
    rhs_main
  }
  
  run_lpm(
    rhs = rhs,
    data = data
  )
}

models_lpm_triple_main <- map(
  triple_vars_main,
  ~ run_triple_by_var_lpm(.x)
)

names(models_lpm_triple_main) <- triple_vars_main

models_lpm_triple_robust <- map(
  triple_vars_robust,
  ~ run_triple_by_var_lpm(.x)
)

names(models_lpm_triple_robust) <- triple_vars_robust

# ---------------------------------------------------------------------------- #
# 18. Cambios post por variable: post x variable
# ---------------------------------------------------------------------------- #

post_vars_main <- triple_vars_main
post_vars_robust <- triple_vars_robust

run_post_by_var_lpm <- function(x,
                                data = lapop,
                                control_vars = basic_predetermined_vars) {
  
  controls_x <- setdiff(control_vars, x)
  
  controls_rhs <- if (length(controls_x) > 0) {
    paste(controls_x, collapse = " + ")
  } else {
    NULL
  }
  
  rhs_main <- paste0(
    "post:", x,
    " + ",
    x
  )
  
  rhs <- if (!is.null(controls_rhs)) {
    paste(rhs_main, controls_rhs, sep = " + ")
  } else {
    rhs_main
  }
  
  run_lpm(
    rhs = rhs,
    data = data
  )
}

models_lpm_post_by_var_main <- map(
  post_vars_main,
  ~ run_post_by_var_lpm(.x)
)

names(models_lpm_post_by_var_main) <- post_vars_main

models_lpm_post_by_var_robust <- map(
  post_vars_robust,
  ~ run_post_by_var_lpm(.x)
)

names(models_lpm_post_by_var_robust) <- post_vars_robust

# ---------------------------------------------------------------------------- #
# 19. Exportar resultados
# ---------------------------------------------------------------------------- #

gof_map_lpm <- data.frame(
  raw = c("nobs", "r.squared", "FE: year", "FE: mun_code"),
  clean = c("Observations", "R²", "Year FE", "Municipality FE"),
  fmt = c(0, 3, 0, 0)
)

gof_map_logit <- data.frame(
  raw = c("nobs", "pseudo.r.squared", "FE: year", "FE: mun_code"),
  clean = c("Observations", "Pseudo R²", "Year FE", "Municipality FE"),
  fmt = c(0, 3, 0, 0)
)

notes_lpm <- paste(
  "Outcome is migration intention.",
  "Linear probability models.",
  "Survey weights used.",
  "Standard errors clustered at the municipality level.",
  "Models with municipality FE identify within-municipality changes.",
  "The post indicator is absorbed by year fixed effects; coefficients of interest are post interactions.",
  "Preferred specifications avoid contemporaneous socioeconomic controls that may be affected by the post-2023 context.",
  "Municipality-level controls are measured in the pre-2023 period and interacted with post.",
  "The main pre-period controls use all years before 2023; last-pre-year controls are reported as robustness."
)

notes_logit <- paste(
  "Outcome is migration intention.",
  "Logit models.",
  "Reported coefficients are odds ratios.",
  "Survey weights used.",
  "Standard errors clustered at the municipality level.",
  "Models include year and municipality fixed effects unless otherwise noted.",
  "The post indicator is absorbed by year fixed effects; coefficients of interest are post interactions.",
  "Preferred specifications avoid contemporaneous socioeconomic controls that may be affected by the post-2023 context.",
  "Municipality-level controls are measured in the pre-2023 period and interacted with post.",
  "The main pre-period controls use all years before 2023; last-pre-year controls are reported as robustness.",
  "Statistical significance is assessed directly from the estimated logit coefficients."
)

notes_selection <- paste(
  "Outcome is migration intention.",
  "Models describe selection into migration intention.",
  "Some specifications include contemporaneous individual characteristics and should be interpreted descriptively.",
  "Survey weights used.",
  "Standard errors clustered at the municipality level."
)

notes_post_selection <- paste(
  "Outcome is migration intention.",
  "Linear probability models.",
  "Survey weights used.",
  "Standard errors clustered at the municipality level.",
  "All models include year and municipality fixed effects.",
  "Each column estimates the interaction between post and the variable named in the column.",
  "Preferred columns use predetermined individual characteristics."
)

notes_triple <- paste(
  "Outcome is migration intention.",
  "Linear probability models.",
  "Survey weights used.",
  "Standard errors clustered at the municipality level.",
  "All models include year and municipality fixed effects.",
  "Each column estimates a triple difference with the variable named in the column.",
  "The coefficients of interest are post x variable x share_1936_1955 and post x variable x share_1956_1978.",
  "Preferred columns use predetermined individual characteristics."
)

# ------------------------- #
# LPM tables
# ------------------------- #

modelsummary(
  models_lpm_selection,
  output = "Output/lapop_lpm_table1_selection.xlsx",
  stars = c("*" = .1, "**" = .05, "***" = .01),
  gof_map = gof_map_lpm,
  notes = notes_selection
)

modelsummary(
  models_lpm_post_selection_main,
  output = "Output/lapop_lpm_table2_post_selection_predetermined.xlsx",
  stars = c("*" = .1, "**" = .05, "***" = .01),
  gof_map = gof_map_lpm,
  notes = notes_post_selection
)

if (length(models_lpm_post_selection_robust) > 0) {
  modelsummary(
    models_lpm_post_selection_robust,
    output = "Output/lapop_lpm_table2b_post_selection_robust_time_varying.xlsx",
    stars = c("*" = .1, "**" = .05, "***" = .01),
    gof_map = gof_map_lpm,
    notes = paste(
      notes_post_selection,
      "These columns include time-varying characteristics and should be interpreted as robustness/descriptive evidence."
    )
  )
}

modelsummary(
  models_lpm_exposure,
  output = "Output/lapop_lpm_table3_spanish_exposure_pre_controls.xlsx",
  stars = c("*" = .1, "**" = .05, "***" = .01),
  gof_map = gof_map_lpm,
  notes = notes_lpm
)

if (length(models_lpm_politics) > 0) {
  modelsummary(
    models_lpm_politics,
    output = "Output/lapop_lpm_table4_politics_predetermined_controls.xlsx",
    stars = c("*" = .1, "**" = .05, "***" = .01),
    gof_map = gof_map_lpm,
    notes = paste(
      notes_lpm,
      "Political variables may be endogenous to the post-2023 context and are interpreted descriptively."
    )
  )
}

modelsummary(
  models_lpm_triple_main,
  output = "Output/lapop_lpm_table5_triple_difference_predetermined.xlsx",
  stars = c("*" = .1, "**" = .05, "***" = .01),
  gof_map = gof_map_lpm,
  notes = notes_triple
)

modelsummary(
  models_lpm_triple_robust,
  output = "Output/lapop_lpm_table5b_triple_difference_robust_all_vars.xlsx",
  stars = c("*" = .1, "**" = .05, "***" = .01),
  gof_map = gof_map_lpm,
  notes = paste(
    notes_triple,
    "This robustness table includes time-varying characteristics."
  )
)

modelsummary(
  models_lpm_post_by_var_main,
  output = "Output/lapop_lpm_table6_post_by_variable_predetermined.xlsx",
  stars = c("*" = .1, "**" = .05, "***" = .01),
  gof_map = gof_map_lpm,
  notes = notes_post_selection
)

modelsummary(
  models_lpm_post_by_var_robust,
  output = "Output/lapop_lpm_table6b_post_by_variable_robust_all_vars.xlsx",
  stars = c("*" = .1, "**" = .05, "***" = .01),
  gof_map = gof_map_lpm,
  notes = paste(
    notes_post_selection,
    "This robustness table includes time-varying characteristics."
  )
)

# ------------------------- #
# LOGIT tables
# ------------------------- #

modelsummary(
  models_logit_selection,
  output = "Output/lapop_logit_table1_selection_odds_ratios.xlsx",
  exponentiate = TRUE,
  stars = c("*" = .1, "**" = .05, "***" = .01),
  gof_map = gof_map_logit,
  notes = notes_selection
)

modelsummary(
  models_logit_post_selection_main,
  output = "Output/lapop_logit_table2_post_selection_predetermined_odds_ratios.xlsx",
  exponentiate = TRUE,
  stars = c("*" = .1, "**" = .05, "***" = .01),
  gof_map = gof_map_logit,
  notes = notes_logit
)

if (length(models_logit_post_selection_robust) > 0) {
  modelsummary(
    models_logit_post_selection_robust,
    output = "Output/lapop_logit_table2b_post_selection_robust_time_varying_odds_ratios.xlsx",
    exponentiate = TRUE,
    stars = c("*" = .1, "**" = .05, "***" = .01),
    gof_map = gof_map_logit,
    notes = paste(
      notes_logit,
      "These columns include time-varying characteristics and should be interpreted as robustness/descriptive evidence."
    )
  )
}

modelsummary(
  models_logit_exposure,
  output = "Output/lapop_logit_table3_spanish_exposure_pre_controls_odds_ratios.xlsx",
  exponentiate = TRUE,
  stars = c("*" = .1, "**" = .05, "***" = .01),
  gof_map = gof_map_logit,
  notes = notes_logit
)

if (length(models_logit_politics) > 0) {
  modelsummary(
    models_logit_politics,
    output = "Output/lapop_logit_table4_politics_predetermined_controls_odds_ratios.xlsx",
    exponentiate = TRUE,
    stars = c("*" = .1, "**" = .05, "***" = .01),
    gof_map = gof_map_logit,
    notes = paste(
      notes_logit,
      "Political variables may be endogenous to the post-2023 context and are interpreted descriptively."
    )
  )
}

# ---------------------------------------------------------------------------- #
# 20. Extraer coeficientes principales para lectura rápida
# ---------------------------------------------------------------------------- #
# Esta versión es robusta a que el p-value aparezca como:
# Pr(>|t|), Pr(>|z|), u otro nombre parecido.

standardize_coeftable <- function(model) {
  
  coeftable_df <- as.data.frame(coeftable(model))
  coeftable_df$term <- rownames(coeftable_df)
  
  estimate_col <- names(coeftable_df)[str_detect(names(coeftable_df), "^Estimate$")]
  se_col <- names(coeftable_df)[str_detect(names(coeftable_df), "Std\\. Error")]
  pval_col <- names(coeftable_df)[str_detect(names(coeftable_df), "^Pr\\(")]
  
  if (length(estimate_col) == 0) {
    estimate_col <- names(coeftable_df)[1]
  }
  
  if (length(se_col) == 0) {
    se_col <- names(coeftable_df)[2]
  }
  
  if (length(pval_col) == 0) {
    coeftable_df$p_value <- NA_real_
  } else {
    coeftable_df$p_value <- coeftable_df[[pval_col[1]]]
  }
  
  coeftable_df %>%
    mutate(
      Estimate = .data[[estimate_col[1]]],
      Std_Error = .data[[se_col[1]]]
    ) %>%
    select(term, Estimate, Std_Error, p_value)
}

extract_key_coefs <- function(models,
                              pattern = "post:|share_1936_1955|share_1956_1978") {
  
  if (length(models) == 0) {
    return(data.frame())
  }
  
  map_dfr(
    names(models),
    function(model_name) {
      
      standardize_coeftable(models[[model_name]]) %>%
        mutate(model = model_name) %>%
        filter(str_detect(term, pattern)) %>%
        select(model, term, Estimate, Std_Error, p_value)
    }
  )
}

extract_triple_by_var_coefs <- function(models,
                                        exposure_1 = "share_1936_1955",
                                        exposure_2 = "share_1956_1978") {
  
  if (length(models) == 0) {
    return(data.frame())
  }
  
  map_dfr(
    names(models),
    function(model_name) {
      
      pattern <- paste0(
        "post:", model_name, ":", exposure_1, "|",
        "post:", model_name, ":", exposure_2
      )
      
      standardize_coeftable(models[[model_name]]) %>%
        mutate(model = model_name) %>%
        filter(str_detect(term, pattern)) %>%
        mutate(
          variable = model_name,
          exposure = case_when(
            str_detect(term, exposure_1) ~ exposure_1,
            str_detect(term, exposure_2) ~ exposure_2,
            TRUE ~ NA_character_
          )
        ) %>%
        select(variable, exposure, term, Estimate, Std_Error, p_value)
    }
  )
}

extract_post_by_var_coefs <- function(models) {
  
  if (length(models) == 0) {
    return(data.frame())
  }
  
  map_dfr(
    names(models),
    function(model_name) {
      
      pattern <- paste0("post:", model_name)
      
      standardize_coeftable(models[[model_name]]) %>%
        mutate(model = model_name) %>%
        filter(term == pattern) %>%
        mutate(variable = model_name) %>%
        select(variable, term, Estimate, Std_Error, p_value)
    }
  )
}

key_lpm_post_main <- extract_key_coefs(models_lpm_post_selection_main)
key_lpm_post_robust <- extract_key_coefs(models_lpm_post_selection_robust)
key_lpm_exposure <- extract_key_coefs(models_lpm_exposure)
key_lpm_triple_main <- extract_triple_by_var_coefs(models_lpm_triple_main)
key_lpm_triple_robust <- extract_triple_by_var_coefs(models_lpm_triple_robust)
key_lpm_post_by_var_main <- extract_post_by_var_coefs(models_lpm_post_by_var_main)
key_lpm_post_by_var_robust <- extract_post_by_var_coefs(models_lpm_post_by_var_robust)

key_logit_post_main <- extract_key_coefs(models_logit_post_selection_main)
key_logit_post_robust <- extract_key_coefs(models_logit_post_selection_robust)
key_logit_exposure <- extract_key_coefs(models_logit_exposure)

write.xlsx(
  list(
    "LPM post predetermined" = key_lpm_post_main,
    "LPM post robust time varying" = key_lpm_post_robust,
    "LPM exposure" = key_lpm_exposure,
    "LPM triple predetermined" = key_lpm_triple_main,
    "LPM triple robust all vars" = key_lpm_triple_robust,
    "LPM post by var predetermined" = key_lpm_post_by_var_main,
    "LPM post by var robust all vars" = key_lpm_post_by_var_robust,
    "Logit post predetermined" = key_logit_post_main,
    "Logit post robust time varying" = key_logit_post_robust,
    "Logit exposure" = key_logit_exposure
  ),
  "Output/lapop_key_coefficients_pre_controls.xlsx",
  overwrite = TRUE
)

# ---------------------------------------------------------------------------- #
# 21. Chequeos finales y base usada
# ---------------------------------------------------------------------------- #

cat("Observaciones en base LAPOP:", nrow(lapop), "\n")
cat("Municipios:", n_distinct(lapop$mun_code), "\n")
cat("Años LAPOP:", paste(sort(unique(lapop$year)), collapse = ", "), "\n")
cat("Share migración promedio:", mean(lapop$intencion_migrar, na.rm = TRUE), "\n")

lapop_summary <- lapop %>%
  group_by(year, post) %>%
  summarise(
    n = n(),
    mean_mig = wmean(intencion_migrar, wt),
    mean_edad = wmean(edad, wt),
    share_hombre = wmean(hombre, wt),
    share_rural = wmean(rural, wt),
    share_desempleado = wmean(desempleado, wt),
    share_ocupado = wmean(ocupado, wt),
    share_estudiante = wmean(estudiante, wt),
    share_en_pareja = wmean(en_pareja, wt),
    share_etnia_minoritaria = wmean(etnia_minoritaria, wt),
    
    share_secundaria_completa_o_mas =
      if ("secundaria_completa_o_mas" %in% names(lapop)) {
        wmean(secundaria_completa_o_mas, wt)
      } else {
        NA_real_
      },
    
    share_superior_incompleta_o_mas =
      if ("superior_incompleta_o_mas" %in% names(lapop)) {
        wmean(superior_incompleta_o_mas, wt)
      } else {
        NA_real_
      },
    
    share_superior_completa =
      if ("superior_completa" %in% names(lapop)) {
        wmean(superior_completa, wt)
      } else {
        NA_real_
      },
    
    mean_share_1936_1955 = wmean(share_1936_1955, wt),
    mean_share_1956_1978 = wmean(share_1956_1978, wt),
    
    mean_mun_pre_all_share_desempleado =
      wmean(mun_pre_all_share_desempleado, wt),
    
    mean_mun_pre_all_share_ocupado =
      wmean(mun_pre_all_share_ocupado, wt),
    
    mean_mun_pre_all_share_estudiante =
      wmean(mun_pre_all_share_estudiante, wt),
    
    mean_mun_pre_all_share_en_pareja =
      wmean(mun_pre_all_share_en_pareja, wt),
    
    mean_mun_pre_last_share_desempleado =
      wmean(mun_pre_last_share_desempleado, wt),
    
    mean_mun_pre_last_share_ocupado =
      wmean(mun_pre_last_share_ocupado, wt),
    
    mean_mun_pre_last_share_estudiante =
      wmean(mun_pre_last_share_estudiante, wt),
    
    mean_mun_pre_last_share_en_pareja =
      wmean(mun_pre_last_share_en_pareja, wt),
    
    .groups = "drop"
  )

write.xlsx(
  lapop_summary,
  "Output/lapop_summary_by_year_weighted_pre_controls.xlsx",
  overwrite = TRUE
)

write_csv(
  lapop,
  "Output/lapop_estimation_sample_with_pre_controls.csv"
)

write.xlsx(
  mun_pre_all_controls,
  "Output/municipal_pre_controls_all_pre_years.xlsx",
  overwrite = TRUE
)

write.xlsx(
  mun_pre_last_controls,
  "Output/municipal_pre_controls_last_pre_year_by_mun.xlsx",
  overwrite = TRUE
)

cat("Código terminado correctamente.\n")