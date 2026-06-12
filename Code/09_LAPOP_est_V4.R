# ---------------------------------------------------------------------------- #
#                                 CÓDIGO 9
#          Estimaciones LAPOP: quiénes tienen más probabilidad de migrar
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

lapop <- read_csv("Data Out/lapop_data_merge.csv", show_col_types = FALSE)

# ---------------------------------------------------------------------------- #
# 1. Preparar LAPOP
# ---------------------------------------------------------------------------- #

lapop <- lapop %>%
  mutate(
    post = if_else(year >= 2023, 1, 0),
    
    # Variables categóricas para fixest
    nivel_educ7 = as.factor(nivel_educ7),
    sit_lab_mig = as.factor(sit_lab_mig),
    etnia_arg = as.factor(etnia_arg),
    estado_civil = as.factor(estado_civil),
    
    # FE como factores
    mun_code = as.factor(mun_code),
    year = as.factor(year)
  )

# ---------------------------------------------------------------------------- #
# 2. Chequeos básicos
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
table(lapop$year, lapop$etnia_arg, useNA = "ifany")

# Chequeo variables clave
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
  "year"
)

missing_vars <- setdiff(vars_needed, names(lapop))

if (length(missing_vars) > 0) {
  stop(paste(
    "Faltan estas variables en lapop:",
    paste(missing_vars, collapse = ", ")
  ))
}

# ---------------------------------------------------------------------------- #
# 3. Funciones auxiliares
# ---------------------------------------------------------------------------- #

wmean <- function(x, w) {
  weighted.mean(x, w = w, na.rm = TRUE)
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
# 4. Bloques de controles
# ---------------------------------------------------------------------------- #

controls_min <- paste(
  "edad",
  "hombre",
  sep = " + "
)

controls_demo <- paste(
  "edad",
  "hombre",
  "i(nivel_educ7)",
  "rural",
  sep = " + "
)

controls_socio <- paste(
  "edad",
  "hombre",
  "i(nivel_educ7)",
  "rural",
  "i(sit_lab_mig)",
  "en_pareja",
  sep = " + "
)

# Uso etnia_minoritaria en vez de i(etnia_arg) para perder menos observaciones
controls_full <- paste(
  "edad",
  "hombre",
  "i(nivel_educ7)",
  "rural",
  "i(sit_lab_mig)",
  "en_pareja",
  "etnia_minoritaria",
  sep = " + "
)

# ---------------------------------------------------------------------------- #
# 5. Descriptivos
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
    share_secundaria_completa_o_mas = wmean(secundaria_completa_o_mas, wt),
    share_superior_incompleta_o_mas = wmean(superior_incompleta_o_mas, wt),
    share_superior_completa = wmean(superior_completa, wt),
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
    share_secundaria_completa_o_mas = wmean(secundaria_completa_o_mas, wt),
    share_superior_incompleta_o_mas = wmean(superior_incompleta_o_mas, wt),
    share_superior_completa = wmean(superior_completa, wt),
    .groups = "drop"
  )

write.xlsx(desc_mig, "Output/lapop_desc_migrants_weighted.xlsx", overwrite = TRUE)
write.xlsx(desc_year, "Output/lapop_desc_by_year_weighted.xlsx", overwrite = TRUE)

# ---------------------------------------------------------------------------- #
# 6. LPM: selección individual promedio
# ---------------------------------------------------------------------------- #
# Pregunta: ¿quiénes tienen mayor probabilidad de querer migrar?

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
# 7. LPM: cambios en selección en 2023
# ---------------------------------------------------------------------------- #

# DE ESTAS ALGUNAS DABAN SIGNIFICATIVAS

# Pregunta: ¿la selección de quienes quieren migrar cambia en 2023?
#
# Nota:
# No usamos post * X sino post:X.
# Como hay year FE, post se absorbe. La interacción post:X sí se identifica.

m_lpm_post_age <- run_lpm(
  rhs = "post:edad + edad + hombre + rural + i(nivel_educ7)"
)

m_lpm_post_gender <- run_lpm(
  rhs = "post:hombre + hombre + edad + rural + i(nivel_educ7)"
)

m_lpm_post_educ <- run_lpm(
  rhs = "post:superior_incompleta_o_mas + superior_incompleta_o_mas + edad + hombre + rural"
)

m_lpm_post_unemp <- run_lpm(
  rhs = "post:desempleado + desempleado + edad + hombre + rural + i(nivel_educ7)"
)

m_lpm_post_student <- run_lpm(
  rhs = "post:estudiante + estudiante + edad + hombre + rural + i(nivel_educ7)"
)

m_lpm_post_partner <- run_lpm(
  rhs = "post:en_pareja + en_pareja + edad + hombre + rural + i(nivel_educ7)"
)

m_lpm_post_ethnicity <- run_lpm(
  rhs = "post:etnia_minoritaria + etnia_minoritaria + edad + hombre + rural + i(nivel_educ7)"
)

models_lpm_post_selection <- list(
  "Age" = m_lpm_post_age,
  "Male" = m_lpm_post_gender,
  "Higher education" = m_lpm_post_educ,
  "Unemployed" = m_lpm_post_unemp,
  "Student" = m_lpm_post_student,
  "In couple" = m_lpm_post_partner,
  "Minority ethnicity" = m_lpm_post_ethnicity
)

# ---------------------------------------------------------------------------- #
# 8. LPM: exposición española municipal
# ---------------------------------------------------------------------------- #

# 8.1 Asociación cross-sectional entre exposición española e intención de migrar
# Sin FE de municipio porque los shares son municipales y se absorberían.

m_lpm_exp_cross <- run_lpm(
  rhs = "share_1936_1955 + share_1956_1978",
  fe = "year"
)

m_lpm_exp_cross_controls <- run_lpm(
  rhs = paste0(
    "share_1936_1955 + share_1956_1978 + ",
    controls_socio
  ),
  fe = "year"
)

# 8.2 Cambio diferencial en 2023 por exposición municipal
# Con FE de municipio y año.
# Los coeficientes de interés son post:share_1936_1955 y post:share_1956_1978.

m_lpm_exp_did <- run_lpm(
  rhs = "post:share_1936_1955 + post:share_1956_1978"
)

m_lpm_exp_did_controls <- run_lpm(
  rhs = paste0(
    "post:share_1936_1955 + post:share_1956_1978 + ",
    controls_socio
  )
)

m_lpm_exp_did_full <- run_lpm(
  rhs = paste0(
    "post:share_1936_1955 + post:share_1956_1978 + ",
    controls_full
  )
)

models_lpm_exposure <- list(
  "Exposure, year FE" = m_lpm_exp_cross,
  "Exposure + controls, year FE" = m_lpm_exp_cross_controls,
  "Post x exposure, mun FE" = m_lpm_exp_did,
  "Post x exposure + controls" = m_lpm_exp_did_controls,
  "Post x exposure + full controls" = m_lpm_exp_did_full
)

# ---------------------------------------------------------------------------- #
# 9. LPM: política individual
# ---------------------------------------------------------------------------- #
# Estos modelos pueden perder más observaciones por missing en variables políticas.

m_lpm_pol_ideology <- run_lpm(
  rhs = paste0("izq_der + ", controls_socio)
)

m_lpm_pol_party <- run_lpm(
  rhs = paste0("identifica_partido + ", controls_socio)
)

m_lpm_pol_prev_vote <- run_lpm(
  rhs = paste0("voto_anterior + ", controls_socio)
)

m_lpm_pol_interest <- run_lpm(
  rhs = paste0("interes_pol_mucho + ", controls_socio)
)

m_lpm_pol_blanknull <- run_lpm(
  rhs = paste0("voto_blanco_nulo + ", controls_socio)
)

models_lpm_politics <- list(
  "Ideology" = m_lpm_pol_ideology,
  "Party ID" = m_lpm_pol_party,
  "Previous vote" = m_lpm_pol_prev_vote,
  "High political interest" = m_lpm_pol_interest,
  "Blank/null vote" = m_lpm_pol_blanknull
)

# ---------------------------------------------------------------------------- #
# 10. LOGIT: selección individual promedio
# ---------------------------------------------------------------------------- #
# Robustness check.
# Mantengo year FE y mun_code FE para que sea comparable con LPM.
# Significancia se evalúa directamente con errores clusterizados del logit.
# En tablas reporto odds ratios con exponentiate = TRUE.

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
# 11. LOGIT: cambios en selección en 2023
# ---------------------------------------------------------------------------- #
# Lo que miramos es si las interacciones post:X son significativas.

m_logit_post_age <- run_logit(
  rhs = "post:edad + edad + hombre + rural + i(nivel_educ7)"
)

m_logit_post_gender <- run_logit(
  rhs = "post:hombre + hombre + edad + rural + i(nivel_educ7)"
)

m_logit_post_educ <- run_logit(
  rhs = "post:superior_incompleta_o_mas + superior_incompleta_o_mas + edad + hombre + rural"
)

m_logit_post_unemp <- run_logit(
  rhs = "post:desempleado + desempleado + edad + hombre + rural + i(nivel_educ7)"
)

m_logit_post_student <- run_logit(
  rhs = "post:estudiante + estudiante + edad + hombre + rural + i(nivel_educ7)"
)

m_logit_post_partner <- run_logit(
  rhs = "post:en_pareja + en_pareja + edad + hombre + rural + i(nivel_educ7)"
)

m_logit_post_ethnicity <- run_logit(
  rhs = "post:etnia_minoritaria + etnia_minoritaria + edad + hombre + rural + i(nivel_educ7)"
)

models_logit_post_selection <- list(
  "Age" = m_logit_post_age,
  "Male" = m_logit_post_gender,
  "Higher education" = m_logit_post_educ,
  "Unemployed" = m_logit_post_unemp,
  "Student" = m_logit_post_student,
  "In couple" = m_logit_post_partner,
  "Minority ethnicity" = m_logit_post_ethnicity
)

# ---------------------------------------------------------------------------- #
# 12. LOGIT: exposición española municipal
# ---------------------------------------------------------------------------- #

# 12.1 Asociación cross-sectional
# Sin FE de municipio para poder estimar los efectos principales de los shares.

m_logit_exp_cross <- run_logit(
  rhs = "share_1936_1955 + share_1956_1978",
  fe = "year"
)

m_logit_exp_cross_controls <- run_logit(
  rhs = paste0(
    "share_1936_1955 + share_1956_1978 + ",
    controls_socio
  ),
  fe = "year"
)

# 12.2 Cambio diferencial en 2023 por exposición
# Con FE de año y municipio.

m_logit_exp_did <- run_logit(
  rhs = "post:share_1936_1955 + post:share_1956_1978"
)

m_logit_exp_did_controls <- run_logit(
  rhs = paste0(
    "post:share_1936_1955 + post:share_1956_1978 + ",
    controls_socio
  )
)

m_logit_exp_did_full <- run_logit(
  rhs = paste0(
    "post:share_1936_1955 + post:share_1956_1978 + ",
    controls_full
  )
)

models_logit_exposure <- list(
  "Exposure, year FE" = m_logit_exp_cross,
  "Exposure + controls, year FE" = m_logit_exp_cross_controls,
  "Post x exposure, mun FE" = m_logit_exp_did,
  "Post x exposure + controls" = m_logit_exp_did_controls,
  "Post x exposure + full controls" = m_logit_exp_did_full
)

# ---------------------------------------------------------------------------- #
# 13. LOGIT: política individual
# ---------------------------------------------------------------------------- #

m_logit_pol_ideology <- run_logit(
  rhs = paste0("izq_der + ", controls_socio)
)

m_logit_pol_party <- run_logit(
  rhs = paste0("identifica_partido + ", controls_socio)
)

m_logit_pol_prev_vote <- run_logit(
  rhs = paste0("voto_anterior + ", controls_socio)
)

m_logit_pol_interest <- run_logit(
  rhs = paste0("interes_pol_mucho + ", controls_socio)
)

m_logit_pol_blanknull <- run_logit(
  rhs = paste0("voto_blanco_nulo + ", controls_socio)
)

models_logit_politics <- list(
  "Ideology" = m_logit_pol_ideology,
  "Party ID" = m_logit_pol_party,
  "Previous vote" = m_logit_pol_prev_vote,
  "High political interest" = m_logit_pol_interest,
  "Blank/null vote" = m_logit_pol_blanknull
)

# # ---------------------------------------------------------------------------- #
# # 14. Outcomes secundarios
# # ---------------------------------------------------------------------------- #
# # Cuidado: intencion_migrar_esp y migracion_probable tienen muchas menos observaciones.
# 
# secondary_outcomes <- c("intencion_migrar_esp", "migracion_probable")
# secondary_outcomes <- intersect(secondary_outcomes, names(lapop))
# 
# models_lpm_secondary <- list()
# models_logit_secondary <- list()
# 
# for (yy in secondary_outcomes) {
#   
#   # LPM: exposición cross-sectional
#   models_lpm_secondary[[paste0(yy, " - exposure")]] <- run_lpm(
#     y = yy,
#     rhs = paste0(
#       "share_1936_1955 + share_1956_1978 + ",
#       controls_socio
#     ),
#     fe = "year"
#   )
#   
#   # LPM: post x exposure
#   models_lpm_secondary[[paste0(yy, " - post x exposure")]] <- run_lpm(
#     y = yy,
#     rhs = paste0(
#       "post:share_1936_1955 + post:share_1956_1978 + ",
#       controls_socio
#     ),
#     fe = "year + mun_code"
#   )
#   
#   # LOGIT: exposición cross-sectional
#   models_logit_secondary[[paste0(yy, " - exposure")]] <- run_logit(
#     y = yy,
#     rhs = paste0(
#       "share_1936_1955 + share_1956_1978 + ",
#       controls_socio
#     ),
#     fe = "year"
#   )
#   
#   # LOGIT: post x exposure
#   models_logit_secondary[[paste0(yy, " - post x exposure")]] <- run_logit(
#     y = yy,
#     rhs = paste0(
#       "post:share_1936_1955 + post:share_1956_1978 + ",
#       controls_socio
#     ),
#     fe = "year + mun_code"
#   )
# }

# ---------------------------------------------------------------------------- #
# 15. Exportar resultados
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
  "The post indicator is absorbed by year fixed effects; coefficients of interest are post interactions."
)

notes_logit <- paste(
  "Outcome is migration intention.",
  "Logit models.",
  "Reported coefficients are odds ratios.",
  "Survey weights used.",
  "Standard errors clustered at the municipality level.",
  "Models include year and municipality fixed effects unless otherwise noted.",
  "The post indicator is absorbed by year fixed effects; coefficients of interest are post interactions.",
  "Statistical significance is assessed directly from the estimated logit coefficients."
)

# ------------------------- #
# LPM tables
# ------------------------- #

modelsummary(
  models_lpm_selection,
  output = "Output/lapop_lpm_table1_selection.xlsx",
  stars = c("*" = .1, "**" = .05, "***" = .01),
  gof_map = gof_map_lpm,
  notes = notes_lpm
)

modelsummary(
  models_lpm_post_selection,
  output = "Output/lapop_lpm_table2_post_selection.xlsx",
  stars = c("*" = .1, "**" = .05, "***" = .01),
  gof_map = gof_map_lpm,
  notes = notes_lpm
)

modelsummary(
  models_lpm_exposure,
  output = "Output/lapop_lpm_table3_spanish_exposure.xlsx",
  stars = c("*" = .1, "**" = .05, "***" = .01),
  gof_map = gof_map_lpm,
  notes = notes_lpm
)

modelsummary(
  models_lpm_politics,
  output = "Output/lapop_lpm_table4_politics.xlsx",
  stars = c("*" = .1, "**" = .05, "***" = .01),
  gof_map = gof_map_lpm,
  notes = notes_lpm
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
  notes = notes_logit
)

modelsummary(
  models_logit_post_selection,
  output = "Output/lapop_logit_table2_post_selection_odds_ratios.xlsx",
  exponentiate = TRUE,
  stars = c("*" = .1, "**" = .05, "***" = .01),
  gof_map = gof_map_logit,
  notes = notes_logit
)

modelsummary(
  models_logit_exposure,
  output = "Output/lapop_logit_table3_spanish_exposure_odds_ratios.xlsx",
  exponentiate = TRUE,
  stars = c("*" = .1, "**" = .05, "***" = .01),
  gof_map = gof_map_logit,
  notes = notes_logit
)

modelsummary(
  models_logit_politics,
  output = "Output/lapop_logit_table4_politics_odds_ratios.xlsx",
  exponentiate = TRUE,
  stars = c("*" = .1, "**" = .05, "***" = .01),
  gof_map = gof_map_logit,
  notes = notes_logit
)

# ------------------------- #
# Secondary outcomes
# ------------------------- #

# if (length(models_lpm_secondary) > 0) {
#   modelsummary(
#     models_lpm_secondary,
#     output = "Output/lapop_lpm_appendix_secondary_outcomes.xlsx",
#     stars = c("*" = .1, "**" = .05, "***" = .01),
#     gof_map = gof_map_lpm,
#     notes = paste(notes_lpm, "Secondary outcomes have limited non-missing observations.")
#   )
# }
# 
# if (length(models_logit_secondary) > 0) {
#   modelsummary(
#     models_logit_secondary,
#     output = "Output/lapop_logit_appendix_secondary_outcomes_odds_ratios.xlsx",
#     exponentiate = TRUE,
#     stars = c("*" = .1, "**" = .05, "***" = .01),
#     gof_map = gof_map_logit,
#     notes = paste(notes_logit, "Secondary outcomes have limited non-missing observations.")
#   )
# }

# ---------------------------------------------------------------------------- #
# 16. Extraer coeficientes principales para lectura rápida
# ---------------------------------------------------------------------------- #

extract_key_coefs <- function(models, pattern = "post:|share_1936_1955|share_1956_1978") {
  
  map_dfr(
    names(models),
    function(model_name) {
      coeftable <- as.data.frame(coeftable(models[[model_name]]))
      coeftable$term <- rownames(coeftable)
      coeftable$model <- model_name
      
      coeftable %>%
        filter(str_detect(term, pattern)) %>%
        select(model, term, Estimate, `Std. Error`, `Pr(>|t|)`)
    }
  )
}

key_lpm_post <- extract_key_coefs(models_lpm_post_selection)
key_lpm_exposure <- extract_key_coefs(models_lpm_exposure)
key_logit_post <- extract_key_coefs(models_logit_post_selection)
key_logit_exposure <- extract_key_coefs(models_logit_exposure)

write.xlsx(
  list(
    "LPM post selection" = key_lpm_post,
    "LPM exposure" = key_lpm_exposure,
    "Logit post selection" = key_logit_post,
    "Logit exposure" = key_logit_exposure
  ),
  "Output/lapop_key_coefficients.xlsx",
  overwrite = TRUE
)

# ---------------------------------------------------------------------------- #
# 17. Chequeos finales y base usada
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
    share_secundaria_completa_o_mas = wmean(secundaria_completa_o_mas, wt),
    share_superior_incompleta_o_mas = wmean(superior_incompleta_o_mas, wt),
    share_superior_completa = wmean(superior_completa, wt),
    mean_share_1936_1955 = wmean(share_1936_1955, wt),
    mean_share_1956_1978 = wmean(share_1956_1978, wt),
    .groups = "drop"
  )

write.xlsx(
  lapop_summary,
  "Output/lapop_summary_by_year_weighted.xlsx",
  overwrite = TRUE
)

write_csv(
  lapop,
  "Output/lapop_estimation_sample_with_controls.csv"
)

cat("Código terminado correctamente.\n")

# ---------------------------------------------------------------------------- #
# 18. Triple diferencia: post x variable x exposición municipal (Reordenar mas arriba)
# ---------------------------------------------------------------------------- #
# Pregunta:
# ¿El cambio post-2023 asociado a la exposición española municipal
# es distinto según características individuales?
#
# Generalización:
# post:X:share_1936_1955
# post:X:share_1956_1978

# ---------------------------------------------------------------------------- #
# 18.1 Variables para triple diferencia
# ---------------------------------------------------------------------------- #

triple_vars <- c(
  "blanco",
  "edad",
  "hombre",
  "rural",
  "superior_incompleta_o_mas",
  "desempleado",
  "ocupado",
  "estudiante",
  "en_pareja",
  "etnia_minoritaria",
  "izq_der",
  "identifica_partido",
  "voto_anterior",
  "interes_pol_mucho",
  "voto_blanco_nulo"
)

# Quedarse solo con las variables que existen en la base
triple_vars <- intersect(triple_vars, names(lapop))

cat("Variables usadas para triple diferencia:\n")
print(triple_vars)

# ---------------------------------------------------------------------------- #
# 18.2 Función auxiliar
# ---------------------------------------------------------------------------- #

run_triple_by_var_lpm <- function(x,
                                  data = lapop,
                                  exposure_1 = "share_1936_1955",
                                  exposure_2 = "share_1956_1978",
                                  controls = NULL) {
  
  rhs_main <- paste0(
    # Efectos post x exposición
    "post:", exposure_1,
    " + post:", exposure_2,
    
    # Interacciones dobles con la variable X
    " + post:", x,
    " + ", x, ":", exposure_1,
    " + ", x, ":", exposure_2,
    
    # Triples diferencias de interés
    " + post:", x, ":", exposure_1,
    " + post:", x, ":", exposure_2
  )
  
  rhs <- if (!is.null(controls) && controls != "") {
    paste(rhs_main, controls, sep = " + ")
  } else {
    rhs_main
  }
  
  run_lpm(
    rhs = rhs,
    data = data
  )
}

# ---------------------------------------------------------------------------- #
# 18.3 Correr modelos triple diferencia
# ---------------------------------------------------------------------------- #

models_lpm_triple_by_var <- map(
  triple_vars,
  ~ run_triple_by_var_lpm(
    x = .x,
    data = lapop,
    controls = NULL
  )
)

names(models_lpm_triple_by_var) <- triple_vars

# ---------------------------------------------------------------------------- #
# 18.4 Triple con controles básicos sin duplicar X
# ---------------------------------------------------------------------------- #

basic_control_vars <- c(
  "edad",
  "hombre",
  "rural",
  "etnia_minoritaria"
)

run_triple_by_var_lpm_controls <- function(x,
                                           data = lapop,
                                           exposure_1 = "share_1936_1955",
                                           exposure_2 = "share_1956_1978",
                                           control_vars = basic_control_vars) {
  
  controls_x <- setdiff(control_vars, x)
  
  controls_rhs <- if (length(controls_x) > 0) {
    paste(controls_x, collapse = " + ")
  } else {
    NULL
  }
  
  run_triple_by_var_lpm(
    x = x,
    data = data,
    exposure_1 = exposure_1,
    exposure_2 = exposure_2,
    controls = controls_rhs
  )
}

models_lpm_triple_by_var_controls <- map(
  triple_vars,
  ~ run_triple_by_var_lpm_controls(
    x = .x,
    data = lapop
  )
)

names(models_lpm_triple_by_var_controls) <- triple_vars

# ---------------------------------------------------------------------------- #
# 18.5 Exportar tabla completa
# ---------------------------------------------------------------------------- #

notes_triple_by_var <- paste(
  "Outcome is migration intention.",
  "Linear probability models.",
  "Survey weights used.",
  "Standard errors clustered at the municipality level.",
  "All models include year and municipality fixed effects.",
  "Each column estimates a triple difference with the variable named in the column.",
  "The coefficients of interest are post x variable x share_1936_1955 and post x variable x share_1956_1978."
)

modelsummary(
  models_lpm_triple_by_var,
  output = "Output/lapop_lpm_table5_triple_difference_by_variable.xlsx",
  stars = c("*" = .1, "**" = .05, "***" = .01),
  gof_map = gof_map_lpm,
  notes = notes_triple_by_var
)

modelsummary(
  models_lpm_triple_by_var_controls,
  output = "Output/lapop_lpm_table5_triple_difference_by_variable_controls.xlsx",
  stars = c("*" = .1, "**" = .05, "***" = .01),
  gof_map = gof_map_lpm,
  notes = paste(
    notes_triple_by_var,
    "Models include basic predetermined controls, excluding the interacted variable itself."
  )
)

# ---------------------------------------------------------------------------- #
# 19. Cambios post por variable: post x variable
# ---------------------------------------------------------------------------- #
# Pregunta:
# ¿Cambia en el período post-2023 la relación entre cada característica
# individual y la intención de migrar?
#
# Coeficiente de interés:
# post:X

# ---------------------------------------------------------------------------- #
# 19.1 Variables para interacciones post x variable
# ---------------------------------------------------------------------------- #

post_vars <- c(
  "blanco",
  "edad",
  "hombre",
  "rural",
  "superior_incompleta_o_mas",
  "desempleado",
  "ocupado",
  "estudiante",
  "en_pareja",
  "etnia_minoritaria",
  "interes_pol_mucho",
  "voto_blanco_nulo"
)

post_vars <- intersect(post_vars, names(lapop))

cat("Variables usadas para post x variable:\n")
print(post_vars)

# ---------------------------------------------------------------------------- #
# 19.2 Función auxiliar: post x variable sin controles
# ---------------------------------------------------------------------------- #

run_post_by_var_lpm <- function(x,
                                data = lapop,
                                controls = NULL) {
  
  rhs_main <- paste0(
    "post:", x,
    " + ", x
  )
  
  rhs <- if (!is.null(controls) && controls != "") {
    paste(rhs_main, controls, sep = " + ")
  } else {
    rhs_main
  }
  
  run_lpm(
    rhs = rhs,
    data = data
  )
}

# ---------------------------------------------------------------------------- #
# 19.3 Correr modelos post x variable sin controles
# ---------------------------------------------------------------------------- #

models_lpm_post_by_var <- map(
  post_vars,
  ~ run_post_by_var_lpm(
    x = .x,
    data = lapop,
    controls = NULL
  )
)

names(models_lpm_post_by_var) <- post_vars

# ---------------------------------------------------------------------------- #
# 19.4 Exportar tabla post x variable sin controles
# ---------------------------------------------------------------------------- #

notes_post_by_var <- paste(
  "Outcome is migration intention.",
  "Linear probability models.",
  "Survey weights used.",
  "Standard errors clustered at the municipality level.",
  "All models include year and municipality fixed effects.",
  "Each column estimates the interaction between post and the variable named in the column.",
  "The coefficient of interest is post x variable."
)

modelsummary(
  models_lpm_post_by_var,
  output = "Output/lapop_lpm_table6_post_by_variable.xlsx",
  stars = c("*" = .1, "**" = .05, "***" = .01),
  gof_map = gof_map_lpm,
  notes = notes_post_by_var
)

# ---------------------------------------------------------------------------- #
# 20. Cambios post por variable con controles básicos
# ---------------------------------------------------------------------------- #

post_control_vars_basic <- c(
  "edad",
  "hombre",
  "rural",
  "etnia_minoritaria"
)

run_post_by_var_lpm_controls <- function(x,
                                         data = lapop,
                                         control_vars = post_control_vars_basic) {
  
  controls_x <- setdiff(control_vars, x)
  
  controls_rhs <- if (length(controls_x) > 0) {
    paste(controls_x, collapse = " + ")
  } else {
    NULL
  }
  
  run_post_by_var_lpm(
    x = x,
    data = data,
    controls = controls_rhs
  )
}

# ---------------------------------------------------------------------------- #
# 20.1 Correr modelos post x variable con controles
# ---------------------------------------------------------------------------- #

models_lpm_post_by_var_controls <- map(
  post_vars,
  ~ run_post_by_var_lpm_controls(
    x = .x,
    data = lapop
  )
)

names(models_lpm_post_by_var_controls) <- post_vars

# ---------------------------------------------------------------------------- #
# 20.2 Exportar tabla post x variable con controles
# ---------------------------------------------------------------------------- #

notes_post_by_var_controls <- paste(
  "Outcome is migration intention.",
  "Linear probability models.",
  "Survey weights used.",
  "Standard errors clustered at the municipality level.",
  "All models include year and municipality fixed effects.",
  "Each column estimates the interaction between post and the variable named in the column.",
  "The coefficient of interest is post x variable.",
  "Models include basic predetermined controls, excluding the interacted variable itself."
)

modelsummary(
  models_lpm_post_by_var_controls,
  output = "Output/lapop_lpm_table7_post_by_variable_controls.xlsx",
  stars = c("*" = .1, "**" = .05, "***" = .01),
  gof_map = gof_map_lpm,
  notes = notes_post_by_var_controls
)

# ---------------------------------------------------------------------------- #
# 20.3 Extraer coeficientes clave: post x variable
# ---------------------------------------------------------------------------- #

extract_post_by_var_coefs <- function(models) {
  
  map_dfr(
    names(models),
    function(model_name) {
      
      coeftable_df <- as.data.frame(coeftable(models[[model_name]]))
      coeftable_df$term <- rownames(coeftable_df)
      coeftable_df$model <- model_name
      
      pattern <- paste0("post:", model_name)
      
      coeftable_df %>%
        filter(term == pattern) %>%
        mutate(variable = model_name) %>%
        select(variable, term, Estimate, `Std. Error`, `Pr(>|t|)`)
    }
  )
}

key_lpm_post_by_var <- extract_post_by_var_coefs(
  models_lpm_post_by_var
)

key_lpm_post_by_var_controls <- extract_post_by_var_coefs(
  models_lpm_post_by_var_controls
)

write.xlsx(
  list(
    "Post x variable" = key_lpm_post_by_var,
    "Post x variable controls" = key_lpm_post_by_var_controls
  ),
  "Output/lapop_key_coefficients_post_by_variable.xlsx",
  overwrite = TRUE
)

