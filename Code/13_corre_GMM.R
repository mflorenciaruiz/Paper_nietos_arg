install.packages("plm")

library(plm)
library(dplyr)

# ============================================================
# 0. Rutas y data
# ============================================================

# Definir el path a la carpeta del proyecto
path <- "/Users/florenciaruiz/BID 2/Paper Valerie/Nietos/Argentina/Paper_nietos_arg"
setwd(path)

dip_nac_mun <- read_csv("Data Out/dip_nac_mun.csv")

# ============================================================
# 1. Preparar el panel: unit = mun_code x tipo_eleccion
# ============================================================

data_panel <- dip_nac_mun %>%
  mutate(
    unit_id         = paste(mun_code, tipo_eleccion, sep = "_"),
    post_share_1936 = post * share_1936_1955,
    post_share_1956 = post * share_1956_1978
  )

# Chequeo de unicidad: cada (unit_id, anio) debe tener una sola fila
data_panel %>%
  count(unit_id, anio) %>%
  filter(n > 1) %>%
  nrow()   # 0

# Chequeo de balance
data_panel %>%
  count(unit_id) %>%
  summarise(min = min(n), max = max(n), mean = mean(n)) # poco desbalanceado

data_pdata <- pdata.frame(data_panel, index = c("unit_id", "anio"))

# ============================================================
# 2. Unit root test sobre el outcome
# ============================================================

# Blank vote share
ur_blank <- purtest(porcentaje_blanco ~ 1,
                    data = data_pdata,
                    test = "madwu", lags = 1, exo = "intercept")
print(ur_blank)

# Turnout
ur_turnout <- purtest(participacion ~ 1,
                      data = data_pdata,
                      test = "madwu", lags = 1, exo = "intercept")
print(ur_turnout)

# ============================================================
# 3. System GMM para blank vote share
# ============================================================

mod_sys_blank <- pgmm(
  porcentaje_blanco ~ lag(porcentaje_blanco, 1) + post_share_1936 + post_share_1956
  | lag(porcentaje_blanco, 2:4)
  | post_share_1936 + post_share_1956,
  data           = data_pdata,
  effect         = "individual",
  model          = "twosteps",
  transformation = "ld",
  robust         = TRUE
)

summary(mod_sys_blank)

data_panel %>%
  group_by(unit_id) %>%
  summarise(
    var_y     = var(porcentaje_blanco, na.rm = TRUE),
    var_post  = var(post_share_1936, na.rm = TRUE)
  ) %>%
  filter(is.na(var_y) | var_y == 0 | is.na(var_post) | var_post == 0)

# ============================================================
# 4. System GMM para turnout
# ============================================================

mod_sys_turnout <- pgmm(
  participacion ~ lag(participacion, 1) + post_share_1936 + post_share_1956
  | lag(participacion, 2:99)
  | post_share_1936 + post_share_1956,
  data           = data_pdata,
  effect         = "twoways",
  model          = "twosteps",
  transformation = "ld",
  robust         = TRUE
)

summary(mod_sys_turnout)

# Pruebo filtrando
data_panel_clean <- data_panel %>%
  filter(!(share_1936_1955 == 0 & share_1956_1978 == 0))

data_pdata_clean <- pdata.frame(data_panel_clean, index = c("unit_id", "anio"))

# Reintentar con pocos lags
mod_sys_test <- pgmm(
  porcentaje_blanco ~ lag(porcentaje_blanco, 1) + post_share_1936 + post_share_1956
  | lag(porcentaje_blanco, 2:3)
  | post_share_1936 + post_share_1956,
  data = data_pdata_clean,
  effect = "individual",     # SIN year FE por ahora
  model = "twosteps",
  transformation = "ld",
  robust = TRUE
)

# ============================================================
# 5. Extraer los diagnosticos para la nota al pie
# ============================================================

extract_diagnostics <- function(mod, ur) {
  s <- summary(mod)
  tibble::tibble(
    Test           = c("Fisher unit root (H0: nonstationary)",
                       "AR(1) test (H0: no autocorrelation)",
                       "AR(2) test (H0: no autocorrelation)",
                       "Sargan test (H0: instruments valid)"),
    Statistic      = c(ur$statistic$statistic,
                       s$m1$statistic,
                       s$m2$statistic,
                       s$sargan$statistic),
    p_value        = c(ur$statistic$p.value,
                       s$m1$p.value,
                       s$m2$p.value,
                       s$sargan$p.value),
    Interpretation = c("Reject → stationary (required for System GMM)",
                       "Reject expected (sanity check)",
                       "NOT reject → original errors uncorrelated",
                       "NOT reject → instruments valid")
  )
}

diag_blank   <- extract_diagnostics(mod_sys_blank, ur_blank)
diag_turnout <- extract_diagnostics(mod_sys_turnout, ur_turnout)

print(diag_blank)
print(diag_turnout)

# ============================================================
# 6. (Opcional) Guardar resultados para el paper
# ============================================================

# Exportar coeficientes
library(broom)
coefs_blank   <- broom::tidy(mod_sys_blank, conf.int = TRUE)
coefs_turnout <- broom::tidy(mod_sys_turnout, conf.int = TRUE)

# Guardar diagnosticos y coeficientes
saveRDS(
  list(
    models      = list(blank = mod_sys_blank, turnout = mod_sys_turnout),
    coefs       = list(blank = coefs_blank,   turnout = coefs_turnout),
    diagnostics = list(blank = diag_blank,    turnout = diag_turnout)
  ),
  "Output/gmm_dynamic_results.rds"
)