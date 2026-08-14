# ============================================================================
# EPH 2025 - Estadisticas descriptivas de transferencias (V12, V12_M)
# A nivel hogar.
# ============================================================================

library(eph)
library(dplyr)
library(purrr)

# ---------------------------------------------------------------------------- #
# 1. Descargar los 4 trimestres de 2025 para individuos Y hogares
# ---------------------------------------------------------------------------- #
#trimestres <- 1:4
trimestres <- 4

descargar <- function(t, type) {
  tryCatch(
    get_microdata(year = 2025, trimester = t, type = type),
    error = function(e) {
      message(sprintf("T%d 2025 [%s] no disponible: %s", t, type, e$message))
      NULL
    }
  )
}

ind_list <- map(trimestres, ~ descargar(.x, "individual"))
hog_list <- map(trimestres, ~ descargar(.x, "hogar"))

ind <- bind_rows(compact(ind_list))
hog <- bind_rows(compact(hog_list))

cat("Personas:", nrow(ind), "|| Hogares:", nrow(hog), "\n")

# ---------------------------------------------------------------------------- #
# 2. Agregar V12_M a nivel hogar (suma de los individuos)
# ---------------------------------------------------------------------------- #
v12m_hogar <- ind %>%
  group_by(CODUSU, NRO_HOGAR, ANO4, TRIMESTRE) %>%
  summarise(V12_M_hogar = sum(V12_M, na.rm = TRUE),
            .groups = "drop")

# ---------------------------------------------------------------------------- #
# 3. Flag "padres separados con hijos" a nivel hogar
#    Proxy: el hogar tiene al menos un hijo/hijastro (CH03 in 3, 4) y el jefe
#    (CH03 == 1) reporta estado civil "separado/divorciado" (CH07 == 3).
#     o "soltero/a" (CH07 == 5)
# ---------------------------------------------------------------------------- #
flag_hog <- ind %>%
  group_by(CODUSU, NRO_HOGAR, ANO4, TRIMESTRE) %>%
  summarise(
    tiene_hijo    = any(CH03 %in% c(3, 4), na.rm = TRUE),
    jefe_separado = any(CH03 == 1 & (CH07 == 3|CH07 == 5), na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(padres_sep_c_hijos = tiene_hijo & jefe_separado)

cat("\nHogares con padres separados y con hijos:",
    sum(flag_hog$padres_sep_c_hijos), "de", nrow(flag_hog),
    sprintf("(%.2f%%)\n", 100 * mean(flag_hog$padres_sep_c_hijos)))

# ---------------------------------------------------------------------------- #
# 4. Merge al archivo de hogares
# ---------------------------------------------------------------------------- #
hog <- hog %>%
  left_join(v12m_hogar, by = c("CODUSU","NRO_HOGAR","ANO4","TRIMESTRE")) %>%
  left_join(flag_hog,   by = c("CODUSU","NRO_HOGAR","ANO4","TRIMESTRE")) %>%
  mutate(V12_M_hogar = ifelse(is.na(V12_M_hogar), 0, V12_M_hogar),
         V12_M_hogar = as.numeric(ifelse(is.na(V12_M_hogar), 0, V12_M_hogar)),
         PONDERA     = as.numeric(PONDERA))

# ---------------------------------------------------------------------------- #
# 4.b Conversion a USD usando TC de Referencia BCRA (Com. A 3500, mayorista)
#     Fuente: BCRA - promedio trimestral simple.
#     https://www.bcra.gob.ar/PublicacionesEstadisticas/Cotizaciones_por_fecha.asp
#     Reemplazar los valores por los promedios oficiales una vez descargados.
# ---------------------------------------------------------------------------- #
tc_2025 <- tibble::tribble(
  ~ANO4, ~TRIMESTRE, ~tc_a3500,
  2025,  1,          1053,   # promedio ene-mar 2025 (aprox)
  2025,  2,          1160,   # promedio abr-jun 2025 (post-levantamiento cepo)
  2025,  3,          1330,   # promedio jul-sep 2025
  2025,  4,          1450    # promedio oct-dic 2025
)

hog <- hog %>%
  left_join(tc_2025, by = c("ANO4", "TRIMESTRE")) %>%
  mutate(V12_M_hogar_usd = V12_M_hogar / tc_a3500)

# ---------------------------------------------------------------------------- #
# 5. Chequeo de consistencia entre V12 (hogar) y V12_M (individuos)
#    Si algun miembro reporta monto positivo pero el hogar dice "no", imputamos "si".
# ---------------------------------------------------------------------------- #
inconsistentes <- hog %>%
  filter(V12_M_hogar > 0, V12 == 2 | is.na(V12)) %>%
  nrow()
cat("Hogares con V12_M_hogar > 0 pero V12 != 1:", inconsistentes,
    sprintf(" (%.2f%% del total)\n", 100 * inconsistentes / nrow(hog)))

hog <- hog %>%
  mutate(V12_corr = ifelse(V12_M_hogar > 0 & (V12 == 2 | is.na(V12)),
                           1, V12))

cat("Antes de la correccion: V12 == 1 en",
    sum(hog$V12 == 1, na.rm = TRUE), "hogares\n")
cat("Despues de la correccion: V12_corr == 1 en",
    sum(hog$V12_corr == 1, na.rm = TRUE), "hogares\n")

# ---------------------------------------------------------------------------- #
# 6. Funcion resumen (todo a nivel hogar, ponderador PONDERA del archivo hogar)
# ---------------------------------------------------------------------------- #
resumen <- function(d, etq) {
  
  d_cond <- d %>% filter(V12_corr == 1, V12_M_hogar > 0)
  monto_cond_ars <- if (nrow(d_cond) > 0) {
    weighted.mean(as.numeric(d_cond$V12_M_hogar),
                  w = as.numeric(d_cond$PONDERA), na.rm = TRUE)
  } else NA_real_
  monto_cond_usd <- if (nrow(d_cond) > 0) {
    weighted.mean(as.numeric(d_cond$V12_M_hogar_usd),
                  w = as.numeric(d_cond$PONDERA), na.rm = TRUE)
  } else NA_real_
  
  d %>%
    summarise(
      muestra                = etq,
      n_hogares              = n(),
      pct_recibe_v12         = weighted.mean(V12_corr == 1,
                                             w = as.numeric(PONDERA),
                                             na.rm = TRUE) * 100,
      monto_prom_incond_ars  = weighted.mean(as.numeric(V12_M_hogar),
                                             w = as.numeric(PONDERA),
                                             na.rm = TRUE),
      monto_prom_incond_usd  = weighted.mean(as.numeric(V12_M_hogar_usd),
                                             w = as.numeric(PONDERA),
                                             na.rm = TRUE),
      monto_prom_cond_ars    = monto_cond_ars,
      monto_prom_cond_usd    = monto_cond_usd
    )
}

# ---------------------------------------------------------------------------- #
# 7. Total vs excluyendo hogares con padres separados y con hijos
# ---------------------------------------------------------------------------- #
tabla <- bind_rows(
  resumen(hog,                                 "Total hogares"),
  resumen(hog %>% filter(!padres_sep_c_hijos), "Sin padres separados con hijos")
)

print(tabla)

# ---------------------------------------------------------------------------- #
# 8. (Opcional) Exportar
# ---------------------------------------------------------------------------- #
# library(writexl)
# write_xlsx(tabla, "Output/eph2025_transferencias_hogar.xlsx")