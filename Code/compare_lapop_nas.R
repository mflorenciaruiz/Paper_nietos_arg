# =========================================================
# MAPAS DE MISSING POR MUNICIPIO — VARIABLES SELECCIONADAS
# Sin título general ni notas dentro del gráfico
# =========================================================

library(haven)
library(dplyr)
library(tidyr)
library(sf)
library(ggplot2)
library(readr)
library(grid)

# =========================================================
# 0. PATHS
# =========================================================

main <- "C:/Users/pilih/Documents/Papers German/Valerie/Paper_nietos_arg"

data_int <- file.path(main, "Data Int")
data_out <- file.path(main, "Data Out")
output   <- file.path(main, "Output")
map_dir  <- file.path(output, "maps")

dir.create(output, recursive = TRUE, showWarnings = FALSE)
dir.create(map_dir, recursive = TRUE, showWarnings = FALSE)

# =========================================================
# 1. CARGAR BASE LAPOP PREPARADA
# =========================================================

lapop <- read_dta(
  file.path(data_int, "lapop_remaining_mixed_tables_ready.dta")
) %>%
  mutate(
    mun_code = as.numeric(mun_code)
  ) %>%
  as.data.frame()

# =========================================================
# 2. VARIABLES QUE SE VAN A MAPEAR
# =========================================================

variables_interes <- c(
  "intencion_migrar",
  "izq_der",
  "voto_blanco_nulo"
)

etiquetas_variables <- c(
  intencion_migrar = "Migration intention",
  izq_der = "Left-right ideology",
  voto_blanco_nulo = "Blank/null vote"
)

# =========================================================
# 3. CHEQUEAR VARIABLES
# =========================================================

variables_requeridas <- c(
  "mun_code",
  "nom_prov_clean",
  "nom_mun_clean",
  variables_interes
)

variables_faltantes <- setdiff(variables_requeridas, names(lapop))

if (length(variables_faltantes) > 0) {
  stop(
    paste0(
      "No se encontraron estas variables en la base LAPOP: ",
      paste(variables_faltantes, collapse = ", ")
    )
  )
}

# =========================================================
# 4. PASAR LAPOP A FORMATO LARGO
# =========================================================

lapop_long <- lapop %>%
  select(
    mun_code,
    nom_prov_clean,
    nom_mun_clean,
    all_of(variables_interes)
  ) %>%
  pivot_longer(
    cols = all_of(variables_interes),
    names_to = "variable",
    values_to = "valor"
  )

# =========================================================
# 5. PROPORCIÓN DE MISSING POR MUNICIPIO Y VARIABLE
# =========================================================

missing_municipio <- lapop_long %>%
  group_by(
    mun_code,
    nom_prov_clean,
    nom_mun_clean,
    variable
  ) %>%
  summarise(
    observaciones = n(),
    observaciones_missing = sum(is.na(valor)),
    proporcion_missing = mean(is.na(valor)),
    porcentaje_missing = 100 * proporcion_missing,
    .groups = "drop"
  ) %>%
  mutate(
    variable_label = recode(variable, !!!etiquetas_variables),
    variable_label = factor(
      variable_label,
      levels = c(
        "Migration intention",
        "Left-right ideology",
        "Blank/null vote"
      )
    )
  )

write_csv(
  missing_municipio,
  file.path(
    output,
    "A10_missing_selected_variables_by_municipality.csv"
  )
)

# =========================================================
# 6. CARGAR GEOMETRÍAS MUNICIPALES
# =========================================================

mapa_base <- readRDS(
  file.path(data_out, "spanish_cohorts_arg.rds")
)

if (!inherits(mapa_base, "sf")) {
  stop("spanish_cohorts_arg.rds no es un objeto sf.")
}

mapa_base <- mapa_base %>%
  st_make_valid() %>%
  mutate(
    mun_code = as.numeric(mun_code)
  ) %>%
  select(
    mun_code,
    geometry
  ) %>%
  distinct(
    mun_code,
    .keep_all = TRUE
  )

# Chequeo: debe haber una geometría por municipio

geometrias_repetidas <- mapa_base %>%
  st_drop_geometry() %>%
  count(mun_code) %>%
  filter(n > 1)

if (nrow(geometrias_repetidas) > 0) {
  warning("Hay códigos municipales repetidos en el objeto espacial.")
}

# =========================================================
# 7. UNIR PROPORCIONES DE MISSING CON EL MAPA
# =========================================================

mapa_datos <- mapa_base %>%
  left_join(
    missing_municipio,
    by = "mun_code"
  ) %>%
  filter(
    !is.na(variable)
  )

# =========================================================
# 8. CREAR ESCALA DISCRETA DINÁMICA
# =========================================================

formatear_numero <- function(x) {
  if (abs(x - round(x)) < 0.000001) {
    return(as.character(round(x)))
  }
  
  format(
    round(x, 1),
    nsmall = 1,
    trim = TRUE,
    decimal.mark = "."
  )
}

crear_escala_missing <- function(maximo) {
  
  if (!is.finite(maximo) || maximo <= 0) {
    etiquetas <- "0%"
    colores <- setNames("#2d1e68", etiquetas)
    
    return(
      list(
        maximo = 0,
        limites = c(0),
        etiquetas = etiquetas,
        colores = colores
      )
    )
  }
  
  cortes_estandar <- c(1, 5, 10, 20)
  
  # Se conservan solamente los cortes que estén por debajo del máximo.
  cortes_intermedios <- cortes_estandar[cortes_estandar < maximo]
  
  # Límites para los valores estrictamente mayores que cero.
  limites_positivos <- unique(c(0, cortes_intermedios, maximo))
  
  etiquetas_intervalos <- character(length(limites_positivos) - 1)
  
  for (i in seq_len(length(limites_positivos) - 1)) {
    
    limite_inferior <- limites_positivos[i]
    limite_superior <- limites_positivos[i + 1]
    
    inferior_txt <- formatear_numero(limite_inferior)
    superior_txt <- formatear_numero(limite_superior)
    
    if (limite_inferior == 0) {
      etiquetas_intervalos[i] <- paste0(">0–", superior_txt, "%")
    } else {
      etiquetas_intervalos[i] <- paste0(
        inferior_txt,
        "–",
        superior_txt,
        "%"
      )
    }
  }
  
  etiquetas <- c("0%", etiquetas_intervalos)
  
  colores_base <- c(
    "#2d1e68",
    "#0f6da5",
    "#00a6a6",
    "#6fcdb2",
    "#c7e9c0",
    "#fff7bc"
  )
  
  colores <- grDevices::colorRampPalette(colores_base)(length(etiquetas))
  colores <- setNames(colores, etiquetas)
  
  list(
    maximo = maximo,
    limites = limites_positivos,
    etiquetas = etiquetas,
    colores = colores
  )
}

maximo_global <- max(
  mapa_datos$porcentaje_missing,
  na.rm = TRUE
)

escala_global <- crear_escala_missing(maximo_global)

# Clasificar los municipios según la escala dinámica.

mapa_datos <- mapa_datos %>%
  mutate(
    missing_cat = case_when(
      is.na(porcentaje_missing) ~ NA_character_,
      porcentaje_missing == 0 ~ "0%",
      porcentaje_missing > 0 ~ as.character(
        cut(
          porcentaje_missing,
          breaks = escala_global$limites,
          labels = escala_global$etiquetas[-1],
          include.lowest = FALSE,
          right = TRUE
        )
      )
    ),
    missing_cat = factor(
      missing_cat,
      levels = escala_global$etiquetas
    )
  )

# =========================================================
# 9. MAPA CONJUNTO
#    Sin título general, subtítulo ni nota dentro del gráfico
# =========================================================

mapa_conjunto <- ggplot() +
  
  # Fondo completo de municipios argentinos
  geom_sf(
    data = mapa_base,
    fill = "grey96",
    color = "grey70",
    linewidth = 0.10
  ) +
  
  # Municipios con observaciones LAPOP
  geom_sf(
    data = mapa_datos,
    aes(fill = missing_cat),
    color = "grey25",
    linewidth = 0.18
  ) +
  
  facet_wrap(
    ~ variable_label,
    ncol = 3,
    drop = FALSE
  ) +
  
  scale_fill_manual(
    values = escala_global$colores,
    breaks = escala_global$etiquetas,
    limits = escala_global$etiquetas,
    drop = FALSE,
    na.value = "grey96",
    name = "Missing share"
  ) +
  
  guides(
    fill = guide_legend(
      title.position = "top",
      title.hjust = 0.5,
      nrow = 1,
      byrow = TRUE
    )
  ) +
  
  theme_void(base_size = 12) +
  
  theme(
    strip.text = element_text(
      face = "bold",
      size = 12,
      margin = margin(b = 8)
    ),
    legend.position = "bottom",
    legend.direction = "horizontal",
    legend.title = element_text(
      face = "bold",
      size = 11
    ),
    legend.text = element_text(
      size = 10
    ),
    legend.key.width = unit(1.15, "cm"),
    legend.key.height = unit(0.45, "cm"),
    panel.spacing = unit(0.8, "lines"),
    plot.title = element_blank(),
    plot.subtitle = element_blank(),
    plot.caption = element_blank(),
    plot.margin = margin(
      t = 5,
      r = 5,
      b = 5,
      l = 5
    )
  )

print(mapa_conjunto)

# =========================================================
# 10. EXPORTAR FIGURA
# =========================================================

ggsave(
  filename = file.path(
    map_dir,
    "A10_missing_selected_variables_no_text.png"
  ),
  plot = mapa_conjunto,
  width = 12,
  height = 6.5,
  dpi = 300,
  bg = "white"
)

ggsave(
  filename = file.path(
    map_dir,
    "A10_missing_selected_variables_no_text.pdf"
  ),
  plot = mapa_conjunto,
  width = 12,
  height = 6.5,
  bg = "white"
)

# =========================================================
# 11. EXPORTAR RESUMEN DE RANGOS
# =========================================================

resumen_rangos <- missing_municipio %>%
  group_by(variable_label) %>%
  summarise(
    municipios_con_lapop = n(),
    municipios_con_algun_missing = sum(
      porcentaje_missing > 0,
      na.rm = TRUE
    ),
    minimo = min(
      porcentaje_missing,
      na.rm = TRUE
    ),
    mediana = median(
      porcentaje_missing,
      na.rm = TRUE
    ),
    maximo = max(
      porcentaje_missing,
      na.rm = TRUE
    ),
    .groups = "drop"
  )

write_csv(
  resumen_rangos,
  file.path(
    output,
    "A10_missing_selected_variables_ranges.csv"
  )
)

# =========================================================
# 12. MOSTRAR RESULTADOS EN LA CONSOLA
# =========================================================

cat("\nEscala usada en la figura:\n")
print(escala_global$etiquetas)

cat(
  "\nMáximo observado usado como límite superior: ",
  round(escala_global$maximo, 1),
  "%\n",
  sep = ""
)

cat("\nResumen por variable:\n")
print(resumen_rangos)

cat("\nArchivos creados:\n")

cat(
  file.path(
    map_dir,
    "A10_missing_selected_variables_no_text.png"
  ),
  "\n"
)

cat(
  file.path(
    map_dir,
    "A10_missing_selected_variables_no_text.pdf"
  ),
  "\n"
)

cat(
  file.path(
    output,
    "A10_missing_selected_variables_by_municipality.csv"
  ),
  "\n"
)

cat(
  file.path(
    output,
    "A10_missing_selected_variables_ranges.csv"
  ),
  "\n"
)