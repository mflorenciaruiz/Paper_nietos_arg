install.packages(c("httr2", "jsonlite"))

library(httr2)
library(jsonlite)
library(dplyr)
library(tidyr)
library(purrr)
library(sf)
library(readxl)
library(Hmisc)
library(readr)
library(writexl)

options(scipen = 999)

# ---------------------- #
#   Padrón electoral
# ---------------------- #
{
padron <- readxl::read_excel("Data raw/Elecciones Argentina/Padron_2017.xlsx")

unique(padron$distrito)
length(unique(padron$secc))
length(unique(padron$codigo))
length(unique(padron$local))

keys <- padron %>% 
  select(distrito, nombre_distrito, secc, seccion) %>%
  mutate(
    # codigo de seccion (distrito+seccion)
    cod_seccion = paste0(distrito, secc)
  ) %>% 
  distinct(distrito, secc, nombre_distrito, seccion, cod_seccion) %>% 
  rename(
    distrito_id = distrito,
    seccion_id  = secc
  )
}
# ---------------------- #
#        API
# ---------------------- #
{
### Prueba
url <- "https://resultados.mininterior.gob.ar/api/resultados/getResultados"

resp <- request(url) %>% 
  req_url_query(
    anioEleccion = 2019,
    tipoRecuento = 2,
    tipoEleccion = 2,
    categoriaId = 1,
    distritoId = 2,
    seccionId = 1
  ) %>% 
  req_perform()

data_prueba <- resp %>%  resp_body_json()

str(data_prueba) # cuando no se especifica distritoId, devuelve un listado nacional.
str(data, max.level = 2)
names(data_prubea)

out_main <- tibble(
  distritoId = 2,
  seccionId  = 1,
  fechaTotalizacion = data_prueba$fechaTotalizacion,
  mesasTotalizadas  = data_prueba$estadoRecuento$mesasTotalizadas,
  cantidadElectores = data_prueba$estadoRecuento$cantidadElectores,
  cantidadVotantes  = data_prueba$estadoRecuento$cantidadVotantes,
  participacionPorcentaje = data_prueba$estadoRecuento$participacionPorcentaje,
  votosNulos   = data_prueba$valoresTotalizadosOtros$votosNulos,
  votosEnBlanco = data_prueba$valoresTotalizadosOtros$votosEnBlanco,
  votosRecurridosImpugnados = data_prueba$valoresTotalizadosOtros$votosRecurridosComandoImpugnados
)

pos <- data_prueba$valoresTotalizadosPositivos
out_parties <- tibble(
  distritoId = 2,
  seccionId  = 1,
  idAgrupacion = map_chr(pos, ~ .x$idAgrupacion %||% NA_character_),
  nombreAgrupacion = map_chr(pos, ~ .x$nombreAgrupacion %||% NA_character_),
  votos = map_dbl(pos, ~ as.numeric(.x$votos %||% NA_real_)),
  votosPorcentaje = map_dbl(pos, ~ as.numeric(.x$votosPorcentaje %||% NA_real_))
)


### Función para trater los datos

# 1) Función: llama a la API para (distritoId, seccionId)
fetch_seccion <- function(distrito_id, 
                          seccion_id, 
                          anio_eleccion,
                          tipo_recuento = 1, # PROVISIONAL (la api solo tiene provisional)
                          tipo_eleccion = 2, # PASO
                          categoria_id  = 3) { # Cargo (1 = presidente, 2 = senador, 3 = diputado)
  
  resp <- request(url) %>%
    req_url_query(
      anioEleccion = anio_eleccion,
      tipoRecuento = tipo_recuento,
      tipoEleccion = tipo_eleccion,
      categoriaId  = categoria_id,
      distritoId   = distrito_id,
      seccionId    = seccion_id
    ) %>%
    req_perform()
  
  x <- resp %>% resp_body_json()
  
  # --- Parte A: resumen
  out_main <- tibble(
    distrito_id = distrito_id,
    seccion_id  = seccion_id,
    fecha_totalizacion   = x$fechaTotalizacion %||% NA_character_,
    mesas_totalizadas    = x$estadoRecuento$mesasTotalizadas %||% NA_real_,
    cantidad_electores   = x$estadoRecuento$cantidadElectores %||% NA_real_,
    cantidad_votantes    = x$estadoRecuento$cantidadVotantes %||% NA_real_,
    participacion_pct    = x$estadoRecuento$participacionPorcentaje %||% NA_real_,
    votos_nulos          = x$valoresTotalizadosOtros$votosNulos %||% NA_real_,
    votos_nulos_pct      = x$valoresTotalizadosOtros$votosNulosPorcentaje %||% NA_real_,
    votos_blanco         = x$valoresTotalizadosOtros$votosEnBlanco %||% NA_real_,
    votos_blanco_pct     = x$valoresTotalizadosOtros$votosEnBlancoPorcentaje %||% NA_real_,
    votos_impugnados     = x$valoresTotalizadosOtros$votosRecurridosComandoImpugnados %||% NA_real_,
    votos_impugnados_pct = x$valoresTotalizadosOtros$votosRecurridosComandoImpugnadosPorcentaje %||% NA_real_
  )
  
  # --- Parte B: votos por agrupación (itero sobre las listas dentro de valoresTotalizadosPositivos)
  pos <- x$valoresTotalizadosPositivos
  out_parties <- tibble(
    distrito_id        = distrito_id,
    seccion_id         = seccion_id,
    id_agrupacion     = map_chr(pos, ~ .x$idAgrupacion %||% NA_character_),
    nombre_agrupacion = map_chr(pos, ~ .x$nombreAgrupacion %||% NA_character_),
    votos             = map_dbl(pos, ~ as.numeric(.x$votos %||% NA_real_)),
    votos_pct         = map_dbl(pos, ~ as.numeric(.x$votosPorcentaje %||% NA_real_))
  )
  
  list(main = out_main, parties = out_parties)
}

# helper: %||% para NAs. Creo el operador que devuelve NA si el valor es null, y el valor mismo si no es null. Esto es útil porque la API a veces devuelve null en lugar de NA, y queremos estandarizarlo. 
`%||%` <- function(a, b) if (!is.null(a)) a else b

# 3) Ejecuto la función con un loop sobre todas las secciones. Devuelve una lista con los dos datasets de cada seccion elecoral (main y parties)
res <- pmap(
  keys %>% select(distrito_id, seccion_id),
  ~ fetch_seccion(..1, ..2, anio_eleccion = 2015, tipo_recuento = 1, tipo_eleccion = 2, categoria_id = 3)
)

# 4) Uno los resultados
main_all <- bind_rows(map(res, "main")) %>%             # combina los mains de cada sección en un solo dataset 
  left_join(keys, by = c("distrito_id", "seccion_id"))  # agrega la información de distrito y sección desde keys

parties_all <- bind_rows(map(res, "parties")) %>%
  left_join(keys, by = c("distrito_id", "seccion_id"))

# main_all: una fila por sección
# parties_all: varias filas por sección (una por agrupación)

# 5) Bajar varios años
years <- c(2015, 2017, 2019, 2021, 2023, 2025) # años con elecciones de diputados nacionales

out_years <- map(years, function(y){
  # scraping dentro de cada año (itera sobre secciones)
  # res es una lista con los resultados para todas las secciones de un año. 
  res <- pmap(
    keys %>% select(distrito_id, seccion_id),
    ~ fetch_seccion(..1, ..2,
                    anio_eleccion = y,
                    tipo_recuento = 1,
                    tipo_eleccion = 2,
                    categoria_id  = 3)
  )
  list(
    year = y,
    main    = dplyr::bind_rows(purrr::map(res, "main"))    %>% dplyr::mutate(year = y),
    parties = dplyr::bind_rows(purrr::map(res, "parties")) %>% dplyr::mutate(year = y)
  )
})

main_panel <- bind_rows(map(out_years, "main"))
parties_panel <- bind_rows(map(out_years, "parties"))

psych::describe(main_panel)
skimr::skim(main_panel)
}
# ---------------------- #
#         Data CP
# ---------------------- #
{
mapa <- st_read("Argentina/Data Raw/Elecciones/CP/geo_x_seccion_arg.geojson")

dip_nac <- read_xlsx("Argentina/Data Raw/Elecciones/CP/datos_electorales_diputados_nacionales_1772828173306.xlsx")

dip_nac <- dip_nac %>%
  separate(Elecciones, into = c("tipo_eleccion", "anio"), sep = " ") %>% 
  mutate(anio = as.numeric(anio)) %>%
  janitor::clean_names() %>% 
  arrange(anio, provincia, seccion, tipo_eleccion) %>% 
  select(anio, provincia, seccion, tipo_eleccion, everything())

# graficar mapa
ggplot()+
  geom_sf(data = mapa, fill = "lightblue", color = "white") +
  theme_void()

# Corrijo el problema con las elecciones de 2021 de Tierra del Fuego (no están bien puestos los nombres de las secciones, las identifico por la cantidad de electores y votantes)
chequeo <- dip_nac %>% filter(provincia == "TIERRA DEL FUEGO") 
table(chequeo$anio, chequeo$seccion) # Tolhuin no existia antes de 2021. El unico problema es el de 2021

dip_nac <- dip_nac %>% 
  mutate(
    seccion = case_when(
      provincia == "TIERRA DEL FUEGO" & tipo_eleccion == "GENERALES" & anio==2021 & electores == 73881 ~ "Rio Grande",
      provincia == "TIERRA DEL FUEGO" & tipo_eleccion == "GENERALES" & anio==2021 & electores == 62298 ~ "Ushuaia",
      provincia == "TIERRA DEL FUEGO" & tipo_eleccion == "GENERALES" & anio==2021 & electores == 5168 ~ "Tolhuin",
      
      provincia == "TIERRA DEL FUEGO" & tipo_eleccion == "PASO" & anio==2021 & electores == 73591 ~ "Rio Grande",
      provincia == "TIERRA DEL FUEGO" & tipo_eleccion == "PASO" & anio==2021 & electores == 61744 ~ "Ushuaia",
      provincia == "TIERRA DEL FUEGO" & tipo_eleccion == "PASO" & anio==2021 & electores == 4880 ~ "Tolhuin",
      
      TRUE ~ seccion
    ),
    id = case_when(
      provincia == "TIERRA DEL FUEGO" & tipo_eleccion == "GENERALES" & anio==2021 & electores == 73881 ~ "TIERRA DEL FUEGO_RIO GRANDE",
      provincia == "TIERRA DEL FUEGO" & tipo_eleccion == "GENERALES" & anio==2021 & electores == 62298 ~ "TIERRA DEL FUEGO_USHUAIA",
      provincia == "TIERRA DEL FUEGO" & tipo_eleccion == "GENERALES" & anio==2021 & electores == 5168 ~ "TIERRA DEL FUEGO_TOLHUIN",
      
      provincia == "TIERRA DEL FUEGO" & tipo_eleccion == "PASO" & anio==2021 & electores == 73591 ~ "TIERRA DEL FUEGO_RIO GRANDE",
      provincia == "TIERRA DEL FUEGO" & tipo_eleccion == "PASO" & anio==2021 & electores == 61744 ~ "TIERRA DEL FUEGO_USHUAIA",
      provincia == "TIERRA DEL FUEGO" & tipo_eleccion == "PASO" & anio==2021 & electores == 4880 ~ "TIERRA DEL FUEGO_TOLHUIN",
      
      TRUE ~ id      
    )
  )

# La paso a nivel de elección (cada elección se identifica por: anio + tipo_eleccion + provincia + seccion + id)

# Separo votos especiales (blancos, nulos, impugnados)
votos_especiales <- dip_nac %>%
  filter(partido %in% c("BLANCO", "NULO", "IMPUGNADO")) %>%
  mutate(
    tipo = case_when(
      partido == "BLANCO" ~ "blanco",
      partido == "NULO" ~ "nulo",
      partido == "IMPUGNADO" ~ "impugnado"
    )
  ) %>%
  select(anio, tipo_eleccion, provincia, seccion, id,
         tipo, votos, porcentaje) %>% 
  group_by(anio, tipo_eleccion, provincia, seccion, id) %>%
  mutate(
    chequeo = row_number()
  )
table(votos_especiales$chequeo) # no hay secciones con más de tres filas, bien
votos_especiales <- votos_especiales %>% 
  select(-chequeo)

# Paso a wide
votos_especiales <- votos_especiales %>%
  pivot_wider(
    names_from = tipo,
    values_from = c(votos, porcentaje),
    names_glue = "{.value}_{tipo}"
  ) %>% 
  mutate(
    votos_blanco = as.numeric(votos_blanco),
    porcentaje_blanco = as.numeric(porcentaje_blanco),
    votos_nulo = as.numeric(votos_nulo),
    porcentaje_nulo = as.numeric(porcentaje_nulo),
    votos_impugnado = as.numeric(votos_impugnado),
    porcentaje_impugnado = as.numeric(porcentaje_impugnado)
  ) %>% 
  arrange(anio, tipo_eleccion, provincia, seccion, id)

# Separo la data de partidos
votos_partidos <- dip_nac %>%
  filter(!partido %in% c("BLANCO", "NULO", "IMPUGNADO")) %>% 
  group_by(anio, tipo_eleccion, provincia, seccion, id) %>%
  arrange(desc(votos), .by_group = TRUE) %>%
  mutate(
    party_num = row_number(),
    party_num = str_pad(party_num, 2, pad = "0")
  ) %>%
  ungroup() %>% 
  mutate(
    votos = as.numeric(votos),
    porcentaje = as.numeric(porcentaje),
    participacion = as.numeric(participacion),
    electores = as.numeric(electores),
    votantes = as.numeric(votantes)
  )

table(votos_partidos$party_num) # hay hasta 25 partidos

# Paso a wide
votos_partidos <- votos_partidos %>%
  pivot_wider(
    id_cols = c(anio, tipo_eleccion, provincia, seccion, id,
                participacion, electores, votantes, ganador),
    names_from = party_num,
    values_from = c(partido, votos, porcentaje),
    names_glue = "{.value}_{party_num}",
    values_fill = list(votos = 0, porcentaje = 0)
  )
rowSums(select(votos_partidos, starts_with("porcentaje_")), na.rm = TRUE) # chequeo que todos sumen aprox 100

# Unir todo
dip_nac_wide <- votos_partidos %>%
  left_join(
    votos_especiales,
    by = c("anio", "tipo_eleccion", "provincia", "seccion", "id")
  ) %>% 
  select(anio, tipo_eleccion, provincia, seccion, id,
         participacion, electores, votantes, ganador, 
         votos_blanco, porcentaje_blanco, votos_impugnado, porcentaje_impugnado, votos_nulo, porcentaje_nulo,
         everything())

# Data más chica sin partidos
dip_nac_wide_small <- dip_nac_wide %>%
  select(-matches("_(\\d+)$")) %>% 
  arrange(tipo_eleccion, provincia, seccion, id, anio)

# Guardo los nombres de los partidos para clasificarlos según ideología
nombres_partidos <- dip_nac %>% 
  select(partido, provincia, anio) %>% 
  distinct() %>% 
  filter(partido != "BLANCO", partido != "NULO", partido != "IMPUGNADO")

# chequeo duplicados en partido-anio
nombres_partidos %>% 
  count(partido, anio) %>% 
  filter(n > 1)

write_xlsx(nombres_partidos, "Argentina/Data Out/nombres_partidos.xlsx")

# Importo los partidos ya clasificados
nombres_partidos <- read_xlsx("Argentina/Data Out/nombres_partidos_clasificado.xlsx")

# Uno la clasificación de partidos a la data de diputados long
dip_nac_partidos <- dip_nac %>% 
  left_join(nombres_partidos, by = c("partido", "provincia", "anio")) %>% 
  select(anio, tipo_eleccion, provincia, seccion, id, partido, clasificacion_binaria,	
         clasificacion_desagregada, familia_politica, everything()) %>% 
  arrange(anio, tipo_eleccion, provincia, seccion, id)

## Variables ideológicas a nivel sección (elección-distrito) 

# 1. Asegurar tipos numéricos y manejar NAs en clasificación
dip_nac_clasif <- dip_nac_partidos %>% 
  mutate(
    votos = as.numeric(votos),
    clasificacion_binaria = if_else(
      is.na(clasificacion_binaria), "no_clasif", clasificacion_binaria
    ),
    clasificacion_desagregada = if_else(
      is.na(clasificacion_desagregada), "no_clasif", clasificacion_desagregada
    ),
    familia_politica = if_else(
      is.na(familia_politica), "otro", familia_politica
    )
  )

# 2. Construir variables para índices ideológicos (excluyendo BLANCO/NULO/IMPUGNADO)
shares_sec <- dip_nac_clasif %>% 
  filter(!partido %in% c("BLANCO", "NULO", "IMPUGNADO")) %>% 
  group_by(anio, tipo_eleccion, provincia, seccion, id) %>% 
  summarise(
    # Total de votos válidos a partidos
    votos_validos        = sum(votos, na.rm = TRUE),
    
    # Votos por bloque ideológico (5 categorías + no clasificados)
    votos_izq            = sum(votos[clasificacion_desagregada == "izquierda"],        na.rm = TRUE),
    votos_cen_izq        = sum(votos[clasificacion_desagregada == "centro-izquierda"], na.rm = TRUE),
    votos_centro         = sum(votos[clasificacion_desagregada == "centro"],            na.rm = TRUE),
    votos_cen_der        = sum(votos[clasificacion_desagregada == "centro-derecha"],   na.rm = TRUE),
    votos_der            = sum(votos[clasificacion_desagregada == "derecha"],           na.rm = TRUE),
    votos_no_clasif      = sum(votos[clasificacion_desagregada == "no_clasif"],         na.rm = TRUE),
    
    # Votos por familia política
    votos_peronistas     = sum(votos[familia_politica == "peronista"], na.rm = TRUE),
    votos_radicales      = sum(votos[familia_politica == "radical"],   na.rm = TRUE),
    
    .groups = "drop"
  ) 

## Incumbencia / voto al oficialismo

# 1. Función para identificar oficialismo nacional según año (mapeo por nombre)
es_oficialismo <- function(partido, anio) {
  p <- toupper(partido)
  dplyr::case_when(
    # Kirchnerismo (2011-2015)
    anio %in% c(2011, 2013, 2015) & 
      stringr::str_detect(p, "PARA LA VICTORIA|DE LA VICTORIA") ~ TRUE,
    # Macrismo / Cambiemos (2017)
    anio == 2017 & 
      stringr::str_detect(p, "CAMBIEMOS|^FRENTE CAMBIA|\\bCAMBIA \\b") ~ TRUE,
    # JxC (2019)
    anio == 2019 & 
      stringr::str_detect(p, "JUNTOS POR EL CAMBIO|\\bCAMBIEMOS\\b|\\bCAMBIA \\b") ~ TRUE,
    # Frente de Todos (2021)
    anio == 2021 & 
      stringr::str_detect(p, "FRENTE DE TODOS") ~ TRUE,
    # Unión por la Patria (2023)
    anio == 2023 & 
      stringr::str_detect(p, "UNION POR LA PATRIA|UNIÓN POR LA PATRIA") ~ TRUE,
    # Mileismo / LLA (2025)
    anio == 2025 & 
      stringr::str_detect(p, "LA LIBERTAD AVANZA|LIBERTAD AVANZA") ~ TRUE,
    TRUE ~ FALSE
  )
}

# 2. Votos al oficialismo nacional por sección-elección
oficialismo_sec <- dip_nac_clasif %>% 
  filter(!partido %in% c("BLANCO", "NULO", "IMPUGNADO")) %>% 
  mutate(es_ofi = es_oficialismo(partido, anio)) %>% 
  group_by(anio, tipo_eleccion, provincia, seccion, id) %>% 
  summarise(
    votos_oficialismo = sum(votos[es_ofi], na.rm = TRUE),
    .groups = "drop"
  )

# 3. Identificar ganador y segundo más votado por sección-elección
top_partidos_sec <- dip_nac_clasif %>% 
  filter(!partido %in% c("BLANCO", "NULO", "IMPUGNADO")) %>% 
  group_by(anio, tipo_eleccion, provincia, seccion, id) %>% 
  arrange(desc(votos), .by_group = TRUE) %>% 
  summarise(
    ganador_partido     = dplyr::first(partido),
    ganador_clasif      = dplyr::first(clasificacion_desagregada),
    ganador_clasif_bin  = dplyr::first(clasificacion_binaria),
    ganador_familia     = dplyr::first(familia_politica),
    votos_1ro           = dplyr::first(votos),
    votos_2do           = dplyr::nth(votos, 2, default = 0),
    .groups = "drop"
  )

# 4. NEP (Laakso-Taagepera) por sección-elección
nep_sec <- dip_nac_clasif %>% 
  filter(!partido %in% c("BLANCO", "NULO", "IMPUGNADO")) %>% 
  group_by(anio, tipo_eleccion, provincia, seccion, id) %>% 
  summarise(
    nep = 1 / sum(porcentaje^2, na.rm = TRUE),
    .groups = "drop"
  )


# Unir todo en una sola data

# 1. Uno los data sets de indicadores políticos (incumbencia e indicadores ideológicos)
ind_pol <- oficialismo_sec %>%
  left_join(shares_sec, by = c("anio", "tipo_eleccion", "provincia", "seccion", "id")) %>% 
  select(anio, tipo_eleccion, provincia, seccion, id, everything())

# 2. Uno la data de indicadores políticos con la data de elecciones sin partidos (que tiene los votos en blanco, nulos, impugnados)
dip_nac_wide_small <- dip_nac_wide_small %>% 
  left_join(
    ind_pol,
    by = c("anio", "tipo_eleccion", "provincia", "seccion", "id")
  ) %>% 
  arrange(tipo_eleccion, provincia, seccion, id, anio)

# Chequeos
summarise(dip_nac_wide_small, 
          total_secciones = n(),
          secciones_con_ind_pol = sum(!is.na(votos_oficialismo)),
          secciones_sin_ind_pol = sum(is.na(votos_oficialismo))
)

describe(dip_nac_wide_small$votos_radicales)
describe(dip_nac_wide_small$votos_peronistas)
describe(dip_nac_wide_small$votos_no_clasif) # todos son clasificados

}
# ------------------------ #
# Merge con data del censo
# ------------------------ #
{
# spanish_cohorts_arg <- readRDS("Data Out/Argentina/spanish_cohorts_arg.rds") # Activar si no tengo la data en la memoria

### Chequeo antes de hacer el merge ###

  # 1. Asegurar misma proyección y arreglar geometrías inválidas
mapa <- st_transform(mapa, st_crs(spanish_cohorts_arg))

spanish_cohorts_arg <- st_make_valid(spanish_cohorts_arg)
mapa <- st_make_valid(mapa)

  # 2. Quedarse solo con las variables identificadoras
mun_sf_small <- spanish_cohorts_arg %>% select(mun_code)
sec_sf_small <- mapa %>% select(id)

  # 3. Intersección espacial
intersections <- st_intersection(mun_sf_small, sec_sf_small)

  # 4. Calcular área de cada intersección
intersections <- intersections %>%
  mutate(area_intersection = st_area(.))

  # 5. Área total de cada municipio y sección
mun_areas <- mun_sf_small %>%
  mutate(area_mun = st_area(.)) %>%
  st_drop_geometry()

sec_areas <- sec_sf_small %>%
  mutate(area_sec = st_area(.)) %>%
  st_drop_geometry()

  # 6. Agregar áreas totales y shares de overlap
intersections <- intersections %>%
  left_join(mun_areas, by = "mun_code") %>%
  left_join(sec_areas, by = "id") %>%
  mutate(
    share_mun = as.numeric(area_intersection / area_mun),
    share_sec = as.numeric(area_intersection / area_sec)
  )

  # 7. Diagnóstico 1 (por municipio): cuántas secciones intersectan cada municipio
diag_mun <- intersections %>%
  st_drop_geometry() %>%
  group_by(mun_code) %>%
  summarise(
    n_sections = n_distinct(id),
    max_share_mun = max(share_mun, na.rm = TRUE),  # qué porcentaje del municipio cae en la sección que más lo cubre?
    .groups = "drop"
  )

  # 8. Diagnóstico 2: cuántos municipios intersecta cada sección
diag_sec <- intersections %>%
  st_drop_geometry() %>%
  group_by(id) %>%
  summarise(
    n_municipios = n_distinct(mun_code),
    max_share_sec = max(share_sec, na.rm = TRUE),  # qué porcentaje de la sección cae en el municipio principal?
    .groups = "drop"
  )

  # 9. Resúmenes rápidos
table(diag_mun$n_sections)
table(diag_sec$n_municipios)

summary(diag_mun$max_share_mun)
summary(diag_sec$max_share_sec)

  # Ningún municipio está en una sola sección, la mayoría tiene entre 5 y 9 secciones --> las secciones electorales son subdivisiones de los municipios.
  # Muchas secciones cruzan varios municipios --> las secciones electorales no siguen exactamente los límites municipales.
  # La mitad de los municipios tiene una sección que cubre ≥97% del municipio
  # Las secciones están casi completamente dentro de un municipio.

  # Asigno a cada sección el municipio con el que tiene mayor intersección (share_mun)
crosswalk_sec_mun <- intersections %>%
  st_drop_geometry() %>%
  group_by(id) %>%
  slice_max(area_intersection, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  select(id, mun_code, share_sec, share_mun)

  # Verificar que cada id quedo una sola vez 
any(duplicated(crosswalk_sec_mun$id))

  # Ver cuántos id del shapefile de secciones quedaron matcheados
n_distinct(mapa$id) # 529
n_distinct(crosswalk_sec_mun$id) # 528
setdiff(unique(mapa$id), unique(crosswalk_sec_mun$id)) # la diferencia es porque "TIERRA DEL FUEGO_ANTARTIDA ARGENTINA" no está en el censo

  # Ver cuántos mun_code del shapefile de municipios aparecen en el crosswalk
n_distinct(spanish_cohorts_arg$mun_code) # 312
n_distinct(crosswalk_sec_mun$mun_code) # 312 --> son iguales: cada municipio es el municipio principal de al menos una sección electoral.

  # Ver si algún id quedó con más de un match por empates
crosswalk_sec_mun %>% count(id) %>% filter(n > 1) # cada id aparece una sola vez --> no hay una seccion asignad a dos municipios

  # Ver si hay matchs dudosos
summary(crosswalk_sec_mun$share_sec)
summary(crosswalk_sec_mun$share_mun)
 
## Hago el merge de la data sin partidos con el censo
  
# 1. Uno la data de elecciones con el crosswalk
length(unique(dip_nac_wide_small$id)) # 532 secciones
length(unique(crosswalk_sec_mun$id)) # 528 secciones
setdiff(unique(crosswalk_sec_mun$id), unique(dip_nac_wide_small$id)) # "BUENOS AIRES_JOSE C PAZ" no esta en la data de elecciones pero esta "BUENOS AIRES_JOSE C. PAZ"
setdiff(unique(dip_nac_wide_small$id), unique(crosswalk_sec_mun$id)) # "TIERRA DEL FUEGO_ANTARTIDA ARGENTINA", "ENTRE RIOS_PARANA - CAMPANA", "SANTA FE_LA CAPITAL CAMP.", "SANTA FE_ROSARIO BARR.", "SANTA FE_ROSARIO CAMP." no estan en el corsswalk
  # Estas secciones no estan en el "mapa" asi que no se pueden crusar con la data de españoles. Pero la pagina oficial indica que son casi las mismas que otras secciones que sí están en el mapa.
  # Soluciono los problemas con las secciones anteriores para que coincida con el cross walk (excepto "TIERRA DEL FUEGO_ANTARTIDA ARGENTINA" que no está en la data del censo)
dip_nac_wide_small <- dip_nac_wide_small %>%
  mutate(
    id = case_when(
      id == "ENTRE RIOS_PARANA - CAMPANA" ~ "ENTRE RIOS_PARANA",
      id == "SANTA FE_LA CAPITAL CAMP." ~ "SANTA FE_LA CAPITAL",
      id == "SANTA FE_ROSARIO BARR." ~ "SANTA FE_ROSARIO",
      id == "SANTA FE_ROSARIO CAMP." ~ "SANTA FE_ROSARIO",
      TRUE ~ id
    )
  ) %>% 
  # saco la Antartida para no tener problemas en el merge
  filter(!id == "TIERRA DEL FUEGO_ANTARTIDA ARGENTINA")
  
crosswalk_sec_mun <- crosswalk_sec_mun %>% 
  select(-share_mun, -share_sec) %>%
  filter(!id == "BUENOS AIRES_JOSE C PAZ") # esta sección está con y sin ".", saco la que no tiene punto así coincida con la data de elecciones

  # Uno las dos datas
dip_nac_wide_small <- dip_nac_wide_small %>%
  left_join(crosswalk_sec_mun, by = c("id")) %>% 
  select(tipo_eleccion, provincia, mun_code, id, seccion, anio, 
         participacion, electores, votantes, ganador, 
         votos_blanco, porcentaje_blanco, votos_impugnado, porcentaje_impugnado, votos_nulo, porcentaje_nulo,
         everything()) %>% 
  arrange(tipo_eleccion, provincia, mun_code, id, seccion, anio)
  
# 2. Agrego las secciones en municipios
dip_nac_mun <- dip_nac_wide_small %>%
  group_by(anio, tipo_eleccion, mun_code) %>%
  summarise(
    
    # Indicadores generales
    electores       = sum(electores, na.rm = TRUE),
    votantes        = sum(votantes, na.rm = TRUE),
    votos_blanco    = sum(votos_blanco, na.rm = TRUE),
    votos_nulo      = sum(votos_nulo, na.rm = TRUE),
    votos_impugnado = sum(votos_impugnado, na.rm = TRUE),
    votos_validos   = sum(votos_validos, na.rm = TRUE),
    
    # Indicadores ideológicos
    votos_izq         = sum(votos_izq, na.rm = TRUE),
    votos_der         = sum(votos_der, na.rm = TRUE),
    votos_centro      = sum(votos_centro, na.rm = TRUE),
    votos_cen_izq     = sum(votos_cen_izq, na.rm = TRUE),
    votos_cen_der     = sum(votos_cen_der, na.rm = TRUE),
    votos_no_clasif   = sum(votos_no_clasif, na.rm = TRUE),
    votos_peronistas  = sum(votos_peronistas, na.rm = TRUE),
    votos_radicales   = sum(votos_radicales, na.rm = TRUE),
    votos_oficialismo = sum(votos_oficialismo, na.rm = TRUE),
    
    .groups = "drop"
  ) %>%
  mutate(
    
    # Indicadores generales
    participacion        = 100 * votantes / electores,
    porcentaje_blanco    = 100 * votos_blanco / votantes,
    porcentaje_nulo      = 100 * votos_nulo / votantes,
    porcentaje_impugnado = 100 * votos_impugnado / votantes,
    
    # Indicadores ideológicos
    share_izq = 100 * votos_izq / votos_validos,
    share_der = 100 * votos_der / votos_validos,
    share_centro = 100 * votos_centro / votos_validos,
    share_cen_izq = 100 * votos_cen_izq / votos_validos,
    share_cen_der = 100 * votos_cen_der / votos_validos,
    share_no_clasif = 100 * votos_no_clasif / votos_validos,
    share_peronistas = 100 * votos_peronistas / votos_validos,
    share_radicales = 100 * votos_radicales / votos_validos,
    share_izq_amplia = share_izq + share_cen_izq,
    share_der_amplia = share_der + share_cen_der,
    indice_izq_der = share_izq_amplia - share_der_amplia,
    indice_cen_izq = if_else((votos_izq + votos_cen_izq) > 0,
                             votos_cen_izq / (votos_izq + votos_cen_izq),
                             NA_real_
                             ),
    indice_cen_der = if_else((votos_der + votos_cen_der) > 0,
                             votos_cen_der / (votos_der + votos_cen_der),
                             NA_real_
                             ),
    votos_clasif = votos_izq + votos_cen_izq + votos_centro + votos_cen_der + votos_der,
    indice_ideologico_pond = if_else(votos_clasif > 0,
                                    (-2 * votos_izq - 1 * votos_cen_izq + 0 * votos_centro + 1 * votos_cen_der + 2 * votos_der) / votos_clasif,
                                    NA_real_
                                    ),
    share_oficialismo = 100 * votos_oficialismo / votos_validos
  ) %>% 
  select(mun_code, anio, tipo_eleccion, everything(), -votos_clasif) %>% 
  arrange(tipo_eleccion, mun_code, anio)

  # Chequeo
summary(dip_nac_mun$participacion)
dip_nac_wide_small %>% filter(mun_code == "034005", anio == 2013, tipo_eleccion== "GENERALES") %>% View() 
dip_nac_wide_small %>% filter(mun_code == "066010", anio == 2017, tipo_eleccion== "PASO") %>% View() # FIX ME: hay outliers pero se constatan con la data, pensar después qué hacer
describe(dip_nac_mun$participacion)
skimr::skim(dip_nac_mun$participacion)

# 3. Indicadores políticos a nivel municipio: ganador, NEP, margen, incumbencia

  # Extraigo el código de municipio para cada id de elección
sec_to_mun <- dip_nac_wide_small %>% 
  select(id, mun_code) %>% 
  distinct() %>% 
  filter(!is.na(mun_code))

  # Agrego votos por partido a nivel municipio
dip_nac_long_mun <- dip_nac_clasif %>% 
  filter(!partido %in% c("BLANCO", "NULO", "IMPUGNADO")) %>% 
  left_join(sec_to_mun, by = "id") %>% 
  filter(!is.na(mun_code)) %>% 
  group_by(anio, tipo_eleccion, mun_code, partido) %>% 
  summarise(
    votos                     = sum(as.numeric(votos), na.rm = TRUE),
    clasificacion_desagregada = dplyr::first(clasificacion_desagregada),
    clasificacion_binaria     = dplyr::first(clasificacion_binaria),
    familia_politica          = dplyr::first(familia_politica),
    .groups = "drop"
  )

  # 3.1. Ganador a nivel municipio
top_partidos_mun <- dip_nac_long_mun %>% 
  group_by(anio, tipo_eleccion, mun_code) %>% 
  arrange(desc(votos), .by_group = TRUE) %>% 
  summarise(
    ganador_partido     = dplyr::first(partido),
    ganador_clasif      = dplyr::first(clasificacion_desagregada),
    ganador_clasif_bin  = dplyr::first(clasificacion_binaria),
    ganador_familia     = dplyr::first(familia_politica),
    votos_1ro           = dplyr::first(votos),
    votos_2do           = dplyr::nth(votos, 2, default = 0),
    .groups = "drop"
  )

  # 3.2. Alternancia: cambia el bloque ideológico del ganador respecto  la elección anterior del mismo tipo (PASO con PASO, GEN con GEN)
alternancia_mun <- top_partidos_mun %>% 
  select(mun_code, tipo_eleccion, anio, ganador_clasif) %>% 
  arrange(mun_code, tipo_eleccion, anio) %>% 
  group_by(mun_code, tipo_eleccion) %>% 
  mutate(
    ganador_clasif_lag = dplyr::lag(ganador_clasif, order_by = anio),
    alternancia = case_when(
      is.na(ganador_clasif_lag)            ~ NA_integer_,
      ganador_clasif != ganador_clasif_lag ~ 1L,
      TRUE                                 ~ 0L
    )
  ) %>% 
  ungroup() %>% 
  select(-ganador_clasif_lag, -ganador_clasif)

  table(alternancia_mun$alternancia, useNA = "ifany")
  
  # 3.3 Alternancia PASO vs GENERALES a nivel municipio
alt_paso_gen_mun <- top_partidos_mun %>% 
    select(anio, tipo_eleccion, mun_code, 
           ganador_partido, ganador_clasif) %>% 
    pivot_wider(
      names_from  = tipo_eleccion,
      values_from = c(ganador_partido, ganador_clasif),
      names_glue  = "{.value}_{tolower(tipo_eleccion)}"
    ) %>% 
    mutate(
      alt_paso_gen_partido = case_when(
        is.na(ganador_partido_paso) | is.na(ganador_partido_generales) ~ NA_integer_,
        ganador_partido_paso != ganador_partido_generales              ~ 1L,
        TRUE                                                           ~ 0L
      ),
      alt_paso_gen_clasif = case_when(
        is.na(ganador_clasif_paso) | is.na(ganador_clasif_generales)   ~ NA_integer_,
        ganador_clasif_paso != ganador_clasif_generales                ~ 1L,
        TRUE                                                           ~ 0L
      )
    ) %>% 
    select(anio, mun_code, alt_paso_gen_partido, alt_paso_gen_clasif)
  
table(alt_paso_gen_mun$alt_paso_gen_partido, useNA = "ifany")
table(alt_paso_gen_mun$alt_paso_gen_clasif, useNA = "ifany")
  
  # 3.4. NEP a nivel municipio
nep_mun <- dip_nac_long_mun %>% 
  group_by(anio, tipo_eleccion, mun_code) %>% 
  mutate(p_i = votos / sum(votos, na.rm = TRUE)) %>% 
  summarise(
    nep = 1 / sum(p_i^2, na.rm = TRUE),
    .groups = "drop"
  )

  # Uno las datas anteriores y calculo los márgenes
extras_mun <- top_partidos_mun %>% 
  left_join(alternancia_mun, by = c("anio", "tipo_eleccion", "mun_code")) %>%
  left_join(alt_paso_gen_mun, by = c("anio", "mun_code")) %>%
  left_join(nep_mun, by = c("anio", "tipo_eleccion", "mun_code")) %>% 
  left_join(
    dip_nac_mun %>% select(anio, tipo_eleccion, mun_code, votos_validos),
    by = c("anio", "tipo_eleccion", "mun_code")
  ) %>% 
  mutate(
    margen = 100 * (votos_1ro - votos_2do) / votos_validos,
    voto_incumbente =  if_else(es_oficialismo(ganador_partido, anio), 1L, 0L)
  ) %>% 
  select(-votos_validos)

  # Uno con la data de elecciones municipales completa
dip_nac_mun <- dip_nac_mun %>% 
  left_join(extras_mun, by = c("anio", "tipo_eleccion", "mun_code"))

# 4. Uno la data de elecciones municipales con la data de españoles
length(unique(spanish_cohorts_arg$mun_code)) # 312 municipios
length(unique(dip_nac_mun$mun_code)) # 312 municipios
setdiff(unique(dip_nac_mun$mun_code), unique(spanish_cohorts_arg$mun_code)) # todos coinciden
setdiff(unique(spanish_cohorts_arg$mun_code), unique(dip_nac_mun$mun_code)) 

dip_nac_mun <- dip_nac_mun %>%
  left_join(spanish_cohorts_arg, 
            by = c("mun_code")) %>% 
  rename(mun_name = admin_name) %>%
  select(mun_code, mun_name, anio, tipo_eleccion, everything()) %>% 
  arrange(tipo_eleccion, mun_code, mun_name, anio)

  # Agrego los nombres de las provincias
provincias <- mapa %>% 
  select(id, provincia) %>%
  st_drop_geometry()

length(unique(provincias$id)) # 529 secciones
length(unique(crosswalk_sec_mun$id)) # 527 secciones
setdiff(unique(crosswalk_sec_mun$id), unique(provincias$id)) 
setdiff(unique(provincias$id), unique(crosswalk_sec_mun$id)) # "TIERRA DEL FUEGO_ANTARTIDA ARGENTINA" y "BUENOS AIRES_JOSE C PAZ"

provincias <- provincias %>% 
  filter(!id == "TIERRA DEL FUEGO_ANTARTIDA ARGENTINA", !id == "BUENOS AIRES_JOSE C PAZ") 

any(duplicated(provincias))
provincias %>%
  count(id, provincia) %>%
  filter(n > 1)
provincias <- distinct(provincias) # Elimino las dos secciones duplicadas que venían asi del mapa (data original)

provincias <- provincias %>% 
  left_join(crosswalk_sec_mun, by = "id") %>% 
  select(provincia, mun_code) %>% 
  arrange(provincia, mun_code) %>% 
  distinct() # me quedo solo con el mapeo de provincias a municipios

any(duplicated(provincias$mun_code))
provincias %>%
  count(mun_code) %>%
  filter(n > 1)
provincias <- provincias %>% 
  filter(!provincia == "TIERRA DEL FUEGO,ANT,IS DEL ATLANTICO SUR") # saco la provincia que aparece repetida pero con un nombre escrito distinto (así venía de la data original)

  # Uno las provincias con la data de elecciones y españoles
length(unique(provincias$mun_code)) # 312 municipios
length(unique(dip_nac_mun$mun_code)) # 312 municipios
setdiff(unique(dip_nac_mun$mun_code), unique(provincias$mun_code)) # todos coinciden
setdiff(unique(provincias$mun_code), unique(dip_nac_mun$mun_code)) # todos coinciden

dip_nac_mun <- dip_nac_mun %>%
  left_join(provincias, by = "mun_code") %>% 
  rename(prov_name = provincia, prov_code = province_code) %>%
  select(mun_code, mun_name,  prov_name, prov_code, anio, tipo_eleccion, everything()) %>% 
  arrange(tipo_eleccion, mun_code, anio)

  # Chequeo (tiene que dar 799)
dip_nac %>%
  filter(anio == 2011, tipo_eleccion == "GENERALES", partido == "IMPUGNADO", provincia== "CIUDAD AUTONOMA DE BUENOS AIRES") %>%
  mutate(votos = as.numeric(votos)) %>%
  summarise(total_votos = sum(votos, na.rm = TRUE))

dip_nac_mun <- dip_nac_mun %>% 
  select(mun_code, mun_name, prov_name, prov_code, anio, tipo_eleccion,
         electores, votantes, participacion, 
         votos_blanco, votos_nulo, votos_impugnado, votos_validos,
         porcentaje_blanco, porcentaje_nulo, porcentaje_impugnado,
         votos_izq, votos_cen_izq, votos_centro, votos_cen_der, votos_der, votos_no_clasif,
         votos_peronistas, votos_radicales, votos_oficialismo,
         share_izq, share_cen_izq, share_centro, share_cen_der, share_der, share_no_clasif,
         share_izq_amplia, share_der_amplia, indice_izq_der, indice_cen_izq, indice_cen_der, indice_ideologico_pond,
         share_peronistas, share_radicales, share_oficialismo,
         ganador_partido, ganador_clasif, ganador_clasif_bin, ganador_familia,
         votos_1ro, votos_2do, margen, voto_incumbente, 
         alternancia, alt_paso_gen_partido, alt_paso_gen_clasif, nep, everything())

names(dip_nac_mun)

# 5. Data sin agregar en municipios (a nivel de sección)

  # Uno la data de elecciones (a nivel de sección) con la data de españoles
length(unique(spanish_cohorts_arg$mun_code)) # 312 municipios
length(unique(dip_nac_wide_small$mun_code)) # 312 municipios
setdiff(unique(dip_nac_wide_small$mun_code), unique(spanish_cohorts_arg$mun_code)) # todos coinciden
setdiff(unique(spanish_cohorts_arg$mun_code), unique(dip_nac_wide_small$mun_code)) # todos coinciden

dip_nac_sec <- dip_nac_wide_small %>%
  left_join(spanish_cohorts_arg, 
            by = c("mun_code")) %>% 
  rename(mun_name = admin_name,
         prov_name = provincia,
         prov_code = province_code,
         seccion_code = id,
         seccion_name = seccion) %>%
  select(prov_name, prov_code, mun_code, mun_name, seccion_code, seccion_name,
         anio, tipo_eleccion, everything()) %>% 
  arrange(tipo_eleccion, prov_name, prov_code, mun_code, mun_name, anio, seccion_code, seccion_name)

## Data para estimación

  # El panel está balanceado?
table(dip_nac_mun$anio)   # No hay paso en 2025
table(dip_nac_sec$anio)   # en secciones no

  # Cuantos municipios hay por año?
table(dip_nac_mun$anio, dip_nac_mun$tipo_eleccion) # 312
table(dip_nac_sec$anio, dip_nac_sec$tipo_eleccion) # No hay paso en 2025

  # Las secciones difieren entre PASO y GENERALES
paso <- dip_nac_sec %>%
  filter(tipo_eleccion == "PASO") %>%
  select(anio, seccion_code) %>%
  distinct()
generales <- dip_nac_sec %>%
  filter(tipo_eleccion == "GENERALES") %>%
  select(anio, seccion_code) %>%
  distinct()
  # Secciones que están en PASO pero no en GENERALES
anti_join(paso, generales, by = c("anio", "seccion_code")) # 2021 CHACO_TAPENAGA y 2011 JUJUY_VALLE GRANDE (se constata con la data de la pagina oficial)
  # Secciones que están en GENERALES pero no en PASO
anti_join(generales %>% filter(anio!=2025), paso, by = c("anio", "seccion_code"))

  # Secciones duplicadas de 2011
dip_nac_sec %>%
  filter(anio == 2011, tipo_eleccion == "PASO") %>%
  count(seccion_code) %>%
  filter(n > 1)

  # Tengo que devolverle el código de sección original a las secciones que modifique para poder unir con el crosswalk
dip_nac_sec <- dip_nac_sec %>%
  mutate(
    seccion_code = case_when(
      seccion_name == "Parana - Campana" ~ "ENTRE RIOS_PARANA - CAMPANA",
      seccion_name == "La Capital Camp." ~ "SANTA FE_LA CAPITAL CAMP.",
      seccion_name == "Rosario Barr."    ~ "SANTA FE_ROSARIO BARR.",
      seccion_name == "Rosario Camp."    ~ "SANTA FE_ROSARIO CAMP." ,
      TRUE ~ seccion_name
    )
  )

  # Creo la variable post
dip_nac_mun <- dip_nac_mun %>% 
  mutate(post = if_else(anio > 2021, 1, 0))

dip_nac_sec <- dip_nac_sec %>% 
  mutate(post = if_else(anio > 2021, 1, 0))

  # Guardo la data
write_csv(dip_nac_mun, "Data Out/dip_nac_mun.csv")
write_csv(dip_nac_sec, "Data Out/dip_nac_sec.csv")

dip_nac_mun_paso <- dip_nac_mun %>% 
  filter(tipo_eleccion == "PASO") 
dip_nac_mun_gen <- dip_nac_mun %>% 
  filter(tipo_eleccion == "GENERALES")

write_csv(dip_nac_mun_paso, "Data Out/dip_nac_mun_paso.csv")
write_csv(dip_nac_mun_gen, "Data Out/dip_nac_mun_gen.csv")

dip_nac_sec_paso <- dip_nac_sec %>% 
  filter(tipo_eleccion == "PASO") 
dip_nac_sec_gen <- dip_nac_sec %>% 
  filter(tipo_eleccion == "GENERALES")

write_csv(dip_nac_sec_paso, "Data Out/dip_nac_sec_paso.csv")
write_csv(dip_nac_sec_gen, "Data Out/dip_nac_sec_gen.csv")

}