# ==============================================================================
# 10_ERA5_Drought_SPEI12_from_NC_Folder.R
#
# OBJETIVO
#   1. Leer directamente los archivos .nc ya descomprimidos en:
#        Data Raw/Clima/Drought
#   2. Seleccionar solamente SPEI-12, reanalysis, consolidated, moda.
#   3. Usar el SPEI-12 del mes calendario inmediatamente anterior a cada
#      eleccion nacional.
#   4. Calcular por municipio el porcentaje del area con:
#        - SPEI-12 < -1.5  (medida principal)
#        - SPEI-12 < -2.0  (robustez)
#   5. Unir el control climatico al panel electoral original.
#   6. Correr las mismas seis especificaciones de la tabla original.
#   7. Exportar una tabla LaTeX con el mismo formato de seis columnas.
#
# IMPORTANTE
#   Los archivos descargados tienen accumulation period = 12. Por lo tanto,
#   el SPEI-12 de septiembre de 2025 resume aproximadamente octubre de 2024
#   a septiembre de 2025. Este codigo NO interpreta los datos como SPEI-6.
#
#   El codigo ignora automaticamente los archivos SPI y usa solo archivos
#   cuyo nombre comienza con SPEI12_.
# ==============================================================================


# ------------------------------------------------------------------------------
# 0. Paquetes
# ------------------------------------------------------------------------------

packages <- c(
  "terra",
  "sf",
  "dplyr",
  "tidyr",
  "stringr",
  "readr",
  "exactextractr",
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

# Carpeta de la captura: contiene los .nc ya descomprimidos.
carpeta_nc <- file.path(
  "Data Raw",
  "Clima",
  "Drought"
)

archivo_shape <- file.path(
  "Data Raw",
  "geo2_ar1970_2010",
  "geo2_ar1970_2010.shp"
)

archivo_elecciones <- file.path(
  "Data Out",
  "dip_nac_mun.csv"
)

carpeta_salida <- file.path(
  "Output",
  "Argentina",
  "Drought_ERA5_SPEI12"
)

dir.create(
  carpeta_salida,
  recursive = TRUE,
  showWarnings = FALSE
)


# ------------------------------------------------------------------------------
# 2. Calendario electoral y mes climatico utilizado
# ------------------------------------------------------------------------------

calendario_elecciones <- tibble::tribble(
  ~anio, ~tipo_eleccion, ~fecha_eleccion,
  2011L, "PASO",       "2011-08-14",
  2011L, "GENERALES",  "2011-10-23",
  2013L, "PASO",       "2013-08-11",
  2013L, "GENERALES",  "2013-10-27",
  2015L, "PASO",       "2015-08-09",
  2015L, "GENERALES",  "2015-10-25",
  2017L, "PASO",       "2017-08-13",
  2017L, "GENERALES",  "2017-10-22",
  2019L, "PASO",       "2019-08-11",
  2019L, "GENERALES",  "2019-10-27",
  2021L, "PASO",       "2021-09-12",
  2021L, "GENERALES",  "2021-11-14",
  2023L, "PASO",       "2023-08-13",
  2023L, "GENERALES",  "2023-10-22",
  2025L, "GENERALES",  "2025-10-26"
) %>%
  mutate(
    fecha_eleccion = as.Date(fecha_eleccion),

    # Primer dia del mes electoral.
    primer_dia_mes_eleccion = as.Date(
      format(fecha_eleccion, "%Y-%m-01")
    ),

    # Ultimo dia del mes anterior y luego primer dia de ese mes.
    fecha_clima = as.Date(
      format(
        primer_dia_mes_eleccion - 1,
        "%Y-%m-01"
      )
    ),

    anio_clima = as.integer(format(fecha_clima, "%Y")),
    mes_clima = as.integer(format(fecha_clima, "%m")),
    yyyymm = format(fecha_clima, "%Y%m")
  ) %>%
  select(
    anio,
    tipo_eleccion,
    fecha_eleccion,
    fecha_clima,
    anio_clima,
    mes_clima,
    yyyymm
  )

cat("\nMes climatico utilizado para cada eleccion:\n")
print(calendario_elecciones, n = Inf)


# ------------------------------------------------------------------------------
# 3. Inventario de archivos NetCDF
# ------------------------------------------------------------------------------

if (!dir.exists(carpeta_nc)) {
  stop(
    paste0(
      "No se encontro la carpeta con los NetCDF:\n",
      carpeta_nc,
      "\n\nCorregir carpeta_nc."
    )
  )
}

archivos_nc <- list.files(
  path = carpeta_nc,
  pattern = "\\.nc$",
  full.names = TRUE,
  recursive = TRUE,
  ignore.case = TRUE
)

if (length(archivos_nc) == 0) {
  stop(
    paste0(
      "No se encontraron archivos .nc en:\n",
      carpeta_nc
    )
  )
}

# Convencion oficial esperada:
# SPEI12_genlogistic_global_era5_moda_ref1991to2020_YYYYMM.nc
inventario_nc <- tibble::tibble(
  archivo = archivos_nc,
  nombre = basename(archivos_nc)
) %>%
  mutate(
    es_spei12 = stringr::str_detect(
      nombre,
      stringr::regex("^SPEI12_", ignore_case = TRUE)
    ),

    es_reanalysis_moda = stringr::str_detect(
      nombre,
      stringr::regex("_era5_moda_", ignore_case = TRUE)
    ),

    # Extrae YYYYMM solamente cuando aparece inmediatamente antes de .nc.
    yyyymm = stringr::str_match(
      nombre,
      "_((?:19|20)\\d{2}(?:0[1-9]|1[0-2]))\\.nc$"
    )[, 2],

    anio_clima = suppressWarnings(
      as.integer(substr(yyyymm, 1, 4))
    ),

    mes_clima = suppressWarnings(
      as.integer(substr(yyyymm, 5, 6))
    )
  )

readr::write_csv(
  inventario_nc,
  file.path(
    carpeta_salida,
    "inventario_archivos_nc.csv"
  )
)

cat("\nArchivos .nc encontrados:", nrow(inventario_nc), "\n")
cat("Archivos que comienzan con SPEI12_:", sum(inventario_nc$es_spei12), "\n")
cat(
  "Archivos SPEI12 de ERA5 reanalysis/moda:",
  sum(inventario_nc$es_spei12 & inventario_nc$es_reanalysis_moda),
  "\n"
)

# Seleccion principal: SPEI12 + ERA5 reanalysis deterministic stream (moda).
archivos_spei12 <- inventario_nc %>%
  filter(
    es_spei12,
    es_reanalysis_moda,
    !is.na(yyyymm)
  )

# Fallback por si el nombre no contiene literalmente _era5_moda_, pero si
# comienza con SPEI12_ y tiene YYYYMM al final.
if (nrow(archivos_spei12) == 0) {
  warning(
    paste0(
      "No se encontraron nombres con _era5_moda_. ",
      "Se usaran todos los archivos que comienzan con SPEI12_."
    )
  )

  archivos_spei12 <- inventario_nc %>%
    filter(
      es_spei12,
      !is.na(yyyymm)
    )
}

if (nrow(archivos_spei12) == 0) {
  stop(
    paste0(
      "No pude identificar archivos SPEI12 con YYYYMM al final del nombre.\n",
      "Revisar inventario_archivos_nc.csv."
    )
  )
}


# ------------------------------------------------------------------------------
# 4. Seleccionar los 15 meses necesarios
# ------------------------------------------------------------------------------

archivos_elecciones <- calendario_elecciones %>%
  left_join(
    archivos_spei12 %>%
      select(
        archivo,
        nombre,
        yyyymm,
        anio_clima,
        mes_clima
      ),
    by = c(
      "yyyymm",
      "anio_clima",
      "mes_clima"
    )
  )

# Control de archivos faltantes.
faltantes <- archivos_elecciones %>%
  filter(is.na(archivo))

if (nrow(faltantes) > 0) {
  readr::write_csv(
    faltantes,
    file.path(
      carpeta_salida,
      "periodos_climaticos_faltantes.csv"
    )
  )

  stop(
    paste0(
      "Faltan archivos SPEI12 para estos meses:\n",
      paste(
        paste0(
          faltantes$anio,
          " ",
          faltantes$tipo_eleccion,
          " -> ",
          faltantes$yyyymm
        ),
        collapse = "\n"
      ),
      "\n\nSe guardo el detalle en periodos_climaticos_faltantes.csv."
    )
  )
}

# Control de duplicados: debe haber exactamente un archivo por YYYYMM.
duplicados <- archivos_elecciones %>%
  count(
    anio,
    tipo_eleccion,
    yyyymm,
    name = "n_archivos"
  ) %>%
  filter(n_archivos != 1)

if (nrow(duplicados) > 0) {
  readr::write_csv(
    archivos_elecciones,
    file.path(
      carpeta_salida,
      "archivos_elecciones_con_duplicados.csv"
    )
  )

  stop(
    paste0(
      "Hay mas de un archivo candidato para uno o mas meses electorales.\n",
      "Revisar archivos_elecciones_con_duplicados.csv."
    )
  )
}

readr::write_csv(
  archivos_elecciones,
  file.path(
    carpeta_salida,
    "archivos_spei12_usados_por_eleccion.csv"
  )
)

cat("\nArchivos que se usaran:\n")
print(
  archivos_elecciones %>%
    select(
      anio,
      tipo_eleccion,
      fecha_clima,
      nombre
    ),
  n = Inf
)


# ------------------------------------------------------------------------------
# 5. Leer municipios armonizados
# ------------------------------------------------------------------------------

if (!file.exists(archivo_shape)) {
  stop(
    paste0(
      "No se encontro el shapefile:\n",
      archivo_shape
    )
  )
}

municipios <- sf::st_read(
  archivo_shape,
  options = "ENCODING=LATIN1",
  quiet = FALSE
) %>%
  mutate(
    mun_code = stringr::str_sub(
      as.character(GEOLEVEL2),
      -6,
      -1
    )
  ) %>%
  filter(
    !mun_code %in% c(
      "094003",
      "094004",
      "030000",
      "099999"
    )
  ) %>%
  sf::st_make_valid()

if (anyDuplicated(municipios$mun_code) > 0) {
  stop("El shapefile contiene mun_code duplicados.")
}

cat("\nMunicipios en el shapefile:", nrow(municipios), "\n")


# ------------------------------------------------------------------------------
# 6. Funciones para leer y agregar cada archivo
# ------------------------------------------------------------------------------

leer_spei12 <- function(archivo) {

  # En la descarga elegida cada archivo deberia contener una sola variable
  # y un solo mes. terra::rast suele abrirla directamente.
  r <- try(
    terra::rast(archivo),
    silent = TRUE
  )

  # Fallback para NetCDF con subdatasets.
  if (inherits(r, "try-error")) {

    subdatasets <- terra::sds(archivo)
    nombres_subdatasets <- names(subdatasets)

    candidato <- grep(
      "spei",
      nombres_subdatasets,
      ignore.case = TRUE
    )

    if (length(candidato) == 0) {
      stop(
        paste0(
          "No pude identificar una variable SPEI dentro de:\n",
          archivo
        )
      )
    }

    r <- subdatasets[[candidato[1]]]
  }

  # Si el archivo abre con mas de una capa, preferir la que contiene SPEI.
  if (terra::nlyr(r) > 1) {

    candidato <- grep(
      "spei",
      names(r),
      ignore.case = TRUE
    )

    if (length(candidato) >= 1) {
      r <- r[[candidato[1]]]
    } else {
      warning(
        paste0(
          "El archivo tiene mas de una capa y ninguna se llama SPEI. ",
          "Se usara la primera: ",
          basename(archivo)
        )
      )
      r <- r[[1]]
    }
  }

  # Algunos NetCDF de ERA5-Drought no guardan explicitamente el CRS.
  # Las coordenadas son longitud/latitud, por lo que corresponde WGS84.
  crs_actual <- terra::crs(r, proj = TRUE)

  if (
    length(crs_actual) == 0 ||
      is.na(crs_actual) ||
      !nzchar(trimws(crs_actual))
  ) {
    terra::crs(r) <- "EPSG:4326"
    cat("CRS ausente en el NetCDF: se asigno EPSG:4326.\n")
  }

  # ERA5-Drought puede usar longitudes 0-360. rotate las pasa a -180-180.
  if (terra::xmin(r) >= 0 && terra::xmax(r) > 180) {
    r <- terra::rotate(r)
  }

  # Control final: st_transform necesita que el raster tenga CRS.
  crs_final <- terra::crs(r, proj = TRUE)

  if (
    length(crs_final) == 0 ||
      is.na(crs_final) ||
      !nzchar(trimws(crs_final))
  ) {
    stop(
      paste0(
        "No fue posible asignar el CRS al archivo:\n",
        archivo
      )
    )
  }

  r
}


extraer_promedio_ponderado <- function(
    raster_layer,
    poligonos,
    raster_area
) {

  resultado <- exactextractr::exact_extract(
    raster_layer,
    poligonos,
    fun = "weighted_mean",
    weights = raster_area,
    progress = FALSE
  )

  resultado[is.nan(resultado)] <- NA_real_
  resultado
}


agregar_un_periodo <- function(
    archivo,
    anio,
    tipo_eleccion,
    fecha_eleccion,
    fecha_clima
) {

  cat(
    "\nProcesando ",
    anio,
    " ",
    tipo_eleccion,
    " | clima: ",
    format(fecha_clima, "%Y-%m"),
    "\n",
    sep = ""
  )

  spei <- leer_spei12(archivo)

  # Transformar municipios al CRS del raster.
  municipios_periodo <- sf::st_transform(
    municipios,
    crs = terra::crs(spei)
  )

  # Recortar el raster a Argentina antes de calcular areas y extraer.
  municipios_vect <- terra::vect(municipios_periodo)
  extension_arg <- terra::ext(municipios_vect)

  # Pequeno margen para asegurar que entren las celdas de borde.
  extension_arg <- terra::ext(
    terra::xmin(extension_arg) - 0.5,
    terra::xmax(extension_arg) + 0.5,
    terra::ymin(extension_arg) - 0.5,
    terra::ymax(extension_arg) + 0.5
  )

  spei <- terra::crop(
    spei,
    extension_arg,
    snap = "out"
  )

  if (terra::ncell(spei) == 0) {
    stop(
      paste0(
        "El raster no tiene celdas sobre Argentina: ",
        basename(archivo)
      )
    )
  }

  # Diagnostico del rango. SPEI deberia tener valores aproximadamente
  # en unidades de desviaciones estandar, con negativos = mas seco.
  rango <- terra::global(
    spei,
    fun = c("min", "max"),
    na.rm = TRUE
  )

  cat("Rango SPEI del recorte:\n")
  print(rango)

  drought_15 <- terra::ifel(
    is.na(spei),
    NA,
    spei < -1.5
  )

  drought_20 <- terra::ifel(
    is.na(spei),
    NA,
    spei < -2
  )

  pixel_area_km2 <- terra::cellSize(
    spei,
    unit = "km"
  )

  names(pixel_area_km2) <- "pixel_area_km2"

  tibble::tibble(
    mun_code = municipios_periodo$mun_code,
    mun_name_drought = municipios_periodo$ADMIN_NAME,
    anio = as.integer(anio),
    tipo_eleccion = as.character(tipo_eleccion),
    fecha_eleccion = as.Date(fecha_eleccion),
    fecha_clima = as.Date(fecha_clima),
    archivo_clima = basename(archivo),

    # Medida principal.
    drought_spei12_pre_election_pct_15 =
      100 * extraer_promedio_ponderado(
        drought_15,
        municipios_periodo,
        pixel_area_km2
      ),

    # Robustez con umbral mas estricto.
    drought_spei12_pre_election_pct_20 =
      100 * extraer_promedio_ponderado(
        drought_20,
        municipios_periodo,
        pixel_area_km2
      ),

    # Medida continua. Menor SPEI = condiciones mas secas.
    spei12_pre_election_mean =
      extraer_promedio_ponderado(
        spei,
        municipios_periodo,
        pixel_area_km2
      )
  )
}


# ------------------------------------------------------------------------------
# 7. Construir panel climatico municipio-eleccion
# ------------------------------------------------------------------------------

clima_municipal <- purrr::pmap_dfr(
  list(
    archivo = archivos_elecciones$archivo,
    anio = archivos_elecciones$anio,
    tipo_eleccion = archivos_elecciones$tipo_eleccion,
    fecha_eleccion = archivos_elecciones$fecha_eleccion,
    fecha_clima = archivos_elecciones$fecha_clima
  ),
  agregar_un_periodo
)

if (
  clima_municipal %>%
    count(mun_code, anio, tipo_eleccion) %>%
    filter(n != 1) %>%
    nrow() > 0
) {
  stop(
    "El panel climatico no es unico por municipio-anio-tipo de eleccion."
  )
}

readr::write_csv(
  clima_municipal,
  file.path(
    carpeta_salida,
    "era5_spei12_pre_election_municipal.csv"
  )
)


# ------------------------------------------------------------------------------
# 8. Controles de disponibilidad y calidad
# ------------------------------------------------------------------------------

availability <- clima_municipal %>%
  group_by(
    anio,
    tipo_eleccion,
    fecha_clima
  ) %>%
  summarise(
    municipalities = n(),
    nonmissing_main = sum(
      !is.na(drought_spei12_pre_election_pct_15)
    ),
    missing_main = sum(
      is.na(drought_spei12_pre_election_pct_15)
    ),
    mean_drought_pct = mean(
      drought_spei12_pre_election_pct_15,
      na.rm = TRUE
    ),
    .groups = "drop"
  ) %>%
  arrange(
    anio,
    tipo_eleccion
  )

cat("\nDisponibilidad climatica:\n")
print(availability, n = Inf)

readr::write_csv(
  availability,
  file.path(
    carpeta_salida,
    "climate_availability_by_election.csv"
  )
)

if (any(availability$missing_main > 0)) {
  stop(
    paste0(
      "Hay municipios sin dato climatico. ",
      "Revisar climate_availability_by_election.csv."
    )
  )
}


# ------------------------------------------------------------------------------
# 9. Leer el panel electoral y unir el clima
# ------------------------------------------------------------------------------

if (!file.exists(archivo_elecciones)) {
  stop(
    paste0(
      "No se encontro el panel electoral:\n",
      archivo_elecciones
    )
  )
}

elecciones <- readr::read_csv(
  archivo_elecciones,
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
    post = as.integer(anio > 2021)
  )

# Recalcular los lags exactamente dentro de municipio y tipo de eleccion.
elecciones <- elecciones %>%
  arrange(
    mun_code,
    tipo_eleccion,
    anio
  ) %>%
  group_by(
    mun_code,
    tipo_eleccion
  ) %>%
  mutate(
    porcentaje_blanco_l = dplyr::lag(porcentaje_blanco),
    participacion_l = dplyr::lag(participacion)
  ) %>%
  ungroup()

n_antes_merge <- nrow(elecciones)

data_all <- elecciones %>%
  left_join(
    clima_municipal %>%
      select(
        mun_code,
        anio,
        tipo_eleccion,
        fecha_eleccion,
        fecha_clima,
        archivo_clima,
        drought_spei12_pre_election_pct_15,
        drought_spei12_pre_election_pct_20,
        spei12_pre_election_mean
      ),
    by = c(
      "mun_code",
      "anio",
      "tipo_eleccion"
    )
  )

if (nrow(data_all) != n_antes_merge) {
  stop("El merge con clima modifico el numero de filas del panel electoral.")
}

unmatched <- data_all %>%
  filter(
    is.na(drought_spei12_pre_election_pct_15)
  ) %>%
  distinct(
    mun_code,
    anio,
    tipo_eleccion
  )

readr::write_csv(
  unmatched,
  file.path(
    carpeta_salida,
    "unmatched_election_climate_rows.csv"
  )
)

if (nrow(unmatched) > 0) {
  stop(
    paste0(
      "Quedaron ",
      nrow(unmatched),
      " filas electorales sin clima. ",
      "Revisar unmatched_election_climate_rows.csv."
    )
  )
}

data_gen <- data_all %>%
  filter(tipo_eleccion == "GENERALES")

readr::write_csv(
  data_all,
  file.path(
    carpeta_salida,
    "dip_nac_mun_with_era5_spei12.csv"
  )
)

cat("\nObservaciones panel completo:", nrow(data_all), "\n")
cat("Municipios:", dplyr::n_distinct(data_all$mun_code), "\n")


# ------------------------------------------------------------------------------
# 10. Modelos con control climatico
# ------------------------------------------------------------------------------

# 1. Voto blanco: PASO + generales.
b1_clima <- fixest::feols(
  porcentaje_blanco ~
    share_1936_1955:post +
    share_1956_1978:post +
    drought_spei12_pre_election_pct_15 |
    mun_code + anio + tipo_eleccion,
  data = data_all,
  vcov = ~mun_code
)

# 2. Voto blanco: solo generales.
b2_clima <- fixest::feols(
  porcentaje_blanco ~
    share_1936_1955:post +
    share_1956_1978:post +
    drought_spei12_pre_election_pct_15 |
    mun_code + anio,
  data = data_gen,
  vcov = ~mun_code
)

# 3. Voto blanco: PASO + generales, con lag del outcome.
b3_clima <- fixest::feols(
  porcentaje_blanco ~
    share_1936_1955:post +
    share_1956_1978:post +
    porcentaje_blanco_l +
    drought_spei12_pre_election_pct_15 |
    mun_code + anio + tipo_eleccion,
  data = data_all,
  vcov = ~mun_code
)

# 4. Participacion: PASO + generales.
t1_clima <- fixest::feols(
  participacion ~
    share_1936_1955:post +
    share_1956_1978:post +
    drought_spei12_pre_election_pct_15 |
    mun_code + anio + tipo_eleccion,
  data = data_all,
  vcov = ~mun_code
)

# 5. Participacion: solo generales.
t2_clima <- fixest::feols(
  participacion ~
    share_1936_1955:post +
    share_1956_1978:post +
    drought_spei12_pre_election_pct_15 |
    mun_code + anio,
  data = data_gen,
  vcov = ~mun_code
)

# 6. Participacion: PASO + generales, con lag del outcome.
t3_clima <- fixest::feols(
  participacion ~
    share_1936_1955:post +
    share_1956_1978:post +
    participacion_l +
    drought_spei12_pre_election_pct_15 |
    mun_code + anio + tipo_eleccion,
  data = data_all,
  vcov = ~mun_code
)

models_climate <- list(
  "(1)" = b1_clima,
  "(2)" = b2_clima,
  "(3)" = b3_clima,
  "(4)" = t1_clima,
  "(5)" = t2_clima,
  "(6)" = t3_clima
)


# ------------------------------------------------------------------------------
# 11. Modelos originales para comparar coeficientes
# ------------------------------------------------------------------------------

b1_base <- fixest::feols(
  porcentaje_blanco ~
    share_1936_1955:post +
    share_1956_1978:post |
    mun_code + anio + tipo_eleccion,
  data = data_all,
  vcov = ~mun_code
)

b2_base <- fixest::feols(
  porcentaje_blanco ~
    share_1936_1955:post +
    share_1956_1978:post |
    mun_code + anio,
  data = data_gen,
  vcov = ~mun_code
)

b3_base <- fixest::feols(
  porcentaje_blanco ~
    share_1936_1955:post +
    share_1956_1978:post +
    porcentaje_blanco_l |
    mun_code + anio + tipo_eleccion,
  data = data_all,
  vcov = ~mun_code
)

t1_base <- fixest::feols(
  participacion ~
    share_1936_1955:post +
    share_1956_1978:post |
    mun_code + anio + tipo_eleccion,
  data = data_all,
  vcov = ~mun_code
)

t2_base <- fixest::feols(
  participacion ~
    share_1936_1955:post +
    share_1956_1978:post |
    mun_code + anio,
  data = data_gen,
  vcov = ~mun_code
)

t3_base <- fixest::feols(
  participacion ~
    share_1936_1955:post +
    share_1956_1978:post +
    participacion_l |
    mun_code + anio + tipo_eleccion,
  data = data_all,
  vcov = ~mun_code
)

models_baseline <- list(
  "(1)" = b1_base,
  "(2)" = b2_base,
  "(3)" = b3_base,
  "(4)" = t1_base,
  "(5)" = t2_base,
  "(6)" = t3_base
)


# ------------------------------------------------------------------------------
# 12. Test de igualdad entre las dos ventanas españolas
# ------------------------------------------------------------------------------

get_p_equal <- function(model) {

  betas <- stats::coef(model)
  V <- stats::vcov(model)

  name1 <- grep(
    "1936_1955",
    names(betas),
    value = TRUE
  )

  name2 <- grep(
    "1956_1978",
    names(betas),
    value = TRUE
  )

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

p_values <- vapply(
  models_climate,
  get_p_equal,
  numeric(1)
)

p_strings <- ifelse(
  is.na(p_values),
  "",
  sprintf("%.3f", p_values)
)


# ------------------------------------------------------------------------------
# 13. Exportar tabla LaTeX con el formato original
# ------------------------------------------------------------------------------

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
    "Lagged voter turnout",

  "drought_spei12_pre_election_pct_15" =
    "Pre-election drought exposure"
)

gof_map <- tibble::tribble(
  ~raw,        ~clean,          ~fmt,
  "nobs",      "Observations",  0,
  "r.squared", "R$^2$",         3
)

add_rows <- tibble::tibble(
  term = c(
    "$p$-value ($\\beta_{36{-}55} = \\beta_{56{-}78}$)",
    "Municipality FE",
    "Time FE",
    "Election type FE",
    "General elections only",
    "Pre-election drought control"
  ),

  m1 = c(p_strings[1], "Yes", "Yes", "Yes", "No",  "Yes"),
  m2 = c(p_strings[2], "Yes", "Yes", "No",  "Yes", "Yes"),
  m3 = c(p_strings[3], "Yes", "Yes", "Yes", "No",  "Yes"),
  m4 = c(p_strings[4], "Yes", "Yes", "Yes", "No",  "Yes"),
  m5 = c(p_strings[5], "Yes", "Yes", "No",  "Yes", "Yes"),
  m6 = c(p_strings[6], "Yes", "Yes", "Yes", "No",  "Yes")
)

names(add_rows) <- c(
  "term",
  "(1)",
  "(2)",
  "(3)",
  "(4)",
  "(5)",
  "(6)"
)

tex <- modelsummary::modelsummary(
  models_climate,
  output = "latex",
  coef_map = coef_map,
  gof_map = gof_map,
  estimate = "{estimate}{stars}",
  statistic = "({std.error})",
  stars = c(
    "*" = 0.10,
    "**" = 0.05,
    "***" = 0.01
  ),
  add_rows = add_rows,
  escape = FALSE
)

lines <- strsplit(
  as.character(tex),
  "\n"
)[[1]]

# Caption y espaciado.
beg_table <- grep(
  "\\\\begin\\{table\\}",
  lines
)

if (length(beg_table) >= 1) {

  lines[beg_table[1]] <- "\\begin{table}[!h]"

  header <- c(
    "\\caption{Effects on Blank Votes and Voter Turnout, Controlling for Pre-Election Drought}",
    "\\renewcommand{\\arraystretch}{1.25}",
    "\\setlength{\\tabcolsep}{6pt}"
  )

  lines <- c(
    lines[1:beg_table[1]],
    header,
    lines[(beg_table[1] + 1):length(lines)]
  )
}

# Encabezado agrupado igual a la tabla original.
top_idx <- grep(
  "\\\\toprule",
  lines
)

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

# Lineas como en el original.
lines <- gsub(
  "\\\\toprule",
  "\\\\hline",
  lines
)

lines <- gsub(
  "\\\\bottomrule",
  "\\\\hline",
  lines
)

midrules <- grep(
  "\\\\midrule",
  lines
)

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

# Espacio antes de Observations.
observations_line <- grep(
  "^Observations",
  lines
)

if (length(observations_line) >= 1) {
  lines <- c(
    lines[1:(observations_line[1] - 1)],
    "\\addlinespace",
    lines[observations_line[1]:length(lines)]
  )
}

# Nota.
end_tabular <- grep(
  "\\\\end\\{tabular\\}",
  lines
)

if (length(end_tabular) >= 1) {

  note <- c(
    "\\vspace{0.3em}",
    "\\captionsetup{justification=justified, singlelinecheck=false}",
    paste0(
      "\\caption*{\\footnotesize Notes: The dependent variable in columns (1)--(3) is the share of blank votes over total voters. ",
      "In columns (4)--(6), the dependent variable is voter turnout. ",
      "Pre-election drought exposure is the percentage of municipal area with SPEI-12 below -1.5 in the calendar month immediately preceding each election. ",
      "Because the accumulation period is 12 months, each monthly SPEI-12 value summarizes the preceding twelve-month climatic balance ending in that month. ",
      "Columns (2) and (5) use general elections only. Columns (3) and (6) include the lagged dependent variable, calculated within municipality and election type. ",
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

archivo_tabla <- file.path(
  carpeta_salida,
  "att_blankvotes_turnout_drought_era5_spei12.tex"
)

writeLines(
  lines,
  archivo_tabla
)

cat("\nTabla LaTeX guardada en:\n", archivo_tabla, "\n")


# ------------------------------------------------------------------------------
# 14. Comparar coeficientes: baseline versus clima
# ------------------------------------------------------------------------------

extract_key <- function(model, version, model_number) {

  broom::tidy(model) %>%
    filter(
      stringr::str_detect(
        term,
        "1936_1955|1956_1978"
      )
    ) %>%
    transmute(
      model = model_number,
      version = version,
      term,
      estimate,
      std.error,
      p.value
    )
}

comparison <- dplyr::bind_rows(
  purrr::imap_dfr(
    models_baseline,
    ~extract_key(.x, "Baseline", .y)
  ),
  purrr::imap_dfr(
    models_climate,
    ~extract_key(.x, "ERA5 SPEI-12 control", .y)
  )
) %>%
  arrange(
    model,
    term,
    version
  )

readr::write_csv(
  comparison,
  file.path(
    carpeta_salida,
    "key_coefficient_comparison_era5_spei12.csv"
  )
)


# ------------------------------------------------------------------------------
# 15. Robusteces opcionales
# ------------------------------------------------------------------------------

# Umbral mas estricto:
#   reemplazar drought_spei12_pre_election_pct_15 por
#   drought_spei12_pre_election_pct_20
#
# Medida continua:
#   reemplazar drought_spei12_pre_election_pct_15 por
#   spei12_pre_election_mean
#
# Recordar para la medida continua:
#   SPEI mas bajo = condiciones mas secas.

cat(
  "\nProceso terminado. Archivos guardados en:\n",
  normalizePath(
    carpeta_salida,
    winslash = "/",
    mustWork = FALSE
  ),
  "\n"
)

##########

# ==============================================================================
# 16. ROBUSTECES ADICIONALES:
#     A) UMBRAL SPEI-12 < -2
#     B) SPEI-12 MUNICIPAL CONTINUO
# ==============================================================================


# ------------------------------------------------------------------------------
# 16.1. Funcion para estimar las seis especificaciones
# ------------------------------------------------------------------------------

estimar_seis_modelos_clima <- function(control_var) {
  
  crear_formula <- function(
    outcome,
    lag_var = NULL,
    solo_generales = FALSE
  ) {
    
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
  
  # 1. Voto blanco: PASO + generales
  b1 <- fixest::feols(
    crear_formula(
      outcome = "porcentaje_blanco"
    ),
    data = data_all,
    vcov = ~mun_code
  )
  
  # 2. Voto blanco: solo generales
  b2 <- fixest::feols(
    crear_formula(
      outcome = "porcentaje_blanco",
      solo_generales = TRUE
    ),
    data = data_gen,
    vcov = ~mun_code
  )
  
  # 3. Voto blanco: PASO + generales, con lag
  b3 <- fixest::feols(
    crear_formula(
      outcome = "porcentaje_blanco",
      lag_var = "porcentaje_blanco_l"
    ),
    data = data_all,
    vcov = ~mun_code
  )
  
  # 4. Participacion: PASO + generales
  t1 <- fixest::feols(
    crear_formula(
      outcome = "participacion"
    ),
    data = data_all,
    vcov = ~mun_code
  )
  
  # 5. Participacion: solo generales
  t2 <- fixest::feols(
    crear_formula(
      outcome = "participacion",
      solo_generales = TRUE
    ),
    data = data_gen,
    vcov = ~mun_code
  )
  
  # 6. Participacion: PASO + generales, con lag
  t3 <- fixest::feols(
    crear_formula(
      outcome = "participacion",
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
# 16.2. Estimar modelos con umbral SPEI-12 < -2
# ------------------------------------------------------------------------------

models_spei20 <- estimar_seis_modelos_clima(
  control_var = "drought_spei12_pre_election_pct_20"
)


# ------------------------------------------------------------------------------
# 16.3. Estimar modelos con SPEI-12 continuo
# ------------------------------------------------------------------------------

models_spei_continuo <- estimar_seis_modelos_clima(
  control_var = "spei12_pre_election_mean"
)


# ------------------------------------------------------------------------------
# 16.4. Mostrar comparaciones en la consola
# ------------------------------------------------------------------------------

cat("\n\nMODELOS CON UMBRAL SPEI-12 < -2\n")

fixest::etable(
  models_spei20,
  keep = "1936_1955|1956_1978|drought",
  fitstat = ~n + r2
)

cat("\n\nMODELOS CON SPEI-12 CONTINUO\n")

fixest::etable(
  models_spei_continuo,
  keep = "1936_1955|1956_1978|spei12",
  fitstat = ~n + r2
)


# ------------------------------------------------------------------------------
# 16.5. Funcion para exportar las tablas LaTeX
# ------------------------------------------------------------------------------

exportar_tabla_robustez <- function(
    models,
    control_var,
    control_label,
    caption,
    nota_control,
    nombre_archivo
) {
  
  p_values_robustez <- vapply(
    models,
    get_p_equal,
    numeric(1)
  )
  
  p_strings_robustez <- ifelse(
    is.na(p_values_robustez),
    "",
    sprintf("%.3f", p_values_robustez)
  )
  
  coef_map_robustez <- c(
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
  
  coef_map_robustez <- c(
    coef_map_robustez,
    stats::setNames(
      control_label,
      control_var
    )
  )
  
  gof_map_robustez <- tibble::tribble(
    ~raw,        ~clean,         ~fmt,
    "nobs",      "Observations", 0,
    "r.squared", "R$^2$",        3
  )
  
  add_rows_robustez <- tibble::tibble(
    term = c(
      "$p$-value ($\\beta_{36{-}55} = \\beta_{56{-}78}$)",
      "Municipality FE",
      "Time FE",
      "Election type FE",
      "General elections only",
      "Climate control"
    ),
    
    m1 = c(
      p_strings_robustez[1],
      "Yes", "Yes", "Yes", "No", "Yes"
    ),
    
    m2 = c(
      p_strings_robustez[2],
      "Yes", "Yes", "No", "Yes", "Yes"
    ),
    
    m3 = c(
      p_strings_robustez[3],
      "Yes", "Yes", "Yes", "No", "Yes"
    ),
    
    m4 = c(
      p_strings_robustez[4],
      "Yes", "Yes", "Yes", "No", "Yes"
    ),
    
    m5 = c(
      p_strings_robustez[5],
      "Yes", "Yes", "No", "Yes", "Yes"
    ),
    
    m6 = c(
      p_strings_robustez[6],
      "Yes", "Yes", "Yes", "No", "Yes"
    )
  )
  
  names(add_rows_robustez) <- c(
    "term",
    "(1)",
    "(2)",
    "(3)",
    "(4)",
    "(5)",
    "(6)"
  )
  
  tex_robustez <- modelsummary::modelsummary(
    models,
    output = "latex",
    coef_map = coef_map_robustez,
    gof_map = gof_map_robustez,
    estimate = "{estimate}{stars}",
    statistic = "({std.error})",
    stars = c(
      "*" = 0.10,
      "**" = 0.05,
      "***" = 0.01
    ),
    add_rows = add_rows_robustez,
    escape = FALSE
  )
  
  lines <- strsplit(
    as.character(tex_robustez),
    "\n"
  )[[1]]
  
  # Caption y espaciado
  beg_table <- grep(
    "\\\\begin\\{table\\}",
    lines
  )
  
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
  
  # Encabezado agrupado
  top_idx <- grep(
    "\\\\toprule",
    lines
  )
  
  if (length(top_idx) >= 1) {
    
    multicol <- paste0(
      " & \\multicolumn{3}{c}{Share of blank votes}",
      " & \\multicolumn{3}{c}{Voter turnout} \\\\"
    )
    
    cmidrule <- paste0(
      "\\cmidrule(l){2-4} ",
      "\\cmidrule(l){5-7}"
    )
    
    lines <- c(
      lines[1:top_idx[1]],
      multicol,
      cmidrule,
      lines[(top_idx[1] + 1):length(lines)]
    )
  }
  
  # Reemplazar lineas booktabs
  lines <- gsub(
    "\\\\toprule",
    "\\\\hline",
    lines
  )
  
  lines <- gsub(
    "\\\\bottomrule",
    "\\\\hline",
    lines
  )
  
  midrules <- grep(
    "\\\\midrule",
    lines
  )
  
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
  
  # Espacio antes de Observations
  observations_line <- grep(
    "^Observations",
    lines
  )
  
  if (length(observations_line) >= 1) {
    lines <- c(
      lines[1:(observations_line[1] - 1)],
      "\\addlinespace",
      lines[observations_line[1]:length(lines)]
    )
  }
  
  # Nota
  end_tabular <- grep(
    "\\\\end\\{tabular\\}",
    lines
  )
  
  if (length(end_tabular) >= 1) {
    
    note <- c(
      "\\vspace{0.3em}",
      "\\captionsetup{justification=justified, singlelinecheck=false}",
      paste0(
        "\\caption*{\\footnotesize Notes: ",
        "The dependent variable in columns (1)--(3) is the share ",
        "of blank votes over total voters. In columns (4)--(6), ",
        "the dependent variable is voter turnout. ",
        nota_control,
        " Columns (2) and (5) use general elections only. ",
        "Columns (3) and (6) include the lagged dependent variable, ",
        "calculated within municipality and election type. ",
        "All specifications include municipality and year fixed effects; ",
        "pooled specifications also include election-type fixed effects. ",
        "Standard errors clustered at the municipality level in parentheses ",
        "(312 clusters). ",
        "* $p<0.10$, ** $p<0.05$, *** $p<0.01$.}"
      )
    )
    
    lines <- c(
      lines[1:end_tabular[1]],
      note,
      lines[(end_tabular[1] + 1):length(lines)]
    )
  }
  
  archivo_salida <- file.path(
    carpeta_salida,
    nombre_archivo
  )
  
  writeLines(
    lines,
    archivo_salida
  )
  
  cat(
    "\nTabla guardada en:\n",
    archivo_salida,
    "\n"
  )
}


# ------------------------------------------------------------------------------
# 16.6. Exportar tabla con umbral -2
# ------------------------------------------------------------------------------

exportar_tabla_robustez(
  models = models_spei20,
  
  control_var =
    "drought_spei12_pre_election_pct_20",
  
  control_label =
    "Pre-election drought exposure: SPEI-12 below -2",
  
  caption =
    paste0(
      "Effects on Blank Votes and Voter Turnout, ",
      "Controlling for Severe Pre-Election Drought"
    ),
  
  nota_control =
    paste0(
      "Pre-election drought exposure is the percentage of municipal area ",
      "with SPEI-12 below -2 in the calendar month immediately preceding ",
      "each election. Each monthly SPEI-12 value summarizes the preceding ",
      "twelve-month climatic balance ending in that month."
    ),
  
  nombre_archivo =
    "att_blankvotes_turnout_drought_era5_spei12_threshold20.tex"
)


# ------------------------------------------------------------------------------
# 16.7. Exportar tabla con SPEI continuo
# ------------------------------------------------------------------------------

exportar_tabla_robustez(
  models = models_spei_continuo,
  
  control_var =
    "spei12_pre_election_mean",
  
  control_label =
    "Mean pre-election SPEI-12",
  
  caption =
    paste0(
      "Effects on Blank Votes and Voter Turnout, ",
      "Controlling for Continuous Pre-Election SPEI-12"
    ),
  
  nota_control =
    paste0(
      "The climate control is the area-weighted municipal mean of SPEI-12 ",
      "in the calendar month immediately preceding each election. ",
      "Lower SPEI-12 values indicate drier conditions. Each monthly ",
      "SPEI-12 value summarizes the preceding twelve-month climatic ",
      "balance ending in that month."
    ),
  
  nombre_archivo =
    "att_blankvotes_turnout_drought_era5_spei12_continuous.tex"
)


# ------------------------------------------------------------------------------
# 16.8. Comparacion de coeficientes entre las cuatro versiones
# ------------------------------------------------------------------------------

extraer_coeficientes_principales <- function(
    models,
    version
) {
  
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


comparison_all_climate <- dplyr::bind_rows(
  
  extraer_coeficientes_principales(
    models_baseline,
    "Baseline"
  ),
  
  extraer_coeficientes_principales(
    models_climate,
    "SPEI-12 threshold -1.5"
  ),
  
  extraer_coeficientes_principales(
    models_spei20,
    "SPEI-12 threshold -2"
  ),
  
  extraer_coeficientes_principales(
    models_spei_continuo,
    "Continuous SPEI-12"
  )
  
) %>%
  arrange(
    model,
    term,
    version
  )


readr::write_csv(
  comparison_all_climate,
  file.path(
    carpeta_salida,
    "key_coefficient_comparison_all_era5_spei12.csv"
  )
)


cat(
  "\nRobusteces terminadas. Resultados guardados en:\n",
  normalizePath(
    carpeta_salida,
    winslash = "/",
    mustWork = FALSE
  ),
  "\n"
)


# ==============================================================================
# MAPAS DEL CONTROL CLIMATICO SPEI-12
# ==============================================================================


# ------------------------------------------------------------------------------
# 1. Paquetes
# ------------------------------------------------------------------------------

packages_maps <- c(
  "ggplot2",
  "sf",
  "dplyr",
  "stringr",
  "tibble"
)

missing_maps <- packages_maps[
  !vapply(
    packages_maps,
    requireNamespace,
    logical(1),
    quietly = TRUE
  )
]

if (length(missing_maps) > 0) {
  install.packages(missing_maps)
}

invisible(
  lapply(
    packages_maps,
    library,
    character.only = TRUE
  )
)


# ------------------------------------------------------------------------------
# 2. Carpeta de salida
# ------------------------------------------------------------------------------

carpeta_mapas <- file.path(
  carpeta_salida,
  "Maps"
)

dir.create(
  carpeta_mapas,
  recursive = TRUE,
  showWarnings = FALSE
)


# ------------------------------------------------------------------------------
# 3. Orden de las elecciones en los paneles
# ------------------------------------------------------------------------------

orden_periodos <- calendario_elecciones %>%
  mutate(
    tipo_eleccion = stringr::str_to_upper(
      tipo_eleccion
    ),
    periodo_mapa = paste(
      anio,
      tipo_eleccion
    )
  ) %>%
  pull(
    periodo_mapa
  )


# ------------------------------------------------------------------------------
# 4. Construir la base espacial municipio-eleccion
# ------------------------------------------------------------------------------

mapa_clima_elecciones <- municipios %>%
  sf::st_transform(
    4326
  ) %>%
  dplyr::select(
    mun_code
  ) %>%
  left_join(
    clima_municipal %>%
      sf::st_drop_geometry() %>%
      transmute(
        mun_code = stringr::str_pad(
          as.character(mun_code),
          width = 6,
          side = "left",
          pad = "0"
        ),
        
        anio = as.integer(
          anio
        ),
        
        tipo_eleccion = stringr::str_to_upper(
          stringr::str_trim(
            tipo_eleccion
          )
        ),
        
        drought_spei12_pre_election_pct_15,
        
        drought_spei12_pre_election_pct_20,
        
        spei12_pre_election_mean
      ),
    by = "mun_code"
  ) %>%
  mutate(
    periodo_mapa = factor(
      paste(
        anio,
        tipo_eleccion
      ),
      levels = orden_periodos
    )
  )


# ------------------------------------------------------------------------------
# 5. Control de cantidad de municipios
# ------------------------------------------------------------------------------

conteo_mapa <- mapa_clima_elecciones %>%
  sf::st_drop_geometry() %>%
  dplyr::count(
    anio,
    tipo_eleccion,
    name = "municipios"
  ) %>%
  tibble::as_tibble()

print(
  conteo_mapa
)

if (
  any(
    conteo_mapa$municipios != 312
  )
) {
  warning(
    paste0(
      "Alguna eleccion no tiene exactamente ",
      "312 municipios en la base de mapas."
    )
  )
}


# Verificar valores faltantes

faltantes_mapa <- mapa_clima_elecciones %>%
  sf::st_drop_geometry() %>%
  group_by(
    anio,
    tipo_eleccion
  ) %>%
  summarise(
    missing_threshold15 = sum(
      is.na(
        drought_spei12_pre_election_pct_15
      )
    ),
    
    missing_threshold20 = sum(
      is.na(
        drought_spei12_pre_election_pct_20
      )
    ),
    
    missing_continuous = sum(
      is.na(
        spei12_pre_election_mean
      )
    ),
    
    .groups = "drop"
  )

print(
  faltantes_mapa
)

readr::write_csv(
  faltantes_mapa,
  file.path(
    carpeta_mapas,
    "map_data_missing_check.csv"
  )
)


# ------------------------------------------------------------------------------
# 6. Funcion para crear y guardar los mapas
# ------------------------------------------------------------------------------

guardar_mapa_spei12 <- function(
    data,
    variable,
    titulo,
    subtitulo,
    titulo_leyenda,
    nombre_archivo,
    tipo_escala = c(
      "porcentaje",
      "spei"
    ),
    n_columnas = 3,
    ancho = 12,
    alto = 15
) {
  
  tipo_escala <- match.arg(
    tipo_escala
  )
  
  mapa <- ggplot(
    data = data
  ) +
    geom_sf(
      aes(
        fill = .data[[variable]]
      ),
      color = "grey60",
      linewidth = 0.05
    ) +
    facet_wrap(
      ~periodo_mapa,
      ncol = n_columnas,
      drop = FALSE
    ) +
    coord_sf(
      datum = NA
    ) +
    labs(
      title = titulo,
      subtitle = subtitulo,
      fill = titulo_leyenda,
      caption = paste0(
        "Cada valor mensual de SPEI-12 resume el balance ",
        "climatico acumulado durante los doce meses que ",
        "terminan en el mes inmediatamente anterior a la eleccion."
      )
    ) +
    theme_void() +
    theme(
      plot.title = element_text(
        face = "bold",
        size = 14
      ),
      
      plot.subtitle = element_text(
        size = 10
      ),
      
      strip.text = element_text(
        face = "bold",
        size = 8
      ),
      
      legend.position = "bottom",
      
      legend.key.width = grid::unit(
        2.8,
        "cm"
      ),
      
      plot.caption = element_text(
        size = 8,
        hjust = 0
      ),
      
      panel.spacing = grid::unit(
        0.4,
        "lines"
      ),
      
      plot.margin = margin(
        10,
        10,
        10,
        10
      )
    )
  
  
  # Escala comun de 0 a 100 para porcentajes
  if (
    tipo_escala == "porcentaje"
  ) {
    
    mapa <- mapa +
      scale_fill_viridis_c(
        limits = c(
          0,
          100
        ),
        
        breaks = c(
          0,
          25,
          50,
          75,
          100
        ),
        
        labels = c(
          "0",
          "25",
          "50",
          "75",
          "100"
        ),
        
        oob = scales::squish,
        
        na.value = "grey90"
      )
  }
  
  
  # Escala divergente para SPEI continuo
  if (
    tipo_escala == "spei"
  ) {
    
    limite_spei <- max(
      abs(
        data[[variable]]
      ),
      na.rm = TRUE
    )
    
    mapa <- mapa +
      scale_fill_gradient2(
        low = "#8c510a",
        mid = "#f7f7f7",
        high = "#01665e",
        
        midpoint = 0,
        
        limits = c(
          -limite_spei,
          limite_spei
        ),
        
        oob = scales::squish,
        
        na.value = "grey90"
      )
  }
  
  
  # Guardar PNG
  ggplot2::ggsave(
    filename = file.path(
      carpeta_mapas,
      paste0(
        nombre_archivo,
        ".png"
      )
    ),
    
    plot = mapa,
    
    width = ancho,
    height = alto,
    units = "in",
    dpi = 300,
    bg = "white"
  )
  
  
  # Guardar PDF
  ggplot2::ggsave(
    filename = file.path(
      carpeta_mapas,
      paste0(
        nombre_archivo,
        ".pdf"
      )
    ),
    
    plot = mapa,
    
    width = ancho,
    height = alto,
    units = "in",
    bg = "white"
  )
  
  
  return(
    mapa
  )
}


# ------------------------------------------------------------------------------
# 7. Mapa principal: porcentaje del area con SPEI-12 < -1.5
# ------------------------------------------------------------------------------

mapa_spei12_15 <- guardar_mapa_spei12(
  data = mapa_clima_elecciones,
  
  variable =
    "drought_spei12_pre_election_pct_15",
  
  titulo =
    "Pre-Election Drought Exposure",
  
  subtitulo = paste0(
    "Percentage of municipal area with ",
    "SPEI-12 below -1.5"
  ),
  
  titulo_leyenda =
    "Municipal area\naffected (%)",
  
  nombre_archivo =
    "map_drought_spei12_threshold15_all_elections",
  
  tipo_escala =
    "porcentaje"
)

print(
  mapa_spei12_15
)


# ------------------------------------------------------------------------------
# 8. Mapa de robustez: porcentaje del area con SPEI-12 < -2
# ------------------------------------------------------------------------------

mapa_spei12_20 <- guardar_mapa_spei12(
  data = mapa_clima_elecciones,
  
  variable =
    "drought_spei12_pre_election_pct_20",
  
  titulo =
    "Severe Pre-Election Drought Exposure",
  
  subtitulo = paste0(
    "Percentage of municipal area with ",
    "SPEI-12 below -2"
  ),
  
  titulo_leyenda =
    "Municipal area\naffected (%)",
  
  nombre_archivo =
    "map_drought_spei12_threshold20_all_elections",
  
  tipo_escala =
    "porcentaje"
)

print(
  mapa_spei12_20
)


# ------------------------------------------------------------------------------
# 9. Mapa del SPEI-12 municipal continuo
# ------------------------------------------------------------------------------

mapa_spei12_continuo <- guardar_mapa_spei12(
  data = mapa_clima_elecciones,
  
  variable =
    "spei12_pre_election_mean",
  
  titulo =
    "Mean Pre-Election SPEI-12",
  
  subtitulo = paste0(
    "Area-weighted municipal mean of SPEI-12; ",
    "lower values indicate drier conditions"
  ),
  
  titulo_leyenda =
    "Mean municipal\nSPEI-12",
  
  nombre_archivo =
    "map_spei12_continuous_all_elections",
  
  tipo_escala =
    "spei"
)

print(
  mapa_spei12_continuo
)


# ------------------------------------------------------------------------------
# 10. Mensaje final
# ------------------------------------------------------------------------------

cat(
  "\nMapas SPEI-12 guardados en:\n",
  
  normalizePath(
    carpeta_mapas,
    winslash = "/",
    mustWork = FALSE
  ),
  
  "\n"
)