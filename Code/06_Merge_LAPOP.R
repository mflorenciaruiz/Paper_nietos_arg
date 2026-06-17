# ---------------------------------------------------------------------------- #
#                                 CÓDIGO 6

# Une la data de LAPOP con la data de españoles del censo. El objetivo es asignar 
# a cada municipio de LAPOP el share de españoles que  tuvo en cada cohorte, 
# usando el nombre del municipio y el nombre de la provincia. 
# Corre estimaciones para analizar potencial sesgo.
# ---------------------------------------------------------------------------- #

# Cargar las librerías necesarias
library(dplyr)
library(haven)
library(stringi)

# Definir el path a la carpeta del proyecto: ARGENTINA
path <- "/Users/florenciaruiz/Library/CloudStorage/OneDrive-Personal/BID/Papers Valerie/Ley de nietos/Argentina"

#path <- "C:\\Users\\pilih\\Documents\\Papers German\\Valerie\\Paper_nietos_arg"
setwd(path)

# ----------------------- #
# 1. Data
# ----------------------- #
{
  # 1. Cargar la data de LAPOP
  lapop_data <- read_dta("Data Int/lapop_append.dta")
  spanish_cohorts_arg_singeo <- read.csv("Data Out/spanish_cohorts_arg.csv") %>% 
    # reconstruyo los digitos 0 que se perdieron en el csv: 3 digitos prov, 3 digitos municipio
    mutate(mun_code = str_pad(as.character(mun_code), width = 6, pad = "0"))
  
  # 2. Preparo la data de LAPOP
  lapop_data <- lapop_data %>% 
    # limpio los nombres de municipios y provincias
    mutate(nom_mun_clean = stri_trans_general(municipio_nom, "Latin-ASCII"),
           nom_mun_clean = toupper(nom_mun_clean),
           nom_mun_clean = str_squish(nom_mun_clean), # elimino espacios duplicados, saltos de linea, espacios antes y despues
           
           nom_prov_clean = stri_trans_general(prov_nom, "Latin-ASCII"),
           nom_prov_clean = toupper(nom_prov_clean),
           nom_prov_clean = str_squish(nom_prov_clean)
    )
  
  # Corrijo los nombres de Buenos Aires
  unique(lapop_data$nom_mun_clean)
  unique(lapop_data$nom_prov_clean)
  
  lapop_data <- lapop_data %>% 
    mutate(
      es_caba = nom_mun_clean == "CABA" | str_detect(nom_mun_clean, "^COMUNA [0-9]+"),
      
      nom_prov_clean = case_when(
        es_caba ~ "CIUDAD AUTONOMA DE BUENOS AIRES",
        !es_caba & nom_prov_clean %in% c("AMBA", "AMBA (CAPITAL FEDERAL Y GBA)") ~ "BUENOS AIRES",
        nom_prov_clean %in% c("PROV DE BUENOS AIRES", "PROV. DE BUENOS AIRES") ~ "BUENOS AIRES",
        
        TRUE ~ nom_prov_clean
      ),
      
      nom_mun_clean = case_when(
        es_caba ~ "CITY OF BUENOS AIRES",
        TRUE ~ nom_mun_clean
      )
    ) %>% 
    select(-es_caba)
  
  # Corrijo Yerba Buena
  lapop_data <- lapop_data %>%
    mutate(
      nom_mun_clean = case_when(
        nom_mun_clean == "YERBA BUENA - MARCOS PAZ" ~ "YERBA BUENA",
        TRUE ~ nom_mun_clean
      )
    )
  
  # Corrijo las capitales (en la data del españoles del censo están como CAPITAL)
  lapop_data <- lapop_data %>%
    mutate(
      nom_mun_clean = case_when(
        str_detect(nom_mun_clean, "CAPITAL") ~ "CAPITAL",
        TRUE ~ nom_mun_clean
      )
    )
  
  lapop_data <- lapop_data %>% 
    select(idnum, year, prov_nom, municipio_nom, nom_prov_clean, nom_mun_clean, everything())
  
  unique(lapop_data$nom_mun_clean)
  unique(lapop_data$nom_prov_clean)
  
  # 3. Preparo la data de españoles del censo
  spanish_arg_clean <- spanish_cohorts_arg_singeo %>% 
    # limpio los nombres de municipios
    mutate(
      admin_name_clean = stri_trans_general(admin_name, "Latin-ASCII"),
      admin_name_clean = toupper(admin_name_clean),
      admin_name_clean = str_squish(admin_name_clean)
    )
  
  dip_nac_wide_small <- read_csv("Data Out/dip_nac_wide_small.csv")
  
  # Agrego los nombres de las provincias (los saco de dip_nac_wide_small porque ahí están limpios)
  provincias <- dip_nac_wide_small %>% 
    select(provincia, mun_code) %>% 
    distinct()
  
  # Uno los nombres de las provincias con la data de españoles limpia, usando el código de municipio
  spanish_arg_clean <- spanish_arg_clean %>% 
    left_join(provincias, by = "mun_code") %>% 
    select(mun_code, province_code, admin_name_clean, provincia, share_1936_1955, share_1956_1978)
}
# ----------------------- #
# 2. Unir LAPOP y españoles 
# ----------------------- #
{
  # Hago un crosswalk entre las dos bases de datos para detectar matches entre los nombres de municipios de LAPOP y los nombres de municipios de españoles del censo. 
  lapop_muns <- lapop_data %>%
    distinct(nom_prov_clean, nom_mun_clean)
  
  crosswalk <- crossing(lapop_muns, spanish_arg_clean)
  
  correct <- crosswalk %>%
    filter(
      nom_prov_clean == provincia,nom_mun_clean == admin_name_clean |
        (str_detect(admin_name_clean, ",") &
           str_detect(
             admin_name_clean,
             regex(paste0("(^|,\\s*)", str_escape(nom_mun_clean), "(\\s*,|$)"))
           )
        )
    )
  
  faltan <- lapop_muns %>%
    anti_join(correct, by = c("nom_prov_clean", "nom_mun_clean"))
  
  # Corrijo a mano los 58 que faltan. La mayoría son LOCALIDADES dentro de MUNCIPIOS o PARTIDOS:
  # 1. Busco en internet la localidad, chequeo el partido o municipio al que pertence
  # 2. Chequeo que esté en la data spanish_arg_clean el partido o municipio y cómo está escrito.
  # 3. Uso el mismo nombre de spanish_arg_clean para reemplazar en nom_mun_clean de lapop_data
  
  lapop_data <- lapop_data %>% 
    mutate(
      nom_mun_clean = case_when(
        # Buenos Aires
        nom_mun_clean == "DOMSELAAR"        & nom_prov_clean == "BUENOS AIRES" ~ "SAN VICENTE",
        nom_mun_clean == "CHAPADMALAL"      & nom_prov_clean == "BUENOS AIRES" ~ "GENERAL PUEYRREDON",
        nom_mun_clean == "GRAL. MANSILLA"   & nom_prov_clean == "BUENOS AIRES" ~ "MAGDALENA",
        nom_mun_clean == "TORRES"           & nom_prov_clean == "BUENOS AIRES" ~ "LUJAN",
        nom_mun_clean == "SAN JUSTO"        & nom_prov_clean == "BUENOS AIRES" ~ "LA MATANZA",
        nom_mun_clean == "CORONEL VIDAL"    & nom_prov_clean == "BUENOS AIRES" ~ "MAR CHIQUITA",
        nom_mun_clean == "SARANDI"          & nom_prov_clean == "BUENOS AIRES" ~ "AVELLANEDA",
        nom_mun_clean == "FLORIDA"          & nom_prov_clean == "BUENOS AIRES" ~ "VICENTE LOPEZ",
        nom_mun_clean == "BERNAL"           & nom_prov_clean == "BUENOS AIRES" ~ "TIGRE",
        nom_mun_clean == "RAFAEL CASTILLO"  & nom_prov_clean == "BUENOS AIRES" ~ "LA MATANZA",
        nom_mun_clean == "BANFIELD"         & nom_prov_clean == "BUENOS AIRES" ~ "LOMAS DE ZAMORA",
        nom_mun_clean == "VIRREY DEL PINO"  & nom_prov_clean == "BUENOS AIRES" ~ "LA MATANZA",
        nom_mun_clean == "MAR DEL PLATA"    & nom_prov_clean == "BUENOS AIRES" ~ "GENERAL PUEYRREDON",
        nom_mun_clean == "HAEDO"            & nom_prov_clean == "BUENOS AIRES" ~ "MORON",
        nom_mun_clean == "VILLA LUZURIAGA"  & nom_prov_clean == "BUENOS AIRES" ~ "LA MATANZA",
        nom_mun_clean == "GONZALEZ CATAN"   & nom_prov_clean == "BUENOS AIRES" ~ "LA MATANZA",
        nom_mun_clean == "DON TORCUATO"     & nom_prov_clean == "BUENOS AIRES" ~ "TIGRE",
        nom_mun_clean == "GENERAL MANSILLA" & nom_prov_clean == "BUENOS AIRES" ~ "MAGDALENA",
        nom_mun_clean == "DON TORCUATO ESTE"& nom_prov_clean == "BUENOS AIRES" ~ "TIGRE",
        nom_mun_clean == "EL TALAR"         & nom_prov_clean == "BUENOS AIRES" ~ "TIGRE",
        nom_mun_clean == "TALAR"            & nom_prov_clean == "BUENOS AIRES" ~ "TIGRE",
        
        # Chaco
        nom_mun_clean == "LAGUNA BLANCA"                & nom_prov_clean == "CHACO" ~ "LIBERTAD",
        nom_mun_clean == "PAMPA ALMIRON"                & nom_prov_clean == "CHACO" ~ "LIBERTADOR GENERAL SAN MARTIN",
        nom_mun_clean == "BARRANQUERAS"                 & nom_prov_clean == "CHACO" ~ "SAN FERNANDO",
        nom_mun_clean == "RESISTENCIA"                  & nom_prov_clean == "CHACO" ~ "SAN FERNANDO",
        nom_mun_clean == "PRESIDENCIA ROQUE SAENZ PENA" & nom_prov_clean == "CHACO" ~ "COMANDANTE FERNANDEZ",
        nom_mun_clean == "JUAN JOSE CASTELLI"           & nom_prov_clean == "CHACO" ~ "GENERAL GUEMES",
        nom_mun_clean == "CHARATA"                      & nom_prov_clean == "CHACO" ~ "CHACABUCO",
        nom_mun_clean == "SAENZ"                        & nom_prov_clean == "CHACO" ~ "COMANDANTE FERNANDEZ",
        nom_mun_clean == "LIBERTADOR GRAL. SAN MARTIN"  & nom_prov_clean == "CHACO" ~ "LIBERTADOR GENERAL SAN MARTIN",
        
        # Córdoba
        nom_mun_clean == "VILLA CARLOS PAZ"  & nom_prov_clean == "CORDOBA" ~ "PUNILLA",
        nom_mun_clean == "ARROYITO"          & nom_prov_clean == "CORDOBA" ~ "SAN JUSTO",
        nom_mun_clean == "CIUDAD DE CORDOBA" & nom_prov_clean == "CORDOBA" ~ "CAPITAL",
        nom_mun_clean == "CORDOBA"           & nom_prov_clean == "CORDOBA" ~ "CAPITAL",
        
        # Corrientes
        nom_mun_clean == "CORRIENTES" & nom_prov_clean == "CORRIENTES" ~ "CAPITAL",
        
        # La Pampa
        nom_mun_clean == "SANTA ROSA"   & nom_prov_clean == "LA PAMPA" ~ "CAPITAL",
        nom_mun_clean == "GENERAL ACHA" & nom_prov_clean == "LA PAMPA" ~ "UTRACAN",
        
        # Mendoza
        nom_mun_clean == "CORDON DEL PLATA" & nom_prov_clean == "MENDOZA" ~ "TUPUNGATO",
        nom_mun_clean == "MENDOZA"          & nom_prov_clean == "MENDOZA" ~ "CAPITAL",
        
        # Neuquen
        nom_mun_clean == "NEUQUEN" & nom_prov_clean == "NEUQUEN" ~ "CONFLUENCIA",
        
        # Río Negro
        nom_mun_clean == "CIPOLLETI"  & nom_prov_clean == "RIO NEGRO" ~ "GENERAL ROCA",
        nom_mun_clean == "GRAL ROCA"  & nom_prov_clean == "RIO NEGRO" ~ "GENERAL ROCA",
        nom_mun_clean == "ALLEN"      & nom_prov_clean == "RIO NEGRO" ~ "GENERAL ROCA",
        nom_mun_clean == "CIPOLLETTI" & nom_prov_clean == "RIO NEGRO" ~ "GENERAL ROCA",
        
        # Salta
        nom_mun_clean == "VAQUEROS" & nom_prov_clean == "SALTA" ~ "LA CALDERA",
        nom_mun_clean == "SALTA"    & nom_prov_clean == "SALTA" ~ "CAPITAL",
        
        # Santa Fé
        nom_mun_clean == "ALVEAR"    & nom_prov_clean == "SANTA FE" ~ "ROSARIO",
        nom_mun_clean == "RUFINO"    & nom_prov_clean == "SANTA FE" ~ "GENERAL LOPEZ",
        nom_mun_clean == "SUNCHALES" & nom_prov_clean == "SANTA FE" ~ "CASTELLANOS",
        nom_mun_clean == "RAFAELA"   & nom_prov_clean == "SANTA FE" ~ "CASTELLANOS",
        
        # Santiago Del Estero
        nom_mun_clean == "ESTACION TACANITAS" & nom_prov_clean == "SANTIAGO DEL ESTERO" ~ "GENERAL TABOADA",
        nom_mun_clean == "SIMBOLAR"           & nom_prov_clean == "SANTIAGO DEL ESTERO" ~ "ROBLES",
        nom_mun_clean == "ANATUYA"            & nom_prov_clean == "SANTIAGO DEL ESTERO" ~ "GENERAL TABOADA",
        
        # Tucumán
        nom_mun_clean == "BANDA DEL RIO SALI"                 & nom_prov_clean == "TUCUMAN" ~ "CRUZ ALTA",
        nom_mun_clean == "SAN MIGUEL DE TUCUMAN"              & nom_prov_clean == "TUCUMAN" ~ "CAPITAL",
        nom_mun_clean == "TAFI VIEJO"                         & nom_prov_clean == "TUCUMAN" ~ "TAFI VIAJO",          # estaba mal escrito en el censo
        nom_mun_clean == "YERBA BUENA-MARCOS PAZ"             & nom_prov_clean == "TUCUMAN" ~ "YERBA BUENA",
        nom_mun_clean == "VILLA MARIANO MORENO - EL COLMENAR" & nom_prov_clean == "TUCUMAN" ~ "TAFI VIAJO",
        
        TRUE ~ nom_mun_clean
      )
    )
  
  # Corro de nuevo el crosswalk
  lapop_muns <- lapop_data %>%
    distinct(nom_prov_clean, nom_mun_clean)
  
  crosswalk <- crossing(lapop_muns, spanish_arg_clean)
  correct <- crosswalk %>%
    filter(
      nom_prov_clean == provincia,nom_mun_clean == admin_name_clean |
        (str_detect(admin_name_clean, ",") &
           str_detect(
             admin_name_clean,
             regex(paste0("(^|,\\s*)", str_escape(nom_mun_clean), "(\\s*,|$)"))
           )
        )
    )
  faltan <- lapop_muns %>%
    anti_join(correct, by = c("nom_prov_clean", "nom_mun_clean"))
  rm(faltan)
  
  # Hago el merge
  lapop_data_merge <- lapop_data %>% 
    left_join(correct %>% select(nom_prov_clean, nom_mun_clean, mun_code, name_censo = admin_name_clean,
                                 province_code, share_1936_1955, share_1956_1978), 
              by = c("nom_prov_clean", "nom_mun_clean")) %>% 
    select(nom_prov_clean, nom_mun_clean, mun_code, name_censo,
           province_code, everything())
  
  write.csv(lapop_data_merge, "Data Out/lapop_data_merge.csv", row.names = FALSE)
}
# ----------------------- #
# 3. Estimaciones 
# ----------------------- #
{
  lapop_data_merge <-  read.csv("Data Out/lapop_data_merge.csv")
  
### 3.1 Estadísticas descriptivas ### 
  
  # Resumen de outcomes políticos y características personales
  skimr::skim(lapop_data_merge %>% 
                select(-share_1956_1978, -share_1936_1955, -strata, -wt, -upm,
                       -municipio_nom, -estratopri, -prov_nom, -year, -idnum,
                       -province_code, -nom_prov_clean, -nom_mun_clean, -mun_code,
                       -name_censo))
  
  table(lapop_data_merge$izq_der)
  table(lapop_data_merge$nivel_educ)
  table(lapop_data_merge$anios_educ)
  names(lapop_data_merge)
  table(lapop_data_merge$voto_blanco_nulo)
  table(lapop_data_merge$izq_der)
  
  # Medias por quintil de españoles e intencion de migrar
  outcomes <- c(
    "izq_der",
    "voto_anterior",
    "identifica_partido",
    "intencion_voto",
    "voto_blanco_nulo",
    "reuniones_comunidad"
  )
  
  lapop_pre <- lapop_data_merge %>% 
    filter(year < 2023)
  
  # 1. Medias por quintil de Spanish share
  
  make_quintile_table <- function(data, share_var, share_label) {
    data %>%
      mutate(
        share_quintile = ntile(.data[[share_var]], 5),
        share_quintile = paste0("Q", share_quintile)
      ) %>%
      group_by(share_quintile) %>%
      summarise(
        exposure = first(share_label),
        n = n(),
        across(
          all_of(outcomes),
          ~ weighted.mean(.x, w = wt, na.rm = TRUE),
          .names = "{.col}"
        ),
        .groups = "drop"
      ) %>%
      select(exposure, share_quintile, n, everything())
  }
  
  means_q_1936_1955 <- make_quintile_table(lapop_pre, "share_1936_1955", "Spanish share 1936-1955")
  means_q_1956_1978 <- make_quintile_table(lapop_pre, "share_1956_1978", "Spanish share 1956-1978")
  
  means_by_quintile <- bind_rows(means_q_1936_1955, means_q_1956_1978) %>% 
    rename(
      Exposure = exposure,
      `Share quintile` = share_quintile,
      Observations = n,
      `Left-right ideology`  = izq_der,
      `Previous vote` = voto_anterior,
      `Party identification` = identifica_partido,
      `Vote intention`  = intencion_voto,
      `Blank/null vote intention` = voto_blanco_nulo,
      `Community meetings`  = reuniones_comunidad
    )
  
  # 2. Medias por intención de migrar
  
  means_by_migration_intent <- lapop_pre %>%
    mutate(
      migration_intent = ifelse(
        intencion_migrar == 1,
        "Intends to migrate",
        "Does not intend to migrate"
      )
    ) %>%
    group_by(migration_intent) %>%
    summarise(
      n = n(),
      across(
        all_of(outcomes),
        ~ weighted.mean(.x, w = wt, na.rm = TRUE),
        .names = "{.col}"
      ),
      .groups = "drop"
    ) %>% 
    mutate(migration_intent = if_else(is.na(migration_intent), "No response", migration_intent)) %>% 
    rename(
      `Migration intent` = migration_intent,
      `Left-right ideology`  = izq_der,
      Observations = n,
      `Previous vote` = voto_anterior,
      `Party identification` = identifica_partido,
      `Vote intention`  = intencion_voto,
      `Blank/null vote intention` = voto_blanco_nulo,
      `Community meetings`  = reuniones_comunidad
    ) 
  
  # Exportar a Excel
  wb <- createWorkbook()
  
  addWorksheet(wb, "Means by Spanish quintile")
  writeData(wb, "Means by Spanish quintile", means_by_quintile)
  
  addWorksheet(wb, "Means by migration intent")
  writeData(wb, "Means by migration intent", means_by_migration_intent)
  
  saveWorkbook(wb, file = "Output/lapop_descriptive_means.xlsx", overwrite = TRUE)
  
### 3.2 Modelos estimados sobre variables políticas ### 
  
  outcome_labels <- c(
    izq_der = "Left-right ideology",
    voto_anterior = "Previous vote",
    identifica_partido = "Party identification",
    intencion_voto = "Vote intention",
    voto_blanco_nulo = "Blank/null vote intention",
    reuniones_comunidad = "Community meetings" 
  )
  
  models <- map(outcomes, function(y) {
    fml <- as.formula(
      paste0(
        y, " ~ ",
        "intencion_migrar * share_1936_1955 + ",
        "intencion_migrar * share_1956_1978 + ",
        "edad + hombre + anios_educ | ",
        "year + name_censo"
      )
    )
    
    feols(fml, data = lapop_pre, weights = ~ wt, cluster = ~ name_censo)
  })
  
  names(models) <- outcome_labels[outcomes]
  
  coef_map <- c(
    "intencion_migrar" = "Intends to migrate",
    "intencion_migrar:share_1936_1955" = "Intends to migrate × Spanish share 1936–1955",
    "intencion_migrar:share_1956_1978" = "Intends to migrate × Spanish share 1956–1978",
    "edad" = "Age",
    "hombre" = "Male",
    "anios_educ" = "Years of education"
  )
  
  modelsummary(
    models,
    output = "Output/lapop_selection_regressions.xlsx",
    coef_map = coef_map,
    stars = c("*" = .10, "**" = .05, "***" = .01),
    statistic = "std.error",
    gof_map = c("nobs", "r.squared")
  )

# Efectos de la ley en variables políticas
  
  # Creo el post
  lapop_data_merge <- lapop_data_merge %>% 
    mutate(post = ifelse(year >= 2023, 1, 0))
  
  feols(reuniones_comunidad ~ post : share_1936_1955 + post : share_1956_1978 + 
          edad + hombre  | year + name_censo, 
        data = lapop_data_merge, weights = ~ wt, cluster = ~ name_censo)
  
  feols(identifica_partido ~ post : share_1936_1955 + post : share_1956_1978 + 
          edad + hombre  | year + name_censo, 
        data = lapop_data_merge, weights = ~ wt, cluster = ~ name_censo)
  
  feols(voto_anterior ~ post : share_1936_1955 + post : share_1956_1978 + 
          edad + hombre  | year + name_censo, 
        data = lapop_data_merge, weights = ~ wt, cluster = ~ name_censo)
  
  feols(intencion_migrar ~ post : share_1936_1955 + post : share_1956_1978 + 
          edad + hombre  | year + name_censo, 
        data = lapop_data_merge, weights = ~ wt, cluster = ~ name_censo)
  
  feols(intencion_voto ~ post : share_1936_1955 + post : share_1956_1978 + 
          edad + hombre  | year + name_censo, 
        data = lapop_data_merge, weights = ~ wt, cluster = ~ name_censo)
  
  feols(interes_pol_mucho ~ post : share_1936_1955 + post : share_1956_1978 + 
          edad + hombre  | year + name_censo, 
        data = lapop_data_merge, weights = ~ wt, cluster = ~ name_censo)
  
  feols(interes_pol_nada ~ post : share_1936_1955 + post : share_1956_1978 + 
         edad + hombre  | year + name_censo, 
       data = lapop_data_merge, weights = ~ wt, cluster = ~ name_censo)
  
  feols(interes_pol_algo ~ post : share_1936_1955 + post : share_1956_1978 + 
          edad + hombre  | year + name_censo, 
        data = lapop_data_merge, weights = ~ wt, cluster = ~ name_censo)
  
  iplot(feols(interes_pol_nada ~ i(year, share_1936_1955) + 
          edad + hombre  | year + name_censo, 
        data = lapop_data_merge, weights = ~ wt, cluster = ~ name_censo))
  
  iplot(feols(interes_pol_nada ~ i(year, share_1956_1978) + 
                edad + hombre  | year + name_censo, 
              data = lapop_data_merge, weights = ~ wt, cluster = ~ name_censo))
}


