############################################################
# Replicar cuadro 2 del Censo 1970
# Comparar libro del censo con microdatos IPUMS
############################################################

library(haven)
library(dplyr)
library(tidyr)
library(readxl)
library(writexl)

############################################################
# 1. Definir rutas
############################################################

ruta_base <- paste0(
  "C:/Users/pilih/Documents/Papers German/Valerie/",
  "Paper_nietos_arg/Data Raw/Censo/censos_arg.dta"
)

ruta_libro <- paste0(
  "C:/Users/pilih/Documents/Papers German/Valerie/",
  "Paper_nietos_arg/Data Raw/Censo/",
  "cuadro_2_censo70_digitalizado.xlsx"
)

ruta_tabla_microdatos <- paste0(
  "C:/Users/pilih/Documents/Papers German/Valerie/",
  "Paper_nietos_arg/Data Raw/Censo/",
  "tabla_censo_1970.xlsx"
)

ruta_salida <- paste0(
  "C:/Users/pilih/Documents/Papers German/Valerie/",
  "Paper_nietos_arg/Data Raw/Censo/",
  "comparacion_libro_microdatos_1970.xlsx"
)

############################################################
# 2. Abrir los microdatos
############################################################

datos <- read_dta(ruta_base)

############################################################
# 3. Crear grupos de edad, sexo y lugar de nacimiento
############################################################

orden_edades <- c(
  "0 - 4", "5 - 9", "10 - 14", "15 - 19",
  "20 - 24", "25 - 29", "30 - 34", "35 - 39",
  "40 - 44", "45 - 49", "50 - 54", "55 - 59",
  "60 - 64", "65 - 69", "70 - 74", "75 - 79",
  "80 - 84", "85 y más"
)

datos_1970 <- datos %>%
  filter(year == 1970) %>%
  mutate(
    grupo_edad = case_when(
      age >= 0  & age <= 4  ~ "0 - 4",
      age >= 5  & age <= 9  ~ "5 - 9",
      age >= 10 & age <= 14 ~ "10 - 14",
      age >= 15 & age <= 19 ~ "15 - 19",
      age >= 20 & age <= 24 ~ "20 - 24",
      age >= 25 & age <= 29 ~ "25 - 29",
      age >= 30 & age <= 34 ~ "30 - 34",
      age >= 35 & age <= 39 ~ "35 - 39",
      age >= 40 & age <= 44 ~ "40 - 44",
      age >= 45 & age <= 49 ~ "45 - 49",
      age >= 50 & age <= 54 ~ "50 - 54",
      age >= 55 & age <= 59 ~ "55 - 59",
      age >= 60 & age <= 64 ~ "60 - 64",
      age >= 65 & age <= 69 ~ "65 - 69",
      age >= 70 & age <= 74 ~ "70 - 74",
      age >= 75 & age <= 79 ~ "75 - 79",
      age >= 80 & age <= 84 ~ "80 - 84",
      age >= 85 & age < 99  ~ "85 y más",
      TRUE ~ NA_character_
    ),
    
    sexo = case_when(
      sex == 1 ~ "Varones",
      sex == 2 ~ "Mujeres",
      TRUE ~ NA_character_
    ),
    
    lugar_nacimiento = case_when(
      nativity == 1 ~ "Nacidos en el país",
      nativity == 2 ~ "Nacidos en el extranjero",
      TRUE ~ NA_character_
    )
  )

############################################################
# 4. Calcular frecuencias ponderadas
############################################################

tabla_base <- datos_1970 %>%
  filter(
    !is.na(grupo_edad),
    !is.na(sexo),
    !is.na(lugar_nacimiento),
    !is.na(perwt)
  ) %>%
  group_by(
    grupo_edad,
    sexo,
    lugar_nacimiento
  ) %>%
  summarise(
    poblacion = sum(perwt),
    .groups = "drop"
  )

############################################################
# 5. Construir tabla con la estructura del libro
############################################################

tabla_censo <- tabla_base %>%
  pivot_wider(
    names_from = c(sexo, lugar_nacimiento),
    values_from = poblacion,
    values_fill = 0
  ) %>%
  mutate(
    `Varones - Total` =
      `Varones_Nacidos en el país` +
      `Varones_Nacidos en el extranjero`,
    
    `Mujeres - Total` =
      `Mujeres_Nacidos en el país` +
      `Mujeres_Nacidos en el extranjero`,
    
    `Total - Nacidos en el país` =
      `Varones_Nacidos en el país` +
      `Mujeres_Nacidos en el país`,
    
    `Total - Nacidos en el extranjero` =
      `Varones_Nacidos en el extranjero` +
      `Mujeres_Nacidos en el extranjero`,
    
    Total =
      `Total - Nacidos en el país` +
      `Total - Nacidos en el extranjero`
  ) %>%
  select(
    grupo_edad,
    Total,
    `Total - Nacidos en el país`,
    `Total - Nacidos en el extranjero`,
    `Varones - Total`,
    
    `Varones - Nacidos en el país` =
      `Varones_Nacidos en el país`,
    
    `Varones - Nacidos en el extranjero` =
      `Varones_Nacidos en el extranjero`,
    
    `Mujeres - Total`,
    
    `Mujeres - Nacidas en el país` =
      `Mujeres_Nacidos en el país`,
    
    `Mujeres - Nacidas en el extranjero` =
      `Mujeres_Nacidos en el extranjero`
  ) %>%
  mutate(
    grupo_edad = factor(
      grupo_edad,
      levels = orden_edades
    )
  ) %>%
  arrange(grupo_edad)

############################################################
# 6. Agregar fila TOTAL
############################################################

fila_total <- tabla_censo %>%
  summarise(
    grupo_edad = "TOTAL",
    across(
      -grupo_edad,
      ~ sum(.x, na.rm = TRUE)
    )
  )

tabla_final <- bind_rows(
  fila_total,
  tabla_censo %>%
    mutate(
      grupo_edad = as.character(grupo_edad)
    )
)

############################################################
# 7. Exportar tabla construida desde los microdatos
############################################################

write_xlsx(
  tabla_final,
  ruta_tabla_microdatos
)

############################################################
# 8. Abrir tabla digitalizada del libro
############################################################

tabla_libro <- read_excel(
  ruta_libro,
  sheet = "Tabla digitalizada",
  skip = 1
) %>%
  filter(!is.na(`Grupo de edad`)) %>%
  filter(
    !grepl(
      "^Fuente:",
      `Grupo de edad`
    )
  ) %>%
  rename(
    grupo_edad = `Grupo de edad`
  )

############################################################
# 9. Revisar nombres de columnas
############################################################

print(names(tabla_libro))
print(names(tabla_final))

############################################################
# 10. Pasar ambas tablas a formato largo
############################################################

libro_largo <- tabla_libro %>%
  pivot_longer(
    cols = -grupo_edad,
    names_to = "categoria",
    values_to = "poblacion_libro"
  )

microdatos_largo <- tabla_final %>%
  pivot_longer(
    cols = -grupo_edad,
    names_to = "categoria",
    values_to = "poblacion_microdatos"
  )

############################################################
# 11. Verificar diferencias en nombres antes de unir
############################################################

categorias_solo_libro <- setdiff(
  unique(libro_largo$categoria),
  unique(microdatos_largo$categoria)
)

categorias_solo_microdatos <- setdiff(
  unique(microdatos_largo$categoria),
  unique(libro_largo$categoria)
)

print("Categorías que aparecen solo en el libro:")
print(categorias_solo_libro)

print("Categorías que aparecen solo en los microdatos:")
print(categorias_solo_microdatos)

edades_solo_libro <- setdiff(
  unique(libro_largo$grupo_edad),
  unique(microdatos_largo$grupo_edad)
)

edades_solo_microdatos <- setdiff(
  unique(microdatos_largo$grupo_edad),
  unique(libro_largo$grupo_edad)
)

print("Grupos de edad que aparecen solo en el libro:")
print(edades_solo_libro)

print("Grupos de edad que aparecen solo en los microdatos:")
print(edades_solo_microdatos)

############################################################
# 12. Unir y calcular diferencias
############################################################

comparacion_larga <- libro_largo %>%
  left_join(
    microdatos_largo,
    by = c(
      "grupo_edad",
      "categoria"
    )
  ) %>%
  mutate(
    diferencia_absoluta =
      poblacion_microdatos - poblacion_libro,
    
    diferencia_proporcional =
      diferencia_absoluta / poblacion_libro,
    
    diferencia_porcentual =
      diferencia_proporcional * 100,
    
    diferencia_porcentual_absoluta =
      abs(diferencia_porcentual)
  ) %>%
  arrange(
    factor(
      grupo_edad,
      levels = c("TOTAL", orden_edades)
    ),
    categoria
  )

############################################################
# 13. Tabla resumida: total de población por edad
############################################################

comparacion_total_edad <- comparacion_larga %>%
  filter(categoria == "Total") %>%
  select(
    grupo_edad,
    poblacion_libro,
    poblacion_microdatos,
    diferencia_absoluta,
    diferencia_proporcional,
    diferencia_porcentual,
    diferencia_porcentual_absoluta
  )

############################################################
# 14. Verificar cuántos valores vacíos quedaron
############################################################

control_vacios <- comparacion_larga %>%
  summarise(
    cantidad_filas = n(),
    vacios_libro = sum(is.na(poblacion_libro)),
    vacios_microdatos = sum(is.na(poblacion_microdatos))
  )

print(control_vacios)

############################################################
# 15. Mostrar mayores diferencias
############################################################

mayores_diferencias <- comparacion_larga %>%
  arrange(
    desc(diferencia_porcentual_absoluta)
  ) %>%
  select(
    grupo_edad,
    categoria,
    poblacion_libro,
    poblacion_microdatos,
    diferencia_absoluta,
    diferencia_porcentual
  )

print(
  mayores_diferencias,
  n = 30
)

############################################################
# 16. Exportar resultados
############################################################

write_xlsx(
  list(
    "Comparación completa" = comparacion_larga,
    "Total por edad" = comparacion_total_edad,
    "Mayores diferencias" = mayores_diferencias,
    "Control vacíos" = control_vacios,
    "Tabla microdatos" = tabla_final,
    "Tabla libro" = tabla_libro
  ),
  ruta_salida
)
