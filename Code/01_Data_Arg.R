library(foreign)
library(dplyr)
library(tidyr)
library(readr)
library(haven)
library(sf)
library(ggplot2)
library(psych)
library(stringr)
library(writexl)
library(fixest)
library(zoo)
library(scales)
library(patchwork)

# Definir el path a la carpeta del proyecto: ARGENTINA
path_flor <- "/Users/florenciaruiz/Library/CloudStorage/OneDrive-Personal/BID/Papers Valerie/Ley de nietos/Argentina"
setwd(path_flor)

font_add( # Agrego times new roman
  family = "Times New Roman",
  regular = "/System/Library/Fonts/Supplemental/Times New Roman.ttf",
  bold = "/System/Library/Fonts/Supplemental/Times New Roman Bold.ttf",
  italic = "/System/Library/Fonts/Supplemental/Times New Roman Italic.ttf",
  bolditalic = "/System/Library/Fonts/Supplemental/Times New Roman Bold Italic.ttf"
)
showtext_auto()
showtext_opts(dpi = 300)   # para que el tamaño del texto se vea bien al exportar a 300 dpi

# Data de municipios harmonizada
municipios_arg_sf <- st_read("Data Raw/geo2_ar1970_2010/geo2_ar1970_2010.shp",
                             options = "ENCODING=LATIN1")
municipios_arg_sf <- municipios_arg_sf %>%
  mutate(
    mun_code  = str_sub(as.character(GEOLEVEL2), -6, -1)
  )

# Censos de Argentina
censos_arg <- read_dta("Data Raw/Censos/Argentina/censos_arg.dta")

# -------------------- #
#        Censo
# -------------------- #
{
# Españoles por municipio
class(censos_arg$perwt)
unique(censos_arg$bplcountry)
class(censos_arg$bplcountry)

length(unique(censos_arg$geo2_ar)) # 314 municipios
length(unique(censos_arg$geolev2))
length(unique(censos_arg$geo2_ar1970)) # 343  
length(unique(censos_arg$geo2_ar1980)) # 347

# Construyo la data con presencia española anual
spanish_arg <- censos_arg %>%
  mutate(
    spanish_born = (bplcountry == 43120),
    mun_code  = str_sub(as.character(geo2_ar), -6, -1)
  ) %>%
  group_by(mun_code, year) %>%
  summarise(
    n_total = sum(perwt, na.rm = TRUE),
    n_spanish_born = sum(perwt[spanish_born], na.rm = TRUE),
    share_spanish_born = (n_spanish_born / n_total)*100,
    log_spanish_born = log1p(n_spanish_born),
    
    # chequeos de tamaño muestral (sin pesos)
    unweighted_n = n(),
    unweighted_spanish = sum(spanish_born, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(mun_code)

max(spanish_arg$share_spanish_born, na.rm = TRUE)
min(spanish_arg$share_spanish_born, na.rm = TRUE) # max y min muy parecido a mx

psych::describe(spanish_arg$share_spanish_born)
psych::describe(spanish_arg$log_spanish_born) # pero está mas distribuido

Hmisc::describe(spanish_arg$share_spanish_born)

# Data de 1970
spanish_arg_70 <- spanish_arg %>% 
  filter(year == 1970)
# Data de 1980
spanish_arg_80 <- spanish_arg %>% 
  filter(year == 1980)

# Censo 1970
censo_arg_1970 <- censos_arg %>% 
  filter(year == 1970) %>% 
  mutate(mun_code = str_sub(as.character(geo2_ar), -6, -1))
# Censo 1980
censo_arg_1980 <- censos_arg %>% 
  filter(year == 1980) %>% 
  mutate(mun_code = str_sub(as.character(geo2_ar), -6, -1))

# Españoles por municipio en cada ventana de llegada
spanish_counts <- censos_arg %>%
  mutate(
    spanish_born = ifelse(bplcountry == 43120, 1, 0),
    mun_code = str_sub(as.character(geo2_ar), -6, -1) # ultimos 3 digitos municipios, primeros 3 provincia 
  ) %>%
  filter(
    spanish_born == 1,
    !yrimm %in% c(0, 9998, 9999)   # Not in universe, In transit, Unknown
  ) %>%
  filter(
    (year == 1970 & yrimm >= 1936 & yrimm <= 1955) |
      (year == 1980 & yrimm >= 1956 & yrimm <= 1978)
  ) %>%
  mutate(
    cohort = case_when(
      year == 1970 & yrimm >= 1936 & yrimm <= 1955 ~ "arrived_1936_1955",
      year == 1980 & yrimm >= 1956 & yrimm <= 1978 ~ "arrived_1956_1978"
    )
  ) %>%
  group_by(mun_code, cohort) %>%
  summarise(
    n_spanish = sum(perwt, na.rm = TRUE),
    unweighted_n = n(),
    .groups = "drop"
  ) %>%
  pivot_wider(
    names_from = cohort,
    values_from = c(n_spanish, unweighted_n),
    values_fill = 0
  ) %>%
  arrange(mun_code)

length(unique(censo_arg_1970$mun_code)) # 314
length(unique(censo_arg_1980$mun_code)) # 312
# municipios que estan en 1970 pero no en 1980
censo_arg_1970 %>% 
  filter(!mun_code %in% censo_arg_1980$mun_code) %>% 
  select(mun_code) %>% 
  distinct() # 099999: Unknown; 030000: Entre Rios [district unknown]
length(unique(spanish_counts$mun_code)) # 226 --> hay muncipios que no tenian españoles
length(unique(municipios_arg_sf$mun_code)) # 314
# municipios que estan en el shapefile pero no en 1970
municipios_arg_sf %>% 
  filter(!mun_code %in% censo_arg_1970$mun_code) %>% 
  select(mun_code) %>% 
  distinct() # 94003 South Georgia and South Sandwich Islands (islas britanicas); 94004 Falkland Islands
# municipios que estan en 1970 pero no en el shapefile
censo_arg_1970 %>% 
  filter(!mun_code %in% municipios_arg_sf$mun_code) %>% 
  select(mun_code) %>% 
  distinct() # 099999: Unknown

# Left join con todos los municipios
spanish_cohorts_arg <- municipios_arg_sf %>%
  filter(!mun_code %in% c("094004", "094003", "030000")) %>% # elimino las islas y entre rios unknown district
  left_join(spanish_counts, by = "mun_code") %>%
  mutate(
    across(starts_with("n_spanish"), ~replace_na(., 0)),
    across(starts_with("unweighted_n"), ~replace_na(., 0))
  ) %>%
  arrange(mun_code)

# Población total municipal en 1970 y 1980
total_pop_arg <- censos_arg %>%
  mutate(
    mun_code = str_sub(as.character(geo2_ar), -6, -1)
  ) %>%
  group_by(mun_code, year) %>%
  summarise(
    total_pop = sum(perwt, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    cohort = case_when(
      year == 1970 ~ "arrived_1936_1955",
      year == 1980 ~ "arrived_1956_1978"
    )
  ) %>%
  select(-year) %>%
  pivot_wider(
    names_from = cohort,
    values_from = total_pop,
    names_prefix = "total_pop_"
  )

# Unir y calcular shares
spanish_cohorts_arg <- spanish_cohorts_arg %>%
  left_join(total_pop_arg, by = "mun_code") %>%
  mutate(
    share_1936_1955 = 100 * n_spanish_arrived_1936_1955 / total_pop_arrived_1936_1955,
    share_1956_1978 = 100 * n_spanish_arrived_1956_1978 / total_pop_arrived_1956_1978
  ) %>% 
  select(mun_code, ADMIN_NAME, n_spanish_arrived_1936_1955, n_spanish_arrived_1956_1978, 
         unweighted_n_arrived_1936_1955, unweighted_n_arrived_1956_1978,
         total_pop_arrived_1936_1955, total_pop_arrived_1956_1978,
         share_1936_1955, share_1956_1978, everything()) %>% 
  rename(province_code = PARENT) %>%
  janitor::clean_names()

Hmisc::describe(spanish_cohorts_arg$share_1936_1955)
Hmisc::describe(spanish_cohorts_arg$share_1956_1978)

class(spanish_cohorts_arg)

saveRDS(spanish_cohorts_arg, "Data Out/spanish_cohorts_arg.rds")

spanish_cohorts_arg_singeo <- spanish_cohorts_arg %>% 
  st_drop_geometry()
class(spanish_cohorts_arg_singeo$mun_code)
write.csv(spanish_cohorts_arg_singeo, "Data Out/spanish_cohorts_arg.csv", row.names = FALSE)

## Españoles por municipio en cada ventana de llegada (USANDO CADA CENSO POR SEPARADO)
spanish_counts_sep <- censos_arg %>%
  mutate(
    spanish_born = ifelse(bplcountry == 43120, 1, 0),
    mun_code = str_sub(as.character(geo2_ar), -6, -1) # ultimos 3 digitos municipios, primeros 3 provincia 
  ) %>%
  filter(
    spanish_born == 1,
    !yrimm %in% c(0, 9998, 9999)   # Not in universe, In transit, Unknown
  ) %>%
  filter(
    (year == 1970 & yrimm >= 1936 & yrimm <= 1978) |
      (year == 1980 & yrimm >= 1936 & yrimm <= 1978)
  ) %>%
  mutate(
    cohort = case_when(
      year == 1970 & yrimm >= 1936 & yrimm <= 1978 ~ "arrived_36_78_c70",
      year == 1980 & yrimm >= 1936 & yrimm <= 1978 ~ "arrived_36_78_c80"
    )
  ) %>%
  group_by(mun_code, cohort) %>%
  summarise(
    n_spanish = sum(perwt, na.rm = TRUE),
    unweighted_n = n(),
    .groups = "drop"
  ) %>%
  pivot_wider(
    names_from = cohort,
    values_from = c(n_spanish, unweighted_n),
    values_fill = 0
  ) %>%
  arrange(mun_code)

anti_join(spanish_counts_sep, spanish_counts, by = "mun_code") %>% 
  select(mun_code) %>% 
  View() # hay municipios en spanish_counts_sep sin un match en spanish_counts

anti_join(spanish_counts, spanish_counts_sep, by = "mun_code") %>% 
  select(mun_code) %>% 
  View() # no hay municipios en spanish_counts sin un match en spanish_counts_sep --> españoles que llegaron en la primera ventana identificados con el censo del 80

# Left join con todos los municipios
spanish_cohorts_sep <- municipios_arg_sf %>%
  filter(!mun_code %in% c("094004", "094003", "030000")) %>% # elimino las islas y entre rios unknown district
  left_join(spanish_counts_sep, by = "mun_code") %>%
  mutate(
    across(starts_with("n_spanish"), ~replace_na(., 0)),
    across(starts_with("unweighted_n"), ~replace_na(., 0))
  ) %>%
  arrange(mun_code)

# Unir y calcular shares
spanish_cohorts_sep <- spanish_cohorts_sep %>%
  left_join(total_pop_arg, by = "mun_code") %>%
  mutate(
    share_36_78_c70 = 100 * n_spanish_arrived_36_78_c70 / total_pop_arrived_1936_1955,
    share_36_78_c80 = 100 * n_spanish_arrived_36_78_c80 / total_pop_arrived_1956_1978
  ) %>% 
  select(mun_code, share_36_78_c70, share_36_78_c80) %>% 
  janitor::clean_names()
spanish_cohorts_sep <- spanish_cohorts_sep %>% 
  st_drop_geometry()

Hmisc::describe(spanish_cohorts_sep$share_36_78_c70)
Hmisc::describe(spanish_cohorts_sep$share_36_78_c80)

# Uno la data a spanish_cohorts_arg
spanish_cohorts_arg <- spanish_cohorts_arg %>% 
  left_join(spanish_cohorts_sep, by = "mun_code")

}
# -------------------- #
#   Cartografía Arg
# -------------------- #
{

# Chequeo que no haya NA en variables clave
any(is.na(municipios_arg_sf$GEOLEVEL2))
any(is.na(spanish_arg$year))
any(is.na(spanish_arg_70$year))
any(is.na(spanish_arg_80$year))

# Uno la data de españoles con la cartografía por año
spanish_arg_70 <- spanish_arg_70 %>%
  full_join(municipios_arg_sf, by = "mun_code") %>% 
  # chequeo
  mutate(solo_df1 = if_else(is.na(GEOLEVEL2), 1, 0),
         solo_df2 = if_else(is.na(year), 1, 0)
  )
class(spanish_arg_70)
spanish_arg_70 <- sf::st_as_sf(spanish_arg_70)

spanish_arg_80 <- spanish_arg_80 %>%
  full_join(municipios_arg_sf, by = "mun_code") %>% 
  # chequeo
  mutate(solo_df1 = if_else(is.na(GEOLEVEL2), 1, 0),
         solo_df2 = if_else(is.na(year), 1, 0)
         )
class(spanish_arg_80)
spanish_arg_80 <- sf::st_as_sf(spanish_arg_80)

# Chequeos
table(spanish_arg_70$solo_df1) 
table(spanish_arg_70$solo_df2)
spanish_arg_70 %>% filter(solo_df1 == 1) %>% View() # 99999 es unknown
spanish_arg_70 %>% filter(solo_df2 == 1) %>% View() 
# No estan en el censo:
  # 94003 South Georgia and South Sandwich Islands (islas britanicas)
  # 94004 Falkland Islands

spanish_arg_70 <- spanish_arg_70 %>% 
  filter(!mun_code %in% c("099999", "094003", "094004"))

table(spanish_arg_80$solo_df1) 
table(spanish_arg_80$solo_df2)
spanish_arg_80 %>% filter(solo_df2 == 1) %>% View() 

spanish_arg_80 <- spanish_arg_80 %>% 
  filter(mun_code!="094003", mun_code!="094004")

# No estan en el censo:
  # 94003 South Georgia and South Sandwich Islands (islas britanicas)
  # 94004 Falkland Islands	
  # 30000 Entre Rios [district unknown]
}
# -------------------- #
#  Distribuciones
# -------------------- #
{
## Distribución de españoles por municipio en 1970 ##

# con %
psych::describe(spanish_arg_70$share_spanish_born)

map70 <-  ggplot(spanish_arg_70) +
  geom_sf(aes(fill = share_spanish_born), color = "grey40", linewidth = 0.3) +
  scale_fill_distiller(
    palette = "Blues", direction = 1,
    limits = c(0, 6), oob = scales::squish,
    #labels = scales::label_number(accuracy = 0.1, suffix = "%"),
    #name = "% Spanish born",
    name = NULL,
    na.value = "grey80" 
  ) + 
  #labs(title = "Spanish-born population by municipality in 1970") +
  theme_void(base_family = "Times New Roman")+
  theme(
    plot.margin = margin(t = 0, r = 4, b = 0, l = 0),  # top, right, bottom, left
    plot.title = element_text(hjust = 0.5, size = 18, face = "bold"),
    legend.text = element_text(size = 14.5),
    legend.title = element_text(size = 15, margin = margin(b = 12)),
    #legend.position = "right"
    legend.position = "none"
  )
map70
ggsave("Output/map_share_spanish_arg_1970_v2.png", width = 5, height = 8, dpi = 300, bg = "white")
ggsave("Output/map_share_spanish_arg_1970_v2.pdf", plot = map70, width = 8, height = 5, bg = "white")

# con log
psych::describe(spanish_arg_70$log_spanish_born)

ggplot(spanish_arg_70) +
  geom_sf(aes(fill = log_spanish_born), color = "grey30", linewidth = 0.01) +
  scale_fill_distiller(
    palette = "Blues", direction = 1,
    limits = c(0, 13), oob = scales::squish,
    labels = scales::label_number(accuracy = 0.1),
    name = "log(1 + Spanish-born)",
    na.value = "grey80" 
  ) + 
  labs(title = "Spanish-born population by municipality in 1970") +
  theme_void() +
  theme(
    plot.margin = margin(t = 0, r = 5, b = 0, l = 0),  # top, right, bottom, left
    plot.title = element_text(hjust = 0.5, size = 10, face = "bold"),
    legend.text = element_text(size = 8),
    legend.title = element_text(size = 9)
  )

ggsave("Output/Argentina/map_log_spanish_arg_1970.png", width = 8, height = 6, dpi = 300, bg = "white")

## Distribución de españoles por municipio en 1980 ##

# con %
psych::describe(spanish_arg_80$share_spanish_born)

map80 <- ggplot(spanish_arg_80) +
  geom_sf(aes(fill = share_spanish_born), color = "grey30", linewidth = 0.3) +
  scale_fill_distiller(
    palette = "Blues", direction = 1,
    limits = c(0, 6), oob = scales::squish,
    #labels = scales::label_number(accuracy = 0.1),
    name = "% Spanish born",
    na.value = "grey80" 
  ) + 
  #labs(title = "Spanish-born population by municipality in 1980") +
  theme_void(base_family = "Times New Roman") +
  theme(
    plot.margin = margin(t = 0, r = 4, b = 0, l = 0),  # top, right, bottom, left
    plot.title = element_text(hjust = 0.5, size = 10, face = "bold"),
    legend.text = element_text(size = 15),
    legend.title = element_text(size = 15, margin = margin(b = 12)),
    legend.position = "right"
  )
map80
ggsave("Output/map_share_spanish_arg_1980_v2.png", plot = map80, width = 5, height = 8, dpi = 300, bg = "white")
ggsave("Output/map_share_spanish_arg_1980_v2.pdf", plot = map80, width = 6, height = 8, bg = "white")

# con log
psych::describe(spanish_arg_80$log_spanish_born)

ggplot(spanish_arg_80) +
  geom_sf(aes(fill = log_spanish_born), color = "grey30", linewidth = 0.01) +
  scale_fill_distiller(
    palette = "Blues", direction = 1,
    limits = c(0, 12), oob = scales::squish,
    labels = scales::label_number(accuracy = 0.1),
    name = "log(1 + Spanish-born)",
    na.value = "grey80" 
  ) + 
  labs(title = "Spanish-born population by municipality in 1980") +
  theme_void() +
  theme(
    plot.margin = margin(t = 0, r = 5, b = 0, l = 0),  # top, right, bottom, left
    plot.title = element_text(hjust = 0.5, size = 10, face = "bold"),
    legend.text = element_text(size = 8),
    legend.title = element_text(size = 9)
  )

ggsave("Output/Argentina/map_log_spanish_arg_1980.png", width = 8, height = 6, dpi = 300, bg = "white")

## Distribución de años de llegada de los españoles ##

# Censo de 1970

censo_arg_1970_filter <- censo_arg_1970 %>%
  # me quedo solo con los españoles
  filter(bplcountry == 43120)

psych::describe(censo_arg_1970_filter$yrimm)

censo_arg_1970_filter <- censo_arg_1970_filter %>%
  #filter(yrimm >= 1900, yrimm <= 1970) # me quedo con los que llegaron entre 1900 y 1970
  filter(yrimm!=9999, yrimm!=9998, yrimm!=0) # elimino los Unknown, In transit y Not in universe

# con shares
ggplot(censo_arg_1970_filter, aes(x = yrimm, weight = perwt)) +
  geom_histogram(
    aes(y = after_stat(count / sum(count))),
    binwidth = 5,
    fill = "steelblue",
    color = "white"
  ) +
  # lineas verticales indicando los años 1936 y 1975
  geom_vline(xintercept = 1936, linetype = "dashed", color = "red") +
  geom_vline(xintercept = 1975, linetype = "dashed", color = "red") +
  labs(
    title = "Year of arrival of Spanish-born population in Argentina (Census 1970)",
    subtitle = "Weighted distribution using census sampling weights",
    x = "Year of arrival",
    y = "Share"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(color = "black"),
    axis.text.y = element_text(color = "black"),
    axis.title.x = element_text(color = "black"),
    axis.title.y = element_text(color = "black"),
    plot.title = element_text(hjust = 0.5, size = 11, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5, size = 9)
  )

# con conteos
ggplot(censo_arg_1970_filter, aes(x = yrimm, weight = perwt)) +
  geom_histogram(
    binwidth = 5,
    boundary = 1875, 
    fill = "grey70",
    color = "white"
  ) +
  # líneas verticales indicando los años 1936 y 1975
  geom_vline(xintercept = 1936, linetype = "dashed", color = "red") +
  geom_vline(xintercept = 1975, linetype = "dashed", color = "red") +
  # anotación del período de exposición
  annotate("text", x = 1955, y = 65000, label = "Exposure years", color = "red", size = 3.5, fontface = "bold") +
  labs(
    title = "Year of arrival of Spanish-born population in Argentina (Census 1970)",
    subtitle = "Weighted counts using census sampling weights",
    x = "Year of arrival",
    y = "Weighted count"
  ) +
  scale_x_continuous(breaks = seq(1875, 1985, by = 10)) +
  scale_y_continuous(labels = comma) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(color = "black"),
    axis.text.y = element_text(color = "black"),
    axis.title.x = element_text(color = "black"),
    axis.title.y = element_text(color = "black"),
    plot.title = element_text(hjust = 0.5, size = 11, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5, size = 9)
  )

ggsave("Output/Argentina/hist_yrimm_arg_1970.png", width = 8, height = 6, dpi = 300, bg = "white")

# con conteos, separando periodos
hist70 <- ggplot(censo_arg_1970_filter, aes(x = yrimm, weight = perwt)) +
  annotate(
    "rect",
    xmin = 1936, xmax = 1955.5,
    ymin = -Inf, ymax = Inf,
    fill =  "lightblue", alpha = 0.18
  ) +
  annotate(
    "rect",
    xmin = 1955.5, xmax = 1978,
    ymin = -Inf, ymax = Inf,
    fill ="lightsalmon", alpha = 0.18
  ) +
  geom_histogram(
    binwidth = 5,
    boundary = 1885,
    fill = "grey70",
    color = "white"
  ) +
  geom_vline(xintercept = c(1936, 1955.5),
             linetype = "solid", color = "steelblue4", linewidth = 0.4) +
  geom_vline(xintercept = c(1955.5, 1978),
             linetype = "solid", color = "tomato3", linewidth = 0.4) +
  annotate("text", x = 1945.8, y = 65000,
           label = "Presumed\nexile window",
           color = "steelblue4", size = 5.4, fontface = "bold") +
  annotate("text", x = 1967, y = 65000,
           label = "Documented\nexile window",
           color = "tomato3", size = 5.4, fontface = "bold") +
  labs(
    #title = "1970 Census",
    #subtitle = "Weighted counts using census sampling weights",
    x = "Year of arrival",
    y = "Weighted count"
  ) +
  scale_x_continuous(breaks = seq(1880, 1985, by = 10), limits = c(1880, 1980)) +
  scale_y_continuous(breaks = seq(0, 80000, by = 20000),  limits = c(0, 85000), labels = comma) +
  theme_minimal(base_family = "Times New Roman") +
  theme(
    axis.text.x = element_text(
      color = "black",
      size = 18,
      angle = 45,
      hjust = 1,
      vjust = 1
    ),
    axis.text.y = element_text(color = "black", size = 18),
    axis.title = element_text(color = "black", size = 18),
    axis.ticks.x = element_line(color = "black", linewidth = 0.3),
    axis.ticks.y = element_line(color = "black", linewidth = 0.3),
    axis.ticks.length = unit(0.15, "cm"),
    axis.line.x = element_line(color = "black", linewidth = 0.3),
    axis.line.y = element_line(color = "black", linewidth = 0.3),
    plot.title = element_text(hjust = 0.5, size = 18, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5, size = 18),
    panel.grid.major = element_blank(),
    panel.grid.minor.x = element_blank()
  )
hist70
ggsave("Output/hist_yrimm_arg_1970_v2.png", plot = hist70, width = 8, height = 6, dpi = 300, bg = "white")
ggsave("Output/hist_yrimm_arg_1970_v2.pdf", plot = hist70, width = 8, height = 6, bg = "white")

# Censo de 1980

censo_arg_1980_filter <- censo_arg_1980 %>%
  # me quedo solo con los españoles
  filter(bplcountry == 43120)

psych::describe(censo_arg_1980_filter$yrimm)

censo_arg_1980_filter <- censo_arg_1980_filter %>%
  #filter(yrimm >= 1900, yrimm <= 1970) # me quedo con los que llegaron entre 1900 y 1970
  filter(yrimm!=9999, yrimm!=9998, yrimm!=0) # elimino los Unknown, In transit y Not in universe

# con shares
ggplot(censo_arg_1980_filter, aes(x = yrimm, weight = perwt)) +
  geom_histogram(
    aes(y = after_stat(count / sum(count))),
    binwidth = 5,
    fill = "steelblue",
    color = "white"
  ) +
  # lineas verticales indicando los años 1936 y 1975
  geom_vline(xintercept = 1936, linetype = "dashed", color = "red") +
  geom_vline(xintercept = 1975, linetype = "dashed", color = "red") +
  labs(
    title = "Year of arrival of Spanish-born population in Argentina (Census 1980)",
    subtitle = "Weighted distribution using census sampling weights",
    x = "Year of arrival",
    y = "Share"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(color = "black"),
    axis.text.y = element_text(color = "black"),
    axis.title.x = element_text(color = "black"),
    axis.title.y = element_text(color = "black"),
    plot.title = element_text(hjust = 0.5, size = 11, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5, size = 9)
  )

# con conteos
ggplot(censo_arg_1980_filter, aes(x = yrimm, weight = perwt)) +
  geom_histogram(
    binwidth = 5,
    boundary = 1885, 
    fill = "grey70",
    color = "white"
  ) +
  # líneas verticales indicando los años 1936 y 1975
  geom_vline(xintercept = 1936, linetype = "dashed", color = "red") +
  geom_vline(xintercept = 1975, linetype = "dashed", color = "red") +
  # anotación del período de exposición
  annotate("text", x = 1955, y = 65000, label = "Exposure years", color = "red", size = 3.5, fontface = "bold") +
  labs(
    title = "Year of arrival of Spanish-born population in Argentina (Census 1980)",
    subtitle = "Weighted counts using census sampling weights",
    x = "Year of arrival",
    y = "Weighted count"
  ) +
  scale_x_continuous(breaks = seq(1885, 1985, by = 10)) +
  scale_y_continuous(
    #breaks = seq(0, 70000), 
    #limits = seq(0, 70000),
    labels = comma) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(color = "black"),
    axis.text.y = element_text(color = "black"),
    axis.title.x = element_text(color = "black"),
    axis.title.y = element_text(color = "black"),
    plot.title = element_text(hjust = 0.5, size = 11, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5, size = 9)
  )

ggsave("Output/Argentina/hist_yrimm_arg_1980.png", width = 8, height = 6, dpi = 300, bg = "white")

# con conteos, separando periodos
hist80 <- ggplot(censo_arg_1980_filter, aes(x = yrimm, weight = perwt)) +
  annotate(
    "rect",
    xmin = 1936, xmax = 1955.5,
    ymin = -Inf, ymax = Inf,
    fill =  "lightblue", alpha = 0.18
  ) +
  annotate(
    "rect",
    xmin = 1955.5, xmax = 1978,
    ymin = -Inf, ymax = Inf,
    fill ="lightsalmon", alpha = 0.18
  ) +
  geom_histogram(
    binwidth = 5,
    boundary = 1885,
    fill = "grey70",
    color = "white"
  ) +
  geom_vline(xintercept = c(1936, 1955.5),
             linetype = "solid", color = "steelblue4", linewidth = 0.4) +
  geom_vline(xintercept = c(1955.5, 1978),
             linetype = "solid", color = "tomato3", linewidth = 0.4) +
  annotate("text", x = 1945.8, y = 65000,
           label = "Presumed\nexile window",
           color = "steelblue4", size = 5.4, fontface = "bold") +
  annotate("text", x = 1967, y = 65000,
           label = "Documented\nexile window",
           color = "tomato3", size = 5.4, fontface = "bold") +
  labs(
    #title = "1980 Census",
    #subtitle = "Weighted counts using census sampling weights",
    x = "Year of arrival",
    y = "Weighted count"
  ) +
  scale_x_continuous(breaks = seq(1880, 1985, by = 10), limits = c(1880, 1980)) +
  scale_y_continuous(breaks = seq(0, 80000, by = 20000),  limits = c(0, 85000), labels = comma) +
  theme_minimal(base_family = "Times New Roman") +
  theme(
    axis.text.x = element_text(
      color = "black",
      size = 18,
      angle = 45,
      hjust = 1,
      vjust = 1
    ),
    axis.text.y = element_text(color = "black", size = 18),
    axis.title = element_text(color = "black", size = 18),
    axis.ticks.x = element_line(color = "black", linewidth = 0.3),
    axis.ticks.y = element_line(color = "black", linewidth = 0.3),
    axis.ticks.length = unit(0.15, "cm"),
    axis.line.x = element_line(color = "black", linewidth = 0.3),
    axis.line.y = element_line(color = "black", linewidth = 0.3),
    plot.title = element_text(hjust = 0.5, size = 18, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5, size = 18),
    panel.grid.major = element_blank(),
    panel.grid.minor.x = element_blank()
  )
hist80 
ggsave("Output/hist_yrimm_arg_1980_v2.png", plot = hist80, width = 8, height = 6, dpi = 300, bg = "white")
ggsave("Output/hist_yrimm_arg_1980_v2.pdf", plot = hist80, width = 8, height = 6, dpi = 300, bg = "white")

# Combino hist 70 con hist 80

hist70 + hist80 + plot_annotation(
  title = "Year of arrival of Spanish-born population in Argentina",
  subtitle = "Weighted counts using census sampling weights",
  theme = theme(
    text = element_text(family = "Times New Roman"),
    plot.title = element_text(hjust = 0.5, size = 12, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5, size = 10)
  )
)
ggsave("Output/hist_yrimm_arg_combined.png", width = 12, height = 6, dpi = 300, bg = "white")

}
