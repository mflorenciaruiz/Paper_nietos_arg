# ============================================================
# Estadísticas de inmigrantes nacidos en Argentina en España
# Fuente: EMCR 2023 (INE España)
# ============================================================

library(tidyverse)
library(scales)
library(gridExtra)
library(knitr)
library(openxlsx)

# Paths
data_path <- "/Users/florenciaruiz/BID 2/Paper Valerie/Nietos/Argentina/Data Raw/España/datos_2023/R/EMCR_2023.RData"
out_path  <- "/Users/florenciaruiz/BID 2/Paper Valerie/Nietos/Argentina/Output/España/"

# Paleta de colores consistente
col_main  <- "#1B4F8A"
col_sec   <- "#C9372C"
col_pair  <- c("#1B4F8A", "#C9372C")
col_scale <- c("#1B4F8A", "#3A7DC9", "#E8A838", "#C9372C", "#6BAE75",
               "#9B59B6", "#E67E22", "#2ECC71", "#E74C3C", "#1ABC9C")

# ── 1. CARGA Y FILTRO ──────────────────────────────────────
load(data_path)

arg <- Microdatos %>%
  filter(PAIS_NACIM == "340", TIPO_MIGR == "IE") %>%
  mutate(
    Sexo       = ifelse(SEXO == "1", "Hombre", "Mujer"),
    Grupo_edad = cut(EDAD,
                     breaks = c(0, 14, 24, 34, 44, 54, 64, Inf),
                     labels = c("0-14", "15-24", "25-34", "35-44",
                                "45-54", "55-64", "65+"),
                     right  = TRUE),
    Trimestre  = TRIM,
    Nac_label  = case_when(
      PAIS_NACIO == "340" ~ "Argentina",
      PAIS_NACIO == "115" ~ "Italia",
      PAIS_NACIO == "108" ~ "España",
      PAIS_NACIO == "126" ~ "Alemania",
      PAIS_NACIO == "198" ~ "Otros UE",
      PAIS_NACIO == "122" ~ "Polonia",
      PAIS_NACIO == "110" ~ "Francia",
      PAIS_NACIO == "123" ~ "Portugal",
      PAIS_NACIO == "348" ~ "Perú",
      TRUE                ~ "Otras"
    ),
    Proc_label = case_when(
      PAIS_PROC_DEST == "340" ~ "Argentina",
      PAIS_PROC_DEST == "115" ~ "Italia",
      PAIS_PROC_DEST == "126" ~ "Alemania",
      PAIS_PROC_DEST == "302" ~ "EE.UU.",
      PAIS_PROC_DEST == "110" ~ "Francia",
      PAIS_PROC_DEST == "199" ~ "Otros Europa",
      PAIS_PROC_DEST == "125" ~ "Reino Unido",
      PAIS_PROC_DEST == "499" ~ "Otros Asia",
      PAIS_PROC_DEST == "348" ~ "Perú",
      PAIS_PROC_DEST == "347" ~ "Paraguay",
      TRUE                    ~ "Otros"
    ),
    Prov_label = case_when(
      PROVDEST == "08" ~ "Barcelona",
      PROVDEST == "28" ~ "Madrid",
      PROVDEST == "46" ~ "Valencia",
      PROVDEST == "29" ~ "Málaga",
      PROVDEST == "07" ~ "Baleares",
      PROVDEST == "03" ~ "Alicante",
      PROVDEST == "43" ~ "Tarragona",
      PROVDEST == "17" ~ "Girona",
      PROVDEST == "35" ~ "Las Palmas",
      PROVDEST == "18" ~ "Granada",
      PROVDEST == "38" ~ "Sta. Cruz Tenerife",
      PROVDEST == "15" ~ "A Coruña",
      PROVDEST == "04" ~ "Almería",
      PROVDEST == "30" ~ "Murcia",
      PROVDEST == "36" ~ "Pontevedra",
      TRUE             ~ "Otras"
    ),
    Tamunio = case_when(
      TAMUDEST == "1" ~ "< 10.000 hab.",
      TAMUDEST == "2" ~ "10.001-20.000",
      TAMUDEST == "3" ~ "20.001-50.000",
      TAMUDEST == "4" ~ "50.001-100.000",
      TAMUDEST == "5" ~ "> 100.000 (no capital)",
      TAMUDEST == "6" ~ "Capital de provincia",
      TRUE            ~ "Sin dato"
    )
  )

N_total <- nrow(arg)
cat("N total argentinos inmigrantes en España 2023:", N_total, "\n")


# ============================================================
# TABLAS
# ============================================================

# ── T1. Distribución por sexo ──
t_sexo <- arg %>%
  count(Sexo) %>%
  mutate(Pct = round(n / sum(n) * 100, 1)) %>%
  rename(N = n, `%` = Pct)

# ── T2. Distribución por grupo de edad ──
t_edad <- arg %>%
  count(Grupo_edad) %>%
  mutate(Pct = round(n / sum(n) * 100, 1)) %>%
  rename(`Grupo de edad` = Grupo_edad, N = n, `%` = Pct)

# ── T3. Distribución por sexo y grupo de edad ──
t_sex_edad <- arg %>%
  count(Sexo, Grupo_edad) %>%
  group_by(Sexo) %>%
  mutate(Pct_dentro = round(n / sum(n) * 100, 1)) %>%
  ungroup() %>%
  rename(`Grupo de edad` = Grupo_edad, N = n, `% dentro de sexo` = Pct_dentro)

# ── T4. Distribución por nacionalidad ──
t_nac <- arg %>%
  count(Nac_label) %>%
  arrange(desc(n)) %>%
  mutate(Pct = round(n / sum(n) * 100, 1)) %>%
  rename(Nacionalidad = Nac_label, N = n, `%` = Pct)

# ── T5. Provincia de destino (top 15 + resto) ──
t_prov_raw <- arg %>%
  count(Prov_label) %>%
  arrange(desc(n))

otras_prov <- t_prov_raw %>%
  filter(Prov_label == "Otras") %>%
  pull(n)

t_prov <- t_prov_raw %>%
  filter(Prov_label != "Otras") %>%
  bind_rows(tibble(Prov_label = "Otras", n = otras_prov)) %>%
  mutate(Pct = round(n / sum(n) * 100, 1)) %>%
  rename(`Provincia de destino` = Prov_label, N = n, `%` = Pct)

# ── T6. País de procedencia ──
t_proc <- arg %>%
  count(Proc_label) %>%
  arrange(desc(n)) %>%
  mutate(Pct = round(n / sum(n) * 100, 1)) %>%
  rename(`País de procedencia` = Proc_label, N = n, `%` = Pct)

# ── T7. Tamaño municipio destino ──
orden_tam <- c("< 10.000 hab.", "10.001-20.000", "20.001-50.000",
               "50.001-100.000", "> 100.000 (no capital)", "Capital de provincia")
t_tamunio <- arg %>%
  filter(Tamunio != "Sin dato") %>%
  count(Tamunio) %>%
  mutate(Tamunio = factor(Tamunio, levels = orden_tam)) %>%
  arrange(Tamunio) %>%
  mutate(Pct = round(n / sum(n) * 100, 1)) %>%
  rename(`Tamaño municipio destino` = Tamunio, N = n, `%` = Pct)

# ── T8. Distribución por trimestre ──
t_trim <- arg %>%
  count(Trimestre) %>%
  mutate(Pct = round(n / sum(n) * 100, 1)) %>%
  rename(N = n, `%` = Pct)

# Edad media por sexo
t_edad_media <- arg %>%
  group_by(Sexo) %>%
  summarise(
    `Edad media` = round(mean(EDAD, na.rm = TRUE), 1),
    `Mediana`    = median(EDAD, na.rm = TRUE),
    `SD`         = round(sd(EDAD, na.rm = TRUE), 1),
    N            = n()
  )


# ============================================================
# EXPORTAR TABLAS A EXCEL
# ============================================================

wb <- createWorkbook()

add_table <- function(wb, sheet_name, df, title) {
  addWorksheet(wb, sheet_name)
  writeData(wb, sheet_name, title, startRow = 1, startCol = 1)
  writeDataTable(wb, sheet_name, df, startRow = 3, startCol = 1,
                 tableStyle = "TableStyleMedium2")
  setColWidths(wb, sheet_name, cols = 1:ncol(df), widths = "auto")
}

add_table(wb, "1_Sexo",       t_sexo,      "Distribución por sexo – Nacidos en Argentina, inmigrantes en España 2023")
add_table(wb, "2_Edad",       t_edad,      "Distribución por grupo de edad")
add_table(wb, "3_Sexo_Edad",  t_sex_edad,  "Distribución por sexo y grupo de edad")
add_table(wb, "4_Nacion",     t_nac,       "Distribución por nacionalidad")
add_table(wb, "5_Provincia",  t_prov,      "Provincia de destino en España")
add_table(wb, "6_Procedencia",t_proc,      "País de procedencia")
add_table(wb, "7_TamMunicio", t_tamunio,   "Tamaño del municipio de destino")
add_table(wb, "8_Trimestre",  t_trim,      "Distribución por trimestre de llegada")
add_table(wb, "9_EdadMedia",  t_edad_media,"Edad media por sexo")

saveWorkbook(wb, paste0(out_path, "tablas_arg_esp_2023.xlsx"), overwrite = TRUE)
cat("Tablas exportadas a Excel.\n")


# ============================================================
# GRÁFICOS
# ============================================================

theme_paper <- theme_minimal(base_size = 12) +
  theme(
    plot.title    = element_text(face = "bold", size = 13, hjust = 0),
    plot.subtitle = element_text(size = 10, color = "grey40"),
    axis.title    = element_text(size = 10),
    legend.position = "bottom",
    panel.grid.minor = element_blank()
  )

# ── G1. Pirámide de edad ──────────────────────────────────
piramide <- arg %>%
  filter(!is.na(Grupo_edad)) %>%
  count(Grupo_edad, Sexo) %>%
  mutate(n_plot = ifelse(Sexo == "Hombre", -n, n))

g1 <- ggplot(piramide, aes(x = Grupo_edad, y = n_plot, fill = Sexo)) +
  geom_col(width = 0.8) +
  coord_flip() +
  scale_y_continuous(
    labels = function(x) comma(abs(x)),
    breaks = seq(-8000, 12000, 2000)
  ) +
  scale_fill_manual(values = col_pair) +
  labs(
    title    = "Pirámide de edad",
    subtitle = "Nacidos en Argentina · Inmigración a España 2023",
    x = "Grupo de edad",
    y = "Número de personas",
    fill = NULL
  ) +
  theme_paper

ggsave(paste0(out_path, "G1_piramide_edad.png"), g1,
       width = 7, height = 5, dpi = 300)

# ── G2. Distribución por sexo ────────────────────────────
sexo_plot <- arg %>% count(Sexo) %>% mutate(Pct = n / sum(n) * 100)

g2 <- ggplot(sexo_plot, aes(x = Sexo, y = n, fill = Sexo)) +
  geom_col(width = 0.5) +
  geom_text(aes(label = paste0(round(Pct, 1), "%")),
            vjust = -0.5, size = 4, fontface = "bold") +
  scale_fill_manual(values = col_pair) +
  scale_y_continuous(labels = comma, expand = expansion(mult = c(0, 0.1))) +
  labs(
    title    = "Distribución por sexo",
    subtitle = "Nacidos en Argentina · Inmigración a España 2023",
    x = NULL, y = "N", fill = NULL
  ) +
  theme_paper +
  theme(legend.position = "none")

ggsave(paste0(out_path, "G2_sexo.png"), g2,
       width = 5, height = 4, dpi = 300)

# ── G3. Distribución de edades (histograma) ──────────────
g3 <- ggplot(arg, aes(x = EDAD, fill = Sexo)) +
  geom_histogram(binwidth = 5, alpha = 0.8, position = "identity", color = "white") +
  scale_fill_manual(values = col_pair) +
  scale_y_continuous(labels = comma) +
  labs(
    title    = "Distribución de edades",
    subtitle = "Nacidos en Argentina · Inmigración a España 2023",
    x = "Edad (años)", y = "N", fill = NULL
  ) +
  theme_paper

ggsave(paste0(out_path, "G3_edades_histograma.png"), g3,
       width = 7, height = 4, dpi = 300)

# ── G4. Provincias de destino (top 12) ───────────────────
prov_top <- arg %>%
  filter(Prov_label != "Otras") %>%
  count(Prov_label) %>%
  arrange(desc(n)) %>%
  slice_head(n = 12) %>%
  mutate(Prov_label = fct_reorder(Prov_label, n),
         Pct = n / N_total * 100)

g4 <- ggplot(prov_top, aes(x = Prov_label, y = n, fill = n)) +
  geom_col(width = 0.75) +
  geom_text(aes(label = paste0(round(Pct, 1), "%")),
            hjust = -0.1, size = 3.2) +
  coord_flip() +
  scale_fill_gradient(low = "#AECDE8", high = col_main) +
  scale_y_continuous(labels = comma, expand = expansion(mult = c(0, 0.15))) +
  labs(
    title    = "Principales provincias de destino",
    subtitle = "Nacidos en Argentina · Inmigración a España 2023",
    x = NULL, y = "N"
  ) +
  theme_paper +
  theme(legend.position = "none")

ggsave(paste0(out_path, "G4_provincias_destino.png"), g4,
       width = 7, height = 5, dpi = 300)

# ── G5. Nacionalidad ──────────────────────────────────────
nac_plot <- arg %>%
  count(Nac_label) %>%
  arrange(desc(n)) %>%
  mutate(Nac_label = fct_reorder(Nac_label, n),
         Pct = n / sum(n) * 100)

g5 <- ggplot(nac_plot, aes(x = Nac_label, y = n, fill = Nac_label)) +
  geom_col(width = 0.75) +
  geom_text(aes(label = paste0(round(Pct, 1), "%")),
            hjust = -0.1, size = 3.2) +
  coord_flip() +
  scale_fill_manual(values = col_scale) +
  scale_y_continuous(labels = comma, expand = expansion(mult = c(0, 0.15))) +
  labs(
    title    = "Distribución por nacionalidad",
    subtitle = "Nacidos en Argentina · Inmigración a España 2023",
    x = NULL, y = "N"
  ) +
  theme_paper +
  theme(legend.position = "none")

ggsave(paste0(out_path, "G5_nacionalidad.png"), g5,
       width = 7, height = 5, dpi = 300)

# ── G6. País de procedencia ───────────────────────────────
proc_plot <- arg %>%
  count(Proc_label) %>%
  arrange(desc(n)) %>%
  filter(Proc_label != "Otros") %>%
  mutate(Proc_label = fct_reorder(Proc_label, n),
         Pct = n / N_total * 100)

g6 <- ggplot(proc_plot, aes(x = Proc_label, y = n, fill = n)) +
  geom_col(width = 0.75) +
  geom_text(aes(label = paste0(round(Pct, 1), "%")),
            hjust = -0.1, size = 3.2) +
  coord_flip() +
  scale_fill_gradient(low = "#F5B7A0", high = col_sec) +
  scale_y_continuous(labels = comma, expand = expansion(mult = c(0, 0.15))) +
  labs(
    title    = "País de procedencia",
    subtitle = "Nacidos en Argentina · Inmigración a España 2023",
    x = NULL, y = "N"
  ) +
  theme_paper +
  theme(legend.position = "none")

ggsave(paste0(out_path, "G6_pais_procedencia.png"), g6,
       width = 7, height = 5, dpi = 300)

# ── G7. Tamaño municipio destino ──────────────────────────
tamunio_plot <- arg %>%
  filter(Tamunio != "Sin dato") %>%
  count(Tamunio) %>%
  mutate(
    Tamunio = factor(Tamunio, levels = rev(orden_tam)),
    Pct = n / sum(n) * 100
  )

g7 <- ggplot(tamunio_plot, aes(x = Tamunio, y = Pct, fill = Tamunio)) +
  geom_col(width = 0.75) +
  geom_text(aes(label = paste0(round(Pct, 1), "%")),
            hjust = -0.1, size = 3.2) +
  coord_flip() +
  scale_fill_manual(values = c("#1B4F8A","#2A6CAC","#3A8AD1",
                               "#6BAED6","#BDD7EE","#DEEAF8")) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
  labs(
    title    = "Tamaño del municipio de destino",
    subtitle = "Nacidos en Argentina · Inmigración a España 2023",
    x = NULL, y = "%"
  ) +
  theme_paper +
  theme(legend.position = "none")

ggsave(paste0(out_path, "G7_tamunio_municipio.png"), g7,
       width = 7, height = 4.5, dpi = 300)

# ── G8. Llegadas por trimestre ───────────────────────────
trim_plot <- arg %>% count(Trimestre)

g8 <- ggplot(trim_plot, aes(x = Trimestre, y = n, group = 1)) +
  geom_line(color = col_main, linewidth = 1.2) +
  geom_point(color = col_main, size = 3.5, fill = "white", shape = 21, stroke = 1.5) +
  geom_text(aes(label = comma(n)), vjust = -1, size = 3.5, color = col_main) +
  scale_y_continuous(labels = comma,
                     limits = c(min(trim_plot$n) * 0.85, max(trim_plot$n) * 1.12)) +
  labs(
    title    = "Llegadas por trimestre",
    subtitle = "Nacidos en Argentina · Inmigración a España 2023",
    x = "Trimestre", y = "N"
  ) +
  theme_paper

ggsave(paste0(out_path, "G8_trimestre.png"), g8,
       width = 6, height = 4, dpi = 300)

# ── G9. Pirámide por nacionalidad (argentinos vs italianos vs españoles) ──
piramide_nac <- arg %>%
  filter(Nac_label %in% c("Argentina", "Italia", "España")) %>%
  filter(!is.na(Grupo_edad)) %>%
  count(Nac_label, Grupo_edad, Sexo) %>%
  group_by(Nac_label) %>%
  mutate(Pct = n / sum(n) * 100,
         Pct_plot = ifelse(Sexo == "Hombre", -Pct, Pct))

g9 <- ggplot(piramide_nac, aes(x = Grupo_edad, y = Pct_plot, fill = Sexo)) +
  geom_col(width = 0.8) +
  coord_flip() +
  facet_wrap(~ Nac_label, ncol = 3) +
  scale_y_continuous(labels = function(x) paste0(abs(x), "%"),
                     breaks = seq(-20, 20, 5)) +
  scale_fill_manual(values = col_pair) +
  labs(
    title    = "Estructura de edad por sexo, según nacionalidad",
    subtitle = "Nacidos en Argentina · Inmigración a España 2023",
    x = "Grupo de edad", y = "% dentro de cada nacionalidad", fill = NULL
  ) +
  theme_paper

ggsave(paste0(out_path, "G9_piramide_por_nac.png"), g9,
       width = 10, height = 5, dpi = 300)

# ── G10. Heatmap: provincia destino × nacionalidad ────────
heat_data <- arg %>%
  filter(Nac_label %in% c("Argentina", "Italia", "España", "Otros UE", "Alemania"),
         Prov_label != "Otras") %>%
  count(Prov_label, Nac_label) %>%
  group_by(Nac_label) %>%
  mutate(Pct = n / sum(n) * 100) %>%
  ungroup()

g10 <- ggplot(heat_data, aes(x = Nac_label, y = fct_reorder(Prov_label, n, sum), fill = Pct)) +
  geom_tile(color = "white", linewidth = 0.5) +
  geom_text(aes(label = round(Pct, 1)), size = 2.8, color = "white") +
  scale_fill_gradient(low = "#BDD7EE", high = "#1B4F8A", name = "%") +
  labs(
    title    = "Distribución provincial según nacionalidad",
    subtitle = "Nacidos en Argentina · Inmigración a España 2023",
    x = "Nacionalidad", y = "Provincia destino"
  ) +
  theme_paper +
  theme(axis.text.x = element_text(angle = 30, hjust = 1))

ggsave(paste0(out_path, "G10_heatmap_prov_nac.png"), g10,
       width = 8, height = 6, dpi = 300)

cat("\n============================================\n")
cat("Todos los gráficos guardados en:\n", out_path, "\n")
cat("============================================\n")
