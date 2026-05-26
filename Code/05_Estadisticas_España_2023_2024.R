# ============================================================
# Estadísticas de inmigrantes nacidos en Argentina en España
# Fuente: EMCR 2023 y 2024 (INE España)
# ============================================================

library(tidyverse)
library(scales)
library(gridExtra)
library(openxlsx)

# Paths
path_23  <- "/Users/florenciaruiz/BID 2/Paper Valerie/Nietos/Argentina/Data Raw/España/datos_2023/R/EMCR_2023.RData"
path_24  <- "/Users/florenciaruiz/BID 2/Paper Valerie/Nietos/Argentina/Data Raw/España/datos_2024/R/EMCR_2024.RData"
out_path <- "/Users/florenciaruiz/BID 2/Paper Valerie/Nietos/Argentina/Output/España/"

# Paleta
col_23   <- "#1B4F8A"
col_24   <- "#C9372C"
col_pair <- c("2023" = "#1B4F8A", "2024" = "#C9372C")
col_sex  <- c("Male" = "#1B4F8A", "Female" = "#C9372C")
col_scale <- c("#1B4F8A","#3A7DC9","#E8A838","#C9372C","#6BAE75",
               "#9B59B6","#E67E22","#2ECC71","#E74C3C","#1ABC9C")

# ── 1. CARGA ────────────────────────────────────────────────
load(path_23)
micro_23 <- Microdatos
rm(Microdatos, Metadatos)

load(path_24)
micro_24 <- df_micro
rm(df_micro, df_meta)

# ── 2. FUNCIÓN DE PROCESAMIENTO ─────────────────────────────
procesar <- function(df, anyo) {
  df %>%
    filter(PAIS_NACIM == "340", TIPO_MIGR == "IE") %>%
    mutate(
      Anyo = as.character(anyo),
      Sexo = ifelse(SEXO == "1", "Male", "Female"),
      Grupo_edad = cut(EDAD,
                       breaks = c(0, 14, 24, 34, 44, 54, 64, Inf),
                       labels = c("0-14","15-24","25-34","35-44","45-54","55-64","65+"),
                       right  = TRUE),
      Nac_label = case_when(
        PAIS_NACIO == "340" ~ "Argentina",
        PAIS_NACIO == "115" ~ "Italy",
        PAIS_NACIO == "108" ~ "Spain",
        PAIS_NACIO == "126" ~ "Germany",
        PAIS_NACIO == "198" ~ "Other EU",
        PAIS_NACIO == "122" ~ "Poland",
        PAIS_NACIO == "110" ~ "France",
        PAIS_NACIO == "123" ~ "Portugal",
        PAIS_NACIO == "348" ~ "Peru",
        TRUE                ~ "Other"
      ),
      Proc_label = case_when(
        PAIS_PROC_DEST == "340" ~ "Argentina",
        PAIS_PROC_DEST == "115" ~ "Italy",
        PAIS_PROC_DEST == "126" ~ "Germany",
        PAIS_PROC_DEST == "302" ~ "USA",
        PAIS_PROC_DEST == "110" ~ "France",
        PAIS_PROC_DEST == "199" ~ "Other Europe",
        PAIS_PROC_DEST == "125" ~ "United Kingdom",
        PAIS_PROC_DEST == "499" ~ "Other Asia",
        PAIS_PROC_DEST == "348" ~ "Peru",
        PAIS_PROC_DEST == "347" ~ "Paraguay",
        TRUE                    ~ "Other"
      ),
      Prov_label = case_when(
        PROVDEST == "08" ~ "Barcelona",
        PROVDEST == "28" ~ "Madrid",
        PROVDEST == "46" ~ "Valencia",
        PROVDEST == "29" ~ "Malaga",
        PROVDEST == "07" ~ "Balearic Islands",
        PROVDEST == "03" ~ "Alicante",
        PROVDEST == "43" ~ "Tarragona",
        PROVDEST == "17" ~ "Girona",
        PROVDEST == "35" ~ "Las Palmas",
        PROVDEST == "18" ~ "Granada",
        PROVDEST == "38" ~ "Santa Cruz de Tenerife",
        PROVDEST == "15" ~ "A Coruna",
        PROVDEST == "04" ~ "Almeria",
        PROVDEST == "30" ~ "Murcia",
        PROVDEST == "36" ~ "Pontevedra",
        TRUE             ~ "Other"
      ),
      Tamunio = case_when(
        TAMUDEST == "1" ~ "< 10,000 inhab.",
        TAMUDEST == "2" ~ "10,001-20,000",
        TAMUDEST == "3" ~ "20,001-50,000",
        TAMUDEST == "4" ~ "50,001-100,000",
        TAMUDEST == "5" ~ "> 100,000 (non-capital)",
        TAMUDEST == "6" ~ "Provincial capital",
        TRUE            ~ "No data"
      ),
      Trimestre = TRIM
    )
}

arg_23 <- procesar(micro_23, 2023)
arg_24 <- procesar(micro_24, 2024)
arg    <- bind_rows(arg_23, arg_24)

N_23 <- nrow(arg_23)
N_24 <- nrow(arg_24)
cat("N 2023:", N_23, "| N 2024:", N_24,
    "| Variación:", round((N_24 - N_23) / N_23 * 100, 1), "%\n")

orden_tam <- c("< 10,000 inhab.","10,001-20,000","20,001-50,000",
               "50,001-100,000","> 100,000 (non-capital)","Provincial capital")


# ============================================================
# TABLAS COMPARATIVAS 2023 vs 2024
# ============================================================

make_comp <- function(df, var, label_var) {
  df %>%
    count(Anyo, !!sym(var)) %>%
    group_by(Anyo) %>%
    mutate(Pct = round(n / sum(n) * 100, 1)) %>%
    ungroup() %>%
    pivot_wider(names_from = Anyo,
                values_from = c(n, Pct),
                names_glue = "{Anyo}_{.value}") %>%
    rename(!!label_var := !!sym(var)) %>%
    select(all_of(label_var),
           `2023_n`, `2023_Pct`, `2024_n`, `2024_Pct`) %>%
    rename(`N 2023` = `2023_n`, `% 2023` = `2023_Pct`,
           `N 2024` = `2024_n`, `% 2024` = `2024_Pct`) %>%
    mutate(`Var. pp` = round(`% 2024` - `% 2023`, 1),
           `Var. %`  = round((`N 2024` - `N 2023`) / `N 2023` * 100, 1))
}

t_resumen <- tibble(
  Indicador    = c("Total inmigrantes", "Edad media", "Mediana edad",
                   "% Hombres", "% Mujeres"),
  `2023` = c(N_23,
             round(mean(arg_23$EDAD, na.rm=TRUE), 1),
             median(arg_23$EDAD, na.rm=TRUE),
             round(mean(arg_23$Sexo=="Hombre")*100, 1),
             round(mean(arg_23$Sexo=="Mujer")*100, 1)),
  `2024` = c(N_24,
             round(mean(arg_24$EDAD, na.rm=TRUE), 1),
             median(arg_24$EDAD, na.rm=TRUE),
             round(mean(arg_24$Sexo=="Hombre")*100, 1),
             round(mean(arg_24$Sexo=="Mujer")*100, 1))
) %>%
  mutate(`Variación` = round(`2024` - `2023`, 1))

t_sexo    <- make_comp(arg, "Sexo",       "Sexo")
t_edad    <- make_comp(arg, "Grupo_edad", "Grupo de edad")
t_nac     <- make_comp(arg, "Nac_label",  "Nacionalidad")
t_prov    <- make_comp(arg, "Prov_label", "Provincia destino")
t_proc    <- make_comp(arg, "Proc_label", "País procedencia")

t_tamunio <- arg %>%
  filter(Tamunio != "No data") %>%
  make_comp("Tamunio", "Municipality size")

t_trim    <- make_comp(arg, "Trimestre", "Trimestre")

t_edad_media <- arg %>%
  group_by(Anyo, Sexo) %>%
  summarise(`Edad media` = round(mean(EDAD, na.rm=TRUE),1),
            Mediana      = median(EDAD, na.rm=TRUE),
            SD           = round(sd(EDAD, na.rm=TRUE),1),
            N            = n(), .groups="drop")


# ── Exportar Excel ──────────────────────────────────────────
wb <- createWorkbook()

add_table <- function(wb, sheet, df, title) {
  addWorksheet(wb, sheet)
  writeData(wb, sheet, title, startRow=1)
  writeDataTable(wb, sheet, df, startRow=3, tableStyle="TableStyleMedium2")
  setColWidths(wb, sheet, cols=1:ncol(df), widths="auto")
}

add_table(wb, "0_Resumen",    t_resumen,    "Indicadores resumen – Nacidos en Argentina, inmigrantes en España")
add_table(wb, "1_Sexo",       t_sexo,       "Distribución por sexo")
add_table(wb, "2_Edad",       t_edad,       "Distribución por grupo de edad")
add_table(wb, "3_Nacion",     t_nac,        "Distribución por nacionalidad")
add_table(wb, "4_Provincia",  t_prov,       "Provincia de destino")
add_table(wb, "5_Procedencia",t_proc,       "País de procedencia")
add_table(wb, "6_TamMunicio", t_tamunio,    "Tamaño del municipio de destino")
add_table(wb, "7_Trimestre",  t_trim,       "Distribución por trimestre")
add_table(wb, "8_EdadMedia",  t_edad_media, "Edad media por sexo y año")

saveWorkbook(wb, paste0(out_path, "tablas_arg_esp_2023_2024.xlsx"), overwrite=TRUE)
cat("Tablas exportadas.\n")


# ============================================================
# GRÁFICOS
# ============================================================

theme_paper <- theme_minimal(base_size=12) +
  theme(
    plot.title    = element_text(face="bold", size=13),
    plot.subtitle = element_text(size=10, color="grey40"),
    axis.title    = element_text(size=10),
    legend.position = "bottom",
    panel.grid.minor = element_blank(),
    strip.text    = element_text(face="bold")
  )

# ── G1. Total anual (barras simples con variación) ──────────
total_anyo <- tibble(
  Anyo = c("2023","2024"), N = c(N_23, N_24),
  var_pct = c(NA, round((N_24-N_23)/N_23*100,1))
)
g1 <- ggplot(total_anyo, aes(x=Anyo, y=N, fill=Anyo)) +
  geom_col(width=0.5) +
  geom_text(aes(label=comma(N)), vjust=-0.5, fontface="bold", size=4.5) +
  annotate("text", x=1.5, y=max(total_anyo$N)*1.08,
           label=paste0("Change: ", total_anyo$var_pct[2], "%"),
           size=4, color="grey30") +
  geom_segment(aes(x=1, xend=2, y=N_23*1.04, yend=N_24*1.04),
               arrow=arrow(length=unit(0.2,"cm")), color="grey50") +
  scale_fill_manual(values=col_pair) +
  scale_y_continuous(labels=comma, expand=expansion(mult=c(0,0.15))) +
  labs(title="Total immigrants born in Argentina",
       subtitle="Spain · 2023 vs 2024", x=NULL, y="N", fill=NULL) +
  theme_paper + theme(legend.position="none")

ggsave(paste0(out_path,"G1_total_anual.png"), g1, width=5, height=4, dpi=300)

# ── G2. Pirámides de edad comparadas ───────────────────────
piramide <- arg %>%
  filter(!is.na(Grupo_edad)) %>%
  count(Anyo, Grupo_edad, Sexo) %>%
  group_by(Anyo) %>%
  mutate(Pct = n / sum(n) * 100,
         Pct_plot = ifelse(Sexo=="Male", -Pct, Pct))

g2 <- ggplot(piramide, aes(x=Grupo_edad, y=Pct_plot, fill=Sexo)) +
  geom_col(width=0.8) +
  coord_flip() +
  facet_wrap(~Anyo) +
  scale_y_continuous(labels=function(x) paste0(abs(x),"%"),
                     breaks=seq(-15,15,5)) +
  scale_fill_manual(values=col_sex) +
  labs(title="Age pyramid",
       subtitle="Born in Argentina · Immigration to Spain",
       x="Age group", y="% of yearly total", fill=NULL) +
  theme_paper

ggsave(paste0(out_path,"G2_piramide_comparada.png"), g2, width=9, height=5, dpi=300)

# ── G3. Grupos de edad: barras agrupadas ────────────────────
edad_plot <- arg %>%
  filter(!is.na(Grupo_edad)) %>%
  count(Anyo, Grupo_edad) %>%
  group_by(Anyo) %>%
  mutate(Pct = n / sum(n) * 100)

g3 <- ggplot(edad_plot, aes(x=Grupo_edad, y=Pct, fill=Anyo)) +
  geom_col(position="dodge", width=0.7) +
  geom_text(aes(label=paste0(round(Pct,1),"%")),
            position=position_dodge(width=0.7), vjust=-0.4, size=3) +
  scale_fill_manual(values=col_pair) +
  scale_y_continuous(expand=expansion(mult=c(0,0.12))) +
  labs(title="Age group distribution",
       subtitle="Born in Argentina · Immigration to Spain",
       x="Age group", y="%", fill="Year") +
  theme_paper

ggsave(paste0(out_path,"G3_grupos_edad_comparado.png"), g3, width=8, height=4.5, dpi=300)

# ── G4. Distribución por sexo ───────────────────────────────
sexo_plot <- arg %>%
  count(Anyo, Sexo) %>%
  group_by(Anyo) %>%
  mutate(Pct = n / sum(n) * 100)

g4 <- ggplot(sexo_plot, aes(x=Anyo, y=Pct, fill=Sexo)) +
  geom_col(width=0.5) +
  geom_text(aes(label=paste0(round(Pct,1),"%")),
            position=position_stack(vjust=0.5), color="white", fontface="bold", size=4) +
  scale_fill_manual(values=col_sex) +
  scale_y_continuous(labels=function(x) paste0(x,"%")) +
  labs(title="Sex distribution",
       subtitle="Born in Argentina · Immigration to Spain",
       x=NULL, y="%", fill=NULL) +
  theme_paper

ggsave(paste0(out_path,"G4_sexo_comparado.png"), g4, width=5, height=4, dpi=300)

# ── G5. Provincias de destino: lollipop comparativo ─────────
prov_comp <- arg %>%
  filter(Prov_label != "Other") %>%
  count(Anyo, Prov_label) %>%
  group_by(Anyo) %>%
  mutate(Pct = n / sum(n) * 100) %>%
  ungroup() %>%
  pivot_wider(names_from=Anyo, values_from=c(n,Pct),
              names_glue="{Anyo}_{.value}") %>%
  mutate(orden = (`2023_n` + `2024_n`) / 2) %>%
  arrange(orden) %>%
  mutate(Prov_label = fct_inorder(Prov_label))

g5 <- ggplot(prov_comp) +
  geom_segment(aes(x=`2023_Pct`, xend=`2024_Pct`,
                   y=Prov_label, yend=Prov_label),
               color="grey70", linewidth=1) +
  geom_point(aes(x=`2023_Pct`, y=Prov_label, color="2023"), size=3) +
  geom_point(aes(x=`2024_Pct`, y=Prov_label, color="2024"), size=3) +
  scale_color_manual(values=col_pair) +
  labs(title="Destination province",
       subtitle="Born in Argentina · Immigration to Spain",
       x="% of yearly total", y=NULL, color="Year") +
  theme_paper

ggsave(paste0(out_path,"G5_provincias_comparado.png"), g5, width=7, height=6, dpi=300)

# ── G6. Nacionalidad: barras agrupadas ──────────────────────
nac_plot <- arg %>%
  count(Anyo, Nac_label) %>%
  group_by(Anyo) %>%
  mutate(Pct = n / sum(n) * 100) %>%
  ungroup() %>%
  mutate(Nac_label = fct_reorder(Nac_label, n, sum))

g6 <- ggplot(nac_plot, aes(x=Nac_label, y=Pct, fill=Anyo)) +
  geom_col(position="dodge", width=0.7) +
  geom_text(aes(label=paste0(round(Pct,1),"%")),
            position=position_dodge(width=0.7), hjust=-0.1, size=3) +
  coord_flip() +
  scale_fill_manual(values=col_pair) +
  scale_y_continuous(expand=expansion(mult=c(0,0.18))) +
  labs(title="Nationality distribution",
       subtitle="Born in Argentina · Immigration to Spain",
       x=NULL, y="%", fill="Year") +
  theme_paper

ggsave(paste0(out_path,"G6_nacionalidad_comparado.png"), g6, width=7, height=5, dpi=300)

# ── G7. País de procedencia ─────────────────────────────────
proc_plot <- arg %>%
  filter(Proc_label != "Other") %>%
  count(Anyo, Proc_label) %>%
  group_by(Anyo) %>%
  mutate(Pct = n / sum(n) * 100) %>%
  ungroup() %>%
  mutate(Proc_label = fct_reorder(Proc_label, n, sum))

g7 <- ggplot(proc_plot, aes(x=Proc_label, y=Pct, fill=Anyo)) +
  geom_col(position="dodge", width=0.7) +
  geom_text(aes(label=paste0(round(Pct,1),"%")),
            position=position_dodge(width=0.7), hjust=-0.1, size=3) +
  coord_flip() +
  scale_fill_manual(values=col_pair) +
  scale_y_continuous(expand=expansion(mult=c(0,0.18))) +
  labs(title="Country of origin",
       subtitle="Born in Argentina · Immigration to Spain",
       x=NULL, y="%", fill="Year") +
  theme_paper

ggsave(paste0(out_path,"G7_procedencia_comparado.png"), g7, width=7, height=5, dpi=300)

# ── G8. Tamaño de municipio ─────────────────────────────────
tam_plot <- arg %>%
  filter(Tamunio != "No data") %>%
  count(Anyo, Tamunio) %>%
  group_by(Anyo) %>%
  mutate(Pct = n / sum(n) * 100,
         Tamunio = factor(Tamunio, levels=orden_tam))

g8 <- ggplot(tam_plot, aes(x=Tamunio, y=Pct, fill=Anyo)) +
  geom_col(position="dodge", width=0.7) +
  geom_text(aes(label=paste0(round(Pct,1),"%")),
            position=position_dodge(width=0.7), hjust=-0.1, size=3) +
  coord_flip() +
  scale_fill_manual(values=col_pair) +
  scale_x_discrete(limits=rev(orden_tam)) +
  scale_y_continuous(expand=expansion(mult=c(0,0.18))) +
  labs(title="Destination municipality size",
       subtitle="Born in Argentina · Immigration to Spain",
       x=NULL, y="%", fill="Year") +
  theme_paper

ggsave(paste0(out_path,"G8_tamunio_comparado.png"), g8, width=8, height=4.5, dpi=300)

# ── G9. Llegadas por trimestre ──────────────────────────────
trim_plot <- arg %>%
  count(Anyo, Trimestre) %>%
  group_by(Anyo) %>%
  mutate(Pct = n / sum(n) * 100)

g9 <- ggplot(trim_plot, aes(x=Trimestre, y=n, color=Anyo, group=Anyo)) +
  geom_line(linewidth=1.2) +
  geom_point(size=3.5, fill="white", shape=21, stroke=1.5) +
  geom_text(aes(label=comma(n)), vjust=-1, size=3.2) +
  scale_color_manual(values=col_pair) +
  scale_y_continuous(labels=comma,
                     limits=c(min(trim_plot$n)*0.82, max(trim_plot$n)*1.12)) +
  labs(title="Arrivals by quarter",
       subtitle="Born in Argentina · Immigration to Spain",
       x="Quarter", y="N", color="Year") +
  theme_paper

ggsave(paste0(out_path,"G9_trimestre_comparado.png"), g9, width=6, height=4, dpi=300)

# ── G10. Variación absoluta por provincia ───────────────────
var_prov <- arg %>%
  filter(Prov_label != "Other") %>%
  count(Anyo, Prov_label) %>%
  pivot_wider(names_from=Anyo, values_from=n, values_fill=0) %>%
  mutate(var_abs = `2024` - `2023`,
         dir     = ifelse(var_abs >= 0, "Increase", "Decrease"),
         Prov_label = fct_reorder(Prov_label, var_abs))

g10 <- ggplot(var_prov, aes(x=Prov_label, y=var_abs, fill=dir)) +
  geom_col(width=0.7) +
  geom_text(aes(label=ifelse(var_abs>=0, paste0("+",var_abs), var_abs)),
            hjust=ifelse(var_prov$var_abs>=0, -0.15, 1.1), size=3.2) +
  coord_flip() +
  geom_hline(yintercept=0, linewidth=0.5) +
  scale_fill_manual(values=c("Increase"=col_23, "Decrease"=col_24)) +
  scale_y_continuous(expand=expansion(mult=c(0.2,0.2))) +
  labs(title="Change 2023→2024 by destination province",
       subtitle="Born in Argentina · Immigration to Spain (absolute N)",
       x=NULL, y="Absolute change (N)", fill=NULL) +
  theme_paper

ggsave(paste0(out_path,"G10_variacion_provincia.png"), g10, width=7, height=6, dpi=300)

# ── G11. Variación en estructura de edad por sexo ───────────
edad_sexo <- arg %>%
  filter(!is.na(Grupo_edad)) %>%
  count(Anyo, Sexo, Grupo_edad) %>%
  group_by(Anyo, Sexo) %>%
  mutate(Pct = n / sum(n) * 100) %>%
  pivot_wider(names_from=Anyo, values_from=c(n,Pct),
              names_glue="{Anyo}_{.value}") %>%
  mutate(delta = `2024_Pct` - `2023_Pct`)

g11 <- ggplot(edad_sexo, aes(x=Grupo_edad, y=delta, fill=Sexo)) +
  geom_col(position="dodge", width=0.7) +
  geom_hline(yintercept=0, linewidth=0.4) +
  facet_wrap(~Sexo) +
  scale_fill_manual(values=col_sex) +
  labs(title="Change in age structure 2023→2024 (percentage points)",
       subtitle="Born in Argentina · Immigration to Spain",
       x="Age group", y="Δ pp", fill=NULL) +
  theme_paper + theme(legend.position="none")

ggsave(paste0(out_path,"G11_cambio_edad_sexo.png"), g11, width=8, height=4.5, dpi=300)

cat("\n============================================\n")
cat("Archivos generados en:\n", out_path, "\n")
cat("============================================\n")
