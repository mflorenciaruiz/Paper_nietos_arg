library(foreign)
library(dplyr)
library(tidyr)
library(readr)
library(haven)
library(ggplot2)
library(psych)
library(stringr)
library(writexl)
library(fixest)
library(zoo)
library(modelsummary)
library(car)
library(sandwich)
library(openxlsx)
library(broom)
library(showtext)

# ------------------------------------------ #
# 0. Data
# ------------------------------------------ #

# Definir el path a la carpeta del proyecto: ARGENTINA
path_flor <- "/Users/florenciaruiz/BID 2/Paper Valerie/Nietos/Argentina/Paper_nietos_arg"
setwd(path_flor)

dip_nac_mun <- read_csv("Data Out/dip_nac_mun.csv")
dip_nac_sec <- read_csv("Data Out/dip_nac_sec.csv")

dip_nac_mun_gen <- read_csv("Data Out/dip_nac_mun_gen.csv")
dip_nac_sec_gen <- read_csv("Data Out/dip_nac_sec_gen.csv")

dip_nac_mun_paso <- read_csv("Data Out/dip_nac_mun_paso.csv")
dip_nac_sec_paso <- read_csv("Data Out/dip_nac_sec_paso.csv")

data_eb <- read_dta("Data Int/data_EB.dta")
# Uno la data final para estimar con los pesos de EB
dip_nac_mun_eb <- dip_nac_mun %>% 
  left_join(data_eb, by = "mun_code")

pesos_cbgps <- read_dta("Data Int/pesos_cbgps.dta") 
# Uno la data final para estimar con los pesos de EB y CBGPS
dip_nac_mun_pesos <- dip_nac_mun_eb %>% 
  left_join(pesos_cbgps, by = "mun_code")

# Censo de 2010
censo_2010 <- read_dta("Data Int/censo_2010_arg_mun.dta")

spanish_cohorts_arg <- read_csv("Data Int/spanish_cohorts_arg.csv")

# Settings para exportar
options("modelsummary_format_numeric_latex" = "plain") # Números sin formato en tablas de modelsummary.

#font_add(family = "Latin Modern Math", regular = "latinmodern-math.otf")
#showtext_auto()

font_add( # Agrego times new roman
  family = "Times New Roman",
  regular = "/System/Library/Fonts/Supplemental/Times New Roman.ttf",
  bold = "/System/Library/Fonts/Supplemental/Times New Roman Bold.ttf",
  italic = "/System/Library/Fonts/Supplemental/Times New Roman Italic.ttf",
  bolditalic = "/System/Library/Fonts/Supplemental/Times New Roman Bold Italic.ttf"
)
showtext_auto()
showtext_opts(dpi = 300)   # para que el tamaño del texto se vea bien al exportar a 300 dpi

# ------------------------------------------ #
#  1. Estimaciones (nivel municipal)
# ------------------------------------------ #

### 1.1. Participación ### 
{
# Paso y generales juntas
feols(participacion ~ share_1936_1955:post  + share_1956_1978:post | 
        mun_code + anio, data = dip_nac_mun)
event_p1 <- feols(participacion ~ 
                     i(anio, share_1936_1955, ref = 2021) + i(anio, share_1956_1978, ref = 2021) |
                     mun_code + anio, data = dip_nac_mun
                   )
iplot(event_p1) 

event_p1_36 <- feols(participacion ~ i(anio, share_1936_1955, ref = 2021) |
                     mun_code + anio, data = dip_nac_mun
                   )
iplot(event_p1_36)

event1_p1_56 <- feols(participacion ~ i(anio, share_1956_1978, ref = 2021) | 
                      mun_code + anio, data = dip_nac_mun
                    )
iplot(event1_p1_56)

# Controlando por tipo de elección

att_p2 <- feols(participacion ~ share_1936_1955:post  + share_1956_1978:post | 
                  mun_code + anio + tipo_eleccion, data = dip_nac_mun)

event_p2_36 <- feols(participacion ~ i(anio, share_1936_1955, ref = 2021) |
                       mun_code + anio + tipo_eleccion, data = dip_nac_mun
)
summary(event_p2_36)
iplot(event_p2_36)

png("Output/Argentina/event_p2_36.png", width = 6.5, height = 4.5, units = "in", res = 300)
op <- par(no.readonly = TRUE)   # guardar par() actual
on.exit(par(op))                # restaurar al final

par(cex.lab = 1.1, 
    cex.axis = 1.1, 
    #family = "Times New Roman", 
    cex.main = 0.9,
    cex.lab = 0.9,
    bty = "l",
    #mar = c(4.2, 7, 0.8, 0.8),    # ↓ margen sup. (tercer número)
    yaxs = "i",                   # sin padding extra en ejes
    mgp  = c(2.0, 0.6, 0))        # acercar etiquetas/axis a la trama
iplot(event_p2_36, main ="Effect on participation - Post × Spanish share (1936–1955) - Election type FE" ,  
      xlab    = "Year", 
      ylim = c(-1.5, 2.5), 
      xlim = c(2011, 2025),
      #ci.lty = 2,                 # líneas de IC punteadas
      ci.col = "grey50",
      grid    = FALSE)
dev.off()

event_p2_56 <- feols(participacion ~ i(anio, share_1956_1978, ref = 2021) | 
                        mun_code + anio + tipo_eleccion, data = dip_nac_mun
)
iplot(event_p2_56)

png("Output/Argentina/event_p2_56.png", width = 6.5, height = 4.5, units = "in", res = 300)
op <- par(no.readonly = TRUE)   # guardar par() actual
on.exit(par(op))                # restaurar al final

par(cex.lab = 1.1, 
    cex.axis = 1.1, 
    #family = "Times New Roman", 
    cex.main = 0.9,
    cex.lab = 0.9,
    bty = "l",
    #mar = c(4.2, 7, 0.8, 0.8),    # ↓ margen sup. (tercer número)
    yaxs = "i",                   # sin padding extra en ejes
    mgp  = c(2.0, 0.6, 0))        # acercar etiquetas/axis a la trama
iplot(event_p2_56, main ="Effect on participation - Post × Spanish share (1956–1978) - Election type FE" ,  
      xlab    = "Year", 
      ylim = c(-10, 10), 
      xlim = c(2011, 2025),
      #ci.lty = 2,                 # líneas de IC punteadas
      ci.col = "grey50",
      grid    = FALSE)
dev.off()

# Solo generales

att_p3 <- feols(participacion ~ share_1936_1955:post  + share_1956_1978:post | 
                  mun_code + anio, data = dip_nac_mun_gen)

event_p3_36 <- feols(participacion ~ i(anio, share_1936_1955, ref = 2021)   | 
                  mun_code + anio, data = dip_nac_mun_gen)
iplot(event_p3_36)

png("Output/Argentina/event_p3_36.png", width = 6.5, height = 4.5, units = "in", res = 300)
op <- par(no.readonly = TRUE)   # guardar par() actual
on.exit(par(op))                # restaurar al final

par(cex.lab = 1.1, 
    cex.axis = 1.1, 
    #family = "Times New Roman", 
    cex.main = 0.9,
    cex.lab = 0.9,
    bty = "l",
    #mar = c(4.2, 7, 0.8, 0.8),    # ↓ margen sup. (tercer número)
    yaxs = "i",                   # sin padding extra en ejes
    mgp  = c(2.0, 0.6, 0))        # acercar etiquetas/axis a la trama
iplot(event_p3_36, main ="Effect on participation - Post × Spanish share (1936–1955) - General elections" ,  
      xlab    = "Year", 
      ylim = c(-2.5, 4), 
      xlim = c(2011, 2025),
      #ci.lty = 2,                 # líneas de IC punteadas
      ci.col = "grey50",
      grid    = FALSE)
dev.off()

event_p3_56 <- feols(participacion ~ i(anio, share_1956_1978, ref = 2021)   | 
                       mun_code + anio, data = dip_nac_mun_gen)
iplot(event_p3_56)
png("Output/Argentina/event_p3_56.png", width = 6.5, height = 4.5, units = "in", res = 300)
op <- par(no.readonly = TRUE)   # guardar par() actual
on.exit(par(op))                # restaurar al final

par(cex.lab = 1.1, 
    cex.axis = 1.1, 
    #family = "Times New Roman", 
    cex.main = 0.9,
    cex.lab = 0.9,
    bty = "l",
    #mar = c(4.2, 7, 0.8, 0.8),    # ↓ margen sup. (tercer número)
    yaxs = "i",                   # sin padding extra en ejes
    mgp  = c(2.0, 0.6, 0))        # acercar etiquetas/axis a la trama
iplot(event_p3_56, main ="Effect on participation - Post × Spanish share (1956–1978) - General elections" ,  
      xlab    = "Year", 
      ylim = c(-10, 15.5), 
      xlim = c(2011, 2025),
      #ci.lty = 2,                 # líneas de IC punteadas
      ci.col = "grey50",
      grid    = FALSE)
dev.off()

# Controlando por tipo de elección y el lag del outcome
att_p2_l <- feols(participacion ~ share_1936_1955:post  + share_1956_1978:post + participacion_l | 
                  mun_code + anio + tipo_eleccion, data = dip_nac_mun)

## Exporto a word

modelsummary(
  list(att_p2, att_p3),
  output = "Output/Argentina/att_participation.docx",
  stars = c('*' = .1, '**' = .05, '***' = .01),
  gof_map = data.frame(
    raw = c("nobs"),
    clean = c("Observations"),
    fmt = c(3, 0)
  ),
  coef_map = c(
    "share_1936_1955:post" = "Post × Spanish share (1936-1955)",
    "post:share_1956_1978" = "Post × Spanish share (1956-1978)"
  ),
  add_rows = tibble::tibble(
    term = c("Municipality FE", "Time FE", "Election type FE", "General elections only"),
    att_b2 = c("Yes", "Yes", "Yes", "No"),
    att_b4 = c("Yes", "Yes", "No", "Yes")
  ),
  notes = "Standard errors clustered at the municipality level in parentheses."
)

## Exporto event study en un mismo gráfico

# 1) Extraer coeficientes de cada event study
extract_es <- function(model, window_label) {
  broom::tidy(model, conf.int = TRUE) %>%
    filter(grepl("anio::", term)) %>%
    mutate(
      anio   = as.numeric(sub("anio::([0-9]+).*", "\\1", term)),
      window = window_label
    ) %>%
    select(anio, window, estimate, conf.low, conf.high)
}

coefs <- bind_rows(
  extract_es(event_p2_36, "1936–1955"),
  extract_es(event_p2_56, "1956–1978"),
  # Año de referencia (2021): coeficiente = 0, sin IC
  tibble::tibble(
    anio = 2021,
    window = c("1936–1955", "1956–1978"),
    estimate = 0,
    conf.low = NA_real_,
    conf.high = NA_real_
  )) %>% 
  mutate(window = case_when(
    window == "1936–1955" ~ "Spanish share 1936–1955",
    window == "1956–1978" ~ "Spanish share 1956–1978",
    TRUE ~ window
  ))

# 2) Plot
(p <- ggplot(coefs %>% filter(anio != 2021), aes(x = anio, y = estimate, shape = window, group = window)) +
    geom_hline(yintercept = 0, linetype = "dotted", color = "black") +
    geom_vline(xintercept = 2021, linetype = "solid", linewidth = 0.3, color = "black") +
    geom_errorbar(
      aes(ymin = conf.low, ymax = conf.high),
      width = 0.3,
      position = position_dodge(width = 0.5),
      linewidth = 0.35,
      linetype = "solid",
      color = "grey60",
      show.legend = FALSE
    ) +
    geom_point(size = 2, position = position_dodge(width = 0.5), color = "black") +
    scale_shape_manual(values = c("Spanish share 1936–1955" = 16, "Spanish share 1956–1978" = 17), 
                       name = NULL) +
    scale_x_continuous(breaks = seq(2011, 2025, by = 2)) +
    labs(
      x = "Year",
      y = "Estimate and 95% CI",
      #title = "Effect turnout"
    ) +
    guides(shape = guide_legend(nrow = 1, byrow = TRUE)) +
    theme_minimal(base_family = "Times New Roman", base_size = 12) +
    theme(
      legend.position = "bottom",
      legend.justification = "center",
      legend.box = "horizontal",
      legend.margin = margin(t = -5, r = 0, b = 0, l = 0),
      plot.margin = margin(t = 10, r = 10, b = 10, l = 10),
      
      plot.title = element_text(hjust = 0.5, size = 12),
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_blank(),
      panel.grid.major.y = element_line(color = "grey85", linewidth = 0.3),
      axis.line = element_line(color = "black", linewidth = 0.4),
      axis.text = element_text(color = "black", size = 11),
      axis.title = element_text(color = "black", size = 11),
      legend.text = element_text(size = 11)
    ))

# 3) Guardar
ggsave("Output/event_p2_combined.pdf", p,
       width = 6.5, height = 4.5, dpi = 300)

}

### 1.2. Voto Blanco ###
{
# Paso y generales juntas
feols(porcentaje_blanco ~ share_1936_1955:post  + share_1956_1978:post | 
        mun_code + anio, data = dip_nac_mun)

event_b1_36 <- feols(porcentaje_blanco ~ i(anio, share_1936_1955, ref = 2021) |
                     mun_code + anio, data = dip_nac_mun
)
summary(event_b1_36)
iplot(event_b1_36)

event_b1_56 <- feols(porcentaje_blanco ~ i(anio, share_1956_1978, ref = 2021) | 
                     mun_code + anio, data = dip_nac_mun
)
iplot(event_b1_56)

# Controlando por tipo de elección (mejor)
att_b2 <- feols(porcentaje_blanco ~ share_1936_1955:post  + share_1956_1978:post | 
                  mun_code + anio + tipo_eleccion, data = dip_nac_mun)

event_b2_36 <- feols(porcentaje_blanco ~ i(anio, share_1936_1955, ref = 2021) |
                       mun_code + anio + tipo_eleccion, data = dip_nac_mun
)
summary(event_b2_36)
iplot(event_b2_36)
summary(att_b2)

png("Output/Argentina/event_b2_36.png", width = 6.5, height = 4.5, units = "in", res = 300)
op <- par(no.readonly = TRUE)   # guardar par() actual
on.exit(par(op))                # restaurar al final

par(cex.lab = 1.1, 
    cex.axis = 1.1, 
    #family = "Times New Roman", 
    cex.main = 0.9,
    cex.lab = 0.9,
    bty = "l",
    #mar = c(4.2, 7, 0.8, 0.8),    # ↓ margen sup. (tercer número)
    yaxs = "i",                   # sin padding extra en ejes
    mgp  = c(2.0, 0.6, 0))        # acercar etiquetas/axis a la trama
iplot(event_b2_36, main ="Effect on share of blank votes - Post × Spanish share (1936–1955) - Election type FE" ,  
      xlab    = "Year", 
      ylim = c(-6.5, 2.5), 
      xlim = c(2011, 2025),
      #ci.lty = 2,                 # líneas de IC punteadas
      ci.col = "grey50",
      grid    = FALSE)
dev.off()

event_b2_56 <- feols(porcentaje_blanco ~ i(anio, share_1956_1978, ref = 2021) | 
                        mun_code + anio + tipo_eleccion, data = dip_nac_mun
)
summary(event_b2_56)
iplot(event_b2_56)

png("Output/Argentina/event_b2_56.png", width = 6.5, height = 4.5, units = "in", res = 300)
op <- par(no.readonly = TRUE)   # guardar par() actual
on.exit(par(op))                # restaurar al final

par(cex.lab = 1.1, 
    cex.axis = 1.1, 
    #family = "Times New Roman", 
    cex.main = 0.9,
    cex.lab = 0.9,
    bty = "l",
    #mar = c(4.2, 7, 0.8, 0.8),    # ↓ margen sup. (tercer número)
    yaxs = "i",                   # sin padding extra en ejes
    mgp  = c(2.0, 0.6, 0))        # acercar etiquetas/axis a la trama
iplot(event_b2_56, main ="Effect on share of blank votes - Post × Spanish share (1956–1978) - Election type FE" ,  
      xlab    = "Year", 
      ylim = c(-20.5, 10.5), 
      xlim = c(2011, 2025),
      #ci.lty = 2,                 # líneas de IC punteadas
      ci.col = "grey50",
      grid    = FALSE)
dev.off()

# Solo PASO
feols(porcentaje_blanco ~ share_1936_1955:post  + share_1956_1978:post | 
        mun_code + anio, data = dip_nac_mun_paso)

event_b3_36 <- feols(porcentaje_blanco ~ i(anio, share_1936_1955, ref = 2021) |
                       mun_code + anio, data = dip_nac_mun_paso
)
summary(event_b3_36)
iplot(event_b3_36)

event_b3_56 <- feols(porcentaje_blanco ~ i(anio, share_1956_1978, ref = 2021) | 
                       mun_code + anio, data = dip_nac_mun_paso
)
summary(event_b3_56)
iplot(event_b3_56)

# Solo generales (mejor)
att_b4 <- feols(porcentaje_blanco ~ share_1936_1955:post  + share_1956_1978:post | 
                 mun_code + anio, data = dip_nac_mun_gen)

event_b4_36 <- feols(porcentaje_blanco ~ i(anio, share_1936_1955, ref = 2021) |
                       mun_code + anio, data = dip_nac_mun_gen)
summary(event_b4_36)
iplot(event_b4_36)

png("Output/Argentina/event_b4_36.png", width = 6.5, height = 4.5, units = "in", res = 300)
op <- par(no.readonly = TRUE)   # guardar par() actual
on.exit(par(op))                # restaurar al final

par(cex.lab = 1.1, 
    cex.axis = 1.1, 
    #family = "Times New Roman", 
    cex.main = 0.9,
    cex.lab = 0.9,
    bty = "l",
    #mar = c(4.2, 7, 0.8, 0.8),    # ↓ margen sup. (tercer número)
    yaxs = "i",                   # sin padding extra en ejes
    mgp  = c(2.0, 0.6, 0))        # acercar etiquetas/axis a la trama
iplot(event_b4_36, main ="Effect on share of blank votes - Post × Spanish share (1936–1955) - General elections" ,  
      xlab    = "Year", 
      ylim = c(-6.5, 2.5), 
      xlim = c(2011, 2025),
      #ci.lty = 2,                 # líneas de IC punteadas
      ci.col = "grey50",
      grid    = FALSE)
dev.off()

event_b4_56 <- feols(porcentaje_blanco ~ i(anio, share_1956_1978, ref = 2021) | 
                       mun_code + anio, data = dip_nac_mun_gen)
summary(event_b4_56)
iplot(event_b4_56)

png("Output/Argentina/event_b4_56.png", width = 6.5, height = 4.5, units = "in", res = 300)
op <- par(no.readonly = TRUE)   # guardar par() actual
on.exit(par(op))                # restaurar al final

par(cex.lab = 1.1, 
    cex.axis = 1.1, 
    #family = "Times New Roman", 
    cex.main = 0.9,
    cex.lab = 0.9,
    bty = "l",
    #mar = c(4.2, 7, 0.8, 0.8),    # ↓ margen sup. (tercer número)
    yaxs = "i",                   # sin padding extra en ejes
    mgp  = c(2.0, 0.6, 0))        # acercar etiquetas/axis a la trama
iplot(event_b4_56, main ="Effect on share of blank votes - Post × Spanish share (1956–1978) - General elections" ,  
      xlab    = "Year", 
      ylim = c(-20, 10.5), 
      xlim = c(2011, 2025),
      #ci.lty = 2,                 # líneas de IC punteadas
      ci.col = "grey50",
      grid    = FALSE)
dev.off()

# Solo generales con pesos
feols(porcentaje_blanco ~ share_1936_1955:post  + share_1956_1978:post | 
        mun_code + anio, data = dip_nac_mun_gen, weights = ~votantes)

event_b5_36 <- feols(porcentaje_blanco ~ i(anio, share_1936_1955, ref = 2021) |
                       mun_code + anio, data = dip_nac_mun_gen, weights = ~votantes)
summary(event_b5_36)
iplot(event_b5_36)

# Controlando por tipo de elección y el lag del outcome
att_b2_l <- feols(porcentaje_blanco ~ share_1936_1955:post  + share_1956_1978:post + porcentaje_blanco_l | 
                  mun_code + anio + tipo_eleccion, data = dip_nac_mun)

# chequeo de IC grandes 
dip_nac_mun %>%
  group_by(anio) %>%
  summarise(
    n_mun = n_distinct(mun_code),
    n_obs = n()
  )
boxplot(porcentaje_blanco ~ anio, data = dip_nac_mun) # 15 19 y 23 son los años con mayor varianza en el voto en blanco, muchos outliers

ggplot(dip_nac_mun, aes(x = factor(anio), y = porcentaje_blanco)) +
  geom_boxplot(fill = "grey85", color = "grey30") +
  labs(
    x = "Year",
    y = "Share of blank votes (%)",
    title = "Distribution of blank votes across municipalities"
  ) +
  theme_classic(base_size = 12)+
  theme(
    axis.text.x = element_text(color="black", size=11),
    plot.title = element_text(hjust = 0.5, color= "black", size=13),
    axis.text.y = element_text(color="black", size=11),
    axis.line = element_line(linewidth = 0.5) 
  )

ggsave("Output/Argentina/boxplot_blank_votes.png", width = 6.5, height = 4.5, dpi = 300, bg = "white")

dip_nac_mun %>%
  group_by(anio) %>%
  summarise(
    n_mun = n_distinct(mun_code),
    sd_blanco = sd(porcentaje_blanco, na.rm = TRUE),
    sd_share = sd(share_1956_1978, na.rm = TRUE),
    mean_blanco = mean(porcentaje_blanco, na.rm = TRUE)
  )

Hmisc::describe(dip_nac_mun[dip_nac_mun$anio==2023,]$porcentaje_blanco)
dip_nac_mun %>%
  filter(anio == 2023, porcentaje_blanco > 45) %>%
  select(mun_name, prov_name, porcentaje_blanco) %>%
  arrange(desc(porcentaje_blanco)) %>% View() # La mayoria son de misiones, no es un error de la data

# Exporto los resultados que me gustaron de votos en blanco
modelsummary(
  list(att_b2, att_b4),
  output = "Output/Argentina/att.docx",
  stars = c('*' = .1, '**' = .05, '***' = .01),
  gof_map = data.frame(
    raw = c("r.squared", "nobs"),
    clean = c("R²", "Observations"),
    fmt = c(3, 0)
  ),
  coef_map = c(
    "share_1936_1955:post" = "Post × Spanish share (1936-1955)",
    "post:share_1956_1978" = "Post × Spanish share (1956-1978)"
  ),
  add_rows = tibble::tibble(
    term = c("Municipality FE", "Time FE", "Election type FE", "General elections only"),
    att_b2 = c("Yes", "Yes", "Yes", "No"),
    att_b4 = c("Yes", "Yes", "No", "Yes")
  ),
  notes = "Standard errors clustered at the municipality level in parentheses."
)

## Testo diferencia de coeficientes entre los dos modelos de voto en blanco exportados
  
# 1. Modelo att b2

  # Test bilateral
vcov_cluster_att_b2 <- vcovCL(att_b2, cluster = dip_nac_mun$mun_code)
linearHypothesis(
  att_b2,
  "share_1936_1955:post = post:share_1956_1978",
  vcov = vcov_cluster_att_b2
)

  # Test unilateral: HA: |beta_2| > |beta_1| --> beta_2 < beta_1 --> beta_2 - beta_1 < 0
b <- coef(att_b2)
V <- vcov(att_b2, cluster = ~ mun_code) 
diff <- b["post:share_1956_1978"] - b["share_1936_1955:post"] 

se_diff <- sqrt(
  V["share_1936_1955:post", "share_1936_1955:post"] +
    V["post:share_1956_1978", "post:share_1956_1978"] -
    2 * V["share_1936_1955:post", "post:share_1956_1978"]
)
t_stat <- diff / se_diff

  # Test bilateral: son distintos?
(p_bilateral <- 2 * (1 - pnorm(abs(t_stat))))
  # Test unilateral: beta2 < beta1?
(p_unilateral <- pnorm(t_stat))

# 1. Modelo att b4

  # Test bilateral
vcov_cluster_att_b4 <- vcovCL(att_b4, cluster = dip_nac_mun_gen$mun_code)
linearHypothesis(
  att_b4,
  "share_1936_1955:post = post:share_1956_1978",
  vcov = vcov_cluster_att_b4
)

  # Test unilateral: HA: |beta_2| > |beta_1| --> beta_2 < beta_1 --> beta_2 - beta_1 < 0
b <- coef(att_b4)
V <- vcov(att_b4, cluster = ~ mun_code)
diff <- b["post:share_1956_1978"] - b["share_1936_1955:post"]
se_diff <- sqrt(
  V["share_1936_1955:post", "share_1936_1955:post"] +
    V["post:share_1956_1978", "post:share_1956_1978"] -
    2 * V["share_1936_1955:post", "post:share_1956_1978"]
)
t_stat <- diff / se_diff

  # Test bilateral: son distintos?
(p_bilateral <- 2 * (1 - pnorm(abs(t_stat))))
  # Test unilateral: beta2 < beta1?
(p_unilateral <- pnorm(t_stat))


## Exporto event study en un mismo gráfico

# 1) Extraer coeficientes de cada event study
coefs <- bind_rows(
  extract_es(event_b2_36, "1936–1955"),
  extract_es(event_b2_56, "1956–1978"),
  # Año de referencia (2021): coeficiente = 0, sin IC
  tibble::tibble(
    anio = 2021,
    window = c("1936–1955", "1956–1978"),
    estimate = 0,
    conf.low = NA_real_,
    conf.high = NA_real_
  )) %>% 
  mutate(window = case_when(
    window == "1936–1955" ~ "Spanish share 1936–1955",
    window == "1956–1978" ~ "Spanish share 1956–1978",
    TRUE ~ window
  ))

# 2) Plot
(p <- ggplot(coefs %>% filter(anio != 2021), aes(x = anio, y = estimate, shape = window, group = window)) +
    geom_hline(yintercept = 0, linetype = "dotted", color = "black") +
    geom_vline(xintercept = 2021, linetype = "solid", linewidth = 0.3, color = "black") +
    geom_errorbar(
      aes(ymin = conf.low, ymax = conf.high),
      width = 0.3,
      position = position_dodge(width = 0.5),
      linewidth = 0.35,
      linetype = "solid",
      color = "grey60",
      show.legend = FALSE
    ) +
    geom_point(size = 2, position = position_dodge(width = 0.5), color = "black") +
    scale_shape_manual(values = c("Spanish share 1936–1955" = 16, "Spanish share 1956–1978" = 17), 
                       name = NULL) +
    scale_x_continuous(breaks = seq(2011, 2025, by = 2)) +
    labs(
      x = "Year",
      y = "Estimate and 95% CI",
      #title = "Effect on share of blank votes"
    ) +
    guides(shape = guide_legend(nrow = 1, byrow = TRUE)) +
    theme_minimal(base_family = "Times New Roman", base_size = 12) +
    theme(
      legend.position = "bottom",
      legend.justification = "center",
      legend.box = "horizontal",
      legend.margin = margin(t = -5, r = 0, b = 0, l = 0),
      plot.margin = margin(t = 10, r = 10, b = 10, l = 10),
      
      plot.title = element_text(hjust = 0.5, size = 12),
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_blank(),
      panel.grid.major.y = element_line(color = "grey85", linewidth = 0.3),
      axis.line = element_line(color = "black", linewidth = 0.4),
      axis.text = element_text(color = "black", size = 11),
      axis.title = element_text(color = "black", size = 11),
      legend.text = element_text(size = 11)
    ))

# 3) Guardar
ggsave("Output/event_b2_combined.pdf", p,
       width = 6.5, height = 4.5, dpi = 300)

## Exporto a tex (votos en blanco y participación)

cm <- c("share_1936_1955:post" = "Spanish share 1936-1955$\\times$Post",
        "post:share_1956_1978" = "Spanish share 1956-1978$\\times$Post",
        "porcentaje_blanco_l"  = "Lagged share of blank votes",
        "participacion_l"      = "Lagged voter turnout"
        )

gm <- tibble::tribble(
  ~raw,        ~clean,         ~fmt,
  "nobs",      "Observations", 0,
  "r.squared", "R$^2$",        3)

models_list <- list("(1)" = att_b2, "(2)" = att_b4, "(3)" = att_b2_l, 
                    "(4)" = att_p2, "(5)" = att_p3, "(6)" = att_p2_l)

# Función para calcular el p-valor del test β_1936-1955 = β_1956-1978
get_p_equal <- function(m) {
  betas <- coef(m)
  V     <- vcov(m)
  
  # Buscar los nombres dinámicamente (por si R los escribe en orden distinto)
  name1 <- grep("1936_1955", names(betas), value = TRUE)
  name2 <- grep("1956_1978", names(betas), value = TRUE)
  if (length(name1) != 1 || length(name2) != 1) return(NA)
  
  diff    <- betas[name1] - betas[name2]
  se_diff <- sqrt(V[name1, name1] + V[name2, name2] - 2 * V[name1, name2])
  t_stat  <- diff / se_diff
  p_val   <- 2 * pnorm(-abs(t_stat))
  
  as.numeric(p_val)
}

# Aplicar a los 6 modelos
p_values  <- sapply(models_list, get_p_equal)
p_strings <- sprintf("%.3f", p_values)

add_rows <- tibble::tibble(
  term = c("$p$-value ($\\beta_{36{-}55} = \\beta_{56{-}78}$)",
           "Municipality FE", "Time FE", "Election type FE", "General elections only"),
  m1 = c(p_strings[1], "Yes", "Yes", "Yes", "No"),
  m2 = c(p_strings[2], "Yes", "Yes", "No", "Yes"),
  m3 = c(p_strings[3], "Yes", "Yes", "Yes", "No"),
  m4 = c(p_strings[4], "Yes", "Yes", "Yes", "No"),
  m5 = c(p_strings[5], "Yes", "Yes", "No", "Yes"),
  m6 = c(p_strings[6], "Yes", "Yes", "Yes", "No")
)
names(add_rows) <- c("term", "(1)", "(2)", "(3)", "(4)", "(5)", "(6)")

tex <- modelsummary(
  models_list,
  output    = "latex",
  coef_map  = cm,
  gof_map   = gm,
  estimate  = "{estimate}{stars}",
  statistic = "({std.error})",
  stars     = c("*" = .10, "**" = .05, "***" = .01),
  add_rows  = add_rows,
  escape    = FALSE)

lines <- strsplit(tex, "\n")[[1]]
ncols <- 7   # 1 label + 6 modelos

# 1) Caption, más espacio vertical y horizontal en la tabla
beg_table <- grep("\\\\begin\\{table\\}", lines)
if (length(beg_table) >= 1) {
  header <- c(
    "\\caption{Effects on Blank Votes and Voter Turnout}",
    "\\renewcommand{\\arraystretch}{1.25}",
    "\\setlength{\\tabcolsep}{6pt}"
  )
  lines <- c(lines[1:beg_table[1]], header, lines[(beg_table[1] + 1):length(lines)])
}

# 2) Header agrupado: "Share of blank votes" (cols 2-4) + "Voter turnout" (cols 5-7)
#    AMBOS en una sola fila
top_idx <- grep("\\\\toprule", lines)
if (length(top_idx) >= 1) {
  multicol <- " & \\multicolumn{3}{c}{Share of blank votes} & \\multicolumn{3}{c}{Voter turnout} \\\\"
  cmidrule <- "\\cmidrule(l){2-4} \\cmidrule(l){5-7}"
  lines <- c(lines[1:top_idx[1]],
             multicol,
             cmidrule,
             lines[(top_idx[1] + 1):length(lines)])
}

# 3) booktabs -> \hline
lines <- gsub("\\\\toprule",    "\\\\hline", lines)
lines <- gsub("\\\\bottomrule", "\\\\hline", lines)

# 4) Midrules
mr <- grep("\\\\midrule", lines)
if (length(mr) >= 1) lines[mr[1]] <- gsub("\\\\midrule", "\\\\hline", lines[mr[1]])
if (length(mr) >= 2) for (i in mr[-1]) lines[i] <- ""

# 5) Fila vacía arriba de Observations
obs <- grep("^Observations", lines)
if (length(obs) >= 1) {
  empty <- paste0(strrep(" &", ncols - 1), " \\\\")
  lines <- c(lines[1:(obs[1] - 1)], empty, lines[obs[1]:length(lines)])
}

# 6) Nota centrada en footnotesize
endtab <- grep("\\\\end\\{tabular\\}", lines)
if (length(endtab) >= 1) {
  nota <- c("\\vspace{0.4em}",
            "\\begin{center}",
            "\\footnotesize Notes: Standard errors clustered at the municipality level in parentheses. * $p<0.10$, ** $p<0.05$, *** $p<0.01$.",
            "\\end{center}")
  lines <- c(lines[1:endtab[1]], nota, lines[(endtab[1] + 1):length(lines)])
}

writeLines(lines, "Output/att_blankvotes_turnout.tex")

}

### 1.3. Voto izquierda vs derecha ### 
{
# 3.1 Share izquierda
  
  # Paso y generales controlando por tipo de elección
  att_iz1 <- feols(share_izq ~ share_1936_1955:post  + share_1956_1978:post | 
                    mun_code + anio + tipo_eleccion, data = dip_nac_mun)
  summary(att_iz1)

  # Controlando también por el lag del outcome
  att_iz1_l <- feols(share_izq ~ share_1936_1955:post  + share_1956_1978:post + share_izq_l | 
                     mun_code + anio + tipo_eleccion, data = dip_nac_mun)
  
# 3.2 Share centro izquierda 

  # Paso y generales controlando por tipo de elección
  att_ciz1 <- feols(share_cen_izq ~ share_1936_1955:post  + share_1956_1978:post | 
                   mun_code + anio + tipo_eleccion, data = dip_nac_mun)
  summary(att_ciz1)

# 3.3 Share centro derecha 

  # Paso y generales controlando por tipo de elección
  att_cde1 <- feols(share_cen_der ~ share_1936_1955:post  + share_1956_1978:post | 
                    mun_code + anio + tipo_eleccion, data = dip_nac_mun)
  summary(att_cde1)

# 3.4 Share derecha 

  # Paso y generales controlando por tipo de elección
  att_de1 <- feols(share_der ~ share_1936_1955:post  + share_1956_1978:post | 
                    mun_code + anio + tipo_eleccion, data = dip_nac_mun)
  summary(att_de1)
  
  # Controlando también por el lag del outcome
  att_de1_l <- feols(share_der ~ share_1936_1955:post  + share_1956_1978:post + share_der_l | 
                        mun_code + anio + tipo_eleccion, data = dip_nac_mun)
  summary(att_de1_l)

# 3.5 Share izquierda amplia 

  # Paso y generales controlando por tipo de elección
  att_iza1 <- feols(share_izq_amplia ~ share_1936_1955:post  + share_1956_1978:post | 
                   mun_code + anio + tipo_eleccion, data = dip_nac_mun)
  summary(att_iza1)
  
  # Controlando también por el lag del outcome
  att_iza1_l <- feols(share_izq_amplia ~ share_1936_1955:post  + share_1956_1978:post + share_izq_amplia_l | 
                       mun_code + anio + tipo_eleccion, data = dip_nac_mun)
  summary(att_iza1_l)

# 3.6 Share derecha amplia 

  # Paso y generales controlando por tipo de elección
  att_dea1 <- feols(share_der_amplia ~ share_1936_1955:post  + share_1956_1978:post | 
                    mun_code + anio + tipo_eleccion, data = dip_nac_mun)
  
  summary(att_dea1)
  
  # Controlando también por el lag del outcome
  att_dea1_l <- feols(share_der_amplia ~ share_1936_1955:post  + share_1956_1978:post + share_der_amplia_l | 
                        mun_code + anio + tipo_eleccion, data = dip_nac_mun)
  summary(att_dea1_l)

# 3.7 Índice izquierda-derecha

  # Paso y generales controlando por tipo de elección
  att_Iid1 <- feols(indice_izq_der ~ share_1936_1955:post  + share_1956_1978:post | 
                    mun_code + anio + tipo_eleccion, data = dip_nac_mun)
  summary(att_Iid1)

# 3.8 Índice ideologico ponderado

  # Paso y generales controlando por tipo de elección
  att_Ip1 <- feols(indice_ideologico_pond ~ share_1936_1955:post  + share_1956_1978:post | 
                    mun_code + anio + tipo_eleccion, data = dip_nac_mun)
  summary(att_Ip1)

}

### 1.4. Voto partidos ### 
{
# 4.1 Share peronista
  
  # Paso y generales controlando por tipo de elección
  att_peron1 <- feols(share_peronistas ~ share_1936_1955:post  + share_1956_1978:post | 
                     mun_code + anio + tipo_eleccion, data = dip_nac_mun)
  summary(att_peron1)
  
  # Controlando también por el lag del outcome
  att_peron1_l <- feols(share_peronistas ~ share_1936_1955:post  + share_1956_1978:post + share_peronistas_l | 
                       mun_code + anio + tipo_eleccion, data = dip_nac_mun)
  summary(att_peron1_l)
  
# 4.2 Share radical
  
  # Paso y generales controlando por tipo de elección
  att_radical1 <- feols(share_radicales ~ share_1936_1955:post  + share_1956_1978:post | 
                        mun_code + anio + tipo_eleccion, data = dip_nac_mun)
  summary(att_radical1)

}

### 1.5. Incumbencia y alternancia ### 
{
  
# 5.1 Share oficialista
  
  # Paso y generales controlando por tipo de elección
  att_of1 <- feols(share_oficialismo ~ share_1936_1955:post  + share_1956_1978:post | 
                     mun_code + anio + tipo_eleccion, data = dip_nac_mun)
  summary(att_of1)
  
  event_of1_36 <- feols(share_oficialismo ~ i(anio, share_1936_1955, ref = 2021) |
                         mun_code + anio + tipo_eleccion, data = dip_nac_mun)
  iplot(event_of1_36)
  
  event_of1_56 <- feols(share_oficialismo ~ i(anio, share_1956_1978, ref = 2021) | 
                          mun_code + anio + tipo_eleccion, data = dip_nac_mun)
  iplot(event_of1_56)
  
  # Controlando también por el lag del outcome
  att_of1_l <- feols(share_oficialismo ~ share_1936_1955:post  + share_1956_1978:post + share_oficialismo_l | 
                          mun_code + anio + tipo_eleccion, data = dip_nac_mun)
  summary(att_of1_l)
  
# 5.2 Voto al incumbente (nacional)
  
  # Paso y generales controlando por tipo de elección
  att_in1 <- feols(voto_incumbente ~ share_1936_1955:post  + share_1956_1978:post | 
                     mun_code + anio + tipo_eleccion, data = dip_nac_mun)
  summary(att_in1)

# 5.3 Alternancia bloque ideológico (mismo tipo de elección, distinto año)
  
  # Paso y generales controlando por tipo de elección
  att_al1 <- feols(alternancia ~ share_1936_1955:post  + share_1956_1978:post | 
                     mun_code + anio + tipo_eleccion, data = dip_nac_mun)
  summary(att_al1)
  
  # Controlando también por el lag del outcome
  att_al1_l <- feols(alternancia ~ share_1936_1955:post  + share_1956_1978:post + alternancia_l | 
                       mun_code + anio + tipo_eleccion, data = dip_nac_mun)
  summary(att_al1_l)
  
# 5.4 Alternancia partido (entre paso y generales, mismo año)
  
  # Paso y generales controlando por tipo de elección
  att_apgp1 <- feols(alt_paso_gen_partido ~ share_1936_1955:post  + share_1956_1978:post | 
                     mun_code + anio, 
                     data = dip_nac_mun %>% 
                       select(mun_code, anio, alt_paso_gen_partido, share_1936_1955, share_1956_1978, post) %>% 
                       distinct())
  summary(att_apgp1)

# 5.5 Alternancia bloque ideológico (entre paso y generales, mismo año)
  
  # Paso y generales controlando por tipo de elección
  att_apgi1 <- feols(alt_paso_gen_clasif ~ share_1936_1955:post  + share_1956_1978:post | 
                     mun_code + anio, 
                     data = dip_nac_mun %>% 
                       select(mun_code, anio, alt_paso_gen_clasif, share_1936_1955, share_1956_1978, post) %>% 
                       distinct())
  summary(att_apgi1)
  
}

### 1.6. Competencia política ### 
{

# 6.1 Margen de victoria
  
  # Paso y generales controlando por tipo de elección
  att_mg1 <- feols(margen ~ share_1936_1955:post  + share_1956_1978:post | 
                     mun_code + anio + tipo_eleccion, data = dip_nac_mun)
  summary(att_mg1)

# 6.2 Indice NEP
  
  # Paso y generales controlando por tipo de elección
  att_nep1 <- feols(nep ~ share_1936_1955:post  + share_1956_1978:post | 
                     mun_code + anio + tipo_eleccion, data = dip_nac_mun)
  summary(att_nep1)
  
}

### 1.7. Exportar resultados de 3-6 ### 
{
  
## Exportar a Excel
  
  modelsummary(
    list(
      # Section 3 models:
      "Left" = att_iz1,
      "Center-left" = att_ciz1,
      "Center-right" = att_cde1,
      "Right" = att_de1,
      "Broad left" = att_iza1,
      "Broad right" = att_dea1,
      "Left-right ideology" = att_Iid1,
      "Ideological polarization" = att_Ip1,
      
      # Section 4 models:
      "Peronism" = att_peron1,
      "Radicalism" = att_radical1,
      
      # Section 5 models:
      "Incumbent party (share)" = att_of1,
      "Incumbent party (dummy)" = att_in1,
      "Ideological alternation" = att_al1,
      "Same party: PASO-general" = att_apgp1,
      "Same bloc: PASO-general" = att_apgi1,
      
      # Section 6 models:
      "Victory margin" = att_mg1,
      "ENP" = att_nep1
    ),
    output = "Argentina/Output/att_extras.xlsx",
    stars = c('*' = .1, '**' = .05, '***' = .01),
    gof_map = data.frame(
      raw = c("r.squared", "nobs"),
      clean = c("R²", "Observations"),
      fmt = c(3, 0)
    ),
    coef_map = c(
      "share_1936_1955:post" = "Post × Spanish share (1936–1955)",
      "post:share_1956_1978" = "Post × Spanish share (1956–1978)"
    ),
    add_rows = tibble(
      term = c("Municipality FE", "Time FE", "Election type FE", "General elections only"),
      "Left" = c("Yes", "Yes", "Yes", "No"),
      "Center-left" = c("Yes", "Yes", "Yes", "No"),
      "Center-right" = c("Yes", "Yes", "Yes", "No"),
      "Right" = c("Yes", "Yes", "Yes", "No"),
      "Broad left" = c("Yes", "Yes", "Yes", "No"),
      "Broad right" = c("Yes", "Yes", "Yes", "No"),
      "Left-right ideology" = c("Yes", "Yes", "Yes", "No"),
      "Ideological polarization" = c("Yes", "Yes", "Yes", "No"),
      "Peronism" = c("Yes", "Yes", "Yes", "No"),
      "Radicalism" = c("Yes", "Yes", "Yes", "No"),
      "Incumbent party (share)" = c("Yes", "Yes", "Yes", "No"),
      "Incumbent party (dummy)" = c("Yes", "Yes", "Yes", "No"),
      "Ideological alternation" = c("Yes", "Yes", "Yes", "No"),
      "Same party: PASO-general" = c("Yes", "Yes", "No", "No"),
      "Same bloc: PASO-general" = c("Yes", "Yes", "No", "No"),
      "Victory margin" = c("Yes", "Yes", "Yes", "No"),
      "ENP" = c("Yes", "Yes", "Yes", "No")
    ),
    notes = c("Standard errors clustered at the municipality level in parentheses.",
              "Same party: PASO-general and Same bloc: PASO-general are estimated at the municipality-year level.")
  )
  
## Exportar a tex
  
gm <- tibble::tribble(
  ~raw,        ~clean,         ~fmt,
  "nobs",      "Observations", 0,
  "r.squared", "R$^2$",        3)
  
# Effects on Left-Wing Vote Shares

cm <- c("share_1936_1955:post" = "Spanish share 1936-1955$\\times$Post",
        "post:share_1956_1978" = "Spanish share 1956-1978$\\times$Post",
        "share_izq_l" = "Lagged left vote share",
        "share_izq_amplia_l" = "Lagged broad left vote share")

models_list <- list("(1)" = att_iz1, "(2)" = att_iz1_l, "(3)" = att_iza1, "(4)" = att_iza1_l)
p_values  <- sapply(models_list, get_p_equal)
p_strings <- sprintf("%.3f", p_values)

add_rows <- tibble::tibble(
  term = c("$p$-value ($\\beta_{36{-}55} = \\beta_{56{-}78}$)",
           "Municipality FE", "Time FE", "Election type FE"),
  m1 = c(p_strings[1], "Yes", "Yes", "Yes"),
  m2 = c(p_strings[2], "Yes", "Yes", "Yes"),
  m3 = c(p_strings[3], "Yes", "Yes", "Yes"),
  m4 = c(p_strings[4], "Yes", "Yes", "Yes"))
names(add_rows) <- c("term", "(1)", "(2)", "(3)", "(4)")
  
tex <- modelsummary(
  models_list,
  output    = "latex",
  coef_map  = cm,
  gof_map   = gm,
  estimate  = "{estimate}{stars}",
  statistic = "({std.error})",
  stars     = c("*" = .10, "**" = .05, "***" = .01),
  add_rows  = add_rows,
  escape    = FALSE)
  
lines <- strsplit(tex, "\n")[[1]]
ncols <- 5   # 1 label + 4 modelos
  
# 1) Caption, más espacio vertical y horizontal en la tabla
beg_table <- grep("\\\\begin\\{table\\}", lines)
if (length(beg_table) >= 1) {
   header <- c(
    "\\caption{Effects on Left-Wing Vote Shares}",
    "\\renewcommand{\\arraystretch}{1.25}",
    "\\setlength{\\tabcolsep}{8pt}"
  )
  lines <- c(lines[1:beg_table[1]], header, lines[(beg_table[1] + 1):length(lines)])
}
  
# 2) Header agrupado: "Left vote share" (cols 2-3) + "Broad left vote share" (cols 4-5)
#    AMBOS en una sola fila
top_idx <- grep("\\\\toprule", lines)
if (length(top_idx) >= 1) {
  multicol <- " & \\multicolumn{2}{c}{Left vote share} & \\multicolumn{2}{c}{Broad left vote share} \\\\"
  cmidrule <- "\\cmidrule(lr){2-3} \\cmidrule(lr){4-5}"
  lines <- c(lines[1:top_idx[1]],
             multicol,
             cmidrule,
              lines[(top_idx[1] + 1):length(lines)])
}
  
# 3) booktabs -> \hline
lines <- gsub("\\\\toprule",    "\\\\hline", lines)
lines <- gsub("\\\\bottomrule", "\\\\hline", lines)
  
# 4) Midrules
mr <- grep("\\\\midrule", lines)
if (length(mr) >= 1) lines[mr[1]] <- gsub("\\\\midrule", "\\\\hline", lines[mr[1]])
if (length(mr) >= 2) for (i in mr[-1]) lines[i] <- ""
  
# 5) Fila vacía arriba de Observations
obs <- grep("^Observations", lines)
if (length(obs) >= 1) {
   empty <- paste0(strrep(" &", ncols - 1), " \\\\")
  lines <- c(lines[1:(obs[1] - 1)], empty, lines[obs[1]:length(lines)])
}
  
# 6) Nota centrada en footnotesize
endtab <- grep("\\\\end\\{tabular\\}", lines)
if (length(endtab) >= 1) {
  nota <- c("\\vspace{0.4em}",
            "\\begin{center}",
            "\\footnotesize Notes: Standard errors clustered at the municipality level in parentheses. * $p<0.10$, ** $p<0.05$, *** $p<0.01$.",
             "\\end{center}")
  lines <- c(lines[1:endtab[1]], nota, lines[(endtab[1] + 1):length(lines)])
}
  
writeLines(lines, "Output/att_left.tex")
  
# Effects on Right-Wing Vote Shares

cm <- c("share_1936_1955:post" = "Spanish share 1936-1955$\\times$Post",
        "post:share_1956_1978" = "Spanish share 1956-1978$\\times$Post",
        "share_der_l" = "Lagged right vote share",
        "share_der_amplia_l" = "Lagged broad right vote share")

models_list <- list("(1)" = att_de1, "(2)" = att_de1_l, "(3)" = att_dea1, "(4)" = att_dea1_l)
p_values  <- sapply(models_list, get_p_equal)
p_strings <- sprintf("%.3f", p_values)

add_rows <- tibble::tibble(
 term = c("$p$-value ($\\beta_{36{-}55} = \\beta_{56{-}78}$)",
           "Municipality FE", "Time FE", "Election type FE"),
  m1 = c(p_strings[1],"Yes", "Yes", "Yes"),
  m2 = c(p_strings[2], "Yes", "Yes", "Yes"),
  m3 = c(p_strings[3],"Yes", "Yes", "Yes"),
  m4 = c(p_strings[4],"Yes", "Yes", "Yes"))
names(add_rows) <- c("term", "(1)", "(2)", "(3)", "(4)")
  
tex <- modelsummary(
    models_list,
    output    = "latex",
    coef_map  = cm,
    gof_map   = gm,
    estimate  = "{estimate}{stars}",
    statistic = "({std.error})",
    stars     = c("*" = .10, "**" = .05, "***" = .01),
    add_rows  = add_rows,
    escape    = FALSE)
  
lines <- strsplit(tex, "\n")[[1]]
ncols <- 5   # 1 label + 4 modelos
  
# 1) Caption, más espacio vertical y horizontal en la tabla
beg_table <- grep("\\\\begin\\{table\\}", lines)
if (length(beg_table) >= 1) {
  header <- c(
    "\\caption{Effects on Right-Wing Vote Shares}",
    "\\renewcommand{\\arraystretch}{1.25}",
    "\\setlength{\\tabcolsep}{8pt}"
  )
  lines <- c(lines[1:beg_table[1]], header, lines[(beg_table[1] + 1):length(lines)])
}
  
# 2) Header agrupado: "Right vote share" (cols 2-3) + "Broad right vote share" (cols 4-5)
#    AMBOS en una sola fila
top_idx <- grep("\\\\toprule", lines)
if (length(top_idx) >= 1) {
  multicol <- " & \\multicolumn{2}{c}{Right vote share} & \\multicolumn{2}{c}{Broad right vote share} \\\\"
  cmidrule <- "\\cmidrule(lr){2-3} \\cmidrule(lr){4-5}"
  lines <- c(lines[1:top_idx[1]],
             multicol,
             cmidrule,
             lines[(top_idx[1] + 1):length(lines)])
}
  
# 3) booktabs -> \hline
lines <- gsub("\\\\toprule",    "\\\\hline", lines)
lines <- gsub("\\\\bottomrule", "\\\\hline", lines)

# 4) Midrules
mr <- grep("\\\\midrule", lines)
if (length(mr) >= 1) lines[mr[1]] <- gsub("\\\\midrule", "\\\\hline", lines[mr[1]])
if (length(mr) >= 2) for (i in mr[-1]) lines[i] <- ""
  
# 5) Fila vacía arriba de Observations
obs <- grep("^Observations", lines)
if (length(obs) >= 1) {
  empty <- paste0(strrep(" &", ncols - 1), " \\\\")
  lines <- c(lines[1:(obs[1] - 1)], empty, lines[obs[1]:length(lines)])
}
  
# 6) Nota centrada en footnotesize
endtab <- grep("\\\\end\\{tabular\\}", lines)
if (length(endtab) >= 1) {
  nota <- c("\\vspace{0.4em}",
            "\\begin{center}",
            "\\footnotesize Notes: Standard errors clustered at the municipality level in parentheses. * $p<0.10$, ** $p<0.05$, *** $p<0.01$.",
            "\\end{center}")
  lines <- c(lines[1:endtab[1]], nota, lines[(endtab[1] + 1):length(lines)])
}
  
writeLines(lines, "Output/att_right.tex")

# Effects on Party Alignment and Electoral Competition

cm <- c(
  "share_1936_1955:post" = "Spanish share 1936-1955$\\times$Post",
  "post:share_1956_1978" = "Spanish share 1956-1978$\\times$Post",
  "share_peronistas_l"   = "Lagged Peronist vote share",
  "share_oficialismo_l"  = "Lagged incumbent vote share",
  "alternancia_l"        = "Lagged ideological alternation"
)

models_list <- list(
  "(1)" = att_peron1,
  "(2)" = att_peron1_l,
  "(3)" = att_of1,
  "(4)" = att_of1_l,
  "(5)" = att_al1,
  "(6)" = att_al1_l
)
p_values  <- sapply(models_list, get_p_equal)
p_strings <- sprintf("%.3f", p_values)

add_rows <- tibble::tibble(
  term = c("$p$-value ($\\beta_{36{-}55} = \\beta_{56{-}78}$)",
           "Municipality FE", "Time FE", "Election type FE"),
  m1 = c(p_strings[1],"Yes", "Yes", "Yes"),
  m2 = c(p_strings[2], "Yes", "Yes", "Yes"),
  m3 = c(p_strings[3],"Yes", "Yes", "Yes"),
  m4 = c(p_strings[4],"Yes", "Yes", "Yes"),
  m5 = c(p_strings[5],"Yes", "Yes", "Yes"),
  m6 = c(p_strings[6],"Yes", "Yes", "Yes")
)

names(add_rows) <- c("term", "(1)", "(2)", "(3)", "(4)", "(5)", "(6)")

tex <- modelsummary(
  models_list,
  output    = "latex",
  coef_map  = cm,
  gof_map   = gm,
  estimate  = "{estimate}{stars}",
  statistic = "({std.error})",
  stars     = c("*" = .10, "**" = .05, "***" = .01),
  add_rows  = add_rows,
  escape    = FALSE
)

lines <- strsplit(tex, "\n")[[1]]
ncols <- 7   # 1 label + 6 modelos

# 1) Caption, más espacio vertical y horizontal en la tabla
beg_table <- grep("\\\\begin\\{table\\}", lines)
if (length(beg_table) >= 1) {
  header <- c(
    "\\caption{Effects on Party Alignment and Electoral Competition}",
    "\\renewcommand{\\arraystretch}{1.25}",
    "\\setlength{\\tabcolsep}{6pt}"
  )
  lines <- c(lines[1:beg_table[1]], header, lines[(beg_table[1] + 1):length(lines)])
}

# 2) Header agrupado:
#    Peronist vote share (cols 2-3)
#    Incumbent vote share (cols 4-5)
#    Ideological alternation (cols 6-7)
top_idx <- grep("\\\\toprule", lines)
if (length(top_idx) >= 1) {
  multicol <- paste0(
    " & \\multicolumn{2}{c}{Peronist vote share}",
    " & \\multicolumn{2}{c}{Incumbent vote share}",
    " & \\multicolumn{2}{c}{Ideological alternation} \\\\"
  )
  
  cmidrule <- "\\cmidrule(lr){2-3} \\cmidrule(lr){4-5} \\cmidrule(lr){6-7}"
  
  lines <- c(
    lines[1:top_idx[1]],
    multicol,
    cmidrule,
    lines[(top_idx[1] + 1):length(lines)]
  )
}

# 3) booktabs -> \hline
lines <- gsub("\\\\toprule",    "\\\\hline", lines)
lines <- gsub("\\\\bottomrule", "\\\\hline", lines)

# 4) Midrules
mr <- grep("\\\\midrule", lines)
if (length(mr) >= 1) lines[mr[1]] <- gsub("\\\\midrule", "\\\\hline", lines[mr[1]])
if (length(mr) >= 2) for (i in mr[-1]) lines[i] <- ""

# 5) Fila vacía arriba de Observations
obs <- grep("^Observations", lines)
if (length(obs) >= 1) {
  empty <- paste0(strrep(" &", ncols - 1), " \\\\")
  lines <- c(lines[1:(obs[1] - 1)], empty, lines[obs[1]:length(lines)])
}

# 6) Nota centrada en footnotesize
endtab <- grep("\\\\end\\{tabular\\}", lines)
if (length(endtab) >= 1) {
  nota <- c(
    "\\vspace{0.4em}",
    "\\begin{center}",
    "\\footnotesize Notes: Standard errors clustered at the municipality level in parentheses. * $p<0.10$, ** $p<0.05$, *** $p<0.01$.",
    "\\end{center}"
  )
  
  lines <- c(lines[1:endtab[1]], nota, lines[(endtab[1] + 1):length(lines)])
}

writeLines(lines, "Output/att_alingment_competition.tex")

}

# ------------------------------------------ #
# 2. Estimaciones (nivel secciones)
# ------------------------------------------ #

### 2.1. Voto Blanco ### 
{
# Controlando por tipo de elección
feols(porcentaje_blanco ~ share_1936_1955:post  + share_1956_1978:post | 
        mun_code + anio + tipo_eleccion, data = dip_nac_sec, cluster = ~mun_code)

event_b1_36_s <- feols(porcentaje_blanco ~ i(anio, share_1936_1955, ref = 2021) |
                         mun_code + anio + tipo_eleccion, data = dip_nac_sec, cluster = ~mun_code)
summary(event_b1_36_s)
iplot(event_b1_36_s)

event_b1_56_s <- feols(porcentaje_blanco ~ i(anio, share_1956_1978, ref = 2021) | 
                         mun_code + anio + tipo_eleccion, data = dip_nac_sec, cluster = ~mun_code)
summary(event_b1_56_s)
iplot(event_b1_56_s)

# Solo generales 
feols(porcentaje_blanco ~ share_1936_1955:post  + share_1956_1978:post | 
        mun_code + anio , data = dip_nac_sec_gen, cluster = ~mun_code)

event_b2_36_s <- feols(porcentaje_blanco ~ i(anio, share_1936_1955, ref = 2021) |
                         mun_code + anio , data = dip_nac_sec_gen, cluster = ~mun_code)
summary(event_b2_36_s)
iplot(event_b2_36_s)

event_b2_56_s <- feols(porcentaje_blanco ~ i(anio, share_1956_1978, ref = 2021) | 
                         mun_code + anio , data = dip_nac_sec_gen, cluster = ~mun_code)
summary(event_b2_56_s)
iplot(event_b2_56_s)
}

# probar otros outcomes (primero generales, despues de derecha izquierda)

# ------------------------------------------ #
# 3. Chequeos de balance
# ------------------------------------------ #

### 3.1. Balance en tendencias pre-tratamiento ### 
{
  # Pre-treatment sample
  dip_nac_mun_pre <- dip_nac_mun %>% 
    filter(anio <= 2021)
  
  # Outcome variables
  outcomes <- c(
    "porcentaje_blanco",
    "participacion",
    "share_izq",
    "share_cen_izq",
    "share_cen_der",
    "share_der",
    "share_izq_amplia",
    "share_der_amplia",
    "indice_izq_der",
    "indice_ideologico_pond",
    "share_peronistas",
    "share_radicales",
    "share_oficialismo",
    "alternancia", 
    "alt_paso_gen_partido",
    "alt_paso_gen_clasif",
    "margen",
    "nep"
  )
  
  # Regresiones
  models_pretrends <- list()
  for (y in outcomes) {
    fml <- as.formula(
      paste0(
        y, " ~ ",
        "i(anio, share_1936_1955, ref = 2011) + ",
        "i(anio, share_1956_1978, ref = 2011) | ",
        "mun_code + anio + tipo_eleccion"
      )
    )
    
    models_pretrends[[y]] <- feols(
      fml,
      data = dip_nac_mun_pre,
      cluster = ~ mun_code
    )
  }
  
  names(models_pretrends) <- c(
    "Blank votes",
    "Turnout",
    "Left",
    "Center-left",
    "Center-right",
    "Right",
    "Broad left",
    "Broad right",
    "Left-right index",
    "Ideological index",
    "Peronist vote",
    "Radical vote",
    "Incumbent party (share)",
    "Ideological alternation",
    "Same party: PASO-general",
    "Same bloc: PASO-general",
    "Margin",
    "ENP"
  )
  
  # Guardar todos los modelos en un excel
  modelsummary(
    models_pretrends,
    output = "Output/pretrends_spanish_share.xlsx",
    stars = c("*" = 0.10, "**" = 0.05, "***" = 0.01),
    gof_omit = "AIC|BIC|Log.Lik|RMSE|Std.Errors|FE|Adj|R2 Adj.|R2 Within Adj."
  )
  
  # Test de significatividad conjunta
  pretrend_tests <- map_dfr(outcomes, function(y) {
    fml <- as.formula(
      paste0(
        y, " ~ ",
        "i(anio, share_1936_1955, ref = 2011) + ",
        "i(anio, share_1956_1978, ref = 2011) | ",
        "mun_code + anio + tipo_eleccion"
        )
    )
  
    mod <- feols(fml, data = dip_nac_mun_pre, cluster = ~ mun_code)
    
    test_1936 <- wald(mod, keep = "share_1936_1955")
    test_1956 <- wald(mod, keep = "share_1956_1978")
    test_all  <- wald(mod, keep = "share_1936_1955|share_1956_1978")

    tibble(outcome = y, 
           p_value_1936_1955 = test_1936$p,
           p_value_1956_1978 = test_1956$p,
           p_value_joint_all = test_all$p
           )
  })
  
  pretrend_tests <- pretrend_tests %>%
    mutate(
      outcome =  dplyr::recode(
        outcome,
        porcentaje_blanco = "Blank votes",
        participacion = "Turnout",
        share_izq = "Left",
        share_cen_izq = "Center-left",
        share_cen_der = "Center-right",
        share_der = "Right",
        share_izq_amplia = "Broad left",
        share_der_amplia = "Broad right",
        indice_izq_der = "Left-right index",
        indice_ideologico_pond = "Ideological index",
        share_peronistas = "Peronist vote",
        share_radicales = "Radical vote",
        share_oficialismo = "Incumbent party (share)",
        alternancia = "Ideological alternation",
        alt_paso_gen_partido = "Same party: PASO-general",
        alt_paso_gen_clasif = "Same bloc: PASO-general",
        margen = "Margin",
        nep = "ENP"
      ),
      across(starts_with("p_value"), ~ round(.x, 3))
    ) %>% 
    rename(
      Outcome = outcome,
      `p-value (1936-1955)` = p_value_1936_1955,
      `p-value (1956-1978)` = p_value_1956_1978,
      `p-value (joint)` = p_value_joint_all
    )
  
  write.xlsx(pretrend_tests, file = "Output/pretrend_joint_tests.xlsx", overwrite = TRUE)
}

### 3.2. Balance en niveles pre-tratamiento ### 
{
# Regresiones promediando los outomes en 2011-2021
  
  # Collapse to municipal pre-treatment averages
  dip_nac_mun_pre_avg <- dip_nac_mun_pre %>% 
    group_by(mun_code) %>% 
    summarise(
      across(
        all_of(outcomes), ~ mean(.x, na.rm = TRUE), .names = "{.col}_pre_avg"),
      share_1936_1955 = first(share_1936_1955),
      share_1956_1978 = first(share_1956_1978),
      .groups = "drop"
    )
  
  # Run balance regressions
  models_balance_levels <- list()
  for (y in outcomes) {
    y_avg <- paste0(y, "_pre_avg")
    fml <- as.formula(
      paste0(y_avg, " ~ ", "share_1936_1955 + share_1956_1978")
      )
    
    models_balance_levels[[y]] <- feols(fml, data = dip_nac_mun_pre_avg, cluster = ~ mun_code)
  }
  
  # Nombres lindos a las columnas
  names(models_balance_levels) <- c(
    "Blank votes",
    "Turnout",
    "Left",
    "Center-left",
    "Center-right",
    "Right",
    "Broad left",
    "Broad right",
    "Left-right index",
    "Ideological index",
    "Peronist vote",
    "Radical vote",
    "Incumbent party (share)",
    "Ideological alternation",
    "Same party: PASO-general",
    "Same bloc: PASO-general",
    "Margin",
    "ENP"
  )
  
  # Exportar
  tab_balance_levels <- modelsummary(
    models_balance_levels,
    output = "Output/preaverage_spanish_share.xlsx",
    stars = c("*" = 0.10, "**" = 0.05, "***" = 0.01),
    gof_omit = "AIC|BIC|Log.Lik|RMSE|Std.Errors|FE|Adj|Within"
  )
  
# Regesiones con FE de año
  
  # Run balance regressions
  models_balance_levels_fe <- list()
  for (y in outcomes) {
    fml <- as.formula(
      paste0(y, " ~ ", "share_1936_1955 + share_1956_1978", "| anio")
    )
    
    models_balance_levels_fe[[y]] <- feols(fml, data = dip_nac_mun_pre, cluster = ~ mun_code)
  }
  
  # Nombres lindos a las columnas
  names(models_balance_levels_fe) <- c(
    "Blank votes",
    "Turnout",
    "Left",
    "Center-left",
    "Center-right",
    "Right",
    "Broad left",
    "Broad right",
    "Left-right index",
    "Ideological index",
    "Peronist vote",
    "Radical vote",
    "Incumbent party (share)",
    "Ideological alternation",
    "Same party: PASO-general",
     "Same bloc: PASO-general",
    "Margin",
    "ENP"
  )
  
  # Exportar
  tab_balance_levels_fe <- modelsummary(
    models_balance_levels_fe,
    output = "Output/pre_fe_spanish_share.xlsx",
    stars = c("*" = 0.10, "**" = 0.05, "***" = 0.01),
    gof_omit = "AIC|BIC|Log.Lik|RMSE|Std.Errors|FE|Adj|Within"
  )
  
  # Guardo la data para usarla para EB
  write_dta(dip_nac_mun_pre_avg, "Data Int/dip_nac_mun_pre_avg.dta")

}

# ------------------------------------------ #
# 4. Estimaciones con EB
# ------------------------------------------ #

### 4.1 Voto en Blanco ###
{
# Set de pesos 1
  event_b2_36_eb1 <- feols(porcentaje_blanco ~ i(anio, share_1936_1955, ref = 2021) |
                         mun_code + anio + tipo_eleccion, data = dip_nac_mun_eb, weights = ~ w_1936_1)
  event_b2_56_eb1 <- feols(porcentaje_blanco ~ i(anio, share_1956_1978, ref = 2021) |
                            mun_code + anio + tipo_eleccion, data = dip_nac_mun_eb, weights = ~ w_1956_1)
  # Resumen pre balance
  summary(event_b2_36)
  summary(event_b2_56)
  # Resumen post balance
  summary(event_b2_56_eb1)
  summary(event_b2_36_eb1)
  
# Set de pesos 2
  event_b2_36_eb2 <- feols(porcentaje_blanco ~ i(anio, share_1936_1955, ref = 2021) |
                            mun_code + anio + tipo_eleccion, data = dip_nac_mun_eb, weights = ~ w_1936_2)
  event_b2_56_eb2 <- feols(porcentaje_blanco ~ i(anio, share_1956_1978, ref = 2021) |
                            mun_code + anio + tipo_eleccion, data = dip_nac_mun_eb, weights = ~ w_1956_2)
  # Resumen pre balance
  summary(event_b2_36)
  summary(event_b2_56)
  # Resumen post balance
  summary(event_b2_36_eb2)
  summary(event_b2_56_eb2)
  
# Set de pesos 3
  event_b2_36_eb3 <- feols(porcentaje_blanco ~ i(anio, share_1936_1955, ref = 2021) |
                             mun_code + anio + tipo_eleccion, data = dip_nac_mun_eb, weights = ~ w_1936_3)
  event_b2_56_eb3 <- feols(porcentaje_blanco ~ i(anio, share_1956_1978, ref = 2021) |
                             mun_code + anio + tipo_eleccion, data = dip_nac_mun_eb, weights = ~ w_1956_3)
  # Resumen pre balance
  summary(event_b2_36)
  summary(event_b2_56)
  # Resumen post balance
  summary(event_b2_36_eb3)
  summary(event_b2_56_eb3)
  
# Set de pesos 4
  event_b2_36_eb4 <- feols(porcentaje_blanco ~ i(anio, share_1936_1955, ref = 2021) |
                             mun_code + anio + tipo_eleccion, data = dip_nac_mun_eb, weights = ~ w_1936_4)
  event_b2_56_eb4 <- feols(porcentaje_blanco ~ i(anio, share_1956_1978, ref = 2021) |
                             mun_code + anio + tipo_eleccion, data = dip_nac_mun_eb, weights = ~ w_1956_4)
  # Resumen pre balance
  summary(event_b2_36)
  summary(event_b2_56)
  # Resumen post balance
  summary(event_b2_36_eb4)
  summary(event_b2_56_eb4)
  
# Set de pesos 5
  event_b2_36_eb5 <- feols(porcentaje_blanco ~ i(anio, share_1936_1955, ref = 2021) |
                             mun_code + anio + tipo_eleccion, data = dip_nac_mun_eb, weights = ~ w_1936_5)
  event_b2_56_eb5 <- feols(porcentaje_blanco ~ i(anio, share_1956_1978, ref = 2021) |
                             mun_code + anio + tipo_eleccion, data = dip_nac_mun_eb, weights = ~ w_1956_5)
  # Resumen pre balance
  summary(event_b2_36)
  summary(event_b2_56)
  # Resumen post balance
  summary(event_b2_36_eb5)
  summary(event_b2_56_eb5)

# Set de pesos 6
  event_b2_36_eb6 <- feols(porcentaje_blanco ~ i(anio, share_1936_1955, ref = 2021) |
                             mun_code + anio + tipo_eleccion, data = dip_nac_mun_eb, weights = ~ w_1936_6)
  event_b2_56_eb6 <- feols(porcentaje_blanco ~ i(anio, share_1956_1978, ref = 2021) |
                             mun_code + anio + tipo_eleccion, data = dip_nac_mun_eb, weights = ~ w_1956_6)
  # Resumen post balance
  summary(event_b2_36_eb6)
  summary(event_b2_56_eb6)
  
# Set de pesos 7
  event_b2_36_eb7 <- feols(porcentaje_blanco ~ i(anio, share_1936_1955, ref = 2021) |
                             mun_code + anio + tipo_eleccion, data = dip_nac_mun_eb, weights = ~ w_1936_7)
  event_b2_56_eb7 <- feols(porcentaje_blanco ~ i(anio, share_1956_1978, ref = 2021) |
                             mun_code + anio + tipo_eleccion, data = dip_nac_mun_eb, weights = ~ w_1956_7)
  # Resumen post balance
  summary(event_b2_36_eb7)
  summary(event_b2_56_eb7)
  
# Set de pesos 8
  event_b2_36_eb8 <- feols(porcentaje_blanco ~ i(anio, share_1936_1955, ref = 2021) |
                            mun_code + anio + tipo_eleccion, data = dip_nac_mun_eb, weights = ~ w_1936_8)
  event_b2_56_eb8 <- feols(porcentaje_blanco ~ i(anio, share_1956_1978, ref = 2021) |
                             mun_code + anio + tipo_eleccion, data = dip_nac_mun_eb, weights = ~ w_1956_8)
  # Resumen post balance
  summary(event_b2_36_eb8)
  summary(event_b2_56_eb8)

# Exporto los resultados
modelsummary(
  list(
    "No EB (36-55)" = event_b2_36,
    "No EB (56-78)" = event_b2_56,
    "EB1 (36-55)" = event_b2_36_eb1,
    "EB1 (56-78)" = event_b2_56_eb1,
    "EB2 (36-55)" = event_b2_36_eb2,
    "EB2 (56-78)" = event_b2_56_eb2,
    "EB3 (36-55)" = event_b2_36_eb3,
    "EB3 (56-78)" = event_b2_56_eb3,
    "EB4 (36-55)" = event_b2_36_eb4,
    "EB4 (56-78)" = event_b2_56_eb4,
    "EB5 (36-55)" = event_b2_36_eb5,
    "EB5 (56-78)" = event_b2_56_eb5,
    "EB6 (36-55)" = event_b2_36_eb6,
    "EB6 (56-78)" = event_b2_56_eb6,
    "EB7 (36-55)" = event_b2_36_eb7,
    "EB7 (56-78)" = event_b2_56_eb7,
    "EB8 (36-55)" = event_b2_36_eb8,
    "EB8 (56-78)" = event_b2_56_eb8
  ),
  output = "Output/event_study_EB_blank.xlsx",
  stars = c("*" = 0.10, "**" = 0.05, "***" = 0.01),
  gof_map = data.frame(
    raw = c("r.squared", "nobs"),
    clean = c("R²", "Observations"),
    fmt = c(3, 0)
    ),
  coef_map = c(
    "anio::2011:share_1936_1955" = "2011 × Spanish share (1936-1955)",
    "anio::2013:share_1936_1955" = "2013 × Spanish share (1936-1955)",
    "anio::2015:share_1936_1955" = "2015 × Spanish share (1936-1955)",
    "anio::2017:share_1936_1955" = "2017 × Spanish share (1936-1955)",
    "anio::2019:share_1936_1955" = "2019 × Spanish share (1936-1955)",
    "anio::2023:share_1936_1955" = "2023 × Spanish share (1936-1955)",
    "anio::2025:share_1936_1955" = "2025 × Spanish share (1936-1955)",
    
    "anio::2011:share_1956_1978" = "2011 × Spanish share (1956-1978)",
    "anio::2013:share_1956_1978" = "2013 × Spanish share (1956-1978)",
    "anio::2015:share_1956_1978" = "2015 × Spanish share (1956-1978)",
    "anio::2017:share_1956_1978" = "2017 × Spanish share (1956-1978)",
    "anio::2019:share_1956_1978" = "2019 × Spanish share (1956-1978)",
    "anio::2023:share_1956_1978" = "2023 × Spanish share (1956-1978)",
    "anio::2025:share_1956_1978" = "2025 × Spanish share (1956-1978)"
  ),
  add_rows = tibble::tibble(
    term = c("Municipality FE", "Time FE", "Election type FE"),
    `No EB (36-55)` = c("Yes", "Yes", "Yes"),
    `No EB (56-78)` = c("Yes", "Yes", "Yes"),
    `EB1 (36-55)` = c("Yes", "Yes", "Yes"),
    `EB1 (56-78)` = c("Yes", "Yes", "Yes"),
    `EB2 (36-55)` = c("Yes", "Yes", "Yes"),
    `EB2 (56-78)` = c("Yes", "Yes", "Yes"),
    `EB3 (36-55)` = c("Yes", "Yes", "Yes"),
    `EB3 (56-78)` = c("Yes", "Yes", "Yes"),
    `EB4 (36-55)` = c("Yes", "Yes", "Yes"),
    `EB4 (56-78)` = c("Yes", "Yes", "Yes"),
    `EB5 (36-55)` = c("Yes", "Yes", "Yes"),
    `EB5 (56-78)` = c("Yes", "Yes", "Yes"),
    `EB6 (36-55)` = c("Yes", "Yes", "Yes"),
    `EB6 (56-78)` = c("Yes", "Yes", "Yes"),
    `EB7 (36-55)` = c("Yes", "Yes", "Yes"),
    `EB7 (56-78)` = c("Yes", "Yes", "Yes"),
    `EB8 (36-55)` = c("Yes", "Yes", "Yes"),
    `EB8 (56-78)` = c("Yes", "Yes", "Yes")
  ),
  notes = "Standard errors clustered at the municipality level in parentheses."
)
  
}

### 4.2 Participación ###
{
  # Set de pesos 1
  event_p2_36_eb1 <- feols(participacion ~ i(anio, share_1936_1955, ref = 2021) |
                             mun_code + anio + tipo_eleccion, data = dip_nac_mun_eb, weights = ~ w_1936_1)
  event_p2_56_eb1 <- feols(participacion ~ i(anio, share_1956_1978, ref = 2021) |
                             mun_code + anio + tipo_eleccion, data = dip_nac_mun_eb, weights = ~ w_1956_1)
  # Resumen post balance
  summary(event_p2_56_eb1)
  summary(event_p2_36_eb1)
  
  # Set de pesos 2
  event_p2_36_eb2 <- feols(participacion ~ i(anio, share_1936_1955, ref = 2021) |
                             mun_code + anio + tipo_eleccion, data = dip_nac_mun_eb, weights = ~ w_1936_2)
  event_p2_56_eb2 <- feols(participacion ~ i(anio, share_1956_1978, ref = 2021) |
                             mun_code + anio + tipo_eleccion, data = dip_nac_mun_eb, weights = ~ w_1956_2)
  # Resumen post balance
  summary(event_p2_36_eb2)
  summary(event_p2_56_eb2)
  
  # Set de pesos 3
  event_p2_36_eb3 <- feols(participacion ~ i(anio, share_1936_1955, ref = 2021) |
                             mun_code + anio + tipo_eleccion, data = dip_nac_mun_eb, weights = ~ w_1936_3)
  event_p2_56_eb3 <- feols(participacion ~ i(anio, share_1956_1978, ref = 2021) |
                             mun_code + anio + tipo_eleccion, data = dip_nac_mun_eb, weights = ~ w_1956_3)
  # Resumen post balance
  summary(event_p2_36_eb3)
  summary(event_p2_56_eb3)
  
  # Set de pesos 4
  event_p2_36_eb4 <- feols(participacion ~ i(anio, share_1936_1955, ref = 2021) |
                             mun_code + anio + tipo_eleccion, data = dip_nac_mun_eb, weights = ~ w_1936_4)
  event_p2_56_eb4 <- feols(participacion ~ i(anio, share_1956_1978, ref = 2021) |
                             mun_code + anio + tipo_eleccion, data = dip_nac_mun_eb, weights = ~ w_1956_4)
  # Resumen post balance
  summary(event_p2_36_eb4)
  summary(event_p2_56_eb4)
  
  # Set de pesos 5
  event_p2_36_eb5 <- feols(participacion ~ i(anio, share_1936_1955, ref = 2021) |
                             mun_code + anio + tipo_eleccion, data = dip_nac_mun_eb, weights = ~ w_1936_5)
  event_p2_56_eb5 <- feols(participacion ~ i(anio, share_1956_1978, ref = 2021) |
                             mun_code + anio + tipo_eleccion, data = dip_nac_mun_eb, weights = ~ w_1956_5)
  # Resumen post balance
  summary(event_p2_36_eb5)
  summary(event_p2_56_eb5)
  
# Set de pesos 6
  event_p2_36_eb6 <- feols(participacion ~ i(anio, share_1936_1955, ref = 2021) |
                             mun_code + anio + tipo_eleccion, data = dip_nac_mun_eb, weights = ~ w_1936_6)
  event_p2_56_eb6 <- feols(participacion ~ i(anio, share_1956_1978, ref = 2021) |
                             mun_code + anio + tipo_eleccion, data = dip_nac_mun_eb, weights = ~ w_1956_6)
  # Resumen post balance
  summary(event_p2_36_eb6)
  summary(event_p2_56_eb6)
  
# Set de pesos 7
  event_p2_36_eb7 <- feols(participacion ~ i(anio, share_1936_1955, ref = 2021) |
                             mun_code + anio + tipo_eleccion, data = dip_nac_mun_eb, weights = ~ w_1936_7)
  event_p2_56_eb7 <- feols(participacion ~ i(anio, share_1956_1978, ref = 2021) |
                             mun_code + anio + tipo_eleccion, data = dip_nac_mun_eb, weights = ~ w_1956_7)
  # Resumen post balance
  summary(event_p2_36_eb7)
  summary(event_p2_56_eb7)

# Set de pesos 8
  event_p2_36_eb8 <- feols(participacion ~ i(anio, share_1936_1955, ref = 2021) |
                             mun_code + anio + tipo_eleccion, data = dip_nac_mun_eb, weights = ~ w_1936_8)
  event_p2_56_eb8 <- feols(participacion ~ i(anio, share_1956_1978, ref = 2021) |
                             mun_code + anio + tipo_eleccion, data = dip_nac_mun_eb, weights = ~ w_1956_8)
  # Resumen post balance
  summary(event_p2_36_eb8)
  summary(event_p2_56_eb8)
  
# Exporto los resultados
modelsummary(
  list(
    "No EB (36-55)" = event_p2_36,
    "No EB (56-78)" = event_p2_56,
    "EB1 (36-55)" = event_p2_36_eb1,
    "EB1 (56-78)" = event_p2_56_eb1,
    "EB2 (36-55)" = event_p2_36_eb2,
    "EB2 (56-78)" = event_p2_56_eb2,
    "EB3 (36-55)" = event_p2_36_eb3,
    "EB3 (56-78)" = event_p2_56_eb3,
    "EB4 (36-55)" = event_p2_36_eb4,
    "EB4 (56-78)" = event_p2_56_eb4,
    "EB5 (36-55)" = event_p2_36_eb5,
    "EB5 (56-78)" = event_p2_56_eb5,
    "EB6 (36-55)" = event_p2_36_eb6,
    "EB6 (56-78)" = event_p2_56_eb6,
    "EB7 (36-55)" = event_p2_36_eb7,
    "EB7 (56-78)" = event_p2_56_eb7,
    "EB8 (36-55)" = event_p2_36_eb8,
    "EB8 (56-78)" = event_p2_56_eb8
  ),
  output = "Output/event_study_EB_participation.xlsx",
  stars = c("*" = 0.10, "**" = 0.05, "***" = 0.01),
  gof_map = data.frame(
    raw = c("r.squared", "nobs"),
    clean = c("R²", "Observations"),
    fmt = c(3, 0)
  ),
  coef_map = c(
    "anio::2011:share_1936_1955" = "2011 × Spanish share (1936-1955)",
    "anio::2013:share_1936_1955" = "2013 × Spanish share (1936-1955)",
    "anio::2015:share_1936_1955" = "2015 × Spanish share (1936-1955)",
    "anio::2017:share_1936_1955" = "2017 × Spanish share (1936-1955)",
    "anio::2019:share_1936_1955" = "2019 × Spanish share (1936-1955)",
    "anio::2023:share_1936_1955" = "2023 × Spanish share (1936-1955)",
    "anio::2025:share_1936_1955" = "2025 × Spanish share (1936-1955)",
    
    "anio::2011:share_1956_1978" = "2011 × Spanish share (1956-1978)",
    "anio::2013:share_1956_1978" = "2013 × Spanish share (1956-1978)",
    "anio::2015:share_1956_1978" = "2015 × Spanish share (1956-1978)",
    "anio::2017:share_1956_1978" = "2017 × Spanish share (1956-1978)",
    "anio::2019:share_1956_1978" = "2019 × Spanish share (1956-1978)",
    "anio::2023:share_1956_1978" = "2023 × Spanish share (1956-1978)",
    "anio::2025:share_1956_1978" = "2025 × Spanish share (1956-1978)"
  ),
  add_rows = tibble::tibble(
    term = c("Municipality FE", "Time FE", "Election type FE"),
    `No EB (36-55)` = c("Yes", "Yes", "Yes"),
    `No EB (56-78)` = c("Yes", "Yes", "Yes"),
    `EB1 (36-55)` = c("Yes", "Yes", "Yes"),
    `EB1 (56-78)` = c("Yes", "Yes", "Yes"),
    `EB2 (36-55)` = c("Yes", "Yes", "Yes"),
    `EB2 (56-78)` = c("Yes", "Yes", "Yes"),
    `EB3 (36-55)` = c("Yes", "Yes", "Yes"),
    `EB3 (56-78)` = c("Yes", "Yes", "Yes"),
    `EB4 (36-55)` = c("Yes", "Yes", "Yes"),
    `EB4 (56-78)` = c("Yes", "Yes", "Yes"),
    `EB5 (36-55)` = c("Yes", "Yes", "Yes"),
    `EB5 (56-78)` = c("Yes", "Yes", "Yes"),
    `EB6 (36-55)` = c("Yes", "Yes", "Yes"),
    `EB6 (56-78)` = c("Yes", "Yes", "Yes"),
    `EB7 (36-55)` = c("Yes", "Yes", "Yes"),
    `EB7 (56-78)` = c("Yes", "Yes", "Yes"),
    `EB8 (36-55)` = c("Yes", "Yes", "Yes"),
    `EB8 (56-78)` = c("Yes", "Yes", "Yes")
  ),
  notes = "Standard errors clustered at the municipality level in parentheses."
)

}

### 4.3 Exporto los att de los mejores set ###
{
# Voto en blanco
  # ATT con el ser 3 pesos del 36
att_b2_eb3_36 <- feols(porcentaje_blanco ~ share_1936_1955:post  + share_1956_1978:post | 
                            mun_code + anio + tipo_eleccion, data = dip_nac_mun_eb, weights = ~ w_1936_3)
summary(att_b2_eb3_36)
  # ATT con el ser 3 pesos del 56
att_b2_eb3_56 <- feols(porcentaje_blanco ~ share_1936_1955:post  + share_1956_1978:post | 
                            mun_code + anio + tipo_eleccion, data = dip_nac_mun_eb, weights = ~ w_1956_3)
summary(att_b2_eb3_56)

# ATT con el ser 4 pesos del 36
att_b2_eb4_36 <- feols(porcentaje_blanco ~ share_1936_1955:post  + share_1956_1978:post | 
                         mun_code + anio + tipo_eleccion, data = dip_nac_mun_eb, weights = ~ w_1936_4)
summary(att_b2_eb4_36)
# ATT con el ser 4 pesos del 56
att_b2_eb4_56 <- feols(porcentaje_blanco ~ share_1936_1955:post  + share_1956_1978:post | 
                         mun_code + anio + tipo_eleccion, data = dip_nac_mun_eb, weights = ~ w_1956_4)
summary(att_b2_eb4_56)

modelsummary(
  list(
    "ATT EB 3 (36-55)" = att_b2_eb3_36,
    "ATT EB 3 (56-78)" = att_b2_eb3_56,
    
    "ATT EB 4 (36-55)" = att_b2_eb4_36,
    "ATT EB 4 (56-78)" = att_b2_eb4_56
  ),
  output = "Output/att_EB_34_blankvotes.xlsx",
  stars = c("*" = 0.10, "**" = 0.05, "***" = 0.01),
  gof_map = data.frame(
    raw = c("r.squared", "nobs"),
    clean = c("R²", "Observations"),
    fmt = c(3, 0)
  ),
  coef_map = c(
    "share_1936_1955:post" = "Spanish share (1936-1955) × Post",
    "post:share_1956_1978" = "Spanish share (1956-1978) × Post"
  ),
  add_rows = tibble::tibble(
    term = c("Municipality FE", "Time FE", "Election type FE"),
    `ATT EB 3 (36-55)` = c("Yes", "Yes", "Yes"),
    `ATT EB 3 (56-78)` = c("Yes", "Yes", "Yes"),
    `ATT EB 4 (36-55)` = c("Yes", "Yes", "Yes"),
    `ATT EB 4 (56-78)` = c("Yes", "Yes", "Yes")
  ),
  notes = "Standard errors clustered at the municipality level in parentheses."
)

# Participación
  # ATT con el ser 3 pesos del 36
att_p2_eb3_36 <- feols(participacion ~ share_1936_1955:post  + share_1956_1978:post | 
                         mun_code + anio + tipo_eleccion, data = dip_nac_mun_eb, weights = ~ w_1936_3)
summary(att_p2_eb3_36)
  # ATT con el ser 3 pesos del 56
att_p2_eb3_56 <- feols(participacion ~ share_1936_1955:post  + share_1956_1978:post | 
                         mun_code + anio + tipo_eleccion, data = dip_nac_mun_eb, weights = ~ w_1956_3)
summary(att_p2_eb3_56)

  # ATT con el ser 4 pesos del 36
att_p2_eb4_36 <- feols(participacion ~ share_1936_1955:post  + share_1956_1978:post | 
                         mun_code + anio + tipo_eleccion, data = dip_nac_mun_eb, weights = ~ w_1936_4)
summary(att_p2_eb4_36)
  # ATT con el ser 4 pesos del 56
att_p2_eb4_56 <- feols(participacion ~ share_1936_1955:post  + share_1956_1978:post | 
                         mun_code + anio + tipo_eleccion, data = dip_nac_mun_eb, weights = ~ w_1956_4)
summary(att_p2_eb4_56)

modelsummary(
  list(
    "ATT EB 3 (36-55)" = att_p2_eb3_36,
    "ATT EB 3 (56-78)" = att_p2_eb3_56,
    
    "ATT EB 4 (36-55)" = att_p2_eb4_36,
    "ATT EB 4 (56-78)" = att_p2_eb4_56
  ),
  output = "Output/att_EB_34_participacion.xlsx",
  stars = c("*" = 0.10, "**" = 0.05, "***" = 0.01),
  gof_map = data.frame(
    raw = c("r.squared", "nobs"),
    clean = c("R²", "Observations"),
    fmt = c(3, 0)
  ),
  coef_map = c(
    "share_1936_1955:post" = "Spanish share (1936-1955) × Post",
    "post:share_1956_1978" = "Spanish share (1956-1978) × Post"
  ),
  add_rows = tibble::tibble(
    term = c("Municipality FE", "Time FE", "Election type FE"),
    `ATT EB 3 (36-55)` = c("Yes", "Yes", "Yes"),
    `ATT EB 3 (56-78)` = c("Yes", "Yes", "Yes"),
    `ATT EB 4 (36-55)` = c("Yes", "Yes", "Yes"),
    `ATT EB 4 (56-78)` = c("Yes", "Yes", "Yes")
  ),
  notes = "Standard errors clustered at the municipality level in parentheses."
)
}

# ------------------------------------------ #
# 5. Estimaciones con EB (si el otro share)
# ------------------------------------------ #

### 5.1 Voto en Blanco ###
{
# Set de pesos 1
event_b2_36_eb1_ss <- feols(porcentaje_blanco ~ i(anio, share_1936_1955, ref = 2021) |
                           mun_code + anio + tipo_eleccion, data = dip_nac_mun_eb, weights = ~ w_ss_1936_1)
event_b2_56_eb1_ss <- feols(porcentaje_blanco ~ i(anio, share_1956_1978, ref = 2021) |
                           mun_code + anio + tipo_eleccion, data = dip_nac_mun_eb, weights = ~ w_ss_1956_1)

# Set de pesos 2
event_b2_36_eb2_ss <- feols(porcentaje_blanco ~ i(anio, share_1936_1955, ref = 2021) |
                           mun_code + anio + tipo_eleccion, data = dip_nac_mun_eb, weights = ~ w_ss_1936_2)
event_b2_56_eb2_ss <- feols(porcentaje_blanco ~ i(anio, share_1956_1978, ref = 2021) |
                           mun_code + anio + tipo_eleccion, data = dip_nac_mun_eb, weights = ~ w_ss_1956_2)

# Set de pesos 3
event_b2_36_eb3_ss <- feols(porcentaje_blanco ~ i(anio, share_1936_1955, ref = 2021) |
                           mun_code + anio + tipo_eleccion, data = dip_nac_mun_eb, weights = ~ w_ss_1936_3)
event_b2_56_eb3_ss <- feols(porcentaje_blanco ~ i(anio, share_1956_1978, ref = 2021) |
                           mun_code + anio + tipo_eleccion, data = dip_nac_mun_eb, weights = ~ w_ss_1956_3)

# Set de pesos 4
event_b2_36_eb4_ss <- feols(porcentaje_blanco ~ i(anio, share_1936_1955, ref = 2021) |
                           mun_code + anio + tipo_eleccion, data = dip_nac_mun_eb, weights = ~ w_ss_1936_4)
event_b2_56_eb4_ss <- feols(porcentaje_blanco ~ i(anio, share_1956_1978, ref = 2021) |
                           mun_code + anio + tipo_eleccion, data = dip_nac_mun_eb, weights = ~ w_ss_1956_4)

# Set de pesos 5
event_b2_36_eb5_ss <- feols(porcentaje_blanco ~ i(anio, share_1936_1955, ref = 2021) |
                           mun_code + anio + tipo_eleccion, data = dip_nac_mun_eb, weights = ~ w_ss_1936_5)
event_b2_56_eb5_ss <- feols(porcentaje_blanco ~ i(anio, share_1956_1978, ref = 2021) |
                           mun_code + anio + tipo_eleccion, data = dip_nac_mun_eb, weights = ~ w_ss_1956_5)

# Set de pesos 6
event_b2_36_eb6_ss <- feols(porcentaje_blanco ~ i(anio, share_1936_1955, ref = 2021) |
                           mun_code + anio + tipo_eleccion, data = dip_nac_mun_eb, weights = ~ w_ss_1936_6)
event_b2_56_eb6_ss <- feols(porcentaje_blanco ~ i(anio, share_1956_1978, ref = 2021) |
                           mun_code + anio + tipo_eleccion, data = dip_nac_mun_eb, weights = ~ w_ss_1956_6)

# Set de pesos 7
event_b2_36_eb7_ss <- feols(porcentaje_blanco ~ i(anio, share_1936_1955, ref = 2021) |
                           mun_code + anio + tipo_eleccion, data = dip_nac_mun_eb, weights = ~ w_ss_1936_7)
event_b2_56_eb7_ss <- feols(porcentaje_blanco ~ i(anio, share_1956_1978, ref = 2021) |
                           mun_code + anio + tipo_eleccion, data = dip_nac_mun_eb, weights = ~ w_ss_1956_7)

# Set de pesos 8
event_b2_36_eb8_ss <- feols(porcentaje_blanco ~ i(anio, share_1936_1955, ref = 2021) |
                           mun_code + anio + tipo_eleccion, data = dip_nac_mun_eb, weights = ~ w_ss_1936_8)
event_b2_56_eb8_ss <- feols(porcentaje_blanco ~ i(anio, share_1956_1978, ref = 2021) |
                           mun_code + anio + tipo_eleccion, data = dip_nac_mun_eb, weights = ~ w_ss_1956_8)

# Exporto los resultados
modelsummary(
  list(
    "No EB (36-55)" = event_b2_36,
    "No EB (56-78)" = event_b2_56,
    "EB1 (36-55)" = event_b2_36_eb1_ss,
    "EB1 (56-78)" = event_b2_56_eb1_ss,
    "EB2 (36-55)" = event_b2_36_eb2_ss,
    "EB2 (56-78)" = event_b2_56_eb2_ss,
    "EB3 (36-55)" = event_b2_36_eb3_ss,
    "EB3 (56-78)" = event_b2_56_eb3_ss,
    "EB4 (36-55)" = event_b2_36_eb4_ss,
    "EB4 (56-78)" = event_b2_56_eb4_ss,
    "EB5 (36-55)" = event_b2_36_eb5_ss,
    "EB5 (56-78)" = event_b2_56_eb5_ss,
    "EB6 (36-55)" = event_b2_36_eb6_ss,
    "EB6 (56-78)" = event_b2_56_eb6_ss,
    "EB7 (36-55)" = event_b2_36_eb7_ss,
    "EB7 (56-78)" = event_b2_56_eb7_ss,
    "EB8 (36-55)" = event_b2_36_eb8_ss,
    "EB8 (56-78)" = event_b2_56_eb8_ss
  ),
  output = "Output/event_study_EB_ss_blank.xlsx",
  stars = c("*" = 0.10, "**" = 0.05, "***" = 0.01),
  gof_map = data.frame(
    raw = c("r.squared", "nobs"),
    clean = c("R²", "Observations"),
    fmt = c(3, 0)
  ),
  coef_map = c(
    "anio::2011:share_1936_1955" = "2011 × Spanish share (1936-1955)",
    "anio::2013:share_1936_1955" = "2013 × Spanish share (1936-1955)",
    "anio::2015:share_1936_1955" = "2015 × Spanish share (1936-1955)",
    "anio::2017:share_1936_1955" = "2017 × Spanish share (1936-1955)",
    "anio::2019:share_1936_1955" = "2019 × Spanish share (1936-1955)",
    "anio::2023:share_1936_1955" = "2023 × Spanish share (1936-1955)",
    "anio::2025:share_1936_1955" = "2025 × Spanish share (1936-1955)",
    
    "anio::2011:share_1956_1978" = "2011 × Spanish share (1956-1978)",
    "anio::2013:share_1956_1978" = "2013 × Spanish share (1956-1978)",
    "anio::2015:share_1956_1978" = "2015 × Spanish share (1956-1978)",
    "anio::2017:share_1956_1978" = "2017 × Spanish share (1956-1978)",
    "anio::2019:share_1956_1978" = "2019 × Spanish share (1956-1978)",
    "anio::2023:share_1956_1978" = "2023 × Spanish share (1956-1978)",
    "anio::2025:share_1956_1978" = "2025 × Spanish share (1956-1978)"
  ),
  add_rows = tibble::tibble(
    term = c("Municipality FE", "Time FE", "Election type FE"),
    `No EB (36-55)` = c("Yes", "Yes", "Yes"),
    `No EB (56-78)` = c("Yes", "Yes", "Yes"),
    `EB1 (36-55)` = c("Yes", "Yes", "Yes"),
    `EB1 (56-78)` = c("Yes", "Yes", "Yes"),
    `EB2 (36-55)` = c("Yes", "Yes", "Yes"),
    `EB2 (56-78)` = c("Yes", "Yes", "Yes"),
    `EB3 (36-55)` = c("Yes", "Yes", "Yes"),
    `EB3 (56-78)` = c("Yes", "Yes", "Yes"),
    `EB4 (36-55)` = c("Yes", "Yes", "Yes"),
    `EB4 (56-78)` = c("Yes", "Yes", "Yes"),
    `EB5 (36-55)` = c("Yes", "Yes", "Yes"),
    `EB5 (56-78)` = c("Yes", "Yes", "Yes"),
    `EB6 (36-55)` = c("Yes", "Yes", "Yes"),
    `EB6 (56-78)` = c("Yes", "Yes", "Yes"),
    `EB7 (36-55)` = c("Yes", "Yes", "Yes"),
    `EB7 (56-78)` = c("Yes", "Yes", "Yes"),
    `EB8 (36-55)` = c("Yes", "Yes", "Yes"),
    `EB8 (56-78)` = c("Yes", "Yes", "Yes")
  ),
  notes = "Standard errors clustered at the municipality level in parentheses."
)
}

### 5.2 Participación ###
{
# Set de pesos 1
event_p2_36_eb1_ss <- feols(participacion ~ i(anio, share_1936_1955, ref = 2021) |
                           mun_code + anio + tipo_eleccion, data = dip_nac_mun_eb, weights = ~ w_ss_1936_1)
event_p2_56_eb1_ss <- feols(participacion ~ i(anio, share_1956_1978, ref = 2021) |
                           mun_code + anio + tipo_eleccion, data = dip_nac_mun_eb, weights = ~ w_ss_1956_1)

# Set de pesos 2
event_p2_36_eb2_ss <- feols(participacion ~ i(anio, share_1936_1955, ref = 2021) |
                           mun_code + anio + tipo_eleccion, data = dip_nac_mun_eb, weights = ~ w_ss_1936_2)
event_p2_56_eb2_ss <- feols(participacion ~ i(anio, share_1956_1978, ref = 2021) |
                           mun_code + anio + tipo_eleccion, data = dip_nac_mun_eb, weights = ~ w_ss_1956_2)

# Set de pesos 3
event_p2_36_eb3_ss <- feols(participacion ~ i(anio, share_1936_1955, ref = 2021) |
                           mun_code + anio + tipo_eleccion, data = dip_nac_mun_eb, weights = ~ w_ss_1936_3)
event_p2_56_eb3_ss <- feols(participacion ~ i(anio, share_1956_1978, ref = 2021) |
                           mun_code + anio + tipo_eleccion, data = dip_nac_mun_eb, weights = ~ w_ss_1956_3)

# Set de pesos 4
event_p2_36_eb4_ss <- feols(participacion ~ i(anio, share_1936_1955, ref = 2021) |
                           mun_code + anio + tipo_eleccion, data = dip_nac_mun_eb, weights = ~ w_ss_1936_4)
event_p2_56_eb4_ss <- feols(participacion ~ i(anio, share_1956_1978, ref = 2021) |
                           mun_code + anio + tipo_eleccion, data = dip_nac_mun_eb, weights = ~ w_ss_1956_4)

# Set de pesos 5
event_p2_36_eb5_ss <- feols(participacion ~ i(anio, share_1936_1955, ref = 2021) |
                           mun_code + anio + tipo_eleccion, data = dip_nac_mun_eb, weights = ~ w_ss_1936_5)
event_p2_56_eb5_ss <- feols(participacion ~ i(anio, share_1956_1978, ref = 2021) |
                           mun_code + anio + tipo_eleccion, data = dip_nac_mun_eb, weights = ~ w_ss_1956_5)

# Set de pesos 6
event_p2_36_eb6_ss <- feols(participacion ~ i(anio, share_1936_1955, ref = 2021) |
                           mun_code + anio + tipo_eleccion, data = dip_nac_mun_eb, weights = ~ w_ss_1936_6)
event_p2_56_eb6_ss <- feols(participacion ~ i(anio, share_1956_1978, ref = 2021) |
                           mun_code + anio + tipo_eleccion, data = dip_nac_mun_eb, weights = ~ w_ss_1956_6)

# Set de pesos 7
event_p2_36_eb7_ss <- feols(participacion ~ i(anio, share_1936_1955, ref = 2021) |
                           mun_code + anio + tipo_eleccion, data = dip_nac_mun_eb, weights = ~ w_ss_1936_7)
event_p2_56_eb7_ss <- feols(participacion ~ i(anio, share_1956_1978, ref = 2021) |
                           mun_code + anio + tipo_eleccion, data = dip_nac_mun_eb, weights = ~ w_ss_1956_7)

# Set de pesos 8
event_p2_36_eb8_ss <- feols(participacion ~ i(anio, share_1936_1955, ref = 2021) |
                           mun_code + anio + tipo_eleccion, data = dip_nac_mun_eb, weights = ~ w_ss_1936_8)
event_p2_56_eb8_ss <- feols(participacion ~ i(anio, share_1956_1978, ref = 2021) |
                           mun_code + anio + tipo_eleccion, data = dip_nac_mun_eb, weights = ~ w_ss_1956_8)

# Exporto los resultados
modelsummary(
  list(
    "No EB (36-55)" = event_p2_36,
    "No EB (56-78)" = event_p2_56,
    "EB1 (36-55)" = event_p2_36_eb1_ss,
    "EB1 (56-78)" = event_p2_56_eb1_ss,
    "EB2 (36-55)" = event_p2_36_eb2_ss,
    "EB2 (56-78)" = event_p2_56_eb2_ss,
    "EB3 (36-55)" = event_p2_36_eb3_ss,
    "EB3 (56-78)" = event_p2_56_eb3_ss,
    "EB4 (36-55)" = event_p2_36_eb4_ss,
    "EB4 (56-78)" = event_p2_56_eb4_ss,
    "EB5 (36-55)" = event_p2_36_eb5_ss,
    "EB5 (56-78)" = event_p2_56_eb5_ss,
    "EB6 (36-55)" = event_p2_36_eb6_ss,
    "EB6 (56-78)" = event_p2_56_eb6_ss,
    "EB7 (36-55)" = event_p2_36_eb7_ss,
    "EB7 (56-78)" = event_p2_56_eb7_ss,
    "EB8 (36-55)" = event_p2_36_eb8_ss,
    "EB8 (56-78)" = event_p2_56_eb8_ss
  ),
  output = "Output/event_study_EB_ss_participation.xlsx",
  stars = c("*" = 0.10, "**" = 0.05, "***" = 0.01),
  gof_map = data.frame(
    raw = c("r.squared", "nobs"),
    clean = c("R²", "Observations"),
    fmt = c(3, 0)
  ),
  coef_map = c(
    "anio::2011:share_1936_1955" = "2011 × Spanish share (1936-1955)",
    "anio::2013:share_1936_1955" = "2013 × Spanish share (1936-1955)",
    "anio::2015:share_1936_1955" = "2015 × Spanish share (1936-1955)",
    "anio::2017:share_1936_1955" = "2017 × Spanish share (1936-1955)",
    "anio::2019:share_1936_1955" = "2019 × Spanish share (1936-1955)",
    "anio::2023:share_1936_1955" = "2023 × Spanish share (1936-1955)",
    "anio::2025:share_1936_1955" = "2025 × Spanish share (1936-1955)",
    
    "anio::2011:share_1956_1978" = "2011 × Spanish share (1956-1978)",
    "anio::2013:share_1956_1978" = "2013 × Spanish share (1956-1978)",
    "anio::2015:share_1956_1978" = "2015 × Spanish share (1956-1978)",
    "anio::2017:share_1956_1978" = "2017 × Spanish share (1956-1978)",
    "anio::2019:share_1956_1978" = "2019 × Spanish share (1956-1978)",
    "anio::2023:share_1956_1978" = "2023 × Spanish share (1956-1978)",
    "anio::2025:share_1956_1978" = "2025 × Spanish share (1956-1978)"
  ),
  add_rows = tibble::tibble(
    term = c("Municipality FE", "Time FE", "Election type FE"),
    `No EB (36-55)` = c("Yes", "Yes", "Yes"),
    `No EB (56-78)` = c("Yes", "Yes", "Yes"),
    `EB1 (36-55)` = c("Yes", "Yes", "Yes"),
    `EB1 (56-78)` = c("Yes", "Yes", "Yes"),
    `EB2 (36-55)` = c("Yes", "Yes", "Yes"),
    `EB2 (56-78)` = c("Yes", "Yes", "Yes"),
    `EB3 (36-55)` = c("Yes", "Yes", "Yes"),
    `EB3 (56-78)` = c("Yes", "Yes", "Yes"),
    `EB4 (36-55)` = c("Yes", "Yes", "Yes"),
    `EB4 (56-78)` = c("Yes", "Yes", "Yes"),
    `EB5 (36-55)` = c("Yes", "Yes", "Yes"),
    `EB5 (56-78)` = c("Yes", "Yes", "Yes"),
    `EB6 (36-55)` = c("Yes", "Yes", "Yes"),
    `EB6 (56-78)` = c("Yes", "Yes", "Yes"),
    `EB7 (36-55)` = c("Yes", "Yes", "Yes"),
    `EB7 (56-78)` = c("Yes", "Yes", "Yes"),
    `EB8 (36-55)` = c("Yes", "Yes", "Yes"),
    `EB8 (56-78)` = c("Yes", "Yes", "Yes")
  ),
  notes = "Standard errors clustered at the municipality level in parentheses."
)


}

### 5.3 Exporto los att de los mejores set ###
{

# Voto en blanco
  # ATT con el ser 3 pesos del 36
att_b2_eb3_ss_36 <- feols(porcentaje_blanco ~ share_1936_1955:post  + share_1956_1978:post | 
                            mun_code + anio + tipo_eleccion, data = dip_nac_mun_eb, weights = ~ w_ss_1936_3)
summary(att_b2_eb3_ss_36)
  # ATT con el ser 3 pesos del 56
att_b2_eb3_ss_56 <- feols(porcentaje_blanco ~ share_1936_1955:post  + share_1956_1978:post | 
                            mun_code + anio + tipo_eleccion, data = dip_nac_mun_eb, weights = ~ w_ss_1956_3)
summary(att_b2_eb3_ss_56)

# Voto en blanco
  # ATT con el ser 4 pesos del 36
att_b2_eb4_ss_36 <- feols(porcentaje_blanco ~ share_1936_1955:post  + share_1956_1978:post | 
                            mun_code + anio + tipo_eleccion, data = dip_nac_mun_eb, weights = ~ w_ss_1936_4)
summary(att_b2_eb4_ss_36)
  # ATT con el ser 4 pesos del 56
att_b2_eb4_ss_56 <- feols(porcentaje_blanco ~ share_1936_1955:post  + share_1956_1978:post | 
                            mun_code + anio + tipo_eleccion, data = dip_nac_mun_eb, weights = ~ w_ss_1956_4)
summary(att_b2_eb4_ss_56)

modelsummary(
  list(
    "ATT EB 3 (SS, 36-55)" = att_b2_eb3_ss_36,
    "ATT EB 3 (SS, 56-78)" = att_b2_eb3_ss_56,
    
    "ATT EB 4 (SS, 36-55)" = att_b2_eb4_ss_36,
    "ATT EB 4 (SS, 56-78)" = att_b2_eb4_ss_56
  ),
  output = "Output/att_EB_ss_34_blankvotes.xlsx",
  stars = c("*" = 0.10, "**" = 0.05, "***" = 0.01),
  gof_map = data.frame(
    raw = c("r.squared", "nobs"),
    clean = c("R²", "Observations"),
    fmt = c(3, 0)
  ),
  coef_map = c(
    "share_1936_1955:post" = "Spanish share (1936-1955) × Post",
    "post:share_1956_1978" = "Spanish share (1956-1978) × Post"
  ),
  add_rows = tibble::tibble(
    term = c("Municipality FE", "Time FE", "Election type FE"),
    `ATT EB 3 (SS, 36-55)` = c("Yes", "Yes", "Yes"),
    `ATT EB 3 (SS, 56-78)` = c("Yes", "Yes", "Yes"),
    `ATT EB 4 (SS, 36-55)` = c("Yes", "Yes", "Yes"),
    `ATT EB 4 (SS, 56-78)` = c("Yes", "Yes", "Yes")
  ),
  notes = "Standard errors clustered at the municipality level in parentheses."
)
  
# Participación  
  # ATT con el ser 3 pesos del 36
att_p2_eb3_ss_36 <- feols(participacion ~ share_1936_1955:post  + share_1956_1978:post | 
                            mun_code + anio + tipo_eleccion, data = dip_nac_mun_eb, weights = ~ w_ss_1936_3)
summary(att_p2_eb3_ss_36)
  # ATT con el ser 3 pesos del 56
att_p2_eb3_ss_56 <- feols(participacion ~ share_1936_1955:post  + share_1956_1978:post | 
                            mun_code + anio + tipo_eleccion, data = dip_nac_mun_eb, weights = ~ w_ss_1956_3)
summary(att_p2_eb3_ss_56)

  # ATT con el ser 4 pesos del 36
att_p2_eb4_ss_36 <- feols(participacion ~ share_1936_1955:post  + share_1956_1978:post | 
                            mun_code + anio + tipo_eleccion, data = dip_nac_mun_eb, weights = ~ w_ss_1936_4)
summary(att_p2_eb4_ss_36)
  # ATT con el ser 4 pesos del 56
att_p2_eb4_ss_56 <- feols(participacion ~ share_1936_1955:post  + share_1956_1978:post | 
                            mun_code + anio + tipo_eleccion, data = dip_nac_mun_eb, weights = ~ w_ss_1956_4)
summary(att_p2_eb4_ss_56)

modelsummary(
  list(
    "ATT EB 3 (SS, 36-55)" = att_p2_eb3_ss_36,
    "ATT EB 3 (SS, 56-78)" = att_p2_eb3_ss_56,
    
    "ATT EB 4 (SS, 36-55)" = att_p2_eb4_ss_36,
    "ATT EB 4 (SS, 56-78)" = att_p2_eb4_ss_56
  ),
  output = "Output/att_EB_ss_34_participation.xlsx",
  stars = c("*" = 0.10, "**" = 0.05, "***" = 0.01),
  gof_map = data.frame(
    raw = c("r.squared", "nobs"),
    clean = c("R²", "Observations"),
    fmt = c(3, 0)
  ),
  coef_map = c(
    "share_1936_1955:post" = "Spanish share (1936-1955) × Post",
    "post:share_1956_1978" = "Spanish share (1956-1978) × Post"
  ),
  add_rows = tibble::tibble(
    term = c("Municipality FE", "Time FE", "Election type FE"),
    `ATT EB 3 (SS, 36-55)` = c("Yes", "Yes", "Yes"),
    `ATT EB 3 (SS, 56-78)` = c("Yes", "Yes", "Yes"),
    `ATT EB 4 (SS, 36-55)` = c("Yes", "Yes", "Yes"),
    `ATT EB 4 (SS, 56-78)` = c("Yes", "Yes", "Yes")
    
  ),
  notes = "Standard errors clustered at the municipality level in parentheses."
)
}

# ------------------------------------------ #
# 6. Estimaciones con CBGPS
# ------------------------------------------ #

### 6.1 Voto en Blanco ###
{
# Set de pesos 1
event_b2_36_cbgps1 <- feols(porcentaje_blanco ~ i(anio, share_1936_1955, ref = 2021) |
                              mun_code + anio + tipo_eleccion, data = dip_nac_mun_pesos, weights = ~ w_cbgps_36_1)
event_b2_56_cbgps1 <- feols(porcentaje_blanco ~ i(anio, share_1956_1978, ref = 2021) |
                              mun_code + anio + tipo_eleccion, data = dip_nac_mun_pesos, weights = ~ w_cbgps_36_1)

# Set de pesos 2
event_b2_36_cbgps2 <- feols(porcentaje_blanco ~ i(anio, share_1936_1955, ref = 2021) |
                              mun_code + anio + tipo_eleccion, data = dip_nac_mun_pesos, weights = ~ w_cbgps_36_2)
event_b2_56_cbgps2 <- feols(porcentaje_blanco ~ i(anio, share_1956_1978, ref = 2021) |
                              mun_code + anio + tipo_eleccion, data = dip_nac_mun_pesos, weights = ~ w_cbgps_36_2)

# Set de pesos 3
event_b2_36_cbgps3 <- feols(porcentaje_blanco ~ i(anio, share_1936_1955, ref = 2021) |
                              mun_code + anio + tipo_eleccion, data = dip_nac_mun_pesos, weights = ~ w_cbgps_36_3)
event_b2_56_cbgps3 <- feols(porcentaje_blanco ~ i(anio, share_1956_1978, ref = 2021) |
                              mun_code + anio + tipo_eleccion, data = dip_nac_mun_pesos, weights = ~ w_cbgps_36_3)

# Set de pesos 4
event_b2_36_cbgps4 <- feols(porcentaje_blanco ~ i(anio, share_1936_1955, ref = 2021) |
                              mun_code + anio + tipo_eleccion, data = dip_nac_mun_pesos, weights = ~ w_cbgps_36_4)
event_b2_56_cbgps4 <- feols(porcentaje_blanco ~ i(anio, share_1956_1978, ref = 2021) |
                              mun_code + anio + tipo_eleccion, data = dip_nac_mun_pesos, weights = ~ w_cbgps_36_4)

# Set de pesos 5
event_b2_36_cbgps5 <- feols(porcentaje_blanco ~ i(anio, share_1936_1955, ref = 2021) |
                              mun_code + anio + tipo_eleccion, data = dip_nac_mun_pesos, weights = ~ w_cbgps_36_5)
event_b2_56_cbgps5 <- feols(porcentaje_blanco ~ i(anio, share_1956_1978, ref = 2021) |
                              mun_code + anio + tipo_eleccion, data = dip_nac_mun_pesos, weights = ~ w_cbgps_36_5)

# Set de pesos 6
event_b2_36_cbgps6 <- feols(porcentaje_blanco ~ i(anio, share_1936_1955, ref = 2021) |
                              mun_code + anio + tipo_eleccion, data = dip_nac_mun_pesos, weights = ~ w_cbgps_36_6)
event_b2_56_cbgps6 <- feols(porcentaje_blanco ~ i(anio, share_1956_1978, ref = 2021) |
                              mun_code + anio + tipo_eleccion, data = dip_nac_mun_pesos, weights = ~ w_cbgps_36_6)

# Set de pesos 7
event_b2_36_cbgps7 <- feols(porcentaje_blanco ~ i(anio, share_1936_1955, ref = 2021) |
                              mun_code + anio + tipo_eleccion, data = dip_nac_mun_pesos, weights = ~ w_cbgps_36_7)
event_b2_56_cbgps7 <- feols(porcentaje_blanco ~ i(anio, share_1956_1978, ref = 2021) |
                              mun_code + anio + tipo_eleccion, data = dip_nac_mun_pesos, weights = ~ w_cbgps_36_7)

# Set de pesos 8
event_b2_36_cbgps8 <- feols(porcentaje_blanco ~ i(anio, share_1936_1955, ref = 2021) |
                              mun_code + anio + tipo_eleccion, data = dip_nac_mun_pesos, weights = ~ w_cbgps_36_8)
event_b2_56_cbgps8 <- feols(porcentaje_blanco ~ i(anio, share_1956_1978, ref = 2021) |
                              mun_code + anio + tipo_eleccion, data = dip_nac_mun_pesos, weights = ~ w_cbgps_36_8)

# Exporto los resultados
modelsummary(
  list(
    "No CBGPS (36-55)" = event_b2_36,
    "No CBGPS (56-78)" = event_b2_56,
    "CBGPS1 (36-55)" = event_b2_36_cbgps1,
    "CBGPS1 (56-78)" = event_b2_56_cbgps1,
    "CBGPS2 (36-55)" = event_b2_36_cbgps2,
    "CBGPS2 (56-78)" = event_b2_56_cbgps2,
    "CBGPS3 (36-55)" = event_b2_36_cbgps3,
    "CBGPS3 (56-78)" = event_b2_56_cbgps3,
    "CBGPS4 (36-55)" = event_b2_36_cbgps4,
    "CBGPS4 (56-78)" = event_b2_56_cbgps4,
    "CBGPS5 (36-55)" = event_b2_36_cbgps5,
    "CBGPS5 (56-78)" = event_b2_56_cbgps5,
    "CBGPS6 (36-55)" = event_b2_36_cbgps6,
    "CBGPS6 (56-78)" = event_b2_56_cbgps6,
    "CBGPS7 (36-55)" = event_b2_36_cbgps7,
    "CBGPS7 (56-78)" = event_b2_56_cbgps7,
    "CBGPS8 (36-55)" = event_b2_36_cbgps8,
    "CBGPS8 (56-78)" = event_b2_56_cbgps8
  ),
  output = "Output/event_study_CBGPS_blank.xlsx",
  stars = c("*" = 0.10, "**" = 0.05, "***" = 0.01),
  gof_map = data.frame(
    raw = c("r.squared", "nobs"),
    clean = c("R²", "Observations"),
    fmt = c(3, 0)
  ),
  coef_map = c(
    "anio::2011:share_1936_1955" = "2011 × Spanish share (1936-1955)",
    "anio::2013:share_1936_1955" = "2013 × Spanish share (1936-1955)",
    "anio::2015:share_1936_1955" = "2015 × Spanish share (1936-1955)",
    "anio::2017:share_1936_1955" = "2017 × Spanish share (1936-1955)",
    "anio::2019:share_1936_1955" = "2019 × Spanish share (1936-1955)",
    "anio::2023:share_1936_1955" = "2023 × Spanish share (1936-1955)",
    "anio::2025:share_1936_1955" = "2025 × Spanish share (1936-1955)",
    
    "anio::2011:share_1956_1978" = "2011 × Spanish share (1956-1978)",
    "anio::2013:share_1956_1978" = "2013 × Spanish share (1956-1978)",
    "anio::2015:share_1956_1978" = "2015 × Spanish share (1956-1978)",
    "anio::2017:share_1956_1978" = "2017 × Spanish share (1956-1978)",
    "anio::2019:share_1956_1978" = "2019 × Spanish share (1956-1978)",
    "anio::2023:share_1956_1978" = "2023 × Spanish share (1956-1978)",
    "anio::2025:share_1956_1978" = "2025 × Spanish share (1956-1978)"
  ),
  add_rows = tibble::tibble(
    term = c("Municipality FE", "Time FE", "Election type FE"),
    `No CBGPS (36-55)` = c("Yes", "Yes", "Yes"),
    `No CBGPS (56-78)` = c("Yes", "Yes", "Yes"),
    `CBGPS1 (36-55)` = c("Yes", "Yes", "Yes"),
    `CBGPS1 (56-78)` = c("Yes", "Yes", "Yes"),
    `CBGPS2 (36-55)` = c("Yes", "Yes", "Yes"),
    `CBGPS2 (56-78)` = c("Yes", "Yes", "Yes"),
    `CBGPS3 (36-55)` = c("Yes", "Yes", "Yes"),
    `CBGPS3 (56-78)` = c("Yes", "Yes", "Yes"),
    `CBGPS4 (36-55)` = c("Yes", "Yes", "Yes"),
    `CBGPS4 (56-78)` = c("Yes", "Yes", "Yes"),
    `CBGPS5 (36-55)` = c("Yes", "Yes", "Yes"),
    `CBGPS5 (56-78)` = c("Yes", "Yes", "Yes"),
    `CBGPS6 (36-55)` = c("Yes", "Yes", "Yes"),
    `CBGPS6 (56-78)` = c("Yes", "Yes", "Yes"),
    `CBGPS7 (36-55)` = c("Yes", "Yes", "Yes"),
    `CBGPS7 (56-78)` = c("Yes", "Yes", "Yes"),
    `CBGPS8 (36-55)` = c("Yes", "Yes", "Yes"),
    `CBGPS8 (56-78)` = c("Yes", "Yes", "Yes")
  ),
  notes = "Standard errors clustered at the municipality level in parentheses."
)

}

### 6.2 Participación ###
{
# Set de pesos 1
event_p2_36_cbgps1 <- feols(participacion ~ i(anio, share_1936_1955, ref = 2021) |
                              mun_code + anio + tipo_eleccion, data = dip_nac_mun_pesos, weights = ~ w_cbgps_36_1)
event_p2_56_cbgps1 <- feols(participacion ~ i(anio, share_1956_1978, ref = 2021) |
                              mun_code + anio + tipo_eleccion, data = dip_nac_mun_pesos, weights = ~ w_cbgps_36_1)

# Set de pesos 2
event_p2_36_cbgps2 <- feols(participacion ~ i(anio, share_1936_1955, ref = 2021) |
                              mun_code + anio + tipo_eleccion, data = dip_nac_mun_pesos, weights = ~ w_cbgps_36_2)
event_p2_56_cbgps2 <- feols(participacion ~ i(anio, share_1956_1978, ref = 2021) |
                              mun_code + anio + tipo_eleccion, data = dip_nac_mun_pesos, weights = ~ w_cbgps_36_2)

# Set de pesos 3
event_p2_36_cbgps3 <- feols(participacion ~ i(anio, share_1936_1955, ref = 2021) |
                              mun_code + anio + tipo_eleccion, data = dip_nac_mun_pesos, weights = ~ w_cbgps_36_3)
event_p2_56_cbgps3 <- feols(participacion ~ i(anio, share_1956_1978, ref = 2021) |
                              mun_code + anio + tipo_eleccion, data = dip_nac_mun_pesos, weights = ~ w_cbgps_36_3)

# Set de pesos 4
event_p2_36_cbgps4 <- feols(participacion ~ i(anio, share_1936_1955, ref = 2021) |
                              mun_code + anio + tipo_eleccion, data = dip_nac_mun_pesos, weights = ~ w_cbgps_36_4)
event_p2_56_cbgps4 <- feols(participacion ~ i(anio, share_1956_1978, ref = 2021) |
                              mun_code + anio + tipo_eleccion, data = dip_nac_mun_pesos, weights = ~ w_cbgps_36_4)

# Set de pesos 5
event_p2_36_cbgps5 <- feols(participacion ~ i(anio, share_1936_1955, ref = 2021) |
                              mun_code + anio + tipo_eleccion, data = dip_nac_mun_pesos, weights = ~ w_cbgps_36_5)
event_p2_56_cbgps5 <- feols(participacion ~ i(anio, share_1956_1978, ref = 2021) |
                              mun_code + anio + tipo_eleccion, data = dip_nac_mun_pesos, weights = ~ w_cbgps_36_5)

# Set de pesos 6
event_p2_36_cbgps6 <- feols(participacion ~ i(anio, share_1936_1955, ref = 2021) |
                              mun_code + anio + tipo_eleccion, data = dip_nac_mun_pesos, weights = ~ w_cbgps_36_6)
event_p2_56_cbgps6 <- feols(participacion ~ i(anio, share_1956_1978, ref = 2021) |
                              mun_code + anio + tipo_eleccion, data = dip_nac_mun_pesos, weights = ~ w_cbgps_36_6)

# Set de pesos 7
event_p2_36_cbgps7 <- feols(participacion ~ i(anio, share_1936_1955, ref = 2021) |
                              mun_code + anio + tipo_eleccion, data = dip_nac_mun_pesos, weights = ~ w_cbgps_36_7)
event_p2_56_cbgps7 <- feols(participacion ~ i(anio, share_1956_1978, ref = 2021) |
                              mun_code + anio + tipo_eleccion, data = dip_nac_mun_pesos, weights = ~ w_cbgps_36_7)

# Set de pesos 8
event_p2_36_cbgps8 <- feols(participacion ~ i(anio, share_1936_1955, ref = 2021) |
                              mun_code + anio + tipo_eleccion, data = dip_nac_mun_pesos, weights = ~ w_cbgps_36_8)
event_p2_56_cbgps8 <- feols(participacion ~ i(anio, share_1956_1978, ref = 2021) |
                              mun_code + anio + tipo_eleccion, data = dip_nac_mun_pesos, weights = ~ w_cbgps_36_8)

# Exporto los resultados
modelsummary(
  list(
    "No CBGPS (36-55)" = event_p2_36,
    "No CBGPS (56-78)" = event_p2_56,
    "CBGPS1 (36-55)" = event_p2_36_cbgps1,
    "CBGPS1 (56-78)" = event_p2_56_cbgps1,
    "CBGPS2 (36-55)" = event_p2_36_cbgps2,
    "CBGPS2 (56-78)" = event_p2_56_cbgps2,
    "CBGPS3 (36-55)" = event_p2_36_cbgps3,
    "CBGPS3 (56-78)" = event_p2_56_cbgps3,
    "CBGPS4 (36-55)" = event_p2_36_cbgps4,
    "CBGPS4 (56-78)" = event_p2_56_cbgps4,
    "CBGPS5 (36-55)" = event_p2_36_cbgps5,
    "CBGPS5 (56-78)" = event_p2_56_cbgps5,
    "CBGPS6 (36-55)" = event_p2_36_cbgps6,
    "CBGPS6 (56-78)" = event_p2_56_cbgps6,
    "CBGPS7 (36-55)" = event_p2_36_cbgps7,
    "CBGPS7 (56-78)" = event_p2_56_cbgps7,
    "CBGPS8 (36-55)" = event_p2_36_cbgps8,
    "CBGPS8 (56-78)" = event_p2_56_cbgps8
  ),
  output = "Output/event_study_CBGPS_participation.xlsx",
  stars = c("*" = 0.10, "**" = 0.05, "***" = 0.01),
  gof_map = data.frame(
    raw = c("r.squared", "nobs"),
    clean = c("R²", "Observations"),
    fmt = c(3, 0)
  ),
  coef_map = c(
    "anio::2011:share_1936_1955" = "2011 × Spanish share (1936-1955)",
    "anio::2013:share_1936_1955" = "2013 × Spanish share (1936-1955)",
    "anio::2015:share_1936_1955" = "2015 × Spanish share (1936-1955)",
    "anio::2017:share_1936_1955" = "2017 × Spanish share (1936-1955)",
    "anio::2019:share_1936_1955" = "2019 × Spanish share (1936-1955)",
    "anio::2023:share_1936_1955" = "2023 × Spanish share (1936-1955)",
    "anio::2025:share_1936_1955" = "2025 × Spanish share (1936-1955)",
    
    "anio::2011:share_1956_1978" = "2011 × Spanish share (1956-1978)",
    "anio::2013:share_1956_1978" = "2013 × Spanish share (1956-1978)",
    "anio::2015:share_1956_1978" = "2015 × Spanish share (1956-1978)",
    "anio::2017:share_1956_1978" = "2017 × Spanish share (1956-1978)",
    "anio::2019:share_1956_1978" = "2019 × Spanish share (1956-1978)",
    "anio::2023:share_1956_1978" = "2023 × Spanish share (1956-1978)",
    "anio::2025:share_1956_1978" = "2025 × Spanish share (1956-1978)"
  ),
  add_rows = tibble::tibble(
    term = c("Municipality FE", "Time FE", "Election type FE"),
    `No CBGPS (36-55)` = c("Yes", "Yes", "Yes"),
    `No CBGPS (56-78)` = c("Yes", "Yes", "Yes"),
    `CBGPS1 (36-55)` = c("Yes", "Yes", "Yes"),
    `CBGPS1 (56-78)` = c("Yes", "Yes", "Yes"),
    `CBGPS2 (36-55)` = c("Yes", "Yes", "Yes"),
    `CBGPS2 (56-78)` = c("Yes", "Yes", "Yes"),
    `CBGPS3 (36-55)` = c("Yes", "Yes", "Yes"),
    `CBGPS3 (56-78)` = c("Yes", "Yes", "Yes"),
    `CBGPS4 (36-55)` = c("Yes", "Yes", "Yes"),
    `CBGPS4 (56-78)` = c("Yes", "Yes", "Yes"),
    `CBGPS5 (36-55)` = c("Yes", "Yes", "Yes"),
    `CBGPS5 (56-78)` = c("Yes", "Yes", "Yes"),
    `CBGPS6 (36-55)` = c("Yes", "Yes", "Yes"),
    `CBGPS6 (56-78)` = c("Yes", "Yes", "Yes"),
    `CBGPS7 (36-55)` = c("Yes", "Yes", "Yes"),
    `CBGPS7 (56-78)` = c("Yes", "Yes", "Yes"),
    `CBGPS8 (36-55)` = c("Yes", "Yes", "Yes"),
    `CBGPS8 (56-78)` = c("Yes", "Yes", "Yes")
  ),
  notes = "Standard errors clustered at the municipality level in parentheses."
)
}

### 6.3 Exporto los att de los mejores set ###
{
  # cbgps participación 1936: 3, 4, o 6
  # cbgps participación 1956: mejores 4 o 5 , si no 1, 2, o 3
  # cbgps votos en blanco 1936: mejores 6 o 7, si no 1-5 
  # cbgps votos en blanco 1956: mejores 3, 4, 7 u 8, si no 1, 2, o 5
# ==> Exporto 3, 4

# Voto en blanco
  # ATT con el set 3 pesos del 36
att_b2_cbgps3_36 <- feols(porcentaje_blanco ~ share_1936_1955:post  + share_1956_1978:post | 
                            mun_code + anio + tipo_eleccion, data = dip_nac_mun_pesos, weights = ~ w_cbgps_36_3)
summary(att_b2_cbgps3_36)
  # ATT con el set 3 pesos del 56
att_b2_cbgps3_56 <- feols(porcentaje_blanco ~ share_1936_1955:post  + share_1956_1978:post | 
                            mun_code + anio + tipo_eleccion, data = dip_nac_mun_pesos, weights = ~ w_cbgps_56_3)
summary(att_b2_cbgps3_56)

  # ATT con el set 4 pesos del 36
att_b2_cbgps4_36 <- feols(porcentaje_blanco ~ share_1936_1955:post  + share_1956_1978:post | 
                            mun_code + anio + tipo_eleccion, data = dip_nac_mun_pesos, weights = ~ w_cbgps_36_4)
summary(att_b2_cbgps4_36)
  # ATT con el set 3 pesos del 56
att_b2_cbgps4_56 <- feols(porcentaje_blanco ~ share_1936_1955:post  + share_1956_1978:post | 
                            mun_code + anio + tipo_eleccion, data = dip_nac_mun_pesos, weights = ~ w_cbgps_56_4)
summary(att_b2_cbgps4_56)

modelsummary(
  list(
    "ATT CBGPS 3 (36-55)" = att_b2_cbgps3_36,
    "ATT CBGPS 3 (56-78)" = att_b2_cbgps3_56,
    
    "ATT CBGPS 4 (36-55)" = att_b2_cbgps4_36,
    "ATT CBGPS 4 (56-78)" = att_b2_cbgps4_56
  ),
  output = "Output/att_CBGPS_34_blankvotes.xlsx",
  stars = c("*" = 0.10, "**" = 0.05, "***" = 0.01),
  gof_map = data.frame(
    raw = c("r.squared", "nobs"),
    clean = c("R²", "Observations"),
    fmt = c(3, 0)
  ),
  coef_map = c(
    "share_1936_1955:post" = "Spanish share (1936-1955) × Post",
    "post:share_1956_1978" = "Spanish share (1956-1978) × Post"
  ),
  add_rows = tibble::tibble(
    term = c("Municipality FE", "Time FE", "Election type FE"),
    `ATT CBGPS 3 (36-55)` = c("Yes", "Yes", "Yes"),
    `ATT CBGPS 3 (56-78)` = c("Yes", "Yes", "Yes"),
    `ATT CBGPS 4 (36-55)` = c("Yes", "Yes", "Yes"),
    `ATT CBGPS 4 (56-78)` = c("Yes", "Yes", "Yes")
  ),
  notes = "Standard errors clustered at the municipality level in parentheses."
)

# Participación

  # ATT con el set 3 pesos del 36
att_p2_cbgps3_36 <- feols(participacion ~ share_1936_1955:post  + share_1956_1978:post | 
                            mun_code + anio + tipo_eleccion, data = dip_nac_mun_pesos, weights = ~ w_cbgps_36_3)
summary(att_p2_cbgps3_36)
  # ATT con el set 3 pesos del 56
att_p2_cbgps3_56 <- feols(participacion ~ share_1936_1955:post  + share_1956_1978:post | 
                            mun_code + anio + tipo_eleccion, data = dip_nac_mun_pesos, weights = ~ w_cbgps_56_3)
summary(att_p2_cbgps3_56)

  # ATT con el set 4 pesos del 36
att_p2_cbgps4_36 <- feols(participacion ~ share_1936_1955:post  + share_1956_1978:post | 
                            mun_code + anio + tipo_eleccion, data = dip_nac_mun_pesos, weights = ~ w_cbgps_36_4)
summary(att_p2_cbgps4_36)
  # ATT con el set 4 pesos del 56
att_p2_cbgps4_56 <- feols(participacion ~ share_1936_1955:post  + share_1956_1978:post | 
                            mun_code + anio + tipo_eleccion, data = dip_nac_mun_pesos, weights = ~ w_cbgps_56_4)
summary(att_p2_cbgps4_56)

modelsummary(
  list(
    "ATT CBGPS 3 (36-55)" = att_p2_cbgps3_36,
    "ATT CBGPS 3 (56-78)" = att_p2_cbgps3_56,
    
    "ATT CBGPS 4 (36-55)" = att_p2_cbgps4_36,
    "ATT CBGPS 4 (56-78)" = att_p2_cbgps4_56
  ),
  output = "Output/att_CBGPS_34_participation.xlsx",
  stars = c("*" = 0.10, "**" = 0.05, "***" = 0.01),
  gof_map = data.frame(
    raw = c("r.squared", "nobs"),
    clean = c("R²", "Observations"),
    fmt = c(3, 0)
  ),
  coef_map = c(
    "share_1936_1955:post" = "Spanish share (1936-1955) × Post",
    "post:share_1956_1978" = "Spanish share (1956-1978) × Post"
  ),
  add_rows = tibble::tibble(
    term = c("Municipality FE", "Time FE", "Election type FE"),
    `ATT CBGPS 3 (36-55)` = c("Yes", "Yes", "Yes"),
    `ATT CBGPS 3 (56-78)` = c("Yes", "Yes", "Yes"),
    `ATT CBGPS 4 (36-55)` = c("Yes", "Yes", "Yes"),
    `ATT CBGPS 4 (56-78)` = c("Yes", "Yes", "Yes")
  ),
  notes = "Standard errors clustered at the municipality level in parentheses."
)
}

# ------------------------------------------ #
# 7. Efectos heterogéneos
# ------------------------------------------ #

### 7.1 Voto en Blanco ###
{

# Densidad de población

  # Divido en sub-muestras en base a terciles de densidad de población
att_b2_dt1 <- feols(porcentaje_blanco ~ share_1936_1955:post + share_1956_1978:post | 
                      mun_code + anio + tipo_eleccion, data = dip_nac_mun %>% 
                      filter(t_density_2010 == 1))
att_b2_dt2 <- feols(porcentaje_blanco ~ share_1936_1955:post + share_1956_1978:post | 
                      mun_code + anio + tipo_eleccion, data = dip_nac_mun %>% 
                      filter(t_density_2010 == 2))
att_b2_dt3 <- feols(porcentaje_blanco ~ share_1936_1955:post + share_1956_1978:post | 
                      mun_code + anio + tipo_eleccion, data = dip_nac_mun %>% 
                      filter(t_density_2010 == 3))

# Urbanización:
  
  # Urbanización y share de españoles centrados
dip_nac_mun <- dip_nac_mun %>% 
  mutate(pct_urb_c = pct_urb_2001 - mean(pct_urb_2001, na.rm = TRUE),
         share_36_c      = share_1936_1955 - mean(share_1936_1955, na.rm = TRUE),
         share_56_c      = share_1956_1978 - mean(share_1956_1978, na.rm = TRUE))

  # Ambas variables continuas centradas
feols(porcentaje_blanco ~ share_36_c:post + share_56_c:post + post:pct_urb_c +
        share_36_c:post:pct_urb_c + share_56_c:post:pct_urb_c |
        mun_code + anio + tipo_eleccion, data = dip_nac_mun, cluster = ~mun_code)
  # Variables continuas sin centrar
feols(porcentaje_blanco ~ share_1936_1955:post + share_1956_1978:post + post:pct_urb_2001 +
        share_1936_1955:post:pct_urb_2001 + share_1956_1978:post:pct_urb_2001 |
        mun_code + anio + tipo_eleccion, data = dip_nac_mun, cluster = ~mun_code)

  # Divido en sub-muestras en base a mayormente urbanos
feols(porcentaje_blanco ~ share_1936_1955:post + share_1956_1978:post | 
        mun_code + anio + tipo_eleccion, data = dip_nac_mun %>% 
        filter(mayormente_urb_2001 == 1))
feols(porcentaje_blanco ~ share_1936_1955:post + share_1956_1978:post | 
        mun_code + anio + tipo_eleccion, data = dip_nac_mun %>% 
        filter(mayormente_urb_2001 == 0)) # no da el power
  
  # Divido en sub-muestras en base a terciles de urbanización
att_b2_ut1 <- feols(porcentaje_blanco ~ share_1936_1955:post + share_1956_1978:post | 
                      mun_code + anio + tipo_eleccion, data = dip_nac_mun %>% 
                      filter(t_urb_2001 == 1))
att_b2_ut2 <- feols(porcentaje_blanco ~ share_1936_1955:post + share_1956_1978:post | 
                      mun_code + anio + tipo_eleccion, data = dip_nac_mun %>% 
                      filter(t_urb_2001 == 2))
att_b2_ut3 <- feols(porcentaje_blanco ~ share_1936_1955:post + share_1956_1978:post | 
                      mun_code + anio + tipo_eleccion, data = dip_nac_mun %>% 
                      filter(t_urb_2001 == 3))

  # Divido en sub-muestras en base a terciles de ruralidad
att_b2_rt1 <- feols(porcentaje_blanco ~ share_1936_1955:post + share_1956_1978:post | 
                      mun_code + anio + tipo_eleccion, data = dip_nac_mun %>% 
                      filter(t_rur_2001 == 1))
att_b2_rt2 <- feols(porcentaje_blanco ~ share_1936_1955:post + share_1956_1978:post | 
                      mun_code + anio + tipo_eleccion, data = dip_nac_mun %>% 
                      filter(t_rur_2001 == 2))
att_b2_rt3 <- feols(porcentaje_blanco ~ share_1936_1955:post + share_1956_1978:post | 
                      mun_code + anio + tipo_eleccion, data = dip_nac_mun %>% 
                      filter(t_rur_2001 == 3))

  # Event study por terciles de urbanización
event_b2_36_urb <- lapply(1:3, function(t) {
  feols(porcentaje_blanco ~ i(anio, share_1936_1955, ref = 2021) |
          mun_code + anio + tipo_eleccion,
        data = dip_nac_mun %>% filter(tercil_urb_2001 == t),
        cluster = ~mun_code)
})
names(event_b2_36_urb) <- c("T1", "T2", "T3")
iplot(event_b2_36_urb$T1)
iplot(event_b2_36_urb$T2)
iplot(event_b2_36_urb$T3)
summary(event_b2_36_urb$T1)

event_b2_56_urb <- lapply(1:3, function(t) {
  feols(porcentaje_blanco ~ i(anio, share_1956_1978, ref = 2021) |
          mun_code + anio + tipo_eleccion,
        data = dip_nac_mun %>% filter(tercil_urb_2001 == t),
        cluster = ~mun_code)
})
names(event_b2_56_urb) <- c("T1", "T2", "T3")
iplot(event_b2_56_urb$T1)
iplot(event_b2_56_urb$T2)
iplot(event_b2_56_urb$T3)

# Sexo:

  # Share female centrado
dip_nac_mun <- dip_nac_mun %>% 
  mutate(share_female_2010_c = share_female_2010 - mean(share_female_2010, na.rm = TRUE))

  # Ambas variables contunuas centradas
feols(porcentaje_blanco ~ share_36_c:post + share_56_c:post + post:share_female_2010_c +
        share_36_c:post:share_female_2010_c + share_56_c:post:share_female_2010_c |
        mun_code + anio + tipo_eleccion, data = dip_nac_mun, cluster = ~mun_code)
  # Variables continuas sin centrar
feols(porcentaje_blanco ~ share_1936_1955:post + share_1956_1978:post + post:share_female_2010 +
        share_1936_1955:post:share_female_2010 + share_1956_1978:post:share_female_2010 |
        mun_code + anio + tipo_eleccion, data = dip_nac_mun, cluster = ~mun_code)

  # Divido en submuestras en base a terciles de female
att_b2_ft1 <- feols(porcentaje_blanco ~ share_1936_1955:post + share_1956_1978:post | 
                      mun_code + anio + tipo_eleccion, data = dip_nac_mun %>% 
                      filter(t_fem_2010 == 1))
att_b2_ft2 <- feols(porcentaje_blanco ~ share_1936_1955:post + share_1956_1978:post | 
                      mun_code + anio + tipo_eleccion, data = dip_nac_mun %>% 
                      filter(t_fem_2010 == 2))
att_b2_ft3 <- feols(porcentaje_blanco ~ share_1936_1955:post + share_1956_1978:post | 
                      mun_code + anio + tipo_eleccion, data = dip_nac_mun %>% 
                      filter(t_fem_2010 == 3))

  # Divido en submuestras en base a terciles de hombres
feols(porcentaje_blanco ~ share_1936_1955:post + share_1956_1978:post | 
        mun_code + anio + tipo_eleccion, data = dip_nac_mun %>% 
        filter(t_male_2010 == 1))
feols(porcentaje_blanco ~ share_1936_1955:post + share_1956_1978:post | 
        mun_code + anio + tipo_eleccion, data = dip_nac_mun %>% 
        filter(t_male_2010 == 2))
feols(porcentaje_blanco ~ share_1936_1955:post + share_1956_1978:post | 
        mun_code + anio + tipo_eleccion, data = dip_nac_mun %>% 
        filter(t_male_2010 == 3))

# Escolaridad:

  # Divido en submuestras en base a terciles de share less primary
feols(porcentaje_blanco ~ share_1936_1955:post + share_1956_1978:post | 
        mun_code + anio + tipo_eleccion, data = dip_nac_mun %>% 
        filter(t_lessp_2010 == 1))
feols(porcentaje_blanco ~ share_1936_1955:post + share_1956_1978:post | 
      mun_code + anio + tipo_eleccion, data = dip_nac_mun %>% 
        filter(t_prim_2010 == 2))
feols(porcentaje_blanco ~ share_1936_1955:post + share_1956_1978:post | 
        mun_code + anio + tipo_eleccion, data = dip_nac_mun %>% 
        filter(t_prim_2010 == 3))

  # Divido en submuestras en base a terciles de share primary
feols(porcentaje_blanco ~ share_1936_1955:post + share_1956_1978:post | 
        mun_code + anio + tipo_eleccion, data = dip_nac_mun %>% 
        filter(t_prim_2010 == 1))
feols(porcentaje_blanco ~ share_1936_1955:post + share_1956_1978:post | 
        mun_code + anio + tipo_eleccion, data = dip_nac_mun %>% 
        filter(t_prim_2010 == 2))
feols(porcentaje_blanco ~ share_1936_1955:post + share_1956_1978:post | 
        mun_code + anio + tipo_eleccion, data = dip_nac_mun %>% 
        filter(t_prim_2010 == 3))

  # Divido en submuestras en base a terciles de share secondary
feols(porcentaje_blanco ~ share_1936_1955:post + share_1956_1978:post | 
        mun_code + anio + tipo_eleccion, data = dip_nac_mun %>% 
        filter(t_sec_2010 == 1))
feols(porcentaje_blanco ~ share_1936_1955:post + share_1956_1978:post | 
        mun_code + anio + tipo_eleccion, data = dip_nac_mun %>% 
        filter(t_sec_2010 == 2))
feols(porcentaje_blanco ~ share_1936_1955:post + share_1956_1978:post | 
        mun_code + anio + tipo_eleccion, data = dip_nac_mun %>% 
        filter(t_sec_2010 == 3))
      # Es medio ambiguo si es malo o bueno estar en el primer tercil

  # Divido en submuestras en base a terciles de share university
feols(porcentaje_blanco ~ share_1936_1955:post + share_1956_1978:post | 
        mun_code + anio + tipo_eleccion, data = dip_nac_mun %>% 
        filter(t_uni_2010 == 1))
feols(porcentaje_blanco ~ share_1936_1955:post + share_1956_1978:post | 
        mun_code + anio + tipo_eleccion, data = dip_nac_mun %>% 
        filter(t_uni_2010 == 2))
feols(porcentaje_blanco ~ share_1936_1955:post + share_1956_1978:post | 
        mun_code + anio + tipo_eleccion, data = dip_nac_mun %>% 
        filter(t_uni_2010 == 3))

  # Divido en submuestras en base a terciles de la mediana de los años de educ
feols(porcentaje_blanco ~ share_1936_1955:post + share_1956_1978:post | 
        mun_code + anio + tipo_eleccion, data = dip_nac_mun %>% 
        filter(t_med_schyr_2010 == 1))
feols(porcentaje_blanco ~ share_1936_1955:post + share_1956_1978:post | 
        mun_code + anio + tipo_eleccion, data = dip_nac_mun %>% 
        filter(t_med_schyr_2010 == 2))
feols(porcentaje_blanco ~ share_1936_1955:post + share_1956_1978:post | 
        mun_code + anio + tipo_eleccion, data = dip_nac_mun %>% 
        filter(t_med_schyr_2010 == 3))

  # Divido en submuestras en base a terciles del promedio de los años de educ
att_b2_meanedt1 <- feols(porcentaje_blanco ~ share_1936_1955:post + share_1956_1978:post | 
                             mun_code + anio + tipo_eleccion, data = dip_nac_mun %>% 
                             filter(t_mean_schyr_2010 == 1))
att_b2_meanedt2 <- feols(porcentaje_blanco ~ share_1936_1955:post + share_1956_1978:post | 
                             mun_code + anio + tipo_eleccion, data = dip_nac_mun %>% 
                             filter(t_mean_schyr_2010 == 2))
att_b2_meanedt3 <- feols(porcentaje_blanco ~ share_1936_1955:post + share_1956_1978:post | 
                             mun_code + anio + tipo_eleccion, data = dip_nac_mun %>% 
                             filter(t_mean_schyr_2010 == 3))

# Edad: 

  # Divido en sub-muestras en base a terciles de la mediana de la edad
att_b2_at1 <- feols(porcentaje_blanco ~ share_1936_1955:post + share_1956_1978:post | 
                      mun_code + anio + tipo_eleccion, data = dip_nac_mun %>% 
                      filter(t_med_dage_2010 == 1))
att_b2_at2 <- feols(porcentaje_blanco ~ share_1936_1955:post + share_1956_1978:post | 
                      mun_code + anio + tipo_eleccion, data = dip_nac_mun %>% 
                      filter(t_med_dage_2010 == 2))
att_b2_at3 <- feols(porcentaje_blanco ~ share_1936_1955:post + share_1956_1978:post | 
                      mun_code + anio + tipo_eleccion, data = dip_nac_mun %>% 
                      filter(t_med_dage_2010 == 3))

  # Divido en sub-muestras en base a terciles del share de personas entre 25 y 44 años
feols(porcentaje_blanco ~ share_1936_1955:post + share_1956_1978:post | 
        mun_code + anio + tipo_eleccion, data = dip_nac_mun %>% 
        filter(t_age2544_2010 == 1))
feols(porcentaje_blanco ~ share_1936_1955:post + share_1956_1978:post | 
        mun_code + anio + tipo_eleccion, data = dip_nac_mun %>% 
        filter(t_age2544_2010 == 2))
feols(porcentaje_blanco ~ share_1936_1955:post + share_1956_1978:post | 
        mun_code + anio + tipo_eleccion, data = dip_nac_mun %>% 
        filter(t_age2544_2010 == 3))

dip_nac_mun %>%
  group_by(t_med_dage_2010) %>%
  summarise(
    n = n(),
    min_median_age = min(median_age_2010, na.rm = TRUE),
    mean_median_age = mean(median_age_2010, na.rm = TRUE),
    max_median_age = max(median_age_2010, na.rm = TRUE)
  )

# Indicador económico

  # Divido en sub-muestras en base a terciles del share de personas desempleadas
feols(porcentaje_blanco ~ share_1936_1955:post + share_1956_1978:post | 
        mun_code + anio + tipo_eleccion, data = dip_nac_mun %>% 
        filter(t_unemp_2010 == 1))
feols(porcentaje_blanco ~ share_1936_1955:post + share_1956_1978:post | 
        mun_code + anio + tipo_eleccion, data = dip_nac_mun %>% 
        filter(t_unemp_2010 == 2))
feols(porcentaje_blanco ~ share_1936_1955:post + share_1956_1978:post | 
        mun_code + anio + tipo_eleccion, data = dip_nac_mun %>% 
        filter(t_unemp_2010 == 3))

# Características políticas

  # Divido en sub-muestras en base a terciles del share de votos a la izquierda amplia
att_b2_izt1 <- feols(porcentaje_blanco ~ share_1936_1955:post + share_1956_1978:post | 
                       mun_code + anio + tipo_eleccion, data = dip_nac_mun %>% 
                       filter(t_izam_pre_avg == 1))
att_b2_izt2 <- feols(porcentaje_blanco ~ share_1936_1955:post + share_1956_1978:post | 
                       mun_code + anio + tipo_eleccion, data = dip_nac_mun %>% 
                       filter(t_izam_pre_avg == 2))
att_b2_izt3 <- feols(porcentaje_blanco ~ share_1936_1955:post + share_1956_1978:post | 
                       mun_code + anio + tipo_eleccion, data = dip_nac_mun %>% 
                       filter(t_izam_pre_avg == 3))

  # Divido en sub-muestras en base a terciles del share de votos a la derecha amplia
feols(porcentaje_blanco ~ share_1936_1955:post + share_1956_1978:post | 
        mun_code + anio + tipo_eleccion, data = dip_nac_mun %>% 
        filter(t_deam_pre_avg == 1))
feols(porcentaje_blanco ~ share_1936_1955:post + share_1956_1978:post | 
        mun_code + anio + tipo_eleccion, data = dip_nac_mun %>% 
        filter(t_deam_pre_avg == 2))
feols(porcentaje_blanco ~ share_1936_1955:post + share_1956_1978:post | 
        mun_code + anio + tipo_eleccion, data = dip_nac_mun %>% 
        filter(t_deam_pre_avg == 3))

  # Divido en sub-muestras en base a terciles del share de votos al peronismo
feols(porcentaje_blanco ~ share_1936_1955:post + share_1956_1978:post | 
        mun_code + anio + tipo_eleccion, data = dip_nac_mun %>% 
        filter(t_per_pre_avg == 1))
feols(porcentaje_blanco ~ share_1936_1955:post + share_1956_1978:post | 
        mun_code + anio + tipo_eleccion, data = dip_nac_mun %>% 
        filter(t_per_pre_avg == 2))
feols(porcentaje_blanco ~ share_1936_1955:post + share_1956_1978:post | 
        mun_code + anio + tipo_eleccion, data = dip_nac_mun %>% 
        filter(t_per_pre_avg == 3))

  # Divido en sub-muestras en base a terciles del share de votos al oficialismo
feols(porcentaje_blanco ~ share_1936_1955:post + share_1956_1978:post | 
        mun_code + anio + tipo_eleccion, data = dip_nac_mun %>% 
        filter(t_ofi_pre_avg == 1))
feols(porcentaje_blanco ~ share_1936_1955:post + share_1956_1978:post | 
        mun_code + anio + tipo_eleccion, data = dip_nac_mun %>% 
        filter(t_ofi_pre_avg == 2))
feols(porcentaje_blanco ~ share_1936_1955:post + share_1956_1978:post | 
        mun_code + anio + tipo_eleccion, data = dip_nac_mun %>% 
        filter(t_ofi_pre_avg == 3))

  # Divido en sub-muestras en base a terciles de alternancia
att_b2_alt1 <- feols(porcentaje_blanco ~ share_1936_1955:post + share_1956_1978:post | 
                      mun_code + anio + tipo_eleccion, data = dip_nac_mun %>% 
                      filter(t_alt_pre_avg == 1))
att_b2_alt2 <- feols(porcentaje_blanco ~ share_1936_1955:post + share_1956_1978:post | 
                       mun_code + anio + tipo_eleccion, data = dip_nac_mun %>% 
                       filter(t_alt_pre_avg == 2))
att_b2_alt3 <- feols(porcentaje_blanco ~ share_1936_1955:post + share_1956_1978:post | 
                       mun_code + anio + tipo_eleccion, data = dip_nac_mun %>% 
                       filter(t_alt_pre_avg == 3))

# Conclusión: efectos más fuertes en municipios 
# - con menos densidad de población, con más mujeres, más educados, con gente más jóven.
# - más de izquierda (izquierda = gente con más movilización politica), con menor alternancia (hay un partido fuerte como alternativa al voto blanco)
}

### 7.2 Participación ###
{
 
# Densidad de población 
  
  # Divido en sub-muestras en base a terciles de urbanización
att_p2_dt1 <- feols(participacion ~ share_1936_1955:post + share_1956_1978:post | 
                      mun_code + anio + tipo_eleccion, data = dip_nac_mun %>% 
                      filter(t_density_2010 == 1))
att_p2_dt2 <- feols(participacion ~ share_1936_1955:post + share_1956_1978:post | 
                      mun_code + anio + tipo_eleccion, data = dip_nac_mun %>% 
                      filter(t_density_2010 == 2))
att_p2_dt3 <- feols(participacion ~ share_1936_1955:post + share_1956_1978:post | 
                      mun_code + anio + tipo_eleccion, data = dip_nac_mun %>% 
                      filter(t_density_2010 == 3))
  
# Urbanización:

  # Divido en sub-muestras en base a terciles de urbanización
att_p2_ut1 <- feols(participacion ~ share_1936_1955:post + share_1956_1978:post | 
                      mun_code + anio + tipo_eleccion, data = dip_nac_mun %>% 
                      filter(t_urb_2001 == 1))
att_p2_ut2 <- feols(participacion ~ share_1936_1955:post + share_1956_1978:post | 
                      mun_code + anio + tipo_eleccion, data = dip_nac_mun %>% 
                      filter(t_urb_2001 == 2))
att_p2_ut3 <- feols(participacion ~ share_1936_1955:post + share_1956_1978:post | 
                      mun_code + anio + tipo_eleccion, data = dip_nac_mun %>% 
                      filter(t_urb_2001 == 3))

# Sexo:

  # Divido en submuestras en base a terciles de female
att_p2_ft1 <- feols(participacion ~ share_1936_1955:post + share_1956_1978:post | 
                      mun_code + anio + tipo_eleccion, data = dip_nac_mun %>% 
                      filter(t_fem_2010 == 1))
att_p2_ft2 <- feols(participacion ~ share_1936_1955:post + share_1956_1978:post | 
                      mun_code + anio + tipo_eleccion, data = dip_nac_mun %>% 
                      filter(t_fem_2010 == 2))
att_p2_ft3 <- feols(participacion ~ share_1936_1955:post + share_1956_1978:post | 
                      mun_code + anio + tipo_eleccion, data = dip_nac_mun %>% 
                      filter(t_fem_2010 == 3))

# Educación:

  # Divido en sub-muestras en base a terciles del promedio de los años de educ
att_p2_meanedt1 <- feols(participacion ~ share_1936_1955:post + share_1956_1978:post | 
                             mun_code + anio + tipo_eleccion, data = dip_nac_mun %>% 
                             filter(t_mean_schyr_2010 == 1))
att_p2_meanedt2 <- feols(participacion ~ share_1936_1955:post + share_1956_1978:post | 
                             mun_code + anio + tipo_eleccion, data = dip_nac_mun %>% 
                             filter(t_mean_schyr_2010 == 2))
att_p2_meanedt3 <- feols(participacion ~ share_1936_1955:post + share_1956_1978:post | 
                             mun_code + anio + tipo_eleccion, data = dip_nac_mun %>% 
                             filter(t_mean_schyr_2010 == 3))
# Edad

  # Divido en sub-muestras en base a terciles de la mediana de la edad
att_p2_at1 <- feols(participacion ~ share_1936_1955:post + share_1956_1978:post | 
                      mun_code + anio + tipo_eleccion, data = dip_nac_mun %>% 
                      filter(t_med_dage_2010 == 1))
att_p2_at2 <- feols(participacion ~ share_1936_1955:post + share_1956_1978:post | 
                      mun_code + anio + tipo_eleccion, data = dip_nac_mun %>% 
                      filter(t_med_dage_2010 == 2))
att_p2_at3 <- feols(participacion ~ share_1936_1955:post + share_1956_1978:post | 
                      mun_code + anio + tipo_eleccion, data = dip_nac_mun %>% 
                      filter(t_med_dage_2010 == 3))

# Características políticas

  # Divido en sub-muestras en base a terciles del share de votos a la izquierda amplia
att_p2_izt1 <- feols(participacion ~ share_1936_1955:post + share_1956_1978:post | 
                       mun_code + anio + tipo_eleccion, data = dip_nac_mun %>% 
                       filter(t_izam_pre_avg == 1))
att_p2_izt2 <- feols(participacion ~ share_1936_1955:post + share_1956_1978:post | 
                       mun_code + anio + tipo_eleccion, data = dip_nac_mun %>% 
                       filter(t_izam_pre_avg == 2))
att_p2_izt3 <- feols(participacion ~ share_1936_1955:post + share_1956_1978:post | 
                       mun_code + anio + tipo_eleccion, data = dip_nac_mun %>% 
                       filter(t_izam_pre_avg == 3))

  # Divido en sub-muestras en base a terciles de alternancia
att_p2_alt1 <- feols(participacion ~ share_1936_1955:post + share_1956_1978:post | 
                       mun_code + anio + tipo_eleccion, data = dip_nac_mun %>% 
                       filter(t_alt_pre_avg == 1))
att_p2_alt2 <- feols(participacion ~ share_1936_1955:post + share_1956_1978:post | 
                       mun_code + anio + tipo_eleccion, data = dip_nac_mun %>% 
                       filter(t_alt_pre_avg == 2))
att_p2_alt3 <- feols(participacion ~ share_1936_1955:post + share_1956_1978:post | 
                       mun_code + anio + tipo_eleccion, data = dip_nac_mun %>% 
                       filter(t_alt_pre_avg == 3))

# Conclusión: efectos más fuertes en municipios MÁS urbanizados,con MÁS mujeres, no es tan claro que pasa con educ, gente MÁS jóven.
}

### 7.4 Exporto los resultados ###
{
## Tabla para votos en blanco

make_panel_tabular <- function(models, group_names, group_sizes, panel_label) {
  cm <- c("share_1936_1955:post" = "Spanish share 1936-1955$\\times$Post",
          "post:share_1956_1978" = "Spanish share 1956-1978$\\times$Post")
  gm <- tibble::tribble(
    ~raw,        ~clean,         ~fmt,
    "nobs",      "Observations", 0,
    "r.squared", "R$^2$",        3)
  
  # Calcular p-valor del test de igualdad para cada modelo
  p_values  <- sapply(models, get_p_equal)
  p_strings <- sprintf("%.3f", p_values)
  
  # Combinar fila de p-valores + filas de FE en un solo bloque
  yes_mat  <- matrix("Yes", nrow = 3, ncol = length(models))
  data_mat <- rbind(p_strings, yes_mat)
  
  add_rows <- cbind(
    data.frame(term = c("$p$-value ($\\beta_{36{-}55} = \\beta_{56{-}78}$)",
                        "Year FE", "Municipality FE", "Election Type FE"),
               stringsAsFactors = FALSE),
    as.data.frame(data_mat, stringsAsFactors = FALSE))
  names(add_rows) <- c("term", names(models))
  
  tex <- modelsummary(
    models, output = "latex",
    coef_map = cm, gof_map = gm,
    estimate = "{estimate}{stars}", statistic = "({std.error})",
    stars = c("*" = .10, "**" = .05, "***" = .01),
    add_rows = add_rows, escape = FALSE)
  
  lines <- strsplit(tex, "\n")[[1]]
  ncols <- 1 + length(models)
  
  # Header agrupado
  top_idx <- grep("\\\\toprule", lines)
  if (length(top_idx) >= 1) {
    mc_parts <- paste0("\\multicolumn{", group_sizes, "}{c}{", group_names, "}")
    multicol <- paste0(" & ", paste(mc_parts, collapse = " & "), " \\\\")
    cmids <- c(); pos <- 2
    for (gs in group_sizes) {
      cmids <- c(cmids, paste0("\\cmidrule(l){", pos, "-", pos + gs - 1, "}"))
      pos <- pos + gs
    }
    cmidrule <- paste(cmids, collapse = " ")
    lines <- c(lines[1:top_idx[1]], multicol, cmidrule,
               lines[(top_idx[1] + 1):length(lines)])
  }
  
  # booktabs -> \hline
  lines <- gsub("\\\\toprule",    "\\\\hline", lines)
  lines <- gsub("\\\\bottomrule", "\\\\hline", lines)
  
  # Midrules: el primero a \hline; el segundo eliminar
  mr <- grep("\\\\midrule", lines)
  if (length(mr) >= 1) lines[mr[1]] <- gsub("\\\\midrule", "\\\\hline", lines[mr[1]])
  if (length(mr) >= 2) for (i in mr[-1]) lines[i] <- ""
  
  # Fila vacía arriba del obs.
  obs <- grep("Observations", lines)
  if (length(obs) >= 1) {
    empty <- paste0(strrep(" &", ncols - 1), " \\\\")
    lines <- c(lines[1:(obs[1] - 1)], empty, lines[obs[1]:length(lines)])
  }
  
  # Quitar \begin{table}, \end{table}, \centering, \caption: dejar solo el tabular
  rm_idx <- grep("(\\\\begin\\{table\\}|\\\\end\\{table\\}|^\\\\centering|\\\\caption)", lines)
  if (length(rm_idx) > 0) lines <- lines[-rm_idx]
  
  # Insertar la etiqueta del panel justo después del segundo \hline (header/body separator)
  #hline_idx <- grep("\\\\hline", lines)
  #if (length(hline_idx) >= 2) {
  #  panel_row <- paste0("\\multicolumn{", ncols, "}{l}{\\textit{", panel_label, "}} \\\\")
  #  lines <- c(lines[1:hline_idx[2]], panel_row,
  #             lines[(hline_idx[2] + 1):length(lines)])
  #}
  
  # Forzar ancho fijo del tabular para que todos los paneles queden alineados
  lines <- gsub("\\begin{tabular}[t]{lcccccc}",
                "\\begin{tabular*}{\\textwidth}{l@{\\extracolsep{\\fill}}cccccc}",
                lines, fixed = TRUE)
  lines <- gsub("\\end{tabular}", "\\end{tabular*}", lines, fixed = TRUE)
  
  lines
}

# Modelos de cada panel
panel_A_models <- list(
  "T1"   = att_b2_dt1,  "T2"   = att_b2_dt2,  "T3"   = att_b2_dt3,
  "T1 "  = att_b2_ft1,  "T2 "  = att_b2_ft2,  "T3 "  = att_b2_ft3)

panel_B_models <- list(
  "T1"   = att_b2_meanedt1, "T2"   = att_b2_meanedt2, "T3"   = att_b2_meanedt3,
  "T1 "  = att_b2_at1,      "T2 "  = att_b2_at2,      "T3 "  = att_b2_at3)

panel_C_models <- list(
  "T1"   = att_b2_izt1, "T2"   = att_b2_izt2, "T3"   = att_b2_izt3,
  "T1 "  = att_b2_alt1,      "T2 "  = att_b2_alt2,      "T3 "  = att_b2_alt3)

panel_A <- make_panel_tabular(
  panel_A_models,
  c("Pop. density", "Female pop."),
  c(3, 3)
  )

panel_B <- make_panel_tabular(
  panel_B_models,
  c("Mean years educ.", "Median age"),
  c(3, 3)
  )
# Sacar la hline de arriba del panel B
first_hline_B <- grep("^\\\\hline$", panel_B)[1]
if (!is.na(first_hline_B)) {
  panel_B <- panel_B[-first_hline_B]
}

panel_C <- make_panel_tabular(
  panel_C_models,
  c("Left vote share", "Ideological alternation"),
  c(3, 3)
)
# Sacar la hline de arriba del panel C
first_hline_C <- grep("^\\\\hline$", panel_C)[1]
if (!is.na(first_hline_C)) {
  panel_C <- panel_C[-first_hline_C]
}

# Combinar en una sola tabla con un único caption y una única nota
final <- c(
  "\\begin{table}[!h]",
  "\\centering",
  "\\renewcommand{\\arraystretch}{1.15}",
  "\\setlength{\\tabcolsep}{6pt}",
  "\\captionsetup{justification=centering}",
  "\\caption{Heterogeneous Effects on Blank Votes}",
  panel_A,
  panel_B,
  panel_C,
  "\\addvspace{0.3em}",
  "\\captionsetup{font=footnotesize, justification=justified, singlelinecheck=false}",
  "\\caption*{Notes: The dependent variable is the share of blank votes. Each column reports estimates for the subsample corresponding to a tercile of the following municipal-level variables: population density; share of female population; average years of education; median age; average left-wing vote share and ideological alternation rate, both measured over 2011–2021. Standard errors clustered at the municipality level are reported in parentheses. * $p<0.10$, ** $p<0.05$, *** $p<0.01$.}",
  "\\end{table}"
)

writeLines(final, "Output/blank_votes_subsamples.tex")

## Tabla para turnout

# Modelos de cada panel
panel_A_models <- list(
  "T1"   = att_p2_dt1,  "T2"   = att_p2_dt2,  "T3"   = att_p2_dt3,
  "T1 "  = att_p2_ft1,  "T2 "  = att_p2_ft2,  "T3 "  = att_p2_ft3)

panel_B_models <- list(
  "T1"   = att_p2_meanedt1, "T2"   = att_p2_meanedt2, "T3"   = att_p2_meanedt3,
  "T1 "  = att_p2_at1,      "T2 "  = att_p2_at2,      "T3 "  = att_p2_at3)

panel_C_models <- list(
  "T1"   = att_p2_izt1, "T2"   = att_p2_izt2, "T3"   = att_p2_izt3,
  "T1 "  = att_p2_alt1, "T2 "  = att_p2_alt2, "T3 "  = att_p2_alt3)

panel_A <- make_panel_tabular(
  panel_A_models,
  c("Pop. density", "Female pop."),
  c(3, 3)
)

panel_B <- make_panel_tabular(
  panel_B_models,
  c("Mean years educ.", "Median age"),
  c(3, 3)
)
# Sacar la hline de arriba del panel B
first_hline_B <- grep("^\\\\hline$", panel_B)[1]
if (!is.na(first_hline_B)) {
  panel_B <- panel_B[-first_hline_B]
}

panel_C <- make_panel_tabular(
  panel_C_models,
  c("Left vote share", "Ideological alternation"),
  c(3, 3)
)
# Sacar la hline de arriba del panel C
first_hline_C <- grep("^\\\\hline$", panel_C)[1]
if (!is.na(first_hline_C)) {
  panel_C <- panel_C[-first_hline_C]
}

# Combinar en una sola tabla con un único caption y una única nota
final <- c(
  "\\begin{table}[!h]",
  "\\centering",
  "\\renewcommand{\\arraystretch}{1.15}",
  "\\setlength{\\tabcolsep}{6pt}",
  "\\captionsetup{justification=centering}",
  "\\caption{Heterogeneous Effects on Turnout}",
  panel_A,
  panel_B,
  panel_C,
  "\\addvspace{0.3em}",
  "\\captionsetup{font=footnotesize, justification=justified, singlelinecheck=false}",
  "\\caption*{Notes: The dependent variable is voter turnout. Each column reports estimates for the subsample corresponding to a tercile of the following municipal-level variables: population density; share of female population; average years of education; median age; average left-wing vote share and ideological alternation rate, both measured over 2011–2021. Standard errors clustered at the municipality level are reported in parentheses. * $p<0.10$, ** $p<0.05$, *** $p<0.01$.}",
  "\\end{table}"
)
writeLines(final, "Output/turnout_subsamples.tex")
}

# ------------------------------------------ #
# 8. Correlaciones: shares de españoles y características municipales 
# ------------------------------------------ #
{
# Uno la data del censo con los shares de españoles
censo_2010 <- censo_2010 %>%
  left_join(spanish_cohorts_arg %>% 
              dplyr::select(mun_code, share_1936_1955, share_1956_1978), by = "mun_code")

# Carcaterísticas municipales a analizar
chars <- c(
  "mean_yrschool"     = "Mean years of education",
  "median_age"        = "Median age",
  #"mean_age"          = "Mean age",
  #"share_age_65plus"  = "Share aged 65+",
  #"share_age_25_44"   = "Share aged 25–44",
  "popdensgeo2"       = "Pop. density",
  "share_female"      = "Share female"
  #"share_unemployed"  = "Share unemployed",
  #"share_laborforce"  = "Share in labor force"
)

# Regresiones sin estandarizar el outcome
results_raw <- lapply(names(chars), function(v) {
  m <- feols(as.formula(paste0(v, " ~ share_1936_1955 + share_1956_1978")),
             data = censo_2010 %>% distinct(mun_code, .keep_all = TRUE))
  tidy(m, conf.int = TRUE) %>% mutate(outcome = v)
}) %>% bind_rows()

test_char_std <- function(data, char_var) {
  # Estandarizar el outcome
  data <- data %>%
    mutate(y_std = (!!sym(char_var) - mean(!!sym(char_var), na.rm = TRUE)) /
             sd(!!sym(char_var), na.rm = TRUE))
  
  # Correr la regresión
  m <- feols(
    y_std ~ share_1936_1955 + share_1956_1978,
    data = data,
    vcov = "hetero"
  )
  
  # Wald test: H0: beta_36 = beta_56
  betas <- coef(m)
  V     <- vcov(m)
  
  diff <- betas["share_1936_1955"] - betas["share_1956_1978"]
  
  se_diff <- sqrt(
    V["share_1936_1955", "share_1936_1955"] +
      V["share_1956_1978", "share_1956_1978"] -
      2 * V["share_1936_1955", "share_1956_1978"]
  )
  
  t_stat <- diff / se_diff
  p_val  <- 2 * pnorm(-abs(t_stat))
  
  # F-test global de la regresión
  ftest <- fitstat(m, "f")
  
  f_stat <- ftest$f$stat
  f_pval <- ftest$f$p
  
  broom::tidy(m, conf.int = TRUE) %>%
    filter(term %in% c("share_1936_1955", "share_1956_1978")) %>%
    mutate(
      p_equal = p_val,
      sig_equal = case_when(
        p_val < 0.01 ~ "***",
        p_val < 0.05 ~ "**",
        p_val < 0.10 ~ "*",
        TRUE         ~ ""
      ),
      f_stat = f_stat,
      f_pval = f_pval,
      sig_f = case_when(
        f_pval < 0.01 ~ "***",
        f_pval < 0.05 ~ "**",
        f_pval < 0.10 ~ "*",
        TRUE          ~ ""
      )
    )
}
# Correr todas las regresiones y juntar resultados
results <- bind_rows(lapply(names(chars), function(v) {
  test_char_std(censo_2010, v) %>%
    mutate(characteristic = chars[v])
}))

# Limpiar para el plot
results <- results %>%
  mutate(
    window = case_when(
      term == "share_1936_1955" ~ "1936–1955",
      term == "share_1956_1978" ~ "1956–1978"
    ),
    characteristic = factor(characteristic, levels = rev(unname(chars)))
  )

# Outcomes electorales pre tratamiento a agregar al mismo plot
chars_panel <- c(
  "share_izq"    = "Left vote share, pre-period",
  "alternancia"  = "Alternation, pre-period"
)

test_char_panel_std <- function(data, char_var) {
  
  data_reg <- data %>%
    mutate(
      y_std = (!!sym(char_var) - mean(!!sym(char_var), na.rm = TRUE)) /
        sd(!!sym(char_var), na.rm = TRUE)
    )
  
  # Regresión con FE de municipio y año
  m <- feols(
    y_std ~ share_1936_1955 + share_1956_1978 |  anio,
    data = data_reg,
    cluster = ~ mun_code
  )
  
  # Wald test: H0 beta_36 = beta_56
  betas <- coef(m)
  V     <- vcov(m)
  
  diff <- betas["share_1936_1955"] - betas["share_1956_1978"]
  
  se_diff <- sqrt(
    V["share_1936_1955", "share_1936_1955"] +
      V["share_1956_1978", "share_1956_1978"] -
      2 * V["share_1936_1955", "share_1956_1978"]
  )
  
  t_stat <- diff / se_diff
  p_val  <- 2 * pnorm(-abs(t_stat))
  
  # F-test global de los regresores
  ftest <- fitstat(m, "f")
  
  f_stat <- ftest$f$stat
  f_pval <- ftest$f$p
  
  broom::tidy(m, conf.int = TRUE) %>%
    filter(term %in% c("share_1936_1955", "share_1956_1978")) %>%
    mutate(
      p_equal = p_val,
      sig_equal = case_when(
        p_val < 0.01 ~ "***",
        p_val < 0.05 ~ "**",
        p_val < 0.10 ~ "*",
        TRUE         ~ ""
      ),
      f_stat = f_stat,
      f_pval = f_pval,
      sig_f = case_when(
        f_pval < 0.01 ~ "***",
        f_pval < 0.05 ~ "**",
        f_pval < 0.10 ~ "*",
        TRUE          ~ ""
      )
    )
}

# Correr las regresiones de panel
results_panel <- bind_rows(lapply(names(chars_panel), function(v) {
  test_char_panel_std(dip_nac_mun_pre, v) %>%
    mutate(characteristic = chars_panel[v])
}))

# Limpiar resultados de panel para que tengan el mismo formato
results_panel <- results_panel %>%
  mutate(
    window = case_when(
      term == "share_1936_1955" ~ "1936–1955",
      term == "share_1956_1978" ~ "1956–1978"
    )
  )

# Unir resultados del censo + resultados electorales pre
results_all <- bind_rows(
  results %>% mutate(source = "Census 2010"),
  results_panel %>% mutate(source = "Electoral panel, pre-period")
)

# Orden deseado en el gráfico
char_order_all <- c(
  unname(chars),
  unname(chars_panel)
)

results_all <- results_all %>%
  mutate(
    characteristic = factor(characteristic, levels = rev(char_order_all))
  )

# Labels de p-valores
sig_labels <- results_all %>%
  distinct(characteristic, p_equal) %>%
  mutate(label = sprintf("p = %.3f", p_equal))

# Coefplot
(p <- ggplot(results_all, aes(x = estimate, y = characteristic,
                         color = window, shape = window)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey60") +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high),
                 height = 0, linewidth = 0.5,
                 position = position_dodge(width = 0.6)) +
  geom_point(size = 2.5, position = position_dodge(width = 0.6)) +
  scale_shape_manual(values = c("1936–1955" = 16, "1956–1978" = 17),
                     name = "Spanish share window") +
  scale_color_manual(values = c("1936–1955" = "black", "1956–1978" = "#555555"),
                     name = "Spanish share window") +
  labs(
    x = "Standardized coefficient (SD of characteristic per unit of share)",
    y = NULL,
    #title = "Municipal characteristics and Spanish-immigration shares"
  ) +
  theme_minimal(base_family = "Times New Roman") +
  theme(
    legend.position  = "bottom",
    plot.title       = element_text(hjust = 0.5, size = 12),
    axis.title.x     = element_text(size = 15, color = "black"),
    axis.text        = element_text(size = 15, color = "black"),
    legend.text      = element_text(size = 15, color = "black"),
    legend.title     = element_text(size = 15, color = "black"), 
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_blank(),
    axis.line        = element_line(color = "black", linewidth = 0.4)
  ))

# Agregar al plot los resultados del test de igualdad de coeficientes
(p <- p +
  geom_text(data = sig_labels,
            aes(x = Inf, y = characteristic, label = label),
            inherit.aes = FALSE,
            hjust = 1.1, vjust = 0.5,
            size = 4,
            color = "black"))

ggsave("Output/coef_plot_chars.pdf", p, width = 9, height = 6)
ggsave("Output/coef_plot_chars.png", p, width = 9, height = 6, dpi = 300)

}

# ------------------------------------------ #
# 9. Estimación sin ventanas por separado
# ------------------------------------------ #

### 9.1 Voto en blanco ###

# Censo de 1970
att_b2_c70 <- feols(porcentaje_blanco ~ share_36_78_c70:post | 
                  mun_code + anio + tipo_eleccion, data = dip_nac_mun)

event_b2_c70 <- feols(porcentaje_blanco ~ i(anio, share_36_78_c70, ref = 2021) |
                       mun_code + anio + tipo_eleccion, data = dip_nac_mun)
summary(event_b2_c70)
iplot(event_b2_c70)
summary(att_b2_c70)

# Censo de 1980
att_b2_c80 <- feols(porcentaje_blanco ~ share_36_78_c80:post | 
                      mun_code + anio + tipo_eleccion, data = dip_nac_mun)

event_b2_c80 <- feols(porcentaje_blanco ~ i(anio, share_36_78_c80, ref = 2021) |
                           mun_code + anio + tipo_eleccion, data = dip_nac_mun)
summary(event_b2_c80)
iplot(event_b2_c80)
summary(att_b2_c80)

### 9.2 Participación ###
  
# Censo de 1970
att_p2_c70 <- feols(participacion ~ share_36_78_c70:post | 
                      mun_code + anio + tipo_eleccion, data = dip_nac_mun)

event_p2_c70 <- feols(participacion ~ i(anio, share_36_78_c70, ref = 2021) |
                           mun_code + anio + tipo_eleccion, data = dip_nac_mun)
summary(event_p2_c70)
iplot(event_p2_c70)
summary(att_p2_c70)

# Censo de 1980
att_p2_c80 <- feols(participacion ~ share_36_78_c80:post | 
                      mun_code + anio + tipo_eleccion, data = dip_nac_mun)

event_p2_c80 <- feols(participacion ~ i(anio, share_36_78_c80, ref = 2021) |
                           mun_code + anio + tipo_eleccion, data = dip_nac_mun)
summary(event_p2_c80)
iplot(event_p2_c80)
summary(att_p2_c80)

### 9.3 Exportar ### 

cm <- c("share_36_78_c80:post" = "Spanish share 1936-1978$\\times$Post",
        "share_36_78_c70:post" = "Spanish share 1936-1978$\\times$Post"
)

gm <- tibble::tribble(
  ~raw,        ~clean,         ~fmt,
  "nobs",      "Observations", 0,
  "r.squared", "R$^2$",        3)

models_list <- list("(1)" = att_b2_c70, "(2)" = att_b2_c80, 
                    "(3)" = att_p2_c70, "(4)" = att_p2_c80)

add_rows <- tibble::tibble(
  term = c("Municipality FE", "Time FE", "Election type FE", 
           "General elections only", "Spanish share census"),
  m1 = c("Yes", "Yes", "Yes", "No", "1970"),
  m2 = c("Yes", "Yes", "No", "Yes", "1980"),
  m3 = c("Yes", "Yes", "Yes", "No", "1970"),
  m4 = c("Yes", "Yes", "Yes", "No", "1980")
)
names(add_rows) <- c("term", "(1)", "(2)", "(3)", "(4)")

tex <- modelsummary(
  models_list,
  output    = "latex",
  coef_map  = cm,
  gof_map   = gm,
  estimate  = "{estimate}{stars}",
  statistic = "({std.error})",
  stars     = c("*" = .10, "**" = .05, "***" = .01),
  add_rows  = add_rows,
  escape    = FALSE)

lines <- strsplit(tex, "\n")[[1]]
ncols <- 7   # 1 label + 6 modelos

# 1) Caption, más espacio vertical y horizontal en la tabla
beg_table <- grep("\\\\begin\\{table\\}", lines)
if (length(beg_table) >= 1) {
  header <- c(
    "\\caption{Effects on Blank Votes and Voter Turnout}",
    "\\renewcommand{\\arraystretch}{1.25}",
    "\\setlength{\\tabcolsep}{6pt}"
  )
  lines <- c(lines[1:beg_table[1]], header, lines[(beg_table[1] + 1):length(lines)])
}

# 2) Header agrupado: "Share of blank votes" (cols 2-4) + "Voter turnout" (cols 5-7)
#    AMBOS en una sola fila
top_idx <- grep("\\\\toprule", lines)
if (length(top_idx) >= 1) {
  multicol <- " & \\multicolumn{2}{c}{Share of blank votes} & \\multicolumn{2}{c}{Voter turnout} \\\\"
  cmidrule <- "\\cmidrule(l){2-3} \\cmidrule(l){4-5}"
  lines <- c(lines[1:top_idx[1]],
             multicol,
             cmidrule,
             lines[(top_idx[1] + 1):length(lines)])
}

# 3) booktabs -> \hline
lines <- gsub("\\\\toprule",    "\\\\hline", lines)
lines <- gsub("\\\\bottomrule", "\\\\hline", lines)

# 4) Midrules
mr <- grep("\\\\midrule", lines)
if (length(mr) >= 1) lines[mr[1]] <- gsub("\\\\midrule", "\\\\hline", lines[mr[1]])
if (length(mr) >= 2) for (i in mr[-1]) lines[i] <- ""

# 5) Fila vacía arriba de Observations
obs <- grep("^Observations", lines)
if (length(obs) >= 1) {
  empty <- paste0(strrep(" &", ncols - 1), " \\\\")
  lines <- c(lines[1:(obs[1] - 1)], empty, lines[obs[1]:length(lines)])
}

# 6) Nota centrada en footnotesize
endtab <- grep("\\\\end\\{tabular\\}", lines)
if (length(endtab) >= 1) {
  nota <- c("\\vspace{0.4em}",
            "\\begin{minipage}{\\textwidth}",
            "\\footnotesize Notes: Spanish share is defined as the share of Spanish-born immigrants who arrived between 1936 and 1978 over the total municipal population. Models (1) and (3) identify this variable using the 1970 census; models (2) and (4) use the 1980 census. Standard errors clustered at the municipality level in parentheses. * $p<0.10$, ** $p<0.05$, *** $p<0.01$.",
            "\\end{minipage}"
            )
  lines <- c(lines[1:endtab[1]], nota, lines[(endtab[1] + 1):length(lines)])
}

writeLines(lines, "Output/att_blankvotes_turnout_nowindow.tex")

## Exporto event study en un mismo gráfico

# Votos en blanco

# 1) Extraer coeficientes de cada event study
coefs <- bind_rows(
  extract_es(event_b2_c70, "share_36_78_c70"),
  extract_es(event_b2_c80, "share_36_78_c80"),
  # Año de referencia (2021): coeficiente = 0, sin IC
  tibble::tibble(
    anio = 2021,
    window = c("share_36_78_c70", "share_36_78_c80"),
    estimate = 0,
    conf.low = NA_real_,
    conf.high = NA_real_
  )) %>% 
  rename(census = window) %>%
  mutate(census = case_when(
    census == "share_36_78_c70" ~ "Spanish share: 1970 Census",
    census == "share_36_78_c80" ~ "Spanish share: 1980 Census",
    TRUE ~ census
  ))

# 2) Plot
(pb <- ggplot(coefs %>% filter(anio != 2021), aes(x = anio, y = estimate, shape = census, group = census)) +
    geom_hline(yintercept = 0, linetype = "dotted", color = "black") +
    geom_vline(xintercept = 2021, linetype = "solid", linewidth = 0.3, color = "black") +
    geom_errorbar(
      aes(ymin = conf.low, ymax = conf.high),
      width = 0.3,
      position = position_dodge(width = 0.5),
      linewidth = 0.35,
      linetype = "solid",
      color = "grey60",
      show.legend = FALSE
    ) +
    geom_point(size = 2, position = position_dodge(width = 0.5), color = "black") +
    scale_shape_manual(values = c("Spanish share: 1970 Census" = 16, "Spanish share: 1980 Census" = 17), 
                       name = NULL) +
    scale_x_continuous(breaks = seq(2011, 2025, by = 2)) +
    labs(
      x = "Year",
      y = "Estimate and 95% CI",
      #title = "Effect on share of blank votes"
    ) +
    guides(shape = guide_legend(nrow = 1, byrow = TRUE)) +
    theme_minimal(base_family = "Times New Roman", base_size = 12) +
    theme(
      legend.position = "bottom",
      legend.justification = "center",
      legend.box = "horizontal",
      legend.margin = margin(t = -5, r = 0, b = 0, l = 0),
      plot.margin = margin(t = 10, r = 10, b = 10, l = 10),
      
      plot.title = element_text(hjust = 0.5, size = 12),
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_blank(),
      panel.grid.major.y = element_line(color = "grey85", linewidth = 0.3),
      axis.line = element_line(color = "black", linewidth = 0.4),
      axis.text = element_text(color = "black", size = 11),
      axis.title = element_text(color = "black", size = 11),
      legend.text = element_text(size = 11)
    ))

# 3) Guardar
ggsave("Output/event_b2_combined_nowindow.pdf", pb,
       width = 6.5, height = 4.5, dpi = 300)

# Participacion
{
# 1) Extraer coeficientes de cada event study
coefs <- bind_rows(
  extract_es(event_p2_c70, "share_36_78_c70"),
  extract_es(event_p2_c80, "share_36_78_c80"),
  # Año de referencia (2021): coeficiente = 0, sin IC
  tibble::tibble(
    anio = 2021,
    window = c("share_36_78_c70", "share_36_78_c80"),
    estimate = 0,
    conf.low = NA_real_,
    conf.high = NA_real_
  )) %>% 
  rename(census = window) %>%
  mutate(census = case_when(
    census == "share_36_78_c70" ~ "Spanish share: 1970 Census",
    census == "share_36_78_c80" ~ "Spanish share: 1980 Census",
    TRUE ~ census
  ))

# 2) Plot
(pp <- ggplot(coefs %>% filter(anio != 2021), aes(x = anio, y = estimate, shape = census, group = census)) +
    geom_hline(yintercept = 0, linetype = "dotted", color = "black") +
    geom_vline(xintercept = 2021, linetype = "solid", linewidth = 0.3, color = "black") +
    geom_errorbar(
      aes(ymin = conf.low, ymax = conf.high),
      width = 0.3,
      position = position_dodge(width = 0.5),
      linewidth = 0.35,
      linetype = "solid",
      color = "grey60",
      show.legend = FALSE
    ) +
    geom_point(size = 2, position = position_dodge(width = 0.5), color = "black") +
    scale_shape_manual(values = c("Spanish share: 1970 Census" = 16, "Spanish share: 1980 Census" = 17), 
                       name = NULL) +
    scale_x_continuous(breaks = seq(2011, 2025, by = 2)) +
    labs(
      x = "Year",
      y = "Estimate and 95% CI",
      #title = "Effect on share of blank votes"
    ) +
    guides(shape = guide_legend(nrow = 1, byrow = TRUE)) +
    theme_minimal(base_family = "Times New Roman", base_size = 12) +
    theme(
      legend.position = "bottom",
      legend.justification = "center",
      legend.box = "horizontal",
      legend.margin = margin(t = -5, r = 0, b = 0, l = 0),
      plot.margin = margin(t = 10, r = 10, b = 10, l = 10),
      
      plot.title = element_text(hjust = 0.5, size = 12),
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_blank(),
      panel.grid.major.y = element_line(color = "grey85", linewidth = 0.3),
      axis.line = element_line(color = "black", linewidth = 0.4),
      axis.text = element_text(color = "black", size = 11),
      axis.title = element_text(color = "black", size = 11),
      legend.text = element_text(size = 11)
    ))

# 3) Guardar
ggsave("Output/event_p2_combined_nowindow.pdf", pp,
       width = 6.5, height = 4.5, dpi = 300)
}
