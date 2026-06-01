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

# Exporto los resultados de participación
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
}

### 1.3. Voto izquierda vs derecha ### 
{
# 3.1 Share izquierda
  
  # Paso y generales controlando por tipo de elección
  att_iz1 <- feols(share_izq ~ share_1936_1955:post  + share_1956_1978:post | 
                    mun_code + anio + tipo_eleccion, data = dip_nac_mun)
  summary(att_iz1)

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

# 3.5 Share izquierda amplia 

  # Paso y generales controlando por tipo de elección
  att_iza1 <- feols(share_izq_amplia ~ share_1936_1955:post  + share_1956_1978:post | 
                   mun_code + anio + tipo_eleccion, data = dip_nac_mun)
  summary(att_iza1)

# 3.6 Share derecha amplia 

  # Paso y generales controlando por tipo de elección
  att_dea1 <- feols(share_der_amplia ~ share_1936_1955:post  + share_1956_1978:post | 
                    mun_code + anio + tipo_eleccion, data = dip_nac_mun)
  summary(att_dea1)

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
