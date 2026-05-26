install.packages("CBPS")
install.packages("cobalt")

library(CBPS)
library(cobalt)
library(dplyr)
library(tidyr)
library(patchwork)
library(ggplot2)

# ----------------------------------------------------- #
# 0. Data
# ----------------------------------------------------- #

path_flor <- "/Users/florenciaruiz/Library/CloudStorage/OneDrive-Personal/BID/Papers Valerie/Ley de nietos/Argentina"
setwd(path_flor)

data_balance <- read_dta("Data Int/data_balance.dta")

# ----------------------------------------------------- #
#  1. Covariate Balancing Generalized Propensity Score
# ----------------------------------------------------- #

### 1.1. Set 1 ### 
{
# Mínimo de covariables: porcentaje_blanco_pre_avg participacion_pre_avg log_pop log_density 
  cbgps_1936_1 <- CBPS(
    share_1936_1955 ~ porcentaje_blanco_pre_avg + participacion_pre_avg + log_pop + log_density,
    data = data_balance,
    method = "exact"
  )
  
  cbgps_1956_1 <- CBPS(
    share_1956_1978 ~ porcentaje_blanco_pre_avg + participacion_pre_avg + log_pop + log_density,
    data = data_balance,
    method = "exact"
  )
  
  # Guardo los pesos
  data_balance$w_cbgps_36_1 <- cbgps_1936_1$weights
  data_balance$w_cbgps_56_1 <- cbgps_1956_1$weights
  
  # Tabla de balance y observaciones
  bal_1936_1 <- bal.tab(cbgps_1936_1, stats = "correlations", un = TRUE) 
  bal_1956_1 <- bal.tab(cbgps_1956_1, stats = "correlations", un = TRUE) 
  
  # Exportar la tablas de correlaciones
  bal_1936_1_corr <- as.data.frame(bal_1936_1$Balance)
  bal_1936_1_corr$Covariate <- rownames(bal_1936_1_corr)
  rownames(bal_1936_1_corr) <- NULL
  bal_1936_1_corr$`Exposure window` <- "1936-1955"
  
  bal_1956_1_corr <- as.data.frame(bal_1956_1$Balance)
  bal_1956_1_corr$Covariate <- rownames(bal_1956_1_corr)
  rownames(bal_1956_1_corr) <- NULL
  bal_1956_1_corr$`Exposure window` <- "1956-1978"
  
  bal_1_corr <- rbind(bal_1936_1_corr, bal_1956_1_corr)
  
  bal_1_corr <- bal_1_corr %>% 
    dplyr::select(Covariate, `Exposure window`, everything(), -Type) %>% 
    rename(`Corr (unadjusted)` =Corr.Un, 
           `Corr (weighted)` = Corr.Adj) %>% 
    mutate(Covariate = case_when(
      Covariate == "log_pop" ~ "Log(Population)",
      Covariate == "log_density" ~ "Log(Density)",
      Covariate == "porcentaje_blanco_pre_avg" ~ "Blank vote share (average 2011-2021)",
      Covariate == "participacion_pre_avg" ~ "Voter turnout (average 2011-2021)",
    ))
  
  # Exportar tablas de observaciones
  bal_1936_1_obs <- as.data.frame(bal_1936_1$Observations)
  bal_1956_1_obs <- as.data.frame(bal_1956_1$Observations)
  
  bal_1936_1_obs$Sample <- rownames(bal_1936_1_obs)
  rownames(bal_1936_1_obs) <- NULL
  bal_1936_1_obs$`Exposure window` <- "1936-1955"
  bal_1956_1_obs$Sample <- rownames(bal_1956_1_obs)
  rownames(bal_1956_1_obs) <- NULL
  bal_1956_1_obs$`Exposure window` <- "1956-1978"
  
  bal_1_obs <- rbind(bal_1936_1_obs, bal_1956_1_obs) %>% 
    dplyr::select(`Exposure window`, Sample, everything())
  
  # Love plot
  var_labels <- c(
    log_pop = "Log population",
    log_density = "Log population density",
    porcentaje_blanco_pre_avg = "Blank vote share (avg. 2011-2021)",
    participacion_pre_avg = "Voter turnout (avg. 2011-2021)"
  )
  (p_1936_1 <- love.plot(cbgps_1936_1, stats = "correlations", position = "bottom", size = 3.5,
                         abs = TRUE, thresholds = c(cor = .1), var.names = var_labels,  
                         colors = c("tomato3", "steelblue4"))+
      ggplot2::theme(
        axis.title = ggplot2::element_text(size = 14),
        axis.text  = ggplot2::element_text(size = 13),
        legend.title = ggplot2::element_text(size = 14),
        legend.text  = ggplot2::element_text(size = 13)
      ))
  
  (p_1956_1 <- love.plot(cbgps_1956_1, stats = "correlations", position = "bottom", size = 3.5,
                         abs = TRUE, thresholds = c(cor = .1), var.names = var_labels, 
                         colors = c("tomato3", "steelblue4")) +
      ggplot2::theme(
        axis.title = ggplot2::element_text(size = 14),
        axis.text  = ggplot2::element_text(size = 13),
        legend.title = ggplot2::element_text(size = 14),
        legend.text  = ggplot2::element_text(size = 13)
      ))
}

### 1.2. Set 2 ### 
{
# Set 1 + variables de demografía
  cbgps_1936_2 <- CBPS(
    share_1936_1955 ~ porcentaje_blanco_pre_avg + participacion_pre_avg + log_pop + log_density + 
                      share_female + mean_age,
    data = data_balance,
    method = "exact"
  )
  
  cbgps_1956_2 <- CBPS(
    share_1956_1978 ~ porcentaje_blanco_pre_avg + participacion_pre_avg + log_pop + log_density + 
                      share_female + mean_age,
    data = data_balance,
    method = "exact"
  )
  
  # Guardo los pesos
  data_balance$w_cbgps_36_2 <- cbgps_1936_2$weights
  data_balance$w_cbgps_56_2 <- cbgps_1956_2$weights
  
  # Tabla de balance y observaciones
  bal_1936_2 <- bal.tab(cbgps_1936_2, stats = "correlations", un = TRUE) 
  bal_1956_2 <- bal.tab(cbgps_1956_2, stats = "correlations", un = TRUE) 
  
  # Exportar la tablas de correlaciones
  bal_1936_2_corr <- as.data.frame(bal_1936_2$Balance)
  bal_1936_2_corr$Covariate <- rownames(bal_1936_2_corr)
  rownames(bal_1936_2_corr) <- NULL
  bal_1936_2_corr$`Exposure window` <- "1936-1955"
  
  bal_1956_2_corr <- as.data.frame(bal_1956_2$Balance)
  bal_1956_2_corr$Covariate <- rownames(bal_1956_2_corr)
  rownames(bal_1956_2_corr) <- NULL
  bal_1956_2_corr$`Exposure window` <- "1956-1978"
  
  bal_2_corr <- rbind(bal_1936_2_corr, bal_1956_2_corr)
  
  bal_2_corr <- bal_2_corr %>% 
    dplyr::select(Covariate, `Exposure window`, everything(), -Type) %>% 
    rename(`Corr (unadjusted)` =Corr.Un, 
           `Corr (weighted)` = Corr.Adj) %>% 
    mutate(Covariate = case_when(
      Covariate == "log_pop" ~ "Log(Population)",
      Covariate == "log_density" ~ "Log(Density)",
      Covariate == "porcentaje_blanco_pre_avg" ~ "Blank vote share (average 2011-2021)",
      Covariate == "participacion_pre_avg" ~ "Voter turnout (average 2011-2021)",
      Covariate == "share_female" ~ "Share of women",
      Covariate == "mean_age" ~ "Mean age"
    ))

  # Exportar tablas de observaciones
  bal_1936_2_obs <- as.data.frame(bal_1936_2$Observations)
  bal_1956_2_obs <- as.data.frame(bal_1956_2$Observations)
  
  bal_1936_2_obs$Sample <- rownames(bal_1936_2_obs)
  rownames(bal_1936_2_obs) <- NULL
  bal_1936_2_obs$`Exposure window` <- "1936-1955"
  bal_1956_2_obs$Sample <- rownames(bal_1956_2_obs)
  rownames(bal_1956_2_obs) <- NULL
  bal_1956_2_obs$`Exposure window` <- "1956-1978"
  
  bal_2_obs <- rbind(bal_1936_2_obs, bal_1956_2_obs) %>% 
    dplyr::select(`Exposure window`, Sample, everything())
  
  # Love plot
  var_labels <- c(
    log_pop = "Log population",
    log_density = "Log population density",
    porcentaje_blanco_pre_avg = "Blank vote share (avg. 2011-2021)",
    participacion_pre_avg = "Voter turnout (avg. 2011-2021)",
    share_female = "Share of women",
    mean_age = "Mean age"
  )
  (p_1936_2 <- love.plot(cbgps_1936_2, stats = "correlations", position = "bottom", size = 3.5,
                         abs = TRUE, thresholds = c(cor = .1), var.names = var_labels,  
                         colors = c("tomato3", "steelblue4"))+
      ggplot2::theme(
        axis.title = ggplot2::element_text(size = 14),
        axis.text  = ggplot2::element_text(size = 13),
        legend.title = ggplot2::element_text(size = 14),
        legend.text  = ggplot2::element_text(size = 13)
      ))
  
  (p_1956_2 <- love.plot(cbgps_1956_2, stats = "correlations", position = "bottom", size = 3.5,
                         abs = TRUE, thresholds = c(cor = .1), var.names = var_labels, 
                         colors = c("tomato3", "steelblue4")) +
      ggplot2::theme(
        axis.title = ggplot2::element_text(size = 14),
        axis.text  = ggplot2::element_text(size = 13),
        legend.title = ggplot2::element_text(size = 14),
        legend.text  = ggplot2::element_text(size = 13)
      ))
} 

### 1.3. Set 3 ### 
{
# Set 2 + variables económicas y educativas
cbgps_1936_3 <- CBPS(
  share_1936_1955 ~ porcentaje_blanco_pre_avg + participacion_pre_avg + log_pop + log_density +
                    share_female + mean_age + share_employed + share_laborforce + share_literate,
  data = data_balance,
  method = "exact"
)

cbgps_1956_3 <- CBPS(
  share_1956_1978 ~ porcentaje_blanco_pre_avg + participacion_pre_avg + log_pop + log_density +
                    share_female + mean_age + share_employed + share_laborforce + share_literate,
  data = data_balance,
  method = "exact"
)

# Guardo los pesos
data_balance$w_cbgps_36_3 <- cbgps_1936_3$weights
data_balance$w_cbgps_56_3 <- cbgps_1956_3$weights

# Tabla de balance y observaciones
bal_1936_3 <- bal.tab(cbgps_1936_3, stats = "correlations", un = TRUE) 
bal_1956_3 <- bal.tab(cbgps_1956_3, stats = "correlations", un = TRUE) 

# Exportar la tablas de correlaciones
bal_1936_3_corr <- as.data.frame(bal_1936_3$Balance)
bal_1936_3_corr$Covariate <- rownames(bal_1936_3_corr)
rownames(bal_1936_3_corr) <- NULL
bal_1936_3_corr$`Exposure window` <- "1936-1955"

bal_1956_3_corr <- as.data.frame(bal_1956_3$Balance)
bal_1956_3_corr$Covariate <- rownames(bal_1956_3_corr)
rownames(bal_1956_3_corr) <- NULL
bal_1956_3_corr$`Exposure window` <- "1956-1978"

bal_3_corr <- rbind(bal_1936_3_corr, bal_1956_3_corr)

bal_3_corr <- bal_3_corr %>% 
  dplyr::select(Covariate, `Exposure window`, everything(), -Type) %>% 
  rename(`Corr (unadjusted)` =Corr.Un, 
         `Corr (weighted)` = Corr.Adj) %>% 
  mutate(Covariate = case_when(
    Covariate == "log_pop" ~ "Log(Population)",
    Covariate == "log_density" ~ "Log(Density)",
    Covariate == "porcentaje_blanco_pre_avg" ~ "Blank vote share (average 2011-2021)",
    Covariate == "participacion_pre_avg" ~ "Voter turnout (average 2011-2021)",
    Covariate == "share_female" ~ "Share of women",
    Covariate == "mean_age" ~ "Mean age",
    Covariate == "share_employed" ~ "Share of employed",
    Covariate == "share_laborforce" ~ "Share of labor force",
    Covariate == "share_literate" ~ "Share of literate"
  ))

# Exportar tablas de observaciones
bal_1936_3_obs <- as.data.frame(bal_1936_3$Observations)
bal_1956_3_obs <- as.data.frame(bal_1956_3$Observations)

bal_1936_3_obs$Sample <- rownames(bal_1936_3_obs)
rownames(bal_1936_3_obs) <- NULL
bal_1936_3_obs$`Exposure window` <- "1936-1955"
bal_1956_3_obs$Sample <- rownames(bal_1956_3_obs)
rownames(bal_1956_3_obs) <- NULL
bal_1956_3_obs$`Exposure window` <- "1956-1978"

bal_3_obs <- rbind(bal_1936_3_obs, bal_1956_3_obs) %>% 
  dplyr::select(`Exposure window`, Sample, everything())

# Love plot
var_labels <- c(
  log_pop = "Log population",
  log_density = "Log population density",
  porcentaje_blanco_pre_avg = "Blank vote share (avg. 2011-2021)",
  participacion_pre_avg = "Voter turnout (avg. 2011-2021)",
  share_female = "Share of women",
  mean_age = "Mean age",
  share_employed = "Share of employed",
  share_laborforce = "Share of labor force",
  share_literate = "Share of literate"
)
(p_1936_3 <- love.plot(cbgps_1936_3, stats = "correlations", position = "bottom", size = 3.5,
                       abs = TRUE, thresholds = c(cor = .1), var.names = var_labels,  
                       colors = c("tomato3", "steelblue4"))+
    ggplot2::theme(
      axis.title = ggplot2::element_text(size = 14),
      axis.text  = ggplot2::element_text(size = 13),
      legend.title = ggplot2::element_text(size = 14),
      legend.text  = ggplot2::element_text(size = 13)
    ))

(p_1956_3 <- love.plot(cbgps_1956_3, stats = "correlations", position = "bottom", size = 3.5,
                       abs = TRUE, thresholds = c(cor = .1), var.names = var_labels, 
                       colors = c("tomato3", "steelblue4")) +
    ggplot2::theme(
      axis.title = ggplot2::element_text(size = 14),
      axis.text  = ggplot2::element_text(size = 13),
      legend.title = ggplot2::element_text(size = 14),
      legend.text  = ggplot2::element_text(size = 13)
    ))

}

### 1.4. Set 4 ### 
{
# Set 3 + outcomes electorales pre tratamiento que estaban desbalanceados (y voto blanco)
cbgps_1936_4 <- CBPS(
  share_1936_1955 ~ porcentaje_blanco_pre_avg + participacion_pre_avg + log_pop + log_density +
    share_female + mean_age + share_employed + share_laborforce + share_literate +
    share_izq_pre_avg + share_peronistas_pre_avg + nep_pre_avg + margen_pre_avg,
  data = data_balance,
  method = "exact"
)

cbgps_1956_4 <- CBPS(
  share_1956_1978 ~ porcentaje_blanco_pre_avg + participacion_pre_avg + log_pop + log_density +
    share_female + mean_age + share_employed + share_laborforce + share_literate +
    share_izq_pre_avg + share_peronistas_pre_avg + nep_pre_avg + margen_pre_avg,
  data = data_balance,
  method = "exact"
)

# Guardo los pesos
data_balance$w_cbgps_36_4 <- cbgps_1936_4$weights
data_balance$w_cbgps_56_4 <- cbgps_1956_4$weights

# Tabla de balance y observaciones
bal_1936_4 <- bal.tab(cbgps_1936_4, stats = "correlations", un = TRUE) # cantidad ajustada baja
bal_1956_4 <- bal.tab(cbgps_1956_4, stats = "correlations", un = TRUE) # cantidad ajustada baja

# Exportar la tablas de correlaciones
bal_1936_4_corr <- as.data.frame(bal_1936_4$Balance)
bal_1936_4_corr$Covariate <- rownames(bal_1936_4_corr)
rownames(bal_1936_4_corr) <- NULL
bal_1936_4_corr$`Exposure window` <- "1936-1955"

bal_1956_4_corr <- as.data.frame(bal_1956_4$Balance)
bal_1956_4_corr$Covariate <- rownames(bal_1956_4_corr)
rownames(bal_1956_4_corr) <- NULL
bal_1956_4_corr$`Exposure window` <- "1956-1978"

bal_4_corr <- rbind(bal_1936_4_corr, bal_1956_4_corr)

bal_4_corr <- bal_4_corr %>% 
  dplyr::select(Covariate, `Exposure window`, everything(), -Type) %>% 
  rename(`Corr (unadjusted)` =Corr.Un, 
         `Corr (weighted)` = Corr.Adj) %>% 
  mutate(Covariate = case_when(
    Covariate == "log_pop" ~ "Log(Population)",
    Covariate == "log_density" ~ "Log(Density)",
    Covariate == "porcentaje_blanco_pre_avg" ~ "Blank vote share (average 2011-2021)",
    Covariate == "participacion_pre_avg" ~ "Voter turnout (average 2011-2021)",
    Covariate == "share_female" ~ "Share of women",
    Covariate == "mean_age" ~ "Mean age",
    Covariate == "share_employed" ~ "Share of employed",
    Covariate == "share_laborforce" ~ "Share of labor force",
    Covariate == "share_literate" ~ "Share of literate",
    Covariate == "share_izq_pre_avg" ~ "Left vote share (average 2011-2021)",
    Covariate == "share_peronistas_pre_avg" ~ "Peronist vote share (average 2011-2021)",
    Covariate == "nep_pre_avg" ~ "NEP index (average 2011-2021)",
    Covariate == "margen_pre_avg" ~ "Margin of victory (average 2011-2021)"
  ))

# Exportar tablas de observaciones
bal_1936_4_obs <- as.data.frame(bal_1936_4$Observations)
bal_1956_4_obs <- as.data.frame(bal_1956_4$Observations)

bal_1936_4_obs$Sample <- rownames(bal_1936_4_obs)
rownames(bal_1936_4_obs) <- NULL
bal_1936_4_obs$`Exposure window` <- "1936-1955"
bal_1956_4_obs$Sample <- rownames(bal_1956_4_obs)
rownames(bal_1956_4_obs) <- NULL
bal_1956_4_obs$`Exposure window` <- "1956-1978"

bal_4_obs <- rbind(bal_1936_4_obs, bal_1956_4_obs) %>% 
  dplyr::select(`Exposure window`, Sample, everything())

# Love plot
var_labels <- c(
  log_pop = "Log population",
  log_density = "Log population density",
  porcentaje_blanco_pre_avg = "Blank vote share (avg. 2011-2021)",
  participacion_pre_avg = "Voter turnout (avg. 2011-2021)",
  share_female = "Share of women",
  mean_age = "Mean age",
  share_employed = "Share of employed",
  share_laborforce = "Share of labor force",
  share_literate = "Share of literate",
  share_izq_pre_avg = "Left vote share (avg. 2011-2021)",
  share_peronistas_pre_avg = "Peronist vote share (avg. 2011-2021)",
  nep_pre_avg = "NEP index (avg. 2011-2021)",
  margen_pre_avg = "Margin of victory (avg. 2011-2021)"
)
(p_1936_4 <- love.plot(cbgps_1936_4, stats = "correlations", position = "bottom", size = 3.5,
                       abs = TRUE, thresholds = c(cor = .1), var.names = var_labels,  
                       colors = c("tomato3", "steelblue4"))+
    ggplot2::theme(
      axis.title = ggplot2::element_text(size = 14),
      axis.text  = ggplot2::element_text(size = 13),
      legend.title = ggplot2::element_text(size = 14),
      legend.text  = ggplot2::element_text(size = 13)
    ))

(p_1956_4 <- love.plot(cbgps_1956_4, stats = "correlations", position = "bottom", size = 3.5,
                       abs = TRUE, thresholds = c(cor = .1), var.names = var_labels, 
                       colors = c("tomato3", "steelblue4")) +
    ggplot2::theme(
      axis.title = ggplot2::element_text(size = 14),
      axis.text  = ggplot2::element_text(size = 13),
      legend.title = ggplot2::element_text(size = 14),
      legend.text  = ggplot2::element_text(size = 13)
    ))
}

### 1.5. Set 5 ### 
{
# Set 1 + outcomes electorales pre tratamiento que estaban desbalanceados
cbgps_1936_5 <- CBPS(
  share_1936_1955 ~ porcentaje_blanco_pre_avg + participacion_pre_avg + log_pop + log_density +
                    share_izq_pre_avg + share_peronistas_pre_avg + nep_pre_avg + margen_pre_avg,
  data = data_balance,
  method = "exact"
)

cbgps_1956_5 <- CBPS(
  share_1956_1978 ~ porcentaje_blanco_pre_avg + participacion_pre_avg + log_pop + log_density +
                    share_izq_pre_avg + share_peronistas_pre_avg + nep_pre_avg + margen_pre_avg,
  data = data_balance,
  method = "exact"
)

# Guardo los pesos
data_balance$w_cbgps_36_5 <- cbgps_1936_5$weights
data_balance$w_cbgps_56_5 <- cbgps_1956_5$weights

# Tabla de balance y observaciones
bal_1936_5 <- bal.tab(cbgps_1936_5, stats = "correlations", un = TRUE) # cantidad ajustada baja
bal_1956_5 <- bal.tab(cbgps_1956_5, stats = "correlations", un = TRUE) # cantidad ajustada MUY baja

# Exportar la tablas de correlaciones
bal_1936_5_corr <- as.data.frame(bal_1936_5$Balance)
bal_1936_5_corr$Covariate <- rownames(bal_1936_5_corr)
rownames(bal_1936_5_corr) <- NULL
bal_1936_5_corr$`Exposure window` <- "1936-1955"

bal_1956_5_corr <- as.data.frame(bal_1956_5$Balance)
bal_1956_5_corr$Covariate <- rownames(bal_1956_5_corr)
rownames(bal_1956_5_corr) <- NULL
bal_1956_5_corr$`Exposure window` <- "1956-1978"

bal_5_corr <- rbind(bal_1936_5_corr, bal_1956_5_corr)

bal_5_corr <- bal_5_corr %>% 
  dplyr::select(Covariate, `Exposure window`, everything(), -Type) %>% 
  rename(`Corr (unadjusted)` =Corr.Un, 
         `Corr (weighted)` = Corr.Adj) %>% 
  mutate(Covariate = case_when(
    Covariate == "log_pop" ~ "Log(Population)",
    Covariate == "log_density" ~ "Log(Density)",
    Covariate == "porcentaje_blanco_pre_avg" ~ "Blank vote share (average 2011-2021)",
    Covariate == "participacion_pre_avg" ~ "Voter turnout (average 2011-2021)",
    Covariate == "share_izq_pre_avg" ~ "Left vote share (average 2011-2021)",
    Covariate == "share_peronistas_pre_avg" ~ "Peronist vote share (average 2011-2021)",
    Covariate == "nep_pre_avg" ~ "NEP index (average 2011-2021)",
    Covariate == "margen_pre_avg" ~ "Margin of victory (average 2011-2021)"
  ))

# Exportar tablas de observaciones
bal_1936_5_obs <- as.data.frame(bal_1936_5$Observations)
bal_1956_5_obs <- as.data.frame(bal_1956_5$Observations)

bal_1936_5_obs$Sample <- rownames(bal_1936_5_obs)
rownames(bal_1936_5_obs) <- NULL
bal_1936_5_obs$`Exposure window` <- "1936-1955"
bal_1956_5_obs$Sample <- rownames(bal_1956_5_obs)
rownames(bal_1956_5_obs) <- NULL
bal_1956_5_obs$`Exposure window` <- "1956-1978"

bal_5_obs <- rbind(bal_1936_5_obs, bal_1956_5_obs) %>% 
  dplyr::select(`Exposure window`, Sample, everything())

# Love plot
var_labels <- c(
  log_pop = "Log population",
  log_density = "Log population density",
  porcentaje_blanco_pre_avg = "Blank vote share (avg. 2011-2021)",
  participacion_pre_avg = "Voter turnout (avg. 2011-2021)",
  share_izq_pre_avg = "Left vote share (avg. 2011-2021)",
  share_peronistas_pre_avg = "Peronist vote share (avg. 2011-2021)",
  nep_pre_avg = "NEP index (avg. 2011-2021)",
  margen_pre_avg = "Margin of victory (avg. 2011-2021)"
)
(p_1936_5 <- love.plot(cbgps_1936_5, stats = "correlations", position = "bottom", size = 3.5,
                       abs = TRUE, thresholds = c(cor = .1), var.names = var_labels,  
                       colors = c("tomato3", "steelblue4"))+
    ggplot2::theme(
      axis.title = ggplot2::element_text(size = 14),
      axis.text  = ggplot2::element_text(size = 13),
      legend.title = ggplot2::element_text(size = 14),
      legend.text  = ggplot2::element_text(size = 13)
    ))

(p_1956_5 <- love.plot(cbgps_1956_5, stats = "correlations", position = "bottom", size = 3.5,
                       abs = TRUE, thresholds = c(cor = .1), var.names = var_labels, 
                       colors = c("tomato3", "steelblue4")) +
    ggplot2::theme(
      axis.title = ggplot2::element_text(size = 14),
      axis.text  = ggplot2::element_text(size = 13),
      legend.title = ggplot2::element_text(size = 14),
      legend.text  = ggplot2::element_text(size = 13)
    ))
}

### 1.6. Set 6 ###
{
# Solo outcomes electorales desbalanceados
cbgps_1936_6 <- CBPS(
  share_1936_1955 ~ participacion_pre_avg + share_izq_pre_avg + share_peronistas_pre_avg +
                    nep_pre_avg + margen_pre_avg,
  data = data_balance,
  method = "exact"
)

cbgps_1956_6 <- CBPS(
  share_1956_1978 ~ participacion_pre_avg + share_izq_pre_avg + share_peronistas_pre_avg +
                    nep_pre_avg + margen_pre_avg,
  data = data_balance,
  method = "exact"
)

# Guardo los pesos
data_balance$w_cbgps_36_6 <- cbgps_1936_6$weights
data_balance$w_cbgps_56_6 <- cbgps_1956_6$weights

# Tabla de balance y observaciones
bal_1936_6 <- bal.tab(cbgps_1936_6, stats = "correlations", un = TRUE)
bal_1956_6 <- bal.tab(cbgps_1956_6, stats = "correlations", un = TRUE)

# Exportar la tablas de correlaciones
bal_1936_6_corr <- as.data.frame(bal_1936_6$Balance)
bal_1936_6_corr$Covariate <- rownames(bal_1936_6_corr)
rownames(bal_1936_6_corr) <- NULL
bal_1936_6_corr$`Exposure window` <- "1936-1955"

bal_1956_6_corr <- as.data.frame(bal_1956_6$Balance)
bal_1956_6_corr$Covariate <- rownames(bal_1956_6_corr)
rownames(bal_1956_6_corr) <- NULL
bal_1956_6_corr$`Exposure window` <- "1956-1978"

bal_6_corr <- rbind(bal_1936_6_corr, bal_1956_6_corr)

bal_6_corr <- bal_6_corr %>% 
  dplyr::select(Covariate, `Exposure window`, everything(), -Type) %>% 
  rename(`Corr (unadjusted)` =Corr.Un, 
         `Corr (weighted)` = Corr.Adj) %>% 
  mutate(Covariate = case_when(
    Covariate == "participacion_pre_avg" ~ "Voter turnout (average 2011-2021)",
    Covariate == "share_izq_pre_avg" ~ "Left vote share (average 2011-2021)",
    Covariate == "share_peronistas_pre_avg" ~ "Peronist vote share (average 2011-2021)",
    Covariate == "nep_pre_avg" ~ "NEP index (average 2011-2021)",
    Covariate == "margen_pre_avg" ~ "Margin of victory (average 2011-2021)"
  ))

# Exportar tablas de observaciones
bal_1936_6_obs <- as.data.frame(bal_1936_6$Observations)
bal_1956_6_obs <- as.data.frame(bal_1956_6$Observations)

bal_1936_6_obs$Sample <- rownames(bal_1936_6_obs)
rownames(bal_1936_6_obs) <- NULL
bal_1936_6_obs$`Exposure window` <- "1936-1955"
bal_1956_6_obs$Sample <- rownames(bal_1956_6_obs)
rownames(bal_1956_6_obs) <- NULL
bal_1956_6_obs$`Exposure window` <- "1956-1978"

bal_6_obs <- rbind(bal_1936_6_obs, bal_1956_6_obs) %>% 
  dplyr::select(`Exposure window`, Sample, everything())

# Love plot
var_labels <- c(
  participacion_pre_avg = "Voter turnout (avg. 2011-2021)",
  share_izq_pre_avg = "Left vote share (avg. 2011-2021)",
  share_peronistas_pre_avg = "Peronist vote share (avg. 2011-2021)",
  nep_pre_avg = "NEP index (avg. 2011-2021)",
  margen_pre_avg = "Margin of victory (avg. 2011-2021)"
)
(p_1936_6 <- love.plot(cbgps_1936_6, stats = "correlations", position = "bottom", size = 3.5,
                       abs = TRUE, thresholds = c(cor = .1), var.names = var_labels,  
                       colors = c("tomato3", "steelblue4"))+
    ggplot2::theme(
      axis.title = ggplot2::element_text(size = 14),
      axis.text  = ggplot2::element_text(size = 13),
      legend.title = ggplot2::element_text(size = 14),
      legend.text  = ggplot2::element_text(size = 13)
    ))

(p_1956_6 <- love.plot(cbgps_1956_6, stats = "correlations", position = "bottom", size = 3.5,
                       abs = TRUE, thresholds = c(cor = .1), var.names = var_labels, 
                       colors = c("tomato3", "steelblue4")) +
    ggplot2::theme(
      axis.title = ggplot2::element_text(size = 14),
      axis.text  = ggplot2::element_text(size = 13),
      legend.title = ggplot2::element_text(size = 14),
      legend.text  = ggplot2::element_text(size = 13)
    ))
}

### 1.7. Set 7 ### 
{
# Alternativa parsimoniosa solo con covariables
cbgps_1936_7 <- CBPS(
  share_1936_1955 ~ log_density + mean_age + share_employed + mean_yrschool,
  data = data_balance,
  method = "exact"
)
  
cbgps_1956_7 <- CBPS(
  share_1956_1978 ~ log_density + mean_age + share_employed + mean_yrschool,
  data = data_balance,
  method = "exact"
)
  
# Guardo los pesos
data_balance$w_cbgps_36_7 <- cbgps_1936_7$weights
data_balance$w_cbgps_56_7 <- cbgps_1956_7$weights

# Tabla de balance y observaciones
bal_1936_7 <- bal.tab(cbgps_1936_7, stats = "correlations", un = TRUE) # Tamaño efectivo MUY chico
bal_1956_7 <- bal.tab(cbgps_1956_7, stats = "correlations", un = TRUE)

# Exportar la tablas de correlaciones
bal_1936_7_corr <- as.data.frame(bal_1936_7$Balance)
bal_1936_7_corr$Covariate <- rownames(bal_1936_7_corr)
rownames(bal_1936_7_corr) <- NULL
bal_1936_7_corr$`Exposure window` <- "1936-1955"

bal_1956_7_corr <- as.data.frame(bal_1956_7$Balance)
bal_1956_7_corr$Covariate <- rownames(bal_1956_7_corr)
rownames(bal_1956_7_corr) <- NULL
bal_1956_7_corr$`Exposure window` <- "1956-1978"

bal_7_corr <- rbind(bal_1936_7_corr, bal_1956_7_corr)

bal_7_corr <- bal_7_corr %>% 
  dplyr::select(Covariate, `Exposure window`, everything(), -Type) %>% 
  rename(`Corr (unadjusted)` =Corr.Un, 
         `Corr (weighted)` = Corr.Adj) %>% 
  mutate(Covariate = case_when(
    Covariate == "log_density" ~ "Log(Density)",
    Covariate == "mean_age" ~ "Mean age",
    Covariate == "share_employed" ~ "Share of employed",
    Covariate == "mean_yrschool" ~ "Mean years of education"
  ))

# Exportar tablas de observaciones
bal_1936_7_obs <- as.data.frame(bal_1936_7$Observations)
bal_1956_7_obs <- as.data.frame(bal_1956_7$Observations)

bal_1936_7_obs$Sample <- rownames(bal_1936_7_obs)
rownames(bal_1936_7_obs) <- NULL
bal_1936_7_obs$`Exposure window` <- "1936-1955"
bal_1956_7_obs$Sample <- rownames(bal_1956_7_obs)
rownames(bal_1956_7_obs) <- NULL
bal_1956_7_obs$`Exposure window` <- "1956-1978"

bal_7_obs <- rbind(bal_1936_7_obs, bal_1956_7_obs) %>% 
  dplyr::select(`Exposure window`, Sample, everything())

# Love plot
var_labels <- c(
  log_density = "Log(Density)",
  mean_age = "Mean age",
  share_employed = "Share of employed",
  mean_yrschool = "Mean years of education"
)
(p_1936_7 <- love.plot(cbgps_1936_7, stats = "correlations", position = "bottom", size = 3.5,
                       abs = TRUE, thresholds = c(cor = .1), var.names = var_labels,  
                       colors = c("tomato3", "steelblue4"))+
    ggplot2::theme(
      axis.title = ggplot2::element_text(size = 14),
      axis.text  = ggplot2::element_text(size = 13),
      legend.title = ggplot2::element_text(size = 14),
      legend.text  = ggplot2::element_text(size = 13)
    ))

(p_1956_7 <- love.plot(cbgps_1956_7, stats = "correlations", position = "bottom", size = 3.5,
                       abs = TRUE, thresholds = c(cor = .1), var.names = var_labels, 
                       colors = c("tomato3", "steelblue4")) +
    ggplot2::theme(
      axis.title = ggplot2::element_text(size = 14),
      axis.text  = ggplot2::element_text(size = 13),
      legend.title = ggplot2::element_text(size = 14),
      legend.text  = ggplot2::element_text(size = 13)
    ))
}

### 1.8. Set 8 ### 
{
# Alternaiva parsimoniosa solo con (otro) set de covariables
cbgps_1936_8 <- CBPS(
  share_1936_1955 ~ log_density + share_age_25_44 + share_employed + share_university,
  data = data_balance,
  method = "exact"
)

cbgps_1956_8 <- CBPS(
  share_1956_1978 ~ log_density + share_age_25_44 + share_employed + share_university,
  data = data_balance,
  method = "exact"
)

# Guardo los pesos
data_balance$w_cbgps_36_8 <- cbgps_1936_8$weights
data_balance$w_cbgps_56_8 <- cbgps_1956_8$weights

# Tabla de balance y observaciones
bal_1936_8 <- bal.tab(cbgps_1936_8, stats = "correlations", un = TRUE) # Tamaño efectivo MUY chico
bal_1956_8 <- bal.tab(cbgps_1956_8, stats = "correlations", un = TRUE)

# Exportar la tablas de correlaciones
bal_1936_8_corr <- as.data.frame(bal_1936_8$Balance)
bal_1936_8_corr$Covariate <- rownames(bal_1936_8_corr)
rownames(bal_1936_8_corr) <- NULL
bal_1936_8_corr$`Exposure window` <- "1936-1955"

bal_1956_8_corr <- as.data.frame(bal_1956_8$Balance)
bal_1956_8_corr$Covariate <- rownames(bal_1956_8_corr)
rownames(bal_1956_8_corr) <- NULL
bal_1956_8_corr$`Exposure window` <- "1956-1978"

bal_8_corr <- rbind(bal_1936_8_corr, bal_1956_8_corr)

bal_8_corr <- bal_8_corr %>% 
  dplyr::select(Covariate, `Exposure window`, everything(), -Type) %>% 
  rename(`Corr (unadjusted)` =Corr.Un, 
         `Corr (weighted)` = Corr.Adj) %>% 
  mutate(Covariate = case_when(
    Covariate == "log_density" ~ "Log(Density)",
    Covariate == "share_age_25_44" ~ "Share of young pop. (25-44)",
    Covariate == "share_employed" ~ "Share of employed",
    Covariate == "share_university" ~ "Share with university education"
  ))

# Exportar tablas de observaciones
bal_1936_8_obs <- as.data.frame(bal_1936_8$Observations)
bal_1956_8_obs <- as.data.frame(bal_1956_8$Observations)

bal_1936_8_obs$Sample <- rownames(bal_1936_8_obs)
rownames(bal_1936_8_obs) <- NULL
bal_1936_8_obs$`Exposure window` <- "1936-1955"
bal_1956_8_obs$Sample <- rownames(bal_1956_8_obs)
rownames(bal_1956_8_obs) <- NULL
bal_1956_8_obs$`Exposure window` <- "1956-1978"

bal_8_obs <- rbind(bal_1936_8_obs, bal_1956_8_obs) %>% 
  dplyr::select(`Exposure window`, Sample, everything())

# Love plot
var_labels <- c(
  log_density = "Log(Density)",
  share_age_25_44 = "Share of young pop. (25-44)",
  share_employed = "Share of employed",
  share_university = "Share with university education"
)
(p_1936_8 <- love.plot(cbgps_1936_8, stats = "correlations", position = "bottom", size = 3.5,
                       abs = TRUE, thresholds = c(cor = .1), var.names = var_labels,  
                       colors = c("tomato3", "steelblue4"))+
    ggplot2::theme(
      axis.title = ggplot2::element_text(size = 14),
      axis.text  = ggplot2::element_text(size = 13),
      legend.title = ggplot2::element_text(size = 14),
      legend.text  = ggplot2::element_text(size = 13)
    ))

(p_1956_8 <- love.plot(cbgps_1956_8, stats = "correlations", position = "bottom", size = 3.5,
                       abs = TRUE, thresholds = c(cor = .1), var.names = var_labels, 
                       colors = c("tomato3", "steelblue4")) +
    ggplot2::theme(
      axis.title = ggplot2::element_text(size = 14),
      axis.text  = ggplot2::element_text(size = 13),
      legend.title = ggplot2::element_text(size = 14),
      legend.text  = ggplot2::element_text(size = 13)
    ))
}

### Exportar ###
{
# Tablas
openxlsx::write.xlsx(
  list(
    corr_1 = bal_1_corr,
    obs_1  = bal_1_obs,
    corr_2 = bal_2_corr,
    obs_2  = bal_2_obs,
    corr_3 = bal_3_corr,
    obs_3  = bal_3_obs,
    corr_4 = bal_4_corr,
    obs_4  = bal_4_obs,
    corr_5 = bal_5_corr,
    obs_5  = bal_5_obs,
    corr_6 = bal_6_corr,
    obs_6  = bal_6_obs,
    corr_7 = bal_7_corr,
    obs_7  = bal_7_obs,
    corr_8 = bal_8_corr,
    obs_8  = bal_8_obs
  ),
  file = "Output/balance_cbgps.xlsx",
  overwrite = TRUE
)

# Gráficos
  
  # 1-4 1936
love_1936_1_4 <- (
  (p_1936_1 + ggtitle("Covariate specification 1")) |
    (p_1936_2 + ggtitle("Covariate specification 2"))
) /
  (
    (p_1936_3 + ggtitle("Covariate specification 3")) |
      (p_1936_4 + ggtitle("Covariate specification 4"))
  ) +
  plot_layout(guides = "collect") +
  plot_annotation(
    title = "Covariate balance: Spanish share 1936-1955",
    subtitle = "CBGPS weights, specifications 1-4"
  ) &
  theme(
    plot.title = element_text(size = 14, hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(size = 12, hjust = 0.5),
    axis.text = element_text(size = 11),
    axis.title = element_text(size = 12),
    legend.text = element_text(size = 12),
    legend.title = element_text(size = 12),
    legend.position = "bottom"
  )
love_1936_1_4

ggsave(
  filename = "Output/loveplots_1936_specs_1_4.png",
  plot = love_1936_1_4,
  width = 14,
  height = 10,
  dpi = 300
)

  # 5-8 1936
love_1936_5_8 <- (
  (p_1936_5 + ggtitle("Covariate specification 5")) |
    (p_1936_6 + ggtitle("Covariate specification 6"))
) /
  (
    (p_1936_7 + ggtitle("Covariate specification 7")) |
      (p_1936_8 + ggtitle("Covariate specification 8"))
  ) +
  plot_layout(guides = "collect") +
  plot_annotation(
    title = "Covariate balance: Spanish share 1936-1955",
    subtitle = "CBGPS weights, specifications 5-8"
  ) &
  theme(
    plot.title = element_text(size = 14, hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(size = 12, hjust = 0.5),
    axis.text = element_text(size = 11),
    axis.title = element_text(size = 12),
    legend.text = element_text(size = 12),
    legend.title = element_text(size = 12),
    legend.position = "bottom"
  )
love_1936_5_8

ggsave(
  filename = "Output/loveplots_1936_specs_5_8.png",
  plot = love_1936_5_8,
  width = 14,
  height = 10,
  dpi = 300
)

  # 1-4 1956
love_1956_1_4 <- (
  (p_1956_1 + ggtitle("Covariate specification 1")) |
    (p_1956_2 + ggtitle("Covariate specification 2"))
) /
  (
    (p_1956_3 + ggtitle("Covariate specification 3")) |
      (p_1956_4 + ggtitle("Covariate specification 4"))
  ) +
  plot_layout(guides = "collect") +
  plot_annotation(
    title = "Covariate balance: Spanish share 1956-1978",
    subtitle = "CBGPS weights, specifications 1-4"
  ) &
  theme(
    plot.title = element_text(size = 14, hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(size = 12, hjust = 0.5),
    axis.text = element_text(size = 11),
    axis.title = element_text(size = 12),
    legend.text = element_text(size = 12),
    legend.title = element_text(size = 12),
    legend.position = "bottom"
  )
love_1956_1_4

ggsave(
  filename = "Output/loveplots_1956_specs_1_4.png",
  plot = love_1956_1_4,
  width = 14,
  height = 10,
  dpi = 300
)
  
  # 5-8 1956
love_1956_5_8 <- (
  (p_1956_5 + ggtitle("Covariate specification 5")) |
    (p_1956_6 + ggtitle("Covariate specification 6"))
) /
  (
    (p_1956_7 + ggtitle("Covariate specification 7")) |
      (p_1956_8 + ggtitle("Covariate specification 8"))
  ) +
  plot_layout(guides = "collect") +
  plot_annotation(
    title = "Covariate balance: Spanish share 1956-1978",
    subtitle = "CBGPS weights, specifications 5-8"
  ) &
  theme(
    plot.title = element_text(size = 14, hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(size = 12, hjust = 0.5),
    axis.text = element_text(size = 11),
    axis.title = element_text(size = 12),
    legend.text = element_text(size = 12),
    legend.title = element_text(size = 12),
    legend.position = "bottom"
  )
love_1956_5_8

ggsave(
  filename = "Output/loveplots_1956_specs_5_8.png",
  plot = love_1956_5_8,
  width = 14,
  height = 10,
  dpi = 300
)

# Guardo la data con los pesos para graficar histogramas en stata (mejor formato)
pesos_cbgps <- data_balance %>% 
  dplyr::select(mun_code,  starts_with("w_cbgps"))
write.dta(pesos_cbgps, "Data Int/pesos_cbgps.dta")
}


