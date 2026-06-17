# ---------------------------------------------------------------------------- #
#     LAPOP: pooled Spanish shares c70/c80 - all estimations in one script
# ---------------------------------------------------------------------------- #
#
# Corre por separado:
#   1. share_36_78_c70
#   2. share_36_78_c80
#
# Para cada share estima:
#   A. Post x pooled share: LPM y Logit
#   B. Triple differences: Post x X x pooled share, LPM y Logit
#
# Exporta:
#   - Modelos .rds en Output/models/
#   - Tablas .xlsx en Output/
#   - Tablas .tex en Output/tex/ para Overleaf
#
# En Overleaf podes usar:
#   \input{post_pooled_share_lpm_logit_key_coefficients_all.tex}
#   \input{triple_pooled_share_key_coefficients_all.tex}
#   \input{triple_pooled_share_all_interactions_all.tex}
#
# Si usas la tabla grande con \addlinespace, incluir en Overleaf:
#   \usepackage{booktabs}
#
# ---------------------------------------------------------------------------- #

library(dplyr)
library(readr)
library(stringr)
library(purrr)
library(tidyr)
library(fixest)
library(openxlsx)
library(haven)

# ---------------------------------------------------------------------------- #
# 0. Paths
# ---------------------------------------------------------------------------- #

path_pili <- "C:\\Users\\pilih\\Documents\\Papers German\\Valerie\\Paper_nietos_arg"
setwd(path_pili)

dir.create("Output", showWarnings = FALSE)
dir.create("Output/tex", showWarnings = FALSE, recursive = TRUE)
dir.create("Output/models", showWarnings = FALSE, recursive = TRUE)

# ---------------------------------------------------------------------------- #
# 1. Shares pooled a correr por separado
# ---------------------------------------------------------------------------- #

exposures <- tibble::tribble(
  ~exposure_var,      ~suffix, ~exposure_label,
  "share_36_78_c70", "c70",   "Share pooled 36-78 c70",
  "share_36_78_c80", "c80",   "Share pooled 36-78 c80"
)

# ---------------------------------------------------------------------------- #
# 2. Funciones auxiliares generales
# ---------------------------------------------------------------------------- #

rhs_join <- function(...) {
  parts <- c(...)
  parts <- parts[!is.na(parts)]
  parts <- parts[str_trim(parts) != ""]
  
  if (length(parts) == 0) {
    return("")
  }
  
  paste(parts, collapse = " + ")
}

wmean <- function(x, w) {
  ok <- !is.na(x) & !is.na(w)
  
  if (sum(ok) == 0) {
    return(NA_real_)
  }
  
  weighted.mean(x[ok], w = w[ok], na.rm = TRUE)
}

safe_wmean <- function(data, var) {
  if (!var %in% names(data)) {
    return(NA_real_)
  }
  
  x <- data[[var]]
  
  if (!is.numeric(x) & !is.integer(x) & !is.logical(x)) {
    return(NA_real_)
  }
  
  wmean(as.numeric(x), data$wt)
}

stars <- function(p) {
  case_when(
    is.na(p) ~ "",
    p < 0.01 ~ "***",
    p < 0.05 ~ "**",
    p < 0.10 ~ "*",
    TRUE ~ ""
  )
}

format_coef <- function(estimate, se, p) {
  if (is.na(estimate)) {
    return("")
  }
  
  paste0(
    sprintf("%.3f", estimate),
    stars(p),
    " (",
    sprintf("%.3f", se),
    ")"
  )
}

format_logit_sign <- function(estimate, p) {
  if (is.na(estimate)) {
    return("")
  }
  
  sign_char <- case_when(
    estimate > 0 ~ "+",
    estimate < 0 ~ "-",
    TRUE ~ "0"
  )
  
  paste0(sign_char, stars(p))
}

format_or <- function(estimate, se, p) {
  if (is.na(estimate)) {
    return("")
  }
  
  or <- exp(estimate)
  se_or <- exp(estimate) * se
  
  paste0(
    sprintf("%.3f", or),
    stars(p),
    " (",
    sprintf("%.3f", se_or),
    ")"
  )
}

latex_escape <- function(x) {
  x %>%
    as.character() %>%
    stringr::str_replace_all("_", "\\\\_") %>%
    stringr::str_replace_all("%", "\\\\%") %>%
    stringr::str_replace_all("&", "\\\\&")
}

tex_row <- function(...) {
  paste0(paste(..., collapse = " & "), " \\\\")
}

term_matches_parts <- function(term, parts) {
  term_parts <- str_split(term, ":", simplify = FALSE)[[1]]
  setequal(term_parts, parts)
}

get_clean_coeftable <- function(model) {
  
  ct <- as.data.frame(coeftable(model))
  ct$term <- rownames(ct)
  
  estimate_col <- names(ct)[str_detect(names(ct), "^Estimate$")]
  se_col <- names(ct)[str_detect(names(ct), "Std\\. Error")]
  p_col <- names(ct)[str_detect(names(ct), "^Pr\\(")]
  
  if (length(estimate_col) == 0) {
    estimate_col <- names(ct)[1]
  }
  
  if (length(se_col) == 0) {
    se_col <- names(ct)[2]
  }
  
  if (length(p_col) == 0) {
    ct$p_value <- NA_real_
  } else {
    ct$p_value <- ct[[p_col[1]]]
  }
  
  ct %>%
    mutate(
      estimate = .data[[estimate_col[1]]],
      se = .data[[se_col[1]]]
    ) %>%
    select(term, estimate, se, p_value)
}

extract_one_term <- function(model,
                             parts,
                             model_type = c("lpm", "logit_sign", "logit_or")) {
  
  model_type <- match.arg(model_type)
  
  ct <- get_clean_coeftable(model)
  
  term_row <- ct %>%
    filter(
      map_lgl(term, ~ term_matches_parts(.x, parts))
    )
  
  if (nrow(term_row) == 0) {
    return("")
  }
  
  if (model_type == "lpm") {
    format_coef(
      estimate = term_row$estimate[1],
      se = term_row$se[1],
      p = term_row$p_value[1]
    )
  } else if (model_type == "logit_sign") {
    format_logit_sign(
      estimate = term_row$estimate[1],
      p = term_row$p_value[1]
    )
  } else {
    format_or(
      estimate = term_row$estimate[1],
      se = term_row$se[1],
      p = term_row$p_value[1]
    )
  }
}

variable_labels <- c(
  "mun_pre_all_mean_edad" = "Mean age",
  "mun_pre_all_share_hombre" = "Share male",
  "popdensgeo2" = "Log Density",
  "mun_pre_all_share_desempleado" = "Share unemployed",
  "mun_pre_all_share_en_pareja" = "Share partnered",
  "mun_pre_all_mean_educ" = "Mean years of education",
  "mun_pre_all_mean_izq_der" = "Mean left-right ideology",
  "mun_pre_all_share_interes_pol_mucho" = "Share very interested in politics",
  "mun_pre_all_share_voto_blanco_nulo" = "Share blank/null vote"
)

label_variable <- function(x) {
  ifelse(
    x %in% names(variable_labels),
    variable_labels[x],
    x
  )
}

# ---------------------------------------------------------------------------- #
# 3. Cargar LAPOP y mergear pooled shares si hace falta
# ---------------------------------------------------------------------------- #

find_spanish_cohorts_path <- function() {
  candidates <- c(
    "Data Out/spanish_cohorts_arg_csv.csv",
    "Data Out/spanish_cohorts_arg.csv",
    "Data Int/spanish_cohorts_arg_csv.csv",
    "Data Int/spanish_cohorts_arg.csv",
    "Data Out/spanish_cohorts_arg_csv",
    "Data Int/spanish_cohorts_arg_csv"
  )
  
  hit <- candidates[file.exists(candidates)]
  
  if (length(hit) == 0) {
    stop(
      paste0(
        "No encuentro spanish_cohorts_arg_csv. Probe estas rutas: ",
        paste(candidates, collapse = ", ")
      )
    )
  }
  
  hit[1]
}

rename_if_found <- function(data, target_name, patterns) {
  
  if (target_name %in% names(data)) {
    return(data)
  }
  
  candidates <- names(data)[
    map_lgl(
      names(data),
      function(nm) {
        nm_low <- str_to_lower(nm)
        all(map_lgl(patterns, ~ str_detect(nm_low, .x)))
      }
    )
  ]
  
  if (length(candidates) == 1) {
    cat(
      "\nRenombrando columna detectada automaticamente: ",
      candidates[1],
      " -> ",
      target_name,
      "\n",
      sep = ""
    )
    
    data <- data %>%
      rename(!!target_name := all_of(candidates[1]))
  }
  
  if (length(candidates) > 1) {
    cat(
      "\nHay mas de una posible columna para ",
      target_name,
      ": ",
      paste(candidates, collapse = ", "),
      "\n",
      sep = ""
    )
    
    stop(
      paste0(
        "No puedo elegir automaticamente cual corresponde a ",
        target_name,
        ". Renombrala manualmente o ajusta los patterns."
      )
    )
  }
  
  data
}

load_lapop_with_pooled_shares <- function(exposure_vars) {
  
  lapop <- read_csv(
    "Data Out/lapop_data_merge.csv",
    show_col_types = FALSE
  ) %>%
    mutate(mun_code = as.character(mun_code))
  
  missing_exposures <- setdiff(exposure_vars, names(lapop))
  
  if (length(missing_exposures) == 0) {
    return(lapop)
  }
  
  spanish_path <- find_spanish_cohorts_path()
  
  spanish_cohorts <- read_csv(
    spanish_path,
    show_col_types = FALSE
  ) %>%
    mutate(mun_code = as.character(mun_code))
  
  cat("\nColumnas disponibles en ", spanish_path, ":\n", sep = "")
  print(names(spanish_cohorts))
  
  spanish_cohorts <- spanish_cohorts %>%
    rename_if_found(
      target_name = "share_36_78_c70",
      patterns = c("share", "36|1936", "78|1978", "c70|70")
    ) %>%
    rename_if_found(
      target_name = "share_36_78_c80",
      patterns = c("share", "36|1936", "78|1978", "c80|80")
    )
  
  missing_spanish_vars <- setdiff(
    c("mun_code", missing_exposures),
    names(spanish_cohorts)
  )
  
  if (length(missing_spanish_vars) > 0) {
    stop(
      paste0(
        "Faltan estas variables en ",
        spanish_path,
        ": ",
        paste(missing_spanish_vars, collapse = ", "),
        "\n\nMira arriba la lista de columnas disponibles y ajusta los nombres en rename_if_found()."
      )
    )
  }
  
  spanish_cohorts <- spanish_cohorts %>%
    select(mun_code, all_of(missing_exposures)) %>%
    distinct(mun_code, .keep_all = TRUE)
  
  lapop <- lapop %>%
    left_join(spanish_cohorts, by = "mun_code")
  
  lapop
}

# ---------------------------------------------------------------------------- #
# 4. Cargar base final
# ---------------------------------------------------------------------------- #

lapop <- load_lapop_with_pooled_shares(exposures$exposure_var)

censo_2010_density <- read_dta("Data Int/censo_2010_arg_mun.dta") %>%
  mutate(mun_code = as.character(mun_code)) %>%
  select(mun_code, popdensgeo2)

lapop <- lapop %>%
  mutate(
    mun_code = as.character(mun_code),
    year_num = as.numeric(year),
    post = if_else(year_num >= 2023, 1, 0)
  ) %>%
  left_join(censo_2010_density, by = "mun_code") %>%
  mutate(
    popdensgeo2 = log(popdensgeo2),
    mun_code = as.factor(mun_code),
    year = as.factor(year_num)
  )

# ---------------------------------------------------------------------------- #
# 5. Construir controles municipales pre-2023
# ---------------------------------------------------------------------------- #

pre_period_all <- lapop %>%
  filter(year_num < 2023)

if (nrow(pre_period_all) == 0) {
  stop("No hay observaciones pre-2023 para calcular controles municipales.")
}

mun_pre_all_controls <- pre_period_all %>%
  group_by(mun_code) %>%
  group_modify(
    ~ tibble(
      mun_pre_all_mean_edad = safe_wmean(.x, "edad"),
      mun_pre_all_share_hombre = safe_wmean(.x, "hombre"),
      mun_pre_all_share_desempleado = safe_wmean(.x, "desempleado"),
      mun_pre_all_share_en_pareja = safe_wmean(.x, "en_pareja"),
      mun_pre_all_mean_educ = safe_wmean(.x, "anios_educ"),
      mun_pre_all_mean_izq_der = safe_wmean(.x, "izq_der"),
      mun_pre_all_share_interes_pol_mucho = safe_wmean(.x, "interes_pol_mucho"),
      mun_pre_all_share_voto_blanco_nulo = safe_wmean(.x, "voto_blanco_nulo")
    )
  ) %>%
  ungroup()

lapop <- lapop %>%
  left_join(mun_pre_all_controls, by = "mun_code")

controls_individual <- c("edad", "hombre")
controls_individual <- controls_individual[controls_individual %in% names(lapop)]

triple_vars_core <- c(
  "mun_pre_all_mean_edad",
  "mun_pre_all_share_hombre",
  "popdensgeo2",
  "mun_pre_all_share_desempleado",
  "mun_pre_all_share_en_pareja",
  "mun_pre_all_mean_educ"
)

triple_vars_politics <- c(
  "mun_pre_all_mean_izq_der",
  "mun_pre_all_share_interes_pol_mucho",
  "mun_pre_all_share_voto_blanco_nulo"
)

triple_vars <- unique(c(
  triple_vars_core,
  triple_vars_politics
))

triple_vars <- triple_vars[
  triple_vars %in% names(lapop) &
    map_lgl(triple_vars, ~ any(!is.na(lapop[[.x]])))
]

cat("Variables usadas para triples:\n")
print(triple_vars)

# ---------------------------------------------------------------------------- #
# 6. Funcion: post x pooled share
# ---------------------------------------------------------------------------- #

run_post_pooled_share <- function(exposure_var, suffix, exposure_label) {
  
  cat("\n============================================================\n")
  cat("POST x POOLED SHARE: ", exposure_var, "\n", sep = "")
  cat("============================================================\n")
  
  required_vars <- c(
    "intencion_migrar",
    "post",
    exposure_var,
    "year",
    "mun_code",
    "wt"
  )
  
  missing_vars <- setdiff(required_vars, names(lapop))
  
  if (length(missing_vars) > 0) {
    stop(
      paste0(
        "Faltan variables para estimar: ",
        paste(missing_vars, collapse = ", ")
      )
    )
  }
  
  print(
    lapop %>%
      summarise(
        exposure = exposure_var,
        n = sum(!is.na(.data[[exposure_var]])),
        mean = mean(.data[[exposure_var]], na.rm = TRUE),
        sd = sd(.data[[exposure_var]], na.rm = TRUE),
        min = min(.data[[exposure_var]], na.rm = TRUE),
        max = max(.data[[exposure_var]], na.rm = TRUE)
      )
  )
  
  specs <- list(
    no_controls = list(
      controls_label = "No",
      controls = character(0)
    ),
    with_controls = list(
      controls_label = "Yes",
      controls = controls_individual
    )
  )
  
  make_formula <- function(controls = character(0)) {
    
    rhs <- rhs_join(
      paste0("post:", exposure_var),
      paste(controls, collapse = " + ")
    )
    
    as.formula(
      paste0(
        "intencion_migrar ~ ",
        rhs,
        " | year + mun_code"
      )
    )
  }
  
  run_lpm <- function(spec_name, spec) {
    
    fml <- make_formula(spec$controls)
    
    cat("\nEstimando LPM post x pooled share. Spec:", spec_name, "\n")
    cat(deparse(fml), "\n\n")
    
    feols(
      fml,
      data = lapop,
      weights = ~ wt,
      cluster = ~ mun_code
    )
  }
  
  run_logit <- function(spec_name, spec) {
    
    fml <- make_formula(spec$controls)
    
    cat("\nEstimando Logit post x pooled share. Spec:", spec_name, "\n")
    cat(deparse(fml), "\n\n")
    
    feglm(
      fml,
      data = lapop,
      family = binomial(link = "logit"),
      weights = ~ wt,
      cluster = ~ mun_code
    )
  }
  
  models_lpm <- imap(specs, ~ run_lpm(.y, .x))
  models_logit <- imap(specs, ~ run_logit(.y, .x))
  
  saveRDS(
    models_lpm,
    paste0("Output/models/models_post_pooled_share_lpm_", suffix, ".rds")
  )
  
  saveRDS(
    models_logit,
    paste0("Output/models/models_post_pooled_share_logit_", suffix, ".rds")
  )
  
  post_table <- map_dfr(
    names(specs),
    function(spec_name) {
      
      lpm_model <- models_lpm[[spec_name]]
      logit_model <- models_logit[[spec_name]]
      
      tibble(
        exposure = exposure_var,
        exposure_label = exposure_label,
        suffix = suffix,
        spec = spec_name,
        controls = specs[[spec_name]]$controls_label,
        term_id = paste0("post_", exposure_var),
        term_label = paste0("Post x ", exposure_label),
        lpm = extract_one_term(
          lpm_model,
          c("post", exposure_var),
          "lpm"
        ),
        logit_sign = extract_one_term(
          logit_model,
          c("post", exposure_var),
          "logit_sign"
        ),
        logit_or = extract_one_term(
          logit_model,
          c("post", exposure_var),
          "logit_or"
        ),
        n_lpm = nobs(lpm_model),
        n_logit = nobs(logit_model)
      )
    }
  )
  
  print(post_table)
  
  write.xlsx(
    post_table,
    paste0("Output/post_pooled_share_lpm_logit_key_coefficients_", suffix, ".xlsx"),
    overwrite = TRUE
  )
  
  post_table
}

# ---------------------------------------------------------------------------- #
# 7. Funcion: triple differences con pooled share
# ---------------------------------------------------------------------------- #

run_triple_pooled_share <- function(exposure_var, suffix, exposure_label) {
  
  cat("\n============================================================\n")
  cat("TRIPLE DIFFERENCES: ", exposure_var, "\n", sep = "")
  cat("============================================================\n")
  
  make_triple_formula <- function(x, with_controls = FALSE) {
    
    rhs_triple <- paste0(
      "post:", exposure_var,
      " + post:", x,
      " + post:", x, ":", exposure_var
    )
    
    if (with_controls) {
      
      other_mun_pre_vars <- setdiff(triple_vars, x)
      
      rhs_other_mun_pre <- paste0(
        "post:",
        other_mun_pre_vars,
        collapse = " + "
      )
      
      rhs_individual <- paste(
        controls_individual,
        collapse = " + "
      )
      
      rhs <- rhs_join(
        rhs_triple,
        rhs_other_mun_pre,
        rhs_individual
      )
      
    } else {
      
      rhs <- rhs_triple
    }
    
    as.formula(
      paste0(
        "intencion_migrar ~ ",
        rhs,
        " | year + mun_code"
      )
    )
  }
  
  run_triple_lpm_one <- function(x, with_controls = FALSE) {
    
    fml <- make_triple_formula(x, with_controls)
    
    cat("\nEstimando LPM triple para:", x, "\n")
    cat("Controles adicionales:", ifelse(with_controls, "Si", "No"), "\n")
    cat(deparse(fml), "\n\n")
    
    feols(
      fml,
      data = lapop,
      weights = ~ wt,
      cluster = ~ mun_code
    )
  }
  
  run_triple_logit_one <- function(x, with_controls = FALSE) {
    
    fml <- make_triple_formula(x, with_controls)
    
    cat("\nEstimando Logit triple para:", x, "\n")
    cat("Controles adicionales:", ifelse(with_controls, "Si", "No"), "\n")
    cat(deparse(fml), "\n\n")
    
    feglm(
      fml,
      data = lapop,
      family = binomial(link = "logit"),
      weights = ~ wt,
      cluster = ~ mun_code
    )
  }
  
  models_lpm_no <- map(
    triple_vars,
    ~ run_triple_lpm_one(.x, with_controls = FALSE)
  )
  names(models_lpm_no) <- triple_vars
  
  models_lpm_yes <- map(
    triple_vars,
    ~ run_triple_lpm_one(.x, with_controls = TRUE)
  )
  names(models_lpm_yes) <- triple_vars
  
  models_logit_no <- map(
    triple_vars,
    ~ run_triple_logit_one(.x, with_controls = FALSE)
  )
  names(models_logit_no) <- triple_vars
  
  models_logit_yes <- map(
    triple_vars,
    ~ run_triple_logit_one(.x, with_controls = TRUE)
  )
  names(models_logit_yes) <- triple_vars
  
  saveRDS(
    models_lpm_no,
    paste0("Output/models/models_triple_pooled_share_no_controls_", suffix, ".rds")
  )
  
  saveRDS(
    models_lpm_yes,
    paste0("Output/models/models_triple_pooled_share_with_controls_", suffix, ".rds")
  )
  
  saveRDS(
    models_logit_no,
    paste0("Output/models/models_logit_triple_pooled_share_no_controls_", suffix, ".rds")
  )
  
  saveRDS(
    models_logit_yes,
    paste0("Output/models/models_logit_triple_pooled_share_with_controls_", suffix, ".rds")
  )
  
  extract_triple_key_for_x <- function(x, controls_label) {
    
    if (controls_label == "No") {
      lpm_model <- models_lpm_no[[x]]
      logit_model <- models_logit_no[[x]]
    } else {
      lpm_model <- models_lpm_yes[[x]]
      logit_model <- models_logit_yes[[x]]
    }
    
    parts <- c("post", x, exposure_var)
    
    tibble(
      exposure = exposure_var,
      exposure_label = exposure_label,
      suffix = suffix,
      variable = x,
      controls = controls_label,
      coef_triple = extract_one_term(
        lpm_model,
        parts,
        "lpm"
      ),
      logit_triple_sign = extract_one_term(
        logit_model,
        parts,
        "logit_sign"
      ),
      logit_triple_or = extract_one_term(
        logit_model,
        parts,
        "logit_or"
      ),
      n_lpm = nobs(lpm_model),
      n_logit = nobs(logit_model)
    )
  }
  
  triple_key_table <- map_dfr(
    triple_vars,
    function(x) {
      bind_rows(
        extract_triple_key_for_x(x, "No"),
        extract_triple_key_for_x(x, "Yes")
      )
    }
  ) %>%
    arrange(variable, controls)
  
  print(triple_key_table)
  
  write.xlsx(
    triple_key_table,
    paste0("Output/triple_pooled_share_key_coefficients_", suffix, ".xlsx"),
    overwrite = TRUE
  )
  
  make_terms_for_x <- function(x) {
    
    tibble(
      term_id = c(
        "post_share_pooled",
        "post_x",
        "post_x_share_pooled"
      ),
      term_label = c(
        paste0("Post x ", exposure_label),
        "Post x X",
        paste0("Post x X x ", exposure_label)
      ),
      parts = list(
        c("post", exposure_var),
        c("post", x),
        c("post", x, exposure_var)
      )
    )
  }
  
  extract_all_interactions_for_x <- function(x) {
    
    terms_x <- make_terms_for_x(x)
    
    lpm_no_model <- models_lpm_no[[x]]
    lpm_yes_model <- models_lpm_yes[[x]]
    
    logit_no_model <- models_logit_no[[x]]
    logit_yes_model <- models_logit_yes[[x]]
    
    terms_x %>%
      mutate(
        exposure = exposure_var,
        exposure_label = exposure_label,
        suffix = suffix,
        variable = x,
        lpm_no = map_chr(
          parts,
          ~ extract_one_term(
            lpm_no_model,
            .x,
            "lpm"
          )
        ),
        logit_no = map_chr(
          parts,
          ~ extract_one_term(
            logit_no_model,
            .x,
            "logit_sign"
          )
        ),
        lpm_yes = map_chr(
          parts,
          ~ extract_one_term(
            lpm_yes_model,
            .x,
            "lpm"
          )
        ),
        logit_yes = map_chr(
          parts,
          ~ extract_one_term(
            logit_yes_model,
            .x,
            "logit_sign"
          )
        ),
        n_no = nobs(lpm_no_model),
        n_yes = nobs(lpm_yes_model)
      ) %>%
      select(
        exposure,
        exposure_label,
        suffix,
        variable,
        term_id,
        term_label,
        lpm_no,
        logit_no,
        lpm_yes,
        logit_yes,
        n_no,
        n_yes
      )
  }
  
  triple_all_interactions_table <- map_dfr(
    triple_vars,
    extract_all_interactions_for_x
  ) %>%
    arrange(exposure, variable, term_id)
  
  write.xlsx(
    triple_all_interactions_table,
    paste0("Output/triple_pooled_share_all_interactions_", suffix, ".xlsx"),
    overwrite = TRUE
  )
  
  list(
    triple_key_table = triple_key_table,
    triple_all_interactions_table = triple_all_interactions_table
  )
}

# ---------------------------------------------------------------------------- #
# 8. Correr todo para c70 y c80
# ---------------------------------------------------------------------------- #

post_results <- pmap(
  exposures,
  function(exposure_var, suffix, exposure_label) {
    run_post_pooled_share(
      exposure_var = exposure_var,
      suffix = suffix,
      exposure_label = exposure_label
    )
  }
)

triple_results <- pmap(
  exposures,
  function(exposure_var, suffix, exposure_label) {
    run_triple_pooled_share(
      exposure_var = exposure_var,
      suffix = suffix,
      exposure_label = exposure_label
    )
  }
)

names(post_results) <- exposures$suffix
names(triple_results) <- exposures$suffix

# ---------------------------------------------------------------------------- #
# 9. Guardar tablas combinadas c70 + c80 en Excel
# ---------------------------------------------------------------------------- #

post_combined <- bind_rows(post_results)

triple_key_combined <- map_dfr(
  triple_results,
  "triple_key_table"
)

triple_all_combined <- map_dfr(
  triple_results,
  "triple_all_interactions_table"
)

write.xlsx(
  post_combined,
  "Output/post_pooled_share_lpm_logit_key_coefficients_all.xlsx",
  overwrite = TRUE
)

write.xlsx(
  triple_key_combined,
  "Output/triple_pooled_share_key_coefficients_all.xlsx",
  overwrite = TRUE
)

write.xlsx(
  triple_all_combined,
  "Output/triple_pooled_share_all_interactions_all.xlsx",
  overwrite = TRUE
)

# ---------------------------------------------------------------------------- #
# 10. Exportar tablas combinadas a LaTeX para Overleaf
# ---------------------------------------------------------------------------- #
#
# Formato consistente con los otros scripts:
#   - tabular simple
#   - \hline
#   - notas con \multicolumn
#   - sin adjustbox / threeparttable / landscape
#
# ---------------------------------------------------------------------------- #

safe_tex <- function(x) {
  ifelse(is.na(x), "", latex_escape(x))
}

# ---------------------------------------------------------------------------- #
# 10.1 Tabla Overleaf: Post x pooled share
# ---------------------------------------------------------------------------- #

post_combined_wide <- post_combined %>%
  select(
    exposure,
    exposure_label,
    spec,
    lpm,
    logit_sign,
    n_lpm
  ) %>%
  mutate(
    spec = if_else(spec == "no_controls", "no_controls", "with_controls")
  ) %>%
  tidyr::pivot_wider(
    names_from = spec,
    values_from = c(lpm, logit_sign, n_lpm),
    names_glue = "{spec}_{.value}"
  ) %>%
  arrange(exposure)

post_combined_latex <- post_combined_wide %>%
  mutate(
    exposure_label = safe_tex(exposure_label),
    no_controls_lpm = safe_tex(no_controls_lpm),
    no_controls_logit_sign = safe_tex(no_controls_logit_sign),
    with_controls_lpm = safe_tex(with_controls_lpm),
    with_controls_logit_sign = safe_tex(with_controls_logit_sign)
  )

latex_lines_post <- c(
  "\\begin{table}[!htbp]",
  "\\centering",
  "\\caption{Post-2023 changes by pooled Spanish exposure}",
  "\\label{tab:post_pooled_share_lpm_logit}",
  "\\small",
  "\\begin{tabular}{lcccccc}",
  "\\hline",
  "Exposure & LPM No & Logit No & LPM Yes & Logit Yes & N No & N Yes \\\\",
  "\\hline"
)

for (i in seq_len(nrow(post_combined_latex))) {
  latex_lines_post <- c(
    latex_lines_post,
    paste0(
      post_combined_latex$exposure_label[i],
      " & ",
      post_combined_latex$no_controls_lpm[i],
      " & ",
      post_combined_latex$no_controls_logit_sign[i],
      " & ",
      post_combined_latex$with_controls_lpm[i],
      " & ",
      post_combined_latex$with_controls_logit_sign[i],
      " & ",
      post_combined_latex$no_controls_n_lpm[i],
      " & ",
      post_combined_latex$with_controls_n_lpm[i],
      " \\\\"
    )
  )
}

latex_lines_post <- c(
  latex_lines_post,
  "\\hline",
  "\\multicolumn{7}{l}{\\footnotesize Notes: The dependent variable is migration intention.} \\\\",
  "\\multicolumn{7}{l}{\\footnotesize LPM columns report coefficients with clustered standard errors in parentheses.} \\\\",
  "\\multicolumn{7}{l}{\\footnotesize Logit columns report the sign and statistical significance of the corresponding Logit coefficient.} \\\\",
  "\\multicolumn{7}{l}{\\footnotesize All models include year and municipality fixed effects.} \\\\",
  "\\multicolumn{7}{l}{\\footnotesize Controls include age and male. Standard errors are clustered at the municipality level.} \\\\",
  "\\multicolumn{7}{l}{\\footnotesize * p$<$0.10, ** p$<$0.05, *** p$<$0.01.} \\\\",
  "\\end{tabular}",
  "\\end{table}"
)

writeLines(
  latex_lines_post,
  "Output/tex/post_pooled_share_lpm_logit_key_coefficients_all.tex"
)

# ---------------------------------------------------------------------------- #
# 10.2 Tabla Overleaf: triples principales c70 y c80 en una tabla
# ---------------------------------------------------------------------------- #
#
# Formato analogo a tus tablas con dos shares:
#   Variable | Controls | LPM c70 | Logit c70 | LPM c80 | Logit c80 | N
#
# ---------------------------------------------------------------------------- #

triple_key_wide <- triple_key_combined %>%
  select(
    suffix,
    variable,
    controls,
    coef_triple,
    logit_triple_sign,
    n_lpm
  ) %>%
  tidyr::pivot_wider(
    names_from = suffix,
    values_from = c(coef_triple, logit_triple_sign),
    names_glue = "{.value}_{suffix}"
  ) %>%
  arrange(variable, controls)

triple_key_latex <- triple_key_wide %>%
  mutate(
    variable = safe_tex(label_variable(variable)),
    controls = safe_tex(controls),
    coef_triple_c70 = safe_tex(coef_triple_c70),
    logit_triple_sign_c70 = safe_tex(logit_triple_sign_c70),
    coef_triple_c80 = safe_tex(coef_triple_c80),
    logit_triple_sign_c80 = safe_tex(logit_triple_sign_c80)
  )

latex_lines_triple_key <- c(
  "\\begin{table}[!htbp]",
  "\\centering",
  "\\caption{Triple differences by pooled Spanish exposure}",
  "\\label{tab:triple_pooled_share_key}",
  "\\small",
  "\\begin{tabular}{llccccr}",
  "\\hline",
  "Variable & Controls & LPM c70 & Logit c70 & LPM c80 & Logit c80 & N \\\\",
  "\\hline"
)

for (i in seq_len(nrow(triple_key_latex))) {
  latex_lines_triple_key <- c(
    latex_lines_triple_key,
    paste0(
      triple_key_latex$variable[i],
      " & ",
      triple_key_latex$controls[i],
      " & ",
      triple_key_latex$coef_triple_c70[i],
      " & ",
      triple_key_latex$logit_triple_sign_c70[i],
      " & ",
      triple_key_latex$coef_triple_c80[i],
      " & ",
      triple_key_latex$logit_triple_sign_c80[i],
      " & ",
      triple_key_latex$n_lpm[i],
      " \\\\"
    )
  )
}

latex_lines_triple_key <- c(
  latex_lines_triple_key,
  "\\hline",
  "\\multicolumn{7}{l}{\\footnotesize Notes: The dependent variable is migration intention.} \\\\",
  "\\multicolumn{7}{l}{\\footnotesize Each row reports a separate triple-difference specification.} \\\\",
  "\\multicolumn{7}{l}{\\footnotesize LPM columns report coefficients on $Post \\times X \\times Share$ with clustered standard errors in parentheses.} \\\\",
  "\\multicolumn{7}{l}{\\footnotesize Logit columns report the sign and statistical significance of the corresponding Logit coefficient.} \\\\",
  "\\multicolumn{7}{l}{\\footnotesize All models include year and municipality fixed effects.} \\\\",
  "\\multicolumn{7}{l}{\\footnotesize Controls include age, male, and $Post$ interacted with the remaining pre-2023 municipal characteristics.} \\\\",
  "\\multicolumn{7}{l}{\\footnotesize Standard errors are clustered at the municipality level.} \\\\",
  "\\multicolumn{7}{l}{\\footnotesize * p$<$0.10, ** p$<$0.05, *** p$<$0.01.} \\\\",
  "\\end{tabular}",
  "\\end{table}"
)

writeLines(
  latex_lines_triple_key,
  "Output/tex/triple_pooled_share_key_coefficients_all.tex"
)

# ---------------------------------------------------------------------------- #
# 10.3 Tablas Overleaf: todas las interacciones, una para c70 y otra para c80
# ---------------------------------------------------------------------------- #
#
# Formato consistente con 09_Tables 2:
#   Municipal characteristic | Term | LPM No | Logit No | LPM Yes | Logit Yes | N No | N Yes
#
# ---------------------------------------------------------------------------- #

export_triple_all_one_share <- function(suffix_value) {
  
  exposure_name <- exposures %>%
    filter(suffix == suffix_value) %>%
    pull(exposure_label)
  
  exposure_name_tex <- latex_escape(exposure_name)
  
  out_path <- paste0(
    "Output/tex/triple_pooled_share_all_interactions_",
    suffix_value,
    ".tex"
  )
  
  table_label <- paste0(
    "tab:triple_pooled_share_all_interactions_",
    suffix_value
  )
  
  df_latex <- triple_all_combined %>%
    filter(suffix == suffix_value) %>%
    mutate(
      variable_label = safe_tex(label_variable(variable)),
      term_label = safe_tex(term_label),
      lpm_no = safe_tex(lpm_no),
      logit_no = safe_tex(logit_no),
      lpm_yes = safe_tex(lpm_yes),
      logit_yes = safe_tex(logit_yes)
    ) %>%
    group_by(variable) %>%
    mutate(
      variable_print = if_else(row_number() == 1, variable_label, "")
    ) %>%
    ungroup()
  
  latex_lines <- c(
    "\\begin{table}[!htbp]",
    "\\centering",
    paste0(
      "\\caption{Triple-difference models: all interaction terms, ",
      exposure_name_tex,
      "}"
    ),
    paste0("\\label{", table_label, "}"),
    "\\scriptsize",
    "\\begin{tabular}{llcccccc}",
    "\\hline",
    "Municipal characteristic & Term & LPM No & Logit No & LPM Yes & Logit Yes & N No & N Yes \\\\",
    "\\hline"
  )
  
  for (i in seq_len(nrow(df_latex))) {
    
    latex_lines <- c(
      latex_lines,
      paste0(
        df_latex$variable_print[i],
        " & ",
        df_latex$term_label[i],
        " & ",
        df_latex$lpm_no[i],
        " & ",
        df_latex$logit_no[i],
        " & ",
        df_latex$lpm_yes[i],
        " & ",
        df_latex$logit_yes[i],
        " & ",
        df_latex$n_no[i],
        " & ",
        df_latex$n_yes[i],
        " \\\\"
      )
    )
    
    if (i %% 3 == 0 && i < nrow(df_latex)) {
      latex_lines <- c(
        latex_lines,
        "\\hline"
      )
    }
  }
  
  latex_lines <- c(
    latex_lines,
    "\\hline",
    "\\multicolumn{8}{l}{\\footnotesize Notes: The dependent variable is migration intention.} \\\\",
    "\\multicolumn{8}{l}{\\footnotesize Each block reports coefficients from a separate triple-difference specification for municipal characteristic $X$.} \\\\",
    "\\multicolumn{8}{l}{\\footnotesize LPM columns report coefficients with clustered standard errors in parentheses.} \\\\",
    "\\multicolumn{8}{l}{\\footnotesize Logit columns report the sign and statistical significance of the corresponding Logit coefficient.} \\\\",
    "\\multicolumn{8}{l}{\\footnotesize All models include year and municipality fixed effects.} \\\\",
    "\\multicolumn{8}{l}{\\footnotesize Controls Yes includes age, male, and $Post$ interacted with the remaining pre-2023 municipal characteristics.} \\\\",
    "\\multicolumn{8}{l}{\\footnotesize $N$ reports the number of observations in the corresponding LPM specification.} \\\\",
    "\\multicolumn{8}{l}{\\footnotesize Standard errors are clustered at the municipality level. * p$<$0.10, ** p$<$0.05, *** p$<$0.01.} \\\\",
    "\\end{tabular}",
    "\\end{table}"
  )
  
  writeLines(latex_lines, out_path)
}

walk(exposures$suffix, export_triple_all_one_share)

writeLines(
  c(
    "\\input{triple_pooled_share_all_interactions_c70.tex}",
    "\\input{triple_pooled_share_all_interactions_c80.tex}"
  ),
  "Output/tex/triple_pooled_share_all_interactions_all.tex"
)

cat("\nTablas LaTeX para Overleaf guardadas en:\n")
cat("  Output/tex/post_pooled_share_lpm_logit_key_coefficients_all.tex\n")
cat("  Output/tex/triple_pooled_share_key_coefficients_all.tex\n")
cat("  Output/tex/triple_pooled_share_all_interactions_c70.tex\n")
cat("  Output/tex/triple_pooled_share_all_interactions_c80.tex\n")
cat("  Output/tex/triple_pooled_share_all_interactions_all.tex\n")

# ---------------------------------------------------------------------------- #
# 11. Mensaje final
# ---------------------------------------------------------------------------- #

cat("\n============================================================\n")
cat("CODIGO TERMINADO CORRECTAMENTE\n")
cat("============================================================\n")
cat("Se corrieron por separado: share_36_78_c70 y share_36_78_c80\n")
cat("\nOutputs principales Excel:\n")
cat("  Output/post_pooled_share_lpm_logit_key_coefficients_all.xlsx\n")
cat("  Output/triple_pooled_share_key_coefficients_all.xlsx\n")
cat("  Output/triple_pooled_share_all_interactions_all.xlsx\n")
cat("\nOutputs Overleaf:\n")
cat("  Output/tex/post_pooled_share_lpm_logit_key_coefficients_all.tex\n")
cat("  Output/tex/triple_pooled_share_key_coefficients_all.tex\n")
cat("  Output/tex/triple_pooled_share_all_interactions_all.tex\n")
cat("\nOutputs por share en Output/ y Output/models/ con sufijos c70 y c80.\n")