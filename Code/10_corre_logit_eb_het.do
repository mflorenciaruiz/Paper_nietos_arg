* ============================================================================ *
*                 Corre logit con EB y Efectos heterogéneos
* ============================================================================ *

* --- 0. Setup --- *
clear all
set more off

global main "/Users/florenciaruiz/BID 2/Paper Valerie/Nietos/Argentina/Paper_nietos_arg"
global data_int "$main/Data Int"
global data_out "$main/Data Out"
global data_raw "$main/Data Raw"
global output   "$main/Output"
global output_eb "$main/Output/EB"

* ============================================================================ *
* Parte 1
*
* Logit con pesos de Entropy Balancing - Intencion de Migrar
* Estima 6 modelos (uno por peso EB), calcula AMEs con SE clusterizados,
* y exporta a Excel para importar en R.
* ============================================================================ *
{
* --- 1.1. Cargar data --- *
use "$data_int/data_EB_v2.dta", clear

keep mun_code w_m_*

merge 1:m mun_code using "$data_out/lapop_data_merge.dta"
br if _merge ==1
drop if _merge == 1 // elimino los que no mergean (tienen los missings en los pesos)

* --- 1.2. Preparar variables --- *
gen post = (year >= 2022)

gen post_share_1936 = post * share_1936_1955
gen post_share_1956 = post * share_1956_1978

label variable post_share_1936 "Post x Spanish share 1936-1955"
label variable post_share_1956 "Post x Spanish share 1956-1978"

tab year, gen(year_d)
gen year12_share_1936 = year_d1 * share_1936_1955
gen year14_share_1936 = year_d2 * share_1936_1955
gen year17_share_1936 = year_d3 * share_1936_1955
gen year19_share_1936 = year_d4 * share_1936_1955
gen year23_share_1936 = year_d5 * share_1936_1955

label variable year12_share_1936 "Year 2012 x Spanish share 1936-1955"
label variable year14_share_1936 "Year 2014 x Spanish share 1936-1955"
label variable year17_share_1936 "Year 2017 x Spanish share 1936-1955"
label variable year19_share_1936 "Year 2019 x Spanish share 1936-1955"
label variable year23_share_1936 "Year 2023 x Spanish share 1936-1955"

gen year12_share_1956 = year_d1 * share_1956_1978
gen year14_share_1956 = year_d2 * share_1956_1978
gen year17_share_1956 = year_d3 * share_1956_1978
gen year19_share_1956 = year_d4 * share_1956_1978
gen year23_share_1956 = year_d5 * share_1956_1978

label variable year12_share_1956 "Year 2012 x Spanish share 1956-1978"
label variable year14_share_1956 "Year 2014 x Spanish share 1956-1978"
label variable year17_share_1956 "Year 2017 x Spanish share 1956-1978"
label variable year19_share_1956 "Year 2019 x Spanish share 1956-1978"
label variable year23_share_1956 "Year 2023 x Spanish share 1956-1978"

tab mun_code if  w_m_1956_3  == . // hay 3 missing porque no tiene intencion de migrar en 2014 entonces no se generan los pesos
egen mun_code_num = group(mun_code)
order mun_code mun_code_num
sort mun_code_num year

drop if missing(w_m_1936_1, w_m_1956_1, w_m_1936_4, w_m_1956_4, w_m_1936_5, w_m_1956_5)

save "$data_int/aux.dta", replace

* --- 1.3. Correr logit para el ATT --- *

* Lista de pesos EB
local eb_weights "w_m_1936_1 w_m_1956_1 w_m_1936_4 w_m_1956_4 w_m_1936_5 w_m_1956_5"

tempname resfile
tempfile results
postfile `resfile' str30 weight_var str3 controls ///
    double ame_36 se_36 p_36 ///
    double ame_56 se_56 p_56 ///
    long nobs double pseudo_r2 ///
    using `results', replace

foreach w of local eb_weights {
    foreach ctrl_label in "No" "Yes" {
        
        * Definir la lista de controles segun la etiqueta
        if "`ctrl_label'" == "Yes" {
            local ctrls "edad hombre"
        }
        else {
            local ctrls ""
        }
        
        display _newline "==========================================="
        display "Peso: `w', Controles individuales: `ctrl_label'"
        display "==========================================="
        
        logit intencion_migrar post_share_1936 post_share_1956 `ctrls' ///
              i.year i.mun_code_num ///
              [pw=`w'], cluster(mun_code)
        
        local N  = e(N)
        local r2 = e(r2_p)
        
        margins, dydx(post_share_1936 post_share_1956) post
        matrix M = r(table)
        
        local ame_36 = M[1,1]
        local se_36  = M[2,1]
        local p_36   = M[4,1]
        local ame_56 = M[1,2]
        local se_56  = M[2,2]
        local p_56   = M[4,2]
        
        post `resfile' ("`w'") ("`ctrl_label'") ///
                       (`ame_36') (`se_36') (`p_36') ///
                       (`ame_56') (`se_56') (`p_56') ///
                       (`N') (`r2')
    }
}

postclose `resfile'

* --- 1.4. Cargar resultados y exportar a Excel --- *
use `results', clear

save "$output_eb/logit_eb_results.dta", replace
export excel using "$output_eb/logit_eb_results.xlsx", firstrow(variables) replace

* --- 1.5. Correr logit para event study de 1936 --- *
use "$data_int/aux.dta", clear

tempname resfile
tempfile results
postfile `resfile' str30 weight_var str3 controls ///
    double ame_36_12 se_36_12 p_36_12 ///
    double ame_36_14 se_36_14 p_36_14 ///
	double ame_36_17 se_36_17 p_36_17 ///
	double ame_36_23 se_36_23 p_36_23 ///
    long nobs double pseudo_r2 ///
    using `results', replace

* Definir la lista incluyendo no_w como primera especificacion
local eb_weights_36 "no_w w_m_1936_1  w_m_1936_4 w_m_1936_5"

foreach w of local eb_weights_36 {
    foreach ctrl_label in "No" "Yes" {
        
        if "`ctrl_label'" == "Yes" {
            local ctrls "edad hombre"
        }
        else {
            local ctrls ""
        }
        
        display _newline "==========================================="
        display "Peso: `w', Controles individuales: `ctrl_label'"
        display "==========================================="
        
        if "`w'" == "no_w" {
            logit intencion_migrar year12_share_1936 year14_share_1936 ///
                  year17_share_1936 year23_share_1936 `ctrls' ///
                  i.year i.mun_code_num, cluster(mun_code)
        }
        else {
            logit intencion_migrar year12_share_1936 year14_share_1936 ///
                  year17_share_1936 year23_share_1936 `ctrls' ///
                  i.year i.mun_code_num [pw=`w'], cluster(mun_code)
        }
        
        local N  = e(N)
        local r2 = e(r2_p)
        
        margins, dydx(year12_share_1936 year14_share_1936 ///
                      year17_share_1936 year23_share_1936) post
        matrix M = r(table)
        
        local ame_36_12 = M[1,1]
        local se_36_12  = M[2,1]
        local p_36_12   = M[4,1]
        local ame_36_14 = M[1,2]
        local se_36_14  = M[2,2]
        local p_36_14   = M[4,2]
        local ame_36_17 = M[1,3]
        local se_36_17  = M[2,3]
        local p_36_17   = M[4,3]
        local ame_36_23 = M[1,4]
        local se_36_23  = M[2,4]
        local p_36_23   = M[4,4]
        
        post `resfile' ("`w'") ("`ctrl_label'") ///
                       (`ame_36_12') (`se_36_12') (`p_36_12') ///
                       (`ame_36_14') (`se_36_14') (`p_36_14') ///
                       (`ame_36_17') (`se_36_17') (`p_36_17') ///
                       (`ame_36_23') (`se_36_23') (`p_36_23') ///
                       (`N') (`r2')
    }
}

postclose `resfile'

* --- 1.6. Cargar resultados y exportar a Excel --- *
use `results', clear

save "$output_eb/logit_eb_results_event_36.dta", replace
export excel using "$output_eb/logit_eb_results_event_36.xlsx", firstrow(variables) replace


* --- 1.5. Correr logit para event study de 1956 --- *
use "$data_int/aux.dta", clear

tempname resfile
tempfile results
postfile `resfile' str30 weight_var str3 controls ///
    double ame_56_12 se_56_12 p_56_12 ///
    double ame_56_14 se_56_14 p_56_14 ///
	double ame_56_17 se_56_17 p_56_17 ///
	double ame_56_23 se_56_23 p_56_23 ///
    long nobs double pseudo_r2 ///
    using `results', replace

* Definir la lista incluyendo no_w como primera especificacion
local eb_weights_56 "no_w w_m_1956_1  w_m_1956_4 w_m_1956_5"

foreach w of local eb_weights_56 {
    foreach ctrl_label in "No" "Yes" {
        
        if "`ctrl_label'" == "Yes" {
            local ctrls "edad hombre"
        }
        else {
            local ctrls ""
        }
        
        display _newline "==========================================="
        display "Peso: `w', Controles individuales: `ctrl_label'"
        display "==========================================="
        
        if "`w'" == "no_w" {
            logit intencion_migrar year12_share_1956 year14_share_1956 ///
                  year17_share_1956 year23_share_1956 `ctrls' ///
                  i.year i.mun_code_num, cluster(mun_code)
        }
        else {
            logit intencion_migrar year12_share_1956 year14_share_1956 ///
                  year17_share_1956 year23_share_1956 `ctrls' ///
                  i.year i.mun_code_num [pw=`w'], cluster(mun_code)
        }
        
        local N  = e(N)
        local r2 = e(r2_p)
        
        margins, dydx(year12_share_1956 year14_share_1956 ///
                      year17_share_1956 year23_share_1956) post
        matrix M = r(table)
        
        local ame_56_12 = M[1,1]
        local se_56_12  = M[2,1]
        local p_56_12   = M[4,1]
        local ame_56_14 = M[1,2]
        local se_56_14  = M[2,2]
        local p_56_14   = M[4,2]
        local ame_56_17 = M[1,3]
        local se_56_17  = M[2,3]
        local p_56_17   = M[4,3]
        local ame_56_23 = M[1,4]
        local se_56_23  = M[2,4]
        local p_56_23   = M[4,4]
        
        post `resfile' ("`w'") ("`ctrl_label'") ///
                       (`ame_56_12') (`se_56_12') (`p_56_12') ///
                       (`ame_56_14') (`se_56_14') (`p_56_14') ///
                       (`ame_56_17') (`se_56_17') (`p_56_17') ///
                       (`ame_56_23') (`se_56_23') (`p_56_23') ///
                       (`N') (`r2')
    }
}

postclose `resfile'

* --- 1.6. Cargar resultados y exportar a Excel --- *
use `results', clear

save "$output_eb/logit_eb_results_event_56.dta", replace
export excel using "$output_eb/logit_eb_results_event_56.xlsx", firstrow(variables) replace

}

* ============================================================================ *
* Parte 2
*
* Efectos heterogéneos por terciles
* Nota: los terciles se calcula sobre la muestra completa de los 312 municipios
* en LAPOP solo hay 56 municipios, pero no se recalculan los terciles
* ============================================================================ *
{
pause on
* --- 2.1. Crear local de limites de terciles --- *
use "$data_out/data_eff_het.dta", clear

local tercile_vars "t_density_2010 t_fem_2010 t_mean_schyr_2010 t_med_dage_2010 t_pea_2010 t_unemp_2010 t_izam_pre_avg t_alt_pre_avg"
local source_vars  "popdensgeo2_2010 share_female_2010 mean_yrschool_2010 median_age_2010 share_laborforce_2010 share_unemployed_2010 share_izq_amplia_pre_avg share_alt_pre_avg"

local i = 1

foreach tv of local tercile_vars {
    local sv : word `i' of `source_vars'
    forvalues t = 1/3 {
        quietly summarize `sv' if `tv' == `t'
        * Nombre del local que queremos crear
        local ubname "ub_`tv'_`t'"
        * Crear el local dinámico
        local `ubname' = r(max)
        * Verificar
        display "`tv', tercile `t': ``ubname''"
    }
    local ++i
}

* --- 2.2. Cargar data y settings --- *
use "$data_out/lapop_data_merge.dta", clear

gen post = (year >= 2022)

gen post_share_1936 = post * share_1936_1955
gen post_share_1956 = post * share_1956_1978

label variable post_share_1936 "Post x Spanish share 1936-1955"
label variable post_share_1956 "Post x Spanish share 1956-1978"

egen mun_code_num = group(mun_code)

merge m:1 mun_code using "$data_out/data_eff_het.dta"

drop if _merge ==2
drop _merge

local tercile_vars "t_density_2010 t_fem_2010 t_mean_schyr_2010 t_med_dage_2010 t_pea_2010 t_unemp_2010 t_izam_pre_avg t_alt_pre_avg"
local source_vars  "popdensgeo2_2010   share_female_2010   mean_yrschool_2010   median_age_2010   share_laborforce_2010   share_unemployed_2010   share_izq_amplia_pre_avg   share_alt_pre_avg"

* Postfile 1: tabla principal (una fila por tercile_var x tercile) ---
tempfile results_per_tercile
tempname resfile1
postfile `resfile1' str30 tercile_var byte tercile ///
    double ame_36 se_36 p_36 ///
    double ame_56 se_56 p_56 ///
    long nobs double pseudo_r2 ///
    double p_equal_cohorts ///
    double tercile_ub ///
    using `results_per_tercile', replace

* Postfile 2: joint tests entre T1/T2/T3 (una fila por tercile_var) ---
tempfile results_joint
tempname resfile2
postfile `resfile2' str30 tercile_var ///
    double p_joint_36 p_joint_56 ///
    using `results_joint', replace

* Postfile 3: pairwise tests (una fila por tercile_var) ---
tempfile results_pairwise
tempname resfile3
postfile `resfile3' str30 tercile_var ///
    double p_36_T1T2 p_36_T1T3 p_36_T2T3 ///
    double p_56_T1T2 p_56_T1T3 p_56_T2T3 ///
    using `results_pairwise', replace

* --- 2.3 Loop principal --- *
local i = 1
foreach tv of local tercile_vars {
    local sv : word `i' of `source_vars'
    local ++i
    
    display _newline _newline "==========================================="
    display "Variable de tercil: `tv'  (source: `sv')"
    display "==========================================="
    
    matrix betas_36 = J(3, 1, .)
    matrix vars_36  = J(3, 3, 0)
    matrix betas_56 = J(3, 1, .)
    matrix vars_56  = J(3, 3, 0)
    
    forvalues t = 1/3 {
        display _newline "--- Tercil `t' ---"
        
        logit intencion_migrar post_share_1936 post_share_1956 ///
              edad hombre i.year i.mun_code_num ///
              if `tv' == `t', cluster(mun_code)
        
        local N  = e(N)
        local r2 = e(r2_p)
        
        margins, dydx(post_share_1936 post_share_1956) post
        matrix M = r(table)
        
        local ame_36_t = M[1,1]
        local se_36_t  = M[2,1]
        local p_36_t   = M[4,1]
        local ame_56_t = M[1,2]
        local se_56_t  = M[2,2]
        local p_56_t   = M[4,2]
        
        test post_share_1936 = post_share_1956
        local p_equal_t = r(p)
        
        matrix betas_36[`t', 1]  = `ame_36_t'
        matrix vars_36[`t', `t'] = `se_36_t' * `se_36_t'
        matrix betas_56[`t', 1]  = `ame_56_t'
        matrix vars_56[`t', `t'] = `se_56_t' * `se_56_t'
        
        local ubname "ub_`tv'_`t'"
		local ub ``ubname''

		display "Macro buscado: `ubname'"
		display "Upper bound: `ub'"
        
        post `resfile1' ("`tv'") (`t') ///
                       (`ame_36_t') (`se_36_t') (`p_36_t') ///
                       (`ame_56_t') (`se_56_t') (`p_56_t') ///
                       (`N') (`r2') ///
                       (`p_equal_t') (`ub')
    }
    
    * --- Joint tests (T1 = T2 = T3) ---
    matrix R = (1, -1, 0 \ 0, 1, -1)
    
    matrix Rb_36  = R * betas_36
    matrix RVR_36 = R * vars_36 * R'
    matrix W_36   = Rb_36' * invsym(RVR_36) * Rb_36
    local p_joint_36 = chi2tail(2, W_36[1,1])
    
    matrix Rb_56  = R * betas_56
    matrix RVR_56 = R * vars_56 * R'
    matrix W_56   = Rb_56' * invsym(RVR_56) * Rb_56
    local p_joint_56 = chi2tail(2, W_56[1,1])
    
    post `resfile2' ("`tv'") (`p_joint_36') (`p_joint_56')
    
    * --- Pairwise tests ---
    local b1_36 = betas_36[1,1]
    local b2_36 = betas_36[2,1]
    local b3_36 = betas_36[3,1]
    local v1_36 = vars_36[1,1]
    local v2_36 = vars_36[2,2]
    local v3_36 = vars_36[3,3]
    
    local p_36_T1T2 = 2 * (1 - normal(abs((`b1_36' - `b2_36') / sqrt(`v1_36' + `v2_36'))))
    local p_36_T1T3 = 2 * (1 - normal(abs((`b1_36' - `b3_36') / sqrt(`v1_36' + `v3_36'))))
    local p_36_T2T3 = 2 * (1 - normal(abs((`b2_36' - `b3_36') / sqrt(`v2_36' + `v3_36'))))
    
    local b1_56 = betas_56[1,1]
    local b2_56 = betas_56[2,1]
    local b3_56 = betas_56[3,1]
    local v1_56 = vars_56[1,1]
    local v2_56 = vars_56[2,2]
    local v3_56 = vars_56[3,3]
    
    local p_56_T1T2 = 2 * (1 - normal(abs((`b1_56' - `b2_56') / sqrt(`v1_56' + `v2_56'))))
    local p_56_T1T3 = 2 * (1 - normal(abs((`b1_56' - `b3_56') / sqrt(`v1_56' + `v3_56'))))
    local p_56_T2T3 = 2 * (1 - normal(abs((`b2_56' - `b3_56') / sqrt(`v2_56' + `v3_56'))))
    
    display "Pairwise 36-55: T1=T2 `p_36_T1T2'  T1=T3 `p_36_T1T3'  T2=T3 `p_36_T2T3'"
    display "Pairwise 56-78: T1=T2 `p_56_T1T2'  T1=T3 `p_56_T1T3'  T2=T3 `p_56_T2T3'"
    
    post `resfile3' ("`tv'") ///
                   (`p_36_T1T2') (`p_36_T1T3') (`p_36_T2T3') ///
                   (`p_56_T1T2') (`p_56_T1T3') (`p_56_T2T3')
}

postclose `resfile1'
postclose `resfile2'
postclose `resfile3'

* ---  2.4 Tabla principal: merge y export ---*

use `results_per_tercile', clear

tempfile per_tercile_saved
save `per_tercile_saved', replace

use `results_joint', clear

merge 1:m tercile_var using `per_tercile_saved', nogenerate
sort tercile_var tercile
order tercile_var tercile ame_36 se_36 p_36 ame_56 se_56 p_56 ///
      p_equal_cohorts nobs pseudo_r2 tercile_ub p_joint_36 p_joint_56

export excel using "$output_eb/heterog_terciles_migration.xlsx", firstrow(variables) replace

* --- 2.5 Tabla pairwise: export ---*

use `results_pairwise', clear
list

export excel using "$output_eb/pairwise_terciles_migration.xlsx", firstrow(variables) replace
}

* ============================================================================ *
* Parte 3
*
* Efectos heterogéneos por terciles (variables de LAPOP)
* Nota: los terciles se calcula sobre la muestra de LAPOP, las variables de LAPOP
* se promedian directamente por municipio.
* ============================================================================ *
{
* --- 3.1. Crear local de limites de terciles --- *
use "$data_out/lapop_data_merge.dta", clear
keep if year != 2023

tab interes_pol_mucho, m
tab en_pareja, m
tab  mun_code if en_pareja == . 
tab  mun_code if interes_pol_mucho==.
bys mun_code: sum interes_pol_mucho
bys mun_code: sum en_pareja 
	// CUANDO ELIMINO 2023 HAY DOS MUNICIPIOS MENOS : 56 -> 54

collapse (mean) interes_pol_mucho en_pareja, by(mun_code)

xtile t_en_pareja         = en_pareja, nq(3)
xtile t_interes_pol_mucho = interes_pol_mucho, nq(3)

tempfile terciles_lapop
save `terciles_lapop', replace

local tercile_vars "t_en_pareja t_interes_pol_mucho"
local source_vars  "en_pareja interes_pol_mucho"

local i = 1

foreach tv of local tercile_vars {
    local sv : word `i' of `source_vars'
    forvalues t = 1/3 {
        quietly summarize `sv' if `tv' == `t'
        * Nombre del local que queremos crear
        local ubname "ub_`tv'_`t'"
        * Crear el local dinámico
        local `ubname' = r(max)
        * Verificar
        display "`tv', tercile `t': ``ubname''"
    }
    local ++i
}

* --- 3.2. Cargar data y settings --- *
use "$data_out/lapop_data_merge.dta", clear

gen post = (year >= 2022)

gen post_share_1936 = post * share_1936_1955
gen post_share_1956 = post * share_1956_1978

label variable post_share_1936 "Post x Spanish share 1936-1955"
label variable post_share_1956 "Post x Spanish share 1956-1978"

egen mun_code_num = group(mun_code)

merge m:1 mun_code using `terciles_lapop'

drop if _merge ==2
drop _merge

local tercile_vars "t_en_pareja t_interes_pol_mucho"
local source_vars  "en_pareja interes_pol_mucho"

* Postfile 1: tabla principal (una fila por tercile_var x tercile) ---
tempfile results_per_tercile
tempname resfile1
postfile `resfile1' str30 tercile_var byte tercile ///
    double ame_36 se_36 p_36 ///
    double ame_56 se_56 p_56 ///
    long nobs double pseudo_r2 ///
    double p_equal_cohorts ///
    double tercile_ub ///
    using `results_per_tercile', replace

* Postfile 2: joint tests entre T1/T2/T3 (una fila por tercile_var) ---
tempfile results_joint
tempname resfile2
postfile `resfile2' str30 tercile_var ///
    double p_joint_36 p_joint_56 ///
    using `results_joint', replace

* Postfile 3: pairwise tests (una fila por tercile_var) ---
tempfile results_pairwise
tempname resfile3
postfile `resfile3' str30 tercile_var ///
    double p_36_T1T2 p_36_T1T3 p_36_T2T3 ///
    double p_56_T1T2 p_56_T1T3 p_56_T2T3 ///
    using `results_pairwise', replace

* --- 3.3 Loop principal --- *
local i = 1
foreach tv of local tercile_vars {
    local sv : word `i' of `source_vars'
    local ++i
    
    display _newline _newline "==========================================="
    display "Variable de tercil: `tv'  (source: `sv')"
    display "==========================================="
    
    matrix betas_36 = J(3, 1, .)
    matrix vars_36  = J(3, 3, 0)
    matrix betas_56 = J(3, 1, .)
    matrix vars_56  = J(3, 3, 0)
    
    forvalues t = 1/3 {
        display _newline "--- Tercil `t' ---"
        
        logit intencion_migrar post_share_1936 post_share_1956 ///
              edad hombre i.year i.mun_code_num ///
              if `tv' == `t', cluster(mun_code)
        
        local N  = e(N)
        local r2 = e(r2_p)
        
        margins, dydx(post_share_1936 post_share_1956) post
        matrix M = r(table)
        
        local ame_36_t = M[1,1]
        local se_36_t  = M[2,1]
        local p_36_t   = M[4,1]
        local ame_56_t = M[1,2]
        local se_56_t  = M[2,2]
        local p_56_t   = M[4,2]
        
        test post_share_1936 = post_share_1956
        local p_equal_t = r(p)
        
        matrix betas_36[`t', 1]  = `ame_36_t'
        matrix vars_36[`t', `t'] = `se_36_t' * `se_36_t'
        matrix betas_56[`t', 1]  = `ame_56_t'
        matrix vars_56[`t', `t'] = `se_56_t' * `se_56_t'
        
        local ubname "ub_`tv'_`t'"
		local ub ``ubname''

		display "Macro buscado: `ubname'"
		display "Upper bound: `ub'"
        
        post `resfile1' ("`tv'") (`t') ///
                       (`ame_36_t') (`se_36_t') (`p_36_t') ///
                       (`ame_56_t') (`se_56_t') (`p_56_t') ///
                       (`N') (`r2') ///
                       (`p_equal_t') (`ub')
    }
    
    * --- Joint tests (T1 = T2 = T3) ---
    matrix R = (1, -1, 0 \ 0, 1, -1)
    
    matrix Rb_36  = R * betas_36
    matrix RVR_36 = R * vars_36 * R'
    matrix W_36   = Rb_36' * invsym(RVR_36) * Rb_36
    local p_joint_36 = chi2tail(2, W_36[1,1])
    
    matrix Rb_56  = R * betas_56
    matrix RVR_56 = R * vars_56 * R'
    matrix W_56   = Rb_56' * invsym(RVR_56) * Rb_56
    local p_joint_56 = chi2tail(2, W_56[1,1])
    
    post `resfile2' ("`tv'") (`p_joint_36') (`p_joint_56')
    
    * --- Pairwise tests ---
    local b1_36 = betas_36[1,1]
    local b2_36 = betas_36[2,1]
    local b3_36 = betas_36[3,1]
    local v1_36 = vars_36[1,1]
    local v2_36 = vars_36[2,2]
    local v3_36 = vars_36[3,3]
    
    local p_36_T1T2 = 2 * (1 - normal(abs((`b1_36' - `b2_36') / sqrt(`v1_36' + `v2_36'))))
    local p_36_T1T3 = 2 * (1 - normal(abs((`b1_36' - `b3_36') / sqrt(`v1_36' + `v3_36'))))
    local p_36_T2T3 = 2 * (1 - normal(abs((`b2_36' - `b3_36') / sqrt(`v2_36' + `v3_36'))))
    
    local b1_56 = betas_56[1,1]
    local b2_56 = betas_56[2,1]
    local b3_56 = betas_56[3,1]
    local v1_56 = vars_56[1,1]
    local v2_56 = vars_56[2,2]
    local v3_56 = vars_56[3,3]
    
    local p_56_T1T2 = 2 * (1 - normal(abs((`b1_56' - `b2_56') / sqrt(`v1_56' + `v2_56'))))
    local p_56_T1T3 = 2 * (1 - normal(abs((`b1_56' - `b3_56') / sqrt(`v1_56' + `v3_56'))))
    local p_56_T2T3 = 2 * (1 - normal(abs((`b2_56' - `b3_56') / sqrt(`v2_56' + `v3_56'))))
    
    display "Pairwise 36-55: T1=T2 `p_36_T1T2'  T1=T3 `p_36_T1T3'  T2=T3 `p_36_T2T3'"
    display "Pairwise 56-78: T1=T2 `p_56_T1T2'  T1=T3 `p_56_T1T3'  T2=T3 `p_56_T2T3'"
    
    post `resfile3' ("`tv'") ///
                   (`p_36_T1T2') (`p_36_T1T3') (`p_36_T2T3') ///
                   (`p_56_T1T2') (`p_56_T1T3') (`p_56_T2T3')
}

postclose `resfile1'
postclose `resfile2'
postclose `resfile3'

* ---  3.4 Tabla principal: merge y export ---*

use `results_per_tercile', clear

tempfile per_tercile_saved
save `per_tercile_saved', replace

use `results_joint', clear

merge 1:m tercile_var using `per_tercile_saved', nogenerate
sort tercile_var tercile
order tercile_var tercile ame_36 se_36 p_36 ame_56 se_56 p_56 ///
      p_equal_cohorts nobs pseudo_r2 tercile_ub p_joint_36 p_joint_56

export excel using "$output/heterog_terciles_migration_lapop.xlsx", firstrow(variables) replace

* --- 3.5 Tabla pairwise: export ---*

use `results_pairwise', clear
list

export excel using "$output/pairwise_terciles_migration_lapop.xlsx", firstrow(variables) replace
}

* ============================================================================ *
* Parte 4
*
* Efectos heterogéneos por la media 
* Nota: la media se calcula sobre la muestra completa de los 312 municipios
* en LAPOP solo hay 56 municipios, pero no se recalculan los terciles
* ============================================================================ *
{
* --- 4.1. Crear local de limites por grupo (Below/Above) ---*
use "$data_out/data_eff_het.dta", clear

local median_vars "p50_density_2010 p50_fem_2010 p50_mean_schyr_2010 p50_med_dage_2010 p50_pea_2010 p50_unemp_2010 p50_izam_pre_avg p50_alt_pre_avg"
local source_vars "popdensgeo2_2010 share_female_2010 mean_yrschool_2010 median_age_2010 share_laborforce_2010 share_unemployed_2010 share_izq_amplia_pre_avg share_alt_pre_avg"

local i = 1
foreach mv of local median_vars {
    local sv : word `i' of `source_vars'
    forvalues m = 1/2 {
        quietly summarize `sv' if `mv' == `m'
        local ubname "ub_`mv'_`m'"
        local `ubname' = r(max)
        display "`mv', group `m': ``ubname''"
    }
    local ++i
}

* --- 4.2. Cargar data y settings ---*
use "$data_out/lapop_data_merge.dta", clear
gen post = (year >= 2022)
gen post_share_1936 = post * share_1936_1955
gen post_share_1956 = post * share_1956_1978
label variable post_share_1936 "Post x Spanish share 1936-1955"
label variable post_share_1956 "Post x Spanish share 1956-1978"
egen mun_code_num = group(mun_code)
merge m:1 mun_code using "$data_out/data_eff_het.dta"
drop if _merge == 2
drop _merge

local median_vars "p50_density_2010 p50_fem_2010 p50_mean_schyr_2010 p50_med_dage_2010 p50_pea_2010 p50_unemp_2010 p50_izam_pre_avg p50_alt_pre_avg"
local source_vars "popdensgeo2_2010 share_female_2010 mean_yrschool_2010 median_age_2010 share_laborforce_2010 share_unemployed_2010 share_izq_amplia_pre_avg share_alt_pre_avg"

* Postfile 1: tabla principal (una fila por median_var x grupo)
tempfile results_per_median
tempname resfile1
postfile `resfile1' str30 median_var byte group ///
    double ame_36 se_36 p_36 ///
    double ame_56 se_56 p_56 ///
    long nobs double pseudo_r2 ///
    double p_equal_cohorts ///
    double group_ub ///
    using `results_per_median', replace

* Postfile 2: test Below vs Above por median_var (una fila por median_var)
tempfile results_pairwise
tempname resfile2
postfile `resfile2' str30 median_var ///
    double p_pw_36 p_pw_56 ///
    using `results_pairwise', replace

* --- 4.3 Loop principal ---*
local i = 1
foreach mv of local median_vars {
    local sv : word `i' of `source_vars'
    local ++i
    
    display _newline _newline "==========================================="
    display "Variable de mediana: `mv' (source: `sv')"
    display "==========================================="
    
    * Matrices de dimensión 2 (Below y Above)
    matrix betas_36 = J(2, 1, .)
    matrix vars_36  = J(2, 2, 0)
    matrix betas_56 = J(2, 1, .)
    matrix vars_56  = J(2, 2, 0)
    
    forvalues m = 1/2 {
        display _newline "--- Grupo `m' (1=Below, 2=Above) ---"
        
        logit intencion_migrar post_share_1936 post_share_1956 ///
              edad hombre i.year i.mun_code_num ///
              if `mv' == `m', cluster(mun_code)
        
        local N  = e(N)
        local r2 = e(r2_p)
        
        margins, dydx(post_share_1936 post_share_1956) post
        matrix M = r(table)
        
        local ame_36_m = M[1,1]
        local se_36_m  = M[2,1]
        local p_36_m   = M[4,1]
        local ame_56_m = M[1,2]
        local se_56_m  = M[2,2]
        local p_56_m   = M[4,2]
        
        test post_share_1936 = post_share_1956
        local p_equal_m = r(p)
        
        matrix betas_36[`m', 1]  = `ame_36_m'
        matrix vars_36[`m', `m'] = `se_36_m' * `se_36_m'
        matrix betas_56[`m', 1]  = `ame_56_m'
        matrix vars_56[`m', `m'] = `se_56_m' * `se_56_m'
        
        local ubname "ub_`mv'_`m'"
        local ub ``ubname''
        display "Upper bound del grupo `m': `ub'"
        
        post `resfile1' ("`mv'") (`m') ///
                       (`ame_36_m') (`se_36_m') (`p_36_m') ///
                       (`ame_56_m') (`se_56_m') (`p_56_m') ///
                       (`N') (`r2') ///
                       (`p_equal_m') (`ub')
    }
    
    * --- Test Below vs Above (z-stat de la diferencia entre subsamples independientes) ---
    local b1_36 = betas_36[1,1]
    local b2_36 = betas_36[2,1]
    local v1_36 = vars_36[1,1]
    local v2_36 = vars_36[2,2]
    local p_pw_36 = 2 * (1 - normal(abs((`b1_36' - `b2_36') / sqrt(`v1_36' + `v2_36'))))
    
    local b1_56 = betas_56[1,1]
    local b2_56 = betas_56[2,1]
    local v1_56 = vars_56[1,1]
    local v2_56 = vars_56[2,2]
    local p_pw_56 = 2 * (1 - normal(abs((`b1_56' - `b2_56') / sqrt(`v1_56' + `v2_56'))))
    
    display "Below vs Above 36-55: p = `p_pw_36'"
    display "Below vs Above 56-78: p = `p_pw_56'"
    
    post `resfile2' ("`mv'") (`p_pw_36') (`p_pw_56')
}

postclose `resfile1'
postclose `resfile2'

* --- 4.4 Tabla principal: merge y export ---*
use `results_per_median', clear
tempfile per_median_saved
save `per_median_saved', replace

use `results_pairwise', clear
merge 1:m median_var using `per_median_saved', nogenerate
sort median_var group

order median_var group ame_36 se_36 p_36 ame_56 se_56 p_56 ///
      p_equal_cohorts nobs pseudo_r2 group_ub p_pw_36 p_pw_56

export excel using "$output/heterog_median_migration.xlsx", ///
    firstrow(variables) replace
}

* ============================================================================ *
* Parte 5
*
* Efectos heterogéneos por la mediana (variables de LAPOP)
* Nota: las variables de LAPOP se agregan a nivel municipio por promedio
* usando solo las olas pre-tratamiento (year != 2023).
* La mediana se calcula sobre esos 54-56 municipios de LAPOP.
* ============================================================================ *
{
* --- 5.1. Collapse LAPOP a nivel municipio y crear splits por la mediana ---*
use "$data_out/lapop_data_merge.dta", clear
keep if year != 2023

gen mujer = .
replace mujer=1 if hombre ==0
replace mujer=0 if hombre==1

* Chequeo rápido de missings
foreach v in interes_pol_mucho en_pareja mujer edad desempleado ///
             secundaria_completa_o_mas izq_der voto_blanco_nulo {
    display "--- `v' ---"
    tab mun_code if `v' == ., missing
}

* Collapse: mean para todas
collapse (mean) interes_pol_mucho en_pareja mujer desempleado ///
                secundaria_completa_o_mas izq_der voto_blanco_nulo ///
		 (p50) edad, ///
         by(mun_code)

* Splits por mediana (2 grupos: 1 = Below, 2 = Above)
xtile p50_interes_pol_mucho     = interes_pol_mucho,        nq(2)
xtile p50_en_pareja             = en_pareja,                nq(2)
xtile p50_mujer                 = mujer,                   nq(2)
xtile p50_edad                  = edad,                     nq(2)
xtile p50_desempleado           = desempleado,              nq(2)
xtile p50_sec_completa          = secundaria_completa_o_mas, nq(2)
xtile p50_izq_der               = izq_der,                  nq(2)
xtile p50_voto_blanco_nulo      = voto_blanco_nulo,         nq(2)

tempfile medians_lapop
save `medians_lapop', replace

* Local con los pares median_var <-> source_var
local median_vars "p50_interes_pol_mucho p50_en_pareja p50_mujer p50_edad p50_desempleado p50_sec_completa p50_izq_der p50_voto_blanco_nulo"
local source_vars "interes_pol_mucho en_pareja mujer edad desempleado secundaria_completa_o_mas izq_der voto_blanco_nulo"

* Crear locales con los upper bounds de cada grupo
local i = 1
foreach mv of local median_vars {
    local sv : word `i' of `source_vars'
    forvalues m = 1/2 {
        quietly summarize `sv' if `mv' == `m'
        local ubname "ub_`mv'_`m'"
        local `ubname' = r(max)
        display "`mv', group `m': ``ubname''"
    }
    local ++i
}

* --- 5.2. Cargar data y settings ---*
use "$data_out/lapop_data_merge.dta", clear
gen post = (year >= 2022)
gen post_share_1936 = post * share_1936_1955
gen post_share_1956 = post * share_1956_1978
label variable post_share_1936 "Post x Spanish share 1936-1955"
label variable post_share_1956 "Post x Spanish share 1956-1978"
egen mun_code_num = group(mun_code)

merge m:1 mun_code using `medians_lapop'
drop if _merge == 2
drop _merge

local median_vars "p50_interes_pol_mucho p50_en_pareja p50_mujer p50_edad p50_desempleado p50_sec_completa p50_izq_der p50_voto_blanco_nulo"
local source_vars "interes_pol_mucho en_pareja mujer edad desempleado secundaria_completa_o_mas izq_der voto_blanco_nulo"

* Postfile 1: tabla principal (una fila por median_var x grupo)
tempfile results_per_median
tempname resfile1
postfile `resfile1' str30 median_var byte group ///
    double ame_36 se_36 p_36 ///
    double ame_56 se_56 p_56 ///
    long nobs double pseudo_r2 ///
    double p_equal_cohorts ///
    double group_ub ///
    using `results_per_median', replace

* Postfile 2: test Below vs Above por median_var
tempfile results_pairwise
tempname resfile2
postfile `resfile2' str30 median_var ///
    double p_pw_36 p_pw_56 ///
    using `results_pairwise', replace

* --- 5.3 Loop principal ---*
local i = 1
foreach mv of local median_vars {
    local sv : word `i' of `source_vars'
    local ++i
    
    display _newline _newline "==========================================="
    display "Variable de mediana: `mv' (source: `sv')"
    display "==========================================="
    
    matrix betas_36 = J(2, 1, .)
    matrix vars_36  = J(2, 2, 0)
    matrix betas_56 = J(2, 1, .)
    matrix vars_56  = J(2, 2, 0)
    
    forvalues m = 1/2 {
        display _newline "--- Grupo `m' (1=Below, 2=Above) ---"
        
        logit intencion_migrar post_share_1936 post_share_1956 ///
              edad hombre i.year i.mun_code_num ///
              if `mv' == `m', cluster(mun_code)
        
        local N  = e(N)
        local r2 = e(r2_p)
        
        margins, dydx(post_share_1936 post_share_1956) post
        matrix M = r(table)
        
        local ame_36_m = M[1,1]
        local se_36_m  = M[2,1]
        local p_36_m   = M[4,1]
        local ame_56_m = M[1,2]
        local se_56_m  = M[2,2]
        local p_56_m   = M[4,2]
        
        test post_share_1936 = post_share_1956
        local p_equal_m = r(p)
        
        matrix betas_36[`m', 1]  = `ame_36_m'
        matrix vars_36[`m', `m'] = `se_36_m' * `se_36_m'
        matrix betas_56[`m', 1]  = `ame_56_m'
        matrix vars_56[`m', `m'] = `se_56_m' * `se_56_m'
        
        local ubname "ub_`mv'_`m'"
        local ub ``ubname''
        display "Upper bound del grupo `m': `ub'"
        
        post `resfile1' ("`mv'") (`m') ///
                       (`ame_36_m') (`se_36_m') (`p_36_m') ///
                       (`ame_56_m') (`se_56_m') (`p_56_m') ///
                       (`N') (`r2') ///
                       (`p_equal_m') (`ub')
    }
    
    * --- Test Below vs Above (z-stat en subsamples independientes) ---
    local b1_36 = betas_36[1,1]
    local b2_36 = betas_36[2,1]
    local v1_36 = vars_36[1,1]
    local v2_36 = vars_36[2,2]
    local p_pw_36 = 2 * (1 - normal(abs((`b1_36' - `b2_36') / sqrt(`v1_36' + `v2_36'))))
    
    local b1_56 = betas_56[1,1]
    local b2_56 = betas_56[2,1]
    local v1_56 = vars_56[1,1]
    local v2_56 = vars_56[2,2]
    local p_pw_56 = 2 * (1 - normal(abs((`b1_56' - `b2_56') / sqrt(`v1_56' + `v2_56'))))
    
    display "Below vs Above 36-55: p = `p_pw_36'"
    display "Below vs Above 56-78: p = `p_pw_56'"
    
    post `resfile2' ("`mv'") (`p_pw_36') (`p_pw_56')
}

postclose `resfile1'
postclose `resfile2'

* --- 5.4 Tabla principal: merge y export ---*
use `results_per_median', clear
tempfile per_median_saved
save `per_median_saved', replace

use `results_pairwise', clear
merge 1:m median_var using `per_median_saved', nogenerate
sort median_var group

order median_var group ame_36 se_36 p_36 ame_56 se_56 p_56 ///
      p_equal_cohorts nobs pseudo_r2 group_ub p_pw_36 p_pw_56

export excel using "$output/heterog_median_migration_lapop.xlsx", ///
    firstrow(variables) replace
}


