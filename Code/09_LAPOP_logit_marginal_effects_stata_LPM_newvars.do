* ============================================================================ *
* LAPOP: Logit con AME, variables municipales comunes y diagnostico LPM
* Traduccion a Stata del script R de efectos marginales.
*
* Para cada especificacion Logit, este do-file estima en paralelo un modelo
* lineal de probabilidad con la MISMA muestra, pesos, controles y efectos fijos.
* Las tablas principales contienen solamente los resultados Logit.
* Las tablas de anexo LPM reportan la proporcion de predicciones estrictamente
* menores que 0 o mayores que 1, expresada como porcentaje de la muestra.
* ============================================================================ *

clear all
set more off
set linesize 255
version 17

* ---------------------------------------------------------------------------- *
* 0. Paths y parametros
* ---------------------------------------------------------------------------- *

* Windows (Pilar)
global main "C:/Users/pilih/Documents/Papers German/Valerie/Paper_nietos_arg"

* Mac (Florencia): descomentar esta linea y comentar la anterior si corresponde
* global main "/Users/florenciaruiz/BID 2/Paper Valerie/Nietos/Argentina/Paper_nietos_arg"

global data_int      "$main/Data Int"
global data_out      "$main/Data Out"
global output        "$main/Output"
global output_tex    "$main/Output/tex"
global output_models "$main/Output/models"

capture mkdir "$output"
capture mkdir "$output_tex"
capture mkdir "$output_models"

global post_cutoff 2022
global scale_factor 1

* ---------------------------------------------------------------------------- *
* 1. Preparar LAPOP y unir variables municipales comunes
* ---------------------------------------------------------------------------- *

import delimited using "$data_out/lapop_data_merge.csv", clear ///
    varnames(1) encoding(UTF-8)

* Variables de LAPOP que deben ser numericas. Esto resuelve CSV con "NA".
local numeric_vars ///
    year mun_code wt intencion_migrar edad hombre ///
    desempleado en_pareja anios_educ izq_der ///
    interes_pol_mucho voto_blanco_nulo ///
    share_1936_1955 share_1956_1978

foreach var of local numeric_vars {
    capture confirm variable `var'
    if !_rc {
        capture confirm numeric variable `var'
        if _rc {
            replace `var' = strtrim(`var')
            replace `var' = "" if inlist(upper(`var'), ///
                "NA", "N/A", "NAN", "NULL", ".")
            destring `var', replace
        }
    }
}

gen double year_num = year
gen byte post = (year_num >= $post_cutoff) if !missing(year_num)

tempfile lapop_raw municipal_covariates lapop_ready
save `lapop_raw', replace

* -------------------------------------------------------------------------- *
* Base municipal comun a las especificaciones electorales y LAPOP
* Creada en 02_Data_API_Arg.R: Data Out/data_eff_het.dta
* -------------------------------------------------------------------------- *

use "$data_out/data_eff_het.dta", clear

keep ///
    mun_code ///
    popdensgeo2_2010 ///
    share_female_2010 ///
    mean_yrschool_2010 ///
    median_age_2010 ///
    share_laborforce_2010 ///
    share_unemployed_2010 ///
    share_izq_amplia_pre_avg ///
    share_alt_pre_avg ///
    t_density_2010 ///
    t_fem_2010 ///
    t_mean_schyr_2010 ///
    t_med_dage_2010 ///
    t_pea_2010 ///
    t_unemp_2010 ///
    t_izam_pre_avg ///
    t_alt_pre_avg

capture confirm string variable mun_code
if !_rc {
    replace mun_code = strtrim(mun_code)
    destring mun_code, replace
}

* data_eff_het debe tener una observacion por municipio.
bysort mun_code: keep if _n == 1
save `municipal_covariates', replace

use `lapop_raw', clear

capture confirm string variable mun_code
if !_rc {
    replace mun_code = strtrim(mun_code)
    destring mun_code, replace
}

merge m:1 mun_code using `municipal_covariates', ///
    keep(master match) generate(merge_municipal)

tab merge_municipal
count if merge_municipal == 1
display as text ///
    "Observaciones LAPOP sin match en data_eff_het: " r(N)
drop merge_municipal

* Identificador numerico para efectos fijos y errores clusterizados.
egen long mun_code_num = group(mun_code), label
label variable mun_code_num "Municipality"

* Etiquetas de las variables municipales comunes.
label variable popdensgeo2_2010          "Population density"
label variable share_female_2010         "Female population share"
label variable mean_yrschool_2010        "Mean years of education"
label variable median_age_2010           "Median age"
label variable share_laborforce_2010     "Share in labor force"
label variable share_unemployed_2010     "Share unemployed"
label variable share_izq_amplia_pre_avg  "Left vote share"
label variable share_alt_pre_avg          "Ideological alternation"

* ---------------------------------------------------------------------------- *
* 3. Interacciones explicitas
* ---------------------------------------------------------------------------- *

gen double pshare36 = post * share_1936_1955
gen double pshare56 = post * share_1956_1978

label variable pshare36 "Post x Share 1936-1955"
label variable pshare56 "Post x Share 1956-1978"

local post_x_vars ///
    popdensgeo2_2010 ///
    share_female_2010 ///
    mean_yrschool_2010 ///
    median_age_2010 ///
    share_laborforce_2010 ///
    share_unemployed_2010 ///
    share_izq_amplia_pre_avg ///
    share_alt_pre_avg

local controls_individual "edad hombre"

foreach x of local post_x_vars {
    capture confirm variable `x'
    if !_rc {
        gen double px_`x' = post * `x'
        gen double pxs36_`x' = post * `x' * share_1936_1955
        gen double pxs56_`x' = post * `x' * share_1956_1978
    }
}

compress
save `lapop_ready', replace
save "$data_int/lapop_logit_mfx_ready_LPM_appendix.dta", replace

* ---------------------------------------------------------------------------- *
* 4. Helpers
* ---------------------------------------------------------------------------- *

* Extrae un AME sin usar la opcion post, para no reemplazar el Logit activo.
capture program drop get_margins_one
program define get_margins_one, rclass
    version 17
    syntax varname

    quietly margins, dydx(`varlist')
    matrix M = r(table)

    return scalar ame = M[1,1]
    return scalar se  = M[2,1]
    return scalar p   = M[4,1]
end

* Estima el LPM sobre exactamente la muestra del Logit y cuenta predicciones
* estrictamente menores que 0 o mayores que 1.
capture program drop get_lpm_bounds
program define get_lpm_bounds, rclass
    version 17
    syntax varlist(min=1 numeric), SAMPLE(varname)

    tempvar lpm_hat

    quietly regress intencion_migrar ///
        `varlist' ///
        i.year_num i.mun_code_num ///
        if `sample' == 1 [pw=wt], ///
        vce(cluster mun_code_num)

    local N = e(N)

    quietly predict double `lpm_hat' if e(sample), xb

    quietly count if e(sample) & `lpm_hat' < 0
    local below0 = r(N)

    quietly count if e(sample) & `lpm_hat' > 1
    local above1 = r(N)

    local outside = `below0' + `above1'

    return scalar nobs        = `N'
    return scalar below0_n    = `below0'
    return scalar above1_n    = `above1'
    return scalar outside_n   = `outside'
    return scalar outside_pct = 100 * `outside' / `N'
end

* ---------------------------------------------------------------------------- *
* 5. Modelo 1: Logit Post x shares
* ---------------------------------------------------------------------------- *

use `lapop_ready', clear

tempname post_share_handle
tempfile post_share_long

postfile `post_share_handle' ///
    str20 spec str3 controls ///
    str50 term_id str80 term_label ///
    double ame se p_value ame_reported se_reported ///
    long nobs double pseudo_r2 ///
    long lpm_below0_n lpm_above1_n lpm_outside_n ///
    double lpm_outside_pct ///
    using `post_share_long', replace

foreach ctrl_label in "No" "Yes" {

    if "`ctrl_label'" == "Yes" {
        local ctrls "`controls_individual'"
        local spec "with_controls"
    }
    else {
        local ctrls ""
        local spec "no_controls"
    }

    display _newline ///
        "Modelo Post x shares | Controles: `ctrl_label'"

    quietly logit intencion_migrar ///
        pshare36 pshare56 ///
        `ctrls' ///
        i.year_num i.mun_code_num ///
        [pw=wt], ///
        vce(cluster mun_code_num)

    local N  = e(N)
    local r2 = e(r2_p)

    tempvar sample_logit
    gen byte `sample_logit' = e(sample)

    quietly get_margins_one pshare36
    local ame36 = r(ame)
    local se36  = r(se)
    local p36   = r(p)

    quietly get_margins_one pshare56
    local ame56 = r(ame)
    local se56  = r(se)
    local p56   = r(p)

    quietly get_lpm_bounds pshare36 pshare56 `ctrls', ///
        sample(`sample_logit')

    local lpm_below = r(below0_n)
    local lpm_above = r(above1_n)
    local lpm_out   = r(outside_n)
    local lpm_pct   = r(outside_pct)

    local lab36 "\(Post \times Share_{1936-1955}\)"
    local lab56 "\(Post \times Share_{1956-1978}\)"

    post `post_share_handle' ///
        ("`spec'") ("`ctrl_label'") ///
        ("pshare36") ("`lab36'") ///
        (`ame36') (`se36') (`p36') ///
        (`ame36' * $scale_factor) (`se36' * $scale_factor) ///
        (`N') (`r2') ///
        (`lpm_below') (`lpm_above') (`lpm_out') (`lpm_pct')

    post `post_share_handle' ///
        ("`spec'") ("`ctrl_label'") ///
        ("pshare56") ("`lab56'") ///
        (`ame56') (`se56') (`p56') ///
        (`ame56' * $scale_factor) (`se56' * $scale_factor) ///
        (`N') (`r2') ///
        (`lpm_below') (`lpm_above') (`lpm_out') (`lpm_pct')

    drop `sample_logit'
}

postclose `post_share_handle'

use `post_share_long', clear
save "$output_models/logit_post_share_mfx_long_LPM_appendix.dta", replace

* ---------------------------------------------------------------------------- *
* 6. Modelo 2: Logit Post x caracteristicas municipales comunes
* ---------------------------------------------------------------------------- *

use `lapop_ready', clear

tempname post_x_handle
tempfile post_x_long

postfile `post_x_handle' ///
    str60 variable str80 term_label str3 controls ///
    double ame se p_value ame_reported se_reported ///
    long nobs double pseudo_r2 ///
    long lpm_below0_n lpm_above1_n lpm_outside_n ///
    double lpm_outside_pct ///
    using `post_x_long', replace

foreach x of local post_x_vars {

    capture confirm variable `x'
    if !_rc {
        quietly count if !missing(`x')

        if r(N) > 0 {
            foreach ctrl_label in "No" "Yes" {

                if "`ctrl_label'" == "Yes" {
                    local ctrls "`controls_individual'"
                }
                else {
                    local ctrls ""
                }

                display _newline ///
                    "Modelo Post x `x' | Controles: `ctrl_label'"

                * X es constante por municipio y queda absorbida por los FE.
                quietly logit intencion_migrar ///
                    px_`x' ///
                    `ctrls' ///
                    i.year_num i.mun_code_num ///
                    [pw=wt], ///
                    vce(cluster mun_code_num)

                local N  = e(N)
                local r2 = e(r2_p)

                tempvar sample_logit
                gen byte `sample_logit' = e(sample)

                quietly get_margins_one px_`x'
                local ame_x = r(ame)
                local se_x  = r(se)
                local p_x   = r(p)

                quietly get_lpm_bounds px_`x' `ctrls', ///
                    sample(`sample_logit')

                local lpm_below = r(below0_n)
                local lpm_above = r(above1_n)
                local lpm_out   = r(outside_n)
                local lpm_pct   = r(outside_pct)

                local label "`x'"
                if "`x'" == "popdensgeo2_2010"         local label "Population density"
                if "`x'" == "share_female_2010"        local label "Female population share"
                if "`x'" == "mean_yrschool_2010"       local label "Mean years of education"
                if "`x'" == "median_age_2010"           local label "Median age"
                if "`x'" == "share_laborforce_2010"    local label "Share in labor force"
                if "`x'" == "share_unemployed_2010"    local label "Share unemployed"
                if "`x'" == "share_izq_amplia_pre_avg" local label "Left vote share"
                if "`x'" == "share_alt_pre_avg"         local label "Ideological alternation"

                post `post_x_handle' ///
                    ("`x'") ("`label'") ("`ctrl_label'") ///
                    (`ame_x') (`se_x') (`p_x') ///
                    (`ame_x' * $scale_factor) (`se_x' * $scale_factor) ///
                    (`N') (`r2') ///
                    (`lpm_below') (`lpm_above') (`lpm_out') (`lpm_pct')

                drop `sample_logit'
            }
        }
    }
}

postclose `post_x_handle'

use `post_x_long', clear
save "$output_models/logit_post_x_mfx_long_LPM_appendix.dta", replace

* ---------------------------------------------------------------------------- *
* 7. Modelo 3: Logit triple diferencias
* ---------------------------------------------------------------------------- *

use `lapop_ready', clear

tempname triple_handle
tempfile triple_long

postfile `triple_handle' ///
    str60 variable str80 variable_label str3 controls ///
    str60 share_group str100 term_id str100 term_label ///
    double ame se p_value ame_reported se_reported ///
    long nobs double pseudo_r2 ///
    long lpm_below0_n lpm_above1_n lpm_outside_n ///
    double lpm_outside_pct ///
    using `triple_long', replace

foreach x of local post_x_vars {

    capture confirm variable `x'
    if !_rc {
        quietly count if !missing(`x')

        if r(N) > 0 {
            foreach ctrl_label in "No" "Yes" {

                local rhs_extra ""

                if "`ctrl_label'" == "Yes" {
                    local rhs_extra "`controls_individual'"

                    foreach z of local post_x_vars {
                        if "`z'" != "`x'" {
                            capture confirm variable px_`z'
                            if !_rc {
                                local rhs_extra "`rhs_extra' px_`z'"
                            }
                        }
                    }
                }

                display _newline ///
                    "Triple diferencia: `x' | Controles: `ctrl_label'"

                quietly logit intencion_migrar ///
                    pshare36 pshare56 ///
                    px_`x' ///
                    pxs36_`x' ///
                    pxs56_`x' ///
                    `rhs_extra' ///
                    i.year_num i.mun_code_num ///
                    [pw=wt], ///
                    vce(cluster mun_code_num)

                local N  = e(N)
                local r2 = e(r2_p)

                tempvar sample_logit
                gen byte `sample_logit' = e(sample)

                quietly get_margins_one pxs36_`x'
                local ame36 = r(ame)
                local se36  = r(se)
                local p36   = r(p)

                quietly get_margins_one pxs56_`x'
                local ame56 = r(ame)
                local se56  = r(se)
                local p56   = r(p)

                quietly get_lpm_bounds ///
                    pshare36 pshare56 ///
                    px_`x' ///
                    pxs36_`x' ///
                    pxs56_`x' ///
                    `rhs_extra', ///
                    sample(`sample_logit')

                local lpm_below = r(below0_n)
                local lpm_above = r(above1_n)
                local lpm_out   = r(outside_n)
                local lpm_pct   = r(outside_pct)

                local label "`x'"
                if "`x'" == "popdensgeo2_2010"         local label "Population density"
                if "`x'" == "share_female_2010"        local label "Female population share"
                if "`x'" == "mean_yrschool_2010"       local label "Mean years of education"
                if "`x'" == "median_age_2010"           local label "Median age"
                if "`x'" == "share_laborforce_2010"    local label "Share in labor force"
                if "`x'" == "share_unemployed_2010"    local label "Share unemployed"
                if "`x'" == "share_izq_amplia_pre_avg" local label "Left vote share"
                if "`x'" == "share_alt_pre_avg"         local label "Ideological alternation"

                local tlab36 "\(Post \times X \times Share_{1936-1955}\)"
                local tlab56 "\(Post \times X \times Share_{1956-1978}\)"

                post `triple_handle' ///
                    ("`x'") ("`label'") ("`ctrl_label'") ///
                    ("share_1936_1955") ///
                    ("pxs36_`x'") ("`tlab36'") ///
                    (`ame36') (`se36') (`p36') ///
                    (`ame36' * $scale_factor) (`se36' * $scale_factor) ///
                    (`N') (`r2') ///
                    (`lpm_below') (`lpm_above') (`lpm_out') (`lpm_pct')

                post `triple_handle' ///
                    ("`x'") ("`label'") ("`ctrl_label'") ///
                    ("share_1956_1978") ///
                    ("pxs56_`x'") ("`tlab56'") ///
                    (`ame56') (`se56') (`p56') ///
                    (`ame56' * $scale_factor) (`se56' * $scale_factor) ///
                    (`N') (`r2') ///
                    (`lpm_below') (`lpm_above') (`lpm_out') (`lpm_pct')

                drop `sample_logit'
            }
        }
    }
}

postclose `triple_handle'

use `triple_long', clear
save "$output_models/logit_triple_mfx_long_LPM_appendix.dta", replace


* ---------------------------------------------------------------------------- *
* 8. Exportar Excel principal: seis hojas sin diagnostico LPM
* ---------------------------------------------------------------------------- *

local excel_file ///
    "$output/logit_marginal_effects_glm_cluster_se_stata.xlsx"

capture erase "`excel_file'"

* 8.1 Post-share long
use "$output_models/logit_post_share_mfx_long_LPM_appendix.dta", clear

gen str40 ame_formatted = ///
    string(ame_reported, "%9.3f") + ///
    cond(p_value < 0.01, "***", ///
        cond(p_value < 0.05, "**", ///
            cond(p_value < 0.10, "*", ""))) + ///
    " (" + string(se_reported, "%9.3f") + ")"

preserve
    keep spec controls term_id term_label ///
        ame se p_value ame_reported se_reported ///
        ame_formatted nobs pseudo_r2

    export excel using "`excel_file'", ///
        sheet("post_share_long") ///
        firstrow(variables) ///
        replace
restore

* 8.2 Post-share wide
preserve
    keep term_id term_label spec ame_formatted nobs

    reshape wide ame_formatted nobs, ///
        i(term_id term_label) ///
        j(spec) string

    rename ame_formattedno_controls no_controls_ame
    rename ame_formattedwith_controls with_controls_ame
    rename nobsno_controls no_controls_n
    rename nobswith_controls with_controls_n

    order term_id term_label ///
        no_controls_ame with_controls_ame ///
        no_controls_n with_controls_n

    export excel using "`excel_file'", ///
        sheet("post_share_wide") ///
        firstrow(variables) ///
        sheetreplace

    save "$output_models/logit_post_share_mfx_wide.dta", replace
restore

* 8.3 Post-X long
use "$output_models/logit_post_x_mfx_long_LPM_appendix.dta", clear

gen str40 ame_formatted = ///
    string(ame_reported, "%9.3f") + ///
    cond(p_value < 0.01, "***", ///
        cond(p_value < 0.05, "**", ///
            cond(p_value < 0.10, "*", ""))) + ///
    " (" + string(se_reported, "%9.3f") + ")"

preserve
    keep variable term_label controls ///
        ame se p_value ame_reported se_reported ///
        ame_formatted nobs pseudo_r2

    export excel using "`excel_file'", ///
        sheet("post_x_long") ///
        firstrow(variables) ///
        sheetreplace
restore

* 8.4 Post-X wide
preserve
    gen str20 spec = ///
        cond(controls == "No", "no_controls", "with_controls")

    gen byte row_order = .
    replace row_order = 1 if variable == "popdensgeo2_2010"
    replace row_order = 2 if variable == "share_female_2010"
    replace row_order = 3 if variable == "mean_yrschool_2010"
    replace row_order = 4 if variable == "median_age_2010"
    replace row_order = 5 if variable == "share_laborforce_2010"
    replace row_order = 6 if variable == "share_unemployed_2010"
    replace row_order = 7 if variable == "share_izq_amplia_pre_avg"
    replace row_order = 8 if variable == "share_alt_pre_avg"

    keep variable term_label row_order spec ame_formatted nobs

    reshape wide ame_formatted nobs, ///
        i(variable term_label row_order) ///
        j(spec) string

    rename ame_formattedno_controls no_controls_ame
    rename ame_formattedwith_controls with_controls_ame
    rename nobsno_controls no_controls_n
    rename nobswith_controls with_controls_n

    sort row_order
    drop row_order

    order variable term_label ///
        no_controls_ame with_controls_ame ///
        no_controls_n with_controls_n

    export excel using "`excel_file'", ///
        sheet("post_x_wide") ///
        firstrow(variables) ///
        sheetreplace

    save "$output_models/logit_post_x_mfx_wide.dta", replace
restore

* 8.5 Triple long
use "$output_models/logit_triple_mfx_long_LPM_appendix.dta", clear

gen str40 ame_formatted = ///
    string(ame_reported, "%9.3f") + ///
    cond(p_value < 0.01, "***", ///
        cond(p_value < 0.05, "**", ///
            cond(p_value < 0.10, "*", ""))) + ///
    " (" + string(se_reported, "%9.3f") + ")"

preserve
    keep variable variable_label controls share_group ///
        term_id term_label ///
        ame se p_value ame_reported se_reported ///
        ame_formatted nobs pseudo_r2

    export excel using "`excel_file'", ///
        sheet("triple_long") ///
        firstrow(variables) ///
        sheetreplace
restore

* 8.6 Triple wide
preserve
    gen str20 spec = ///
        cond(controls == "No", "no_controls", "with_controls")

    gen str40 key = ///
        spec + "_" + subinstr(share_group, "share_", "", .)

    gen byte row_order = .
    replace row_order = 1 if variable == "popdensgeo2_2010"
    replace row_order = 2 if variable == "share_female_2010"
    replace row_order = 3 if variable == "mean_yrschool_2010"
    replace row_order = 4 if variable == "median_age_2010"
    replace row_order = 5 if variable == "share_laborforce_2010"
    replace row_order = 6 if variable == "share_unemployed_2010"
    replace row_order = 7 if variable == "share_izq_amplia_pre_avg"
    replace row_order = 8 if variable == "share_alt_pre_avg"

    * Nombre corto durante reshape para respetar el limite de 32 caracteres.
    rename ame_formatted cell

    keep variable variable_label row_order key cell nobs

    reshape wide cell nobs, ///
        i(variable variable_label row_order) ///
        j(key) string

    rename cellno_controls_1936_1955 no_controls_1936_1955
    rename cellno_controls_1956_1978 no_controls_1956_1978
    rename cellwith_controls_1936_1955 with_controls_1936_1955
    rename cellwith_controls_1956_1978 with_controls_1956_1978

    rename nobsno_controls_1936_1955 no_controls_n
    drop nobsno_controls_1956_1978
    rename nobswith_controls_1936_1955 with_controls_n
    drop nobswith_controls_1956_1978

    sort row_order
    drop row_order

    order variable variable_label ///
        no_controls_1936_1955 no_controls_1956_1978 ///
        with_controls_1936_1955 with_controls_1956_1978 ///
        no_controls_n with_controls_n

    export excel using "`excel_file'", ///
        sheet("triple_wide") ///
        firstrow(variables) ///
        sheetreplace

    save "$output_models/logit_triple_mfx_wide.dta", replace
restore

* ---------------------------------------------------------------------------- *
* 9. Exportar diagnostico LPM a un Excel separado para el anexo
* ---------------------------------------------------------------------------- *

local lpm_excel ///
    "$output/lpm_predictions_outside_01_stata.xlsx"

capture erase "`lpm_excel'"

* 9.1 Post x shares: solo especificacion con controles
use "$output_models/logit_post_share_mfx_long_LPM_appendix.dta", clear

keep if spec == "with_controls"
bysort spec: keep if _n == 1
keep lpm_outside_pct

gen str70 specification = ///
    "Post-period Spanish historical exposure"

rename lpm_outside_pct controls_pct

format controls_pct %9.2f
label variable specification "Specification"
label variable controls_pct "Controls (%)"

order specification controls_pct

export excel using "`lpm_excel'", ///
    sheet("post_share") ///
    firstrow(varlabels) ///
    replace

save "$output_models/lpm_outside_01_post_share_stata.dta", replace

* 9.2 Post x caracteristicas municipales comunes: solo con controles
use "$output_models/logit_post_x_mfx_long_LPM_appendix.dta", clear

keep if controls == "Yes"

gen byte row_order = .
replace row_order = 1 if variable == "popdensgeo2_2010"
replace row_order = 2 if variable == "share_female_2010"
replace row_order = 3 if variable == "mean_yrschool_2010"
replace row_order = 4 if variable == "median_age_2010"
replace row_order = 5 if variable == "share_laborforce_2010"
replace row_order = 6 if variable == "share_unemployed_2010"
replace row_order = 7 if variable == "share_izq_amplia_pre_avg"
replace row_order = 8 if variable == "share_alt_pre_avg"

keep variable term_label row_order lpm_outside_pct
rename lpm_outside_pct controls_pct

sort row_order
format controls_pct %9.2f
label variable term_label "Municipal characteristic"
label variable controls_pct "Controls (%)"

order variable term_label controls_pct row_order

preserve
    drop variable row_order
    export excel using "`lpm_excel'", ///
        sheet("post_x") ///
        firstrow(varlabels) ///
        sheetreplace
restore

save "$output_models/lpm_outside_01_post_x_stata.dta", replace

* 9.3 Triple diferencias: solo especificaciones con controles
use "$output_models/logit_triple_mfx_long_LPM_appendix.dta", clear

keep if controls == "Yes"
bysort variable: keep if _n == 1

gen byte row_order = .
replace row_order = 1 if variable == "popdensgeo2_2010"
replace row_order = 2 if variable == "share_female_2010"
replace row_order = 3 if variable == "mean_yrschool_2010"
replace row_order = 4 if variable == "median_age_2010"
replace row_order = 5 if variable == "share_laborforce_2010"
replace row_order = 6 if variable == "share_unemployed_2010"
replace row_order = 7 if variable == "share_izq_amplia_pre_avg"
replace row_order = 8 if variable == "share_alt_pre_avg"

keep variable variable_label row_order lpm_outside_pct
rename lpm_outside_pct controls_pct

sort row_order
format controls_pct %9.2f
label variable variable_label "Municipal characteristic"
label variable controls_pct "Controls (%)"

order variable variable_label controls_pct row_order

preserve
    drop variable row_order
    export excel using "`lpm_excel'", ///
        sheet("triple") ///
        firstrow(varlabels) ///
        sheetreplace
restore

save "$output_models/lpm_outside_01_triple_stata.dta", replace

* ---------------------------------------------------------------------------- *
* 10. Exportar tablas LaTeX principales, sin diagnostico LPM
* ---------------------------------------------------------------------------- *

* ---------------------------------------------------------------------------- *
* 10.1 Post x shares
* Formato largo: controles y no controles en filas diferentes
* ---------------------------------------------------------------------------- *

use "$output_models/logit_post_share_mfx_wide.dta", clear

* Convertir las columnas de controles en filas
rename no_controls_ame   ame_no
rename with_controls_ame ame_yes
rename no_controls_n     nobs_no
rename with_controls_n   nobs_yes

reshape long ame_ nobs_, ///
    i(term_id term_label) ///
    j(control_type) string

rename ame_  ame_formatted
rename nobs_ nobs

* Crear indicador de controles
gen str3 controls = ""
replace controls = "No"  if control_type == "no"
replace controls = "Yes" if control_type == "yes"

* Orden de los términos
gen byte term_order = .
replace term_order = 1 if term_id == "pshare36"
replace term_order = 2 if term_id == "pshare56"

* Primero sin controles y luego con controles
gen byte controls_order = .
replace controls_order = 1 if controls == "No"
replace controls_order = 2 if controls == "Yes"

sort term_order controls_order

* Formatear observaciones
gen str15 n_formatted = string(nobs, "%12.0fc")

capture file close tex

file open tex using ///
    "$output_tex/logit_post_share_mfx_glm_cluster_se_stata.tex", ///
    write replace text

file write tex "\begin{table}[!htbp]" _n
file write tex "\normalsize" _n
file write tex "\renewcommand{\arraystretch}{1.15}" _n
file write tex "\centering" _n

file write tex ///
    "\caption{Logit average marginal effects: post-period Spanish historical exposure}" _n

file write tex "\label{tab:logit_post_share_mfx_stata}" _n
file write tex "\begin{tabular}{llcc}" _n
file write tex "\hline" _n

file write tex ///
    "Term & Controls & Logit AME & N \\" _n

file write tex "\hline" _n

forvalues i = 1/`=_N' {

    local lab  = term_label[`i']
    local ctrl = controls[`i']
    local ame  = ame_formatted[`i']
    local n    = n_formatted[`i']

    file write tex ///
        "`lab' & `ctrl' & `ame' & `n' \\" _n

    * Espacio después del par No/Yes de cada término
    if controls_order[`i'] == 2 & `i' < _N {
        file write tex "\addlinespace" _n
    }
}

file write tex "\hline" _n

file write tex ///
    "\multicolumn{4}{l}{\footnotesize Notes: The dependent variable is migration intention. Each pair of rows reports separate Logit models.} \\" _n

file write tex ///
    "\multicolumn{4}{l}{\footnotesize Logit entries report average marginal effects on the 0--1 probability scale. All models include year and municipality fixed effects} \\" _n

file write tex ///
    "\multicolumn{4}{l}{\footnotesize and use survey weights. Specifications with controls include age and a male indicator. Standard errors clustered at the} \\" _n

file write tex ///
    "\multicolumn{4}{l}{\footnotesize municipality level are in parentheses. * \(p<0.10\), ** \(p<0.05\), *** \(p<0.01\).} \\" _n

file write tex "\end{tabular}" _n
file write tex "\end{table}" _n

file close tex

* ---------------------------------------------------------------------------- *
* 10.2 Post x X
* Formato largo: controles y no controles en una misma columna
* ---------------------------------------------------------------------------- *

use "$output_models/logit_post_x_mfx_wide.dta", clear

* Convertir las columnas separadas en filas
rename no_controls_ame   ame_no
rename with_controls_ame ame_yes
rename no_controls_n     nobs_no
rename with_controls_n   nobs_yes

reshape long ame_ nobs_, ///
    i(variable term_label) ///
    j(control_type) string

rename ame_  ame_formatted
rename nobs_ nobs

* Etiqueta de controles
gen str3 controls = ""

replace controls = "No"  if control_type == "no"
replace controls = "Yes" if control_type == "yes"

* Orden de las variables
gen byte row_order = .

replace row_order = 1 if variable == "popdensgeo2_2010"
replace row_order = 2 if variable == "share_female_2010"
replace row_order = 3 if variable == "mean_yrschool_2010"
replace row_order = 4 if variable == "median_age_2010"
replace row_order = 5 if variable == "share_laborforce_2010"
replace row_order = 6 if variable == "share_unemployed_2010"
replace row_order = 7 if variable == "share_izq_amplia_pre_avg"
replace row_order = 8 if variable == "share_alt_pre_avg"

* Primero sin controles y después con controles
gen byte controls_order = .

replace controls_order = 1 if controls == "No"
replace controls_order = 2 if controls == "Yes"

sort row_order controls_order

* Formatear observaciones
gen str15 n_formatted = string(nobs, "%12.0fc")

capture file close tex

file open tex using ///
    "$output_tex/logit_post_x_mfx_glm_cluster_se_stata.tex", ///
    write replace text

file write tex "\begin{table}[!htbp]" _n
file write tex "\normalsize" _n
file write tex "\renewcommand{\arraystretch}{1.15}" _n
file write tex "\centering" _n

file write tex ///
    "\caption{Post-period changes by municipal characteristics}" _n

file write tex "\label{tab:logit_post_x_mfx_stata}" _n
file write tex "\begin{tabular}{llcc}" _n
file write tex "\hline" _n

file write tex ///
    "Municipal characteristic & Controls & Logit AME & N \\" _n

file write tex "\hline" _n

forvalues i = 1/`=_N' {

    local lab  = term_label[`i']
    local ctrl = controls[`i']
    local ame  = ame_formatted[`i']
    local n    = n_formatted[`i']

    file write tex ///
        "`lab' & `ctrl' & `ame' & `n' \\" _n

    * Agregar espacio después de cada par No/Yes
    if controls_order[`i'] == 2 & `i' < _N {
        file write tex "\addlinespace" _n
    }
}

file write tex "\hline" _n

file write tex ///
    "\multicolumn{4}{l}{\footnotesize Notes: The dependent variable is migration intention. Each pair of rows reports separate Logit models} \\" _n

file write tex ///
    "\multicolumn{4}{l}{\footnotesize interacting \(Post\) with the municipal characteristic indicated in the first column. Logit entries report average} \\" _n

file write tex ///
    "\multicolumn{4}{l}{\footnotesize marginal effects on the 0--1 probability scale, with municipality-clustered standard errors in parentheses. All models include} \\" _n

file write tex ///
    "\multicolumn{4}{l}{\footnotesize year and municipality fixed effects and use survey weights. Specifications with controls include age and a male indicator.} \\" _n

file write tex ///
    "\multicolumn{4}{l}{\footnotesize * \(p<0.10\), ** \(p<0.05\), *** \(p<0.01\).} \\" _n

file write tex "\end{tabular}" _n
file write tex "\end{table}" _n

file close tex

* ---------------------------------------------------------------------------- *
* 10.3 Triple diferencias
* Formato largo: No/Yes en una columna y solo AME del Logit
* ---------------------------------------------------------------------------- *

use "$output_models/logit_triple_mfx_wide.dta", clear


* ---------------------------------------------------------------------------- *
* 10.3.1 Pasar de formato ancho a formato largo
* ---------------------------------------------------------------------------- *

* Ventana 1936-1955
rename no_controls_1936_1955   ame36_no
rename with_controls_1936_1955 ame36_yes

* Ventana 1956-1978
rename no_controls_1956_1978   ame56_no
rename with_controls_1956_1978 ame56_yes

* Observaciones
rename no_controls_n   nobs_no
rename with_controls_n nobs_yes

reshape long ame36_ ame56_ nobs_, ///
    i(variable variable_label) ///
    j(control_type) string

rename ame36_ ame_36
rename ame56_ ame_56
rename nobs_  nobs


* ---------------------------------------------------------------------------- *
* 10.3.2 Crear columna de controles
* ---------------------------------------------------------------------------- *

gen str3 controls = ""

replace controls = "No"  if control_type == "no"
replace controls = "Yes" if control_type == "yes"

drop control_type


* ---------------------------------------------------------------------------- *
* 10.3.3 Ordenar las características municipales
* ---------------------------------------------------------------------------- *

gen byte row_order = .

replace row_order = 1 if variable == "popdensgeo2_2010"
replace row_order = 2 if variable == "share_female_2010"
replace row_order = 3 if variable == "mean_yrschool_2010"
replace row_order = 4 if variable == "median_age_2010"
replace row_order = 5 if variable == "share_laborforce_2010"
replace row_order = 6 if variable == "share_unemployed_2010"
replace row_order = 7 if variable == "share_izq_amplia_pre_avg"
replace row_order = 8 if variable == "share_alt_pre_avg"

gen byte controls_order = .

replace controls_order = 1 if controls == "No"
replace controls_order = 2 if controls == "Yes"

sort row_order controls_order


* ---------------------------------------------------------------------------- *
* 10.3.4 Formatear observaciones
* ---------------------------------------------------------------------------- *

gen str15 n_formatted = string(nobs, "%12.0fc")


* ---------------------------------------------------------------------------- *
* 10.3.5 Exportar tabla LaTeX
* ---------------------------------------------------------------------------- *

capture file close tex

file open tex using ///
    "$output_tex/logit_triple_mfx_glm_cluster_se_stata.tex", ///
    write replace text

file write tex "\begin{table}[!htbp]" _n
file write tex "\normalsize" _n
file write tex "\renewcommand{\arraystretch}{1.15}" _n
file write tex "\centering" _n
file write tex "\caption{Triple differences by municipal characteristics}" _n
file write tex "\label{tab:logit_triple_mfx_stata}" _n
file write tex "\begin{tabular}{llccr}" _n
file write tex "\hline" _n

file write tex ///
    " & & \multicolumn{2}{c}{Logit average marginal effects} & \\" _n

file write tex ///
    "Municipal characteristic & Controls & 1936--1955 & 1956--1978 & N \\" _n

file write tex "\hline" _n


* ---------------------------------------------------------------------------- *
* Escribir filas
* ---------------------------------------------------------------------------- *

forvalues i = 1/`=_N' {

    local lab  = variable_label[`i']
    local ctrl = controls[`i']
    local a36  = ame_36[`i']
    local a56  = ame_56[`i']
    local n    = n_formatted[`i']

    file write tex ///
        "`lab' & `ctrl' & `a36' & `a56' & `n' \\" _n

    * Espacio después de cada par No/Yes
    if controls_order[`i'] == 2 & `i' < _N {
        file write tex "\addlinespace" _n
    }
}


* ---------------------------------------------------------------------------- *
* Notas
* ---------------------------------------------------------------------------- *

file write tex "\hline" _n
file write tex "\end{tabular}" _n

file write tex "\begin{minipage}{\textwidth}" _n

file write tex ///
    "\footnotesize Notes: The dependent variable is migration intention. Each pair of rows reports separate triple-difference Logit specifications for the municipal characteristic indicated in the first column. Entries report average marginal effects on the 0--1 probability scale for \(Post \times X \times Share_{1936-1955}\) and \(Post \times X \times Share_{1956-1978}\). All models include year and municipality fixed effects, use survey weights, and report standard errors clustered at the municipality level in parentheses. Specifications without controls include the two \(Post \times Share\) terms, \(Post \times X\), and the two corresponding triple interactions. Specifications with controls additionally include age, a male indicator, and \(Post\) interacted with the remaining municipal characteristics. * \(p<0.10\), ** \(p<0.05\), *** \(p<0.01\)." _n

file write tex "\end{minipage}" _n
file write tex "\end{table}" _n

file close tex

* ---------------------------------------------------------------------------- *
* 11. Heterogeneidad por terciles municipales comunes
* ---------------------------------------------------------------------------- *

use `lapop_ready', clear

* Los terciles ya fueron calculados sobre la base municipal amplia en
* 02_Data_API_Arg.R y se importaron desde data_eff_het.dta. No se recalculan
* sobre los municipios observados en LAPOP.
local tercile_vars ///
    t_density_2010 ///
    t_fem_2010 ///
    t_mean_schyr_2010 ///
    t_med_dage_2010 ///
    t_pea_2010 ///
    t_unemp_2010 ///
    t_izam_pre_avg ///
    t_alt_pre_avg

local source_vars ///
    popdensgeo2_2010 ///
    share_female_2010 ///
    mean_yrschool_2010 ///
    median_age_2010 ///
    share_laborforce_2010 ///
    share_unemployed_2010 ///
    share_izq_amplia_pre_avg ///
    share_alt_pre_avg

tempfile heterog_per_tercile heterog_joint heterog_pairwise
tempname h1 h2 h3

postfile `h1' ///
    str35 tercile_var byte tercile ///
    double ame_36 se_36 p_36 ame_56 se_56 p_56 ///
    double p_equal_cohorts ///
    long nobs double pseudo_r2 double tercile_ub ///
    long lpm_below0_n lpm_above1_n lpm_outside_n ///
    double lpm_outside_pct ///
    using `heterog_per_tercile', replace

postfile `h2' ///
    str35 tercile_var double p_joint_36 p_joint_56 ///
    using `heterog_joint', replace

postfile `h3' ///
    str35 tercile_var ///
    double p_36_T1T2 p_36_T1T3 p_36_T2T3 ///
    double p_56_T1T2 p_56_T1T3 p_56_T2T3 ///
    using `heterog_pairwise', replace

local i = 1
foreach tv of local tercile_vars {

    local sv : word `i' of `source_vars'
    local ++i

    matrix betas_36 = J(3,1,.)
    matrix vars_36  = J(3,3,0)
    matrix betas_56 = J(3,1,.)
    matrix vars_56  = J(3,3,0)

    forvalues t = 1/3 {

        display _newline ///
            "Heterogeneidad: `tv' | Tercil `t'"

        quietly logit intencion_migrar ///
            pshare36 pshare56 ///
            edad hombre ///
            i.year_num i.mun_code_num ///
            if `tv' == `t' [pw=wt], ///
            vce(cluster mun_code_num)

        local N  = e(N)
        local r2 = e(r2_p)

        tempvar sample_logit
        gen byte `sample_logit' = e(sample)

        quietly margins, ///
            dydx(pshare36 pshare56) ///
            post

        matrix M = r(table)

        local a36 = M[1,1]
        local s36 = M[2,1]
        local p36 = M[4,1]
        local a56 = M[1,2]
        local s56 = M[2,2]
        local p56 = M[4,2]

        quietly test pshare36 = pshare56
        local peq = r(p)

        quietly get_lpm_bounds ///
            pshare36 pshare56 edad hombre, ///
            sample(`sample_logit')

        local lpm_below = r(below0_n)
        local lpm_above = r(above1_n)
        local lpm_out   = r(outside_n)
        local lpm_pct   = r(outside_pct)

        quietly summarize `sv' if `tv' == `t', meanonly
        local ub = r(max)

        matrix betas_36[`t',1] = `a36'
        matrix vars_36[`t',`t'] = `s36'^2
        matrix betas_56[`t',1] = `a56'
        matrix vars_56[`t',`t'] = `s56'^2

        post `h1' ///
            ("`tv'") (`t') ///
            (`a36') (`s36') (`p36') ///
            (`a56') (`s56') (`p56') ///
            (`peq') (`N') (`r2') (`ub') ///
            (`lpm_below') (`lpm_above') (`lpm_out') (`lpm_pct')

        drop `sample_logit'
    }

    matrix R = (1,-1,0 \ 0,1,-1)

    matrix rb36 = R * betas_36
    matrix rv36 = R * vars_36 * R'
    matrix w36  = rb36' * invsym(rv36) * rb36
    local pj36  = chi2tail(2, w36[1,1])

    matrix rb56 = R * betas_56
    matrix rv56 = R * vars_56 * R'
    matrix w56  = rb56' * invsym(rv56) * rb56
    local pj56  = chi2tail(2, w56[1,1])

    post `h2' ("`tv'") (`pj36') (`pj56')

    local b1 = betas_36[1,1]
    local b2 = betas_36[2,1]
    local b3 = betas_36[3,1]
    local v1 = vars_36[1,1]
    local v2 = vars_36[2,2]
    local v3 = vars_36[3,3]

    local p36_12 = 2 * normal(-abs((`b2' - `b1') / sqrt(`v1' + `v2')))
    local p36_13 = 2 * normal(-abs((`b3' - `b1') / sqrt(`v1' + `v3')))
    local p36_23 = 2 * normal(-abs((`b3' - `b2') / sqrt(`v2' + `v3')))

    local b1 = betas_56[1,1]
    local b2 = betas_56[2,1]
    local b3 = betas_56[3,1]
    local v1 = vars_56[1,1]
    local v2 = vars_56[2,2]
    local v3 = vars_56[3,3]

    local p56_12 = 2 * normal(-abs((`b2' - `b1') / sqrt(`v1' + `v2')))
    local p56_13 = 2 * normal(-abs((`b3' - `b1') / sqrt(`v1' + `v3')))
    local p56_23 = 2 * normal(-abs((`b3' - `b2') / sqrt(`v2' + `v3')))

    post `h3' ///
        ("`tv'") ///
        (`p36_12') (`p36_13') (`p36_23') ///
        (`p56_12') (`p56_13') (`p56_23')
}

postclose `h1'
postclose `h2'
postclose `h3'

* ---------------------------------------------------------------------------- *
* 12. Exportar heterogeneidad y completar el Excel LPM del anexo
* ---------------------------------------------------------------------------- *

use `heterog_joint', clear
merge 1:m tercile_var using `heterog_per_tercile', nogenerate
sort tercile_var tercile

order ///
    tercile_var tercile ///
    ame_36 se_36 p_36 ///
    ame_56 se_56 p_56 ///
    p_equal_cohorts ///
    nobs pseudo_r2 tercile_ub ///
    lpm_below0_n lpm_above1_n lpm_outside_n lpm_outside_pct ///
    p_joint_36 p_joint_56

save ///
    "$output_models/heterog_terciles_migration_lapop_stata_LPM_appendix.dta", ///
    replace

* Output principal de heterogeneidad, sin columnas LPM
preserve
    keep ///
        tercile_var tercile ///
        ame_36 se_36 p_36 ///
        ame_56 se_56 p_56 ///
        p_equal_cohorts ///
        nobs pseudo_r2 tercile_ub ///
        p_joint_36 p_joint_56

    save ///
        "$output_models/heterog_terciles_migration_lapop_stata.dta", ///
        replace

    export excel using ///
        "$output/heterog_terciles_migration_lapop_stata.xlsx", ///
        firstrow(variables) replace
restore

* No se exporta diagnostico LPM por terciles.
* La heterogeneidad Logit y sus tests se mantienen sin cambios.
capture erase "$output_models/lpm_outside_01_heterogeneity_stata.dta"

use `heterog_pairwise', clear
save ///
    "$output_models/pairwise_terciles_migration_lapop_stata.dta", ///
    replace

export excel using ///
    "$output/pairwise_terciles_migration_lapop_stata.xlsx", ///
    firstrow(variables) replace

* ---------------------------------------------------------------------------- *
* 13. Tabla LaTeX principal de heterogeneidad, sin diagnostico LPM
* ---------------------------------------------------------------------------- *

use ///
    "$output_models/heterog_terciles_migration_lapop_stata.dta", ///
    clear

gen str20 f_ame36 = ///
    string(ame_36, "%9.3f") + ///
    cond(p_36 < 0.01, "***", ///
        cond(p_36 < 0.05, "**", ///
            cond(p_36 < 0.10, "*", "")))

gen str20 f_se36 = ///
    "(" + string(se_36, "%9.3f") + ")"

gen str20 f_ame56 = ///
    string(ame_56, "%9.3f") + ///
    cond(p_56 < 0.01, "***", ///
        cond(p_56 < 0.05, "**", ///
            cond(p_56 < 0.10, "*", "")))

gen str20 f_se56 = ///
    "(" + string(se_56, "%9.3f") + ")"

gen str20 f_peq = string(p_equal_cohorts, "%9.3f")
gen str20 f_ub  = "\(" + string(tercile_ub, "%9.3f") + "\)"
gen str20 f_n   = string(nobs, "%12.0fc")

capture file close tex
file open tex using ///
    "$output/migration_subsamples_lapop_stata.tex", ///
    write replace text

file write tex "\begin{table}[!h]" _n
file write tex "\normalsize" _n
file write tex "\renewcommand{\arraystretch}{1.15}" _n
file write tex "\centering" _n
file write tex "\setlength{\tabcolsep}{6pt}" _n
file write tex "\captionsetup{justification=centering}" _n
file write tex "\caption{Heterogeneous Effects on Migration Intention}" _n

local joint_text ""
local panel = 0

foreach tv of local tercile_vars {

    local ++panel
    local group "`tv'"

    if "`tv'" == "t_density_2010"   local group "Population density"
    if "`tv'" == "t_fem_2010"       local group "Female population share"
    if "`tv'" == "t_mean_schyr_2010" local group "Mean years of education"
    if "`tv'" == "t_med_dage_2010"  local group "Median age"
    if "`tv'" == "t_pea_2010"       local group "Share in labor force"
    if "`tv'" == "t_unemp_2010"     local group "Share unemployed"
    if "`tv'" == "t_izam_pre_avg"   local group "Left vote share"
    if "`tv'" == "t_alt_pre_avg"    local group "Ideological alternation"

    file write tex ///
        "\begin{tabular*}{\textwidth}{l@{\extracolsep{\fill}}ccc}" _n

    if `panel' == 1 {
        file write tex "\hline" _n
    }

    file write tex "& \multicolumn{3}{c}{`group'} \\" _n
    file write tex "\cmidrule(l){2-4}" _n
    file write tex "& T1 & T2 & T3 \\" _n
    file write tex "\hline" _n

    local a361 ""
    local a362 ""
    local a363 ""
    local s361 ""
    local s362 ""
    local s363 ""
    local a561 ""
    local a562 ""
    local a563 ""
    local s561 ""
    local s562 ""
    local s563 ""
    local n1 ""
    local n2 ""
    local n3 ""
    local p1 ""
    local p2 ""
    local p3 ""
    local u1 ""
    local u2 ""
    local u3 ""

    forvalues t = 1/3 {
        quietly levelsof f_ame36 if tercile_var == "`tv'" & tercile == `t', local(a36`t') clean
        quietly levelsof f_se36  if tercile_var == "`tv'" & tercile == `t', local(s36`t') clean
        quietly levelsof f_ame56 if tercile_var == "`tv'" & tercile == `t', local(a56`t') clean
        quietly levelsof f_se56  if tercile_var == "`tv'" & tercile == `t', local(s56`t') clean
        quietly levelsof f_n     if tercile_var == "`tv'" & tercile == `t', local(n`t') clean
        quietly levelsof f_peq   if tercile_var == "`tv'" & tercile == `t', local(p`t') clean
        quietly levelsof f_ub    if tercile_var == "`tv'" & tercile == `t', local(u`t') clean
    }

    file write tex ///
        "Spanish share 1936--1955\(\times\)Post & `a361' & `a362' & `a363' \\" _n
    file write tex " & `s361' & `s362' & `s363' \\" _n
    file write tex ///
        "Spanish share 1956--1978\(\times\)Post & `a561' & `a562' & `a563' \\" _n
    file write tex " & `s561' & `s562' & `s563' \\" _n
    file write tex "\addlinespace" _n
    file write tex "Observations & `n1' & `n2' & `n3' \\" _n
    file write tex ///
        "\(p\)-value (\(\beta_{36{-}55}=\beta_{56{-}78}\)) & `p1' & `p2' & `p3' \\" _n
    file write tex "Tercile upper bound & `u1' & `u2' & `u3' \\" _n
    file write tex "\hline" _n
    file write tex "\end{tabular*}" _n

    quietly summarize p_joint_36 if tercile_var == "`tv'", meanonly
    local j36 : display %5.3f r(mean)
    quietly summarize p_joint_56 if tercile_var == "`tv'", meanonly
    local j56 : display %5.3f r(mean)

    if "`joint_text'" == "" {
        local joint_text "`group' (`j36' / `j56')"
    }
    else {
        local joint_text "`joint_text'; `group' (`j36' / `j56')"
    }
}

file write tex "\addvspace{0.3em}" _n
file write tex ///
    "\captionsetup{font=footnotesize, justification=justified, singlelinecheck=false}" _n
file write tex ///
    "\caption*{\footnotesize Notes: The dependent variable is migration intention. Each set of columns reports subsample estimates by tercile of the specified municipal characteristic. The municipal characteristics and terciles are imported from Data Out/data_eff_het.dta and are therefore identical to those used in the electoral-outcome specifications. Census characteristics are measured in 2010; political characteristics are pre-treatment municipal averages. All specifications include municipality and year fixed effects and individual controls for age and a male indicator. The reported coefficients are average marginal effects from a Logit specification. Standard errors clustered at the municipality level are in parentheses. Joint Wald-test p-values, reported as (1936--1955 / 1956--1978), are: `joint_text'. * \(p<0.10\), ** \(p<0.05\), *** \(p<0.01\).}" _n
file write tex "\end{table}" _n
file close tex

* ---------------------------------------------------------------------------- *
* 14. Tabla LaTeX unificada del diagnostico LPM para el anexo
* Solo especificaciones con controles; se excluyen los terciles
* ---------------------------------------------------------------------------- *

* Eliminar las tablas separadas anteriores para evitar confusion
capture erase "$output_tex/lpm_outside_01_post_share_stata.tex"
capture erase "$output_tex/lpm_outside_01_post_x_stata.tex"
capture erase "$output_tex/lpm_outside_01_triple_stata.tex"
capture erase "$output_tex/lpm_outside_01_heterogeneity_stata.tex"

capture file close tex

file open tex using ///
    "$output_tex/lpm_outside_01_all_stata.tex", ///
    write replace text

file write tex "\begin{table}[!htbp]" _n
file write tex "\normalsize" _n
file write tex "\renewcommand{\arraystretch}{1.15}" _n
file write tex "\centering" _n
file write tex ///
    "\caption{Linear probability model: proportion of predictions outside [0,1]}" _n
file write tex "\label{tab:lpm_outside_all}" _n

file write tex "\begin{tabular}{lc}" _n
file write tex "\hline" _n
file write tex "Specification & Controls \\" _n
file write tex "\hline" _n

* Panel A: Post x exposicion historica
file write tex ///
    "\multicolumn{2}{l}{\textit{Panel A: Post-period Spanish historical exposure}} \\" _n

use "$output_models/lpm_outside_01_post_share_stata.dta", clear

gen str12 yes_fmt = string(controls_pct, "%5.2f") + "\%"
local p1 = yes_fmt[1]

file write tex ///
    "Post-period Spanish historical exposure & `p1' \\" _n

file write tex "\addlinespace" _n

* Panel B: Post x caracteristicas municipales comunes
file write tex ///
    "\multicolumn{2}{l}{\textit{Panel B: Post-period municipal characteristics}} \\" _n

use "$output_models/lpm_outside_01_post_x_stata.dta", clear

gen str12 yes_fmt = string(controls_pct, "%5.2f") + "\%"
sort row_order

forvalues i = 1/`=_N' {
    local lab = term_label[`i']
    local p1  = yes_fmt[`i']

    file write tex ///
        "`lab' & `p1' \\" _n
}

file write tex "\addlinespace" _n

* Panel C: triples diferencias
file write tex ///
    "\multicolumn{2}{l}{\textit{Panel C: Triple-difference specifications}} \\" _n

use "$output_models/lpm_outside_01_triple_stata.dta", clear

gen str12 yes_fmt = string(controls_pct, "%5.2f") + "\%"
sort row_order

forvalues i = 1/`=_N' {
    local lab = variable_label[`i']
    local p1  = yes_fmt[`i']

    file write tex ///
        "`lab' & `p1' \\" _n
}

file write tex "\hline" _n
file write tex "\end{tabular}" _n
file write tex "\vspace{0.4em}" _n

file write tex "\begin{minipage}{0.98\textwidth}" _n
file write tex ///
    "\footnotesize Notes: Entries report the unweighted proportion of observations, expressed as a percentage, whose fitted value from the linear probability model is strictly below 0 or above 1. Only specifications with controls are reported. Each LPM uses the same estimation sample, regressors, survey weights, year fixed effects, and municipality fixed effects as the corresponding Logit model. In Panels A and B, controls include age and a male indicator. In Panel C, controls include age, male, and Post interacted with the remaining municipal characteristics. Heterogeneity specifications by municipal tercile are not included." _n

file write tex "\end{minipage}" _n
file write tex "\end{table}" _n
file close tex

* ---------------------------------------------------------------------------- *
* 15. Mensaje final
* ---------------------------------------------------------------------------- *

display as result _newline ///
    "Proceso terminado. Tablas Logit principales:"

display as result "  `excel_file'"
display as result ///
    "  $output_tex/logit_post_share_mfx_glm_cluster_se_stata.tex"
display as result ///
    "  $output_tex/logit_post_x_mfx_glm_cluster_se_stata.tex"
display as result ///
    "  $output_tex/logit_triple_mfx_glm_cluster_se_stata.tex"
display as result ///
    "  $output/heterog_terciles_migration_lapop_stata.xlsx"
display as result ///
    "  $output/migration_subsamples_lapop_stata.tex"

display as result _newline ///
    "Tablas LPM separadas para el anexo:"

display as result "  `lpm_excel'"
display as result _newline ///
    "Excel del diagnostico LPM:"

display as result ///
    "  `lpm_excel'"

display as result _newline ///
    "Tabla LPM unificada para el anexo:"

display as result ///
    "  $output_tex/lpm_outside_01_all_stata.tex"


* ============================================================================ *
* APPEND-ONLY BLOCK FOR:
* 09_LAPOP_logit_marginal_effects_stata_LPM_newvars.do
*
* Paste this block AFTER the current last section of that do-file.
*
* Purpose:
*   Replace the remaining mixed tables with Logit average marginal effects
*   and use the LPM only to diagnose fitted values outside [0,1].
*
* Tables covered:
*   A10  Individual characteristics
*   A14  Triple-difference models: all interaction terms
*   A18  Post x pooled Spanish exposure (1970 and 1980 Census measures)
*   A19  Triple differences with pooled Spanish exposure
*   A20  All pooled-share interactions, 1970 Census measure
*   A21  All pooled-share interactions, 1980 Census measure
*   A22  Pooled-share pre-trends (reconstructed from the table specification)
*
* AME are reported on the 0--1 probability scale. Each LPM is estimated on exactly the
* same sample and with the same regressors as its corresponding Logit model,
* but only the number and percentage of fitted values outside [0,1] are retained.
* ============================================================================ *

* ---------------------------------------------------------------------------- *
* 16. Setup and data needed by the remaining mixed tables
* ---------------------------------------------------------------------------- *

if "$main" == "" {
    display as error "Run this block after the main marginal-effects do-file."
    exit 198
}

if "$scale_factor" == "" global scale_factor 1

use "$data_int/lapop_logit_mfx_ready_LPM_appendix.dta", clear

* Variables used by the individual-characteristics table.
* A10 is an individual-level table, so these variables necessarily come from LAPOP.
local extra_numeric ///
    secundaria_completa_o_mas ///
    share_36_78_c70 share_36_78_c80

foreach v of local extra_numeric {
    capture confirm variable `v'
    if !_rc {
        capture confirm numeric variable `v'
        if _rc {
            replace `v' = strtrim(`v')
            replace `v' = "" if inlist(upper(`v'), "NA", "N/A", "NAN", "NULL", ".")
            destring `v', replace
        }
    }
}

* Merge the two pooled exposure measures when they are not already in LAPOP.
local need_pooled = 0
foreach v in share_36_78_c70 share_36_78_c80 {
    capture confirm variable `v'
    if _rc local need_pooled = 1
}

if `need_pooled' {
    capture confirm file "$data_out/spanish_cohorts_arg.csv"
    if _rc {
        display as error "Missing file: $data_out/spanish_cohorts_arg.csv"
        exit 601
    }

    tempfile pooled_shares
    preserve
        import delimited using "$data_out/spanish_cohorts_arg.csv", clear ///
            varnames(1) encoding(UTF-8)

        keep mun_code share_36_78_c70 share_36_78_c80

        foreach v in mun_code share_36_78_c70 share_36_78_c80 {
            capture confirm numeric variable `v'
            if _rc {
                replace `v' = strtrim(`v')
                replace `v' = "" if inlist(upper(`v'), "NA", "N/A", "NAN", "NULL", ".")
                destring `v', replace
            }
        }

        bysort mun_code: keep if _n == 1
        save `pooled_shares', replace
    restore

    capture drop share_36_78_c70 share_36_78_c80
    merge m:1 mun_code using `pooled_shares', keep(master match) nogen
}

* Check variables required for Table A10.
local individual_required ///
    edad hombre desempleado en_pareja secundaria_completa_o_mas ///
    izq_der interes_pol_mucho voto_blanco_nulo

foreach v of local individual_required {
    capture confirm variable `v'
    if _rc {
        display as error "Required variable not found: `v'"
        exit 111
    }
}

* Municipal characteristics used in the existing Stata marginal-effects tables.
* These are the common census/electoral variables imported from data_eff_het.dta,
* not municipal averages constructed from LAPOP respondents.
local common_x_vars ///
    popdensgeo2_2010 ///
    share_female_2010 ///
    mean_yrschool_2010 ///
    median_age_2010 ///
    share_laborforce_2010 ///
    share_unemployed_2010 ///
    share_izq_amplia_pre_avg ///
    share_alt_pre_avg

foreach x of local common_x_vars {
    capture confirm variable `x'
    if _rc {
        display as error "Common municipal variable not found: `x'"
        exit 111
    }
}

* Explicit interaction variables used by A14 and the pooled tables.
capture confirm variable pshare36
if _rc gen double pshare36 = post * share_1936_1955
capture confirm variable pshare56
if _rc gen double pshare56 = post * share_1956_1978

capture drop pp70 pp80
gen double pp70 = post * share_36_78_c70
gen double pp80 = post * share_36_78_c80

foreach x of local common_x_vars {
    * px_*, pxs36_* and pxs56_* normally already exist because the main
    * marginal-effects do-file creates them from the census/electoral variables.
    capture confirm variable px_`x'
    if _rc gen double px_`x' = post * `x'

    capture confirm variable pxs36_`x'
    if _rc gen double pxs36_`x' = post * `x' * share_1936_1955

    capture confirm variable pxs56_`x'
    if _rc gen double pxs56_`x' = post * `x' * share_1956_1978

    capture drop ppx70_`x' ppx80_`x'
    gen double ppx70_`x' = post * `x' * share_36_78_c70
    gen double ppx80_`x' = post * `x' * share_36_78_c80
}

compress
save "$data_int/lapop_remaining_mixed_tables_ready.dta", replace

* ---------------------------------------------------------------------------- *
* 17. Additional helpers
* ---------------------------------------------------------------------------- *

capture program drop get_margins_one
program define get_margins_one, rclass
    version 17
    syntax varname

    quietly margins, dydx(`varlist')
    matrix M = r(table)

    return scalar ame = M[1,1]
    return scalar se  = M[2,1]
    return scalar p   = M[4,1]
end

* The LPM is used only as a diagnostic for fitted values outside [0,1].
* Coefficients, standard errors, and p-values from the LPM are not exported.

* Workbook containing the additional Logit AME results.
local separated_xlsx "$output/remaining_mixed_tables_AME_stata.xlsx"
capture erase "`separated_xlsx'"

* One observation per additional LPM specification.
tempname remaining_lpm_h
tempfile remaining_lpm_diagnostics

postfile `remaining_lpm_h' ///
    byte table_order str12 table_id ///
    str80 specification str70 characteristic ///
    str20 exposure str12 controls ///
    long nobs below0_n above1_n outside_n ///
    double outside_pct ///
    using `remaining_lpm_diagnostics', replace

* ---------------------------------------------------------------------------- *
* 18. TABLE A10: individual characteristics
* ---------------------------------------------------------------------------- *

use "$data_int/lapop_remaining_mixed_tables_ready.dta", clear

tempname ind_ame_h
tempfile ind_ame_long

postfile `ind_ame_h' ///
    byte spec_order str25 spec str40 spec_label ///
    byte var_order str40 variable str70 variable_label ///
    double estimate se p_value long nobs ///
    using `ind_ame_long', replace


local ind_vars_1 ///
    "edad hombre desempleado en_pareja secundaria_completa_o_mas izq_der interes_pol_mucho voto_blanco_nulo"
local ind_vars_2 ///
    "edad hombre desempleado en_pareja secundaria_completa_o_mas izq_der interes_pol_mucho"
local ind_vars_3 ///
    "edad hombre desempleado en_pareja secundaria_completa_o_mas interes_pol_mucho voto_blanco_nulo"

local ind_spec_1 "full"
local ind_spec_2 "no_blank_null_vote"
local ind_spec_3 "no_ideology"
local ind_label_1 "Full"
local ind_label_2 "No blank/null vote"
local ind_label_3 "No ideology"

forvalues s = 1/3 {
    local vars  "`ind_vars_`s''"
    local spec  "`ind_spec_`s''"
    local slab  "`ind_label_`s''"

    display as text _newline "A10 | `slab'"

    quietly logit intencion_migrar ///
        `vars' ///
        i.year_num i.mun_code_num ///
        [pw=wt], vce(cluster mun_code_num)

    local N = e(N)
    tempvar sample_logit
    gen byte `sample_logit' = e(sample)

    foreach v of local vars {
        quietly get_margins_one `v'

        local vorder = .
        local vlabel "`v'"
        if "`v'" == "edad"                        local vorder = 1
        if "`v'" == "edad"                        local vlabel "Age"
        if "`v'" == "hombre"                      local vorder = 2
        if "`v'" == "hombre"                      local vlabel "Male"
        if "`v'" == "desempleado"                 local vorder = 3
        if "`v'" == "desempleado"                 local vlabel "Unemployed"
        if "`v'" == "en_pareja"                   local vorder = 4
        if "`v'" == "en_pareja"                   local vlabel "Partnered"
        if "`v'" == "secundaria_completa_o_mas"   local vorder = 5
        if "`v'" == "secundaria_completa_o_mas"   local vlabel "High School or more"
        if "`v'" == "izq_der"                     local vorder = 6
        if "`v'" == "izq_der"                     local vlabel "Left-right ideology"
        if "`v'" == "interes_pol_mucho"            local vorder = 7
        if "`v'" == "interes_pol_mucho"            local vlabel "Very interested in politics"
        if "`v'" == "voto_blanco_nulo"             local vorder = 8
        if "`v'" == "voto_blanco_nulo"             local vlabel "Blank/null vote"

        post `ind_ame_h' ///
            (`s') ("`spec'") ("`slab'") ///
            (`vorder') ("`v'") ("`vlabel'") ///
            (r(ame) * $scale_factor) (r(se) * $scale_factor) (r(p)) (`N')
    }

    quietly get_lpm_bounds `vars', sample(`sample_logit')

    post `remaining_lpm_h' ///
        (5) ("A10") ///
        ("`slab'") ("") ("") ("Yes") ///
        (r(nobs)) (r(below0_n)) (r(above1_n)) ///
        (r(outside_n)) (r(outside_pct))

    drop `sample_logit'
}

postclose `ind_ame_h'

use `ind_ame_long', clear
save "$output_models/individual_characteristics_ame_stata.dta", replace
export excel using "`separated_xlsx'", sheet("A10_individual_AME") firstrow(variables) replace


* A10 Logit AME LaTeX.
use "$output_models/individual_characteristics_ame_stata.dta", clear
gen str40 result = string(estimate, "%9.3f") + ///
    cond(p_value < .01, "***", cond(p_value < .05, "**", cond(p_value < .10, "*", ""))) + ///
    " (" + string(se, "%9.3f") + ")"

preserve
    bysort spec_order: keep if _n == 1
    sort spec_order
    forvalues s = 1/3 {
        quietly summarize nobs if spec_order == `s', meanonly
        local ind_ame_n`s' : display %12.0fc r(mean)
    }
restore

keep var_order variable variable_label spec_order result
reshape wide result, i(var_order variable variable_label) j(spec_order)
sort var_order

capture file close tex
file open tex using "$output_tex/individual_characteristics_logit_ame_stata.tex", write replace text
file write tex "\begin{table}[!htbp]" _n
file write tex "\normalsize" _n
file write tex "\renewcommand{\arraystretch}{1.15}" _n
file write tex "\centering" _n
file write tex "\caption{Individual characteristics and migration intention: Logit average marginal effects}" _n
file write tex "\label{tab:individual_characteristics_ame_stata}" _n
file write tex "\setlength{\tabcolsep}{4pt}" _n
file write tex "\captionsetup{justification=centering}" _n
file write tex "\begin{tabular}{lccc}" _n
file write tex "\hline" _n
file write tex "Variable & Full & No blank/null vote & No ideology \\" _n
file write tex "\hline" _n
forvalues i = 1/`=_N' {
    local lab = variable_label[`i']
    local c1 = result1[`i']
    local c2 = result2[`i']
    local c3 = result3[`i']
    file write tex "`lab' & `c1' & `c2' & `c3' \\" _n
}
file write tex "\hline" _n
file write tex "Observations & `ind_ame_n1' & `ind_ame_n2' & `ind_ame_n3' \\" _n
file write tex "\hline" _n
file write tex "\end{tabular}" _n
file write tex "\par\vspace{1.5mm}" _n
file write tex "\begin{minipage}{0.98\linewidth}" _n
file write tex "\footnotesize\setlength{\parindent}{0pt}\setlength{\parskip}{1pt}" _n
file write tex "Notes: Entries are Logit average marginal effects on the 0--1 probability scale; clustered standard errors are in parentheses. All models include year and municipality fixed effects and use survey weights. * \(p<0.10\), ** \(p<0.05\), *** \(p<0.01\)." _n
file write tex "\end{minipage}" _n
file write tex "\end{table}" _n
file close tex

* ---------------------------------------------------------------------------- *
* 19. TABLE A14: all terms from the two-window triple-difference models
* ---------------------------------------------------------------------------- *

use "$data_int/lapop_remaining_mixed_tables_ready.dta", clear

tempname all2_ame_h
tempfile all2_ame_long

postfile `all2_ame_h' ///
    byte x_order str40 x str70 x_label byte term_order ///
    str60 term_id str100 term_label byte control_order str3 controls ///
    double estimate se p_value long nobs ///
    using `all2_ame_long', replace


local xcounter = 0
foreach x of local common_x_vars {
    local ++xcounter

    local xlabel "`x'"
    if "`x'" == "popdensgeo2_2010"      local xlabel "Population density"
    if "`x'" == "share_female_2010"     local xlabel "Female population share"
    if "`x'" == "mean_yrschool_2010"    local xlabel "Mean years of education"
    if "`x'" == "median_age_2010"        local xlabel "Median age"
    if "`x'" == "share_laborforce_2010"     local xlabel "Share in labor force"
    if "`x'" == "share_unemployed_2010"     local xlabel "Share unemployed"
    if "`x'" == "share_izq_amplia_pre_avg"  local xlabel "Left vote share"
    if "`x'" == "share_alt_pre_avg"      local xlabel "Ideological alternation"

    foreach corder in 1 2 {
        local controls "No"
        local rhs_extra ""
        if `corder' == 2 {
            local controls "Yes"
            local rhs_extra "edad hombre"
            foreach z of local common_x_vars {
                if "`z'" != "`x'" local rhs_extra "`rhs_extra' px_`z'"
            }
        }

        local rhs_core "pshare36 pshare56 px_`x' pxs36_`x' pxs56_`x'"

        quietly logit intencion_migrar ///
            `rhs_core' `rhs_extra' ///
            i.year_num i.mun_code_num ///
            [pw=wt], vce(cluster mun_code_num)

        local N = e(N)
        tempvar sample_logit
        gen byte `sample_logit' = e(sample)

        local terms "pshare36 pshare56 px_`x' pxs36_`x' pxs56_`x'"
        local t = 0
        foreach term of local terms {
            local ++t
            local tid "`term'"
            local tlab ""
            if `t' == 1 local tlab "\(Post \times Share_{1936-1955}\)"
            if `t' == 2 local tlab "\(Post \times Share_{1956-1978}\)"
            if `t' == 3 local tlab "\(Post \times X\)"
            if `t' == 4 local tlab "\(Post \times X \times Share_{1936-1955}\)"
            if `t' == 5 local tlab "\(Post \times X \times Share_{1956-1978}\)"

            quietly get_margins_one `term'
            post `all2_ame_h' ///
                (`xcounter') ("`x'") ("`xlabel'") (`t') ///
                ("`tid'") ("`tlab'") (`corder') ("`controls'") ///
                (r(ame) * $scale_factor) (r(se) * $scale_factor) (r(p)) (`N')
        }

        quietly get_lpm_bounds `rhs_core' `rhs_extra', ///
            sample(`sample_logit')

        post `remaining_lpm_h' ///
            (6) ("A14") ///
            ("All interaction terms") ("`xlabel'") ///
            ("Two windows") ("`controls'") ///
            (r(nobs)) (r(below0_n)) (r(above1_n)) ///
            (r(outside_n)) (r(outside_pct))

        drop `sample_logit'
    }
}

postclose `all2_ame_h'

use `all2_ame_long', clear
save "$output_models/triple_all_interactions_ame_stata.dta", replace
export excel using "`separated_xlsx'", sheet("A14_all_terms_AME") firstrow(variables) sheetreplace


* Generic A14-style Logit AME table.
use "$output_models/triple_all_interactions_ame_stata.dta", clear
gen str40 result = string(estimate, "%9.3f") + ///
    cond(p_value < .01, "***", cond(p_value < .05, "**", cond(p_value < .10, "*", ""))) + ///
    " (" + string(se, "%9.3f") + ")"
keep x_order x x_label term_order term_label control_order result nobs
reshape wide result nobs, i(x_order x x_label term_order term_label) j(control_order)
gen str35 n_pair = string(nobs1, "%12.0fc") + " / " + string(nobs2, "%12.0fc")
sort x_order term_order

capture file close tex
file open tex using "$output_tex/triple_all_interactions_logit_ame_stata.tex", write replace text
file write tex "\begin{table}[!htbp]" _n
file write tex "\normalsize" _n
file write tex "\renewcommand{\arraystretch}{1.15}" _n
file write tex "\centering" _n
file write tex "\caption{Triple-difference models: all interaction terms, Logit average marginal effects}" _n
file write tex "\label{tab:triple_all_interactions_ame_stata}" _n
file write tex "\setlength{\tabcolsep}{4pt}" _n
file write tex "\captionsetup{justification=centering}" _n
file write tex "\begin{tabular}{llccr}" _n
file write tex "\hline" _n
file write tex "Municipal characteristic & Term & No controls & Controls & N (No/Yes) \\" _n
file write tex "\hline" _n
forvalues i = 1/`=_N' {
    local xprint = x_label[`i']
    if `i' > 1 & x_order[`i'] == x_order[`i'-1] local xprint ""
    local term = term_label[`i']
    local c1 = result1[`i']
    local c2 = result2[`i']
    local npair = n_pair[`i']
    file write tex "`xprint' & `term' & `c1' & `c2' & `npair' \\" _n
    if term_order[`i'] == 5 & `i' < _N file write tex "\addlinespace" _n
}
file write tex "\hline" _n
file write tex "\end{tabular}" _n
file write tex "\par\vspace{1.5mm}" _n
file write tex "\begin{minipage}{0.98\linewidth}" _n
file write tex "\footnotesize\setlength{\parindent}{0pt}\setlength{\parskip}{1pt}" _n
file write tex "Notes: Entries are Logit average marginal effects on the 0--1 probability scale; clustered standard errors are in parentheses. N is reported as observations without controls / observations with controls. All models include year and municipality fixed effects and use survey weights. Controls additionally include age, male, and Post interacted with the remaining common census/electoral municipal characteristics. Lower-order interaction terms are conditional on the other interacted variables being zero. * \(p<0.10\), ** \(p<0.05\), *** \(p<0.01\)." _n
file write tex "\end{minipage}" _n
file write tex "\end{table}" _n
file close tex

* ---------------------------------------------------------------------------- *
* 20. TABLES A18-A21: pooled Spanish exposure
* ---------------------------------------------------------------------------- *

use "$data_int/lapop_remaining_mixed_tables_ready.dta", clear

tempname poolpost_ame_h poolall_ame_h
tempfile poolpost_ame_long poolall_ame_long

postfile `poolpost_ame_h' ///
    byte exposure_order str3 exposure str40 exposure_label ///
    byte control_order str3 controls double estimate se p_value long nobs ///
    using `poolpost_ame_long', replace


postfile `poolall_ame_h' ///
    byte exposure_order str3 exposure str40 exposure_label ///
    byte x_order str40 x str70 x_label byte term_order ///
    str100 term_label byte control_order str3 controls ///
    double estimate se p_value long nobs ///
    using `poolall_ame_long', replace


foreach eorder in 1 2 {
    local exposure "c70"
    local elabel "Spanish presence from 1970 Census"
    local pterm "pp70"
    local sharevar "share_36_78_c70"
    local exposure_diag "1970 Census"
    if `eorder' == 2 {
        local exposure "c80"
        local elabel "Spanish presence from 1980 Census"
        local pterm "pp80"
        local sharevar "share_36_78_c80"
        local exposure_diag "1980 Census"
    }

    * A18: Post x pooled exposure.
    foreach corder in 1 2 {
        local controls "No"
        local rhs_extra ""
        if `corder' == 2 {
            local controls "Yes"
            local rhs_extra "edad hombre"
        }

        quietly logit intencion_migrar ///
            `pterm' `rhs_extra' ///
            i.year_num i.mun_code_num ///
            [pw=wt], vce(cluster mun_code_num)

        local N = e(N)
        tempvar sample_logit
        gen byte `sample_logit' = e(sample)

        quietly get_margins_one `pterm'
        post `poolpost_ame_h' ///
            (`eorder') ("`exposure'") ("`elabel'") ///
            (`corder') ("`controls'") ///
            (r(ame) * $scale_factor) (r(se) * $scale_factor) (r(p)) (`N')

        quietly get_lpm_bounds `pterm' `rhs_extra', ///
            sample(`sample_logit')

        post `remaining_lpm_h' ///
            (7) ("A18") ///
            ("Post x pooled Spanish exposure") ("") ///
            ("`exposure_diag'") ("`controls'") ///
            (r(nobs)) (r(below0_n)) (r(above1_n)) ///
            (r(outside_n)) (r(outside_pct))

        drop `sample_logit'
    }

    * A19-A21: pooled triple models; store all three interaction terms.
    local xcounter = 0
    foreach x of local common_x_vars {
        local ++xcounter

        local xlabel "`x'"
        if "`x'" == "popdensgeo2_2010"          local xlabel "Population density"
        if "`x'" == "share_female_2010"         local xlabel "Female population share"
        if "`x'" == "mean_yrschool_2010"        local xlabel "Mean years of education"
        if "`x'" == "median_age_2010"            local xlabel "Median age"
        if "`x'" == "share_laborforce_2010"     local xlabel "Share in labor force"
        if "`x'" == "share_unemployed_2010"     local xlabel "Share unemployed"
        if "`x'" == "share_izq_amplia_pre_avg"  local xlabel "Left vote share"
        if "`x'" == "share_alt_pre_avg"          local xlabel "Ideological alternation"

        local triplevar "ppx70_`x'"
        if `eorder' == 2 local triplevar "ppx80_`x'"

        foreach corder in 1 2 {
            local controls "No"
            local rhs_extra ""
            if `corder' == 2 {
                local controls "Yes"
                local rhs_extra "edad hombre"
                foreach z of local common_x_vars {
                    if "`z'" != "`x'" local rhs_extra "`rhs_extra' px_`z'"
                }
            }

            local rhs_core "`pterm' px_`x' `triplevar'"

            quietly logit intencion_migrar ///
                `rhs_core' `rhs_extra' ///
                i.year_num i.mun_code_num ///
                [pw=wt], vce(cluster mun_code_num)

            local N = e(N)
            tempvar sample_logit
            gen byte `sample_logit' = e(sample)

            local terms "`pterm' px_`x' `triplevar'"
            local t = 0
            foreach term of local terms {
                local ++t
                local tlab ""
                if `t' == 1 local tlab "\(Post \times Share_{1936-1978}\)"
                if `t' == 2 local tlab "\(Post \times X\)"
                if `t' == 3 local tlab "\(Post \times X \times Share_{1936-1978}\)"

                quietly get_margins_one `term'
                post `poolall_ame_h' ///
                    (`eorder') ("`exposure'") ("`elabel'") ///
                    (`xcounter') ("`x'") ("`xlabel'") (`t') ///
                    ("`tlab'") (`corder') ("`controls'") ///
                    (r(ame) * $scale_factor) (r(se) * $scale_factor) (r(p)) (`N')
            }

            quietly get_lpm_bounds `rhs_core' `rhs_extra', ///
                sample(`sample_logit')

            post `remaining_lpm_h' ///
                (8) ("A19-A21") ///
                ("Pooled triple difference") ("`xlabel'") ///
                ("`exposure_diag'") ("`controls'") ///
                (r(nobs)) (r(below0_n)) (r(above1_n)) ///
                (r(outside_n)) (r(outside_pct))

            drop `sample_logit'
        }
    }
}

postclose `poolpost_ame_h'
postclose `poolall_ame_h'

use `poolpost_ame_long', clear
save "$output_models/post_pooled_share_ame_stata.dta", replace
export excel using "`separated_xlsx'", sheet("A18_pooled_post_AME") firstrow(variables) sheetreplace


use `poolall_ame_long', clear
save "$output_models/triple_pooled_all_terms_ame_stata.dta", replace
export excel using "`separated_xlsx'", sheet("A19_A21_pooled_AME") firstrow(variables) sheetreplace


* A18 AME table.
use "$output_models/post_pooled_share_ame_stata.dta", clear
gen str40 result = string(estimate, "%9.3f") + ///
    cond(p_value < .01, "***", cond(p_value < .05, "**", cond(p_value < .10, "*", ""))) + ///
    " (" + string(se, "%9.3f") + ")"
gen byte model_order = (exposure_order - 1) * 2 + control_order
gen byte row_id = 1
keep row_id model_order result nobs
reshape wide result nobs, i(row_id) j(model_order)

capture file close tex
file open tex using "$output_tex/post_pooled_share_logit_ame_stata.tex", write replace text
file write tex "\begin{table}[!htbp]" _n
file write tex "\normalsize" _n
file write tex "\renewcommand{\arraystretch}{1.15}" _n
file write tex "\centering" _n
file write tex "\caption{Effect on migration intentions: pooled Spanish exposure, Logit average marginal effects}" _n
file write tex "\label{tab:post_pooled_share_ame_stata}" _n
file write tex "\setlength{\tabcolsep}{4pt}" _n
file write tex "\captionsetup{justification=centering}" _n
file write tex "\begin{tabular}{lcccc}" _n
file write tex "\hline" _n
file write tex " & (1) & (2) & (3) & (4) \\" _n
file write tex "\hline" _n
local c1 = result1[1]
local c2 = result2[1]
local c3 = result3[1]
local c4 = result4[1]
file write tex "Spanish share 1936--1978 \(\times\) Post & `c1' & `c2' & `c3' & `c4' \\" _n
file write tex "\hline" _n
local n1 : display %12.0fc nobs1[1]
local n2 : display %12.0fc nobs2[1]
local n3 : display %12.0fc nobs3[1]
local n4 : display %12.0fc nobs4[1]
file write tex "Observations & `n1' & `n2' & `n3' & `n4' \\" _n
file write tex "Year FE & Yes & Yes & Yes & Yes \\" _n
file write tex "Municipality FE & Yes & Yes & Yes & Yes \\" _n
file write tex "Controls & No & Yes & No & Yes \\" _n
file write tex "Spanish share census & 1970 & 1970 & 1980 & 1980 \\" _n
file write tex "\hline" _n
file write tex "\end{tabular}" _n
file write tex "\par\vspace{1.5mm}" _n
file write tex "\begin{minipage}{0.98\linewidth}" _n
file write tex "\footnotesize\setlength{\parindent}{0pt}\setlength{\parskip}{1pt}" _n
file write tex "Notes: Entries are Logit average marginal effects on the 0--1 probability scale for \(Post \times Share_{1936-1978}\). All models include year and municipality fixed effects, use survey weights, and cluster standard errors by municipality. Controls include age and male. * \(p<0.10\), ** \(p<0.05\), *** \(p<0.01\)." _n
file write tex "\end{minipage}" _n
file write tex "\end{table}" _n
file close tex

* A19 key triple terms, AME: keep term 3 and combine c70/c80.
use "$output_models/triple_pooled_all_terms_ame_stata.dta", clear
keep if term_order == 3
gen str40 result = string(estimate, "%9.3f") + ///
    cond(p_value < .01, "***", cond(p_value < .05, "**", cond(p_value < .10, "*", ""))) + ///
    " (" + string(se, "%9.3f") + ")"
keep x_order x x_label control_order controls exposure_order result nobs
reshape wide result nobs, i(x_order x x_label control_order controls) j(exposure_order)
gen str35 n_pair = string(nobs1, "%12.0fc") + " / " + string(nobs2, "%12.0fc")
sort x_order control_order

capture file close tex
file open tex using "$output_tex/triple_pooled_share_logit_ame_stata.tex", write replace text
file write tex "\begin{table}[!htbp]" _n
file write tex "\normalsize" _n
file write tex "\renewcommand{\arraystretch}{1.15}" _n
file write tex "\centering" _n
file write tex "\caption{Triple differences: pooled Spanish exposure, Logit average marginal effects}" _n
file write tex "\label{tab:triple_pooled_share_ame_stata}" _n
file write tex "\setlength{\tabcolsep}{4pt}" _n
file write tex "\captionsetup{justification=centering}" _n
file write tex "\begin{tabular}{llccr}" _n
file write tex "\hline" _n
file write tex "Municipal characteristic & Controls & 1970 Census & 1980 Census & N (1970/1980) \\" _n
file write tex "\hline" _n
forvalues i = 1/`=_N' {
    local lab = x_label[`i']
    local ctrl = controls[`i']
    local c70 = result1[`i']
    local c80 = result2[`i']
    local npair = n_pair[`i']
    file write tex "`lab' & `ctrl' & `c70' & `c80' & `npair' \\" _n
    if control_order[`i'] == 2 & `i' < _N file write tex "\addlinespace" _n
}
file write tex "\hline" _n
file write tex "\end{tabular}" _n
file write tex "\par\vspace{1.5mm}" _n
file write tex "\begin{minipage}{0.98\linewidth}" _n
file write tex "\footnotesize\setlength{\parindent}{0pt}\setlength{\parskip}{1pt}" _n
file write tex "Notes: Entries are Logit average marginal effects on the 0--1 probability scale for \(Post \times X \times Share_{1936-1978}\). N is reported as observations in the 1970-Census model / observations in the 1980-Census model. All models include year and municipality fixed effects, survey weights, and municipality-clustered standard errors. Controls include age, male, and Post interacted with the remaining common census/electoral municipal characteristics. * \(p<0.10\), ** \(p<0.05\), *** \(p<0.01\)." _n
file write tex "\end{minipage}" _n
file write tex "\end{table}" _n
file close tex

* A20/A21 all-interaction tables: one AME and one LPM table per Census measure.
foreach eorder in 1 2 {
    local exposure "c70"
    local etitle "Spanish presence from 1970 Census"
    if `eorder' == 2 {
        local exposure "c80"
        local etitle "Spanish presence from 1980 Census"
    }

    use "$output_models/triple_pooled_all_terms_ame_stata.dta", clear
    keep if exposure_order == `eorder'
    gen str40 result = string(estimate, "%9.3f") + ///
        cond(p_value < .01, "***", cond(p_value < .05, "**", cond(p_value < .10, "*", ""))) + ///
        " (" + string(se, "%9.3f") + ")"
    keep x_order x x_label term_order term_label control_order result nobs
    reshape wide result nobs, i(x_order x x_label term_order term_label) j(control_order)
    gen str35 n_pair = string(nobs1, "%12.0fc") + " / " + string(nobs2, "%12.0fc")
    sort x_order term_order

    capture file close tex
    file open tex using "$output_tex/triple_pooled_all_interactions_`exposure'_logit_ame_stata.tex", write replace text
    file write tex "\begin{table}[!htbp]" _n
    file write tex "\normalsize" _n
    file write tex "\renewcommand{\arraystretch}{1.15}" _n
    file write tex "\centering" _n
    file write tex "\caption{Triple-difference models: all interaction terms, `etitle', Logit average marginal effects}" _n
    file write tex "\label{tab:triple_pooled_all_`exposure'_ame_stata}" _n
file write tex "\setlength{\tabcolsep}{4pt}" _n
file write tex "\captionsetup{justification=centering}" _n
    file write tex "\begin{tabular}{llccr}" _n
    file write tex "\hline" _n
    file write tex "Municipal characteristic & Term & No controls & Controls & N (No/Yes) \\" _n
    file write tex "\hline" _n
    forvalues i = 1/`=_N' {
        local xprint = x_label[`i']
        if `i' > 1 & x_order[`i'] == x_order[`i'-1] local xprint ""
        local term = term_label[`i']
        local c1 = result1[`i']
        local c2 = result2[`i']
        local npair = n_pair[`i']
        file write tex "`xprint' & `term' & `c1' & `c2' & `npair' \\" _n
        if term_order[`i'] == 3 & `i' < _N file write tex "\addlinespace" _n
    }
    file write tex "\hline" _n
    file write tex "\end{tabular}" _n
file write tex "\par\vspace{1.5mm}" _n
file write tex "\begin{minipage}{0.98\linewidth}" _n
file write tex "\footnotesize\setlength{\parindent}{0pt}\setlength{\parskip}{1pt}" _n
file write tex "Notes: Entries are Logit average marginal effects on the 0--1 probability scale; clustered standard errors are in parentheses. N is reported as observations without controls / observations with controls. All models include year and municipality fixed effects and use survey weights. Controls additionally include age, male, and Post interacted with the remaining common census/electoral municipal characteristics. * \(p<0.10\), ** \(p<0.05\), *** \(p<0.01\)." _n
file write tex "\end{minipage}" _n
    file write tex "\end{table}" _n
    file close tex

}

* ---------------------------------------------------------------------------- *
* 21. TABLE A22: pooled-exposure pre-trends
* Source script was not among the uploaded R files; this section reconstructs
* the specification shown in Table A22 (2019 is the omitted year).
* ---------------------------------------------------------------------------- *

use "$data_int/lapop_remaining_mixed_tables_ready.dta", clear

foreach e in 70 80 {
    local sharevar "share_36_78_c`e'"
    foreach y in 2012 2014 2017 2023 {
        gen double es`e'_`y' = (year_num == `y') * `sharevar'
    }
}

tempname pre_ame_h
tempfile pre_ame_long

postfile `pre_ame_h' ///
    byte term_order int year_effect byte model_order ///
    str3 exposure str3 controls double estimate se p_value long nobs ///
    using `pre_ame_long', replace


local model_order = 0
foreach corder in 1 2 {
    local controls "No"
    local rhs_extra ""
    if `corder' == 2 {
        local controls "Yes"
        local rhs_extra "edad hombre"
    }

    foreach e in 70 80 {
        local ++model_order
        local exposure "c`e'"
        local exposure_diag "1970 Census"
        if `e' == 80 local exposure_diag "1980 Census"
        local terms "es`e'_2012 es`e'_2014 es`e'_2017 es`e'_2023"

        quietly logit intencion_migrar ///
            `terms' `rhs_extra' ///
            i.year_num i.mun_code_num ///
            [pw=wt], vce(cluster mun_code_num)

        local N = e(N)
        tempvar sample_logit
        gen byte `sample_logit' = e(sample)

        local t = 0
        foreach y in 2012 2014 2017 2023 {
            local ++t
            local term "es`e'_`y'"
            quietly get_margins_one `term'
            post `pre_ame_h' ///
                (`t') (`y') (`model_order') ("`exposure'") ("`controls'") ///
                (r(ame) * $scale_factor) (r(se) * $scale_factor) (r(p)) (`N')
        }

        quietly get_lpm_bounds `terms' `rhs_extra', ///
            sample(`sample_logit')

        post `remaining_lpm_h' ///
            (9) ("A22") ///
            ("Pooled exposure x survey year; 2019 omitted") ("") ///
            ("`exposure_diag'") ("`controls'") ///
            (r(nobs)) (r(below0_n)) (r(above1_n)) ///
            (r(outside_n)) (r(outside_pct))

        drop `sample_logit'
    }
}

postclose `pre_ame_h'

use `pre_ame_long', clear
save "$output_models/pooled_pretrends_ame_stata.dta", replace
export excel using "`separated_xlsx'", sheet("A22_pretrends_AME") firstrow(variables) sheetreplace


* A22 AME table.
use "$output_models/pooled_pretrends_ame_stata.dta", clear
gen str40 result = string(estimate, "%9.3f") + ///
    cond(p_value < .01, "***", cond(p_value < .05, "**", cond(p_value < .10, "*", ""))) + ///
    " (" + string(se, "%9.3f") + ")"
keep term_order year_effect model_order result nobs
reshape wide result nobs, i(term_order year_effect) j(model_order)
sort term_order

capture file close tex
file open tex using "$output_tex/pooled_pretrends_logit_ame_stata.tex", write replace text
file write tex "\begin{table}[!htbp]" _n
file write tex "\normalsize" _n
file write tex "\renewcommand{\arraystretch}{1.15}" _n
file write tex "\centering" _n
file write tex "\caption{Pre-trends in migration intention: pooled Spanish exposure, Logit average marginal effects}" _n
file write tex "\label{tab:pooled_pretrends_ame_stata}" _n
file write tex "\setlength{\tabcolsep}{4pt}" _n
file write tex "\captionsetup{justification=centering}" _n
file write tex "\begin{tabular}{lcccc}" _n
file write tex "\hline" _n
file write tex " & (1) & (2) & (3) & (4) \\" _n
file write tex "\hline" _n
forvalues i = 1/`=_N' {
    local y = year_effect[`i']
    local c1 = result1[`i']
    local c2 = result2[`i']
    local c3 = result3[`i']
    local c4 = result4[`i']
    file write tex "Spanish share 1936--1978 \(\times\) `y' & `c1' & `c2' & `c3' & `c4' \\" _n
}
file write tex "\hline" _n
local n1 : display %12.0fc nobs1[1]
local n2 : display %12.0fc nobs2[1]
local n3 : display %12.0fc nobs3[1]
local n4 : display %12.0fc nobs4[1]
file write tex "Observations & `n1' & `n2' & `n3' & `n4' \\" _n
file write tex "Municipality FE & Yes & Yes & Yes & Yes \\" _n
file write tex "Year FE & Yes & Yes & Yes & Yes \\" _n
file write tex "Controls & No & No & Yes & Yes \\" _n
file write tex "Spanish share census & 1970 & 1980 & 1970 & 1980 \\" _n
file write tex "\hline" _n
file write tex "\end{tabular}" _n
file write tex "\par\vspace{1.5mm}" _n
file write tex "\begin{minipage}{0.98\linewidth}" _n
file write tex "\footnotesize\setlength{\parindent}{0pt}\setlength{\parskip}{1pt}" _n
file write tex "Notes: Entries are Logit average marginal effects on the 0--1 probability scale, measured relative to 2019. All models include year and municipality fixed effects, survey weights, and municipality-clustered standard errors. Controls include age and male. * \(p<0.10\), ** \(p<0.05\), *** \(p<0.01\)." _n
file write tex "\end{minipage}" _n
file write tex "\end{table}" _n
file close tex

* ---------------------------------------------------------------------------- *
* 22. Exportar el diagnostico LPM adicional en una unica tabla
* Solo especificaciones con controles
* ---------------------------------------------------------------------------- *

postclose `remaining_lpm_h'

use `remaining_lpm_diagnostics', clear
keep if controls == "Yes"
sort table_order characteristic exposure specification

format nobs below0_n above1_n outside_n %12.0fc
format outside_pct %9.2f

label variable table_id       "Table"
label variable specification  "Specification"
label variable characteristic "Municipal characteristic"
label variable exposure       "Exposure"
label variable controls       "Controls"
label variable nobs           "Observations"
label variable below0_n       "Predictions below 0"
label variable above1_n       "Predictions above 1"
label variable outside_n      "Predictions outside [0,1]"
label variable outside_pct    "Outside [0,1] (%)"

save "$output_models/lpm_outside_01_remaining_models_stata.dta", replace

export excel using "`lpm_excel'", ///
    sheet("remaining_models") ///
    firstrow(varlabels) ///
    sheetreplace

noi display as result _newline ///
    "============================================================"
noi display as result ///
    "LPM PREDICTIONS OUTSIDE [0,1]: CONTROLLED ADDITIONAL MODELS"
noi display as result ///
    "============================================================"
noi list table_id specification characteristic exposure ///
    nobs below0_n above1_n outside_n outside_pct, ///
    sepby(table_id) noobs abbreviate(30)

* One multipage LaTeX table for controlled A10, A14, A18-A21, and A22 models.
capture file close tex
file open tex using ///
    "$output_tex/lpm_outside_01_remaining_models_stata.tex", ///
    write replace text

file write tex "\begin{longtable}{p{0.10\textwidth}p{0.31\textwidth}p{0.26\textwidth}p{0.14\textwidth}r}" _n
file write tex "\caption{Linear probability model: predictions outside [0,1] in controlled additional specifications}\label{tab:lpm_outside_remaining}\\" _n
file write tex "\hline" _n
file write tex "Table & Specification & Municipal characteristic & Exposure & Outside [0,1] (\%) \\" _n
file write tex "\hline" _n
file write tex "\endfirsthead" _n
file write tex "\multicolumn{5}{c}{\textit{Table continued}} \\" _n
file write tex "\hline" _n
file write tex "Table & Specification & Municipal characteristic & Exposure & Outside [0,1] (\%) \\" _n
file write tex "\hline" _n
file write tex "\endhead" _n

forvalues i = 1/`=_N' {
    local tab  = table_id[`i']
    local spec = specification[`i']
    local xlab = characteristic[`i']
    local exp  = exposure[`i']
    local pct  : display %5.2f outside_pct[`i']

    if `i' > 1 & table_order[`i'] != table_order[`i'-1] {
        file write tex "\addlinespace" _n
    }

    file write tex "`tab' & `spec' & `xlab' & `exp' & `pct'\% \\" _n
}

file write tex "\hline" _n
file write tex "\end{longtable}" _n
file write tex "\begin{minipage}{0.98\textwidth}" _n
file write tex "\footnotesize Notes: Entries report the unweighted percentage of observations whose fitted value from the linear probability model is strictly below 0 or above 1. Only controlled specifications are reported. Each LPM uses exactly the estimation sample, regressors, survey weights, year fixed effects, and municipality fixed effects of the corresponding Logit model. A10 contains the three multivariate individual-characteristic specifications. A14 reports one diagnostic for each municipal characteristic. A18 uses the pooled Spanish-exposure measure from the 1970 and 1980 censuses. A19--A21 use the pooled triple-difference specifications, and A22 reports the pooled pre-trend models with 2019 as the omitted year." _n
file write tex "\end{minipage}" _n
file close tex

* ---------------------------------------------------------------------------- *
* 23. Final message
* ---------------------------------------------------------------------------- *

display as result _newline "Additional Logit AME tables and LPM diagnostics completed."
display as result "Logit AME workbook: `separated_xlsx'"
display as result "LPM diagnostic workbook: `lpm_excel'"
display as result "LPM diagnostic table:"
display as result "  $output_tex/lpm_outside_01_remaining_models_stata.tex"
display as result "Logit AME LaTeX files created in: $output_tex"
display as result "  individual_characteristics_logit_ame_stata.tex"
display as result "  triple_all_interactions_logit_ame_stata.tex"
display as result "  post_pooled_share_logit_ame_stata.tex"
display as result "  triple_pooled_share_logit_ame_stata.tex"
display as result "  triple_pooled_all_interactions_c70_logit_ame_stata.tex"
display as result "  triple_pooled_all_interactions_c80_logit_ame_stata.tex"
display as result "  pooled_pretrends_logit_ame_stata.tex"

* ============================================================================ *
* PORCENTAJE DE OBSERVACIONES CON MISSING — TABLA A10
* ============================================================================ *

use "$data_int/lapop_remaining_mixed_tables_ready.dta", clear

quietly count
local N_total = r(N)

local ind_vars_1 ///
    "edad hombre desempleado en_pareja secundaria_completa_o_mas izq_der interes_pol_mucho voto_blanco_nulo"

local ind_vars_2 ///
    "edad hombre desempleado en_pareja secundaria_completa_o_mas izq_der interes_pol_mucho"

local ind_vars_3 ///
    "edad hombre desempleado en_pareja secundaria_completa_o_mas interes_pol_mucho voto_blanco_nulo"

local ind_label_1 "Full"
local ind_label_2 "No blank/null vote"
local ind_label_3 "No ideology"

display as text _newline "============================================================"
display as text "MISSING VALUES — INDIVIDUAL CHARACTERISTICS TABLE"
display as text "Total observations in original data: " ///
    as result %12.0fc `N_total'
display as text "============================================================"

forvalues s = 1/3 {

    local vars "`ind_vars_`s''"

    egen byte nmiss_spec = rowmiss( ///
        intencion_migrar ///
        `vars' ///
        year_num ///
        mun_code_num ///
        wt ///
    )

    quietly count if nmiss_spec > 0
    local N_missing = r(N)

    quietly count if nmiss_spec == 0
    local N_complete = r(N)

    local pct_missing  = 100 * `N_missing' / `N_total'
    local pct_complete = 100 * `N_complete' / `N_total'

    display as text _newline "`ind_label_`s'':"
    display as text "  Observations with at least one missing value: " ///
        as result %12.0fc `N_missing' ///
        as text " (" ///
        as result %6.2f `pct_missing' ///
        as text "%)"

    display as text "  Complete observations: " ///
        as result %12.0fc `N_complete' ///
        as text " (" ///
        as result %6.2f `pct_complete' ///
        as text "%)"

    drop nmiss_spec
}

display as text _newline "============================================================"

* Esta seccion es solo para chequear cantidad de clusters
* ============================================================================ *
* APPEND AT THE END OF THE MAIN DO-FILE
* Number of municipality clusters in each distinct Logit specification
* The corresponding LPM uses the same sample and clustering variable, so it has
* the same number of clusters.
* ============================================================================ *

use "$data_int/lapop_remaining_mixed_tables_ready.dta", clear

* Re-declare lists so this block also works if saved as a separate do-file
* and called from the end of the main script.
local common_x_vars ///
    popdensgeo2_2010 ///
    share_female_2010 ///
    mean_yrschool_2010 ///
    median_age_2010 ///
    share_laborforce_2010 ///
    share_unemployed_2010 ///
    share_izq_amplia_pre_avg ///
    share_alt_pre_avg

local tercile_vars ///
    t_density_2010 ///
    t_fem_2010 ///
    t_mean_schyr_2010 ///
    t_med_dage_2010 ///
    t_pea_2010 ///
    t_unemp_2010 ///
    t_izam_pre_avg ///
    t_alt_pre_avg

local source_vars ///
    popdensgeo2_2010 ///
    share_female_2010 ///
    mean_yrschool_2010 ///
    median_age_2010 ///
    share_laborforce_2010 ///
    share_unemployed_2010 ///
    share_izq_amplia_pre_avg ///
    share_alt_pre_avg

* Interactions needed by the pre-trend specifications.
foreach e in 70 80 {
    local sharevar "share_36_78_c`e'"
    foreach y in 2012 2014 2017 2023 {
        capture drop es`e'_`y'
        gen double es`e'_`y' = (year_num == `y') * `sharevar'
    }
}

* Helper: count distinct municipalities in the active estimation sample.
capture program drop get_cluster_count
program define get_cluster_count, rclass
    version 17
    tempvar cluster_tag
    quietly egen byte `cluster_tag' = tag(mun_code_num) if e(sample)
    quietly count if `cluster_tag' == 1
    return scalar n_clusters = r(N)
end

tempname cluster_handle
tempfile cluster_counts

postfile `cluster_handle' ///
    str45 table_name ///
    str80 specification ///
    str70 characteristic ///
    str12 exposure ///
    str12 controls ///
    int tercile ///
    long observations ///
    long clusters ///
    using `cluster_counts', replace

* ---------------------------------------------------------------------------- *
* A. Post x the two historical shares
* ---------------------------------------------------------------------------- *
foreach ctrl_label in "No" "Yes" {
    local ctrls ""
    if "`ctrl_label'" == "Yes" local ctrls "edad hombre"

    quietly logit intencion_migrar ///
        pshare36 pshare56 `ctrls' ///
        i.year_num i.mun_code_num ///
        [pw=wt], vce(cluster mun_code_num)

    local N = e(N)
    quietly get_cluster_count
    local G = r(n_clusters)

    post `cluster_handle' ///
        ("logit_post_share_mfx") ///
        ("Post x shares 1936-1955 and 1956-1978") ///
        ("") ("two windows") ("`ctrl_label'") (.) (`N') (`G')
}

* ---------------------------------------------------------------------------- *
* B. Post x municipal characteristic
* ---------------------------------------------------------------------------- *
foreach x of local common_x_vars {
    local xlabel "`x'"
    if "`x'" == "popdensgeo2_2010"         local xlabel "Population density"
    if "`x'" == "share_female_2010"        local xlabel "Female population share"
    if "`x'" == "mean_yrschool_2010"       local xlabel "Mean years of education"
    if "`x'" == "median_age_2010"           local xlabel "Median age"
    if "`x'" == "share_laborforce_2010"    local xlabel "Share in labor force"
    if "`x'" == "share_unemployed_2010"    local xlabel "Share unemployed"
    if "`x'" == "share_izq_amplia_pre_avg" local xlabel "Left vote share"
    if "`x'" == "share_alt_pre_avg"         local xlabel "Ideological alternation"

    foreach ctrl_label in "No" "Yes" {
        local ctrls ""
        if "`ctrl_label'" == "Yes" local ctrls "edad hombre"

        quietly logit intencion_migrar ///
            px_`x' `ctrls' ///
            i.year_num i.mun_code_num ///
            [pw=wt], vce(cluster mun_code_num)

        local N = e(N)
        quietly get_cluster_count
        local G = r(n_clusters)

        post `cluster_handle' ///
            ("logit_post_x_mfx") ///
            ("Post x municipal characteristic") ///
            ("`xlabel'") ("") ("`ctrl_label'") (.) (`N') (`G')
    }
}

* ---------------------------------------------------------------------------- *
* C. Triple-difference models; these are also the models underlying A14
* ---------------------------------------------------------------------------- *
foreach x of local common_x_vars {
    local xlabel "`x'"
    if "`x'" == "popdensgeo2_2010"         local xlabel "Population density"
    if "`x'" == "share_female_2010"        local xlabel "Female population share"
    if "`x'" == "mean_yrschool_2010"       local xlabel "Mean years of education"
    if "`x'" == "median_age_2010"           local xlabel "Median age"
    if "`x'" == "share_laborforce_2010"    local xlabel "Share in labor force"
    if "`x'" == "share_unemployed_2010"    local xlabel "Share unemployed"
    if "`x'" == "share_izq_amplia_pre_avg" local xlabel "Left vote share"
    if "`x'" == "share_alt_pre_avg"         local xlabel "Ideological alternation"

    foreach ctrl_label in "No" "Yes" {
        local rhs_extra ""
        if "`ctrl_label'" == "Yes" {
            local rhs_extra "edad hombre"
            foreach z of local common_x_vars {
                if "`z'" != "`x'" local rhs_extra "`rhs_extra' px_`z'"
            }
        }

        quietly logit intencion_migrar ///
            pshare36 pshare56 px_`x' pxs36_`x' pxs56_`x' ///
            `rhs_extra' ///
            i.year_num i.mun_code_num ///
            [pw=wt], vce(cluster mun_code_num)

        local N = e(N)
        quietly get_cluster_count
        local G = r(n_clusters)

        post `cluster_handle' ///
            ("logit_triple_mfx / A14") ///
            ("Two-window triple difference") ///
            ("`xlabel'") ("two windows") ("`ctrl_label'") (.) (`N') (`G')
    }
}

* ---------------------------------------------------------------------------- *
* D. Heterogeneity by municipal terciles
* ---------------------------------------------------------------------------- *
local i = 1
foreach tv of local tercile_vars {
    local sv : word `i' of `source_vars'
    local ++i

    local xlabel "`tv'"
    if "`tv'" == "t_density_2010"    local xlabel "Population density"
    if "`tv'" == "t_fem_2010"        local xlabel "Female population share"
    if "`tv'" == "t_mean_schyr_2010" local xlabel "Mean years of education"
    if "`tv'" == "t_med_dage_2010"   local xlabel "Median age"
    if "`tv'" == "t_pea_2010"        local xlabel "Share in labor force"
    if "`tv'" == "t_unemp_2010"      local xlabel "Share unemployed"
    if "`tv'" == "t_izam_pre_avg"    local xlabel "Left vote share"
    if "`tv'" == "t_alt_pre_avg"     local xlabel "Ideological alternation"

    forvalues t = 1/3 {
        quietly logit intencion_migrar ///
            pshare36 pshare56 edad hombre ///
            i.year_num i.mun_code_num ///
            if `tv' == `t' [pw=wt], ///
            vce(cluster mun_code_num)

        local N = e(N)
        quietly get_cluster_count
        local G = r(n_clusters)

        post `cluster_handle' ///
            ("migration_subsamples") ///
            ("Heterogeneity by municipal tercile") ///
            ("`xlabel'") ("two windows") ("Yes") (`t') (`N') (`G')
    }
}

* ---------------------------------------------------------------------------- *
* E. A10: individual-characteristic specifications
* ---------------------------------------------------------------------------- *
local ind_vars_1 ///
    "edad hombre desempleado en_pareja secundaria_completa_o_mas izq_der interes_pol_mucho voto_blanco_nulo"
local ind_vars_2 ///
    "edad hombre desempleado en_pareja secundaria_completa_o_mas izq_der interes_pol_mucho"
local ind_vars_3 ///
    "edad hombre desempleado en_pareja secundaria_completa_o_mas interes_pol_mucho voto_blanco_nulo"

local ind_label_1 "Full"
local ind_label_2 "No blank/null vote"
local ind_label_3 "No ideology"

forvalues s = 1/3 {
    local vars "`ind_vars_`s''"
    local slab "`ind_label_`s''"

    quietly logit intencion_migrar ///
        `vars' ///
        i.year_num i.mun_code_num ///
        [pw=wt], vce(cluster mun_code_num)

    local N = e(N)
    quietly get_cluster_count
    local G = r(n_clusters)

    post `cluster_handle' ///
        ("A10 individual characteristics") ///
        ("`slab'") ("") ("") ("n/a") (.) (`N') (`G')
}

* ---------------------------------------------------------------------------- *
* F. A18: Post x pooled Spanish exposure
* ---------------------------------------------------------------------------- *
foreach e in 70 80 {
    local pterm "pp`e'"
    local exposure "1970 Census"
    if `e' == 80 local exposure "1980 Census"

    foreach ctrl_label in "No" "Yes" {
        local rhs_extra ""
        if "`ctrl_label'" == "Yes" local rhs_extra "edad hombre"

        quietly logit intencion_migrar ///
            `pterm' `rhs_extra' ///
            i.year_num i.mun_code_num ///
            [pw=wt], vce(cluster mun_code_num)

        local N = e(N)
        quietly get_cluster_count
        local G = r(n_clusters)

        post `cluster_handle' ///
            ("A18 pooled post") ///
            ("Post x pooled Spanish exposure") ///
            ("") ("`exposure'") ("`ctrl_label'") (.) (`N') (`G')
    }
}

* ---------------------------------------------------------------------------- *
* G. A19-A21: pooled triple-difference models
* ---------------------------------------------------------------------------- *
foreach e in 70 80 {
    local pterm "pp`e'"
    local exposure "1970 Census"
    if `e' == 80 local exposure "1980 Census"

    foreach x of local common_x_vars {
        local xlabel "`x'"
        if "`x'" == "popdensgeo2_2010"         local xlabel "Population density"
        if "`x'" == "share_female_2010"        local xlabel "Female population share"
        if "`x'" == "mean_yrschool_2010"       local xlabel "Mean years of education"
        if "`x'" == "median_age_2010"           local xlabel "Median age"
        if "`x'" == "share_laborforce_2010"    local xlabel "Share in labor force"
        if "`x'" == "share_unemployed_2010"    local xlabel "Share unemployed"
        if "`x'" == "share_izq_amplia_pre_avg" local xlabel "Left vote share"
        if "`x'" == "share_alt_pre_avg"         local xlabel "Ideological alternation"

        local triplevar "ppx`e'_`x'"

        foreach ctrl_label in "No" "Yes" {
            local rhs_extra ""
            if "`ctrl_label'" == "Yes" {
                local rhs_extra "edad hombre"
                foreach z of local common_x_vars {
                    if "`z'" != "`x'" local rhs_extra "`rhs_extra' px_`z'"
                }
            }

            quietly logit intencion_migrar ///
                `pterm' px_`x' `triplevar' ///
                `rhs_extra' ///
                i.year_num i.mun_code_num ///
                [pw=wt], vce(cluster mun_code_num)

            local N = e(N)
            quietly get_cluster_count
            local G = r(n_clusters)

            post `cluster_handle' ///
                ("A19-A21 pooled triple") ///
                ("Pooled triple difference") ///
                ("`xlabel'") ("`exposure'") ("`ctrl_label'") (.) (`N') (`G')
        }
    }
}

* ---------------------------------------------------------------------------- *
* H. A22: pooled-exposure pre-trends
* ---------------------------------------------------------------------------- *
foreach ctrl_label in "No" "Yes" {
    local rhs_extra ""
    if "`ctrl_label'" == "Yes" local rhs_extra "edad hombre"

    foreach e in 70 80 {
        local exposure "1970 Census"
    if `e' == 80 local exposure "1980 Census"
        local terms "es`e'_2012 es`e'_2014 es`e'_2017 es`e'_2023"

        quietly logit intencion_migrar ///
            `terms' `rhs_extra' ///
            i.year_num i.mun_code_num ///
            [pw=wt], vce(cluster mun_code_num)

        local N = e(N)
        quietly get_cluster_count
        local G = r(n_clusters)

        post `cluster_handle' ///
            ("A22 pooled pre-trends") ///
            ("Pooled exposure x survey year; 2019 omitted") ///
            ("") ("`exposure'") ("`ctrl_label'") (.) (`N') (`G')
    }
}

postclose `cluster_handle'

use `cluster_counts', clear
sort table_name characteristic exposure controls tercile

format observations clusters %12.0fc

noi display as result _newline ///
    "============================================================"
noi display as result ///
    "NUMBER OF MUNICIPALITY CLUSTERS BY ESTIMATION"
noi display as result ///
    "============================================================"

noi list table_name specification characteristic exposure controls tercile ///
    observations clusters, ///
    sepby(table_name) noobs abbreviate(32)

noi display as result ///
    "============================================================"
noi display as text ///
    "The corresponding LPM has the same number of clusters because it uses the Logit estimation sample."
noi display as result ///
    "============================================================"

* ============================================================================ *


* ============================================================================ *
* 24. REEXPORTAR TABLAS LOGIT AME CON EL FORMATO FINAL DE OVERLEAF
*
* Este bloque no reestima modelos ni modifica los resultados guardados.
* Sobrescribe solamente los archivos .tex para:
*   - reportar los errores estandar en el renglon inferior;
*   - reproducir las notas y decisiones de formato del documento;
*   - eliminar las columnas N cuando N ya se informa en las notas;
*   - usar Spanish share x Post, en vez de Post x Spanish share;
*   - agregar filas de especificacion al pie de la tabla individual.
* ============================================================================ *

* Numero de clusters municipales utilizado en las notas.
use "$data_int/lapop_logit_mfx_ready_LPM_appendix.dta", clear
capture drop __cluster_tag
egen byte __cluster_tag = tag(mun_code_num) if !missing(mun_code_num)
quietly count if __cluster_tag == 1
local ame_clusters = r(N)
drop __cluster_tag

* ---------------------------------------------------------------------------- *
* 24.1 Spanish historical exposure x Post
* ---------------------------------------------------------------------------- *
use "$output_models/logit_post_share_mfx_long_LPM_appendix.dta", clear

quietly summarize nobs if controls == "No", meanonly
local post_share_n_no : display %12.0fc r(mean)
quietly summarize nobs if controls == "Yes", meanonly
local post_share_n_yes : display %12.0fc r(mean)

gen str20 b_fmt = string(ame_reported, "%9.3f") + ///
    cond(p_value < .01, "***", cond(p_value < .05, "**", cond(p_value < .10, "*", "")))
gen str20 se_fmt = "(" + string(se_reported, "%9.3f") + ")"

gen byte term_order = .
replace term_order = 1 if term_id == "pshare36"
replace term_order = 2 if term_id == "pshare56"
gen byte control_order = cond(controls == "No", 1, 2)
sort term_order control_order

capture file close tex
file open tex using "$output_tex/logit_post_share_mfx_glm_cluster_se_stata.tex", write replace text
file write tex "\begin{table}[!htbp]" _n
file write tex "\normalsize" _n
file write tex "\renewcommand{\arraystretch}{1.25}" _n
file write tex "\setlength{\tabcolsep}{6pt}" _n
file write tex "\centering" _n
file write tex "\caption{Logit average marginal effects: post-period Spanish historical exposure}" _n
file write tex "\label{tab:logit_post_share_mfx_stata}" _n
file write tex "\begin{tabular}{llc}" _n
file write tex "\hline" _n
file write tex "Term & Controls & Logit AME \\" _n
file write tex "\hline" _n

forvalues i = 1/`=_N' {
    local lab "Spanish share 1936--1955 \(\times\) Post"
    if term_id[`i'] == "pshare56" local lab "Spanish share 1956--1978 \(\times\) Post"
    local ctrl = controls[`i']
    local b = b_fmt[`i']
    local s = se_fmt[`i']

    file write tex "`lab' & `ctrl' & `b' \\" _n
    file write tex " & & `s' \\" _n

    if control_order[`i'] == 2 & `i' < _N file write tex "\addlinespace" _n
}

file write tex "\hline" _n
file write tex "\end{tabular}" _n
file write tex "\par\vspace{1.5mm}" _n
file write tex "\begin{minipage}{\textwidth}" _n
file write tex "{\footnotesize" _n
file write tex "Notes: The dependent variable is migration intention. Entries are Logit average marginal effects on the 0--1 probability scale. All models include year and municipality fixed effects. Controls include age and male. Standard errors are clustered at the municipality level and reported in parentheses (`ame_clusters' clusters). The number of observations is `post_share_n_no' in models without controls and `post_share_n_yes' in models with controls. * \(p<0.10\), ** \(p<0.05\), *** \(p<0.01\)." _n
file write tex "}" _n
file write tex "\end{minipage}" _n
file write tex "\end{table}" _n
file close tex

* ---------------------------------------------------------------------------- *
* 24.2 Post x municipal characteristics
* ---------------------------------------------------------------------------- *
use "$output_models/logit_post_x_mfx_long_LPM_appendix.dta", clear

quietly summarize nobs if controls == "No", meanonly
local post_x_n_no : display %12.0fc r(mean)
quietly summarize nobs if controls == "Yes", meanonly
local post_x_n_yes : display %12.0fc r(mean)

gen str20 b_fmt = string(ame_reported, "%9.3f") + ///
    cond(p_value < .01, "***", cond(p_value < .05, "**", cond(p_value < .10, "*", "")))
gen str20 se_fmt = "(" + string(se_reported, "%9.3f") + ")"

gen byte row_order = .
replace row_order = 1 if variable == "popdensgeo2_2010"
replace row_order = 2 if variable == "share_female_2010"
replace row_order = 3 if variable == "mean_yrschool_2010"
replace row_order = 4 if variable == "median_age_2010"
replace row_order = 5 if variable == "share_laborforce_2010"
replace row_order = 6 if variable == "share_unemployed_2010"
replace row_order = 7 if variable == "share_izq_amplia_pre_avg"
replace row_order = 8 if variable == "share_alt_pre_avg"
gen byte control_order = cond(controls == "No", 1, 2)
sort row_order control_order

capture file close tex
file open tex using "$output_tex/logit_post_x_mfx_glm_cluster_se_stata.tex", write replace text
file write tex "\begin{table}[!htbp]" _n
file write tex "\centering" _n
file write tex "\renewcommand{\arraystretch}{1.25}" _n
file write tex "\setlength{\tabcolsep}{6pt}" _n
file write tex "\caption{Post-period changes by municipal characteristics}" _n
file write tex "\label{tab:logit_post_x_mfx_stata}" _n
file write tex "\small" _n
file write tex "\begin{tabular}{llc}" _n
file write tex "\hline" _n
file write tex "Municipal characteristic & Controls & Logit AME: \(Post \times X\) \\" _n
file write tex "\hline" _n

forvalues i = 1/`=_N' {
    local lab = term_label[`i']
    local ctrl = controls[`i']
    local b = b_fmt[`i']
    local s = se_fmt[`i']

    file write tex "`lab' & `ctrl' & `b' \\" _n
    file write tex " & & `s' \\" _n

    if control_order[`i'] == 2 & `i' < _N file write tex "\addlinespace" _n
}

file write tex "\hline" _n
file write tex "\end{tabular}" _n
file write tex "\par\vspace{1.5mm}" _n
file write tex "\begin{minipage}{\textwidth}" _n
file write tex "{\footnotesize Notes: The dependent variable is migration intention. Each pair of rows reports separate Logit models interacting \(Post\) with the municipal characteristic indicated in the first column. Entries are Logit average marginal effects on the 0--1 probability scale. Standard errors are clustered at the municipality level and reported in parentheses (`ame_clusters' clusters). All models include year and municipality fixed effects. In rows with Controls = Yes, controls include age and male. The number of observations is `post_x_n_no' in models without controls and `post_x_n_yes' in models with controls. * \(p<0.10\), ** \(p<0.05\), *** \(p<0.01\).}" _n
file write tex "\end{minipage}" _n
file write tex "\end{table}" _n
file close tex

* ---------------------------------------------------------------------------- *
* 24.3 Triple differences: key terms from the two historical windows
* ---------------------------------------------------------------------------- *
use "$output_models/logit_triple_mfx_long_LPM_appendix.dta", clear

quietly summarize nobs if controls == "No", meanonly
local triple_n_no : display %12.0fc r(mean)
quietly summarize nobs if controls == "Yes", meanonly
local triple_n_yes : display %12.0fc r(mean)

gen str3 window = "36" if share_group == "share_1936_1955"
replace window = "56" if share_group == "share_1956_1978"
keep variable variable_label controls window ame_reported se_reported p_value nobs
reshape wide ame_reported se_reported p_value, ///
    i(variable variable_label controls nobs) j(window) string

gen str20 b36 = string(ame_reported36, "%9.3f") + ///
    cond(p_value36 < .01, "***", cond(p_value36 < .05, "**", cond(p_value36 < .10, "*", "")))
gen str20 s36 = "(" + string(se_reported36, "%9.3f") + ")"
gen str20 b56 = string(ame_reported56, "%9.3f") + ///
    cond(p_value56 < .01, "***", cond(p_value56 < .05, "**", cond(p_value56 < .10, "*", "")))
gen str20 s56 = "(" + string(se_reported56, "%9.3f") + ")"

gen byte row_order = .
replace row_order = 1 if variable == "popdensgeo2_2010"
replace row_order = 2 if variable == "share_female_2010"
replace row_order = 3 if variable == "mean_yrschool_2010"
replace row_order = 4 if variable == "median_age_2010"
replace row_order = 5 if variable == "share_laborforce_2010"
replace row_order = 6 if variable == "share_unemployed_2010"
replace row_order = 7 if variable == "share_izq_amplia_pre_avg"
replace row_order = 8 if variable == "share_alt_pre_avg"
gen byte control_order = cond(controls == "No", 1, 2)
sort row_order control_order

capture file close tex
file open tex using "$output_tex/logit_triple_mfx_glm_cluster_se_stata.tex", write replace text
file write tex "\begin{table}[!htbp]" _n
file write tex "\centering" _n
file write tex "\renewcommand{\arraystretch}{1.25}" _n
file write tex "\setlength{\tabcolsep}{6pt}" _n
file write tex "\caption{Triple differences by municipal characteristics}" _n
file write tex "\label{tab:logit_triple_mfx_stata}" _n
file write tex "\small" _n
file write tex "\begin{tabular}{llcc}" _n
file write tex "\hline" _n
file write tex " & & \multicolumn{2}{c}{Spanish share \(\times\) Post \(\times X\): Logit average marginal effects} \\" _n
file write tex "Municipal characteristic & Controls & 1936--1955 & 1956--1978 \\" _n
file write tex "\hline" _n

forvalues i = 1/`=_N' {
    local lab = variable_label[`i']
    local ctrl = controls[`i']
    local c36 = b36[`i']
    local e36 = s36[`i']
    local c56 = b56[`i']
    local e56 = s56[`i']

    file write tex "`lab' & `ctrl' & `c36' & `c56' \\" _n
    file write tex " & & `e36' & `e56' \\" _n

    if control_order[`i'] == 2 & `i' < _N file write tex "\addlinespace" _n
}

file write tex "\hline" _n
file write tex "\end{tabular}" _n
file write tex "\par\vspace{1.5mm}" _n
file write tex "\begin{minipage}{\textwidth}" _n
file write tex "\footnotesize Notes: The dependent variable is migration intention. Each pair of rows reports separate triple-difference Logit specifications for the municipal characteristic indicated in the first column. Entries report average marginal effects on the 0--1 probability scale for \(Spanish\ share_{1936-1955} \times Post \times X\) and \(Spanish\ share_{1956-1978} \times Post \times X\). All models include year and municipality fixed effects. Standard errors are clustered at the municipality level and reported in parentheses (`ame_clusters' clusters). Specifications without controls include the two Spanish-share-by-Post terms, \(Post \times X\), and the two corresponding triple interactions. Specifications with controls additionally include age, male, and \(Post\) interacted with the remaining municipal characteristics. The number of observations is `triple_n_no' in models without controls and `triple_n_yes' in models with controls. * \(p<0.10\), ** \(p<0.05\), *** \(p<0.01\)." _n
file write tex "\end{minipage}" _n
file write tex "\end{table}" _n
file close tex

* ---------------------------------------------------------------------------- *
* 24.4 Individual characteristics
* ---------------------------------------------------------------------------- *
use "$data_int/lapop_remaining_mixed_tables_ready.dta", clear
quietly count
local individual_n_total = r(N)

local fmt_ind_vars_1 "edad hombre desempleado en_pareja secundaria_completa_o_mas izq_der interes_pol_mucho voto_blanco_nulo"
local fmt_ind_vars_2 "edad hombre desempleado en_pareja secundaria_completa_o_mas izq_der interes_pol_mucho"
local fmt_ind_vars_3 "edad hombre desempleado en_pareja secundaria_completa_o_mas interes_pol_mucho voto_blanco_nulo"

forvalues s = 1/3 {
    capture drop __miss_spec
    egen byte __miss_spec = rowmiss(intencion_migrar `fmt_ind_vars_`s'' year_num mun_code_num wt)
    quietly count if __miss_spec > 0
    local individual_missing_`s' = 100 * r(N) / `individual_n_total'
    local individual_missing_fmt_`s' : display %5.2f `individual_missing_`s''
    drop __miss_spec
}

use "$output_models/individual_characteristics_ame_stata.dta", clear

forvalues s = 1/3 {
    quietly summarize nobs if spec_order == `s', meanonly
    local individual_n_`s' : display %12.0fc r(mean)
}

keep var_order variable variable_label spec_order estimate se p_value
reshape wide estimate se p_value, i(var_order variable variable_label) j(spec_order)
sort var_order

forvalues s = 1/3 {
    gen str20 b`s' = cond(missing(estimate`s'), "", ///
        string(estimate`s', "%9.3f") + ///
        cond(p_value`s' < .01, "***", cond(p_value`s' < .05, "**", cond(p_value`s' < .10, "*", ""))))
    gen str20 e`s' = cond(missing(se`s'), "", "(" + string(se`s', "%9.3f") + ")")
}

capture file close tex
file open tex using "$output_tex/individual_characteristics_logit_ame_stata.tex", write replace text
file write tex "\begin{table}[htbp]" _n
file write tex "\normalsize" _n
file write tex "\renewcommand{\arraystretch}{1.25}" _n
file write tex "\setlength{\tabcolsep}{6pt}" _n
file write tex "\centering" _n
file write tex "\caption{Individual characteristics and migration intention: Logit average marginal effects}" _n
file write tex "\label{tab:individual_characteristics_ame_stata}" _n
file write tex "\setlength{\tabcolsep}{4pt}" _n
file write tex "\captionsetup{justification=centering}" _n
file write tex "\begin{tabular}{lccc}" _n
file write tex "\hline" _n
file write tex " & (1) & (2) & (3) \\" _n
file write tex "\hline" _n

forvalues i = 1/`=_N' {
    local lab = variable_label[`i']
    local c1 = b1[`i']
    local c2 = b2[`i']
    local c3 = b3[`i']
    local s1 = e1[`i']
    local s2 = e2[`i']
    local s3 = e3[`i']

    file write tex "`lab' & `c1' & `c2' & `c3' \\" _n
    file write tex " & `s1' & `s2' & `s3' \\" _n
}

file write tex "\hline" _n
file write tex "Observations & `individual_n_1' & `individual_n_2' & `individual_n_3' \\" _n
file write tex "Year FE & Yes & Yes & Yes \\" _n
file write tex "Municipality FE & Yes & Yes & Yes \\" _n
file write tex "Survey weights & Yes & Yes & Yes \\" _n
file write tex "Blank/null vote included & Yes & No & Yes \\" _n
file write tex "Ideology included & Yes & Yes & No \\" _n
file write tex "\hline" _n
file write tex "\end{tabular}" _n
file write tex "\par\vspace{1.5mm}" _n
file write tex "\begin{minipage}{0.98\linewidth}" _n
file write tex "\footnotesize\setlength{\parindent}{0pt}\setlength{\parskip}{1pt}" _n
file write tex "Notes: The dependent variable is migration intention. Entries are Logit average marginal effects on the 0--1 probability scale. Standard errors are clustered at the municipality level and reported in parentheses (`ame_clusters' clusters). All models include year and municipality fixed effects. Missing values account for `individual_missing_fmt_1'\%, `individual_missing_fmt_2'\%, and `individual_missing_fmt_3'\% of the original sample in the full, no blank/null vote, and no ideology specifications, respectively. Unlike the other tables, this table relies exclusively on LAPOP data, as its individual-level structure allows us to examine the relationship between respondents' personal characteristics and their intention to migrate. * \(p<0.10\), ** \(p<0.05\), *** \(p<0.01\)." _n
file write tex "\end{minipage}" _n
file write tex "\end{table}" _n
file close tex

* ---------------------------------------------------------------------------- *
* 24.5 Two-window triple differences: all interaction terms
* ---------------------------------------------------------------------------- *
use "$output_models/triple_all_interactions_ame_stata.dta", clear

quietly summarize nobs if controls == "No", meanonly
local all_terms_n_no : display %12.0fc r(mean)
quietly summarize nobs if controls == "Yes", meanonly
local all_terms_n_yes : display %12.0fc r(mean)

keep x_order x x_label term_order control_order estimate se p_value nobs
reshape wide estimate se p_value nobs, ///
    i(x_order x x_label term_order) j(control_order)
sort x_order term_order

gen str20 b1 = string(estimate1, "%9.3f") + ///
    cond(p_value1 < .01, "***", cond(p_value1 < .05, "**", cond(p_value1 < .10, "*", "")))
gen str20 e1 = "(" + string(se1, "%9.3f") + ")"
gen str20 b2 = string(estimate2, "%9.3f") + ///
    cond(p_value2 < .01, "***", cond(p_value2 < .05, "**", cond(p_value2 < .10, "*", "")))
gen str20 e2 = "(" + string(se2, "%9.3f") + ")"

capture file close tex
file open tex using "$output_tex/triple_all_interactions_logit_ame_stata.tex", write replace text
file write tex "\begin{table}[!htbp]" _n
file write tex "\normalsize" _n
file write tex "\renewcommand{\arraystretch}{1.15}" _n
file write tex "\setlength{\tabcolsep}{6pt}" _n
file write tex "\centering" _n
file write tex "\caption{Triple-difference models: all interaction terms, Logit average marginal effects}" _n
file write tex "\label{tab:triple_all_interactions_ame_stata}" _n
file write tex "\setlength{\tabcolsep}{4pt}" _n
file write tex "\captionsetup{justification=centering}" _n
file write tex "\begin{tabular}{llcc}" _n
file write tex "\hline" _n
file write tex "Municipal characteristic & Term & No controls & Controls \\" _n
file write tex "\hline" _n

forvalues i = 1/`=_N' {
    local xprint = x_label[`i']
    if `i' > 1 & x_order[`i'] == x_order[`i'-1] local xprint ""

    local term "Spanish share 1936--1955 \(\times\) Post"
    if term_order[`i'] == 2 local term "Spanish share 1956--1978 \(\times\) Post"
    if term_order[`i'] == 3 local term "\(Post \times X\)"
    if term_order[`i'] == 4 local term "Spanish share 1936--1955 \(\times\) Post \(\times X\)"
    if term_order[`i'] == 5 local term "Spanish share 1956--1978 \(\times\) Post \(\times X\)"

    local c1 = b1[`i']
    local c2 = b2[`i']
    local s1 = e1[`i']
    local s2 = e2[`i']

    file write tex "`xprint' & `term' & `c1' & `c2' \\" _n
    file write tex " & & `s1' & `s2' \\" _n

    if term_order[`i'] == 5 & `i' < _N file write tex "\addlinespace" _n
}

file write tex "\hline" _n
file write tex "\end{tabular}" _n
file write tex "\par\vspace{1.5mm}" _n
file write tex "\begin{minipage}{0.98\linewidth}" _n
file write tex "\footnotesize\setlength{\parindent}{0pt}\setlength{\parskip}{1pt}" _n
file write tex "Notes: The dependent variable is migration intention. Each block reports a separate triple-difference specification for municipal characteristic \(X\). Entries are Logit average marginal effects on the 0--1 probability scale. Standard errors are clustered at the municipality level and reported in parentheses (`ame_clusters' clusters). All models include year and municipality fixed effects. Controls additionally include age, male, and \(Post\) interacted with the remaining pre-2022 municipal characteristics. Lower-order interaction terms are conditional on the other interacted variables being zero. The number of observations is `all_terms_n_no' in models without controls and `all_terms_n_yes' in models with controls. * \(p<0.10\), ** \(p<0.05\), *** \(p<0.01\)." _n
file write tex "\end{minipage}" _n
file write tex "\end{table}" _n
file close tex

* ---------------------------------------------------------------------------- *
* ---------------------------------------------------------------------------- *
* 24.6 Pooled Spanish exposure x Post
* ---------------------------------------------------------------------------- *
use "$output_models/post_pooled_share_ame_stata.dta", clear

gen byte model_order = (exposure_order - 1) * 2 + control_order
gen str20 b_fmt = string(estimate, "%9.3f") + ///
    cond(p_value < .01, "***", cond(p_value < .05, "**", cond(p_value < .10, "*", "")))
gen str20 se_fmt = "(" + string(se, "%9.3f") + ")"
sort model_order

forvalues m = 1/4 {
    local pooled_b_`m' = b_fmt[`m']
    local pooled_se_`m' = se_fmt[`m']
    local pooled_n_`m' : display %12.0fc nobs[`m']
}

capture file close tex
file open tex using "$output_tex/post_pooled_share_logit_ame_stata.tex", write replace text
file write tex "\begin{table}[!htbp]" _n
file write tex "\normalsize" _n
file write tex "\renewcommand{\arraystretch}{1.15}" _n
file write tex "\centering" _n
file write tex "\caption{Effect on migration intentions: pooled Spanish exposure, Logit average marginal effects}" _n
file write tex "\label{tab:post_pooled_share_ame_stata}" _n
file write tex "\setlength{\tabcolsep}{4pt}" _n
file write tex "\captionsetup{justification=centering}" _n
file write tex "\begin{tabular}{lcccc}" _n
file write tex "\hline" _n
file write tex " & (1) & (2) & (3) & (4) \\" _n
file write tex "\hline" _n
file write tex "Spanish share 1936--1978 \(\times\) Post & `pooled_b_1' & `pooled_b_2' & `pooled_b_3' & `pooled_b_4' \\" _n
file write tex " & `pooled_se_1' & `pooled_se_2' & `pooled_se_3' & `pooled_se_4' \\" _n
file write tex "\hline" _n
file write tex "Observations & `pooled_n_1' & `pooled_n_2' & `pooled_n_3' & `pooled_n_4' \\" _n
file write tex "Year FE & Yes & Yes & Yes & Yes \\" _n
file write tex "Municipality FE & Yes & Yes & Yes & Yes \\" _n
file write tex "Controls & No & Yes & No & Yes \\" _n
file write tex "Spanish share census & 1970 & 1970 & 1980 & 1980 \\" _n
file write tex "\hline" _n
file write tex "\end{tabular}" _n
file write tex "\par\vspace{1.5mm}" _n
file write tex "\begin{minipage}{0.98\linewidth}" _n
file write tex "\footnotesize\setlength{\parindent}{0pt}\setlength{\parskip}{1pt}" _n
file write tex "Notes: The dependent variable is migration intention. Each column reports a separate Logit specification. Entries are Logit average marginal effects on the 0--1 probability scale for \(Spanish\ share_{1936-1978} \times Post\). Spanish share is the share of Spanish-born immigrants who arrived between 1936 and 1978 over total municipal population, measured using the 1970 census and the 1980 census. All models include year and municipality fixed effects. Standard errors are clustered at the municipality level and reported in parentheses (`ame_clusters' clusters). Controls include age and male. * \(p<0.10\), ** \(p<0.05\), *** \(p<0.01\)." _n
file write tex "\end{minipage}" _n
file write tex "\end{table}" _n
file close tex

* ---------------------------------------------------------------------------- *
* 24.7 Pooled Spanish exposure: key triple-difference terms
* ---------------------------------------------------------------------------- *
use "$output_models/triple_pooled_all_terms_ame_stata.dta", clear
keep if term_order == 3

quietly summarize nobs if controls == "No", meanonly
local pooled_triple_n_no : display %12.0fc r(mean)
quietly summarize nobs if controls == "Yes", meanonly
local pooled_triple_n_yes : display %12.0fc r(mean)

gen str20 b = string(estimate, "%9.3f") + ///
    cond(p_value < .01, "***", cond(p_value < .05, "**", cond(p_value < .10, "*", "")))
gen str20 e = "(" + string(se, "%9.3f") + ")"
keep x_order x x_label control_order controls exposure_order b e
reshape wide b e, i(x_order x x_label control_order controls) j(exposure_order)
sort x_order control_order

capture file close tex
file open tex using "$output_tex/triple_pooled_share_logit_ame_stata.tex", write replace text
file write tex "\begin{table}[!htbp]" _n
file write tex "\normalsize" _n
file write tex "\renewcommand{\arraystretch}{1.25}" _n
file write tex "\setlength{\tabcolsep}{6pt}" _n
file write tex "\centering" _n
file write tex "\caption{Triple differences: pooled Spanish exposure, Logit average marginal effects}" _n
file write tex "\label{tab:triple_pooled_share_ame_stata}" _n
file write tex "\setlength{\tabcolsep}{4pt}" _n
file write tex "\captionsetup{justification=centering}" _n
file write tex "\begin{tabular}{llcc}" _n
file write tex "\hline" _n
file write tex "Municipal characteristic & Controls & 1970 Census & 1980 Census \\" _n
file write tex "\hline" _n

forvalues i = 1/`=_N' {
    local lab = x_label[`i']
    local ctrl = controls[`i']
    local c70 = b1[`i']
    local s70 = e1[`i']
    local c80 = b2[`i']
    local s80 = e2[`i']

    file write tex "`lab' & `ctrl' & `c70' & `c80' \\" _n
    file write tex " & & `s70' & `s80' \\" _n

    if control_order[`i'] == 2 & `i' < _N file write tex "\addlinespace" _n
}

file write tex "\hline" _n
file write tex "\end{tabular}" _n
file write tex "\par\vspace{1.5mm}" _n
file write tex "\begin{minipage}{0.98\linewidth}" _n
file write tex "\footnotesize\setlength{\parindent}{0pt}\setlength{\parskip}{1pt}" _n
file write tex "Notes: The dependent variable is migration intention. Each pair of rows reports a separate triple-difference specification. Entries are Logit average marginal effects on the 0--1 probability scale for \(Spanish\ share_{1936-1978} \times Post \times X\). All models include year and municipality fixed effects. Standard errors are clustered at the municipality level and reported in parentheses (`ame_clusters' clusters). Controls include age, male, and \(Post\) interacted with the remaining common census/electoral municipal characteristics. The number of observations is `pooled_triple_n_no' in models without controls and `pooled_triple_n_yes' in models with controls. * \(p<0.10\), ** \(p<0.05\), *** \(p<0.01\)." _n
file write tex "\end{minipage}" _n
file write tex "\end{table}" _n
file close tex

* ---------------------------------------------------------------------------- *
* 24.8 Pooled Spanish exposure: all interaction terms, by census
* ---------------------------------------------------------------------------- *
foreach eorder in 1 2 {
    local exposure "c70"
    local etitle "Spanish presence from 1970 Census"
    local census_year "1970"
    local remaining_controls "remaining common census/electoral municipal characteristics"

    if `eorder' == 2 {
        local exposure "c80"
        local etitle "Spanish presence from 1980 Census"
        local census_year "1980"
        local remaining_controls "remaining pre-2022 municipal characteristics"
    }

    use "$output_models/triple_pooled_all_terms_ame_stata.dta", clear
    keep if exposure_order == `eorder'

    quietly summarize nobs if controls == "No", meanonly
    local pooled_all_n_no : display %12.0fc r(mean)
    quietly summarize nobs if controls == "Yes", meanonly
    local pooled_all_n_yes : display %12.0fc r(mean)

    keep x_order x x_label term_order control_order estimate se p_value
    reshape wide estimate se p_value, ///
        i(x_order x x_label term_order) j(control_order)
    sort x_order term_order

    gen str20 b1 = string(estimate1, "%9.3f") + ///
        cond(p_value1 < .01, "***", cond(p_value1 < .05, "**", cond(p_value1 < .10, "*", "")))
    gen str20 e1 = "(" + string(se1, "%9.3f") + ")"
    gen str20 b2 = string(estimate2, "%9.3f") + ///
        cond(p_value2 < .01, "***", cond(p_value2 < .05, "**", cond(p_value2 < .10, "*", "")))
    gen str20 e2 = "(" + string(se2, "%9.3f") + ")"

    capture file close tex
    file open tex using "$output_tex/triple_pooled_all_interactions_`exposure'_logit_ame_stata.tex", write replace text
    file write tex "\begin{table}[!htbp]" _n
    file write tex "\normalsize" _n
    file write tex "\renewcommand{\arraystretch}{1.25}" _n
    file write tex "\setlength{\tabcolsep}{6pt}" _n
    file write tex "\centering" _n
    file write tex "\caption{Triple-difference models: all interaction terms, `etitle', Logit average marginal effects}" _n
    file write tex "\label{tab:triple_pooled_all_`exposure'_ame_stata}" _n
    file write tex "\setlength{\tabcolsep}{4pt}" _n
    file write tex "\captionsetup{justification=centering}" _n
    file write tex "\begin{tabular}{llcc}" _n
    file write tex "\hline" _n
    file write tex "Municipal characteristic & Term & No controls & Controls \\" _n
    file write tex "\hline" _n

    forvalues i = 1/`=_N' {
        local xprint = x_label[`i']
        if `i' > 1 & x_order[`i'] == x_order[`i'-1] local xprint ""

        local term "Spanish share 1936--1978 \(\times\) Post"
        if term_order[`i'] == 2 local term "\(Post \times X\)"
        if term_order[`i'] == 3 local term "Spanish share 1936--1978 \(\times\) Post \(\times X\)"

        local c1 = b1[`i']
        local s1 = e1[`i']
        local c2 = b2[`i']
        local s2 = e2[`i']

        file write tex "`xprint' & `term' & `c1' & `c2' \\" _n
        file write tex " & & `s1' & `s2' \\" _n

        if term_order[`i'] == 3 & `i' < _N file write tex "\addlinespace" _n
    }

    file write tex "\hline" _n
    file write tex "\end{tabular}" _n
    file write tex "\par\vspace{1.5mm}" _n
    file write tex "\begin{minipage}{0.98\linewidth}" _n
    file write tex "\footnotesize\setlength{\parindent}{0pt}\setlength{\parskip}{1pt}" _n
    file write tex "Notes: The dependent variable is migration intention. Each block reports a separate triple-difference specification in which the first-column variable is the municipal characteristic \(X\). Entries are Logit average marginal effects on the 0--1 probability scale. Standard errors are clustered at the municipality level and reported in parentheses (`ame_clusters' clusters). Spanish share is the share of Spanish-born immigrants who arrived between 1936 and 1978 over total municipal population, measured using the `census_year' census. All models include year and municipality fixed effects. Controls additionally include age, male, and \(Post\) interacted with the `remaining_controls'. The number of observations is `pooled_all_n_no' in models without controls and `pooled_all_n_yes' in models with controls. * \(p<0.10\), ** \(p<0.05\), *** \(p<0.01\)." _n
    file write tex "\end{minipage}" _n
    file write tex "\end{table}" _n
    file close tex
}

* ---------------------------------------------------------------------------- *
* 24.9 Pooled-exposure pre-trends
* ---------------------------------------------------------------------------- *
use "$output_models/pooled_pretrends_ame_stata.dta", clear
keep term_order year_effect model_order estimate se p_value nobs
reshape wide estimate se p_value nobs, i(term_order year_effect) j(model_order)
sort term_order

forvalues m = 1/4 {
    gen str20 b`m' = string(estimate`m', "%9.3f") + ///
        cond(p_value`m' < .01, "***", cond(p_value`m' < .05, "**", cond(p_value`m' < .10, "*", "")))
    gen str20 e`m' = "(" + string(se`m', "%9.3f") + ")"
    local pre_n_`m' : display %12.0fc nobs`m'[1]
}

capture file close tex
file open tex using "$output_tex/pooled_pretrends_logit_ame_stata.tex", write replace text
file write tex "\begin{table}[!htbp]" _n
file write tex "\normalsize" _n
file write tex "\renewcommand{\arraystretch}{1.25}" _n
file write tex "\setlength{\tabcolsep}{6pt}" _n
file write tex "\centering" _n
file write tex "\caption{Pre-trends in migration intention: pooled Spanish exposure, Logit average marginal effects}" _n
file write tex "\label{tab:pooled_pretrends_ame_stata}" _n
file write tex "\setlength{\tabcolsep}{4pt}" _n
file write tex "\captionsetup{justification=centering}" _n
file write tex "\begin{tabular}{lcccc}" _n
file write tex "\hline" _n
file write tex " & (1) & (2) & (3) & (4) \\" _n
file write tex "\hline" _n

forvalues i = 1/`=_N' {
    local y = year_effect[`i']
    local c1 = b1[`i']
    local c2 = b2[`i']
    local c3 = b3[`i']
    local c4 = b4[`i']
    local s1 = e1[`i']
    local s2 = e2[`i']
    local s3 = e3[`i']
    local s4 = e4[`i']

    file write tex "Spanish share 1936--1978 \(\times\) `y' & `c1' & `c2' & `c3' & `c4' \\" _n
    file write tex " & `s1' & `s2' & `s3' & `s4' \\" _n
}

file write tex "\hline" _n
file write tex "Observations & `pre_n_1' & `pre_n_2' & `pre_n_3' & `pre_n_4' \\" _n
file write tex "Municipality FE & Yes & Yes & Yes & Yes \\" _n
file write tex "Year FE & Yes & Yes & Yes & Yes \\" _n
file write tex "Controls & No & No & Yes & Yes \\" _n
file write tex "Spanish share census & 1970 & 1980 & 1970 & 1980 \\" _n
file write tex "\hline" _n
file write tex "\end{tabular}" _n
file write tex "\par\vspace{1.5mm}" _n
file write tex "\begin{minipage}{0.98\linewidth}" _n
file write tex "\footnotesize\setlength{\parindent}{0pt}\setlength{\parskip}{1pt}" _n
file write tex "Notes: The dependent variable is migration intention. Entries are Logit average marginal effects on the 0--1 probability scale. Spanish share is the share of Spanish-born immigrants who arrived between 1936 and 1978 over total municipal population, measured using the 1970 census and the 1980 census. Coefficients are reported relative to 2019, the latest survey year before the shock. All models include year and municipality fixed effects. Standard errors are clustered at the municipality level and reported in parentheses (`ame_clusters' clusters). Controls include age and male. * \(p<0.10\), ** \(p<0.05\), *** \(p<0.01\)." _n
file write tex "\end{minipage}" _n
file write tex "\end{table}" _n
file close tex

noi display as result _newline ///
    "Standardized Logit AME LaTeX tables exported with standard errors below coefficients."
noi display as result ///
    "The estimation results, Excel outputs, LPM diagnostics, and tercile tables were not modified."

* ============================================================================ *

* ============================================================================ *
* REEXPORTAR LAS TRES TABLAS LARGAS CON ERRORES ESTANDAR AL COSTADO
*
* Agregar este bloque AL FINAL del do-file.
*
* Tablas modificadas:
*   1. triple_all_interactions_logit_ame_stata.tex
*   2. triple_pooled_all_interactions_c70_logit_ame_stata.tex
*   3. triple_pooled_all_interactions_c80_logit_ame_stata.tex
*
* Los AME permanecen en la escala de probabilidad 0-1.
* Las estimaciones, errores estandar, p-values y muestras no se modifican.
* ============================================================================ *

* Si por algun motivo no quedo definido el numero de clusters,
* usar el valor correspondiente a los resultados actuales.
if "`ame_clusters'" == "" {
    local ame_clusters "56"
}


* ---------------------------------------------------------------------------- *
* A. Triple differences: all interaction terms, two historical windows
* ---------------------------------------------------------------------------- *

use "$output_models/triple_all_interactions_ame_stata.dta", clear

* Numero de observaciones para las notas
quietly summarize nobs if controls == "No", meanonly
local all_terms_n_no : display %12.0fc r(mean)

quietly summarize nobs if controls == "Yes", meanonly
local all_terms_n_yes : display %12.0fc r(mean)

* Una fila por termino y columnas separadas por especificacion
keep ///
    x_order x x_label ///
    term_order control_order ///
    estimate se p_value nobs

reshape wide estimate se p_value nobs, ///
    i(x_order x x_label term_order) ///
    j(control_order)

sort x_order term_order

* Coeficiente, estrellas y error estandar en una misma celda
gen str50 cell1 = ///
    string(estimate1, "%9.3f") + ///
    cond(p_value1 < .01, "***", ///
        cond(p_value1 < .05, "**", ///
            cond(p_value1 < .10, "*", ""))) + ///
    " (" + string(se1, "%9.3f") + ")"

gen str50 cell2 = ///
    string(estimate2, "%9.3f") + ///
    cond(p_value2 < .01, "***", ///
        cond(p_value2 < .05, "**", ///
            cond(p_value2 < .10, "*", ""))) + ///
    " (" + string(se2, "%9.3f") + ")"

capture file close tex

file open tex using ///
    "$output_tex/triple_all_interactions_logit_ame_stata.tex", ///
    write replace text

file write tex "\begin{table}[!htbp]" _n
file write tex "\small" _n
file write tex "\renewcommand{\arraystretch}{1}" _n
file write tex "\setlength{\tabcolsep}{3pt}" _n
file write tex "\centering" _n

file write tex ///
    "\caption{Triple-difference models: all interaction terms, Logit average marginal effects}" _n

file write tex ///
    "\label{tab:triple_all_interactions_ame_stata}" _n

file write tex ///
    "\captionsetup{justification=centering}" _n

file write tex "\begin{tabular}{llcc}" _n
file write tex "\hline" _n

file write tex ///
    "Municipal characteristic & Term & No controls & Controls \\" _n

file write tex "\hline" _n

forvalues i = 1/`=_N' {

    local xprint = x_label[`i']

    if `i' > 1 & x_order[`i'] == x_order[`i'-1] {
        local xprint ""
    }

    local term ///
        "Spanish share 1936--1955 \(\times\) Post"

    if term_order[`i'] == 2 {
        local term ///
            "Spanish share 1956--1978 \(\times\) Post"
    }

    if term_order[`i'] == 3 {
        local term ///
            "\(Post \times X\)"
    }

    if term_order[`i'] == 4 {
        local term ///
            "Spanish share 1936--1955 \(\times\) Post \(\times X\)"
    }

    if term_order[`i'] == 5 {
        local term ///
            "Spanish share 1956--1978 \(\times\) Post \(\times X\)"
    }

    local c1 = cell1[`i']
    local c2 = cell2[`i']

    file write tex ///
        "`xprint' & `term' & `c1' & `c2' \\" _n

    if term_order[`i'] == 5 & `i' < _N {
        file write tex "\addlinespace" _n
    }
}

file write tex "\hline" _n
file write tex "\end{tabular}" _n
file write tex "\par\vspace{1.5mm}" _n
file write tex "\begin{minipage}{0.98\linewidth}" _n

file write tex ///
    "\footnotesize\setlength{\parindent}{0pt}\setlength{\parskip}{1pt}" _n

file write tex ///
    "Notes: The dependent variable is migration intention. Each block reports a separate triple-difference specification for municipal characteristic \(X\). Entries are Logit average marginal effects on the 0--1 probability scale, with standard errors reported in parentheses beside the corresponding estimate. Standard errors are clustered at the municipality level (`ame_clusters' clusters). All models include year and municipality fixed effects. Controls additionally include age, male, and \(Post\) interacted with the remaining pre-2022 municipal characteristics. Lower-order interaction terms are conditional on the other interacted variables being zero. The number of observations is `all_terms_n_no' in models without controls and `all_terms_n_yes' in models with controls. * \(p<0.10\), ** \(p<0.05\), *** \(p<0.01\)." _n

file write tex "\end{minipage}" _n
file write tex "\end{table}" _n

file close tex


* ---------------------------------------------------------------------------- *
* B. Pooled Spanish exposure: all interaction terms, by census
*
* Este loop exporta:
*   - triple_pooled_all_interactions_c70_logit_ame_stata.tex
*   - triple_pooled_all_interactions_c80_logit_ame_stata.tex
* ---------------------------------------------------------------------------- *

foreach eorder in 1 2 {

    local exposure "c70"
    local etitle ///
        "Spanish presence from 1970 Census"
    local census_year "1970"
    local remaining_controls ///
        "remaining common census/electoral municipal characteristics"

    if `eorder' == 2 {

        local exposure "c80"
        local etitle ///
            "Spanish presence from 1980 Census"
        local census_year "1980"
        local remaining_controls ///
            "remaining pre-2022 municipal characteristics"
    }

    use ///
        "$output_models/triple_pooled_all_terms_ame_stata.dta", ///
        clear

    keep if exposure_order == `eorder'

    * Numero de observaciones para las notas
    quietly summarize nobs if controls == "No", meanonly
    local pooled_all_n_no : display %12.0fc r(mean)

    quietly summarize nobs if controls == "Yes", meanonly
    local pooled_all_n_yes : display %12.0fc r(mean)

    keep ///
        x_order x x_label ///
        term_order control_order ///
        estimate se p_value

    reshape wide estimate se p_value, ///
        i(x_order x x_label term_order) ///
        j(control_order)

    sort x_order term_order

    * Coeficiente, estrellas y error estandar en una misma celda
    gen str50 cell1 = ///
        string(estimate1, "%9.3f") + ///
        cond(p_value1 < .01, "***", ///
            cond(p_value1 < .05, "**", ///
                cond(p_value1 < .10, "*", ""))) + ///
        " (" + string(se1, "%9.3f") + ")"

    gen str50 cell2 = ///
        string(estimate2, "%9.3f") + ///
        cond(p_value2 < .01, "***", ///
            cond(p_value2 < .05, "**", ///
                cond(p_value2 < .10, "*", ""))) + ///
        " (" + string(se2, "%9.3f") + ")"

    capture file close tex

    file open tex using ///
        "$output_tex/triple_pooled_all_interactions_`exposure'_logit_ame_stata.tex", ///
        write replace text

    file write tex "\begin{table}[!htbp]" _n
    file write tex "\small" _n
    file write tex "\renewcommand{\arraystretch}{1}" _n
    file write tex "\setlength{\tabcolsep}{3pt}" _n
    file write tex "\centering" _n

    file write tex ///
        "\caption{Triple-difference models: all interaction terms, `etitle', Logit average marginal effects}" _n

    file write tex ///
        "\label{tab:triple_pooled_all_`exposure'_ame_stata}" _n

    file write tex ///
        "\captionsetup{justification=centering}" _n

    file write tex "\begin{tabular}{llcc}" _n
    file write tex "\hline" _n

    file write tex ///
        "Municipal characteristic & Term & No controls & Controls \\" _n

    file write tex "\hline" _n

    forvalues i = 1/`=_N' {

        local xprint = x_label[`i']

        if `i' > 1 & x_order[`i'] == x_order[`i'-1] {
            local xprint ""
        }

        local term ///
            "Spanish share 1936--1978 \(\times\) Post"

        if term_order[`i'] == 2 {
            local term ///
                "\(Post \times X\)"
        }

        if term_order[`i'] == 3 {
            local term ///
                "Spanish share 1936--1978 \(\times\) Post \(\times X\)"
        }

        local c1 = cell1[`i']
        local c2 = cell2[`i']

        file write tex ///
            "`xprint' & `term' & `c1' & `c2' \\" _n

        if term_order[`i'] == 3 & `i' < _N {
            file write tex "\addlinespace" _n
        }
    }

    file write tex "\hline" _n
    file write tex "\end{tabular}" _n
    file write tex "\par\vspace{1.5mm}" _n
    file write tex "\begin{minipage}{0.98\linewidth}" _n

    file write tex ///
        "\footnotesize\setlength{\parindent}{0pt}\setlength{\parskip}{1pt}" _n

    file write tex ///
        "Notes: The dependent variable is migration intention. Each block reports a separate triple-difference specification in which the first-column variable is the municipal characteristic \(X\). Entries are Logit average marginal effects on the 0--1 probability scale, with standard errors reported in parentheses beside the corresponding estimate. Standard errors are clustered at the municipality level (`ame_clusters' clusters). Spanish share is the share of Spanish-born immigrants who arrived between 1936 and 1978 over total municipal population, measured using the `census_year' census. All models include year and municipality fixed effects. Controls additionally include age, male, and \(Post\) interacted with the `remaining_controls'. The number of observations is `pooled_all_n_no' in models without controls and `pooled_all_n_yes' in models with controls. * \(p<0.10\), ** \(p<0.05\), *** \(p<0.01\)." _n

    file write tex "\end{minipage}" _n
    file write tex "\end{table}" _n

    file close tex
}


noi display as result _newline ///
    "Three long Logit AME tables re-exported with standard errors beside estimates."

noi display as result ///
    "  $output_tex/triple_all_interactions_logit_ame_stata.tex"

noi display as result ///
    "  $output_tex/triple_pooled_all_interactions_c70_logit_ame_stata.tex"

noi display as result ///
    "  $output_tex/triple_pooled_all_interactions_c80_logit_ame_stata.tex"
	

* ============================================================================ *
* 25. PARALLEL LINEAR PROBABILITY MODEL TABLES FOR THE APPENDIX
*
* This section is append-only. It does not alter any Logit estimation or any
* existing output. For every final Logit table, it estimates a parallel LPM on
* exactly the corresponding Logit estimation sample, with the same regressors,
* survey weights, year and municipality fixed effects, and municipality-
* clustered standard errors.
*
* Output names mirror the Logit table names, replacing "logit" / "logit_ame"
* with "lpm". Every LaTeX note reports the unweighted percentage of fitted
* values strictly below 0 or above 1.
* ============================================================================ *

* ---------------------------------------------------------------------------- *
* 25.0 Setup and helper
* ---------------------------------------------------------------------------- *

capture confirm file "$data_int/lapop_remaining_mixed_tables_ready.dta"
if _rc {
    display as error "Missing file: $data_int/lapop_remaining_mixed_tables_ready.dta"
    display as error "Run the preceding sections of this do-file first."
    exit 601
}

use "$data_int/lapop_remaining_mixed_tables_ready.dta", clear

local lpm_common_x_vars ///
    popdensgeo2_2010 ///
    share_female_2010 ///
    mean_yrschool_2010 ///
    median_age_2010 ///
    share_laborforce_2010 ///
    share_unemployed_2010 ///
    share_izq_amplia_pre_avg ///
    share_alt_pre_avg

local lpm_tercile_vars ///
    t_density_2010 ///
    t_fem_2010 ///
    t_mean_schyr_2010 ///
    t_med_dage_2010 ///
    t_pea_2010 ///
    t_unemp_2010 ///
    t_izam_pre_avg ///
    t_alt_pre_avg

local lpm_source_vars ///
    popdensgeo2_2010 ///
    share_female_2010 ///
    mean_yrschool_2010 ///
    median_age_2010 ///
    share_laborforce_2010 ///
    share_unemployed_2010 ///
    share_izq_amplia_pre_avg ///
    share_alt_pre_avg

local lpm_ind_vars_1 ///
    "edad hombre desempleado en_pareja secundaria_completa_o_mas izq_der interes_pol_mucho voto_blanco_nulo"
local lpm_ind_vars_2 ///
    "edad hombre desempleado en_pareja secundaria_completa_o_mas izq_der interes_pol_mucho"
local lpm_ind_vars_3 ///
    "edad hombre desempleado en_pareja secundaria_completa_o_mas interes_pol_mucho voto_blanco_nulo"

local lpm_ind_label_1 "Full"
local lpm_ind_label_2 "No blank/null vote"
local lpm_ind_label_3 "No ideology"

* Recreate pre-trend interactions if needed.
foreach e in 70 80 {
    local sharevar "share_36_78_c`e'"
    foreach y in 2012 2014 2017 2023 {
        capture drop es`e'_`y'
        gen double es`e'_`y' = (year_num == `y') * `sharevar'
    }
}

compress
save "$data_int/lapop_lpm_parallel_tables_ready.dta", replace

capture program drop lpm_active_diagnostics
program define lpm_active_diagnostics, rclass
    version 17

    tempvar lpm_hat cluster_tag

    quietly predict double `lpm_hat' if e(sample), xb

    quietly count if e(sample) & `lpm_hat' < 0
    local below0 = r(N)

    quietly count if e(sample) & `lpm_hat' > 1
    local above1 = r(N)

    local outside = `below0' + `above1'
    local N = e(N)

    quietly egen byte `cluster_tag' = tag(mun_code_num) if e(sample)
    quietly count if `cluster_tag' == 1
    local G = r(N)

    return scalar nobs        = `N'
    return scalar clusters    = `G'
    return scalar below0_n    = `below0'
    return scalar above1_n    = `above1'
    return scalar outside_n   = `outside'
    return scalar outside_pct = 100 * `outside' / `N'
end

* ---------------------------------------------------------------------------- *
* 25.1 Spanish historical exposure x Post
* ---------------------------------------------------------------------------- *

use "$data_int/lapop_lpm_parallel_tables_ready.dta", clear

tempname lpm_ps_h
tempfile lpm_ps_long
postfile `lpm_ps_h' ///
    byte term_order str20 term_id str80 term_label ///
    byte control_order str3 controls ///
    double estimate se p_value ///
    long nobs clusters below0_n above1_n outside_n ///
    double outside_pct ///
    using `lpm_ps_long', replace

foreach corder in 1 2 {
    local controls "No"
    local rhs_extra ""
    if `corder' == 2 {
        local controls "Yes"
        local rhs_extra "edad hombre"
    }

    quietly logit intencion_migrar ///
        pshare36 pshare56 `rhs_extra' ///
        i.year_num i.mun_code_num ///
        [pw=wt], vce(cluster mun_code_num)

    tempvar sample_logit
    gen byte `sample_logit' = e(sample)

    quietly regress intencion_migrar ///
        pshare36 pshare56 `rhs_extra' ///
        i.year_num i.mun_code_num ///
        if `sample_logit' == 1 [pw=wt], ///
        vce(cluster mun_code_num)

    quietly lpm_active_diagnostics

    local lpm_N       = r(nobs)
    local lpm_G       = r(clusters)
    local lpm_below   = r(below0_n)
    local lpm_above   = r(above1_n)
    local lpm_outside = r(outside_n)
    local lpm_pct     = r(outside_pct)

    foreach torder in 1 2 {
        local term "pshare36"
        local tlab "Spanish share 1936--1955 x Post"
        if `torder' == 2 {
            local term "pshare56"
            local tlab "Spanish share 1956--1978 x Post"
        }

        local b = _b[`term']
        local s = _se[`term']
        local p = 2 * ttail(e(df_r), abs(`b' / `s'))

        post `lpm_ps_h' ///
            (`torder') ("`term'") ("`tlab'") ///
            (`corder') ("`controls'") ///
            (`b') (`s') (`p') ///
            (`lpm_N') (`lpm_G') ///
            (`lpm_below') (`lpm_above') (`lpm_outside') ///
            (`lpm_pct')
    }

    drop `sample_logit'
}
postclose `lpm_ps_h'

use `lpm_ps_long', clear
save "$output_models/lpm_post_share_mfx_long_stata.dta", replace

* ---------------------------------------------------------------------------- *
* 25.2 Post x municipal characteristics
* ---------------------------------------------------------------------------- *

use "$data_int/lapop_lpm_parallel_tables_ready.dta", clear

tempname lpm_px_h
tempfile lpm_px_long
postfile `lpm_px_h' ///
    byte x_order str40 variable str70 variable_label ///
    byte control_order str3 controls ///
    double estimate se p_value ///
    long nobs clusters below0_n above1_n outside_n ///
    double outside_pct ///
    using `lpm_px_long', replace

local xorder = 0
foreach x of local lpm_common_x_vars {
    local ++xorder

    local xlabel "`x'"
    if "`x'" == "popdensgeo2_2010"         local xlabel "Population density"
    if "`x'" == "share_female_2010"        local xlabel "Female population share"
    if "`x'" == "mean_yrschool_2010"       local xlabel "Mean years of education"
    if "`x'" == "median_age_2010"           local xlabel "Median age"
    if "`x'" == "share_laborforce_2010"    local xlabel "Share in labor force"
    if "`x'" == "share_unemployed_2010"    local xlabel "Share unemployed"
    if "`x'" == "share_izq_amplia_pre_avg" local xlabel "Left vote share"
    if "`x'" == "share_alt_pre_avg"         local xlabel "Ideological alternation"

    foreach corder in 1 2 {
        local controls "No"
        local rhs_extra ""
        if `corder' == 2 {
            local controls "Yes"
            local rhs_extra "edad hombre"
        }

        quietly logit intencion_migrar ///
            px_`x' `rhs_extra' ///
            i.year_num i.mun_code_num ///
            [pw=wt], vce(cluster mun_code_num)

        tempvar sample_logit
        gen byte `sample_logit' = e(sample)

        quietly regress intencion_migrar ///
            px_`x' `rhs_extra' ///
            i.year_num i.mun_code_num ///
            if `sample_logit' == 1 [pw=wt], ///
            vce(cluster mun_code_num)

        quietly lpm_active_diagnostics

        local lpm_N       = r(nobs)
        local lpm_G       = r(clusters)
        local lpm_below   = r(below0_n)
        local lpm_above   = r(above1_n)
        local lpm_outside = r(outside_n)
        local lpm_pct     = r(outside_pct)

        local b = _b[px_`x']
        local s = _se[px_`x']
        local p = 2 * ttail(e(df_r), abs(`b' / `s'))

        post `lpm_px_h' ///
            (`xorder') ("`x'") ("`xlabel'") ///
            (`corder') ("`controls'") ///
            (`b') (`s') (`p') ///
            (`lpm_N') (`lpm_G') ///
            (`lpm_below') (`lpm_above') (`lpm_outside') ///
            (`lpm_pct')

        drop `sample_logit'
    }
}
postclose `lpm_px_h'

use `lpm_px_long', clear
save "$output_models/lpm_post_x_mfx_long_stata.dta", replace

* ---------------------------------------------------------------------------- *
* 25.3 Two-window triple differences: all terms
* ---------------------------------------------------------------------------- *

use "$data_int/lapop_lpm_parallel_tables_ready.dta", clear

tempname lpm_tr_h
tempfile lpm_tr_long
postfile `lpm_tr_h' ///
    byte x_order str40 x str70 x_label ///
    byte term_order str60 term_id str100 term_label ///
    byte control_order str3 controls ///
    double estimate se p_value ///
    long nobs clusters below0_n above1_n outside_n ///
    double outside_pct ///
    using `lpm_tr_long', replace

local xorder = 0
foreach x of local lpm_common_x_vars {
    local ++xorder

    local xlabel "`x'"
    if "`x'" == "popdensgeo2_2010"         local xlabel "Population density"
    if "`x'" == "share_female_2010"        local xlabel "Female population share"
    if "`x'" == "mean_yrschool_2010"       local xlabel "Mean years of education"
    if "`x'" == "median_age_2010"           local xlabel "Median age"
    if "`x'" == "share_laborforce_2010"    local xlabel "Share in labor force"
    if "`x'" == "share_unemployed_2010"    local xlabel "Share unemployed"
    if "`x'" == "share_izq_amplia_pre_avg" local xlabel "Left vote share"
    if "`x'" == "share_alt_pre_avg"         local xlabel "Ideological alternation"

    foreach corder in 1 2 {
        local controls "No"
        local rhs_extra ""
        if `corder' == 2 {
            local controls "Yes"
            local rhs_extra "edad hombre"
            foreach z of local lpm_common_x_vars {
                if "`z'" != "`x'" local rhs_extra "`rhs_extra' px_`z'"
            }
        }

        local rhs_core "pshare36 pshare56 px_`x' pxs36_`x' pxs56_`x'"

        quietly logit intencion_migrar ///
            `rhs_core' `rhs_extra' ///
            i.year_num i.mun_code_num ///
            [pw=wt], vce(cluster mun_code_num)

        tempvar sample_logit
        gen byte `sample_logit' = e(sample)

        quietly regress intencion_migrar ///
            `rhs_core' `rhs_extra' ///
            i.year_num i.mun_code_num ///
            if `sample_logit' == 1 [pw=wt], ///
            vce(cluster mun_code_num)

        quietly lpm_active_diagnostics

        local lpm_N       = r(nobs)
        local lpm_G       = r(clusters)
        local lpm_below   = r(below0_n)
        local lpm_above   = r(above1_n)
        local lpm_outside = r(outside_n)
        local lpm_pct     = r(outside_pct)

        local terms "pshare36 pshare56 px_`x' pxs36_`x' pxs56_`x'"
        local torder = 0
        foreach term of local terms {
            local ++torder

            local tlab "Spanish share 1936--1955 x Post"
            if `torder' == 2 local tlab "Spanish share 1956--1978 x Post"
            if `torder' == 3 local tlab "Post x X"
            if `torder' == 4 local tlab "Spanish share 1936--1955 x Post x X"
            if `torder' == 5 local tlab "Spanish share 1956--1978 x Post x X"

            local b = _b[`term']
            local s = _se[`term']
            local p = 2 * ttail(e(df_r), abs(`b' / `s'))

            post `lpm_tr_h' ///
                (`xorder') ("`x'") ("`xlabel'") ///
                (`torder') ("`term'") ("`tlab'") ///
                (`corder') ("`controls'") ///
                (`b') (`s') (`p') ///
                (`lpm_N') (`lpm_G') ///
                (`lpm_below') (`lpm_above') (`lpm_outside') ///
                (`lpm_pct')
        }

        drop `sample_logit'
    }
}
postclose `lpm_tr_h'

use `lpm_tr_long', clear
save "$output_models/lpm_triple_all_terms_stata.dta", replace

* ---------------------------------------------------------------------------- *
* 25.4 Heterogeneity by municipal terciles
* ---------------------------------------------------------------------------- *

use "$data_int/lapop_lpm_parallel_tables_ready.dta", clear

tempname lpm_het_h lpm_het_joint_h
tempfile lpm_het_long lpm_het_joint

postfile `lpm_het_h' ///
    byte panel_order str35 tercile_var str70 characteristic ///
    byte tercile ///
    double b36 se36 p36 b56 se56 p56 p_equal ///
    long nobs clusters ///
    double tercile_ub ///
    long below0_n above1_n outside_n double outside_pct ///
    using `lpm_het_long', replace

postfile `lpm_het_joint_h' ///
    str35 tercile_var double p_joint_36 p_joint_56 ///
    using `lpm_het_joint', replace

local panel = 0
local i = 1
foreach tv of local lpm_tercile_vars {
    local ++panel
    local sv : word `i' of `lpm_source_vars'
    local ++i

    local xlabel "`tv'"
    if "`tv'" == "t_density_2010"    local xlabel "Population density"
    if "`tv'" == "t_fem_2010"        local xlabel "Female population share"
    if "`tv'" == "t_mean_schyr_2010" local xlabel "Mean years of education"
    if "`tv'" == "t_med_dage_2010"   local xlabel "Median age"
    if "`tv'" == "t_pea_2010"        local xlabel "Share in labor force"
    if "`tv'" == "t_unemp_2010"      local xlabel "Share unemployed"
    if "`tv'" == "t_izam_pre_avg"    local xlabel "Left vote share"
    if "`tv'" == "t_alt_pre_avg"     local xlabel "Ideological alternation"

    matrix lpm_betas36 = J(3,1,.)
    matrix lpm_vars36  = J(3,3,0)
    matrix lpm_betas56 = J(3,1,.)
    matrix lpm_vars56  = J(3,3,0)

    forvalues t = 1/3 {
        quietly logit intencion_migrar ///
            pshare36 pshare56 edad hombre ///
            i.year_num i.mun_code_num ///
            if `tv' == `t' [pw=wt], ///
            vce(cluster mun_code_num)

        tempvar sample_logit
        gen byte `sample_logit' = e(sample)

        quietly regress intencion_migrar ///
            pshare36 pshare56 edad hombre ///
            i.year_num i.mun_code_num ///
            if `sample_logit' == 1 [pw=wt], ///
            vce(cluster mun_code_num)

        quietly lpm_active_diagnostics

        local lpm_N       = r(nobs)
        local lpm_G       = r(clusters)
        local lpm_below   = r(below0_n)
        local lpm_above   = r(above1_n)
        local lpm_outside = r(outside_n)
        local lpm_pct     = r(outside_pct)

        local b36 = _b[pshare36]
        local s36 = _se[pshare36]
        local p36 = 2 * ttail(e(df_r), abs(`b36' / `s36'))
        local b56 = _b[pshare56]
        local s56 = _se[pshare56]
        local p56 = 2 * ttail(e(df_r), abs(`b56' / `s56'))

        quietly test pshare36 = pshare56
        local peq = r(p)

        quietly summarize `sv' if `tv' == `t', meanonly
        local ub = r(max)

        matrix lpm_betas36[`t',1] = `b36'
        matrix lpm_vars36[`t',`t'] = `s36'^2
        matrix lpm_betas56[`t',1] = `b56'
        matrix lpm_vars56[`t',`t'] = `s56'^2

        post `lpm_het_h' ///
            (`panel') ("`tv'") ("`xlabel'") (`t') ///
            (`b36') (`s36') (`p36') (`b56') (`s56') (`p56') (`peq') ///
            (`lpm_N') (`lpm_G') (`ub') ///
            (`lpm_below') (`lpm_above') (`lpm_outside') (`lpm_pct')

        drop `sample_logit'
    }

    matrix lpm_R = (1,-1,0 \ 0,1,-1)

    matrix lpm_rb36 = lpm_R * lpm_betas36
    matrix lpm_rv36 = lpm_R * lpm_vars36 * lpm_R'
    matrix lpm_w36  = lpm_rb36' * invsym(lpm_rv36) * lpm_rb36
    local pj36 = chi2tail(2, lpm_w36[1,1])

    matrix lpm_rb56 = lpm_R * lpm_betas56
    matrix lpm_rv56 = lpm_R * lpm_vars56 * lpm_R'
    matrix lpm_w56  = lpm_rb56' * invsym(lpm_rv56) * lpm_rb56
    local pj56 = chi2tail(2, lpm_w56[1,1])

    post `lpm_het_joint_h' ("`tv'") (`pj36') (`pj56')
}
postclose `lpm_het_h'
postclose `lpm_het_joint_h'

use `lpm_het_long', clear
merge m:1 tercile_var using `lpm_het_joint', nogen
sort panel_order tercile
save "$output_models/heterog_terciles_migration_lapop_lpm_stata.dta", replace

* ---------------------------------------------------------------------------- *
* 25.5 Individual characteristics
* ---------------------------------------------------------------------------- *

use "$data_int/lapop_lpm_parallel_tables_ready.dta", clear

tempname lpm_ind_h
tempfile lpm_ind_long
postfile `lpm_ind_h' ///
    byte spec_order str25 spec str40 spec_label ///
    byte var_order str40 variable str70 variable_label ///
    double estimate se p_value ///
    long nobs clusters below0_n above1_n outside_n ///
    double outside_pct ///
    using `lpm_ind_long', replace

local lpm_ind_spec_1 "full"
local lpm_ind_spec_2 "no_blank_null_vote"
local lpm_ind_spec_3 "no_ideology"

forvalues sorder = 1/3 {
    local vars "`lpm_ind_vars_`sorder''"
    local spec "`lpm_ind_spec_`sorder''"
    local slab "`lpm_ind_label_`sorder''"

    quietly logit intencion_migrar ///
        `vars' ///
        i.year_num i.mun_code_num ///
        [pw=wt], vce(cluster mun_code_num)

    tempvar sample_logit
    gen byte `sample_logit' = e(sample)

    quietly regress intencion_migrar ///
        `vars' ///
        i.year_num i.mun_code_num ///
        if `sample_logit' == 1 [pw=wt], ///
        vce(cluster mun_code_num)

    quietly lpm_active_diagnostics

    local lpm_N       = r(nobs)
    local lpm_G       = r(clusters)
    local lpm_below   = r(below0_n)
    local lpm_above   = r(above1_n)
    local lpm_outside = r(outside_n)
    local lpm_pct     = r(outside_pct)

    foreach v of local vars {
        local vorder = .
        local vlabel "`v'"
        if "`v'" == "edad"                      local vorder = 1
        if "`v'" == "edad"                      local vlabel "Age"
        if "`v'" == "hombre"                    local vorder = 2
        if "`v'" == "hombre"                    local vlabel "Male"
        if "`v'" == "desempleado"               local vorder = 3
        if "`v'" == "desempleado"               local vlabel "Unemployed"
        if "`v'" == "en_pareja"                 local vorder = 4
        if "`v'" == "en_pareja"                 local vlabel "Partnered"
        if "`v'" == "secundaria_completa_o_mas" local vorder = 5
        if "`v'" == "secundaria_completa_o_mas" local vlabel "High School or more"
        if "`v'" == "izq_der"                   local vorder = 6
        if "`v'" == "izq_der"                   local vlabel "Left-right ideology"
        if "`v'" == "interes_pol_mucho"          local vorder = 7
        if "`v'" == "interes_pol_mucho"          local vlabel "Very interested in politics"
        if "`v'" == "voto_blanco_nulo"           local vorder = 8
        if "`v'" == "voto_blanco_nulo"           local vlabel "Blank/null vote"

        local b = _b[`v']
        local se = _se[`v']
        local p = 2 * ttail(e(df_r), abs(`b' / `se'))

        post `lpm_ind_h' ///
            (`sorder') ("`spec'") ("`slab'") ///
            (`vorder') ("`v'") ("`vlabel'") ///
            (`b') (`se') (`p') ///
            (`lpm_N') (`lpm_G') ///
            (`lpm_below') (`lpm_above') (`lpm_outside') ///
            (`lpm_pct')
    }

    drop `sample_logit'
}
postclose `lpm_ind_h'

use `lpm_ind_long', clear
save "$output_models/individual_characteristics_lpm_stata.dta", replace

* ---------------------------------------------------------------------------- *
* 25.6 Pooled Spanish exposure x Post
* ---------------------------------------------------------------------------- *

use "$data_int/lapop_lpm_parallel_tables_ready.dta", clear

tempname lpm_poolpost_h
tempfile lpm_poolpost_long
postfile `lpm_poolpost_h' ///
    byte exposure_order str3 exposure str20 exposure_label ///
    byte control_order str3 controls ///
    double estimate se p_value ///
    long nobs clusters below0_n above1_n outside_n ///
    double outside_pct ///
    using `lpm_poolpost_long', replace

foreach eorder in 1 2 {
    local e = 70
    local exposure "c70"
    local elabel "1970 Census"
    if `eorder' == 2 {
        local e = 80
        local exposure "c80"
        local elabel "1980 Census"
    }

    local pterm "pp`e'"

    foreach corder in 1 2 {
        local controls "No"
        local rhs_extra ""
        if `corder' == 2 {
            local controls "Yes"
            local rhs_extra "edad hombre"
        }

        quietly logit intencion_migrar ///
            `pterm' `rhs_extra' ///
            i.year_num i.mun_code_num ///
            [pw=wt], vce(cluster mun_code_num)

        tempvar sample_logit
        gen byte `sample_logit' = e(sample)

        quietly regress intencion_migrar ///
            `pterm' `rhs_extra' ///
            i.year_num i.mun_code_num ///
            if `sample_logit' == 1 [pw=wt], ///
            vce(cluster mun_code_num)

        quietly lpm_active_diagnostics

        local lpm_N       = r(nobs)
        local lpm_G       = r(clusters)
        local lpm_below   = r(below0_n)
        local lpm_above   = r(above1_n)
        local lpm_outside = r(outside_n)
        local lpm_pct     = r(outside_pct)

        local b = _b[`pterm']
        local se = _se[`pterm']
        local p = 2 * ttail(e(df_r), abs(`b' / `se'))

        post `lpm_poolpost_h' ///
            (`eorder') ("`exposure'") ("`elabel'") ///
            (`corder') ("`controls'") ///
            (`b') (`se') (`p') ///
            (`lpm_N') (`lpm_G') ///
            (`lpm_below') (`lpm_above') (`lpm_outside') ///
            (`lpm_pct')

        drop `sample_logit'
    }
}
postclose `lpm_poolpost_h'

use `lpm_poolpost_long', clear
save "$output_models/post_pooled_share_lpm_stata.dta", replace

* ---------------------------------------------------------------------------- *
* 25.7 Pooled Spanish exposure: triple differences, all terms
* ---------------------------------------------------------------------------- *

use "$data_int/lapop_lpm_parallel_tables_ready.dta", clear

tempname lpm_pooltr_h
tempfile lpm_pooltr_long
postfile `lpm_pooltr_h' ///
    byte exposure_order str3 exposure str20 exposure_label ///
    byte x_order str40 x str70 x_label ///
    byte term_order str60 term_id str100 term_label ///
    byte control_order str3 controls ///
    double estimate se p_value ///
    long nobs clusters below0_n above1_n outside_n ///
    double outside_pct ///
    using `lpm_pooltr_long', replace

foreach eorder in 1 2 {
    local e = 70
    local exposure "c70"
    local elabel "1970 Census"
    if `eorder' == 2 {
        local e = 80
        local exposure "c80"
        local elabel "1980 Census"
    }

    local pterm "pp`e'"

    local xorder = 0
    foreach x of local lpm_common_x_vars {
        local ++xorder

        local xlabel "`x'"
        if "`x'" == "popdensgeo2_2010"         local xlabel "Population density"
        if "`x'" == "share_female_2010"        local xlabel "Female population share"
        if "`x'" == "mean_yrschool_2010"       local xlabel "Mean years of education"
        if "`x'" == "median_age_2010"           local xlabel "Median age"
        if "`x'" == "share_laborforce_2010"    local xlabel "Share in labor force"
        if "`x'" == "share_unemployed_2010"    local xlabel "Share unemployed"
        if "`x'" == "share_izq_amplia_pre_avg" local xlabel "Left vote share"
        if "`x'" == "share_alt_pre_avg"         local xlabel "Ideological alternation"

        local triplevar "ppx`e'_`x'"

        foreach corder in 1 2 {
            local controls "No"
            local rhs_extra ""
            if `corder' == 2 {
                local controls "Yes"
                local rhs_extra "edad hombre"
                foreach z of local lpm_common_x_vars {
                    if "`z'" != "`x'" local rhs_extra "`rhs_extra' px_`z'"
                }
            }

            local rhs_core "`pterm' px_`x' `triplevar'"

            quietly logit intencion_migrar ///
                `rhs_core' `rhs_extra' ///
                i.year_num i.mun_code_num ///
                [pw=wt], vce(cluster mun_code_num)

            tempvar sample_logit
            gen byte `sample_logit' = e(sample)

            quietly regress intencion_migrar ///
                `rhs_core' `rhs_extra' ///
                i.year_num i.mun_code_num ///
                if `sample_logit' == 1 [pw=wt], ///
                vce(cluster mun_code_num)

            quietly lpm_active_diagnostics

            local lpm_N       = r(nobs)
            local lpm_G       = r(clusters)
            local lpm_below   = r(below0_n)
            local lpm_above   = r(above1_n)
            local lpm_outside = r(outside_n)
            local lpm_pct     = r(outside_pct)

            local terms "`pterm' px_`x' `triplevar'"
            local torder = 0
            foreach term of local terms {
                local ++torder

                local tlab "Spanish share 1936--1978 x Post"
                if `torder' == 2 local tlab "Post x X"
                if `torder' == 3 local tlab "Spanish share 1936--1978 x Post x X"

                local b = _b[`term']
                local se = _se[`term']
                local p = 2 * ttail(e(df_r), abs(`b' / `se'))

                post `lpm_pooltr_h' ///
                    (`eorder') ("`exposure'") ("`elabel'") ///
                    (`xorder') ("`x'") ("`xlabel'") ///
                    (`torder') ("`term'") ("`tlab'") ///
                    (`corder') ("`controls'") ///
                    (`b') (`se') (`p') ///
                    (`lpm_N') (`lpm_G') ///
                (`lpm_below') (`lpm_above') (`lpm_outside') ///
                (`lpm_pct')
            }

            drop `sample_logit'
        }
    }
}
postclose `lpm_pooltr_h'

use `lpm_pooltr_long', clear
save "$output_models/triple_pooled_all_terms_lpm_stata.dta", replace

* ---------------------------------------------------------------------------- *
* 25.8 Pooled-exposure pre-trends
* ---------------------------------------------------------------------------- *

use "$data_int/lapop_lpm_parallel_tables_ready.dta", clear

tempname lpm_pre_h
tempfile lpm_pre_long
postfile `lpm_pre_h' ///
    byte term_order int year_effect byte model_order ///
    byte exposure_order str3 exposure str20 exposure_label ///
    byte control_order str3 controls ///
    double estimate se p_value ///
    long nobs clusters below0_n above1_n outside_n ///
    double outside_pct ///
    using `lpm_pre_long', replace

local model_order = 0
foreach corder in 1 2 {
    local controls "No"
    local rhs_extra ""
    if `corder' == 2 {
        local controls "Yes"
        local rhs_extra "edad hombre"
    }

    foreach eorder in 1 2 {
        local e = 70
        local exposure "c70"
        local elabel "1970 Census"
        if `eorder' == 2 {
            local e = 80
            local exposure "c80"
            local elabel "1980 Census"
        }

        local ++model_order
        local terms "es`e'_2012 es`e'_2014 es`e'_2017 es`e'_2023"

        quietly logit intencion_migrar ///
            `terms' `rhs_extra' ///
            i.year_num i.mun_code_num ///
            [pw=wt], vce(cluster mun_code_num)

        tempvar sample_logit
        gen byte `sample_logit' = e(sample)

        quietly regress intencion_migrar ///
            `terms' `rhs_extra' ///
            i.year_num i.mun_code_num ///
            if `sample_logit' == 1 [pw=wt], ///
            vce(cluster mun_code_num)

        quietly lpm_active_diagnostics

        local lpm_N       = r(nobs)
        local lpm_G       = r(clusters)
        local lpm_below   = r(below0_n)
        local lpm_above   = r(above1_n)
        local lpm_outside = r(outside_n)
        local lpm_pct     = r(outside_pct)

        local torder = 0
        foreach y in 2012 2014 2017 2023 {
            local ++torder
            local term "es`e'_`y'"
            local b = _b[`term']
            local se = _se[`term']
            local p = 2 * ttail(e(df_r), abs(`b' / `se'))

            post `lpm_pre_h' ///
                (`torder') (`y') (`model_order') ///
                (`eorder') ("`exposure'") ("`elabel'") ///
                (`corder') ("`controls'") ///
                (`b') (`se') (`p') ///
                (`lpm_N') (`lpm_G') ///
                (`lpm_below') (`lpm_above') (`lpm_outside') ///
                (`lpm_pct')
        }

        drop `sample_logit'
    }
}
postclose `lpm_pre_h'

use `lpm_pre_long', clear
save "$output_models/pooled_pretrends_lpm_stata.dta", replace

* ============================================================================ *
* 25.9 Export parallel LPM LaTeX tables
* ============================================================================ *

* ---------------------------------------------------------------------------- *
* 25.9.1 Spanish historical exposure x Post
* Logit counterpart: logit_post_share_mfx_glm_cluster_se_stata.tex
* ---------------------------------------------------------------------------- *

use "$output_models/lpm_post_share_mfx_long_stata.dta", clear

quietly summarize nobs if controls == "No", meanonly
local lpm_ps_n_no : display %12.0fc r(mean)
quietly summarize nobs if controls == "Yes", meanonly
local lpm_ps_n_yes : display %12.0fc r(mean)
quietly summarize clusters, meanonly
local lpm_ps_clusters : display %12.0fc r(max)
quietly summarize outside_pct if controls == "No", meanonly
local lpm_ps_out_no : display %5.2f r(mean)
quietly summarize outside_pct if controls == "Yes", meanonly
local lpm_ps_out_yes : display %5.2f r(mean)

gen str20 b_fmt = string(estimate, "%9.3f") + ///
    cond(p_value < .01, "***", cond(p_value < .05, "**", cond(p_value < .10, "*", "")))
gen str20 se_fmt = "(" + string(se, "%9.3f") + ")"
sort term_order control_order

capture file close tex
file open tex using "$output_tex/lpm_post_share_mfx_glm_cluster_se_stata.tex", write replace text
file write tex "\begin{table}[!htbp]" _n
file write tex "\normalsize" _n
file write tex "\renewcommand{\arraystretch}{1.25}" _n
file write tex "\setlength{\tabcolsep}{6pt}" _n
file write tex "\centering" _n
file write tex "\caption{Linear probability model: post-period Spanish historical exposure}" _n
file write tex "\label{tab:lpm_post_share_mfx_stata}" _n
file write tex "\begin{tabular}{llc}" _n
file write tex "\hline" _n
file write tex "Term & Controls & LPM coefficient \\" _n
file write tex "\hline" _n
forvalues i = 1/`=_N' {
    local lab "Spanish share 1936--1955 \(\times\) Post"
    if term_order[`i'] == 2 local lab "Spanish share 1956--1978 \(\times\) Post"
    local ctrl = controls[`i']
    local b = b_fmt[`i']
    local s = se_fmt[`i']
    file write tex "`lab' & `ctrl' & `b' \\" _n
    file write tex " & & `s' \\" _n
    if control_order[`i'] == 2 & `i' < _N file write tex "\addlinespace" _n
}
file write tex "\hline" _n
file write tex "\end{tabular}" _n
file write tex "\par\vspace{1.5mm}" _n
file write tex "\begin{minipage}{\textwidth}" _n
file write tex "{\footnotesize Notes: The dependent variable is migration intention. Entries are linear probability model coefficients, with standard errors clustered at the municipality level and reported in parentheses. Each LPM uses exactly the estimation sample of the corresponding Logit specification and the same survey weights, year fixed effects, municipality fixed effects, and controls. Controls include age and male. The number of observations is `lpm_ps_n_no' without controls and `lpm_ps_n_yes' with controls. The unweighted percentage of fitted values outside [0,1] is: `lpm_ps_out_no'\% without controls and `lpm_ps_out_yes'\% with controls. * \(p<0.10\), ** \(p<0.05\), *** \(p<0.01\).}" _n
file write tex "\end{minipage}" _n
file write tex "\end{table}" _n
file close tex

* ---------------------------------------------------------------------------- *
* 25.9.2 Post x municipal characteristics
* Logit counterpart: logit_post_x_mfx_glm_cluster_se_stata.tex
* ---------------------------------------------------------------------------- *

use "$output_models/lpm_post_x_mfx_long_stata.dta", clear
quietly summarize nobs if controls == "No", meanonly
local lpm_px_n_no : display %12.0fc r(mean)
quietly summarize nobs if controls == "Yes", meanonly
local lpm_px_n_yes : display %12.0fc r(mean)
quietly summarize clusters, meanonly
local lpm_px_clusters : display %12.0fc r(max)

gen str20 b_fmt = string(estimate, "%9.3f") + ///
    cond(p_value < .01, "***", cond(p_value < .05, "**", cond(p_value < .10, "*", "")))
gen str20 se_fmt = "(" + string(se, "%9.3f") + ")"

preserve
    keep x_order variable variable_label control_order outside_pct
    reshape wide outside_pct, i(x_order variable variable_label) j(control_order)
    sort x_order
    local lpm_px_diag ""
    forvalues i = 1/`=_N' {
        local lab = variable_label[`i']
        local p1 : display %5.2f outside_pct1[`i']
        local p2 : display %5.2f outside_pct2[`i']
        local sep ""
        if `i' > 1 local sep "; "
        local lpm_px_diag "`lpm_px_diag'`sep'`lab' (`p1'\% / `p2'\%)"
    }
restore

sort x_order control_order
capture file close tex
file open tex using "$output_tex/lpm_post_x_mfx_glm_cluster_se_stata.tex", write replace text
file write tex "\begin{table}[!htbp]" _n
file write tex "\centering" _n
file write tex "\renewcommand{\arraystretch}{1.25}" _n
file write tex "\setlength{\tabcolsep}{6pt}" _n
file write tex "\caption{Linear probability model: post-period changes by municipal characteristics}" _n
file write tex "\label{tab:lpm_post_x_mfx_stata}" _n
file write tex "\small" _n
file write tex "\begin{tabular}{llc}" _n
file write tex "\hline" _n
file write tex "Municipal characteristic & Controls & LPM: \(Post \times X\) \\" _n
file write tex "\hline" _n
forvalues i = 1/`=_N' {
    local lab = variable_label[`i']
    local ctrl = controls[`i']
    local b = b_fmt[`i']
    local s = se_fmt[`i']
    file write tex "`lab' & `ctrl' & `b' \\" _n
    file write tex " & & `s' \\" _n
    if control_order[`i'] == 2 & `i' < _N file write tex "\addlinespace" _n
}
file write tex "\hline" _n
file write tex "\end{tabular}" _n
file write tex "\par\vspace{1.5mm}" _n
file write tex "\begin{minipage}{\textwidth}" _n
file write tex "{\footnotesize Notes: The dependent variable is migration intention. Each pair of rows reports a separate linear probability model interacting \(Post\) with the municipal characteristic in the first column. Standard errors are clustered at the municipality level. Each LPM uses exactly the corresponding Logit estimation sample and the same survey weights and fixed effects. Controls include age and male. The number of observations is `lpm_px_n_no' without controls and `lpm_px_n_yes' with controls. The unweighted percentage of fitted values outside [0,1], reported as No controls / Controls, is: `lpm_px_diag'. * \(p<0.10\), ** \(p<0.05\), *** \(p<0.01\).}" _n
file write tex "\end{minipage}" _n
file write tex "\end{table}" _n
file close tex

* ---------------------------------------------------------------------------- *
* 25.9.3 Triple differences: key terms from the two historical windows
* Logit counterpart: logit_triple_mfx_glm_cluster_se_stata.tex
* ---------------------------------------------------------------------------- *

use "$output_models/lpm_triple_all_terms_stata.dta", clear

preserve
    keep if term_order == 1
    keep x_order x x_label control_order outside_pct
    reshape wide outside_pct, i(x_order x x_label) j(control_order)
    sort x_order
    local lpm_tr_diag ""
    forvalues i = 1/`=_N' {
        local lab = x_label[`i']
        local p1 : display %5.2f outside_pct1[`i']
        local p2 : display %5.2f outside_pct2[`i']
        local sep ""
        if `i' > 1 local sep "; "
        local lpm_tr_diag "`lpm_tr_diag'`sep'`lab' (`p1'\% / `p2'\%)"
    }
restore

quietly summarize nobs if controls == "No", meanonly
local lpm_tr_n_no : display %12.0fc r(mean)
quietly summarize nobs if controls == "Yes", meanonly
local lpm_tr_n_yes : display %12.0fc r(mean)
quietly summarize clusters, meanonly
local lpm_tr_clusters : display %12.0fc r(max)

keep if inlist(term_order, 4, 5)
gen str3 window = "36" if term_order == 4
replace window = "56" if term_order == 5
keep x_order x x_label control_order controls window estimate se p_value
reshape wide estimate se p_value, i(x_order x x_label control_order controls) j(window) string
sort x_order control_order

gen str20 b36 = string(estimate36, "%9.3f") + ///
    cond(p_value36 < .01, "***", cond(p_value36 < .05, "**", cond(p_value36 < .10, "*", "")))
gen str20 s36 = "(" + string(se36, "%9.3f") + ")"
gen str20 b56 = string(estimate56, "%9.3f") + ///
    cond(p_value56 < .01, "***", cond(p_value56 < .05, "**", cond(p_value56 < .10, "*", "")))
gen str20 s56 = "(" + string(se56, "%9.3f") + ")"

capture file close tex
file open tex using "$output_tex/lpm_triple_mfx_glm_cluster_se_stata.tex", write replace text
file write tex "\begin{table}[!htbp]" _n
file write tex "\centering" _n
file write tex "\renewcommand{\arraystretch}{1.25}" _n
file write tex "\setlength{\tabcolsep}{6pt}" _n
file write tex "\caption{Linear probability model: triple differences by municipal characteristics}" _n
file write tex "\label{tab:lpm_triple_mfx_stata}" _n
file write tex "\small" _n
file write tex "\begin{tabular}{llcc}" _n
file write tex "\hline" _n
file write tex " & & \multicolumn{2}{c}{Spanish share \(\times\) Post \(\times X\): LPM coefficients} \\" _n
file write tex "Municipal characteristic & Controls & 1936--1955 & 1956--1978 \\" _n
file write tex "\hline" _n
forvalues i = 1/`=_N' {
    local lab = x_label[`i']
    local ctrl = controls[`i']
    local c36 = b36[`i']
    local e36 = s36[`i']
    local c56 = b56[`i']
    local e56 = s56[`i']
    file write tex "`lab' & `ctrl' & `c36' & `c56' \\" _n
    file write tex " & & `e36' & `e56' \\" _n
    if control_order[`i'] == 2 & `i' < _N file write tex "\addlinespace" _n
}
file write tex "\hline" _n
file write tex "\end{tabular}" _n
file write tex "\par\vspace{1.5mm}" _n
file write tex "\begin{minipage}{\textwidth}" _n
file write tex "\footnotesize Notes: The dependent variable is migration intention. Each pair of rows reports a separate triple-difference linear probability model. Standard errors are clustered at the municipality level. Each LPM uses exactly the corresponding Logit estimation sample, regressors, survey weights, and fixed effects. Specifications without controls include the two Spanish-share-by-Post terms, \(Post \times X\), and the two triple interactions. Controlled specifications additionally include age, male, and \(Post\) interacted with the remaining municipal characteristics. The number of observations is `lpm_tr_n_no' without controls and `lpm_tr_n_yes' with controls. The unweighted percentage of fitted values outside [0,1], reported as No controls / Controls, is: `lpm_tr_diag'. * \(p<0.10\), ** \(p<0.05\), *** \(p<0.01\)." _n
file write tex "\end{minipage}" _n
file write tex "\end{table}" _n
file close tex

* ---------------------------------------------------------------------------- *
* 25.9.4 Heterogeneity by municipal terciles
* Logit counterpart: migration_subsamples_lapop_stata.tex
* ---------------------------------------------------------------------------- *

use "$output_models/heterog_terciles_migration_lapop_lpm_stata.dta", clear

gen str20 f_b36 = string(b36, "%9.3f") + ///
    cond(p36 < .01, "***", cond(p36 < .05, "**", cond(p36 < .10, "*", "")))
gen str20 f_s36 = "(" + string(se36, "%9.3f") + ")"
gen str20 f_b56 = string(b56, "%9.3f") + ///
    cond(p56 < .01, "***", cond(p56 < .05, "**", cond(p56 < .10, "*", "")))
gen str20 f_s56 = "(" + string(se56, "%9.3f") + ")"
gen str20 f_peq = string(p_equal, "%9.3f")
gen str20 f_ub  = "\(" + string(tercile_ub, "%9.3f") + "\)"
gen str20 f_n   = string(nobs, "%12.0fc")

local lpm_het_diag ""
forvalues p = 1/8 {
    quietly levelsof characteristic if panel_order == `p', local(panel_lab) clean
    local pvals ""
    forvalues t = 1/3 {
        quietly summarize outside_pct if panel_order == `p' & tercile == `t', meanonly
        local pt : display %5.2f r(mean)
        if `t' == 1 local pvals "`pt'\%"
        if `t' > 1 local pvals "`pvals' / `pt'\%"
    }
    local sep ""
    if `p' > 1 local sep "; "
    local lpm_het_diag "`lpm_het_diag'`sep'`panel_lab' (`pvals')"
}

capture file close tex
file open tex using "$output/migration_subsamples_lapop_lpm_stata.tex", write replace text
file write tex "\begin{table}[!h]" _n
file write tex "\normalsize" _n
file write tex "\renewcommand{\arraystretch}{1.15}" _n
file write tex "\centering" _n
file write tex "\setlength{\tabcolsep}{6pt}" _n
file write tex "\captionsetup{justification=centering}" _n
file write tex "\caption{Heterogeneous effects on migration intention: linear probability models}" _n

local lpm_joint_text ""
forvalues p = 1/8 {
    quietly levelsof characteristic if panel_order == `p', local(group) clean

    file write tex "\begin{tabular*}{\textwidth}{l@{\extracolsep{\fill}}ccc}" _n
    if `p' == 1 file write tex "\hline" _n
    file write tex "& \multicolumn{3}{c}{`group'} \\" _n
    file write tex "\cmidrule(l){2-4}" _n
    file write tex "& T1 & T2 & T3 \\" _n
    file write tex "\hline" _n

    forvalues t = 1/3 {
        quietly levelsof f_b36 if panel_order == `p' & tercile == `t', local(b36`t') clean
        quietly levelsof f_s36 if panel_order == `p' & tercile == `t', local(s36`t') clean
        quietly levelsof f_b56 if panel_order == `p' & tercile == `t', local(b56`t') clean
        quietly levelsof f_s56 if panel_order == `p' & tercile == `t', local(s56`t') clean
        quietly levelsof f_n   if panel_order == `p' & tercile == `t', local(n`t') clean
        quietly levelsof f_peq if panel_order == `p' & tercile == `t', local(peq`t') clean
        quietly levelsof f_ub  if panel_order == `p' & tercile == `t', local(ub`t') clean
    }

    file write tex "Spanish share 1936--1955\(\times\)Post & `b361' & `b362' & `b363' \\" _n
    file write tex " & `s361' & `s362' & `s363' \\" _n
    file write tex "Spanish share 1956--1978\(\times\)Post & `b561' & `b562' & `b563' \\" _n
    file write tex " & `s561' & `s562' & `s563' \\" _n
    file write tex "\addlinespace" _n
    file write tex "Observations & `n1' & `n2' & `n3' \\" _n
    file write tex "\(p\)-value (\(\beta_{36{-}55}=\beta_{56{-}78}\)) & `peq1' & `peq2' & `peq3' \\" _n
    file write tex "Tercile upper bound & `ub1' & `ub2' & `ub3' \\" _n
    file write tex "\hline" _n
    file write tex "\end{tabular*}" _n

    quietly summarize p_joint_36 if panel_order == `p', meanonly
    local j36 : display %5.3f r(mean)
    quietly summarize p_joint_56 if panel_order == `p', meanonly
    local j56 : display %5.3f r(mean)

    if `p' == 1 local lpm_joint_text "`group' (`j36' / `j56')"
    if `p' > 1 local lpm_joint_text "`lpm_joint_text'; `group' (`j36' / `j56')"
}

file write tex "\addvspace{0.3em}" _n
file write tex "\captionsetup{font=footnotesize, justification=justified, singlelinecheck=false}" _n
file write tex "\caption*{\footnotesize Notes: The dependent variable is migration intention. Each set of columns reports LPM estimates by tercile of the specified municipal characteristic. Every LPM uses exactly the corresponding Logit estimation sample and the same regressors, survey weights, year fixed effects, municipality fixed effects, and controls for age and male. Standard errors clustered by municipality are in parentheses. Joint Wald-test p-values, reported as 1936--1955 / 1956--1978, are: `lpm_joint_text'. The unweighted percentage of fitted values outside [0,1], reported as T1 / T2 / T3, is: `lpm_het_diag'. * \(p<0.10\), ** \(p<0.05\), *** \(p<0.01\).}" _n
file write tex "\end{table}" _n
file close tex

* ---------------------------------------------------------------------------- *
* 25.9.5 Individual characteristics
* Logit counterpart: individual_characteristics_logit_ame_stata.tex
* ---------------------------------------------------------------------------- *

use "$output_models/individual_characteristics_lpm_stata.dta", clear

forvalues s = 1/3 {
    quietly summarize nobs if spec_order == `s', meanonly
    local lpm_ind_n_`s' : display %12.0fc r(mean)
    quietly summarize outside_pct if spec_order == `s', meanonly
    local lpm_ind_out_`s' : display %5.2f r(mean)
}
quietly summarize clusters, meanonly
local lpm_ind_clusters : display %12.0fc r(max)

keep var_order variable variable_label spec_order estimate se p_value
reshape wide estimate se p_value, i(var_order variable variable_label) j(spec_order)
sort var_order

forvalues s = 1/3 {
    gen str20 b`s' = cond(missing(estimate`s'), "", ///
        string(estimate`s', "%9.3f") + ///
        cond(p_value`s' < .01, "***", cond(p_value`s' < .05, "**", cond(p_value`s' < .10, "*", ""))))
    gen str20 e`s' = cond(missing(se`s'), "", "(" + string(se`s', "%9.3f") + ")")
}

capture file close tex
file open tex using "$output_tex/individual_characteristics_lpm_stata.tex", write replace text
file write tex "\begin{table}[htbp]" _n
file write tex "\normalsize" _n
file write tex "\renewcommand{\arraystretch}{1.25}" _n
file write tex "\setlength{\tabcolsep}{6pt}" _n
file write tex "\centering" _n
file write tex "\caption{Individual characteristics and migration intention: linear probability models}" _n
file write tex "\label{tab:individual_characteristics_lpm_stata}" _n
file write tex "\setlength{\tabcolsep}{4pt}" _n
file write tex "\captionsetup{justification=centering}" _n
file write tex "\begin{tabular}{lccc}" _n
file write tex "\hline" _n
file write tex " & (1) & (2) & (3) \\" _n
file write tex "\hline" _n
forvalues i = 1/`=_N' {
    local lab = variable_label[`i']
    local c1 = b1[`i']
    local c2 = b2[`i']
    local c3 = b3[`i']
    local s1 = e1[`i']
    local s2 = e2[`i']
    local s3 = e3[`i']
    file write tex "`lab' & `c1' & `c2' & `c3' \\" _n
    file write tex " & `s1' & `s2' & `s3' \\" _n
}
file write tex "\hline" _n
file write tex "Observations & `lpm_ind_n_1' & `lpm_ind_n_2' & `lpm_ind_n_3' \\" _n
file write tex "Year FE & Yes & Yes & Yes \\" _n
file write tex "Municipality FE & Yes & Yes & Yes \\" _n
file write tex "Survey weights & Yes & Yes & Yes \\" _n
file write tex "Blank/null vote included & Yes & No & Yes \\" _n
file write tex "Ideology included & Yes & Yes & No \\" _n
file write tex "\hline" _n
file write tex "\end{tabular}" _n
file write tex "\par\vspace{1.5mm}" _n
file write tex "\begin{minipage}{0.98\linewidth}" _n
file write tex "\footnotesize\setlength{\parindent}{0pt}\setlength{\parskip}{1pt}" _n
file write tex "Notes: The dependent variable is migration intention. Entries are LPM coefficients, with standard errors clustered at the municipality level and reported in parentheses. Each model uses exactly the estimation sample of its corresponding Logit specification and the same survey weights and fixed effects. The unweighted percentages of fitted values outside [0,1] are `lpm_ind_out_1'\%, `lpm_ind_out_2'\%, and `lpm_ind_out_3'\% in columns (1), (2), and (3), respectively. * \(p<0.10\), ** \(p<0.05\), *** \(p<0.01\)." _n
file write tex "\end{minipage}" _n
file write tex "\end{table}" _n
file close tex

* ---------------------------------------------------------------------------- *
* 25.9.6 Two-window triple differences: all interaction terms
* Logit counterpart: triple_all_interactions_logit_ame_stata.tex
* ---------------------------------------------------------------------------- *

use "$output_models/lpm_triple_all_terms_stata.dta", clear

quietly summarize nobs if controls == "No", meanonly
local lpm_all_n_no : display %12.0fc r(mean)
quietly summarize nobs if controls == "Yes", meanonly
local lpm_all_n_yes : display %12.0fc r(mean)
quietly summarize clusters, meanonly
local lpm_all_clusters : display %12.0fc r(max)

keep x_order x x_label term_order control_order estimate se p_value
reshape wide estimate se p_value, i(x_order x x_label term_order) j(control_order)
sort x_order term_order

gen str50 cell1 = string(estimate1, "%9.3f") + ///
    cond(p_value1 < .01, "***", cond(p_value1 < .05, "**", cond(p_value1 < .10, "*", ""))) + ///
    " (" + string(se1, "%9.3f") + ")"
gen str50 cell2 = string(estimate2, "%9.3f") + ///
    cond(p_value2 < .01, "***", cond(p_value2 < .05, "**", cond(p_value2 < .10, "*", ""))) + ///
    " (" + string(se2, "%9.3f") + ")"

capture file close tex
file open tex using "$output_tex/triple_all_interactions_lpm_stata.tex", write replace text
file write tex "\begin{table}[!htbp]" _n
file write tex "\small" _n
file write tex "\renewcommand{\arraystretch}{1}" _n
file write tex "\setlength{\tabcolsep}{3pt}" _n
file write tex "\centering" _n
file write tex "\caption{Triple-difference models: all interaction terms, linear probability models}" _n
file write tex "\label{tab:triple_all_interactions_lpm_stata}" _n
file write tex "\captionsetup{justification=centering}" _n
file write tex "\begin{tabular}{llcc}" _n
file write tex "\hline" _n
file write tex "Municipal characteristic & Term & No controls & Controls \\" _n
file write tex "\hline" _n
forvalues i = 1/`=_N' {
    local xprint = x_label[`i']
    if `i' > 1 & x_order[`i'] == x_order[`i'-1] local xprint ""

    local term "Spanish share 1936--1955 \(\times\) Post"
    if term_order[`i'] == 2 local term "Spanish share 1956--1978 \(\times\) Post"
    if term_order[`i'] == 3 local term "\(Post \times X\)"
    if term_order[`i'] == 4 local term "Spanish share 1936--1955 \(\times\) Post \(\times X\)"
    if term_order[`i'] == 5 local term "Spanish share 1956--1978 \(\times\) Post \(\times X\)"

    local c1 = cell1[`i']
    local c2 = cell2[`i']
    file write tex "`xprint' & `term' & `c1' & `c2' \\" _n
    if term_order[`i'] == 5 & `i' < _N file write tex "\addlinespace" _n
}
file write tex "\hline" _n
file write tex "\end{tabular}" _n
file write tex "\par\vspace{1.5mm}" _n
file write tex "\begin{minipage}{0.98\linewidth}" _n
file write tex "\footnotesize\setlength{\parindent}{0pt}\setlength{\parskip}{1pt}" _n
file write tex "Notes: The dependent variable is migration intention. Each block reports a separate triple-difference LPM. Standard errors are reported in parentheses beside coefficients and clustered at the municipality level. Every LPM uses exactly the corresponding Logit estimation sample, regressors, survey weights, and fixed effects. Controls additionally include age, male, and \(Post\) interacted with the remaining municipal characteristics. Lower-order interactions are conditional on the other interacted variables being zero. The number of observations is `lpm_all_n_no' without controls and `lpm_all_n_yes' with controls. The unweighted percentage of fitted values outside [0,1], reported as No controls / Controls, is: `lpm_tr_diag'. * \(p<0.10\), ** \(p<0.05\), *** \(p<0.01\)." _n
file write tex "\end{minipage}" _n
file write tex "\end{table}" _n
file close tex

* ---------------------------------------------------------------------------- *
* 25.9.7 Pooled Spanish exposure x Post
* Logit counterpart: post_pooled_share_logit_ame_stata.tex
* ---------------------------------------------------------------------------- *

use "$output_models/post_pooled_share_lpm_stata.dta", clear
gen byte model_order = (exposure_order - 1) * 2 + control_order
gen str20 b_fmt = string(estimate, "%9.3f") + ///
    cond(p_value < .01, "***", cond(p_value < .05, "**", cond(p_value < .10, "*", "")))
gen str20 se_fmt = "(" + string(se, "%9.3f") + ")"
sort model_order

quietly summarize clusters, meanonly
local lpm_poolpost_clusters : display %12.0fc r(max)
forvalues m = 1/4 {
    local lpm_poolpost_b_`m' = b_fmt[`m']
    local lpm_poolpost_se_`m' = se_fmt[`m']
    local lpm_poolpost_n_`m' : display %12.0fc nobs[`m']
    local lpm_poolpost_out_`m' : display %5.2f outside_pct[`m']
}

capture file close tex
file open tex using "$output_tex/post_pooled_share_lpm_stata.tex", write replace text
file write tex "\begin{table}[!htbp]" _n
file write tex "\normalsize" _n
file write tex "\renewcommand{\arraystretch}{1.15}" _n
file write tex "\centering" _n
file write tex "\caption{Effect on migration intentions: pooled Spanish exposure, linear probability models}" _n
file write tex "\label{tab:post_pooled_share_lpm_stata}" _n
file write tex "\setlength{\tabcolsep}{4pt}" _n
file write tex "\captionsetup{justification=centering}" _n
file write tex "\begin{tabular}{lcccc}" _n
file write tex "\hline" _n
file write tex " & (1) & (2) & (3) & (4) \\" _n
file write tex "\hline" _n
file write tex "Spanish share 1936--1978 \(\times\) Post & `lpm_poolpost_b_1' & `lpm_poolpost_b_2' & `lpm_poolpost_b_3' & `lpm_poolpost_b_4' \\" _n
file write tex " & `lpm_poolpost_se_1' & `lpm_poolpost_se_2' & `lpm_poolpost_se_3' & `lpm_poolpost_se_4' \\" _n
file write tex "\hline" _n
file write tex "Observations & `lpm_poolpost_n_1' & `lpm_poolpost_n_2' & `lpm_poolpost_n_3' & `lpm_poolpost_n_4' \\" _n
file write tex "Year FE & Yes & Yes & Yes & Yes \\" _n
file write tex "Municipality FE & Yes & Yes & Yes & Yes \\" _n
file write tex "Controls & No & Yes & No & Yes \\" _n
file write tex "Spanish share census & 1970 & 1970 & 1980 & 1980 \\" _n
file write tex "\hline" _n
file write tex "\end{tabular}" _n
file write tex "\par\vspace{1.5mm}" _n
file write tex "\begin{minipage}{0.98\linewidth}" _n
file write tex "\footnotesize\setlength{\parindent}{0pt}\setlength{\parskip}{1pt}" _n
file write tex "Notes: The dependent variable is migration intention. Each column reports an LPM using exactly the corresponding Logit estimation sample and the same survey weights, year fixed effects, municipality fixed effects, controls, and standard errors clustered at the municipality level. Controls include age and male. The unweighted percentages of fitted values outside [0,1] are `lpm_poolpost_out_1'\%, `lpm_poolpost_out_2'\%, `lpm_poolpost_out_3'\%, and `lpm_poolpost_out_4'\% in columns (1)--(4), respectively. * \(p<0.10\), ** \(p<0.05\), *** \(p<0.01\)." _n
file write tex "\end{minipage}" _n
file write tex "\end{table}" _n
file close tex

* ---------------------------------------------------------------------------- *
* 25.9.8 Pooled Spanish exposure: key triple-difference terms
* Logit counterpart: triple_pooled_share_logit_ame_stata.tex
* ---------------------------------------------------------------------------- *

use "$output_models/triple_pooled_all_terms_lpm_stata.dta", clear

preserve
    keep if term_order == 1
    keep exposure_order x_order x x_label control_order controls outside_pct
    reshape wide outside_pct, i(x_order x x_label control_order controls) j(exposure_order)
    sort x_order control_order
    local lpm_pooltr_diag ""
    forvalues i = 1/`=_N' {
        local lab = x_label[`i']
        local ctrl = controls[`i']
        local p70 : display %5.2f outside_pct1[`i']
        local p80 : display %5.2f outside_pct2[`i']
        local sep ""
        if `i' > 1 local sep "; "
        local lpm_pooltr_diag "`lpm_pooltr_diag'`sep'`lab', `ctrl' (`p70'\% / `p80'\%)"
    }
restore

quietly summarize nobs if controls == "No", meanonly
local lpm_pooltr_n_no : display %12.0fc r(mean)
quietly summarize nobs if controls == "Yes", meanonly
local lpm_pooltr_n_yes : display %12.0fc r(mean)
quietly summarize clusters, meanonly
local lpm_pooltr_clusters : display %12.0fc r(max)

keep if term_order == 3
gen str20 b = string(estimate, "%9.3f") + ///
    cond(p_value < .01, "***", cond(p_value < .05, "**", cond(p_value < .10, "*", "")))
gen str20 e = "(" + string(se, "%9.3f") + ")"
keep x_order x x_label control_order controls exposure_order b e
reshape wide b e, i(x_order x x_label control_order controls) j(exposure_order)
sort x_order control_order

capture file close tex
file open tex using "$output_tex/triple_pooled_share_lpm_stata.tex", write replace text
file write tex "\begin{table}[!htbp]" _n
file write tex "\normalsize" _n
file write tex "\renewcommand{\arraystretch}{1.25}" _n
file write tex "\setlength{\tabcolsep}{6pt}" _n
file write tex "\centering" _n
file write tex "\caption{Triple differences: pooled Spanish exposure, linear probability models}" _n
file write tex "\label{tab:triple_pooled_share_lpm_stata}" _n
file write tex "\setlength{\tabcolsep}{4pt}" _n
file write tex "\captionsetup{justification=centering}" _n
file write tex "\begin{tabular}{llcc}" _n
file write tex "\hline" _n
file write tex "Municipal characteristic & Controls & 1970 Census & 1980 Census \\" _n
file write tex "\hline" _n
forvalues i = 1/`=_N' {
    local lab = x_label[`i']
    local ctrl = controls[`i']
    local c70 = b1[`i']
    local s70 = e1[`i']
    local c80 = b2[`i']
    local s80 = e2[`i']
    file write tex "`lab' & `ctrl' & `c70' & `c80' \\" _n
    file write tex " & & `s70' & `s80' \\" _n
    if control_order[`i'] == 2 & `i' < _N file write tex "\addlinespace" _n
}
file write tex "\hline" _n
file write tex "\end{tabular}" _n
file write tex "\par\vspace{1.5mm}" _n
file write tex "\begin{minipage}{0.98\linewidth}" _n
file write tex "\footnotesize\setlength{\parindent}{0pt}\setlength{\parskip}{1pt}" _n
file write tex "Notes: The dependent variable is migration intention. Each pair of rows reports a separate pooled triple-difference LPM. Every LPM uses exactly the corresponding Logit estimation sample and the same regressors, survey weights, fixed effects, controls, and standard errors clustered at the municipality level. Controls include age, male, and \(Post\) interacted with the remaining municipal characteristics. The number of observations is `lpm_pooltr_n_no' without controls and `lpm_pooltr_n_yes' with controls. The unweighted percentage of fitted values outside [0,1], reported as 1970 Census / 1980 Census, is: `lpm_pooltr_diag'. * \(p<0.10\), ** \(p<0.05\), *** \(p<0.01\)." _n
file write tex "\end{minipage}" _n
file write tex "\end{table}" _n
file close tex

* ---------------------------------------------------------------------------- *
* 25.9.9 Pooled Spanish exposure: all interaction terms, by census
* Logit counterparts: triple_pooled_all_interactions_c70/c80_logit_ame_stata.tex
* ---------------------------------------------------------------------------- *

foreach eorder in 1 2 {
    local exposure "c70"
    local etitle "Spanish presence from 1970 Census"
    local census_year "1970"
    local remaining_controls "remaining common census/electoral municipal characteristics"
    if `eorder' == 2 {
        local exposure "c80"
        local etitle "Spanish presence from 1980 Census"
        local census_year "1980"
        local remaining_controls "remaining pre-2022 municipal characteristics"
    }

    use "$output_models/triple_pooled_all_terms_lpm_stata.dta", clear
    keep if exposure_order == `eorder'

    quietly summarize nobs if controls == "No", meanonly
    local lpm_poolall_n_no : display %12.0fc r(mean)
    quietly summarize nobs if controls == "Yes", meanonly
    local lpm_poolall_n_yes : display %12.0fc r(mean)
    quietly summarize clusters, meanonly
    local lpm_poolall_clusters : display %12.0fc r(max)

    preserve
        keep if term_order == 1
        keep x_order x x_label control_order outside_pct
        reshape wide outside_pct, i(x_order x x_label) j(control_order)
        sort x_order
        local lpm_poolall_diag ""
        forvalues i = 1/`=_N' {
            local lab = x_label[`i']
            local p1 : display %5.2f outside_pct1[`i']
            local p2 : display %5.2f outside_pct2[`i']
            local sep ""
            if `i' > 1 local sep "; "
            local lpm_poolall_diag "`lpm_poolall_diag'`sep'`lab' (`p1'\% / `p2'\%)"
        }
    restore

    keep x_order x x_label term_order control_order estimate se p_value
    reshape wide estimate se p_value, i(x_order x x_label term_order) j(control_order)
    sort x_order term_order

    gen str50 cell1 = string(estimate1, "%9.3f") + ///
        cond(p_value1 < .01, "***", cond(p_value1 < .05, "**", cond(p_value1 < .10, "*", ""))) + ///
        " (" + string(se1, "%9.3f") + ")"
    gen str50 cell2 = string(estimate2, "%9.3f") + ///
        cond(p_value2 < .01, "***", cond(p_value2 < .05, "**", cond(p_value2 < .10, "*", ""))) + ///
        " (" + string(se2, "%9.3f") + ")"

    capture file close tex
    file open tex using "$output_tex/triple_pooled_all_interactions_`exposure'_lpm_stata.tex", write replace text
    file write tex "\begin{table}[!htbp]" _n
    file write tex "\small" _n
    file write tex "\renewcommand{\arraystretch}{1}" _n
    file write tex "\setlength{\tabcolsep}{3pt}" _n
    file write tex "\centering" _n
    file write tex "\caption{Triple-difference models: all interaction terms, `etitle', linear probability models}" _n
    file write tex "\label{tab:triple_pooled_all_`exposure'_lpm_stata}" _n
    file write tex "\captionsetup{justification=centering}" _n
    file write tex "\begin{tabular}{llcc}" _n
    file write tex "\hline" _n
    file write tex "Municipal characteristic & Term & No controls & Controls \\" _n
    file write tex "\hline" _n
    forvalues i = 1/`=_N' {
        local xprint = x_label[`i']
        if `i' > 1 & x_order[`i'] == x_order[`i'-1] local xprint ""
        local term "Spanish share 1936--1978 \(\times\) Post"
        if term_order[`i'] == 2 local term "\(Post \times X\)"
        if term_order[`i'] == 3 local term "Spanish share 1936--1978 \(\times\) Post \(\times X\)"
        local c1 = cell1[`i']
        local c2 = cell2[`i']
        file write tex "`xprint' & `term' & `c1' & `c2' \\" _n
        if term_order[`i'] == 3 & `i' < _N file write tex "\addlinespace" _n
    }
    file write tex "\hline" _n
    file write tex "\end{tabular}" _n
    file write tex "\par\vspace{1.5mm}" _n
    file write tex "\begin{minipage}{0.98\linewidth}" _n
    file write tex "\footnotesize\setlength{\parindent}{0pt}\setlength{\parskip}{1pt}" _n
    file write tex "Notes: The dependent variable is migration intention. Each block reports a separate pooled triple-difference LPM. Coefficients and standard errors clustered at the municipality level are reported in the same cell. Spanish share is measured using the `census_year' census. Every LPM uses exactly the corresponding Logit estimation sample, regressors, survey weights, and fixed effects. Controls additionally include age, male, and \(Post\) interacted with the `remaining_controls'. The number of observations is `lpm_poolall_n_no' without controls and `lpm_poolall_n_yes' with controls. The unweighted percentage of fitted values outside [0,1], reported as No controls / Controls, is: `lpm_poolall_diag'. * \(p<0.10\), ** \(p<0.05\), *** \(p<0.01\)." _n
    file write tex "\end{minipage}" _n
    file write tex "\end{table}" _n
    file close tex
}

* ---------------------------------------------------------------------------- *
* 25.9.10 Pooled-exposure pre-trends
* Logit counterpart: pooled_pretrends_logit_ame_stata.tex
* ---------------------------------------------------------------------------- *

use "$output_models/pooled_pretrends_lpm_stata.dta", clear
keep term_order year_effect model_order estimate se p_value nobs clusters outside_pct
reshape wide estimate se p_value nobs clusters outside_pct, i(term_order year_effect) j(model_order)
sort term_order

forvalues m = 1/4 {
    gen str20 b`m' = string(estimate`m', "%9.3f") + ///
        cond(p_value`m' < .01, "***", cond(p_value`m' < .05, "**", cond(p_value`m' < .10, "*", "")))
    gen str20 e`m' = "(" + string(se`m', "%9.3f") + ")"
    local lpm_pre_n_`m' : display %12.0fc nobs`m'[1]
    local lpm_pre_out_`m' : display %5.2f outside_pct`m'[1]
}
quietly summarize clusters1, meanonly
local lpm_pre_clusters : display %12.0fc r(max)

capture file close tex
file open tex using "$output_tex/pooled_pretrends_lpm_stata.tex", write replace text
file write tex "\begin{table}[!htbp]" _n
file write tex "\normalsize" _n
file write tex "\renewcommand{\arraystretch}{1.25}" _n
file write tex "\setlength{\tabcolsep}{6pt}" _n
file write tex "\centering" _n
file write tex "\caption{Pre-trends in migration intention: pooled Spanish exposure, linear probability models}" _n
file write tex "\label{tab:pooled_pretrends_lpm_stata}" _n
file write tex "\setlength{\tabcolsep}{4pt}" _n
file write tex "\captionsetup{justification=centering}" _n
file write tex "\begin{tabular}{lcccc}" _n
file write tex "\hline" _n
file write tex " & (1) & (2) & (3) & (4) \\" _n
file write tex "\hline" _n
forvalues i = 1/`=_N' {
    local y = year_effect[`i']
    local c1 = b1[`i']
    local c2 = b2[`i']
    local c3 = b3[`i']
    local c4 = b4[`i']
    local s1 = e1[`i']
    local s2 = e2[`i']
    local s3 = e3[`i']
    local s4 = e4[`i']
    file write tex "Spanish share 1936--1978 \(\times\) `y' & `c1' & `c2' & `c3' & `c4' \\" _n
    file write tex " & `s1' & `s2' & `s3' & `s4' \\" _n
}
file write tex "\hline" _n
file write tex "Observations & `lpm_pre_n_1' & `lpm_pre_n_2' & `lpm_pre_n_3' & `lpm_pre_n_4' \\" _n
file write tex "Municipality FE & Yes & Yes & Yes & Yes \\" _n
file write tex "Year FE & Yes & Yes & Yes & Yes \\" _n
file write tex "Controls & No & No & Yes & Yes \\" _n
file write tex "Spanish share census & 1970 & 1980 & 1970 & 1980 \\" _n
file write tex "\hline" _n
file write tex "\end{tabular}" _n
file write tex "\par\vspace{1.5mm}" _n
file write tex "\begin{minipage}{0.98\linewidth}" _n
file write tex "\footnotesize\setlength{\parindent}{0pt}\setlength{\parskip}{1pt}" _n
file write tex "Notes: The dependent variable is migration intention. Entries are LPM coefficients relative to 2019. Every LPM uses exactly the corresponding Logit estimation sample and the same survey weights, year fixed effects, municipality fixed effects, controls, and standard errors clustered at the municipality level. Controls include age and male. The unweighted percentages of fitted values outside [0,1] are `lpm_pre_out_1'\%, `lpm_pre_out_2'\%, `lpm_pre_out_3'\%, and `lpm_pre_out_4'\% in columns (1)--(4), respectively. * \(p<0.10\), ** \(p<0.05\), *** \(p<0.01\)." _n
file write tex "\end{minipage}" _n
file write tex "\end{table}" _n
file close tex

* ---------------------------------------------------------------------------- *
* 25.10 Final message
* ---------------------------------------------------------------------------- *

noi display as result _newline "Parallel LPM appendix tables completed."
noi display as result "The existing Logit outputs were not modified."
noi display as result "Created:"
noi display as result "  $output_tex/lpm_post_share_mfx_glm_cluster_se_stata.tex"
noi display as result "  $output_tex/lpm_post_x_mfx_glm_cluster_se_stata.tex"
noi display as result "  $output_tex/lpm_triple_mfx_glm_cluster_se_stata.tex"
noi display as result "  $output/migration_subsamples_lapop_lpm_stata.tex"
noi display as result "  $output_tex/individual_characteristics_lpm_stata.tex"
noi display as result "  $output_tex/triple_all_interactions_lpm_stata.tex"
noi display as result "  $output_tex/post_pooled_share_lpm_stata.tex"
noi display as result "  $output_tex/triple_pooled_share_lpm_stata.tex"
noi display as result "  $output_tex/triple_pooled_all_interactions_c70_lpm_stata.tex"
noi display as result "  $output_tex/triple_pooled_all_interactions_c80_lpm_stata.tex"
noi display as result "  $output_tex/pooled_pretrends_lpm_stata.tex"


* ---------------------------------------------------------------------------- *
* TESTS DE IGUALDAD ENTRE VENTANAS PARA TABLE A11
* ---------------------------------------------------------------------------- *

preserve

* Volver a cargar la base utilizada para estimar Table A11
use "$data_int/lapop_logit_mfx_ready_LPM_appendix.dta", clear

foreach ctrl_label in "No" "Yes" {

    local ctrls ""

    if "`ctrl_label'" == "Yes" {
        local ctrls "edad hombre"
    }

    noi display _newline
    noi display as text ///
        "============================================================"

    noi display as text ///
        "Table A11 - Controls: `ctrl_label'"

    noi display as text ///
        "============================================================"


    * ------------------------------------------------------------------------ *
    * Mismo Logit utilizado en Table A11
    * ------------------------------------------------------------------------ *

    quietly logit intencion_migrar ///
        pshare36 pshare56 ///
        `ctrls' ///
        i.year_num i.mun_code_num ///
        [pw=wt], ///
        vce(cluster mun_code_num)

    local N = e(N)


    * ------------------------------------------------------------------------ *
    * TEST 1: igualdad entre coeficientes Logit
    *
    * H0: beta_1936-1955 = beta_1956-1978
    * Este es el test consistente con Table A1.
    * ------------------------------------------------------------------------ *

    quietly lincom pshare36 - pshare56

    local diff_beta = r(estimate)
    local se_beta   = r(se)
    local p_beta    = r(p)

    noi display as result ///
        "Wald test on underlying Logit coefficients"

    noi display as text ///
        "H0: beta_1936-1955 = beta_1956-1978"

    noi display as text ///
        "Difference: " as result %9.4f `diff_beta'

    noi display as text ///
        "Clustered SE of difference: " as result %9.4f `se_beta'

    noi display as text ///
        "p-value: " as result %9.4f `p_beta'


    * ------------------------------------------------------------------------ *
    * TEST 2: igualdad entre average marginal effects
    *
    * H0: AME_1936-1955 = AME_1956-1978
    * Se muestra como comprobacion adicional.
    * ------------------------------------------------------------------------ *

    quietly margins, dydx(pshare36 pshare56)

    tempname B_ame V_ame
    matrix `B_ame' = r(b)
    matrix `V_ame' = r(V)

    local ame36 = el(`B_ame', 1, 1)
    local ame56 = el(`B_ame', 1, 2)

    local diff_ame = ///
        `ame36' - `ame56'

    local se_diff_ame = sqrt( ///
        el(`V_ame', 1, 1) + ///
        el(`V_ame', 2, 2) - ///
        2 * el(`V_ame', 1, 2) ///
    )

    local z_ame = ///
        `diff_ame' / `se_diff_ame'

    local p_ame = ///
        2 * normal(-abs(`z_ame'))

    noi display _newline

    noi display as result ///
        "Wald test on average marginal effects"

    noi display as text ///
        "H0: AME_1936-1955 = AME_1956-1978"

    noi display as text ///
        "AME 1936-1955: " as result %9.4f `ame36'

    noi display as text ///
        "AME 1956-1978: " as result %9.4f `ame56'

    noi display as text ///
        "Difference: " as result %9.4f `diff_ame'

    noi display as text ///
        "Delta-method SE of difference: " ///
        as result %9.4f `se_diff_ame'

    noi display as text ///
        "p-value: " as result %9.4f `p_ame'

    noi display as text ///
        "Observations: " as result %12.0fc `N'
}

restore

/*
* ============================================================================ *
* TESTS PARA TABLE A21
* Spanish presence from 1980 Census
*
* Test 1:
* H0: beta(Post x Share) = beta(Post x Share x X)
*
* Test 2:
* H0: beta(Post x Share) + beta(Post x Share x X) = 0
*
* Significance:
* * p<0.10, ** p<0.05, *** p<0.01
* ============================================================================ *

preserve

* Base utilizada para las tablas A18-A21
use "$data_int/lapop_remaining_mixed_tables_ready.dta", clear


* Caracteristicas municipales incluidas en Table A21
local common_x_vars ///
    popdensgeo2_2010 ///
    share_female_2010 ///
    mean_yrschool_2010 ///
    median_age_2010 ///
    share_laborforce_2010 ///
    share_unemployed_2010 ///
    share_izq_amplia_pre_avg ///
    share_alt_pre_avg


* Table A21 utiliza la exposicion pooled del Censo 1980
local pterm "pp80"


foreach x of local common_x_vars {

    * Etiquetas para la consola
    local xlabel "`x'"

    if "`x'" == "popdensgeo2_2010" {
        local xlabel "Population density"
    }

    if "`x'" == "share_female_2010" {
        local xlabel "Female population share"
    }

    if "`x'" == "mean_yrschool_2010" {
        local xlabel "Mean years of education"
    }

    if "`x'" == "median_age_2010" {
        local xlabel "Median age"
    }

    if "`x'" == "share_laborforce_2010" {
        local xlabel "Share in labor force"
    }

    if "`x'" == "share_unemployed_2010" {
        local xlabel "Share unemployed"
    }

    if "`x'" == "share_izq_amplia_pre_avg" {
        local xlabel "Left vote share"
    }

    if "`x'" == "share_alt_pre_avg" {
        local xlabel "Ideological alternation"
    }


    * Triple interaccion correspondiente a X
    local triplevar "ppx80_`x'"


    foreach ctrl_label in "No" "Yes" {

        local rhs_extra ""

        * Misma estructura de controles utilizada en Table A21:
        * edad, hombre y Post x las restantes caracteristicas municipales
        if "`ctrl_label'" == "Yes" {

            local rhs_extra "edad hombre"

            foreach z of local common_x_vars {

                if "`z'" != "`x'" {
                    local rhs_extra ///
                        "`rhs_extra' px_`z'"
                }
            }
        }


        * -------------------------------------------------------------------- *
        * Reestimar exactamente la especificacion de Table A21
        * -------------------------------------------------------------------- *

        quietly logit intencion_migrar ///
            `pterm' ///
            px_`x' ///
            `triplevar' ///
            `rhs_extra' ///
            i.year_num i.mun_code_num ///
            [pw=wt], ///
            vce(cluster mun_code_num)

        local N = e(N)

        * Guardar los dos coeficientes individuales
        local beta_post_share = _b[`pterm']
        local beta_triple     = _b[`triplevar']


        * -------------------------------------------------------------------- *
        * TEST 1
        *
        * H0:
        * beta(Post x Share) = beta(Post x Share x X)
        * -------------------------------------------------------------------- *

        quietly lincom ///
            `pterm' - `triplevar'

        local difference_equal = r(estimate)
        local se_equal         = r(se)
        local p_equal          = r(p)


        * Estrellas del Test 1
        local star_equal ""

        if `p_equal' < 0.10 {
            local star_equal "*"
        }

        if `p_equal' < 0.05 {
            local star_equal "**"
        }

        if `p_equal' < 0.01 {
            local star_equal "***"
        }


        * -------------------------------------------------------------------- *
        * TEST 2
        *
        * H0:
        * beta(Post x Share) + beta(Post x Share x X) = 0
        * -------------------------------------------------------------------- *

        quietly lincom ///
            `pterm' + `triplevar'

        local additive_effect = r(estimate)
        local se_additive     = r(se)
        local p_additive      = r(p)


        * Estrellas del Test 2
        local star_additive ""

        if `p_additive' < 0.10 {
            local star_additive "*"
        }

        if `p_additive' < 0.05 {
            local star_additive "**"
        }

        if `p_additive' < 0.01 {
            local star_additive "***"
        }


        * -------------------------------------------------------------------- *
        * Mostrar resultados en la consola
        * -------------------------------------------------------------------- *

        noi display _newline
        noi display as text ///
            "============================================================"

        noi display as text ///
            "TABLE A21"

        noi display as text ///
            "Municipal characteristic: `xlabel'"

        noi display as text ///
            "Controls: `ctrl_label'"

        noi display as text ///
            "============================================================"


        noi display as text ///
            "Underlying Logit coefficients:"

        noi display as text ///
            "  beta(Post x Share): " ///
            as result %10.5f `beta_post_share'

        noi display as text ///
            "  beta(Post x Share x X): " ///
            as result %10.5f `beta_triple'


        noi display _newline

        noi display as result ///
            "TEST 1: Equality between treatment coefficients"

        noi display as text ///
            "H0: beta(Post x Share) = beta(Post x Share x X)"

        noi display as text ///
            "Difference: " ///
            as result %10.5f `difference_equal'

        noi display as text ///
            "Clustered SE: " ///
            as result %10.5f `se_equal'

        noi display as text ///
            "p-value: " ///
            as result %5.3f `p_equal' ///
            as result "`star_equal'"


        noi display _newline

        noi display as result ///
            "TEST 2: Additive treatment effect equals zero"

        noi display as text ///
            "H0: beta(Post x Share) + beta(Post x Share x X) = 0"

        noi display as text ///
            "Sum of coefficients: " ///
            as result %10.5f `additive_effect'

        noi display as text ///
            "Clustered SE: " ///
            as result %10.5f `se_additive'

        noi display as text ///
            "p-value: " ///
            as result %5.3f `p_additive' ///
            as result "`star_additive'"

        noi display as text ///
            "Observations: " ///
            as result %12.0fc `N'
    }
}


noi display _newline
noi display as text ///
    "Significance levels: * p<0.10, ** p<0.05, *** p<0.01"

restore
*/

* ============================================================================ *
* WALD TESTS FOR TABLE A14
*
* Test 1:
* H0: beta(Post x Share 1936-1955)
*     = beta(Post x Share 1956-1978)
*
* Test 2:
* H0: beta(Post x Share 1936-1955)
*     + beta(Post x Share 1956-1978) = 0
*
* Tests are conducted on the underlying Logit coefficients.
*
* Significance levels:
* * p<0.10, ** p<0.05, *** p<0.01
* ============================================================================ *

preserve

* Base utilizada para Table A14
use "$data_int/lapop_remaining_mixed_tables_ready.dta", clear


* Caracteristicas municipales incluidas en Table A14
local common_x_vars ///
    popdensgeo2_2010 ///
    share_female_2010 ///
    mean_yrschool_2010 ///
    median_age_2010 ///
    share_laborforce_2010 ///
    share_unemployed_2010 ///
    share_izq_amplia_pre_avg ///
    share_alt_pre_avg


foreach x of local common_x_vars {

    * ------------------------------------------------------------------------ *
    * Etiqueta de la caracteristica municipal
    * ------------------------------------------------------------------------ *

    local xlabel "`x'"

    if "`x'" == "popdensgeo2_2010" {
        local xlabel "Population density"
    }

    if "`x'" == "share_female_2010" {
        local xlabel "Female population share"
    }

    if "`x'" == "mean_yrschool_2010" {
        local xlabel "Mean years of education"
    }

    if "`x'" == "median_age_2010" {
        local xlabel "Median age"
    }

    if "`x'" == "share_laborforce_2010" {
        local xlabel "Share in labor force"
    }

    if "`x'" == "share_unemployed_2010" {
        local xlabel "Share unemployed"
    }

    if "`x'" == "share_izq_amplia_pre_avg" {
        local xlabel "Left vote share"
    }

    if "`x'" == "share_alt_pre_avg" {
        local xlabel "Ideological alternation"
    }


    foreach ctrl_label in "No" "Yes" {

        local rhs_extra ""

        * Misma estructura de controles utilizada en Table A14
        if "`ctrl_label'" == "Yes" {

            local rhs_extra "edad hombre"

            foreach z of local common_x_vars {

                if "`z'" != "`x'" {
                    local rhs_extra ///
                        "`rhs_extra' px_`z'"
                }
            }
        }


        * -------------------------------------------------------------------- *
        * Reestimar exactamente la especificacion de Table A14
        * ------------------------------------------------------------------------ *

        quietly logit intencion_migrar ///
            pshare36 ///
            pshare56 ///
            px_`x' ///
            pxs36_`x' ///
            pxs56_`x' ///
            `rhs_extra' ///
            i.year_num i.mun_code_num ///
            [pw=wt], ///
            vce(cluster mun_code_num)

        local N = e(N)

        * Coeficientes Logit subyacentes
        local beta36 = _b[pshare36]
        local beta56 = _b[pshare56]


        * ==================================================================== *
        * TEST 1
        *
        * H0:
        * beta(Post x Share 1936-1955)
        * =
        * beta(Post x Share 1956-1978)
        * ==================================================================== *

        quietly lincom pshare36 - pshare56

        local difference_equal = r(estimate)
        local se_equal         = r(se)
        local p_equal          = r(p)

        local star_equal ""

        if `p_equal' < 0.10 {
            local star_equal "*"
        }

        if `p_equal' < 0.05 {
            local star_equal "**"
        }

        if `p_equal' < 0.01 {
            local star_equal "***"
        }


        * ==================================================================== *
        * TEST 2
        *
        * H0:
        * beta(Post x Share 1936-1955)
        * +
        * beta(Post x Share 1956-1978)
        * = 0
        * ==================================================================== *

        quietly lincom pshare36 + pshare56

        local additive_effect = r(estimate)
        local se_additive     = r(se)
        local p_additive      = r(p)

        local star_additive ""

        if `p_additive' < 0.10 {
            local star_additive "*"
        }

        if `p_additive' < 0.05 {
            local star_additive "**"
        }

        if `p_additive' < 0.01 {
            local star_additive "***"
        }


        * ==================================================================== *
        * MOSTRAR RESULTADOS EN LA CONSOLA
        * ==================================================================== *

        noi display _newline
        noi display as text ///
            "============================================================"

        noi display as text ///
            "TABLE A14"

        noi display as text ///
            "Municipal characteristic: `xlabel'"

        noi display as text ///
            "Controls: `ctrl_label'"

        noi display as text ///
            "============================================================"


        noi display as text ///
            "Underlying Logit coefficients:"

        noi display as text ///
            "  beta(Post x Share 1936-1955): " ///
            as result %10.5f `beta36'

        noi display as text ///
            "  beta(Post x Share 1956-1978): " ///
            as result %10.5f `beta56'


        noi display _newline

        noi display as result ///
            "TEST 1: Equality between the two treatment coefficients"

        noi display as text ///
            "H0: beta(Post x Share 1936-1955) = " ///
            "beta(Post x Share 1956-1978)"

        noi display as text ///
            "Difference: " ///
            as result %10.5f `difference_equal'

        noi display as text ///
            "Clustered SE: " ///
            as result %10.5f `se_equal'

        noi display as text ///
            "p-value: " ///
            as result %5.3f `p_equal' ///
            as result "`star_equal'"


        noi display _newline

        noi display as result ///
            "TEST 2: Additive treatment effect equals zero"

        noi display as text ///
            "H0: beta(Post x Share 1936-1955) + " ///
            "beta(Post x Share 1956-1978) = 0"

        noi display as text ///
            "Sum of coefficients: " ///
            as result %10.5f `additive_effect'

        noi display as text ///
            "Clustered SE: " ///
            as result %10.5f `se_additive'

        noi display as text ///
            "p-value: " ///
            as result %5.3f `p_additive' ///
            as result "`star_additive'"

        noi display as text ///
            "Observations: " ///
            as result %12.0fc `N'
    }
}


noi display _newline
noi display as text ///
    "Significance levels: * p<0.10, ** p<0.05, *** p<0.01"

restore