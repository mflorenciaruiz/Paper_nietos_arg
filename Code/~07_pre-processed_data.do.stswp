/*******************************************************************************
                       Código 7: Pre-processed data
						   
Fecha: 18 mayo 2026
Objetivo: Probar métodos de procesamiento previo de la data para balancear grupos

*******************************************************************************/

cd "/Users/florenciaruiz/BID 2/Paper Valerie/Nietos/Argentina/Paper_nietos_arg"
global main "/Users/florenciaruiz/BID 2/Paper Valerie/Nietos/Argentina/Paper_nietos_arg"
global data_int "$main/Data Int"
global data_out "$main/Data Out"
global data_raw "$main/Data Raw"
global output "$main/Output"
global output_eb "$main/Output/EB"

cap mkdir "$output_eb"

* ------------------------------------------------ *
* 1. Censo 2010
* ------------------------------------------------ *
{
* Importo la data del censo 
use "$data_raw/Censo/Censo 2010/censo_2010_arg.dta", clear

drop sample regionw

* Creo que código de inegi
gen mun_code_num = geo2_ar
tostring geo2_ar, gen(mun_code_str)
gen mun_code = substr(mun_code_str, -6, 6)
drop mun_code_str mun_code_num 

** Creo variables a nivel de municipio
	
	* La densidad de poblacion es a nivel geolevel2, chequeo que no varie dentro de geo2_ar
egen tag= tag(popdensgeo2 geo2_ar)
 tab tag // el pop densiy es el mismo dentro de cada municipio geo2_ar
 
	* Sexo 
tab sex, m
label list sex_lbl
gen male = .
	replace male = 1 if sex == 1
	replace male = 0 if sex == 2
gen female = .
	replace female = 1 if sex == 2
	replace female = 0 if sex == 1

	* Grupos de edad
tab age
label list age_lbl

gen age_0_14   = inrange(age, 0, 14)   if age < .
gen age_15_24  = inrange(age, 15, 24)  if age < .
gen age_25_44  = inrange(age, 25, 44)  if age < .
gen age_45_64  = inrange(age, 45, 64)  if age < .
gen age_65plus = age >= 65             if age < .

	* Nivel educativo alcanzado
tab edattain, m
label list edattain_lbl
replace edattain = . if edattain ==0 // son los NOT IN UNIVERSE

levelsof edattain if edattain < ., local(edu_levels)
foreach e of local edu_levels {
    gen edu_`e' = edattain == `e' if edattain < .
}

	* Años de educación promedio
tab	yrschool, m
label list yrschool_lbl 
replace yrschool =. if yrschool ==90 // Not specified
replace yrschool =. if yrschool ==91 // Some primary
replace yrschool =. if yrschool ==92 // Some technical after primary
replace yrschool =. if yrschool ==93 // Some secondary
replace yrschool =. if yrschool ==94 // Some tertiary
replace yrschool =. if yrschool ==95 // Adult literacy
replace yrschool =. if yrschool ==96 // Special education
replace yrschool =. if yrschool ==98 // Unknown/missing
replace yrschool =. if yrschool ==99 //NIU (not in universe)

	* Alfabetismo
tab lit
label list lit_lbl
gen literate = .
	replace literate = 1 if lit == 2
	replace literate = 0 if lit == 1
	
	* Categorías de actividad
tab empstat
label list empstat_lbl
tab age if empstat == 0
tab age if empstat == 3 // solo la responden mayores de 13
replace empstat = . if empstat == 0 // son los NOT IN UNIVERSE

levelsof empstat if empstat < ., local(emp_levels)
foreach e of local emp_levels {
    gen empstat_`e' = empstat == `e' if empstat < .
}

	* PEA
tab labforce
label list labforce_lbl
tab age if labforce == 9 
tab age if labforce == 1 | labforce == 2 // solo la responden mayores de 14
gen in_laborforce = .
	replace in_laborforce = 1 if labforce == 2
	replace in_laborforce = 0 if labforce == 1

	* Colapso a nivel municipio
collapse ///
    (sum) pop = perwt ///
    (mean) share_female = female     ///
           share_male   = male     ///
           mean_age = age                ///
           share_age_0_14   = age_0_14   ///
           share_age_15_24  = age_15_24  ///
           share_age_25_44  = age_25_44  ///
           share_age_45_64  = age_45_64 ///
           share_age_65plus = age_65plus ///
           share_literate = literate   ///
           mean_yrschool = yrschool      ///
           share_laborforce = in_laborforce ///
           edu_* ///
           empstat_* ///
		   popdensgeo2 ///
	(p50) median_age = age ///
	       median_yrschool = yrschool ///
    [pw = perwt], by(mun_code)

foreach var of varlis share_* edu_* empstat_* {
	replace `var' = `var'*100
}

* Formatos
format share_* %9.4f
format mean_* %9.4f
format pop %12.0fc
format edu_* %9.4f
format empstat_* %9.4f
format popdensgeo2 %12.2fc

rename edu_1 share_less_primary
rename edu_2 share_primary
rename edu_3 share_secondary
rename edu_4 share_university

rename  empstat_1 share_employed
rename  empstat_2 share_unemployed
rename  empstat_3 share_inactive

save "$data_int/censo_2010_arg_mun.dta", replace
}
* ------------------------------------------------ *
* 2. Merge con españoles y outcomes pre treatment
* ------------------------------------------------ *
{
* Importo la data que tiene el pct de españoles por municipios
use "$data_int/dip_nac_mun_pre_avg.dta", clear

* Uno con la data del censo
merge 1:1 mun_code using "$data_int/censo_2010_arg_mun.dta"
drop _merge

gen log_pop = ln(pop)
gen log_density = ln(popdensgeo2)

save "$data_int/data_for_balance.dta", replace
}
* ------------------------------------------------ *
* 3. Entropy Balance
* ------------------------------------------------ *
{
	
* ----- 3.1 Corro el EB -----*

*ssc install ebct, replace

corr share_1936_1955 share_1956_1978 //  0.6451 

* Sets de covariables para balancear
	* Alternativa 1: minimo de covariables
global covs1 porcentaje_blanco_pre_avg participacion_pre_avg log_pop log_density 

	* Alternativa 2: 1 + variables de demografía
global covs2 porcentaje_blanco_pre_avg participacion_pre_avg log_pop log_density share_female mean_age

	* Alternativa 3: 2 + variables económicas y educativas 
global covs3 porcentaje_blanco_pre_avg participacion_pre_avg log_pop log_density share_female mean_age ///
	   share_employed share_laborforce share_literate

	* Alternativa 4: 3 + outcomes electorales pre tratamiento que estaban desbalanceados (y voto blanco)
global covs4 porcentaje_blanco_pre_avg participacion_pre_avg log_pop log_density share_female mean_age /// 
       share_employed share_laborforce share_literate share_izq_pre_avg share_peronistas_pre_avg nep_pre_avg margen_pre_avg

	* Alternativa 5: 1 + outcomes electorales pre tratamiento que estaban desbalanceados
global covs5 porcentaje_blanco_pre_avg participacion_pre_avg log_pop log_density share_izq_pre_avg ///
       share_peronistas_pre_avg nep_pre_avg margen_pre_avg
	   
	* Alternativa 6: outcomes electorales desbalanceados solamente
global covs6 participacion_pre_avg share_izq_pre_avg share_peronistas_pre_avg nep_pre_avg margen_pre_avg

	* Alternativa 7: parsimoniosa solo con covariables
global covs7 log_density mean_age share_employed mean_yrschool

	* Alternativa 8: parsimoniosa solo con otro set de covariables
global covs8 log_density share_age_25_44 share_employed share_university

* Pesos para el primer tratamiento (1936-1955), incluyo el segundo dentro del balance
	* Alternativa 1:
ebct $covs1 share_1956_1978, treatvar(share_1936_1955)
rename _weight w_1936_1

	* Alternativa 2:
ebct $covs2 share_1956_1978, treatvar(share_1936_1955) 
rename _weight w_1936_2
	
	* Alternativa 3:
ebct $covs3 share_1956_1978, treatvar(share_1936_1955) 
rename _weight w_1936_3

	* Alternativa 4:
ebct $covs4 share_1956_1978, treatvar(share_1936_1955) 
rename _weight w_1936_4

	* Alternativa 5:
ebct $covs5 share_1956_1978, treatvar(share_1936_1955) 
rename _weight w_1936_5

	* Alternativa 6:
ebct $covs6 share_1956_1978, treatvar(share_1936_1955) 
rename _weight w_1936_6

	* Alternativa 7:
ebct $covs7 share_1956_1978, treatvar(share_1936_1955) 
rename _weight w_1936_7

	* Alternativa 8:
ebct $covs8 share_1956_1978, treatvar(share_1936_1955) 
rename _weight w_1936_8

* Pesos para el segundo tratamiento (1956-1978), incluyo el primero dentro del balance
	* Alternativa 1:
ebct $covs1 share_1936_1955, treatvar(share_1956_1978) 
rename _weight w_1956_1

	* Alternativa 2:
ebct $covs2 share_1936_1955, treatvar(share_1956_1978) 
rename _weight w_1956_2

	* Alternativa 3:
ebct $covs3 share_1936_1955, treatvar(share_1956_1978) 
rename _weight w_1956_3

	* Alternativa 4:
ebct $covs4 share_1936_1955, treatvar(share_1956_1978) 
rename _weight w_1956_4

	* Alternativa 5:
ebct $covs5 share_1936_1955, treatvar(share_1956_1978)
rename _weight w_1956_5 

	* Alternativa 6:
ebct $covs6 share_1936_1955, treatvar(share_1956_1978)
rename _weight w_1956_6

	* Alternativa 7:
ebct $covs7 share_1936_1955, treatvar(share_1956_1978)
rename _weight w_1956_7

	* Alternativa 8:
ebct $covs8 share_1936_1955, treatvar(share_1956_1978)
rename _weight w_1956_8

* ----- 3.2 Chequeos ----- *

** Correlación tratamiento-covariables con pesos

local outfile "$output/balance_corr.xlsx"

* Borrar archivo anterior si existe
capture erase "`outfile'"

forvalues i = 1/8 {
	
    * Loop 1: tratamiento 1936-1955
  
    preserve
    
        tempname postcorr
        tempfile corr_table
        
        postfile `postcorr' str80 variable corr_before corr_after using `corr_table', replace
        
        foreach v of varlist ${covs`i'} {
            
            * Correlación sin ponderar
            quietly corr `v' share_1936_1955
            local c_before = r(rho)
            
            * Correlación ponderada con pesos correspondientes
            quietly corr `v' share_1936_1955 [aw = w_1936_`i']
            local c_after = r(rho)
            
            post `postcorr' ("`v'") (`c_before') (`c_after')
        }
        
        postclose `postcorr'
        
        use `corr_table', clear
        
        format corr_before corr_after %9.4f
        
        export excel using "`outfile'", firstrow(variables) sheet("covs`i'_36") sheetreplace
    
    restore
 
    * Loop 2: tratamiento 1956-1978
    
    preserve
    
        tempname postcorr
        tempfile corr_table
        
        postfile `postcorr' str80 variable corr_before corr_after using `corr_table', replace
        
        foreach v of varlist ${covs`i'} {
            
            * Correlación sin ponderar
            quietly corr `v' share_1956_1978
            local c_before = r(rho)
            
            * Correlación ponderada con pesos correspondientes
            quietly corr `v' share_1956_1978 [aw = w_1956_`i']
            local c_after = r(rho)
            
            post `postcorr' ("`v'") (`c_before') (`c_after')
        }
        
        postclose `postcorr'
        
        use `corr_table', clear
        
        format corr_before corr_after %9.4f
        
        export excel using "`outfile'", firstrow(variables) sheet("covs`i'_56") sheetreplace
    
    restore
}

** Chequeo de relaciones no lineales

local outfile "$output/balance_nolineal.xlsx"

capture erase "`outfile'"

* Chequeo para los pesos de share_1936_1955
preserve

    tempname postr2
    tempfile r2_table_36

    postfile `postr2' str10 covset str80 variable r2_unweighted r2_weighted using `r2_table_36', replace

    forvalues i = 1/8 {

        foreach v of varlist ${covs`i'} {

            * Unweighted pseudo-regression
            quietly regress `v' ///
                c.share_1936_1955 ///
                c.share_1936_1955#c.share_1936_1955 ///
                c.share_1936_1955#c.share_1936_1955#c.share_1936_1955 ///
                c.share_1936_1955#c.share_1936_1955#c.share_1936_1955#c.share_1936_1955

            local r2_unw = e(r2)

            * Weighted pseudo-regression
            quietly regress `v' ///
                c.share_1936_1955 ///
                c.share_1936_1955#c.share_1936_1955 ///
                c.share_1936_1955#c.share_1936_1955#c.share_1936_1955 ///
                c.share_1936_1955#c.share_1936_1955#c.share_1936_1955#c.share_1936_1955 ///
                [aw = w_1936_`i']

            local r2_w = e(r2)

            post `postr2' ("covs`i'") ("`v'") (`r2_unw') (`r2_w')
        }
    }

    postclose `postr2'

    use `r2_table_36', clear

    gen diff_r2 = r2_weighted - r2_unweighted
    gen reduction_r2 = r2_unweighted - r2_weighted

    format r2_unweighted r2_weighted diff_r2 reduction_r2 %9.4f

    gsort covset -r2_weighted

    export excel using "`outfile'", firstrow(variables) sheet("pseudoR2_36") sheetreplace

restore

* Chequeo para los pesos de share_1956_1978
preserve

    tempname postr2
    tempfile r2_table_56

    postfile `postr2' str10 covset str80 variable r2_unweighted r2_weighted using `r2_table_56', replace

    forvalues i = 1/8 {

        foreach v of varlist ${covs`i'} {

            * Unweighted pseudo-regression
            quietly regress `v' ///
                c.share_1956_1978 ///
                c.share_1956_1978#c.share_1956_1978 ///
                c.share_1956_1978#c.share_1956_1978#c.share_1956_1978 ///
                c.share_1956_1978#c.share_1956_1978#c.share_1956_1978#c.share_1956_1978

            local r2_unw = e(r2)

            * Weighted pseudo-regression
            quietly regress `v' ///
                c.share_1956_1978 ///
                c.share_1956_1978#c.share_1956_1978 ///
                c.share_1956_1978#c.share_1956_1978#c.share_1956_1978 ///
                c.share_1956_1978#c.share_1956_1978#c.share_1956_1978#c.share_1956_1978 ///
                [aw = w_1956_`i']

            local r2_w = e(r2)

            post `postr2' ("covs`i'") ("`v'") (`r2_unw') (`r2_w')
        }
    }

    postclose `postr2'

    use `r2_table_56', clear

    gen diff_r2 = r2_weighted - r2_unweighted
    gen reduction_r2 = r2_unweighted - r2_weighted

    format r2_unweighted r2_weighted diff_r2 reduction_r2 %9.4f

    gsort covset -r2_weighted

    export excel using "`outfile'", firstrow(variables) sheet("pseudoR2_56") sheetreplace

restore

* Si R2 ≈ 0 con pesos para todas las covariables, balance es bueno.
* Si quedan R2 altos en alguna, considerar p=3 o agregar términos no-lineales.

** Resumen de distribución de pesos

	* Distrubución de pesos de 1936
local graphs36 ""
forvalues i = 1/4 {
	histogram w_1936_`i', percent title("Covariate set `i'", size(*0.8)) xtitle("EBCT weight") ytitle("Percent") name(hist_w1936_`i', replace)
    local graphs36 "`graphs36' hist_w1936_`i'"
}
graph combine `graphs36', cols(2) title("Distribution of EBCT weights: share 1936-1955", size(medsmall)) name(hist_weights_1936, replace)
graph export "$output/hist_weights_1936_14.png", replace width(2400)

local graphs36 ""
forvalues i = 5/8 {
	histogram w_1936_`i', percent title("Covariate set `i'", size(*0.8)) xtitle("EBCT weight") ytitle("Percent") name(hist_w1936_`i', replace)
    local graphs36 "`graphs36' hist_w1936_`i'"
}
graph combine `graphs36', cols(2) title("Distribution of EBCT weights: share 1936-1955", size(medsmall)) name(hist_weights_1936, replace)
graph export "$output/hist_weights_1936_58.png", replace width(2400)

	* Distrubución de pesos de 1956
local graphs56 ""
forvalues i = 1/4 {
    histogram w_1956_`i', percent title("Covariate set `i'", size(*0.8)) xtitle("EBCT weight") ytitle("Percent") name(hist_w1956_`i', replace)
    local graphs56 "`graphs56' hist_w1956_`i'"
}
graph combine `graphs56', cols(2) title("Distribution of EBCT weights: share 1956-1978", size(medsmall)) name(hist_weights_1956, replace)
graph export "$output/hist_weights_1956_14.png", replace width(2400)

local graphs56 ""
forvalues i = 5/8 {
    histogram w_1956_`i', percent title("Covariate set `i'", size(*0.8)) xtitle("EBCT weight") ytitle("Percent") name(hist_w1956_`i', replace)
    local graphs56 "`graphs56' hist_w1956_`i'"
}
graph combine `graphs56', cols(2) title("Distribution of EBCT weights: share 1956-1978", size(medsmall)) name(hist_weights_1956, replace)
graph export "$output/hist_weights_1956_58.png", replace width(2400)

** Tabla de estadísticas descriptivas de los pesos
preserve

tempname postw
tempfile weights_stats

postfile `postw' str20 weight mean sd min p99 max using `weights_stats', replace

foreach period in 1936 1956 {
    forvalues i = 1/8 {
        
        quietly summarize w_`period'_`i', detail
        
        post `postw' ("w_`period'_`i'") (r(mean)) (r(sd)) (r(min)) (r(p99)) (r(max))
    }
}

postclose `postw'
use `weights_stats', clear
format mean sd min p99 max %9.4f

export excel using "$output/weights_summary.xlsx", firstrow(variables) replace

restore

* ----- 3.3 Guardo la data con los pesos ----- *

keep mun_code w_*

save "$data_int/data_EB.dta", replace

}
* ------------------------------------------------ *
* 4. Entropy Balance versión 2
* ------------------------------------------------ *
{
* En esta versión corro de nuevo el EB pero sin incluir el otro share dentro de las covariables (ss = sin otro share)

use "$data_int/data_for_balance.dta", clear

* ----- 4.1 Corro el EB -----*
* Pesos para el primer tratamiento (1936-1955)
	* Alternativa 1:
ebct $covs1, treatvar(share_1936_1955)
rename _weight w_ss_1936_1

	* Alternativa 2:
ebct $covs2, treatvar(share_1936_1955) 
rename _weight w_ss_1936_2
	
	* Alternativa 3:
ebct $covs3, treatvar(share_1936_1955) 
rename _weight w_ss_1936_3

	* Alternativa 4:
ebct $covs4, treatvar(share_1936_1955) 
rename _weight w_ss_1936_4

	* Alternativa 5:
ebct $covs5, treatvar(share_1936_1955) 
rename _weight w_ss_1936_5

	* Alternativa 6:
ebct $covs6, treatvar(share_1936_1955) 
rename _weight w_ss_1936_6

	* Alternativa 7:
ebct $covs7, treatvar(share_1936_1955) 
rename _weight w_ss_1936_7

	* Alternativa 8:
ebct $covs8, treatvar(share_1936_1955) 
rename _weight w_ss_1936_8

* Pesos para el segundo tratamiento (1956-1978)
	* Alternativa 1:
ebct $covs1, treatvar(share_1956_1978) 
rename _weight w_ss_1956_1

	* Alternativa 2:
ebct $covs2, treatvar(share_1956_1978) 
rename _weight w_ss_1956_2

	* Alternativa 3:
ebct $covs3, treatvar(share_1956_1978) 
rename _weight w_ss_1956_3

	* Alternativa 4:
ebct $covs4, treatvar(share_1956_1978) 
rename _weight w_ss_1956_4

	* Alternativa 5:
ebct $covs5, treatvar(share_1956_1978)
rename _weight w_ss_1956_5 

	* Alternativa 6:
ebct $covs6, treatvar(share_1956_1978)
rename _weight w_ss_1956_6

	* Alternativa 7:
ebct $covs7, treatvar(share_1956_1978)
rename _weight w_ss_1956_7

	* Alternativa 8:
ebct $covs8, treatvar(share_1956_1978)
rename _weight w_ss_1956_8

* ----- 4.2 Chequeos ----- *

** Correlación tratamiento-covariables con pesos

local outfile "$output/balance_corr_2.xlsx"

* Borrar archivo anterior si existe
capture erase "`outfile'"

forvalues i = 1/8 {
	
    * Loop 1: tratamiento 1936-1955
  
    preserve
    
        tempname postcorr
        tempfile corr_table
        
        postfile `postcorr' str80 variable corr_before corr_after using `corr_table', replace
        
        foreach v of varlist ${covs`i'} {
            
            * Correlación sin ponderar
            quietly corr `v' share_1936_1955
            local c_before = r(rho)
            
            * Correlación ponderada con pesos correspondientes
            quietly corr `v' share_1936_1955 [aw = w_ss_1936_`i']
            local c_after = r(rho)
            
            post `postcorr' ("`v'") (`c_before') (`c_after')
        }
        
        postclose `postcorr'
        
        use `corr_table', clear
        
        format corr_before corr_after %9.4f
        
        export excel using "`outfile'", firstrow(variables) sheet("covs`i'_36") sheetreplace
    
    restore
 
    * Loop 2: tratamiento 1956-1978
    
    preserve
    
        tempname postcorr
        tempfile corr_table
        
        postfile `postcorr' str80 variable corr_before corr_after using `corr_table', replace
        
        foreach v of varlist ${covs`i'} {
            
            * Correlación sin ponderar
            quietly corr `v' share_1956_1978
            local c_before = r(rho)
            
            * Correlación ponderada con pesos correspondientes
            quietly corr `v' share_1956_1978 [aw = w_ss_1956_`i']
            local c_after = r(rho)
            
            post `postcorr' ("`v'") (`c_before') (`c_after')
        }
        
        postclose `postcorr'
        
        use `corr_table', clear
        
        format corr_before corr_after %9.4f
        
        export excel using "`outfile'", firstrow(variables) sheet("covs`i'_56") sheetreplace
    
    restore
}

** Chequeo de relaciones no lineales

local outfile "$output/balance_nolineal_2.xlsx"

capture erase "`outfile'"

* Chequeo para los pesos de share_1936_1955
preserve

    tempname postr2
    tempfile r2_table_36

    postfile `postr2' str10 covset str80 variable r2_unweighted r2_weighted using `r2_table_36', replace

    forvalues i = 1/8 {

        foreach v of varlist ${covs`i'} {

            * Unweighted pseudo-regression
            quietly regress `v' ///
                c.share_1936_1955 ///
                c.share_1936_1955#c.share_1936_1955 ///
                c.share_1936_1955#c.share_1936_1955#c.share_1936_1955 ///
                c.share_1936_1955#c.share_1936_1955#c.share_1936_1955#c.share_1936_1955

            local r2_unw = e(r2)

            * Weighted pseudo-regression
            quietly regress `v' ///
                c.share_1936_1955 ///
                c.share_1936_1955#c.share_1936_1955 ///
                c.share_1936_1955#c.share_1936_1955#c.share_1936_1955 ///
                c.share_1936_1955#c.share_1936_1955#c.share_1936_1955#c.share_1936_1955 ///
                [aw = w_ss_1936_`i']

            local r2_w = e(r2)

            post `postr2' ("covs`i'") ("`v'") (`r2_unw') (`r2_w')
        }
    }

    postclose `postr2'

    use `r2_table_36', clear

    gen diff_r2 = r2_weighted - r2_unweighted
    gen reduction_r2 = r2_unweighted - r2_weighted

    format r2_unweighted r2_weighted diff_r2 reduction_r2 %9.4f

    gsort covset -r2_weighted

    export excel using "`outfile'", firstrow(variables) sheet("pseudoR2_36") sheetreplace

restore

* Chequeo para los pesos de share_1956_1978
preserve

    tempname postr2
    tempfile r2_table_56

    postfile `postr2' str10 covset str80 variable r2_unweighted r2_weighted using `r2_table_56', replace

    forvalues i = 1/8 {

        foreach v of varlist ${covs`i'} {

            * Unweighted pseudo-regression
            quietly regress `v' ///
                c.share_1956_1978 ///
                c.share_1956_1978#c.share_1956_1978 ///
                c.share_1956_1978#c.share_1956_1978#c.share_1956_1978 ///
                c.share_1956_1978#c.share_1956_1978#c.share_1956_1978#c.share_1956_1978

            local r2_unw = e(r2)

            * Weighted pseudo-regression
            quietly regress `v' ///
                c.share_1956_1978 ///
                c.share_1956_1978#c.share_1956_1978 ///
                c.share_1956_1978#c.share_1956_1978#c.share_1956_1978 ///
                c.share_1956_1978#c.share_1956_1978#c.share_1956_1978#c.share_1956_1978 ///
                [aw = w_ss_1956_`i']

            local r2_w = e(r2)

            post `postr2' ("covs`i'") ("`v'") (`r2_unw') (`r2_w')
        }
    }

    postclose `postr2'

    use `r2_table_56', clear

    gen diff_r2 = r2_weighted - r2_unweighted
    gen reduction_r2 = r2_unweighted - r2_weighted

    format r2_unweighted r2_weighted diff_r2 reduction_r2 %9.4f

    gsort covset -r2_weighted

    export excel using "`outfile'", firstrow(variables) sheet("pseudoR2_56") sheetreplace

restore

* Si R2 ≈ 0 con pesos para todas las covariables, balance es bueno.
* Si quedan R2 altos en alguna, considerar p=3 o agregar términos no-lineales.

** Resumen de distribución de pesos

	* Distrubución de pesos de 1936
local graphs36 ""
forvalues i = 1/4 {
	histogram w_ss_1936_`i', percent title("Covariate set `i'", size(*0.8)) xtitle("EBCT weight") ytitle("Percent") name(hist_w1936_`i', replace)
    local graphs36 "`graphs36' hist_w1936_`i'"
}
graph combine `graphs36', cols(2) title("Distribution of EBCT weights: share 1936-1955", size(medsmall)) name(hist_weights_1936, replace)
graph export "$output/hist_weights_ss_1936_14.png", replace width(2400)

local graphs36 ""
forvalues i = 5/8 {
	histogram w_ss_1936_`i', percent title("Covariate set `i'", size(*0.8)) xtitle("EBCT weight") ytitle("Percent") name(hist_w1936_`i', replace)
    local graphs36 "`graphs36' hist_w1936_`i'"
}
graph combine `graphs36', cols(2) title("Distribution of EBCT weights: share 1936-1955", size(medsmall)) name(hist_weights_1936, replace)
graph export "$output/hist_weights_ss_1936_58.png", replace width(2400)

	* Distrubución de pesos de 1956
local graphs56 ""
forvalues i = 1/4 {
    histogram w_ss_1956_`i', percent title("Covariate set `i'", size(*0.8)) xtitle("EBCT weight") ytitle("Percent") name(hist_w1956_`i', replace)
    local graphs56 "`graphs56' hist_w1956_`i'"
}
graph combine `graphs56', cols(2) title("Distribution of EBCT weights: share 1956-1978", size(medsmall)) name(hist_weights_1956, replace)
graph export "$output/hist_weights_ss_1956_14.png", replace width(2400)

local graphs56 ""
forvalues i = 5/8 {
    histogram w_ss_1956_`i', percent title("Covariate set `i'", size(*0.8)) xtitle("EBCT weight") ytitle("Percent") name(hist_w1956_`i', replace)
    local graphs56 "`graphs56' hist_w1956_`i'"
}
graph combine `graphs56', cols(2) title("Distribution of EBCT weights: share 1956-1978", size(medsmall)) name(hist_weights_1956, replace)
graph export "$output/hist_weights_ss_1956_58.png", replace width(2400)

** Tabla de estadísticas descriptivas de los pesos
preserve

tempname postw
tempfile weights_stats

postfile `postw' str20 weight mean sd min p99 max using `weights_stats', replace

foreach period in 1936 1956 {
    forvalues i = 1/8 {
        
        quietly summarize w_ss_`period'_`i', detail
        
        post `postw' ("w_ss_`period'_`i'") (r(mean)) (r(sd)) (r(min)) (r(p99)) (r(max))
    }
}

postclose `postw'
use `weights_stats', clear
format mean sd min p99 max %9.4f

export excel using "$output/weights_summary_2.xlsx", firstrow(variables) replace

restore

* ----- 4.3 Guardo la data con los pesos ----- *

keep mun_code w_*

merge 1:1 mun_code using "$data_int/data_EB.dta"
drop _merge
save "$data_int/data_EB.dta", replace


}
* ------------------------------------------------ *
* 5. Entropy Balance versión 3
* ------------------------------------------------ *
* En esta versión corro el EB con la lógica del SC (outcomes lageado + variables demográficas) y pesos diferentes por outcome

* -----    5.0 Data    -----*
use "$data_out/dip_nac_mun.dta", clear

keep mun_code anio tipo_eleccion participacion porcentaje_blanco share_1936_1955 share_1956_1978
keep if anio < 2022

bys mun_code anio: egen participacion_prom = mean(participacion)
bys mun_code anio: egen blanco_prom = mean(porcentaje_blanco)

* Paso a formato wide
isid mun_code anio tipo_eleccion // Chequeo IDs unicos 

gen elec_anio = string(anio) + "_" + tipo_eleccion
drop tipo_eleccion anio

reshape wide participacion porcentaje_blanco participacion_prom blanco_prom, ///
    i(mun_code) j(elec_anio) string

foreach y in 2011 2013 2015 2017 2019 2021{
	drop participacion_prom`y'_GENERALES
	rename participacion_prom`y'_PASO participacion_prom`y'
	
	drop blanco_prom`y'_GENERALES
	rename blanco_prom`y'_PASO blanco_prom`y'
}

order participacion_prom* blanco_prom*, last
	
merge 1:1 mun_code using "$data_int/censo_2010_arg_mun.dta"
drop _merge

gen log_pop = ln(pop)
gen log_density = ln(popdensgeo2)

* Data de migración de LAPOP
preserve

use "$data_out/lapop_data_merge.dta", clear
codebook mun_code
keep mun_code intencion_migrar year
drop if year == 2023

collapse (mean) intencion_migrar, by (mun_code year)

* Paso a formato wide
isid mun_code year  // Chequeo IDs unicos 
tostring year, replace
reshape wide intencion_migrar , ///
    i(mun_code) j(year) string
	
tempfile intencion_migrar_mun
save `intencion_migrar_mun', replace

restore

merge 1:1 mun_code using `intencion_migrar_mun'

* ----- 5.1 Corro el EB -----*

*** Blank vote share ***

* Pesos para el primer tratamiento (1936-1955)

	* Solo los outcome pre 
ebct porcentaje_blanco*, treatvar(share_1936_1955)
rename _weight w_b_1936_1

	* Outcome pre + covariables demográficas
ebct porcentaje_blanco* log_density share_female mean_age mean_yrschool, treatvar(share_1936_1955)
rename _weight w_b_1936_2

ebct porcentaje_blanco* log_density mean_age mean_yrschool, treatvar(share_1936_1955)
rename _weight w_b_1936_3

ebct porcentaje_blanco* log_density mean_age, treatvar(share_1936_1955)
rename _weight w_b_1936_4

ebct porcentaje_blanco* log_density mean_yrschool, treatvar(share_1936_1955)
rename _weight w_b_1936_5

	* Outcome pre promedio + covariables demográficas
ebct blanco_prom* log_density share_female mean_age mean_yrschool, treatvar(share_1936_1955)
rename _weight w_b_1936_6

ebct blanco_prom* log_density mean_age mean_yrschool, treatvar(share_1936_1955)
rename _weight w_b_1936_7

ebct blanco_prom* log_density mean_age, treatvar(share_1936_1955)
rename _weight w_b_1936_8

ebct blanco_prom* log_density mean_yrschool, treatvar(share_1936_1955)
rename _weight w_b_1936_9
	// no se gana mucho en distribucion de los pesos por reducir los outcomes
	
* Pesos para el primer tratamiento (1956-1978)

	* Solo los outcome pre 
ebct porcentaje_blanco*, treatvar(share_1956_1978)
rename _weight w_b_1956_1

	* Outcome pre + covariables demográficas
ebct porcentaje_blanco* log_density share_female mean_age mean_yrschool, treatvar(share_1956_1978)
rename _weight w_b_1956_2

ebct porcentaje_blanco* log_density mean_age mean_yrschool, treatvar(share_1956_1978)
rename _weight w_b_1956_3

ebct porcentaje_blanco* log_density mean_age, treatvar(share_1956_1978)
rename _weight w_b_1956_4

ebct porcentaje_blanco* log_density mean_yrschool, treatvar(share_1956_1978)
rename _weight w_b_1956_5

	* Outcome pre promedio + covariables demográficas
ebct blanco_prom* log_density share_female mean_age mean_yrschool, treatvar(share_1956_1978)
rename _weight w_b_1956_6

ebct blanco_prom* log_density mean_age mean_yrschool, treatvar(share_1956_1978)
rename _weight w_b_1956_7

ebct blanco_prom* log_density mean_age, treatvar(share_1956_1978)
rename _weight w_b_1956_8

ebct blanco_prom* log_density mean_yrschool, treatvar(share_1956_1978)
rename _weight w_b_1956_9
	
*** Turnout ***

* Pesos para el primer tratamiento (1936-1955)

	* Solo los outcome pre 
ebct participacion*, treatvar(share_1936_1955)
rename _weight w_p_1936_1

	* Outcome pre + covariables demográficas
ebct participacion* log_density share_female mean_age mean_yrschool, treatvar(share_1936_1955)
rename _weight w_p_1936_2

ebct participacion* log_density mean_age mean_yrschool, treatvar(share_1936_1955)
rename _weight w_p_1936_3

ebct participacion* log_density mean_age, treatvar(share_1936_1955)
rename _weight w_p_1936_4

ebct participacion* log_density mean_yrschool, treatvar(share_1936_1955)
rename _weight w_p_1936_5

	* Outcome pre promedio + covariables demográficas
ebct participacion_prom* log_density share_female mean_age mean_yrschool, treatvar(share_1936_1955)
rename _weight w_p_1936_6

ebct participacion_prom* log_density mean_age mean_yrschool, treatvar(share_1936_1955)
rename _weight w_p_1936_7

ebct participacion_prom* log_density mean_age, treatvar(share_1936_1955)
rename _weight w_p_1936_8

ebct participacion_prom* log_density mean_yrschool, treatvar(share_1936_1955)
rename _weight w_p_1936_9
	// no se gana mucho en distribucion de los pesos por reducir los outcomes
	
* Pesos para el primer tratamiento (1956-1978)

	* Solo los outcome pre 
ebct participacion*, treatvar(share_1956_1978)
rename _weight w_p_1956_1

	* Outcome pre + covariables demográficas
ebct participacion* log_density share_female mean_age mean_yrschool, treatvar(share_1956_1978)
rename _weight w_p_1956_2

ebct participacion* log_density mean_age mean_yrschool, treatvar(share_1956_1978)
rename _weight w_p_1956_3

ebct participacion* log_density mean_age, treatvar(share_1956_1978)
rename _weight w_p_1956_4

ebct participacion* log_density mean_yrschool, treatvar(share_1956_1978)
rename _weight w_p_1956_5

	* Outcome pre promedio + covariables demográficas
ebct participacion_prom* log_density share_female mean_age mean_yrschool, treatvar(share_1956_1978)
rename _weight w_p_1956_6

ebct participacion_prom* log_density mean_age mean_yrschool, treatvar(share_1956_1978)
rename _weight w_p_1956_7

ebct participacion_prom* log_density mean_age, treatvar(share_1956_1978)
rename _weight w_p_1956_8

ebct participacion_prom* log_density mean_yrschool, treatvar(share_1956_1978)
rename _weight w_p_1956_9

*** Migration intention ***

* Pesos para el primer tratamiento (1936-1955)

	* Solo los outcome pre 
ebct intencion_migrar* if _merge ==3, treatvar(share_1936_1955)
rename _weight w_m_1936_1

	* Outcome pre + covariables demográficas
cap ebct intencion_migrar* log_density share_female mean_age mean_yrschool if _merge ==3, treatvar(share_1936_1955) //Hessian is not negative semidefinite
cap rename _weight w_m_1936_2

ebct intencion_migrar* log_density mean_age mean_yrschool if _merge ==3, treatvar(share_1936_1955)
rename _weight w_m_1936_3

ebct intencion_migrar* log_density mean_age if _merge ==3, treatvar(share_1936_1955)
rename _weight w_m_1936_4

ebct intencion_migrar* log_density mean_yrschool if _merge ==3, treatvar(share_1936_1955)
rename _weight w_m_1936_5

	* Outcome pre promedio + covariables demográficas
cap ebct intencion_migrar* log_density share_female mean_age mean_yrschool if _merge ==3, treatvar(share_1936_1955) // Hessian is not negative semidefinite
cap rename _weight w_m_1936_6 

ebct intencion_migrar* log_density mean_age mean_yrschool if _merge ==3, treatvar(share_1936_1955)
rename _weight w_m_1936_7

ebct intencion_migrar* log_density mean_age if _merge ==3, treatvar(share_1936_1955)
rename _weight w_m_1936_8

ebct intencion_migrar* log_density mean_yrschool if _merge ==3, treatvar(share_1936_1955)
rename _weight w_m_1936_9

* Pesos para el primer tratamiento (1956-1978)

	* Solo los outcome pre 
ebct intencion_migrar* if _merge ==3, treatvar(share_1956_1978)
rename _weight w_m_1956_1

	* Outcome pre + covariables demográficas
ebct intencion_migrar* log_density share_female mean_age mean_yrschool if _merge ==3, treatvar(share_1956_1978)
rename _weight w_m_1956_2

ebct intencion_migrar* log_density mean_age mean_yrschool if _merge ==3, treatvar(share_1956_1978) // convergence not achieved
rename _weight w_m_1956_3

ebct intencion_migrar* log_density mean_age if _merge ==3, treatvar(share_1956_1978)
rename _weight w_m_1956_4

ebct intencion_migrar* log_density mean_yrschool if _merge ==3, treatvar(share_1956_1978)
rename _weight w_m_1956_5

	* Outcome pre promedio + covariables demográficas
ebct intencion_migrar* log_density share_female mean_age mean_yrschool if _merge ==3, treatvar(share_1956_1978)
rename _weight w_m_1956_6

ebct intencion_migrar* log_density mean_age mean_yrschool if _merge ==3, treatvar(share_1956_1978)
rename _weight w_m_1956_7

ebct intencion_migrar* log_density mean_age if _merge ==3, treatvar(share_1956_1978)
rename _weight w_m_1956_8

ebct intencion_migrar* log_density mean_yrschool if _merge ==3, treatvar(share_1956_1978)
rename _weight w_m_1956_9

* ----- 5.2 Resumen de distribución de pesos ----- *

*** Blank vote share ***

set graphics off
	* Distrubución de pesos de 1936
local graphs36 ""
forvalues i = 1/9 {
	histogram w_b_1936_`i', percent title("Covariate set `i'", size(*0.8)) ///
	                        xtitle("EBCT weight") ytitle("Percent") name(hist_w1936_b_`i', replace)
    local graphs36 "`graphs36' hist_w1936_b_`i'"
}
graph combine `graphs36', cols(3) title("Distribution of EBCT weights", size(medsmall)) ///
                          subtitle("Treatment: spanish share 1936-1955 - Outcome: blank vote share", size(medsmall)) ///
                          name(hist_weights_b_1936, replace)
graph export "$output_eb/hist_weights_b_1936.png", replace width(2400)

preserve
	* Exporto solo los que voy a usar
	drop w_b_1936_3
	rename w_b_1936_4 w_b_1936_3
	rename w_b_1936_5 w_b_1936_4
		foreach i in 1 2 3 4 {
			histogram w_b_1936_`i', percent  name(hist_w1936_b_`i', replace) ///
			          xtitle("EBCT weight", size(vlarge)) ///
			          ytitle("Percent", size(vlarge))  fcolor(gs12) lcolor(gs8)  ///
			          xlabel(, labsize(vlarge)) ylabel(0(10)50, labsize(vlarge)) 
			graph export "$output_eb/hist_weights_`i'_b_1936.png", replace width(2400)
		}
restore

	* Distrubución de pesos de 1956
local graphs56 ""
forvalues i = 1/9 {
    histogram w_b_1956_`i', percent title("Covariate set `i'", size(*0.8)) ///
	                        xtitle("EBCT weight") ytitle("Percent") name(hist_w1956_b_`i', replace)
    local graphs56 "`graphs56' hist_w1956_b_`i'"
}
graph combine `graphs56', cols(3) title("Distribution of EBCT weights: share 1956-1978", size(medsmall)) ///
								  subtitle("Treatment: spanish share 1956-1978 - Outcome: blank vote share", size(medsmall)) ///
                                  name(hist_weights_b_1956, replace)
graph export "$output_eb/hist_weights_b_1956.png", replace width(2400)

preserve
	* Exporto solo los que voy a usar
	drop w_b_1956_3
	rename w_b_1956_4 w_b_1956_3
	rename w_b_1956_5 w_b_1956_4
		foreach i in 1 2 3 4 {
			histogram w_b_1956_`i', percent name(hist_w1956_b_`i', replace) ///
			          xtitle("EBCT weight", size(vlarge)) ///
					  ytitle("Percent", size(vlarge))  fcolor(gs12) lcolor(gs8)  ///
					  xlabel(, labsize(vlarge)) ylabel(0(10)50, labsize(vlarge)) 
			graph export "$output_eb/hist_weights_`i'_b_1956.png", replace width(2400)
		}
restore

*** Turnout ***
	* Distrubución de pesos de 1936
local graphs36 ""
forvalues i = 1/9 {
	histogram w_p_1936_`i', percent title("Covariate set `i'", size(*0.8)) ///
	                        xtitle("EBCT weight") ytitle("Percent") name(hist_w1936_p_`i', replace)
    local graphs36 "`graphs36' hist_w1936_p_`i'"
}
graph combine `graphs36', cols(3) title("Distribution of EBCT weights", size(medsmall)) ///
                          subtitle("Treatment: spanish share 1936-1955 - Outcome: turnout", size(medsmall)) ///
                          name(hist_weights_p_1936, replace)
graph export "$output_eb/hist_weights_p_1936.png", replace width(2400)

preserve
	* Exporto solo los que voy a usar
	drop w_p_1936_3
	rename w_p_1936_4 w_p_1936_3
	rename w_p_1936_5 w_p_1936_4
		foreach i in 1 2 3 4 {
			histogram w_p_1936_`i', percent name(hist_w1936_p_`i', replace) ///
			          xtitle("EBCT weight", size(vlarge)) /// 
			          ytitle("Percent", size(vlarge))  fcolor(gs12) lcolor(gs8)  ///
			          xlabel(, labsize(vlarge)) ylabel(0(10)40, labsize(vlarge)) 
			graph export "$output_eb/hist_weights_`i'_p_1936.png", replace width(2400)
		}
restore

	* Distrubución de pesos de 1956
local graphs56 ""
forvalues i = 1/9 {
    histogram w_p_1956_`i', percent title("Covariate set `i'", size(*0.8)) ///
	                        xtitle("EBCT weight") ytitle("Percent") name(hist_w1956_p_`i', replace)
    local graphs56 "`graphs56' hist_w1956_p_`i'"
}
graph combine `graphs56', cols(3) title("Distribution of EBCT weights: share 1956-1978", size(medsmall)) ///
								  subtitle("Treatment: spanish share 1956-1978 - Outcome: turnout", size(medsmall)) ///
                                  name(hist_weights_p_1956, replace)
graph export "$output_eb/hist_weights_p_1956.png", replace width(2400)

preserve
	* Exporto solo los que voy a usar
	drop w_p_1956_3
	rename w_p_1956_4 w_p_1956_3
	rename w_p_1956_5 w_p_1956_4
		foreach i in 1 2 3 4 {
			histogram w_p_1956_`i', percent name(hist_w1956_p_`i', replace) ///
			           xtitle("EBCT weight", size(vlarge)) ///
			          ytitle("Percent", size(vlarge))  fcolor(gs12) lcolor(gs8)  ///
			          xlabel(, labsize(vlarge)) ylabel(0(10)40, labsize(vlarge)) 
			graph export "$output_eb/hist_weights_`i'_p_1956.png", replace width(2400)
		}
restore

*** Migration intention ***
	* Distrubución de pesos de 1936
local graphs36 ""
foreach i in 1 3 4 5 7 8 9 {
	histogram w_m_1936_`i', percent title("Covariate set `i'", size(*0.8)) ///
	                        xtitle("EBCT weight") ytitle("Percent") name(hist_w1936_m_`i', replace)
    local graphs36 "`graphs36' hist_w1936_m_`i'"
}
graph combine `graphs36', cols(3) title("Distribution of EBCT weights", size(medsmall)) ///
                          subtitle("Treatment: spanish share 1936-1955 - Outcome: intention to migrate", size(medsmall)) ///
                          name(hist_weights_m_1936, replace)
graph export "$output_eb/hist_weights_m_1936.png", replace width(2400)

preserve
	* Exporto solo los que voy a usar
	drop w_m_1936_3
	rename w_m_1936_4 w_m_1936_3
	rename w_m_1936_5 w_m_1936_4
		foreach i in 1 3 4 {
			histogram w_m_1936_`i', percent name(hist_w1936_m_`i', replace) ///
			          xtitle("EBCT weight", size(vlarge)) ///
			          ytitle("Percent", size(vlarge))  fcolor(gs12) lcolor(gs8)  ///
			          xlabel(, labsize(vlarge)) ylabel(0(10)85, labsize(vlarge)) 
			graph export "$output_eb/hist_weights_`i'_m_1936.png", replace width(2400)
		}
restore

	* Distrubución de pesos de 1956
local graphs56 ""
foreach i in 1 2 4 5 6 7 8 9 {
    histogram w_m_1956_`i', percent title("Covariate set `i'", size(*0.8)) ///
	                        xtitle("EBCT weight") ytitle("Percent") name(hist_w1956_m_`i', replace)
    local graphs56 "`graphs56' hist_w1956_m_`i'"
}
graph combine `graphs56', cols(3) title("Distribution of EBCT weights: share 1956-1978", size(medsmall)) ///
						  subtitle("Treatment: spanish share 1956-1978 - Outcome: intention to migrate", ///
						  size(medsmall)) ///
                          name(hist_weights_m_1956, replace)
graph export "$output_eb/hist_weights_m_1956.png", replace width(2400)

preserve
	* Exporto solo los que voy a usar
	drop w_m_1956_3
	rename w_m_1956_4 w_m_1956_3
	rename w_m_1956_5 w_m_1956_4
		foreach i in 1 3 4 {
			histogram w_m_1956_`i', percent name(hist_w1956_m_`i', replace) ///
			           xtitle("EBCT weight", size(vlarge)) ///
			          ytitle("Percent", size(vlarge))  fcolor(gs12) lcolor(gs8)  ///
			          xlabel(, labsize(vlarge)) ylabel(0(10)85, labsize(vlarge)) 
			graph export "$output_eb/hist_weights_`i'_m_1956.png", replace width(2400)
		}
restore

set graphics on

* ----- 5.3 Tabla de estadísticas descriptivas de los pesos ----- *

* ----- 5.4 Guardo la data con los pesos ----- *
keep mun_code w_*
save "$data_int/data_EB_v2.dta", replace


* ------------------------------------------------ * 
* 6. Pesos de CBGPS (de R)
* ------------------------------------------------ *
{
// Traigo la data de R con los pesos generados usando CBGPS para generar las distribuciones

use "$data_int/pesos_cbgps.dta", clear

** Resumen de distribución de pesos

	* Distrubución de pesos de 1936
local graphs36 ""
forvalues i = 1/4 {
	histogram w_cbgps_36_`i', percent title("Covariate set `i'", size(*0.8)) xtitle("CBGPS weight") ytitle("Percent") name(hist_w1936_`i', replace)
    local graphs36 "`graphs36' hist_w1936_`i'"
}
graph combine `graphs36', cols(2) title("Distribution of CBGPS weights: share 1936-1955", size(medsmall)) name(hist_weights_1936, replace)
graph export "$output/hist_weights_CBGPS_1936_14.png", replace width(2400)

local graphs36 ""
forvalues i = 5/8 {
	histogram w_cbgps_36_`i', percent title("Covariate set `i'", size(*0.8)) xtitle("CBGPS weight") ytitle("Percent") name(hist_w1936_`i', replace)
    local graphs36 "`graphs36' hist_w1936_`i'"
}
graph combine `graphs36', cols(2) title("Distribution of CBGPS weights: share 1936-1955", size(medsmall)) name(hist_weights_1936, replace)
graph export "$output/hist_weights_CBGPS_1936_58.png", replace width(2400)

	* Distrubución de pesos de 1956
local graphs56 ""
forvalues i = 1/4 {
    histogram w_cbgps_56_`i', percent title("Covariate set `i'", size(*0.8)) xtitle("CBGPS weight") ytitle("Percent") name(hist_w1956_`i', replace)
    local graphs56 "`graphs56' hist_w1956_`i'"
}
graph combine `graphs56', cols(2) title("Distribution of CBGPS weights: share 1956-1978", size(medsmall)) name(hist_weights_1956, replace)
graph export "$output/hist_weights_CBGPS_1956_14.png", replace width(2400)

local graphs56 ""
forvalues i = 5/8 {
    histogram w_bgps_56_`i', percent title("Covariate set `i'", size(*0.8)) xtitle("CBGPS weight") ytitle("Percent") name(hist_w1956_`i', replace)
    local graphs56 "`graphs56' hist_w1956_`i'"
}
graph combine `graphs56', cols(2) title("Distribution of CBGPS weights: share 1956-1978", size(medsmall)) name(hist_weights_1956, replace)
graph export "$output/hist_weights_CBGPS_1956_58.png", replace width(2400)

** Tabla de estadísticas descriptivas de los pesos
preserve

tempname postw
tempfile weights_stats

postfile `postw' str20 weight mean sd min p99 max using `weights_stats', replace

foreach period in 36 56 {
    forvalues i = 1/8 {
        
        quietly summarize w_cbgps_`period'_`i', detail
        
        post `postw' ("w_cbgps_`period'_`i'") (r(mean)) (r(sd)) (r(min)) (r(p99)) (r(max))
    }
}

postclose `postw'
use `weights_stats', clear
format mean sd min p99 max %9.4f

export excel using "$output/weights_summary_CBGS.xlsx", firstrow(variables) replace

restore
}
