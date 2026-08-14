* ============================================================================ *
*                 Corre estimaciones sobre remesas de LAPOP
* ============================================================================ *

* --------------------------------------------------------------
* 0. Setup
* --------------------------------------------------------------

clear all
set more off

global main "/Users/florenciaruiz/BID 2/Paper Valerie/Nietos/Argentina/Paper_nietos_arg"
global data_int "$main/Data Int"
global data_out "$main/Data Out"
global data_raw "$main/Data Raw"
global output   "$main/Output"
global output_eb "$main/Output/EB"

use "$data_out/lapop_data_merge.dta", clear

keep if year == 2023 | year == 2019 | year == 2012

gen post = (year >= 2022)

tab recibe_remesas
bys mun_code: tab recibe_remesas, m

gen post_share_1936 = post * share_1936_1955
gen post_share_1956 = post * share_1956_1978

label variable post_share_1936 "Post x Spanish share 1936-1955"
label variable post_share_1956 "Post x Spanish share 1956-1978"

egen mun_code_num = group(mun_code)

* --------------------------------------------------------------
*                  Estimaciones sobre remesas
* --------------------------------------------------------------
{
	eststo clear
	
* --------------------------------------------------------------
* Modelo 1: LPM (Linear Probability Model)
* --------------------------------------------------------------
eststo lpm: reghdfe recibe_remesas post_share_1936 post_share_1956 ///
    edad hombre, ///
    absorb(year mun_code_num) ///
    cluster(mun_code)

* Agregar informacion adicional que quiero mostrar en la tabla
estadd local yearFE "Yes"
estadd local munFE  "Yes"

* --------------------------------------------------------------
* Modelo 2: Logit (para obtener AME)
* --------------------------------------------------------------
qui logit recibe_remesas post_share_1936 post_share_1956 ///
    edad hombre i.year i.mun_code_num, ///
    cluster(mun_code)

* Guardo N y pseudo-R2 antes de que margins los sobreescriba
scalar logit_N   = e(N)
scalar logit_r2p = e(r2_p)

* Calcular efectos marginales promedio (AME)
qui margins, dydx(post_share_1936 post_share_1956 edad hombre) post

* Guardar como si fueran coeficientes (post ya reemplazo e(b) y e(V))
eststo logit_ame

estadd local yearFE "Yes"
estadd local munFE  "Yes"

* --------------------------------------------------------------
* Exportar a LaTeX
* --------------------------------------------------------------
esttab lpm logit_ame using "$output/remittances_lpm_logit.tex", ///
    replace booktabs ///
    b(%9.3f) se(%9.3f) ///
    star(* 0.10 ** 0.05 *** 0.01) ///
    mtitles("(1)" "(2)") ///
    keep(post_share_1936 post_share_1956 edad hombre) ///
    order(post_share_1936 post_share_1956 edad hombre) ///
    coeflabels(post_share_1936 "Spanish share 1936-1955 $\times$ Post" ///
               post_share_1956 "Spanish share 1956-1978 $\times$ Post" ///
               edad "Age" ///
               hombre "Male") ///
    stats(N r2 yearFE munFE, ///
          labels("Observations" "R\textsuperscript{2}" ///
                 "Time FE" "Municipality FE") ///
          fmt(%9.0fc %9.3f)) ///
    nonotes ///
    addnotes("\textit{Notes}: The dependent variable is a binary indicator equal to one if the respondent lives in a household that receives remittances from abroad. Column (1) reports OLS estimates from a linear probability model. Column (2) reports average marginal effects from a logit specification. Standard errors clustered at the municipality level in parentheses. \\sym{*} \\(p<0.10\\), \\sym{**} \\(p<0.05\\), \\sym{***} \\(p<0.01\\). \textit{Sources}: LAPOP AmericasBarometer (outcome and individual controls); IPUMS Argentine Censuses 1970 and 1980 (treatment variables).")
}

* --------------------------------------------------------------
*             Correlaciones con relación exterior
* --------------------------------------------------------------
* --------------------------------------------------------------
* Correlaciones LAPOP 2023: univariate regressions
* --------------------------------------------------------------

* --- Familia en el exterior --- *
	
	* LPM
reg familia_exterior share_1936_1955 hombre edad, vce(cluster mun_code) 
reg familia_exterior share_1956_1978 hombre edad, vce(cluster mun_code) 
	* Logit
logit familia_exterior post_share_1936 edad hombre, cluster(mun_code)
logit familia_exterior share_1956_1978 edad hombre, cluster(mun_code)
	
* --- Probabilidad de que moleste un vecino español --- *
	* LPM
reg molesta_vecino_esp share_1936_1955  hombre edad, vce(cluster mun_code) 
reg molesta_vecino_esp share_1956_1978  hombre edad, vce(cluster mun_code) 

	* Logit
logit molesta_vecino_esp post_share_1936 edad hombre, cluster(mun_code)
logit molesta_vecino_esp share_1956_1978 edad hombre, cluster(mun_code)

* --- Probabilidad de que moleste un vecino inmigrante --- *
	* LPM
reg molesta_vecino_inm share_1936_1955 hombre edad, vce(cluster mun_code) 
reg molesta_vecino_inm share_1956_1978 hombre edad, vce(cluster mun_code) 

	* Logit
logit molesta_vecino_inm post_share_1936 edad hombre, cluster(mun_code)
logit molesta_vecino_inm share_1956_1978 edad hombre, cluster(mun_code)

* --- Desacuerdo en proveer servicios a inmigrantes espñoles  --- *
	* LPM
reg desac_serv_esp share_1936_1955 hombre edad, vce(cluster mun_code) 
reg desac_serv_esp share_1956_1978 hombre edad, vce(cluster mun_code) 

	* Logit
logit desac_serv_esp post_share_1936 edad hombre, cluster(mun_code)
logit desac_serv_esp share_1956_1978 edad hombre, cluster(mun_code)

* --- Desacuerdo en proveer servicios a inmigrantes --- *
	
	* LPM	
reg desac_serv_inm share_1936_1955 hombre edad, vce(cluster mun_code) 
reg desac_serv_inm share_1956_1978 hombre edad, vce(cluster mun_code) 

	* Logit
logit desac_serv_inm post_share_1936 edad hombre, cluster(mun_code)
logit desac_serv_inm share_1956_1978 edad hombre, cluster(mun_code)

* --- Intención de migrar a españa --- *
gen intencion_migrar_esp2 = .
	replace intencion_migrar_esp2 = 1 if intencion_migrar_esp ==1
	replace intencion_migrar_esp2 = 0 if intencion_migrar ==0 | (intencion_migrar ==1 & intencion_migrar_esp ==0)

	* LPM
reg intencion_migrar_esp2 share_1936_1955 hombre edad, vce(cluster mun_code) 
reg intencion_migrar_esp2 share_1956_1978 hombre edad, vce(cluster mun_code) 
	* Logit
logit intencion_migrar_esp2 post_share_1936 edad hombre, cluster(mun_code)
logit intencion_migrar_esp2 share_1956_1978 edad hombre, cluster(mun_code)

eststo clear
keep if year == 2023

local outcomes familia_exterior molesta_vecino_esp molesta_vecino_inm ///
               desac_serv_esp desac_serv_inm intencion_migrar_esp2


* Loop: cada logit con UNA sola share

foreach y of local outcomes {
    * Regresion con share_1936_1955
    qui logit `y' share_1936_1955 hombre edad, cluster(mun_code)
    qui margins, dydx(share_1936_1955) post
    eststo `y'_a
    
    * Regresion con share_1956_1978
    qui logit `y' share_1956_1978 hombre edad, cluster(mun_code)
    qui margins, dydx(share_1956_1978) post
    eststo `y'_b
}


* Tabla A: Share 1936-1955 (6 columnas)

esttab familia_exterior_a molesta_vecino_esp_a molesta_vecino_inm_a ///
       desac_serv_esp_a desac_serv_inm_a intencion_migrar_esp2_a ///
    using "$output/lapop_2023_mechanisms_1936.tex", ///
    replace booktabs ///
    b(%9.3f) se(%9.3f) ///
    star(* 0.10 ** 0.05 *** 0.01) ///
    mtitles("(1)" "(2)" "(3)" "(4)" "(5)" "(6)") ///
    keep(share_1936_1955) ///
    coeflabels(share_1936_1955 "Spanish share 1936-1955") ///
    stats(N, labels("Observations") fmt(%9.0fc)) ///
    nonotes ///
    addnotes("\textit{Notes}: Each column reports the average marginal effect of Spanish share 1936-1955 from a separate logit regression on the outcome indicated in the column header. The dependent variables are: (1) family abroad; (2) bothered by Spanish neighbor; (3) bothered by immigrant neighbor; (4) disagreement with services to Spanish immigrants; (5) disagreement with services to immigrants; (6) intention to migrate to Spain. Individual controls include age and male. Standard errors clustered at the municipality level in parentheses. * p<0.10, ** p<0.05, *** p<0.01. \textit{Sources}: LAPOP AmericasBarometer, 2023 round (dependent variables and individual controls); IPUMS Argentine Census 1970 (Spanish immigration share).")


* Tabla B: Share 1956-1978 (6 columnas)

esttab familia_exterior_b molesta_vecino_esp_b molesta_vecino_inm_b ///
       desac_serv_esp_b desac_serv_inm_b intencion_migrar_esp_b ///
    using "$output/lapop_2023_mechanisms_1956.tex", ///
    replace booktabs ///
    b(%9.3f) se(%9.3f) ///
    star(* 0.10 ** 0.05 *** 0.01) ///
    mtitles("(1)" "(2)" "(3)" "(4)" "(5)" "(6)") ///
    keep(share_1956_1978) ///
    coeflabels(share_1956_1978 "Spanish share 1956-1978") ///
    stats(N, labels("Observations") fmt(%9.0fc)) ///
    nonotes ///
    addnotes("\textit{Notes}: Each column reports the average marginal effect of Spanish share 1956-1978 from a separate logit regression on the outcome indicated in the column header. The dependent variables are: (1) family abroad; (2) bothered by Spanish neighbor; (3) bothered by immigrant neighbor; (4) disagreement with services to Spanish immigrants; (5) disagreement with services to immigrants; (6) intention to migrate to Spain. Individual controls include age and male. Standard errors clustered at the municipality level in parentheses. * p<0.10, ** p<0.05, *** p<0.01. \textit{Sources}: LAPOP AmericasBarometer, 2023 round (dependent variables and individual controls); IPUMS Argentine Census 1980 (Spanish immigration share).")
	

* Tabla C

esttab familia_exterior_a familia_exterior_b intencion_migrar_esp2_a intencion_migrar_esp2_b ///
    using "$output/lapop_2023_mechanisms.tex", ///
    replace fragment ///
    b(%9.3f) se(%9.3f) ///
    star(* 0.10 ** 0.05 *** 0.01) ///
    mtitles("(1)" "(2)" "(3)" "(4)") ///
    keep(share_1936_1955 share_1956_1978) ///
    order(share_1936_1955 share_1956_1978) ///
	coeflabels(share_1956_1978 "Spanish share 1956-1978" share_1936_1955 "Spanish share 1936-1955") ///
    stats(N, labels("Observations") fmt(%9.0fc)) ///
    nonotes ///
    prehead("\begin{table}[htbp]" ///
            "\centering" ///
            "\caption{Historical Spanish Exposure and Migration-Related Outcomes}" ///
            "\begin{tabular}{l*{4}{c}}" ///
            "\hline") ///
    postfoot("\hline" ///
             "\end{tabular}" ///
			 "\captionsetup{justification = justified}" ///
             "\caption*{\footnotesize \textit{Notes:} Each column reports the average marginal effect of each Spanish share from a separate logit regression for the outcome indicated in the column header. The dependent variable in columns (1) and (2) is an indicator for having a family member living abroad. The dependent variable in columns (3) and (4) is an indicator for intending to migrate to Spain, coded as 0 for individuals with no intention to migrate or with an intention to migrate to another country. Individual controls include age and male. Standard errors clustered at the municipality level in parentheses. * p<0.10, ** p<0.05, *** p<0.01. \textit{Sources}: LAPOP AmericasBarometer, 2023 round (dependent variables and individual controls); IPUMS Argentine Census 1970 (Spanish immigration share).}" ///
             "\end{table}")
			 

