* ============================================================================ *
*     Panel unit root tests + bootstrap-based bias-corrected FE (xtbcfe)
* ============================================================================ *

clear all
set more off

* --- Paths (ajustar si hace falta) --- *
global data_out "/Users/florenciaruiz/BID 2/Paper Valerie/Nietos/Argentina/Paper_nietos_arg/Data Out"
global output   "/Users/florenciaruiz/BID 2/Paper Valerie/Nietos/Argentina/Paper_nietos_arg/Output"

* --- Instalar paquetes --- *
*ssc install xtbcfe
*ssc install moremata
*ssc install distinct

			 
* ============================================================================ *
*                                PASO y generales
* ============================================================================ *

* ============================================================================ *
* 0. Cargar data
* ============================================================================ *
use "$data_out/dip_nac_mun.dta", clear  

* ============================================================================ *
* 1. Setup del panel
* ============================================================================ *
* Unidad = combinación municipio × tipo de elección.
* Tiempo = secuencia de elección (1, 2, ..., T_i) para evitar los huecos
* (las elecciones ocurren cada 2 años, xtset con anio los trataría como irregulares).

sort mun_code tipo_eleccion anio
by mun_code tipo_eleccion: gen election_seq = _n
egen panel_id = group(mun_code tipo_eleccion)

sort mun_code tipo_eleccion anio
by mun_code tipo_eleccion: gen L_porcentaje_blanco = porcentaje_blanco[_n-1]
by mun_code tipo_eleccion: gen L_participacion = participacion[_n-1]

xtset panel_id election_seq

xtdescribe

* --- Crear interacciones --- *
capture drop post_share_1936 post_share_1956
gen post_share_1936 = post * share_1936_1955
gen post_share_1956 = post * share_1956_1978
label variable post_share_1936 "Spanish share 1936-1955 x Post"
label variable post_share_1956 "Spanish share 1956-1978 x Post"

* Tipo de eleccion como factor
encode tipo_eleccion, gen(tipo_eleccion_num)
egen mun_code_num = group(mun_code)

* Crear dummies de año
capture drop anio_d*
tab anio, gen(anio_d)

* ============================================================================ *
* 2. Tests de raíz unitaria sobre la dependiente
* ============================================================================ *
* Todos testean H0: raíz unitaria (no estacionaria).
* Rechazar H0 (p<0.05) => estacionaria => OK usar panel dinámico.
* Opción demean: resta la media temporal cross-sectional para controlar
* dependencia contemporánea entre municipios.

* 2.1 Im-Pesaran-Shin (IPS)
 // Acepta panel no balanceado. H1: al menos un panel es estacionario.
xtunitroot ips porcentaje_blanco, demean
xtunitroot ips participacion, demean

* 2.2 Fisher-type (ADF)
 // Acepta panel no balanceado. Combina p-values de ADFs individuales.
xtunitroot fisher porcentaje_blanco, dfuller lags(0) demean
xtunitroot fisher participacion, dfuller lags(0) demean

* ============================================================================ *
* 3. FE estándar (sesgado, para comparar)
* ============================================================================ *

estimates dir
estimates clear

* --- Blank Votes --- *
reghdfe porcentaje_blanco L.porcentaje_blanco ///
        post_share_1936 post_share_1956, ///
        absorb(panel_id anio) cluster(mun_code)
estimates store fe_std_b

reghdfe porcentaje_blanco L.porcentaje_blanco post_share_1936 post_share_1956, ///
        absorb(mun_code anio tipo_eleccion)
estimates store fe_additive_b

*xtreg porcentaje_blanco L_porcentaje_blanco ///
      post_share_1936 post_share_1956 ///
      i.anio tipo_eleccion_num i.mun_code_num, cluster(mun_code)

* --- Participación --- *
reghdfe participacion L.participacion ///
        post_share_1936 post_share_1956, ///
        absorb(panel_id anio) cluster(mun_code)
estimates store fe_std_p

reghdfe participacion L.participacion post_share_1936 post_share_1956, ///
        absorb(mun_code anio tipo_eleccion)
estimates store fe_additive_p

* ============================================================================ *
* 4. BCFE - Bootstrap-based bias-corrected FE (De Vos et al. 2015)
* ============================================================================ *
* Opciones elegidas:
*   lags(1)             : un rezago de la dependiente
*   resampling(cshet)   : bootstrap dentro de cada unidad → permite
*                          heteroscedasticidad municipio-específica
*   initialization(bi)  : burn-in initialization (más robusta que la determinística)
*   te                  : agrega efectos temporales (≈ i.anio dentro del panel)
*   bciters(500)        : 500 iteraciones para el bias correction
*   inference(inf_ci )  : percentiles empíricos del bootstrap para los CI. Alternativa: inf_se (SEs bootstrappeados con distribución t)
*   infiters(500)       : 500 iteraciones para inferencia

xtbcfe porcentaje_blanco post_share_1936 post_share_1956, ///
    lags(1) ///
	te ///
    resampling(cshet) ///
    initialization(bi) ///
    bciters(500) ///
    inference(inf_ci) ///
    infiters(1000)
estimates store bcfe_b

xtbcfe participacion post_share_1936 post_share_1956, ///
    lags(1) ///
	te ///
    resampling(cshet) ///
    initialization(bi) ///
    bciters(500) ///
    inference(inf_ci) ///
    infiters(1000)
estimates store bcfe_p

* --- Alternativa más laxa (heteroscedasticidad general) ---*
xtbcfe porcentaje_blanco post_share_1936 post_share_1956, ///
     lags(1) resampling(wboot) initialization(bi) te ///
     bciters(500) inference(inf_ci) infiters(1000)
estimates store bcfe_wboot_b
 
xtbcfe participacion post_share_1936 post_share_1956, ///
     lags(1) resampling(wboot) initialization(bi) te ///
     bciters(500) inference(inf_ci) infiters(1000)
estimates store bcfe_wboot_p

* ============================================================================ *
* 5. LSDVC (Kiviet, analítico)
* ============================================================================ *

capture xtlsdvc porcentaje_blanco post_share_1936 post_share_1956 ///
    i.election_seq, initial(ah) vcov(50)
if _rc == 0 estimates store lsdvc

* ============================================================================ *
* 6. Tabla comparativa
* ============================================================================ *
* Cols 1 y 4 (additive baseline)
foreach est in fe_additive_b fe_additive_p {
    estadd local muni_fe "Yes"           : `est'
    estadd local time_fe "Yes"           : `est'
    estadd local election_fe "Yes"       : `est'
    estadd local mun_election_fe "No"    : `est'
}

* Cols 2 y 5 (mun × tipo, sin BCFE)
foreach est in fe_std_b fe_std_p {
    estadd local muni_fe "No"            : `est'
    estadd local time_fe "Yes"           : `est'
    estadd local election_fe "No"        : `est'
    estadd local mun_election_fe "Yes"   : `est'
}

* Cols 3 y 6 (BCFE)
foreach est in bcfe_b bcfe_p {
    estadd local muni_fe "No"            : `est'
    estadd local time_fe "Yes"           : `est'
    estadd local election_fe "No"        : `est'
    estadd local mun_election_fe "Yes"   : `est'
}


esttab fe_additive_b fe_std_b bcfe_b ///
       fe_additive_p fe_std_p bcfe_p ///
	   using "$output/correction_blankvotes_turnout.tex", ///
    replace fragment ///
	nomtitles ///
    b(%9.3f) se(%9.3f) ///
    star(* 0.10 ** 0.05 *** 0.01) ///
    stats(N muni_fe time_fe  election_fe mun_election_fe, fmt(0 %s %s %s %s) ///
    labels("Observations" "Municipality FE" "Time FE" "Election type FE" "Municipality x Election type FE" "General elections only")) ///
	order(post_share_1936 post_share_1956 L.porcentaje_blanco L.participacion) ///
    keep(L.porcentaje_blanco L.participacion post_share_1936 post_share_1956) ///
     mgroups("Share of blank votes" "Voter turnout", ///
            pattern(1 0 0 1 0 0) ///
            prefix(\multicolumn{@span}{c}{) suffix(}) ///
            span erepeat(\cmidrule(lr){@span})) ///
    varlabels(L.porcentaje_blanco "Lag(blank vote share)" ///
	          L.participacion "Lag(turnout)" ///
              post_share_1936 "Spanish share 1936--1955 $\times$ Post" ///
              post_share_1956 "Spanish share 1956--1978 $\times$ Post") ///
    nonotes ///
    prehead("\begin{table}[htbp]" ///
            "\centering" ///
            "\caption{Bias-corrected fixed-effects estimates}" ///
            "\label{tab:dynamic_correction}" ///
            "\begin{tabular}{l*{6}{c}}" ///
            "\hline") ///
    postfoot("\hline" ///
             "\end{tabular}" ///
			 "\captionsetup{justification = justified}" ///
             "\caption*{\footnotesize \textit{Notes:} The dependent variable in columns (1)--(3) is the share of blank votes over total voters; in columns (4)--(6) it is voter turnout. Columns (1) and (4) report the baseline specification with additive municipality, year, and election-type fixed effects. Columns (2) and (5) replace the additive municipality and election-type fixed effects with their interaction (municipality $\times$ election-type). Columns (3) and (6) apply the bootstrap-based bias-corrected fixed-effects estimator (BCFE) of De Vos, Everaert, and Ruyssen (2015) to the specification in columns (2) and (5). All specifications include one lag of the dependent variable and observations from PASO and general elections. Prior to estimation, the stationarity of each dependent variable was tested using the Im-Pesaran-Shin (2003) test and the Fisher-type augmented Dickey-Fuller test (Choi 2001); both reject the null of a unit root at the 1\% level for both outcomes. In columns (1), (2), (4), and (5), standard errors are clustered at the municipality level and reported in parentheses. In columns (3) and (6), the bias-correction step uses 500 bootstrap iterations, and confidence intervals are computed from the 2.5th and 97.5th percentiles of the bootstrap distribution over 1,000 additional iterations, with residuals resampled with replacement within each panel unit. * $p<0.10$, ** $p<0.05$, *** $p<0.01$. \\ \textit{Sources}:National Electoral Commission (dependent variables); IPUMS Argentine Census 1970 (Spanish immigration shares).}" ///
             "\end{table}")
			 
			 
* ============================================================================ *
*                        Solo elecciones generales
* ============================================================================ *

* ============================================================================ *
* 0. Cargar data
* ============================================================================ *
use "$data_out/dip_nac_mun.dta", clear  

* ============================================================================ *
* 1. Setup del panel
* ============================================================================ *
* Unidad = combinación municipio 
* Tiempo = años

keep if tipo_eleccion == "GENERALES"

* Tipo de eleccion como factor y time index regular
egen mun_code_num = group(mun_code)
sort mun_code
by mun_code: gen election_seq = _n

sort mun_code_num election_seq
xtset mun_code_num election_seq

xtdescribe

* --- Crear interacciones --- *
capture drop post_share_1936 post_share_1956
gen post_share_1936 = post * share_1936_1955
gen post_share_1956 = post * share_1956_1978
label variable post_share_1936 "Spanish share 1936-1955 x Post"
label variable post_share_1956 "Spanish share 1956-1978 x Post"

* ============================================================================ *
* 2. Tests de raíz unitaria sobre la dependiente
* ============================================================================ *
* Todos testean H0: raíz unitaria (no estacionaria).
* Rechazar H0 (p<0.05) => estacionaria => OK usar panel dinámico.
* Opción demean: resta la media temporal cross-sectional para controlar
* dependencia contemporánea entre municipios.

* 2.1 Im-Pesaran-Shin (IPS)
 // Acepta panel no balanceado. H1: al menos un panel es estacionario.
xtunitroot ips porcentaje_blanco, demean
xtunitroot ips participacion, demean

* 2.2 Fisher-type (ADF)
 // Acepta panel no balanceado. Combina p-values de ADFs individuales.
xtunitroot fisher porcentaje_blanco, dfuller lags(0) demean
xtunitroot fisher participacion, dfuller lags(0) demean

* ============================================================================ *
* 3. BCFE - Bootstrap-based bias-corrected FE (De Vos et al. 2015)
* ============================================================================ *
* Opciones elegidas:
*   lags(1)             : un rezago de la dependiente
*   resampling(cshet)   : bootstrap dentro de cada unidad → permite
*                          heteroscedasticidad municipio-específica
*   initialization(bi)  : burn-in initialization (más robusta que la determinística)
*   te                  : agrega efectos temporales (≈ i.anio dentro del panel)
*   bciters(500)        : 500 iteraciones para el bias correction
*   inference(inf_ci )  : percentiles empíricos del bootstrap para los CI. Alternativa: inf_se (SEs bootstrappeados con distribución t)
*   infiters(500)       : 500 iteraciones para inferencia

xtbcfe porcentaje_blanco post_share_1936 post_share_1956, ///
    lags(1) ///
	te ///
    resampling(cshet) ///
    initialization(bi) ///
    bciters(500) ///
    inference(inf_ci) ///
    infiters(1000)
estimates store bcfe_g_b

xtbcfe participacion post_share_1936 post_share_1956, ///
    lags(1) ///
	te ///
    resampling(cshet) ///
    initialization(bi) ///
    bciters(500) ///
    inference(inf_ci) ///
    infiters(1000)
estimates store bcfe_g_p

* ============================================================================ *
* 5. Tabla comparativa
* ============================================================================ *
estadd local time_fe "Yes" : bcfe_g_b
estadd local muni_fe "Yes" : bcfe_g_b

estadd local time_fe "Yes" : bcfe_g_p
estadd local muni_fe "Yes" : bcfe_g_p

estadd local general "Yes" : bcfe_g_p
estadd local general "Yes" : bcfe_g_b

esttab bcfe_g_b bcfe_g_p using "$output/correction_blankvotes_turnout_v2.tex", ///
    replace fragment ///
    b(%9.3f) se(%9.3f) ///
    star(* 0.10 ** 0.05 *** 0.01) ///
    stats(N muni_fe time_fe general, fmt(0 %s %s %s) ///
    labels("Observations" "Municipality FE" "Time FE" "General elections only")) ///
	order(post_share_1936 post_share_1956 L.porcentaje_blanco L.participacion) ///
    keep(L.porcentaje_blanco L.participacion post_share_1936 post_share_1956) ///
    mtitles("Blank vote share" "Voter turnout") ///
    varlabels(L.porcentaje_blanco "Lag(blank vote share)" ///
	          L.participacion "Lag(turnout)" ///
              post_share_1936 "Spanish share 1936--1955 $\times$ Post" ///
              post_share_1956 "Spanish share 1956--1978 $\times$ Post") ///
    nonotes ///
    prehead("\begin{table}[htbp]" ///
            "\centering" ///
            "\caption{Bias-corrected fixed-effects estimates}" ///
            "\label{tab:dynamic_correction}" ///
            "\begin{tabular}{l*{2}{c}}" ///
            "\hline") ///
    postfoot("\hline" ///
             "\end{tabular}" ///
			 "\captionsetup{justification = justified}" ///
             "\caption*{\footnotesize \textit{Notes:} Both columns report estimates from the bootstrap-based bias-corrected fixed-effects estimator (BCFE) of De Vos, Everaert, and Ruyssen (2015). The dependent variable in column (1) is the share of blank votes over total voters; the dependent variable in column (2) is voter turnout. The sample is restricted to general elections. The specification includes municipality fixed effects, time fixed effects, and one lag of the dependent variable. Prior to estimation we tested the stationarity of each dependent variable using the Im-Pesaran-Shin (2003) test and the Fisher-type augmented Dickey-Fuller test (Choi 2001); both reject the null of a unit root at the 1\% level for both outcomes. The bias-correction step uses 500 bootstrap iterations. Confidence intervals are computed from the 2.5th and 97.5th percentiles of the bootstrap distribution over 1,000 additional iterations, with residuals resampled with replacement within each panel unit. * $p<0.10$, ** $p<0.05$, *** $p<0.01$. \\ \textit{Sources}: National Electoral Commission (dependent variables); IPUMS Argentine Census 1970 (Spanish immigration shares).}" ///
             "\end{table}")
