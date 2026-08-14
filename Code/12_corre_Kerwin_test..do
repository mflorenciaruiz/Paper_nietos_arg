* ==============================================================================
*          Test conjunto de balance (Kerwin, Sterck & Rostom, 2024)
*
* Nota: se usan caracteristicas municipales del Censo 2010 sobre Spanish shares
* ==============================================================================

clear all
set more off

* --- Paths ---
global main      "/Users/florenciaruiz/BID 2/Paper Valerie/Nietos/Argentina/Paper_nietos_arg"
global data_int  "$main/Data Int"
global data_out  "$main/Data Out"

* --- Instalar ritest si hace falta ---
cap which ritest
if _rc {
    net install ritest, from("https://raw.githubusercontent.com/simonheb/ritest/master/") replace
}

* ============================================================================ *
* 1. Cargar bases y hacer el merge
* ============================================================================ *

* --- Spanish shares (viene del código 1) ---
use "$data_out/spanish_cohorts_arg.dta", clear

keep mun_code share_1936_1955 share_1956_1978

tempfile shares
save `shares'

* --- Censo 2010 a nivel municipal ---
use "$data_int/censo_2010_arg_mun.dta", clear

* --- Merge ---
merge 1:1 mun_code using `shares'
drop _merge

* ==========================================================================
* 2. Definir covariables de balance (las 6 variables censales de Figura A5)
* ==========================================================================

global balance_vars ///
    mean_yrschool    ///
    median_age       ///
    popdensgeo2      ///
    share_female     ///
    share_laborforce ///
    share_unemployed

* Sacar municipios con missing en cualquier covariable o en las shares
egen missing_any = rowmiss($balance_vars share_1936_1955 share_1956_1978)
count if missing_any > 0 // no hay municipios con missings
if r(N) > 0 {
    display as text "Se eliminan " r(N) " municipios con missings"
    drop if missing_any > 0
}
drop missing_any

count
display as text "Municipios en el analisis: " r(N)

* ==========================================================================
* 3. Test F clasico (baseline, para comparar)
* ==========================================================================

display _newline "--- Spanish share 1936-1955 ---"
reg share_1936_1955 $balance_vars , robust
test $balance_vars
scalar F_36           = r(F)
scalar p_classical_36 = r(p)
display "F = " %7.3f F_36 ",  p (F clasica) = " %6.4f p_classical_36

display _newline "--- Spanish share 1956-1978 ---"
reg share_1956_1978 $balance_vars , robust
test $balance_vars
scalar F_56           = r(F)
scalar p_classical_56 = r(p)
display "F = " %7.3f F_56 ",  p (F clasica) = " %6.4f p_classical_56

* ==========================================================================
* 4. Test Kerwin (randomization inference)
* ==========================================================================

* --- Ventana 1936-1955 ---
display _newline "--- Spanish share 1936-1955 ---"
ritest share_1936_1955 e(F), reps(100) seed(12345) : ///
    reg share_1936_1955 $balance_vars
matrix M36 = r(p)
scalar p_ri_36 = M36[1,1]

* --- Ventana 1956-1978 ---
display _newline "--- Spanish share 1956-1978 ---"
ritest share_1956_1978 e(F), reps(100) seed(12345) : ///
    reg share_1956_1978 $balance_vars
matrix M56 = r(p)
scalar p_ri_56 = M56[1,1]

* ==========================================================================
* 5. Resumen final
* ==========================================================================

display _newline "=================================================="
display "RESUMEN"
display "=================================================="
display "Variables incluidas: $balance_vars "
display "Spanish share 1936-1955:"
display "  F                    = " %7.3f F_36
display "  p-valor F clasico    = " %6.4f p_classical_36
display "  p-valor Kerwin (RI)  = " %6.4f p_ri_36
display "Spanish share 1956-1978:"
display "  F                    = " %7.3f F_56
display "  p-valor F clasico    = " %6.4f p_classical_56
display "  p-valor Kerwin (RI)  = " %6.4f p_ri_56
display "=================================================="
