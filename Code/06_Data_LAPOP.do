/*******************************************************************************
                            Código 6: Data LAPOP
						   
Fecha: 15 mayo 2026
Objetivo: Limpiar la data de LAPOP de las rondas 2012-2023 y unir las rondas

*******************************************************************************/

global main "/Users/florenciaruiz/BID 2/Paper Valerie/Nietos/Argentina/Paper_nietos_arg"
global data_raw_lapop "$main/Data Raw/LAPOP"
global data_int "$main/Data Int"
global data_out "$main/Data Out"

* -------------------- *
* 1. Lapop 2023
* -------------------- *
{
use "$data_raw_lapop/2023/lapop_2023", clear

keep idnum prov municipio year q1tc_r q2 edre upm strata wt  /// // características de personas/municipios
     q14 q14dnew q14f /// // Intenciones de migrar
	 pol1 cp8 vb2 vb20 vb10 pra4n l1n // Participación, interés político e Ideología
	  
* Limpieza de variables
	
	* Identidicador del cuestionario
tostring idnum, gen(idnum_str) format(%20.0f)
drop idnum
rename idnum_str idnum

	* Provincia
gen prov_cod = prov
decode prov, gen(prov_nom)
drop prov

	* Municipio
gen municipio_cod = municipio
decode municipio, gen(municipio_nom)
drop municipio
order idnum prov_cod prov_nom municipio_cod municipio_nom year

	* Sexo (hombre = 1; mujer. = 0)
tab q1tc_r
label list q1tc_r_es
gen hombre = .
	replace hombre = 1 if q1tc_r ==1
	replace hombre = 0 if q1tc_r ==2
drop q1tc_r
	
	* Edad
tab q2
rename q2 edad

	* Educación
tab edre
label list edre_es
rename edre nivel_educ
replace nivel_educ =. if nivel_educ == .a | nivel_educ == .b

	* Asiste a reuniones de un comité o junta de mejoras en la comunidad
tab cp8
label list cp8_es
gen reuniones_comunidad = .
	replace reuniones_comunidad = 1 if cp8 == 1 | cp8 == 2 | cp8 == 3
	replace reuniones_comunidad = 0 if cp8 == 4
drop cp8

	* Escala izquierda derecha
tab l1n
label list l1n_es
gen izq_der = l1n
	replace izq_der = . if l1n == .a | l1n == .b
tab izq_der
drop l1n

	* Registro de votaciones (en la presidencial de 2018)
tab vb2
label list vb2_es
gen voto_anterior = .
	replace voto_anterior = 1 if vb2 == 1
	replace voto_anterior = 0 if vb2 == 2
drop vb2

	* Identificación con partido político
tab vb10
label list vb10_es
gen identifica_partido = .
	replace identifica_partido = 1 if vb10 == 1
	replace identifica_partido = 0 if vb10 == 2
drop vb10

	* Interés en la política
tab pol1
label list pol1_es
gen interes_pol_mucho = .
	replace interes_pol_mucho = 1 if pol1 == 1
	replace interes_pol_mucho = 0 if pol1 == 2 | pol1 == 3 | pol1 == 4
gen interes_pol_algo = .
	replace interes_pol_algo = 1 if pol1 == 2
	replace interes_pol_algo = 0 if pol1 == 1 | pol1 == 3 | pol1 == 4
gen interes_pol_poco = .
	replace interes_pol_poco = 1 if pol1 == 3
	replace interes_pol_poco = 0 if pol1 == 1 | pol1 == 2 | pol1 == 4
gen interes_pol_nada = .
	replace interes_pol_nada = 1 if pol1 == 4
	replace interes_pol_nada = 0 if pol1 == 1 | pol1 == 2 | pol1 == 3
drop  pol1

	* Intención de voto
tab vb20
label list vb20_es
gen intencion_voto = .
	replace intencion_voto = 1 if vb20 == 2 | vb20 == 3 | vb20 == 4 // voto o voto en blanco o nulo
	replace intencion_voto = 0 if vb20 == 1
gen voto_blanco_nulo = .
	replace voto_blanco_nulo = 1 if vb20 == 4
	replace voto_blanco_nulo = 0 if vb20 == 2 | vb20 == 3 // de los que vota a algún candidato
drop vb20
	
	* Reflecciones sobre postularse a un cargo público
tab pra4n
label list pra4n_es
gen postularia_cargo = .
	replace postularia_cargo = 1 if pra4n == 1
	replace postularia_cargo = 0 if pra4n == 2
drop pra4n
	
	* Intención de trabajar o vivir en otro país
tab q14
label list q14_es
gen intencion_migrar = .
	replace intencion_migrar = 1 if q14 == 1
	replace intencion_migrar = 0 if q14 == 2
drop q14

	* País al que tiene la intención de migrar
tab q14dnew
label list q14dnew_es
gen intencion_migrar_esp = 0
	replace intencion_migrar_esp = 1 if q14dnew == 4
	replace intencion_migrar_esp = . if q14dnew == .a | q14dnew == .b | q14dnew == .c
drop q14dnew

	* Probabilidad de trabajar o vivir en otro país (próximos tres años), solo para los que contestaron q14
tab q14f
label list q14f_es
gen migracion_probable = .
	replace migracion_probable = 1 if q14f == 1 | q14f == 2 // muy probable o algo probable
	replace migracion_probable = 0 if q14f == 3 | q14f == 4 // poco probable o nada probable

gen pro_de_migracion = q14f
	replace pro_de_migracion = . if q14f == .a | q14f == .b | q14f == .c
tab pro_de_migracion, m	
drop q14f

* Guardo la data
save "$data_int/lapop_2023_clean.dta", replace

}
* -------------------- *
* 2. Lapop 2021
* -------------------- *
{
use "$data_raw_lapop/2021/lapop_2021", clear
// No tiene intenciónes de migrar ni otras variables relevantes
}
* -------------------- *
* 3. Lapop 2019
* -------------------- *
{
clear
cd "$data_raw_lapop/2018_19"
unicode encoding set latin1
unicode analyze lapop_2019.dta
unicode translate lapop_2019.dta

use lapop_2019.dta, clear

* Guardo una versión limpia
*save lapop_2019_unicode.dta, replace

keep idnum prov municipio fecha q1 q2 ed upm estratopri wt  /// // características de personas/municipios
     q14 /// // Intenciones de migrar
	 pol1 cp8 vb2 vb20 vb10 pra4n l1 // Participación, interés político e Ideología
	  
* Limpieza de variables

	* Identidicador del cuestionario
tostring idnum, gen(idnum_str) format(%20.0f)
drop idnum
rename idnum_str idnum
	
	* Creo que año
tab fecha
gen year = 2019
drop fecha

	* Provincia
gen prov_cod = prov
decode prov, gen(prov_nom)
drop prov

	* Municipio
gen municipio_cod = municipio
decode municipio, gen(municipio_nom)
drop municipio
order idnum prov_cod prov_nom municipio_cod municipio_nom year

	* Sexo (hombre = 1; mujer. = 0)
tab q1
label list q1_esp
gen hombre = .
	replace hombre = 1 if q1 ==1
	replace hombre = 0 if q1 ==2
drop q1
	
	* Edad
tab q2
rename q2 edad

	* Educación
tab ed
label list ed_esp
rename ed anios_educ
replace anios_educ =. if anios_educ == .a | anios_educ == .c | anios_educ == 18

	* Asiste a reuniones de un comité o junta de mejoras en la comunidad
tab cp8
label list cp8_esp
gen reuniones_comunidad = .
	replace reuniones_comunidad = 1 if cp8 == 1 | cp8 == 2 | cp8 == 3
	replace reuniones_comunidad = 0 if cp8 == 4
drop cp8

	* Escala izquierda derecha
tab l1
label list l1_esp
gen izq_der = l1
	replace izq_der = . if l1 == .a | l1 == .b | l1 == .c
tab izq_der
drop l1

	* Registro de votaciones (en la presidenciales)
tab vb2
label list vb2_esp
gen voto_anterior = .
	replace voto_anterior = 1 if vb2 == 1
	replace voto_anterior = 0 if vb2 == 2
drop vb2

	* Identificación con partido político
tab vb10
label list vb10_esp
gen identifica_partido = .
	replace identifica_partido = 1 if vb10 == 1
	replace identifica_partido = 0 if vb10 == 2
drop vb10

	* Interés en la política
tab pol1
label list pol1_esp
gen interes_pol_mucho = .
	replace interes_pol_mucho = 1 if pol1 == 1
	replace interes_pol_mucho = 0 if pol1 == 2 | pol1 == 3 | pol1 == 4
gen interes_pol_algo = .
	replace interes_pol_algo = 1 if pol1 == 2
	replace interes_pol_algo = 0 if pol1 == 1 | pol1 == 3 | pol1 == 4
gen interes_pol_poco = .
	replace interes_pol_poco = 1 if pol1 == 3
	replace interes_pol_poco = 0 if pol1 == 1 | pol1 == 2 | pol1 == 4
gen interes_pol_nada = .
	replace interes_pol_nada = 1 if pol1 == 4
	replace interes_pol_nada = 0 if pol1 == 1 | pol1 == 2 | pol1 == 3
drop  pol1

	* Intención de voto
tab vb20
label list vb20_esp
gen intencion_voto = .
	replace intencion_voto = 1 if vb20 == 2 | vb20 == 3 | vb20 == 4 // voto o voto en blanco o nulo
	replace intencion_voto = 0 if vb20 == 1
gen voto_blanco_nulo = .
	replace voto_blanco_nulo = 1 if vb20 == 4
	replace voto_blanco_nulo = 0 if vb20 == 2 | vb20 == 3 // de los que vota a algún candidato
drop vb20
	
	* Reflecciones sobre postularse a un cargo público
tab pra4n
label list pra4n_esp
gen postularia_cargo = .
	replace postularia_cargo = 1 if pra4n == 1
	replace postularia_cargo = 0 if pra4n == 2
drop pra4n
	
	* Intención de trabajar o vivir en otro país
tab q14
label list q14_esp
gen intencion_migrar = .
	replace intencion_migrar = 1 if q14 == 1
	replace intencion_migrar = 0 if q14 == 2
drop q14

* Guardo la data
save "$data_int/lapop_2019_clean.dta", replace

}
* -------------------- *
* 4. Lapop 2017
* -------------------- *
{
clear
cd "$data_raw_lapop/2016_17"
unicode encoding set latin1
unicode analyze lapop_2017.dta
unicode translate lapop_2017.dta

use lapop_2017.dta, clear

* Guardo una versión limpia
*save lapop_2017_unicode.dta, replace

keep idnum prov municipio fecha q1 q2 ed upm estratopri wt  /// // características de personas/municipios
     q14 /// // Intenciones de migrar
	 pol1 cp8 vb2 vb20 vb10 pra4 l1 // Participación, interés político e Ideología
	 
* Limpieza de variables

	* Identidicador del cuestionario
tostring idnum, gen(idnum_str) format(%20.0f)
drop idnum
rename idnum_str idnum
	
	* Creo que año
tab fecha
gen year = 2017
drop fecha

	* Provincia
gen prov_cod = prov
decode prov, gen(prov_nom)
drop prov

	* Municipio
gen municipio_cod = municipio
decode municipio, gen(municipio_nom)
drop municipio
order idnum prov_cod prov_nom municipio_cod municipio_nom year

	* Sexo (hombre = 1; mujer. = 0)
tab q1
label list q1_eng
gen hombre = .
	replace hombre = 1 if q1 ==1
	replace hombre = 0 if q1 ==2
drop q1
	
	* Edad
tab q2
rename q2 edad

	* Educación
tab ed
label list ed_eng
rename ed anios_educ
replace anios_educ =. if anios_educ == .a | anios_educ == .c | anios_educ == 18

	* Asiste a reuniones de un comité o junta de mejoras en la comunidad
tab cp8
label list cp8_eng
gen reuniones_comunidad = .
	replace reuniones_comunidad = 1 if cp8 == 1 | cp8 == 2 | cp8 == 3
	replace reuniones_comunidad = 0 if cp8 == 4
drop cp8

	* Escala izquierda derecha
tab l1
label list l1_eng
gen izq_der = l1
	replace izq_der = . if l1 == .a | l1 == .b | l1 == .c
tab izq_der
drop l1

	* Registro de votaciones (en la presidenciales)
tab vb2
label list vb2_eng
gen voto_anterior = .
	replace voto_anterior = 1 if vb2 == 1
	replace voto_anterior = 0 if vb2 == 2
drop vb2

	* Identificación con partido político
tab vb10
label list vb10_eng
gen identifica_partido = .
	replace identifica_partido = 1 if vb10 == 1
	replace identifica_partido = 0 if vb10 == 2
drop vb10

	* Interés en la política
tab pol1
label list pol1_eng
gen interes_pol_mucho = .
	replace interes_pol_mucho = 1 if pol1 == 1
	replace interes_pol_mucho = 0 if pol1 == 2 | pol1 == 3 | pol1 == 4
gen interes_pol_algo = .
	replace interes_pol_algo = 1 if pol1 == 2
	replace interes_pol_algo = 0 if pol1 == 1 | pol1 == 3 | pol1 == 4
gen interes_pol_poco = .
	replace interes_pol_poco = 1 if pol1 == 3
	replace interes_pol_poco = 0 if pol1 == 1 | pol1 == 2 | pol1 == 4
gen interes_pol_nada = .
	replace interes_pol_nada = 1 if pol1 == 4
	replace interes_pol_nada = 0 if pol1 == 1 | pol1 == 2 | pol1 == 3
drop  pol1

	* Intención de voto
tab vb20
label list vb20_eng
gen intencion_voto = .
	replace intencion_voto = 1 if vb20 == 2 | vb20 == 3 | vb20 == 4 // voto o voto en blanco o nulo
	replace intencion_voto = 0 if vb20 == 1
gen voto_blanco_nulo = .
	replace voto_blanco_nulo = 1 if vb20 == 4
	replace voto_blanco_nulo = 0 if vb20 == 2 | vb20 == 3 // de los que vota a algún candidato
drop vb20
	
	* Reflecciones sobre postularse a un cargo público
tab pra4
label list pra4_eng
gen postularia_cargo = .
	replace postularia_cargo = 1 if pra4 == 1
	replace postularia_cargo = 0 if pra4 == 2
drop pra4
	
	* Intención de trabajar o vivir en otro país
tab q14
label list q14_eng
gen intencion_migrar = .
	replace intencion_migrar = 1 if q14 == 1
	replace intencion_migrar = 0 if q14 == 2
drop q14	 

* Guardo la data
save "$data_int/lapop_2017_clean.dta", replace
	 
}
* -------------------- *
* 5. Lapop 2014
* -------------------- *
{
clear
cd "$data_raw_lapop/2014"
unicode encoding set latin1
unicode analyze lapop_2014.dta
unicode translate lapop_2014.dta

use lapop_2014.dta, clear

* Guardo una versión limpia
*save lapop_2014_unicode.dta, replace

keep idnum prov municipio fecha q1 q2 ed upm estratopri wt  /// // características de personas/municipios
     q14 /// // Intenciones de migrar
	 pol1 cp8 vb2 vb20 vb10 l1 // Participación, interés político e Ideología
	 
* Limpieza de variables
	
	* Creo que año
tab fecha
gen year = 2014
drop fecha 

	* Provincia
gen prov_cod = prov
decode prov, gen(prov_nom)
drop prov

	* Municipio
gen municipio_cod = municipio
decode municipio, gen(municipio_nom)
drop municipio
order idnum prov_cod prov_nom municipio_cod municipio_nom year

	* Sexo (hombre = 1; mujer. = 0)
tab q1
label list q1_esp
gen hombre = .
	replace hombre = 1 if q1 ==1
	replace hombre = 0 if q1 ==2
drop q1
	
	* Edad
tab q2
rename q2 edad

	* Educación
tab ed
label list ed_esp
rename ed anios_educ
replace anios_educ =. if anios_educ == .a | anios_educ == .c | anios_educ == 18

	* Asiste a reuniones de un comité o junta de mejoras en la comunidad
tab cp8
label list cp8_esp
gen reuniones_comunidad = .
	replace reuniones_comunidad = 1 if cp8 == 1 | cp8 == 2 | cp8 == 3
	replace reuniones_comunidad = 0 if cp8 == 4
drop cp8

	* Escala izquierda derecha
tab l1
label list l1_esp
gen izq_der = l1
	replace izq_der = . if l1 == .a | l1 == .b | l1 == .c
tab izq_der
drop l1

	* Registro de votaciones (en la presidenciales)
tab vb2
label list vb2_esp
gen voto_anterior = .
	replace voto_anterior = 1 if vb2 == 1
	replace voto_anterior = 0 if vb2 == 2
drop vb2

	* Identificación con partido político
tab vb10
label list vb10_esp
gen identifica_partido = .
	replace identifica_partido = 1 if vb10 == 1
	replace identifica_partido = 0 if vb10 == 2
drop vb10

	* Interés en la política
tab pol1
label list pol1_esp
gen interes_pol_mucho = .
	replace interes_pol_mucho = 1 if pol1 == 1
	replace interes_pol_mucho = 0 if pol1 == 2 | pol1 == 3 | pol1 == 4
gen interes_pol_algo = .
	replace interes_pol_algo = 1 if pol1 == 2
	replace interes_pol_algo = 0 if pol1 == 1 | pol1 == 3 | pol1 == 4
gen interes_pol_poco = .
	replace interes_pol_poco = 1 if pol1 == 3
	replace interes_pol_poco = 0 if pol1 == 1 | pol1 == 2 | pol1 == 4
gen interes_pol_nada = .
	replace interes_pol_nada = 1 if pol1 == 4
	replace interes_pol_nada = 0 if pol1 == 1 | pol1 == 2 | pol1 == 3
drop  pol1

	* Intención de voto
tab vb20
label list vb20_esp
gen intencion_voto = .
	replace intencion_voto = 1 if vb20 == 2 | vb20 == 3 | vb20 == 4 // voto o voto en blanco o nulo
	replace intencion_voto = 0 if vb20 == 1
gen voto_blanco_nulo = .
	replace voto_blanco_nulo = 1 if vb20 == 4
	replace voto_blanco_nulo = 0 if vb20 == 2 | vb20 == 3 // de los que vota a algún candidato
drop vb20

	
	* Intención de trabajar o vivir en otro país
tab q14
label list q14_esp
gen intencion_migrar = .
	replace intencion_migrar = 1 if q14 == 1
	replace intencion_migrar = 0 if q14 == 2
drop q14	 

* Guardo la data
save "$data_int/lapop_2014_clean.dta", replace
	 
}
* -------------------- *
* 6. Lapop 2012
* -------------------- *
{
clear
cd "$data_raw_lapop/2012"
unicode encoding set latin1
unicode analyze lapop_2012.dta
unicode translate lapop_2012.dta

use lapop_2012.dta, clear

* Guardo una versión limpia
*save lapop_2012_unicode.dta, replace

keep idnum prov municipio fecha q1 q2 ed upm estratopri wt  /// // características de personas/municipios
     q14 /// // Intenciones de migrar
	 pol1 cp8 vb2 vb20 vb10 l1 // Participación, interés político e Ideología
	 
* Limpieza de variables
	
	* Identidicador del cuestionario
tostring idnum, gen(idnum_str) format(%20.0f)
drop idnum
rename idnum_str idnum
	
	* Creo que año
tab fecha
gen year = 2012
drop fecha

	* Provincia
gen prov_cod = prov
decode prov, gen(prov_nom)
drop prov

	* Municipio
gen municipio_cod = municipio
decode municipio, gen(municipio_nom)
drop municipio
order idnum prov_cod prov_nom municipio_cod municipio_nom year

	* Sexo (hombre = 1; mujer. = 0)
tab q1
label list q1
gen hombre = .
	replace hombre = 1 if q1 ==1
	replace hombre = 0 if q1 ==2
drop q1
	
	* Edad
tab q2
rename q2 edad

	* Educación
tab ed
label list ed
rename ed anios_educ
replace anios_educ =. if anios_educ == .a | anios_educ == .c | anios_educ == 18

	* Asiste a reuniones de un comité o junta de mejoras en la comunidad
tab cp8
label list cp8
gen reuniones_comunidad = .
	replace reuniones_comunidad = 1 if cp8 == 1 | cp8 == 2 | cp8 == 3
	replace reuniones_comunidad = 0 if cp8 == 4
drop cp8

	* Escala izquierda derecha
tab l1
label list l1
gen izq_der = l1
	replace izq_der = . if l1 == .a | l1 == .b | l1 == .c
tab izq_der
drop l1

	* Registro de votaciones (en la presidenciales)
tab vb2
label list vb2
gen voto_anterior = .
	replace voto_anterior = 1 if vb2 == 1
	replace voto_anterior = 0 if vb2 == 2
drop vb2

	* Identificación con partido político
tab vb10
label list vb10
gen identifica_partido = .
	replace identifica_partido = 1 if vb10 == 1
	replace identifica_partido = 0 if vb10 == 2
drop vb10

	* Interés en la política
tab pol1
label list pol1
gen interes_pol_mucho = .
	replace interes_pol_mucho = 1 if pol1 == 1
	replace interes_pol_mucho = 0 if pol1 == 2 | pol1 == 3 | pol1 == 4
gen interes_pol_algo = .
	replace interes_pol_algo = 1 if pol1 == 2
	replace interes_pol_algo = 0 if pol1 == 1 | pol1 == 3 | pol1 == 4
gen interes_pol_poco = .
	replace interes_pol_poco = 1 if pol1 == 3
	replace interes_pol_poco = 0 if pol1 == 1 | pol1 == 2 | pol1 == 4
gen interes_pol_nada = .
	replace interes_pol_nada = 1 if pol1 == 4
	replace interes_pol_nada = 0 if pol1 == 1 | pol1 == 2 | pol1 == 3
drop  pol1

	* Intención de voto
tab vb20
label list vb20
gen intencion_voto = .
	replace intencion_voto = 1 if vb20 == 2 | vb20 == 3 | vb20 == 4 // voto o voto en blanco o nulo
	replace intencion_voto = 0 if vb20 == 1
gen voto_blanco_nulo = .
	replace voto_blanco_nulo = 1 if vb20 == 4
	replace voto_blanco_nulo = 0 if vb20 == 2 | vb20 == 3 // de los que vota a algún candidato
drop vb20
	
	* Intención de trabajar o vivir en otro país
tab q14
label list q14
gen intencion_migrar = .
	replace intencion_migrar = 1 if q14 == 1
	replace intencion_migrar = 0 if q14 == 2
drop q14	 

* Guardo la data
save "$data_int/lapop_2012_clean.dta", replace
	 
}
* -------------------- *
* 7. Junto las rondas
* -------------------- *
{
use "$data_int/lapop_2012_clean", clear

append using "$data_int/lapop_2014_clean"
append using "$data_int/lapop_2017_clean"
append using "$data_int/lapop_2019_clean"
append using "$data_int/lapop_2023_clean"

tab year
drop prov_cod municipio_cod

save "$data_int/lapop_append.dta", replace
}

* -------------------- *
* 8. Análisis de las chances de ir a España
* -------------------- *

use "$data_int/lapop_2023_clean.dta", clear

svyset upm [pw=wt], strata(strata)

* Intencion de migrara en general
svy: tab intencion_migrar, percent // --> 24.95%
svy: tab intencion_migrar, count

tab intencion_migrar, m

* Intención de migrar a España
svy: tab intencion_migrar_esp, percent

tab intencion_migrar_esp, m
tab intencion_migrar_esp intencion_migrar, m

* Creo la variable sin missing en los que no tienen intencion de migrar
gen migrar_esp_nm = .
	replace migrar_esp_nm = 1 if intencion_migrar_esp ==1
	replace migrar_esp_nm = 0 if intencion_migrar == 0 | intencion_migrar_esp ==0
tab  migrar_esp_nm
svy: tab migrar_esp_nm, percent // intencion de migrar a españa como % del total de la muestra mayor a 16 --> 9.82%

* Probabilidad de migrara a España. Creo la variable sin missing en los que no tienen intencion de migrar
tab pro_de_migracion, m 
tab pro_de_migracion intencion_migrar, m 
tab pro_de_migracion intencion_migrar_esp, m 

gen prob_mig_esp_nm = pro_de_migracion if intencion_migrar_esp == 1
	replace prob_mig_esp_nm = 5 if intencion_migrar == 0 | intencion_migrar_esp == 0 
tab prob_mig_esp_nm, m

svy: tab prob_mig_esp_nm, percent //   muy probable = 3.297 %, algo probable = 3.903%, poco probable = 2.221%, nada probable = 0.4038%




