/*******************************************************************************
                            Código 6: Data LAPOP
						   
Fecha: 15 mayo 2026
Objetivo: Limpiar la data de LAPOP de las rondas 2012-2023 y unir las rondas

*******************************************************************************/

global main "C:\Users\pilih\Documents\Papers German\Valerie\Paper_nietos_arg"
global data_raw_lapop "$main/Data Raw/LAPOP"
global data_int "$main/Data Int"
global data_out "$main/Data Out"

* -------------------- *
* 1. Lapop 2023
* -------------------- *
{
use "$data_raw_lapop/2023/lapop_2023", clear

keep idnum prov municipio year q1tc_r q2 edre upm strata wt ur ocup4a etid q11n /// // características de personas/municipios
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
 * Este es el único año que no se preguntaron los años de educación, armamos categorías equivalentes en los demás años

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
drop q14f


	* Ruralidad
* UR: 1 Urbano, 2 Rural
tab ur, missing

gen rural = .
	replace rural = 0 if ur == 1
	replace rural = 1 if ur == 2

label define rural_lbl 0 "Urbano" 1 "Rural", replace
label values rural rural_lbl

drop ur

	* Trabajo
/*
Situación laboral para análisis de migración y votaciones.

ocup4a:
1 = Trabajando
2 = No trabaja en este momento, pero tiene trabajo
3 = Está buscando trabajo activamente
4 = Estudiante
5 = Quehaceres del hogar
6 = Jubilado/a, pensionado/a o incapacitado/a
7 = No trabaja y no busca trabajo

Supuestos:
1. ocup4a == 1 o 2 se clasifica como ocupado/a.
2. ocup4a == 3 se clasifica como desempleado/a.
3. ocup4a == 4 se separa como estudiante.
4. ocup4a == 5, 6 o 7 se agrupa como otro fuera de la fuerza laboral.
*/

gen sit_lab_mig = .
replace sit_lab_mig = 1 if inlist(ocup4a, 1, 2)
replace sit_lab_mig = 2 if ocup4a == 3
replace sit_lab_mig = 3 if ocup4a == 4
replace sit_lab_mig = 4 if inlist(ocup4a, 5, 6, 7)

label define sit_lab_mig_lbl ///
    1 "Ocupado/a" ///
    2 "Desempleado/a" ///
    3 "Estudiante" ///
    4 "Otro fuera de fuerza laboral", replace

label values sit_lab_mig sit_lab_mig_lbl

gen desempleado = .
replace desempleado = 1 if ocup4a == 3
replace desempleado = 0 if inlist(ocup4a, 1, 2, 4, 5, 6, 7)

gen ocupado = .
replace ocupado = 1 if inlist(ocup4a, 1, 2)
replace ocupado = 0 if inlist(ocup4a, 3, 4, 5, 6, 7)

gen estudiante = .
replace estudiante = 1 if ocup4a == 4
replace estudiante = 0 if inlist(ocup4a, 1, 2, 3, 5, 6, 7)

drop ocup4a

	* Etnia
/*
Etnicidad / autoidentificación étnico-racial en Argentina:
Se construye a partir de etid.

Categorías originales:
1 = Blanca
2 = Mestiza
3 = Indígena
4 = Negra
5 = Mulata
6 = Otra

Supuestos:
1. La variable captura autoidentificación étnico-racial, no ascendencia objetiva.
2. Dado el tamaño reducido de algunas categorías en Argentina, se agrupan
   categorías minoritarias para evitar celdas pequeñas.
3. `etnia_minoritaria` identifica personas que no se autoidentifican como
   blancas o mestizas. Agrupa indígena, negra, mulata y otra.
4. La categoría minoritaria es heterogénea y debe interpretarse como control
   agregado, no como un grupo sustantivamente homogéneo.
*/

gen etnia_arg = .
replace etnia_arg = 1 if etid == 1
replace etnia_arg = 2 if etid == 2
replace etnia_arg = 3 if inlist(etid, 3, 4, 5, 6)

label define etnia_arg_lbl ///
    1 "Blanca" ///
    2 "Mestiza" ///
    3 "Otra autoidentificación", replace
label values etnia_arg etnia_arg_lbl

gen etnia_minoritaria = .
replace etnia_minoritaria = 0 if inlist(etid, 1, 2)
replace etnia_minoritaria = 1 if inlist(etid, 3, 4, 5, 6)

label define etnia_minoritaria_lbl ///
    0 "Blanca o mestiza" ///
    1 "Otra autoidentificación étnico-racial", replace
label values etnia_minoritaria etnia_minoritaria_lbl

tab year etnia_arg, missing
tab year etnia_minoritaria, missing

drop etid

	* Parejas
/*
Estado civil:
Se construye a partir de q11.

Categorías originales:
1 = Soltero
2 = Casado
3 = Unión libre / convive / vive en pareja
4 = Divorciado
5 = Separado
6 = Viudo
7 = Unión civil

Supuestos:
1. Se clasifica como "en pareja" a personas casadas, en unión libre
   o en unión civil.
2. Se clasifica como "no en pareja" a personas solteras, divorciadas,
   separadas o viudas.
3. La variable captura estado civil o situación convivencial declarada,
   no necesariamente composición efectiva del hogar.
4. Para migración, `en_pareja` se interpreta como proxy de arraigo familiar
   o restricciones familiares potenciales.
*/

rename q11n q11

gen estado_civil = .
replace estado_civil = 1 if q11 == 1
replace estado_civil = 2 if inlist(q11, 2, 3, 7)
replace estado_civil = 3 if inlist(q11, 4, 5)
replace estado_civil = 4 if q11 == 6

label define estado_civil_lbl ///
    1 "Soltero/a" ///
    2 "Casado/a o en unión" ///
    3 "Separado/a o divorciado/a" ///
    4 "Viudo/a", replace
label values estado_civil estado_civil_lbl

gen en_pareja = .
replace en_pareja = 1 if inlist(q11, 2, 3, 7)
replace en_pareja = 0 if inlist(q11, 1, 4, 5, 6)

label define en_pareja_lbl 0 "No en pareja" 1 "En pareja", replace
label values en_pareja en_pareja_lbl

tab year estado_civil, missing
tab year en_pareja, missing

drop q11

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

keep idnum prov municipio fecha q1 q2 ed upm estratopri wt ur ocup4a etid q11 /// // características de personas/municipios
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

	* Ruralidad
* UR: 1 Urbano, 2 Rural
tab ur, missing

gen rural = .
	replace rural = 0 if ur == 1
	replace rural = 1 if ur == 2

label define rural_lbl 0 "Urbano" 1 "Rural", replace
label values rural rural_lbl

drop ur

	* Trabajo
/*
Situación laboral para análisis de migración y votaciones.

ocup4a:
1 = Trabajando
2 = No trabaja en este momento, pero tiene trabajo
3 = Está buscando trabajo activamente
4 = Estudiante
5 = Quehaceres del hogar
6 = Jubilado/a, pensionado/a o incapacitado/a
7 = No trabaja y no busca trabajo

Supuestos:
1. ocup4a == 1 o 2 se clasifica como ocupado/a.
2. ocup4a == 3 se clasifica como desempleado/a.
3. ocup4a == 4 se separa como estudiante.
4. ocup4a == 5, 6 o 7 se agrupa como otro fuera de la fuerza laboral.
*/

gen sit_lab_mig = .
replace sit_lab_mig = 1 if inlist(ocup4a, 1, 2)
replace sit_lab_mig = 2 if ocup4a == 3
replace sit_lab_mig = 3 if ocup4a == 4
replace sit_lab_mig = 4 if inlist(ocup4a, 5, 6, 7)

label define sit_lab_mig_lbl ///
    1 "Ocupado/a" ///
    2 "Desempleado/a" ///
    3 "Estudiante" ///
    4 "Otro fuera de fuerza laboral", replace

label values sit_lab_mig sit_lab_mig_lbl

gen desempleado = .
replace desempleado = 1 if ocup4a == 3
replace desempleado = 0 if inlist(ocup4a, 1, 2, 4, 5, 6, 7)

gen ocupado = .
replace ocupado = 1 if inlist(ocup4a, 1, 2)
replace ocupado = 0 if inlist(ocup4a, 3, 4, 5, 6, 7)

gen estudiante = .
replace estudiante = 1 if ocup4a == 4
replace estudiante = 0 if inlist(ocup4a, 1, 2, 3, 5, 6, 7)

drop ocup4a

	* Etnia
/*
Etnicidad / autoidentificación étnico-racial en Argentina:
Se construye a partir de etid.

Categorías originales:
1 = Blanca
2 = Mestiza
3 = Indígena
4 = Negra
5 = Mulata
6 = Otra

Supuestos:
1. La variable captura autoidentificación étnico-racial, no ascendencia objetiva.
2. Dado el tamaño reducido de algunas categorías en Argentina, se agrupan
   categorías minoritarias para evitar celdas pequeñas.
3. `etnia_minoritaria` identifica personas que no se autoidentifican como
   blancas o mestizas. Agrupa indígena, negra, mulata y otra.
4. La categoría minoritaria es heterogénea y debe interpretarse como control
   agregado, no como un grupo sustantivamente homogéneo.
*/

gen etnia_arg = .
replace etnia_arg = 1 if etid == 1
replace etnia_arg = 2 if etid == 2
replace etnia_arg = 3 if inlist(etid, 3, 4, 5, 6)

label define etnia_arg_lbl ///
    1 "Blanca" ///
    2 "Mestiza" ///
    3 "Otra autoidentificación", replace
label values etnia_arg etnia_arg_lbl

gen etnia_minoritaria = .
replace etnia_minoritaria = 0 if inlist(etid, 1, 2)
replace etnia_minoritaria = 1 if inlist(etid, 3, 4, 5, 6)

label define etnia_minoritaria_lbl ///
    0 "Blanca o mestiza" ///
    1 "Otra autoidentificación étnico-racial", replace
label values etnia_minoritaria etnia_minoritaria_lbl

tab year etnia_arg, missing
tab year etnia_minoritaria, missing

drop etid

	* Pareja
/*
Estado civil:
Se construye a partir de q11.

Categorías originales:
1 = Soltero
2 = Casado
3 = Unión libre / convive / vive en pareja
4 = Divorciado
5 = Separado
6 = Viudo
7 = Unión civil

Supuestos:
1. Se clasifica como "en pareja" a personas casadas, en unión libre
   o en unión civil.
2. Se clasifica como "no en pareja" a personas solteras, divorciadas,
   separadas o viudas.
3. La variable captura estado civil o situación convivencial declarada,
   no necesariamente composición efectiva del hogar.
4. Para migración, `en_pareja` se interpreta como proxy de arraigo familiar
   o restricciones familiares potenciales.
*/

gen estado_civil = .
replace estado_civil = 1 if q11 == 1
replace estado_civil = 2 if inlist(q11, 2, 3, 7)
replace estado_civil = 3 if inlist(q11, 4, 5)
replace estado_civil = 4 if q11 == 6

label define estado_civil_lbl ///
    1 "Soltero/a" ///
    2 "Casado/a o en unión" ///
    3 "Separado/a o divorciado/a" ///
    4 "Viudo/a", replace
label values estado_civil estado_civil_lbl

gen en_pareja = .
replace en_pareja = 1 if inlist(q11, 2, 3, 7)
replace en_pareja = 0 if inlist(q11, 1, 4, 5, 6)

label define en_pareja_lbl 0 "No en pareja" 1 "En pareja", replace
label values en_pareja en_pareja_lbl

tab year estado_civil, missing
tab year en_pareja, missing

drop q11

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

keep idnum prov municipio fecha q1 q2 ed upm estratopri wt ur ocup4a etid q11 /// // características de personas/municipios
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


	* Ruralidad
* UR: 1 Urbano, 2 Rural
tab ur, missing

gen rural = .
	replace rural = 0 if ur == 1
	replace rural = 1 if ur == 2

label define rural_lbl 0 "Urbano" 1 "Rural", replace
label values rural rural_lbl

drop ur

	* Trabajo
/*
Situación laboral para análisis de migración y votaciones.

ocup4a:
1 = Trabajando
2 = No trabaja en este momento, pero tiene trabajo
3 = Está buscando trabajo activamente
4 = Estudiante
5 = Quehaceres del hogar
6 = Jubilado/a, pensionado/a o incapacitado/a
7 = No trabaja y no busca trabajo

Supuestos:
1. ocup4a == 1 o 2 se clasifica como ocupado/a.
2. ocup4a == 3 se clasifica como desempleado/a.
3. ocup4a == 4 se separa como estudiante.
4. ocup4a == 5, 6 o 7 se agrupa como otro fuera de la fuerza laboral.
*/

gen sit_lab_mig = .
replace sit_lab_mig = 1 if inlist(ocup4a, 1, 2)
replace sit_lab_mig = 2 if ocup4a == 3
replace sit_lab_mig = 3 if ocup4a == 4
replace sit_lab_mig = 4 if inlist(ocup4a, 5, 6, 7)

label define sit_lab_mig_lbl ///
    1 "Ocupado/a" ///
    2 "Desempleado/a" ///
    3 "Estudiante" ///
    4 "Otro fuera de fuerza laboral", replace

label values sit_lab_mig sit_lab_mig_lbl

gen desempleado = .
replace desempleado = 1 if ocup4a == 3
replace desempleado = 0 if inlist(ocup4a, 1, 2, 4, 5, 6, 7)

gen ocupado = .
replace ocupado = 1 if inlist(ocup4a, 1, 2)
replace ocupado = 0 if inlist(ocup4a, 3, 4, 5, 6, 7)

gen estudiante = .
replace estudiante = 1 if ocup4a == 4
replace estudiante = 0 if inlist(ocup4a, 1, 2, 3, 5, 6, 7)

drop ocup4a

	* Etnia
/*
Etnicidad / autoidentificación étnico-racial en Argentina:
Se construye a partir de etid.

Categorías originales:
1 = Blanca
2 = Mestiza
3 = Indígena
4 = Negra
5 = Mulata
6 = Otra

Supuestos:
1. La variable captura autoidentificación étnico-racial, no ascendencia objetiva.
2. Dado el tamaño reducido de algunas categorías en Argentina, se agrupan
   categorías minoritarias para evitar celdas pequeñas.
3. `etnia_minoritaria` identifica personas que no se autoidentifican como
   blancas o mestizas. Agrupa indígena, negra, mulata y otra.
4. La categoría minoritaria es heterogénea y debe interpretarse como control
   agregado, no como un grupo sustantivamente homogéneo.
*/

gen etnia_arg = .
replace etnia_arg = 1 if etid == 1
replace etnia_arg = 2 if etid == 2
replace etnia_arg = 3 if inlist(etid, 3, 4, 5, 6)

label define etnia_arg_lbl ///
    1 "Blanca" ///
    2 "Mestiza" ///
    3 "Otra autoidentificación", replace
label values etnia_arg etnia_arg_lbl

gen etnia_minoritaria = .
replace etnia_minoritaria = 0 if inlist(etid, 1, 2)
replace etnia_minoritaria = 1 if inlist(etid, 3, 4, 5, 6)

label define etnia_minoritaria_lbl ///
    0 "Blanca o mestiza" ///
    1 "Otra autoidentificación étnico-racial", replace
label values etnia_minoritaria etnia_minoritaria_lbl

tab year etnia_arg, missing
tab year etnia_minoritaria, missing

drop etid


	* Pareja
/*
Estado civil:
Se construye a partir de q11.

Categorías originales:
1 = Soltero
2 = Casado
3 = Unión libre / convive / vive en pareja
4 = Divorciado
5 = Separado
6 = Viudo
7 = Unión civil

Supuestos:
1. Se clasifica como "en pareja" a personas casadas, en unión libre
   o en unión civil.
2. Se clasifica como "no en pareja" a personas solteras, divorciadas,
   separadas o viudas.
3. La variable captura estado civil o situación convivencial declarada,
   no necesariamente composición efectiva del hogar.
4. Para migración, `en_pareja` se interpreta como proxy de arraigo familiar
   o restricciones familiares potenciales.
*/

gen estado_civil = .
replace estado_civil = 1 if q11 == 1
replace estado_civil = 2 if inlist(q11, 2, 3, 7)
replace estado_civil = 3 if inlist(q11, 4, 5)
replace estado_civil = 4 if q11 == 6

label define estado_civil_lbl ///
    1 "Soltero/a" ///
    2 "Casado/a o en unión" ///
    3 "Separado/a o divorciado/a" ///
    4 "Viudo/a", replace
label values estado_civil estado_civil_lbl

gen en_pareja = .
replace en_pareja = 1 if inlist(q11, 2, 3, 7)
replace en_pareja = 0 if inlist(q11, 1, 4, 5, 6)

label define en_pareja_lbl 0 "No en pareja" 1 "En pareja", replace
label values en_pareja en_pareja_lbl

tab year estado_civil, missing
tab year en_pareja, missing

drop q11

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

keep idnum prov municipio fecha q1 q2 ed upm estratopri wt ur ocup4a etid q11 /// // características de personas/municipios
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


	* Ruralidad
* UR: 1 Urbano, 2 Rural
tab ur, missing

gen rural = .
	replace rural = 0 if ur == 1
	replace rural = 1 if ur == 2

label define rural_lbl 0 "Urbano" 1 "Rural", replace
label values rural rural_lbl

drop ur

	* Trabajo
/*
Situación laboral para análisis de migración y votaciones.

ocup4a:
1 = Trabajando
2 = No trabaja en este momento, pero tiene trabajo
3 = Está buscando trabajo activamente
4 = Estudiante
5 = Quehaceres del hogar
6 = Jubilado/a, pensionado/a o incapacitado/a
7 = No trabaja y no busca trabajo

Supuestos:
1. ocup4a == 1 o 2 se clasifica como ocupado/a.
2. ocup4a == 3 se clasifica como desempleado/a.
3. ocup4a == 4 se separa como estudiante.
4. ocup4a == 5, 6 o 7 se agrupa como otro fuera de la fuerza laboral.
*/

gen sit_lab_mig = .
replace sit_lab_mig = 1 if inlist(ocup4a, 1, 2)
replace sit_lab_mig = 2 if ocup4a == 3
replace sit_lab_mig = 3 if ocup4a == 4
replace sit_lab_mig = 4 if inlist(ocup4a, 5, 6, 7)

label define sit_lab_mig_lbl ///
    1 "Ocupado/a" ///
    2 "Desempleado/a" ///
    3 "Estudiante" ///
    4 "Otro fuera de fuerza laboral", replace

label values sit_lab_mig sit_lab_mig_lbl

gen desempleado = .
replace desempleado = 1 if ocup4a == 3
replace desempleado = 0 if inlist(ocup4a, 1, 2, 4, 5, 6, 7)

gen ocupado = .
replace ocupado = 1 if inlist(ocup4a, 1, 2)
replace ocupado = 0 if inlist(ocup4a, 3, 4, 5, 6, 7)

gen estudiante = .
replace estudiante = 1 if ocup4a == 4
replace estudiante = 0 if inlist(ocup4a, 1, 2, 3, 5, 6, 7)

drop ocup4a

	* Etnia
/*
Etnicidad / autoidentificación étnico-racial en Argentina:
Se construye a partir de etid.

Categorías originales:
1 = Blanca
2 = Mestiza
3 = Indígena
4 = Negra
5 = Mulata
6 = Otra

Supuestos:
1. La variable captura autoidentificación étnico-racial, no ascendencia objetiva.
2. Dado el tamaño reducido de algunas categorías en Argentina, se agrupan
   categorías minoritarias para evitar celdas pequeñas.
3. `etnia_minoritaria` identifica personas que no se autoidentifican como
   blancas o mestizas. Agrupa indígena, negra, mulata y otra.
4. La categoría minoritaria es heterogénea y debe interpretarse como control
   agregado, no como un grupo sustantivamente homogéneo.
*/

gen etnia_arg = .
replace etnia_arg = 1 if etid == 1
replace etnia_arg = 2 if etid == 2
replace etnia_arg = 3 if inlist(etid, 3, 4, 5, 6)

label define etnia_arg_lbl ///
    1 "Blanca" ///
    2 "Mestiza" ///
    3 "Otra autoidentificación", replace
label values etnia_arg etnia_arg_lbl

gen etnia_minoritaria = .
replace etnia_minoritaria = 0 if inlist(etid, 1, 2)
replace etnia_minoritaria = 1 if inlist(etid, 3, 4, 5, 6)

label define etnia_minoritaria_lbl ///
    0 "Blanca o mestiza" ///
    1 "Otra autoidentificación étnico-racial", replace
label values etnia_minoritaria etnia_minoritaria_lbl

tab year etnia_arg, missing
tab year etnia_minoritaria, missing

drop etid


	* Pareja
/*
Estado civil:
Se construye a partir de q11.

Categorías originales:
1 = Soltero
2 = Casado
3 = Unión libre / convive / vive en pareja
4 = Divorciado
5 = Separado
6 = Viudo
7 = Unión civil

Supuestos:
1. Se clasifica como "en pareja" a personas casadas, en unión libre
   o en unión civil.
2. Se clasifica como "no en pareja" a personas solteras, divorciadas,
   separadas o viudas.
3. La variable captura estado civil o situación convivencial declarada,
   no necesariamente composición efectiva del hogar.
4. Para migración, `en_pareja` se interpreta como proxy de arraigo familiar
   o restricciones familiares potenciales.
*/

gen estado_civil = .
replace estado_civil = 1 if q11 == 1
replace estado_civil = 2 if inlist(q11, 2, 3, 7)
replace estado_civil = 3 if inlist(q11, 4, 5)
replace estado_civil = 4 if q11 == 6

label define estado_civil_lbl ///
    1 "Soltero/a" ///
    2 "Casado/a o en unión" ///
    3 "Separado/a o divorciado/a" ///
    4 "Viudo/a", replace
label values estado_civil estado_civil_lbl

gen en_pareja = .
replace en_pareja = 1 if inlist(q11, 2, 3, 7)
replace en_pareja = 0 if inlist(q11, 1, 4, 5, 6)

label define en_pareja_lbl 0 "No en pareja" 1 "En pareja", replace
label values en_pareja en_pareja_lbl

tab year estado_civil, missing
tab year en_pareja, missing

drop q11

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

keep idnum prov municipio fecha q1 q2 ed upm estratopri wt ur ocup4a etid q11 /// // características de personas/municipios
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

	* Ruralidad
* UR: 1 Urbano, 2 Rural
tab ur, missing

gen rural = .
	replace rural = 0 if ur == 1
	replace rural = 1 if ur == 2

label define rural_lbl 0 "Urbano" 1 "Rural", replace
label values rural rural_lbl

drop ur	 

	* Trabajo
/*
Situación laboral para análisis de migración y votaciones.

ocup4a:
1 = Trabajando
2 = No trabaja en este momento, pero tiene trabajo
3 = Está buscando trabajo activamente
4 = Estudiante
5 = Quehaceres del hogar
6 = Jubilado/a, pensionado/a o incapacitado/a
7 = No trabaja y no busca trabajo

Supuestos:
1. ocup4a == 1 o 2 se clasifica como ocupado/a.
2. ocup4a == 3 se clasifica como desempleado/a.
3. ocup4a == 4 se separa como estudiante.
4. ocup4a == 5, 6 o 7 se agrupa como otro fuera de la fuerza laboral.
*/

gen sit_lab_mig = .
replace sit_lab_mig = 1 if inlist(ocup4a, 1, 2)
replace sit_lab_mig = 2 if ocup4a == 3
replace sit_lab_mig = 3 if ocup4a == 4
replace sit_lab_mig = 4 if inlist(ocup4a, 5, 6, 7)

label define sit_lab_mig_lbl ///
    1 "Ocupado/a" ///
    2 "Desempleado/a" ///
    3 "Estudiante" ///
    4 "Otro fuera de fuerza laboral", replace

label values sit_lab_mig sit_lab_mig_lbl

gen desempleado = .
replace desempleado = 1 if ocup4a == 3
replace desempleado = 0 if inlist(ocup4a, 1, 2, 4, 5, 6, 7)

gen ocupado = .
replace ocupado = 1 if inlist(ocup4a, 1, 2)
replace ocupado = 0 if inlist(ocup4a, 3, 4, 5, 6, 7)

gen estudiante = .
replace estudiante = 1 if ocup4a == 4
replace estudiante = 0 if inlist(ocup4a, 1, 2, 3, 5, 6, 7)

drop ocup4a

	* Etnia
/*
Etnicidad / autoidentificación étnico-racial en Argentina:
Se construye a partir de etid.

Categorías originales:
1 = Blanca
2 = Mestiza * VOLVER A ARMAR SEPARANDO MESTIZO
3 = Indígena
4 = Negra
5 = Mulata
6 = Otra

Supuestos:
1. La variable captura autoidentificación étnico-racial, no ascendencia objetiva.
2. Dado el tamaño reducido de algunas categorías en Argentina, se agrupan
   categorías minoritarias para evitar celdas pequeñas.
3. `etnia_minoritaria` identifica personas que no se autoidentifican como
   blancas o mestizas. Agrupa indígena, negra, mulata y otra.
4. La categoría minoritaria es heterogénea y debe interpretarse como control
   agregado, no como un grupo sustantivamente homogéneo.
*/

gen etnia_arg = .
replace etnia_arg = 1 if etid == 1
replace etnia_arg = 2 if etid == 2
replace etnia_arg = 3 if inlist(etid, 3, 4, 5, 6)

label define etnia_arg_lbl ///
    1 "Blanca" ///
    2 "Mestiza" ///
    3 "Otra autoidentificación", replace
label values etnia_arg etnia_arg_lbl

gen etnia_minoritaria = .
replace etnia_minoritaria = 0 if inlist(etid, 1, 2)
replace etnia_minoritaria = 1 if inlist(etid, 3, 4, 5, 6)

label define etnia_minoritaria_lbl ///
    0 "Blanca o mestiza" ///
    1 "Otra autoidentificación étnico-racial", replace
label values etnia_minoritaria etnia_minoritaria_lbl

tab year etnia_arg, missing
tab year etnia_minoritaria, missing

drop etid


	* Pareja
/*
Estado civil:
Se construye a partir de q11.

Categorías originales:
1 = Soltero
2 = Casado
3 = Unión libre / convive / vive en pareja
4 = Divorciado
5 = Separado
6 = Viudo
7 = Unión civil

Supuestos:
1. Se clasifica como "en pareja" a personas casadas, en unión libre
   o en unión civil.
2. Se clasifica como "no en pareja" a personas solteras, divorciadas,
   separadas o viudas.
3. La variable captura estado civil o situación convivencial declarada,
   no necesariamente composición efectiva del hogar.
4. Para migración, `en_pareja` se interpreta como proxy de arraigo familiar
   o restricciones familiares potenciales.
*/

gen estado_civil = .
replace estado_civil = 1 if q11 == 1
replace estado_civil = 2 if inlist(q11, 2, 3, 7)
replace estado_civil = 3 if inlist(q11, 4, 5)
replace estado_civil = 4 if q11 == 6

label define estado_civil_lbl ///
    1 "Soltero/a" ///
    2 "Casado/a o en unión" ///
    3 "Separado/a o divorciado/a" ///
    4 "Viudo/a", replace
label values estado_civil estado_civil_lbl

gen en_pareja = .
replace en_pareja = 1 if inlist(q11, 2, 3, 7)
replace en_pareja = 0 if inlist(q11, 1, 4, 5, 6)

label define en_pareja_lbl 0 "No en pareja" 1 "En pareja", replace
label values en_pareja en_pareja_lbl

tab year estado_civil, missing
tab year en_pareja, missing

drop q11

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

*svyset upm
*[pw=weight1500], strata(strata)

* -------------------- *
* 8. Edito en todas
* -------------------- *

use "$data_int/lapop_append.dta", clear

/*
Nota sobre armonización educativa:
Las rondas 2012, 2014, 2017 y 2019 reportan años de educación (`anios_educ`).
La ronda 2023 reporta nivel educativo categórico (`nivel_educ`, proveniente de edre).

Para hacer las rondas comparables, se construye `nivel_educ7`.

Supuestos para 2012-2019:
0 años     = ninguna
1-5 años   = primaria incompleta
6 años     = primaria completa
7-11 años  = secundaria incompleta
12 años    = secundaria completa
13-15 años = terciaria/universitaria incompleta
16-17 años = terciaria/universitaria completa

Para 2023:
Se usa directamente la codificación de edre:
0 = ninguna
1 = primaria incompleta
2 = primaria completa
3 = secundaria incompleta
4 = secundaria completa
5 = terciaria/universitaria incompleta
6 = terciaria/universitaria completa

La variable debe interpretarse como una aproximación armonizada entre rondas.
En 2012-2019 no se observa directamente si la persona obtuvo el título,
sino años acumulados de educación.
*/

gen nivel_educ7 = .

* 2012-2019: a partir de años de educación
replace nivel_educ7 = 0 if inlist(year, 2012, 2014, 2017, 2019) & anios_educ == 0
replace nivel_educ7 = 1 if inlist(year, 2012, 2014, 2017, 2019) & inrange(anios_educ, 1, 5)
replace nivel_educ7 = 2 if inlist(year, 2012, 2014, 2017, 2019) & anios_educ == 6
replace nivel_educ7 = 3 if inlist(year, 2012, 2014, 2017, 2019) & inrange(anios_educ, 7, 11)
replace nivel_educ7 = 4 if inlist(year, 2012, 2014, 2017, 2019) & anios_educ == 12
replace nivel_educ7 = 5 if inlist(year, 2012, 2014, 2017, 2019) & inrange(anios_educ, 13, 15)
replace nivel_educ7 = 6 if inlist(year, 2012, 2014, 2017, 2019) & inrange(anios_educ, 16, 17)

* 2023: a partir de nivel educativo categórico edre
replace nivel_educ7 = nivel_educ if year == 2023 & inrange(nivel_educ, 0, 6)

label define nivel_educ7_lbl ///
	0 "Ninguna" ///
	1 "Primaria incompleta" ///
	2 "Primaria completa" ///
	3 "Secundaria incompleta" ///
	4 "Secundaria completa" ///
	5 "Terciaria/universitaria incompleta" ///
	6 "Terciaria/universitaria completa", replace

label values nivel_educ7 nivel_educ7_lbl

* Dummies educativas
gen primaria_completa_o_mas = .
replace primaria_completa_o_mas = 1 if inrange(nivel_educ7, 2, 6)
replace primaria_completa_o_mas = 0 if inrange(nivel_educ7, 0, 1)

gen secundaria_completa_o_mas = .
replace secundaria_completa_o_mas = 1 if inrange(nivel_educ7, 4, 6)
replace secundaria_completa_o_mas = 0 if inrange(nivel_educ7, 0, 3)

gen superior_incompleta_o_mas = .
replace superior_incompleta_o_mas = 1 if inrange(nivel_educ7, 5, 6)
replace superior_incompleta_o_mas = 0 if inrange(nivel_educ7, 0, 4)

gen superior_completa = .
replace superior_completa = 1 if nivel_educ7 == 6
replace superior_completa = 0 if inrange(nivel_educ7, 0, 5)

tab year nivel_educ7, missing
tab year secundaria_completa_o_mas, missing
tab year superior_incompleta_o_mas, missing

save "$data_int/lapop_append.dta", replace

