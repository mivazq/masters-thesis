////////////////////////////////////////////////////////////////////////////////
* File name:        clean_deflators.do
* Author:           Miguel Vázquez Vázquez
* Creation date:    20 November 2023
* Description:		This do file prepares the deflators that I will use to go 
*                   from revenues and costs to quantities. I will prepare the
*                   deflators at different ISIC level codes by using Producer
*                   Price Indexes.
*                   The deflator information was downloaded from Ecuador's
*                   Statistical Institute (INEC - Instituto Nacional de Estadistica 
*                   y Censos) using the following link (which changes with time
*                   as new data is added):
*                   "https://www.ecuadorencifras.gob.ec/documentos/web-inec/Estadisticas_Economicas/IPP/2023/Noviembre/4.Tabulados_series_hist_2023_11_Excel.zip" 
*                   Alternatively, navigate to 
*                   "https://www.ecuadorencifras.gob.ec/indice-de-precios-al-productor-de-disponibilidad-nacional/"
*                   and then click on "Excel" under "Tabulados y series historicas".
*                   Unfortunately, this data uses ISIC Rev 4 classification while 
*                   our data uses ISIC Rev 3.1, so I have to crosswalk them.
*                   To do this I download a crosswalk table prepared by the United
*                   Nations which can be found at this link:
*                   "https://unstats.un.org/unsd/classifications/Econ/tables/ISIC/ISIC4_ISIC31/ISIC4_ISIC31.txt"
* Input:
*                   $ecuRaw/prices/DECON_03_IPPDN_Serie_historica_CIIU_2023_11.xlsx
*                   $ecuRaw/nomenclatures/crosswalk_4_3.1.txt
* Output:
*                   $pathEst/input/isic_codes.csv
////////////////////////////////////////////////////////////////////////////////
quietly do "~/data/transactions_ecuador/3_mivazq/Masters_Thesis/setup.do"
////////////////////////////////////////////////////////////////////////////////

* Load PPI data
import excel "$ecuRaw/prices/6-Indices_Var_Total_(Nac_Expor)_CIIU_3.xlsx", sheet("INDICES CIIU-3") cellrange(A6:GY302) case(lower) allstring clear

* Make variable names viable Stata variable names and rename
foreach var of varlist * {
    qui replace `var' = subinstr(`var',".","_",.) if _n==1
    qui replace `var' = subinstr(`var',"-","_",.) if _n==1
    qui replace `var' = subinstr(`var'," ","_",.) if _n==1
}
foreach var of varlist * {
    local varname : di `var'[1]
    rename `var' `varname'
}
drop if _n==1
rename (Código_CIIU_3 DESCRIPCIÓN) (code description)

* Keep only necessary rows
drop if missing(Ene_08)

* Keep only necessary years
keep code description *_07 *_08 *_09 *_10 *_11 *_12 *_13 *_14 *_15

* Destring index values
qui destring *_07 *_08 *_09 *_10 *_11 *_12 *_13 *_14 *_15, replace

* Generate year indexes by taking averages across the 12 months (this is also how INEC does)
foreach val in 07 08 09 10 11 12 13 14 15 {
    egen year_20`val' = rowmean(*_`val')
}

* Drop monthly values
drop *_07 *_08 *_09 *_10 *_11 *_12 *_13 *_14 *_15

* Adjust index to be base 2007 (to see evolution for all years)
foreach val in 08 09 10 11 12 13 14 15 {
    gen adj_year_20`val' = year_20`val'/year_2007*100
}
drop year_*

* Generate variable to merge
gen rev3 = substr(code, 2, .)
replace rev3 = code if missing(rev3)
replace rev3 = "Total" if code=="T"

* Keep only class level 
keep if strlen(rev3)==4

* Load ISIC codes data 
preserve
    import delimited $ecuRaw/nomenclatures/crosswalk_3_3.1.txt, encoding(UTF8) varnames(1) stringcols(_all) clear
    tempfile codes
    save `codes'
restore

* Merge with crosswalk
merge 1:m rev3 using `codes', assert(match using) keep(match) keepusing(rev31) nogen
drop description

* Collapse at rev3.1 level by taking means when needed
gen section = substr(code,1,1)
replace rev31 = section+rev31
gcollapse (mean) adj*, by(rev31)
rename rev31 code



* Export deflators
reshape long adj_year_, i(code) j(year)
drop n
rename adj_year_ deflator
export delimited $pathEst/input/deflators.csv, replace



/*
* Load PPI data
import excel $ecuRaw/prices/DECON_03_IPPDN_Serie_historica_CIIU_2023_11.xlsx, sheet("1. Indice Nacional") cellrange(A8:LB175) case(lower) allstring clear

* Make variable names viable Stata variable names and rename
foreach var of varlist * {
    qui replace `var' = subinstr(`var',"-","_",.) if _n==1
}
foreach var of varlist * {
    local varname : di `var'[1]
    rename `var' `varname'
}
drop if _n==1
rename (Nivel CIIU Descripción) (level code description)

* Keep only necessary years
keep level code description *_08 *_09 *_10 *_11 *_12 *_13 *_14 *_15

* Destring index values
qui destring *_08 *_09 *_10 *_11 *_12 *_13 *_14 *_15, replace

* Generate year indexes by taking averages across the 12 months (this is also how INEC does)
foreach val in 08 09 10 11 12 13 14 15 {
    egen year_20`val' = rowmean(*_`val')
}

* Drop monthly values
drop *_08 *_09 *_10 *_11 *_12 *_13 *_14 *_15

* Generate variable to merge
gen isic4code = substr(code, 2, .)
replace isic4code = code if missing(isic4code) | code=="Total"

* Load ISIC codes data 
preserve
    import delimited $ecuRaw/nomenclatures/crosswalk_4_3.1.txt, encoding(UTF8) varnames(1) stringcols(_all) clear
    tempfile codes
    save `codes'
restore

* Merge with crosswalk
merge 1:m isic4code using `codes', keep(master match)

* Let's see if they have 3.1 ... otherwise it will be a pain in the ass

* For now export 1 general deflator
keep if level=="0"
drop level code description isic4code
gen n = 1
reshape long year_, i(n) j(year)
drop n
rename year_ deflator
export delimited $pathEst/input/deflators.csv
*/
