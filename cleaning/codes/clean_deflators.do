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
*
*                   https://contenido.bce.fin.ec/documentos/PublicacionesNotas/Catalogo/CuentasNacionales/PIBProduccionCorriente2020p.xlsx
*                   https://contenido.bce.fin.ec/documentos/PublicacionesNotas/Catalogo/CuentasNacionales/PIBProduccionConstante2020p.xlsx
* Input:
*                   $ecuRaw/prices/DECON_03_IPPDN_Serie_historica_CIIU_2023_11.xlsx
*                   $ecuRaw/nomenclatures/crosswalk_4_3.1.txt
* Output:
*                   $pathCle/output/isic_codes.csv
////////////////////////////////////////////////////////////////////////////////
quietly do "~/data/transactions_ecuador/3_mivazq/Masters_Thesis/setup.do"
////////////////////////////////////////////////////////////////////////////////

* First let's create the general GDP deflator for other sections other then A,B,C,D

* GDP data at constant prices
    import excel "$ecuRaw/prices/PIBProduccion.xlsx", sheet("PIB_enfoque_PRODUCCIÓN_k") cellrange(B5:P10) case(lower) allstring clear

    * Make variable names viable Stata variable names and rename
    foreach var of varlist * {
        qui replace `var' = subinstr(`var'," (p*)","",.) if _n==1 // provisory
    }
    foreach var of varlist * {
        local varname : di `var'[1]
        rename `var' y_`varname'
    }
    drop if _n==1

    * Keep only "Production" series (we trying to emulate PPI)
    keep if y_Variable=="Producción"
    drop y_Variable

    * Reshape and save
    gen code = "lorem ipsum"
    reshape long y_, i(code) j(year)
    drop code
    rename (y_) (gdp_constant_prices)
    isid year
    tempfile constant
    save `constant'
    
* GDP data at current prices
    import excel "$ecuRaw/prices/PIBProduccion.xlsx", sheet("PIB_enfoque_PRODUCCIÓN_c") cellrange(B5:P10) case(lower) allstring clear

    * Make variable names viable Stata variable names and rename
    foreach var of varlist * {
        qui replace `var' = subinstr(`var'," (p*)","",.) if _n==1 // provisory
    }
    foreach var of varlist * {
        local varname : di `var'[1]
        rename `var' y_`varname'
    }
    drop if _n==1

    * Keep only "Production" series (we trying to emulate PPI)
    keep if y_Variable=="Producción"
    drop y_Variable

    * Reshape and save
    gen code = "lorem ipsum"
    reshape long y_, i(code) j(year)
    drop code
    rename (y_) (gdp_current_prices)
    isid year
    tempfile current
    save `current'
    
* Merge the two
use `constant'
merge 1:1 year using `current', assert(match) nogen

* Generate general PPI
destring gdp_*, replace
gen ppi = gdp_current_prices/gdp_constant_prices*100

* Ensure 2007 is already base year (i.e. constant==current)
assert ppi==100 if year==2007
drop gdp_current_prices gdp_constant_prices

* Keep only 2008-2011
keep if year>=2008 & year<=2011

* Save
save $pathCle/output/deflators_general.dta, replace

////////////////////////////////////////////////////////////////////////////////

* Now let's prepare sections of Rev 3 and Rev 3.1 to merge in with the
* crosswalks which only have numbers (thus no section information)

* Prepare ISIC Rev3 sections to be merged into crosswalk file
import delimited $ecuRaw/nomenclatures/ISIC_codes_3.csv, encoding(UTF8) varnames(1) delimiters("%%%") stringcols(_all) clear
split codedescription, gen(var) parse("       ") // split into 2 columns (doesn't work with tab delimiter)
destring var1, gen(codes) force // destring to identify sections (cannot be destringed since strings)
gen section = substr(var1,1,1) if missing(codes) // generate sections
carryforward section, replace // carry them forward across codes
drop if missing(codes) // drop "section" rows
drop codedescription codes var2 // drop useless variables
drop if strlen(var1)!=4 // keep only ISIC classes
rename (var1 section) (rev3 rev3_sec) // rename variables to merge
tempfile secs_for_crosswalk_3
save `secs_for_crosswalk_3'

* Prepare ISIC Rev3.1 sections to be merged into crosswalk file
import delimited $ecuRaw/nomenclatures/ISIC_codes.csv, encoding(UTF8) varnames(1) stringcols(_all) clear
destring code, gen(codes) force // destring to identify sections (cannot be destringed since strings)
gen section = code if missing(codes) // generate sections
carryforward section, replace // carry them forward across codes
drop if missing(codes) // drop "section" rows
drop codes description // drop useless variables
drop if strlen(code)!=4 // keep only ISIC classes
rename (code section) (rev31 rev31_sec) // rename variables to merge
tempfile secs_for_crosswalk_31
save `secs_for_crosswalk_31'


* The official crosswalks are at the class level, so going deeper than that does
* not make sense. The highest level of granularity we will keep for our deflators 
* will be the ISIC class. In what follows I prepare crosswalks at different 
* levels (class, group, division, section). For the other levels besides class, 
* I collapse at the rev31-rev3 match level calculating weights along the way. 
* E.g. it might be that to get the deflator for a specific rev31 code we use 80%
* of a rev3 deflator and 20% of another one.

* Prepare crosswalk at CLASS level (note: it already is)
import delimited $ecuRaw/nomenclatures/crosswalk_3_3.1.txt, encoding(UTF8) varnames(1) stringcols(_all) clear
keep rev31 rev3
tempfile isic_clas
save `isic_clas'

* Prepare crosswalk at GROUP level
import delimited $ecuRaw/nomenclatures/crosswalk_3_3.1.txt, encoding(UTF8) varnames(1) stringcols(_all) clear
replace rev31 = substr(rev31,1,3)
replace rev3  = substr(rev3, 1,3)
keep rev31 rev3
gduplicates drop
tempfile isic_grps
save `isic_grps'

* Prepare crosswalk at DIVISION level
import delimited $ecuRaw/nomenclatures/crosswalk_3_3.1.txt, encoding(UTF8) varnames(1) stringcols(_all) clear
replace rev31 = substr(rev31,1,2)
replace rev3  = substr(rev3, 1,2)
keep rev31 rev3
gduplicates drop
tempfile isic_divs
save `isic_divs'

* Prepare crosswalk at SECTION level (first get sections)
import delimited $ecuRaw/nomenclatures/crosswalk_3_3.1.txt, encoding(UTF8) varnames(1) stringcols(_all) clear
merge m:1 rev3  using `secs_for_crosswalk_3',  nogen
merge m:1 rev31 using `secs_for_crosswalk_31', nogen
replace rev3_sec = "P" if inlist(rev31, "9600", "9700") // Non-existent in rev3
keep rev31_sec rev3_sec
rename (rev31_sec rev3_sec) (rev31 rev3)
gduplicates drop
tempfile isic_secs
save `isic_secs'

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

* Drop total index (not needed)
drop if code=="T"

* Keep only necessary years
keep code description *_07 *_08 *_09 *_10 *_11

* Destring index values
qui destring *_07 *_08 *_09 *_10 *_11, replace

* Generate year indexes by taking averages across the 12 months (this is also how INEC does)
foreach val in 07 08 09 10 11 {
    egen year_20`val' = rowmean(*_`val')
}

* Drop monthly values
drop *_07 *_08 *_09 *_10 *_11

* Adjust index to be base 2007 (to see evolution for all years)
foreach val in 08 09 10 11 {
    gen adj_year_20`val' = year_20`val'/year_2007*100
}
drop year_*

* Generate variable to merge
gen rev3 = substr(code, 2, .)
replace rev3 = code if missing(rev3)

* Drop description
drop description

////////////////////////////////////////////////////////////////////////////////

* Deflators at level 1 (SECTION)
preserve
    * Keep only section level
    keep if strlen(rev3)==1

    * Merge with crosswalk
    merge 1:m rev3 using `isic_secs', assert(match using) keep(match) nogen
    
    * Drop codes outside of scope
    drop if !inlist(rev31, "A", "B", "C", "D")
    
    * Calculate weights for mean
    gen weight = 1
    bys rev31: egen tot_weight = sum(weight)
    replace weight = weight/tot_weight
    drop tot_weight
    
    * Collapse at rev3.1 level by taking means when needed
    gcollapse (sum) adj* [pw=weight], by(rev31)
    rename rev31 isic_section
    
    * Reshape deflators
    reshape long adj_year_, i(isic_section) j(year)
    rename adj_year_ deflator
    
    * Export
    isid isic_section year
    save $pathCle/output/deflators_isic_section.dta, replace
restore


* Deflators at level 2 (DIVISION)
preserve
    * Keep only dvision level
    keep if strlen(rev3)==2

    * Merge with crosswalk
    merge 1:m rev3 using `isic_divs', assert(match using) keep(match) nogen
    
    * Drop codes outside of scope
    destring rev31, gen(tmp)
    drop if tmp>37 // 37 is the last division of manufacturing (section D)
    drop tmp
    
    * Calculate weights for mean
    gen weight = 1
    bys rev31: egen tot_weight = sum(weight)
    replace weight = weight/tot_weight
    drop tot_weight
    
    * Collapse at rev3.1 level by taking means when needed
    replace rev31 = substr(code,1,1)+rev31
    gcollapse (sum) adj* [pw=weight], by(rev31)
    rename rev31 isic_division
    
    * Reshape deflators
    reshape long adj_year_, i(isic_division) j(year)
    rename adj_year_ deflator
    
    * Export
    isid isic_division year
    save $pathCle/output/deflators_isic_division.dta, replace
restore


* Deflators at level 3 (GROUP)
preserve
    * Keep only group level
    keep if strlen(rev3)==3

    * Merge with crosswalk
    merge 1:m rev3 using `isic_grps', assert(match using) keep(match) nogen
    
    * Drop codes outside of scope
    destring rev31, gen(tmp)
    drop if tmp>372 // 372 is the last group of manufacturing (section D)
    drop tmp
    
    * Recalculate weights based on what's available
    gen weight = 1
    bys rev31: egen tot_weight = sum(weight)
    replace weight = weight/tot_weight
    drop tot_weight
    
    * Collapse at rev3.1 level by taking means when needed
    replace rev31 = substr(code,1,1)+rev31
    gcollapse (sum) adj* [pw=weight], by(rev31)
    rename rev31 isic_group
    
    * Reshape deflators
    reshape long adj_year_, i(isic_group) j(year)
    rename adj_year_ deflator
    
    * Export
    isid isic_group year
    save $pathCle/output/deflators_isic_group.dta, replace
restore


* Deflators at level 4 (CLASS)
preserve
    * Keep only class level 
    keep if strlen(rev3)==4

    * Merge with crosswalk
    merge 1:m rev3 using `isic_clas', assert(match using) keep(match) nogen
    
    * Drop codes outside of scope
    destring rev31, gen(tmp)
    drop if tmp>3720 // 372 is the last group of manufacturing (section D)
    drop tmp
    
    * Recalculate weights based on what's available
    gen weight = 1
    bys rev31: egen tot_weight = sum(weight)
    replace weight = weight/tot_weight
    drop tot_weight
    
    * Collapse at rev3.1 level by taking means when needed
    gen section = substr(code,1,1)
    replace rev31 = section+rev31
    gcollapse (sum) adj* [pw=weight], by(rev31)
    rename rev31 isic_class

    * Reshape deflators
    reshape long adj_year_, i(isic_class) j(year)
    rename adj_year_ deflator
    
    * Export
    isid isic_class year
    save $pathCle/output/deflators_isic_class.dta, replace
restore

////////////////////////////////////////////////////////////////////////////////

* Let's now create a file with all ISIC Rev.3 codes at all levels and their
* respective deflators, as well as the general deflator for non-ABCD

* Load ISIC Rev 3.1 file and prepare
import delimited $ecuRaw/nomenclatures/ISIC_codes.csv, encoding(UTF8) varnames(1) stringcols(_all) clear
destring code, gen(codes) force // destring to identify sections (cannot be destringed since strings)
gen section = code if missing(codes) // generate sections
carryforward section, replace // carry them forward across codes
replace code = section + code if code!=section // append section letter at beginning
drop codes section

* Make panel of 4 years
gen year = 2008
expand 2 if year==2008, gen(y2009)
replace year = 2009 if y2009
expand 2 if year==2008, gen(y2010)
replace year = 2010 if y2010
expand 2 if year==2008, gen(y2011)
replace year = 2011 if y2011
drop y2009 y2010 y2011

* Merge with different datasets created earlier
gen deflator = .
foreach level in section division group class {
    rename code isic_`level'
    merge 1:1 year isic_`level' using $pathCle/output/deflators_isic_`level'.dta, assert(master match_update) keep(master match_update) update nogen
    rename isic_`level' code
}

* Fill missing deflators by trying to go one level up and see if I get a match 
rename deflator deflator1
rename code orig_code
gen code = substr(orig_code, 1, strlen(orig_code)-1) // go one level up
replace code = substr(code, 1, strlen(code)-1) if strlen(code)==2 // length of 2 does not exist, keep moving up
gen deflator = .
foreach level in section division group {
    rename code isic_`level'
    merge m:1 year isic_`level' using $pathCle/output/deflators_isic_`level'.dta, keep(master match_update) update nogen
    rename isic_`level' code
}

* Fill missing deflators by going one more level up and see if I get a match 
rename deflator deflator2
replace code = substr(code, 1, strlen(code)-1) // go one level up
replace code = substr(code, 1, strlen(code)-1) if strlen(code)==2 // length of 2 does not exist, keep moving up
gen deflator = .
foreach level in section division {
    rename code isic_`level'
    merge m:1 year isic_`level' using $pathCle/output/deflators_isic_`level'.dta, keep(master match_update) update nogen
    rename isic_`level' code
}

* Fill missing deflators by going one last level up and see if I get a match 
rename deflator deflator3
replace code = substr(code, 1, strlen(code)-1) // go one level up
replace code = substr(code, 1, strlen(code)-1) if strlen(code)==2 // length of 2 does not exist, keep moving up
gen deflator = .
foreach level in section {
    rename code isic_`level'
    merge m:1 year isic_`level' using $pathCle/output/deflators_isic_`level'.dta, keep(master match_update) update nogen
    rename isic_`level' code
}

* Fill in original deflator by using always the most precise one when available
rename deflator deflator4
replace deflator1 = deflator2 if missing(deflator1)
replace deflator1 = deflator3 if missing(deflator1)
replace deflator1 = deflator4 if missing(deflator1)
assert !missing(deflator1) if inlist(substr(orig_code,1,1), "A", "B", "C", "D")

* Drop auxiliary variables, rename others
drop deflator2 deflator3 deflator4 code
rename (orig_code deflator1) (code deflator)

* For the industries with missing deflators, I use the general one (economy-wide)
merge m:1 year using $pathCle/output/deflators_general.dta, assert(match) nogen keepusing(ppi)
replace deflator = ppi if missing(deflator)
drop ppi

* Export final complete file
isid code year
export delimited $pathCle/output/deflators.csv, replace

* Remove auxiliary stata files
rm $pathCle/output/deflators_general.dta
rm $pathCle/output/deflators_isic_section.dta 
rm $pathCle/output/deflators_isic_division.dta 
rm $pathCle/output/deflators_isic_group.dta 
rm $pathCle/output/deflators_isic_class.dta 

////////////////////////////////////////////////////////////////////////////////

* Finlly let's prepare deflators for assets, using capital prices table from World
* Penn's Tables. 

* Load CPI data
import excel "$ecuRaw/prices/FebPwtExport1242024.xlsx", sheet("Data") case(lower) firstrow allstring clear

* Rename variables
drop variablecode
rename (regioncode yearcode aggvalue) (region year value)

* Keep Ecuador only (index is based on USA 2017 = 1.0)
keep if region=="ECU"
drop region

* Destring all variables
destring year value, replace

* Keep only 2007-2011
keep if year>=2007 & year<=2011

* Adjust to have 2007 as base year
assert year==2007 if _n==1
local value_2007 = value[1]
di "`value_2007'"
replace value = value/`value_2007'*100
drop if year==2007

* Export final complete file
isid year
export delimited $pathCle/output/assets_deflators.csv, replace
