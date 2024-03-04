////////////////////////////////////////////////////////////////////////////////
* File name:        clean_tax_filings.do
* Author:           Miguel Vázquez Vázquez
* Creation date:    20 November 2023
* Description:      This do file appends all raw F101 and F102 data, adds labels,
*                   renames variables, and deals with duplicates.
* Input:
*                   $ecuRaw/F101/F101_2008_jul2012.dta
*                   $ecuRaw/F101/F101_2009_jul2012.dta
*                   $ecuRaw/F101/F101_2010_jul2012.dta
*                   $ecuRaw/F101/F101_2011_jul2012.dta
*                   $ecuRaw/F102/F102_2008_jul2012.dta
*                   $ecuRaw/F102/F102_2009_jul2012.dta
*                   $ecuRaw/F102/F102_2010_jul2012.dta
*                   $ecuRaw/F102/F102_2011_jul2012.dta
* Output:
*                   $pathCle/output/tax_filings.csv
////////////////////////////////////////////////////////////////////////////////
quietly do "~/data/transactions_ecuador/3_mivazq/Masters_Thesis/setup.do"
////////////////////////////////////////////////////////////////////////////////

* Create folder to store intermediate cleaned files
cap mkdir "$pathCle/input/cleaning_intermediate/F10X/"

* Retrieve filenames 
local files "F101_2008_jul2012.dta F101_2009_jul2012.dta F101_2010_jul2012.dta F101_2011_jul2012.dta F102_2008_jul2012.dta F102_2009_jul2012.dta F102_2010_jul2012.dta F102_2011_jul2012.dta"
    
* Iterate over files
foreach file of local files {
        
    * Get information from file name
    local form = substr("`file'", 1, 4)
    local strt = strpos("`file'", "20") // look for "20" in filename
    local year = substr("`file'", `strt', 4)
        
    di as input "Now processing `form' filers for file of year `year'"
    use "$ecuRaw/`form'/`file'", clear
    
    * First drop if all zeros
    drop if declaracion_cero=="S"
    
    * Rename and keep needed variables (use capture in case they are already fine)
    cap rename id id_sri
    cap rename anio_fiscal year
    cap rename fecha_recepcion sub_date
    keep  id_sri year sub_date c1* c2* c3* c4* c5* c6* c7* c8* c9*
    order c1* c2* c3* c4* c5* c6* c7* c8* c9*, sequential
    order id_sri year sub_date, first
    
    * Destring submission date (if string), drop missings
    cap conf str# var sub_date
    if !_rc {
        gen temp = clock(sub_date, "YMDhms"), after(sub_date)
        drop sub_date
        rename temp sub_date
    }
    drop if missing(sub_date)
    
    * Destring year (if string), assert correctness
    cap conf str# var year
    if !_rc {
        qui destring year, replace
    }
    assert year==`year'
    
    * Destring tax form items that are strings
    ds c*, has(type string)
    foreach var in `r(varlist)' {
        replace `var' = subinstr(`var', ",", ".", 1) // replaces spanish decimals
        destring `var', replace
    } 
    
    * If F102, run script to rename variables
    if "`form'"=="F102" {
        di "Now renaming variables for file `file'"
        renameF102toF101
        di "Renaming done for file `file'"
    }
    
    * Run script to create variables from tax items
    di "Now constructing variables for file `file'"
    defineF10X
    di "Constructing done for file `file'"
    
    * Drop all zero revenue and zero cost filings, they are useless for us:
    * Most cases are individuals filing only short-form F102 (personal)
    drop if (tot_R_calc==0 & tot_C_calc==0)
    
    * Increase storage precision and reformat identifying variables
    recast double _all
    format id_sri   %20.0g
    format year     %10.0g
    format sub_date %tc
    ds id_sri year sub_date, not
    format `r(varlist)' %20.2f
    
    * Save
    save "$pathCle/input/cleaning_intermediate/F10X/`form'_`year'.dta", replace

}

* Combine all files
local files: dir "$pathCle/input/cleaning_intermediate/F10X/" files "F10*_20*.dta"
clear all
foreach file of local files {
    qui append using "$pathCle/input/cleaning_intermediate/F10X/`file'"
}

* Check identifying variables are not missing nor zeros
foreach var of varlist id_sri year sub_date {
    assert `var'!=0 & !missing(`var')
}

* Fix IDs (should already be fine, but just to be sure)
replaceID id_sri

* Drop other assets besides fixed assets and accumulated depreciations and provisions
drop tot_CA_calc tot_DA_calc tot_LA_calc tot_A_calc
drop tot_A_calc_non_neg tot_CA_prov tot_FA_acdp tot_DA_acam tot_LA_prov

* First of all let's replace calculated FA with reported one when the former 
* is negative. Since it's not possible to have negative FA, I will assume the 
* reported value to be correct when the calculated value turns out negative.
* In the vast majority of these cases, firms reported 0 FA.
count if tot_FA_calc<0
count if tot_FA_calc<0 & tot_FA==0
replace tot_FA_calc = tot_FA if tot_FA_calc < 0

* Unfortunately we cannot do the same for costs of production (materials) since 
* no official subtotal is reported. Thus we drop observations where the calculated
* value is negative
drop if cost_prod_total<0

* Drop self-reported values, we don't care anymore
drop tot_CA tot_FA tot_DA tot_LA tot_A tot_R tot_CC tot_CE tot_C

* Round all variables to full dollars and recalculate totals
foreach var of varlist *_calc cost_* revenue_* {
    replace `var' = round(`var')
}
replace revenue_op_total = revenue_op_dom + revenue_op_exp
replace tot_R_calc = revenue_op_total + revenue_nop_total
ereplace tot_C_calc = rowtotal(cost*), missing

* Generate result (profit/loss)
gen double result = tot_R_calc - tot_C_calc

* Drop if variables of interest for estimation are zero
drop if tot_FA_calc==0 // tangible fixed assets, don't care about other assets
drop if revenue_op_total==0 // don't care about non-operative revenue
drop if cost_prod_total==0 // 0 materials not credible and not viable for our estimation
drop if cost_labour_total==0 // 0 labor not credible and not viable for our estimation
assert tot_FA_calc>0 & revenue_op_total>0 & cost_prod_total>0 & cost_labour_total>0

* Keep only variables of interest
keep id_sri year sub_date tot_FA_calc revenue_op_total cost_prod_total cost_labour_total cost_interest_total

* Rename variables (remove _calc and _total suffixes)
rename (tot_FA_calc  revenue_op_total   cost_prod_total cost_labour_total cost_interest_total) ///
       (fixed_assets operative_revenues material_costs  labour_costs      capital_costs)

* Deal with duplicates (id-year)
gsort id_sri year sub_date

    * First drop exact duplicates to reduce number observations instantly
    gduplicates drop
    
    * Eliminate remaining duplicates by keeping latest submission only
    by id_sri year: gegen last_time = max(sub_date)
    format last_time %tc
    keep if sub_date==last_time
    drop last_time sub_date

    * At this point, I deliberately take the rows with the highest total values (few cases)
    gduplicates tag id_sri year, gen(dup)
    egen double rowtot = rowtotal(fixed_assets operative_revenues material_costs labour_costs)
    by id_sri year: egen double max_rowtot = max(rowtot)
    by id_sri year: egen double min_rowtot = min(rowtot)
    drop if rowtot==min_rowtot & dup>0 & max_rowtot!=min_rowtot
    drop dup rowtot max_rowtot min_rowtot
    
    * Drop public oil exporter. It is extremely huge, not at all comparable with 
    * any other firms in Ecuador, and it was part of OPEC at the time such that 
    * their prices where not decided on the market anyway.
    drop if id_sri == 129098 | id_sri == 128357 // two IDs merged in 2010
   
    * Ensure that finally we have unique entries
    isid id_sri year

* Make panel (all firms all 4 years)
fillin id_sri year

* Generate first filing year
gen first_filing = year if !_fillin
bys id_sri: ereplace first_filing = min(first_filing)
gen last_filing = year if !_fillin
bys id_sri: ereplace last_filing = max(last_filing)

* Count filings done
gen filings_count = 1 if !_fillin
bys id_sri: ereplace filings_count = sum(filings_count)

* Calculate filings due (there should be no empty years between filed years)
gen filings_due = (year>=first_filing & year<=last_filing)
bys id_sri: ereplace filings_due = sum(filings_due)

* Generate entry/exit/active years
gen entry  = (year==first_filing)
gen exit   = (year==last_filing)
gen active = !(_fillin)
drop _fillin

* Generate is_exporter dummy
// gen is_exporter = (revenue_op_exp > 0) if !missing(revenue_op_exp)

* Order
order id_sri year *_filing entry exit active *_assets *_revenues *_costs

* Format variables
format id_sri year *_filing entry exit active %10.0g
format *_assets *_revenues *_costs %20.0g

* Drop all firms which have missing filings. I deem their data unreliable for now.
* Alternatively, I could take averages to fill in
{
    * Store observations with this problem aside and perform imputation on a 
    * separate file
    preserve
        keep if filings_due!=filings_count
        save "$pathCle/input/cleaning_intermediate/F10X/firms_to_impute.dta", replace
    restore
}
drop if filings_due!=filings_count
drop filings_count filings_due 

* Save
compress
export delimited $pathCle/output/tax_filings.csv, replace
