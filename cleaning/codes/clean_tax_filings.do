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

* Drop reported values, we don't care
drop tot_CA tot_FA tot_DA tot_LA tot_A tot_R tot_CC tot_CE tot_C

* Check identifying variables are not missing nor zeros
foreach var of varlist id_sri year sub_date {
    assert `var'!=0 & !missing(`var')
}

* Fix IDs (should already be fine, but just to be sure)
replaceID id_sri

* For now we drop all cases with negative calculated total values. I may change
* this and adjust by correcting the provision/depreciation/etc. amounts to breakeven
    * Assets
    //     drop if tot_CA_calc<0 | tot_FA_calc<0 | tot_DA_calc<0 | tot_LA_calc<0
    drop if tot_FA_calc<0 
    //assert tot_A_calc>=0
    drop tot_A_calc_non_neg tot_CA_prov tot_FA_acdp tot_DA_acam tot_LA_prov
    
    * Revenue
    assert tot_R_calc>=0
    
    * Costs
    drop if cost_prod_total<0
    assert tot_C_calc>=0 // total can only be negative due to cost_prod_total
    
* Round all variables to 2 decimal places and recalculate totals
foreach var of varlist *_calc cost_* revenue_* {
    replace `var' = round(`var', 0.01)
}
ereplace tot_C_calc = rowtotal(cost*), missing
replace revenue_op_total = revenue_op_dom + revenue_op_exp
replace tot_R_calc = revenue_op_total + revenue_nop_total
replace tot_A_calc = tot_CA_calc + tot_FA_calc + tot_DA_calc + tot_LA_calc

* Generate result (profit/loss)
gen double result = tot_R_calc - tot_C_calc

* Now we can also drop zeros (if a firm has zero total assets, zero total revenue, or zero total costs)
drop if round(tot_C_calc)==0 | round(tot_R_calc)==0 | round(tot_A_calc)==0

* I will either use FA or FA+DA or A. Definitely not CA or LA
gen double tot_AFA_calc = tot_FA_calc + tot_DA_calc, after(tot_FA_calc)
gen double tot_TFA_calc = tot_FA_calc, after(tot_FA_calc)
drop tot_CA_calc tot_LA_calc tot_FA_calc tot_DA_calc
lab var tot_AFA_calc "(=) TOTAL ALL FIXED ASSETS - CALCULATED"
lab var tot_TFA_calc "(=) TOTAL TANGIBLE FIXED ASSETS - CALCULATED"

* Drop if variables of interest for estimation are zero
    drop if round(tot_TFA_calc)==0 // tangible fixed assets, don't care about other assets
    drop if round(revenue_op_total)==0 // don't care about non-operative revenue
    drop if round(cost_labour_total)==0 // 0 labor not credible and not viable for our estimation
    drop if round(cost_prod_total)==0 // 0 materials not credible and not viable for our estimation

* Keep only variables of interest
keep id_sri year sub_date tot_TFA_calc revenue_op_total cost_prod_total cost_labour_total

* Deal with duplicates (id-year)
    * First drop exact duplicates to reduce number observations instantly
    gduplicates drop
    
    * Eliminate remaining duplicates by keeping latest submission only
    gsort id_sri year sub_date
    by id_sri year: gegen last_time = max(sub_date)
    format last_time %tc
    keep if sub_date==last_time
    drop last_time sub_date

    * At this point, I take the rows with the highest total values
    gduplicates tag id_sri year, gen(dup)
    egen double rowtot = rowtotal(tot_TFA_calc revenue_op_total cost_prod_total cost_labour_total)
    bys id_sri year: egen double max_rowtot = max(rowtot)
    bys id_sri year: egen double min_rowtot = min(rowtot)
    drop if rowtot==min_rowtot & dup>0 & max_rowtot!=min_rowtot
    drop dup rowtot max_rowtot min_rowtot
        
//     * There are still a few exact duplicates that are not considered exact due 
//     * to rounding or where totals are identical but distribution across different 
//     * cost positions is different. Here, I keep a random one since I have no way 
//     * to know.
//     gduplicates tag id_sri year, gen(dup)
//     set seed 16012024
//     gen rnd = runiform() if dup>0
//     bys id_sri year: egen max_rnd = max(rnd)
//     drop if rnd!=max_rnd & dup>0
//     drop dup rnd max_rnd
    
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

* Drop all firms which have missing filings. I deem their data unreliable for now.
* Alternatively, I could take averages to fill in
drop if filings_due!=filings_count
drop filings_count filings_due 

* Generate entry/exit/active years
gen entry  = (year==first_filing)
gen exit   = (year==last_filing)
gen active = !(_fillin)
drop _fillin

* Rename variables (remove _calc and _total suffixes)
rename *_calc *
rename *_total *
rename(tot_TFA revenue_op cost_prod cost_labour) (tang_fixed_assets operative_revenues material_costs labour_costs)

* Generate is_exporter dummy
// gen is_exporter = (revenue_op_exp > 0) if !missing(revenue_op_exp)

* Order
order id_sri year *_filing entry exit active *_assets *_revenues *_costs

* Format variables
format id_sri year *_filing entry exit active %10.0g
format *_assets *_revenues *_costs %20.2f

* Save
compress
export delimited $pathCle/output/tax_filings.csv, replace
