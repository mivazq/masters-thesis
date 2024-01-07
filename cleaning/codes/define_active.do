////////////////////////////////////////////////////////////////////////////////
* File name:        define_active.do
* Author:           Miguel Vázquez Vázquez
* Creation date:    07 January 2024
* Description:      This do file appends all raw F101 and F102 data just in the 
*                   scope of defining which firms are active.
* Input:
*                   $ecuRaw/F101/
*                   $ecuRaw/F102/
* Output:
*                   $pathCle/output/active_firms.csv
////////////////////////////////////////////////////////////////////////////////
quietly do "~/data/transactions_ecuador/3_mivazq/Masters_Thesis/setup.do"
////////////////////////////////////////////////////////////////////////////////

* Create folder to store intermediate cleaned files
cap mkdir "$pathCle/input/cleaning_intermediate/active/"

* Iterate over files
foreach form in F101 F102 {
      
    local norm_files:  dir "$ecuRaw/`form'/" files "*.dta"
    local amend_files: dir "$ecuRaw/`form'/amendments" files "*.dta" 
  
    foreach type in norm amend {
        foreach file of local `type'_files {

            * Load file
            local path = cond("`type'"=="norm", "$ecuRaw/`form'", "$ecuRaw/`form'/amendments")
            local start = strpos("`file'", "20")
            local year = substr("`file'", `start', 4) // based on filename
                        
            di as input "Now processing `form' filers for '`type'' file of year `year'"
            use "`path'/`file'", clear

            * Rename and keep needed variables (use capture in case they are already fine)
            cap rename id id_sri
            cap rename anio_fiscal year
            cap rename fecha_recepcion sub_date
            cap rename c_* c* // all tax cells
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
            
            * Keep only relevant tax cells
            if "`form'"=="F101" {
                gen revenue_total = c1930
                gen cost_total    = c3380

            }
            if "`form'"=="F102" {
                gen revenue_total = c1440
                gen cost_total    = c2760
            }
            drop c1* c2* c3* c4* c5* c6* c7* c8* c9*
            
            * Drop all zero revenue and zero cost filings, they are useless for us:
            * Most cases are individuals filing only short-form F102 (personal)
            drop if (revenue_total==0 & cost_total==0)

            * Increase storage precision and reformat identifying variables
            recast double _all
            format id_sri   %20.0g
            format year     %10.0g
            format sub_date %tc

            * Save
            save "$pathCle/input/cleaning_intermediate/active/`form'_`type'_`year'.dta", replace
        }
    }
    
    * Combine all files
    local `form'_files: dir "$pathCle/input/cleaning_intermediate/active/" files "`form'_*_20*.dta"
    clear all
    foreach file of local `form'_files {
        qui append using "$pathCle/input/cleaning_intermediate/active/`file'"
    }
    
    * Check identifying variables are not missing nor zeros
    foreach var of varlist id_sri year sub_date {
        assert `var'!=0 & !missing(`var')
    }
    
    * Fix IDs (should already be fine, but just to be sure)
    replaceID id_sri

    * Here we don't care about duplicates, since we know we will be keping one 
    * and we don't care about which one. We only care about id-year pairs
    gduplicates drop id_sri year, force
        
    * Save dataset
    save "$pathCle/input/cleaning_intermediate/active/full_`form'.dta", replace
    
}

* Combine F101 and F102 forms 
use $pathCle/input/cleaning_intermediate/active/full_F101.dta, clear
append using $pathCle/input/cleaning_intermediate/active/full_F102.dta
gsort id_sri year 

* Merge public oil exporter
replace id_sri = 129098 if id_sri == 128357

* Same as above, just keep id-year pairs, we don't care about duplicate selection here
keep id_sri year
gduplicates drop

* Generate first/last non-zero filing
assert !missing(year) & !missing(id_sri)
egen first_filing = min(year), by(id_sri)
egen last_filing  = max(year), by(id_sri)
egen filings_done = count(year), by(id_sri)
gen filings_due   = last_filing-first_filing+1

* Generate variable relating to sample period (2008-2011)
keep if year>=2008 & year<=2011
egen first_filing_sample = min(year), by(id_sri)
egen last_filing_sample  = max(year), by(id_sri)
egen filings_done_sample = count(year), by(id_sri)
gen filings_due_sample   = last_filing_sample-first_filing_sample+1
gen missing_year_sample  = (filings_due_sample>filings_done_sample)

* Save dataset
compress
export delimited $pathCle/output/active_firms.csv, replace
