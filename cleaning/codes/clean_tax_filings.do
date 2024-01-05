////////////////////////////////////////////////////////////////////////////////
* File name:        clean_tax_filings.do
* Author:           Miguel Vázquez Vázquez
* Creation date:    20 November 2023
* Description:      This do file appends all raw F101 and F102 data, adds labels,
*                   renames variables, and deals with duplicates.
* Input:
*                   $ecuRaw/F101/
*                   $ecuRaw/F102/
* Output:
*                   $pathCle/output/tax_filings.csv
////////////////////////////////////////////////////////////////////////////////
quietly do "~/data/transactions_ecuador/3_mivazq/Masters_Thesis/setup.do"
////////////////////////////////////////////////////////////////////////////////

* Create folder to store intermediate cleaned files
cap mkdir "$pathCle/input/cleaning_intermediate/F10X/"

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
            
            * Process only files in 2008-2011 timeframe
            if `year'<2008 | `year'>2011 {
                continue
            }
            
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
                gen asset_fix_prope = c480
                gen asset_fix_ships = c490
                gen asset_fix_furni = c500
                gen asset_fix_machi = c510
                gen asset_fix_compu = c540
                gen asset_fix_vehic = c550
                gen asset_fix_other = c560
                gen asset_fix_acdep = -c580 // Accumulated depreciation, to be interpreted in negative
                gen asset_fix_lands = c590
                gen asset_fix_unfin = c650
                gen asset_fix_total = c690 // Should equal the sum of all the above
                gen revenue_total            = c1930
                gen revenue_export           = c1820
                gen revenue_other_abroad     = c1830 // ? "(+) OTHER INCOME FROM ABROAD"
                gen cost_total               = c3380
                gen cost_labor               = c2280+c2290+c2300+c2310+c2360+c2370
                gen cost_imports_goods       = c1980+c1990+c2030
                gen cost_imports_services    = c2400+c2410
                gen cost_asset_fix_dep_produ = c3220+c3240
                gen cost_asset_fix_dep_admin = c3230+c3250
                gen profit                   = c3420
                gen loss                     = c3430
            }
            if "`form'"=="F102" {
                gen asset_fix_prope = c420
                gen asset_fix_ships = c430
                gen asset_fix_furni = c440
                gen asset_fix_machi = c450
                gen asset_fix_compu = c480
                gen asset_fix_vehic = c490
                gen asset_fix_other = c500
                gen asset_fix_acdep = -c530 // Accumulated depreciation, to be interpreted in negative
                gen asset_fix_lands = c540
                gen asset_fix_unfin = c550
                gen asset_fix_total = c560 // Should equal the sum of all the above
                gen revenue_total            = c1440
                gen revenue_export           = c1370
                gen revenue_other_abroad     = c1380 // ? "(+) OTHER INCOME FROM ABROAD"
                gen cost_total               = c2760
                gen cost_labor               = c1620+c1630+c1650+c1660+c1670+c1680
                gen cost_imports_goods       = c1490+c1500+c1550        
                gen cost_imports_services    = c1730+c1740
                gen cost_asset_fix_dep_produ = c2510+c2550
                gen cost_asset_fix_dep_admin = c2520+c2560
                gen profit                   = c2800
                gen loss                     = c2810
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
            save "$pathCle/input/cleaning_intermediate/F10X/`form'_`type'_`year'.dta", replace
        }
    }
    
    * Combine all files
    local `form'_files: dir "$pathCle/input/cleaning_intermediate/F10X/" files "`form'_*_20*.dta"
    clear all
    foreach file of local `form'_files {
        qui append using "$pathCle/input/cleaning_intermediate/F10X/`file'"
    }
    
    * Check identifying variables are not missing nor zeros
    foreach var of varlist id_sri year sub_date {
        assert `var'!=0 & !missing(`var')
    }
    
    * Fix IDs (should already be fine, but just to be sure)
    replaceID id_sri

    * Deal with duplicates (id-year)
        * First drop exact duplicates to reduce number observations instantly
        gduplicates drop
        
        * Eliminate remaining duplicates by keeping latest submission only
        gsort id_sri year sub_date
        by id_sri year: gegen last_time = max(sub_date)
        format last_time %tc
        keep if sub_date==last_time
        drop last_time
    
//         * Eliminate remaining duplicates by dropping "empty" filings
//         gduplicates tag id_sri year, gen(dup)
//         egen nvar_zero = anycount(revenue_total cost_total profit loss), values(0)
//         drop if nvar_zero==4 & dup>0 // if both are zero both will ge dropped, I don't care
//         drop dup nvar_zero
//        
        * Deal with last duplicates on a case by case basis (after visual inspection)
        gduplicates tag id_sri year, gen(dup)
        //br if dup>0
            * F101
            if "`form'"=="F101" {
                drop if id_sri==28353 & year==2008 & dofc(sub_date) == date("10jun2009", "DMY") & round(revenue_total)==500 // drop "lowest" entry
                drop if id_sri==36419 & year==2008 & dofc(sub_date) == date("27apr2009", "DMY") & cost_total==0 // drop entry with missing cost & profit
                replace revenue_total = 8423.79 if id_sri==61717 & year==2008 & dofc(sub_date) == date("09feb2009", "DMY") & revenue_total==0 // copy revenue from other entry
                drop if id_sri==61717 & year==2008 & dofc(sub_date) == date("09feb2009", "DMY") & cost_total==0 // drop entry with missing cost & profit
                drop if id_sri==127612 & year==2008 & dofc(sub_date) == date("09feb2012", "DMY") & round(cost_total)==108714 // drop "lowest" entry
                drop if id_sri==351863 & year==2008 & dofc(sub_date) == date("12apr2009", "DMY") & round(cost_total)==459 // drop "lowest" entry
                drop if id_sri==78712 & year==2009 & dofc(sub_date) == date("22apr2010", "DMY") & round(asset_fix_total)==0 // drop "lowest" entry
            }
            * F102
            if "`form'"=="F102" {
                drop if id_sri==386966 & year==2008 & dofc(sub_date) == date("22oct2009", "DMY") & round(revenue_total)==796609 // drop "lowest" entry
                drop if id_sri==533429 & year==2008 & dofc(sub_date) == date("19feb2009", "DMY") & cost_total==0 // drop entry with missing cost & profit
                drop if id_sri==579723 & year==2009 & dofc(sub_date) == date("10mar2010", "DMY") & round(revenue_total)==195127 // drop "lowest" entry
                drop if id_sri==653149 & year==2009 & dofc(sub_date) == date("09jul2012", "DMY") & round(revenue_total)==371997 // drop "lowest" entry
                drop if id_sri==666923 & year==2009 & dofc(sub_date) == date("02jun2010", "DMY") & round(revenue_total)==344110 // drop wrong entry
                drop if id_sri==672484 & year==2008 & dofc(sub_date) == date("18mar2009", "DMY") & round(revenue_total)==207756 // drop "lowest" entry
                drop if id_sri==730147 & year==2008 & dofc(sub_date) == date("18mar2009", "DMY") & profit==0 // drop wrong entry
            }
        drop dup
    
    * Ensure no more duplicates
    isid id_sri year
    
    * Label source form
    gen form = "`form'", before(id_sri)
    
    * Fix case in F102 where costs are obviously wrong (misplaced decimal/thousand separator)
    replace cost_total = cost_total/1000 if id_sri == 592050 & year==2009 & form=="F102"
    
    * Fix case in F101 where someone reported costs with negative sign
    replace cost_total = -cost_total if id_sri == 12348952 & year==2017 & form=="F101"
    
    * Assert values are always zero or greater
    assert (revenue_total>=0 & cost_total>=0 & profit>=0 & loss>=0)
    
    * Round values to full dullar
    foreach var of varlist revenue_total cost_total profit loss {
        replace `var' = round(`var')
    }
    
    * Generate result since profit/loss are not well defined
    gen double result = revenue_total - cost_total
    format result %20.0g

    * Check results
    count if result == 0
    count if result == 0 & profit == 0 & loss == 0
    count if result >  0
    count if result >  0 & profit >  0 & loss == 0
    count if result <  0
    count if result <  0 & profit == 0 & loss >  0
    
    * Inspect big discrepancies
    gen     sign = "+" if result >  0
    replace sign = "-" if result <  0
    replace sign = "0" if result == 0
    gen     disc = result - profit if sign == "+"
    replace disc = -result - loss  if sign == "-"
    //br if (disc > 1 | disc < -1) & !missing(disc)
    
    * It's interesting to see that the biggest problems always come from 2008/2009 
    * observations, exactly the years for which we don't have amendments. It was 
    * a good decision to use the amendment data as well.
    drop profit loss sign disc
    
    * Save dataset
    save "$pathCle/input/cleaning_intermediate/F10X/full_`form'.dta", replace
    
}

* Combine F101 and F102 forms 
use $pathCle/input/cleaning_intermediate/F10X/full_F101.dta, clear
append using $pathCle/input/cleaning_intermediate/F10X/full_F102.dta
gsort id_sri year form 

* Deal with duplicates by simply applying the same criterion as above, keep last filing
by id_sri year: gegen last_time = max(sub_date)
format last_time %tc
keep if sub_date==last_time
drop last_time sub_date

* For the 2 remaining duplicate pairs we keep the F101 filing
gduplicates tag id_sri year, gen(dup)
drop if form!="F101" & dup==1
drop dup

* Ensure no more duplicates
isid id_sri year

* Merge Merge public oil exporter
assert form == "F101" if inlist(id_sri, 128357, 129098)
replace id_sri = 129098 if id_sri == 128357
gcollapse (sum) revenue_total cost_total result, by(form id_sri year)

* Format
format id_sri year revenue_total cost_total result %20.0g

* Save
compress
export delimited $pathCle/output/tax_filings.csv, replace

