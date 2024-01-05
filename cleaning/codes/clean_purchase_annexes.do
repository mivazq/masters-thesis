////////////////////////////////////////////////////////////////////////////////
* File name:        clean_purchase_annexes.do
* Author:           Miguel Vázquez Vázquez
* Creation date:    20 November 2023
* Description:      This do file cleans the purchase annexes, aggregates them and 
* 					builds the yearly purchases database.
* Input:
*                   $ecuRaw/purchaseRecords/
* Output:
*                   $pathCle/output/purchase_annexes.csv
////////////////////////////////////////////////////////////////////////////////
quietly do "~/data/transactions_ecuador/3_mivazq/Masters_Thesis/setup.do"
////////////////////////////////////////////////////////////////////////////////
* 1 - SEPARATELY CLEAN ALL FILES
* 2 - DEAL WITH OVERLAPPING 2012 SOURCES
* 3 - COMBINE ALL FILES AND DELETE DUPLICATE ENTRIES
////////////////////////////////////////////////////////////////////////////////
**#                     1 - SEPARATELY CLEAN ALL FILES
////////////////////////////////////////////////////////////////////////////////

* Store all the dta names in `files'
local files: dir "$ecuRaw/purchaseRecords" files "*.dta" 
di `files'

* Create folder to store intermediate cleaned files
cap mkdir "$pathCle/input/cleaning_intermediate/PA/"

* Iterate over all files
foreach file in `files' {
	di "Cleaning `file'..."
	use "$ecuRaw/purchaseRecords/`file'", clear 
    
	* Keep only when condition == "CDEF" ("Carga Definitiva"): Definitive Upload 
	* CDEF lines are final, others have been amended
	drop if condition != "CDEF"
	
	* Rename variables
	rename id_purchaser id_buyer
	rename id_supplier  id_seller
	format id_buyer 	id_seller %20.0g
    
    * Transform SSN ids into RUCs and merge public oil firm
    replaceID id_seller id_buyer
    replace id_seller = 129098 if id_seller == 128357
    replace id_buyer  = 129098 if id_buyer  == 128357
    
    * Delete transactions among same firms
    qui count if id_seller==id_buyer
    di as result "Number of same-firm transactions deleted: " r(N)
	drop if id_buyer == id_seller
    
    * Generate purchase_date for files that only have register_date
    if inlist("`file'", "annexo_q4_2012.dta","annexo_2013.dta","compras_2014.dta","compras_2015.dta") {
        di "Generating purchase date" 
        gen purchase_date = register_date
    }
    
    * Rename the purchase_date for compras_2012_check_readme.dta
    if "`file'" == "compras_2012_check_readme.dta" {
        rename fecha_emision_compro purchase_date
    }
    
    * Reformat to Stata numeric date format
    foreach x in purchase_date register_date {
        if inlist("`file'","annexo_q4_2012.dta","annexo_2013.dta") { 
            * Already correctly formated
            continue
        }
        rename `x' `x'_txt
        if inlist("`file'", "comprasATS2011id_sriS1.dta", "comprasATS2011id_sriS2.dta", "comprasREO2011id_sriS1.dta", "comprasREO2011id_sriS2.dta") { 
            * In these files, the date format is MDY, in the rest is DMY
            gen `x' = date(substr(`x'_txt,1,10),"MDY")
        }
        else {
            gen `x' = date(substr(`x'_txt,1,10),"DMY")
        }
        format `x' %td
        drop `x'_txt
    }
    
    * Use register_date as the main source of date, if missing use purchase_date
    replace register_date = purchase_date if register_date == .
    drop if register_date == . & purchase_date == .
	di as result "Dates OK"
    
    * Keep only transactions in the 2008-2015 window
    gen year = yofd(register_date)
    assert !missing(year)
    count if year < 2008 | year > 2015
    keep if year >= 2008 & year <= 2015
    
    * Destring monetary variables if in string
    foreach var of varlist tax_base_12 vat tax_base_0 non_subject_VAT {
        if substr("`:type `var''",1,3) == "str" {
            qui count if substr(`var',1,1)=="0" & `var'!="0"
            di as result "There are " r(N) " cases of leading zeros in var `var'"
            destring `var', replace
        }
    }
    format tax_base_12 vat tax_base_0 non_subject_VAT %20.0g
    
	* Save
    keep  register_date year id_buyer id_seller tax_base_12 vat tax_base_0 non_subject_VAT
    order register_date year id_buyer id_seller tax_base_12 vat tax_base_0 non_subject_VAT
	compress
	save "$pathCle/output/cleaning_intermediate/PA/`file'", replace
}


////////////////////////////////////////////////////////////////////////////////
**#                 2 - DEAL WITH OVERLAPPING 2012 SOURCES
////////////////////////////////////////////////////////////////////////////////

// Note that I don't want to drop duplicates in the single datasets, since I 
// don't do it for the other years. I only want to drop duplicates *across* 
// the two different 2012 datasets.

* Load first 2012 dataset and tag duplicates
use $pathCle/input/cleaning_intermediate/PA/compras_2012_check_readme.dta, clear
gduplicates tag, gen(tag_q1q3)

* Load second 2012 dataset and tag duplicates
preserve
    use $pathCle/input/cleaning_intermediate/PA/annexo_q4_2012.dta, clear
    gduplicates tag, gen(tag_q4)
    tempfile annexo_q4
    save `annexo_q4'
restore

* Append two sources together and tag duplicates again (excluding "tag" variables)
append using `annexo_q4'
qui ds tag*, not
gduplicates tag `r(varlist)', gen(tag_both)

* Sum individual duplicates tags (taking missing as 0)
egen tag_q1q3q4 = rowtotal(tag_q1q3 tag_q4)

* Drop observations that appear identically in both datasets but are not 
* duplicates in the individual datasets. I then drop duplicates from the q4
* annex.
drop if tag_both!=tag_q1q3q4 & !missing(tag_q4)
drop tag*

* Save combined 2012 file
compress
save $pathCle/input/cleaning_intermediate/PA/compras_2012.dta, replace


////////////////////////////////////////////////////////////////////////////////
**#             3 - COMBINE ALL FILES AND SELECT TRANSACTIONS
////////////////////////////////////////////////////////////////////////////////

* Store all the file names in intermediate
local files: dir "$pathCle/input/cleaning_intermediate/PA/" files "*.dta"

* Combine all files together (excluding the two separate 2012 files)
clear all
foreach file in `files' {
    if !inlist("`file'", "compras_2012_check_readme.dta", "annexo_q4_2012.dta") {
        append using $pathCle/input/cleaning_intermediate/PA/`file'
    }
}

* Drop wrong extreme outliers (>=100,000,000 $ single transaction)
    * Tag extreme values
    foreach var in tax_base_12 vat tax_base_0 non_subject_VAT  {
        gen rounded_`var' = round(`var')
    }
    gen tag = ((abs(rounded_tax_base_12    )>=100000000 & !missing(rounded_tax_base_12    )) | ///
               (abs(rounded_vat            )>=100000000 & !missing(rounded_vat            )) | ///
               (abs(rounded_tax_base_0     )>=100000000 & !missing(rounded_tax_base_0     )) | ///
               (abs(rounded_non_subject_VAT)>=100000000 & !missing(rounded_non_subject_VAT)))
    tab tag // 160 observations

    * Set all missings to zero for these 160 observations
    foreach var of varlist tax_base_12 vat tax_base_0 non_subject_VAT rounded_* {
        replace `var' = 0 if missing(`var') & tag==1
    }

    * The majority of the 160 cases have corresponding rates (12%) but most likely
    * have instered the SSN of the other party in the wrong field. We replace those
    * false entries with zeros. No feasible way of fixing the wrong entry.
    gen rate = vat/tax_base_12 if tag==1
    gen tag1 = (round(rate,0.01)==0.12 & vat<100000000 & tax_base_12<100000000 & tag==1)
    replace tax_base_0 = 0 if tag1==1 & tax_base_0>=100000000
    replace non_subject_VAT = 0 if tag1==1 & non_subject_VAT>=100000000
    replace tag = 0 if tag1==1 // These are solved

    * In the cases where the huge values are in the tax_base_0 and non_subject_VAT
    * variables we will again set them to zero, as I have no way of proofing their
    * credibility and they are most most likely mistakes
    gen tag2 = ((tax_base_0>=100000000 | non_subject_VAT>=100000000) & vat==0 & tax_base_12==0 & tag==1)
    replace tax_base_0 = 0 if tag2==1 & tax_base_0>=100000000
    replace non_subject_VAT = 0 if tag2==1 & non_subject_VAT>=100000000
    replace tag = 0 if tag2==1 // These are solved

    * If a purchase has huge tax_base_12 value and 0 VAT, I set it to zero in this case
    gen tag3 = (tax_base_12>=100000000 & vat==0 & tag==1)
    replace tax_base_12 = 0 if tag3==1
    replace tag = 0 if tag3==1 // These are solved

    * Similarly, I correct the two cases were VAT is right (12%) but in the wrong
    * magnitude (e.g., 0.012%)
    gen tag4 = (tax_base_12>=100000000 & round(rate,0.01)!=0.12 & tag==1)
    replace tax_base_12 = vat/12*100 if tag4==1
    replace tag = 0 if tag4==1 // These are solved

    * Set suspiscious observations with 999999999 values to 0
    foreach var of varlist tax_base_12 tax_base_0 non_subject_VAT {
        replace `var' = 0 if `var'==999999999 & tag==1
    }
    replace vat = 0 if tax_base_12==0 & tag==1

    * 5 out of the remaining 8 observations involve the big oil company and are indeed
    * credible. I checked the other 3 buyer-seller combinations and the magnitude is
    * completely off. Bother tax_base_12 and vat should be heavily adjusted but I
    * have no reference point, so I set all 3 cases to 0.
    gen tag5 = (id_buyer!=129098 & id_seller!=129098 & tax_base_12>=100000000 & tag==1)
    replace tax_base_12 = 0 if tag5==1
    replace vat = 0 if tag5==1
    replace tag = 0 if tag5==1 // These are solved
    
    * Now only 5 single transactions above 100,000,000$ remain and they are correct
    drop tag* rate rounded_*
    
* Deal with missings
    * First replace missing values in the three categories with zeros
    foreach var in tax_base_12 tax_base_0 non_subject_VAT {
        replace `var'  = 0 if `var' == .
    }
    
    * If VAT field is missing we impute it to be 12% of tax_base_12
    replace vat = 0.12*tax_base_12 if missing(vat)
    
* Deal with negative values
// In theory negative values should not be allowed but the very vast majority
// of the ones in tax_base_12 have a corresponding negative VAT value of 12%.
// This seems to indicate that they are not simply mistakes. Since some firms 
// might file annexes that were automatically registered via electronic bills
// it is possible that firms returned stuff they had bought and thus appear
// negative values.
// Once we aggregate at year level we will think about potential negatives
count if tax_base_12<0
count if vat<0
count if tax_base_12<0 & vat<0
count if tax_base_0<0
count if non_subject_VAT<0

* Fix incosistencies between tax_base_12 and VAT, which should always be 12%. I
* trust here the VAT value and instead adjust tax_base_12. Mostly already correct.
replace tax_base_12 = (0.12^-1)*vat

* Generate total value of the transaction as the sum of the three components
egen transaction_value = rowtotal(tax_base_12 tax_base_0 non_subject_VAT)

* Collapse at year-buyer-seller level
gcollapse (sum) transaction_value, by(year id_buyer id_seller)

* Round to the full dollar
replace transaction_value = round(transaction_value)

* Drop observations with total yearly purchase value being zero or negative
drop if transaction_value<=0
assert !missing(transaction_value)
assert transaction_value>=1 // due to rounding


////////////////////////////////////////////////////////////////////////////////
**#                             3 - STORE DATASETS
////////////////////////////////////////////////////////////////////////////////

* Export at year-buyer-seller level
compress
export delimited $pathCle/output/intermediate_transactions.csv, replace

* Export intermediate cost
preserve
    gcollapse (sum) cost_transactions = transaction_value, by(year id_buyer)
    rename id_buyer id_sri
    export delimited $pathCle/output/intermediate_cost.csv, replace
restore

* Export intermediate revenue
preserve
    gcollapse (sum) revenue_transactions = transaction_value, by(year id_seller)
    rename id_seller id_sri
    export delimited $pathCle/output/intermediate_revenue.csv, replace
restore

//     * Delete folder of intermediate files and its contents
//     local files: dir "$pathCle/input/cleaning_intermediate/PA/" files "*.dta"
//     foreach file in `files' {
//         rm "$pathCle/input/cleaning_intermediate/PA/`file'"
//     }
//     rmdir "$pathCle/input/cleaning_intermediate/PA/"

