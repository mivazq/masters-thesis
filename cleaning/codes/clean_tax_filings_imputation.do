////////////////////////////////////////////////////////////////////////////////
* File name:        clean_tax_filings_imputation.do
* Author:           Miguel Vázquez Vázquez
* Creation date:    20 November 2023
* Description:      This do file takes the already cleaned data of firms that need
*                   imputation due to having missing years.
* Input:
*                   $pathCle/input/cleaning_intermediate/F10X/F101_2008.dta
*                   $pathCle/input/cleaning_intermediate/F10X/F101_2009.dta
*                   $pathCle/input/cleaning_intermediate/F10X/F101_2010.dta
*                   $pathCle/input/cleaning_intermediate/F10X/F101_2011.dta
*                   $pathCle/input/cleaning_intermediate/F10X/F102_2008.dta
*                   $pathCle/input/cleaning_intermediate/F10X/F102_2009.dta
*                   $pathCle/input/cleaning_intermediate/F10X/F102_2010.dta
*                   $pathCle/input/cleaning_intermediate/F10X/F102_2011.dta
*                   $pathCle/input/cleaning_intermediate/F10X/firms_to_impute.dta
* Output:
*                   $pathCle/output/tax_filings_imputed.csv
////////////////////////////////////////////////////////////////////////////////
quietly do "~/data/transactions_ecuador/3_mivazq/Masters_Thesis/setup.do"
////////////////////////////////////////////////////////////////////////////////

* Load panel of firms that need imputation
use "$pathCle/input/cleaning_intermediate/F10X/firms_to_impute.dta", clear

* Flag observations that need imputation
gen impute = (year>first_filing & year<last_filing & active==0)

* Check that flag is correct
bys id_sri: egen tot_imputed = total(impute)
assert tot_imputed == filings_due - filings_count
drop filings_due filings_count
tab tot_imputed // 83% of cases we impute 1 year, 17% of cases we impute 2 years

* Store list of year-ids to be "restored"
keep if impute
keep id_sri year
tempfile impute_obs
save `impute_obs'

* Process again the intermediate files to keep the observations to be imputed as 
* a starter

* Combine all files
local files: dir "$pathCle/input/cleaning_intermediate/F10X/" files "F10*_20*.dta"
clear all
foreach file of local files {
    qui append using "$pathCle/input/cleaning_intermediate/F10X/`file'"
}

* Keep only id-year pairs that we want to impute
merge m:1 id_sri year using `impute_obs'

* Generate flag to keep only observations of firms that need some imputation
bys id_sri: egen flag = max(_merge)
replace flag = 0 if flag==1
replace flag = 1 if flag==2 | flag==3
drop if flag==0
drop flag

* Create fake submission date for firms with no filings
replace sub_date = 1650000000000 if _merge==2

* Create flag for observations that need imputation
gen impute = (_merge==2 | _merge==3)

* Drop reported values, we don't care
//drop tot_CA tot_FA tot_DA tot_LA tot_A tot_R tot_CC tot_CE tot_C

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
* no official subtotal is reported. Thus we set negatives to zeros
replace cost_prod_total = 0 if cost_prod_total<0

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

* Keep only variables of interest
keep id_sri year sub_date tot_FA_calc revenue_op_total cost_prod_total cost_labour_total impute

* Rename variables (remove _calc and _total suffixes)
rename (tot_FA_calc  revenue_op_total   cost_prod_total cost_labour_total) ///
       (fixed_assets operative_revenues material_costs  labour_costs)

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

* Perform imputation of values when they are zero or even missing
foreach var of varlist fixed_assets operative_revenues material_costs labour_costs {
    replace `var' = . if `var'==0
}

* Keep only observations to be imputed and store
keep if impute==1
tempfile to_be_imputed
save `to_be_imputed'

* Load panel of firms that need imputation
use "$pathCle/input/cleaning_intermediate/F10X/firms_to_impute.dta", clear

* Flag observations that need imputation, drop them, and append new "recovered" ones
gen impute = (year>first_filing & year<last_filing & active==0)
drop if impute==1
append using `to_be_imputed'
gsort id_sri year

* Fill variables (by construction observations we impute cannot be exit/entry years)
replace entry = 0  if impute==1
replace exit = 0   if impute==1
replace active = 1 if impute==1
by id_sri: replace first_filing = first_filing[_n-1] if impute==1
by id_sri: replace last_filing  = last_filing[_n-1]  if impute==1
drop filings_count filings_due

* Check correlations
corr fixed_assets operative_revenues material_costs labour_costs capital_costs
* It seems that material costs correlate extremely highly with revenues whereas
* labor/capital costs correlate the highest with fixed assets. Thus I will generate
* ratios material/revenue to fill missing materials and labour/assets to fill 
* missing labour when it's possible. This should be more robust to outliers compared 
* to the other way which I'm using later when this is not possible.

* Generate ratios to help imputation and weights
gen m_y = material_costs/operative_revenues
gen l_k = labour_costs/fixed_assets
gen r_k = capital_costs/fixed_assets
by id_sri: egen tot_y = total(operative_revenues) if !missing(m_y)
by id_sri: egen tot_k = total(fixed_assets)       if !missing(l_k)
gen w_m_y = operative_revenues/tot_y
gen w_l_k = fixed_assets/tot_k
gen w_r_k = fixed_assets/tot_k
by id_sri: egen ratio_m_y = total(w_m_y*m_y) // weighted mean
by id_sri: egen ratio_l_k = total(w_l_k*l_k) // weighted mean
by id_sri: egen ratio_r_k = total(w_r_k*r_k) // weighted mean

* Fill in material and labour costs using firm-specific ratios
replace material_costs = operative_revenues * ratio_m_y if missing(material_costs) & impute==1
replace labour_costs   = fixed_assets       * ratio_l_k if missing(labour_costs)   & impute==1
replace capital_costs  = fixed_assets       * ratio_r_k if missing(capital_costs)  & impute==1

* Fill in fixed assets and operative revenue using the inverse ratios (use labor for FA)
replace operative_revenues = material_costs / ratio_m_y if missing(operative_revenues) & impute==1
replace fixed_assets       = labour_costs / ratio_l_k   if missing(fixed_assets)       & impute==1

* Drop ratios 
drop m_y l_k r_k tot_y tot_k w_m_y w_l_k w_r_k ratio_m_y ratio_l_k ratio_r_k

* For the remaining missings (either completely missing observations or e.g. both
* materials and revenues missings) I will just linearly fill the holes by taking
* means between the observations around
by id_sri: replace fixed_assets       = (fixed_assets[_n-1]       + fixed_assets[_n+1])/2       if missing(fixed_assets)       & impute==1
by id_sri: replace operative_revenues = (operative_revenues[_n-1] + operative_revenues[_n+1])/2 if missing(operative_revenues) & impute==1
by id_sri: replace material_costs     = (material_costs[_n-1]     + material_costs[_n+1])/2     if missing(material_costs)     & impute==1
by id_sri: replace labour_costs       = (labour_costs[_n-1]       + labour_costs[_n+1])/2       if missing(labour_costs)       & impute==1
by id_sri: replace capital_costs      = (capital_costs[_n-1]      + capital_costs[_n+1])/2      if missing(capital_costs)      & impute==1
 
* Repeat the same but using 2-step lag for cases where 2 consecutive years are 
* missing. I.e. we have 2008 and 2011 but are missing 2009 and 2010. I take the 
* space between the value in 2008 and 2011 and create two equidistant points that
* linearly fill the hole.
by id_sri: replace fixed_assets       = fixed_assets[_n-1]       + (fixed_assets[_n+2]       - fixed_assets[_n-1])*(1/3)       if missing(fixed_assets)       & impute==1 & year==2009
by id_sri: replace fixed_assets       = fixed_assets[_n-1]       + (fixed_assets[_n+1]       - fixed_assets[_n-2])*(1/3)       if missing(fixed_assets)       & impute==1 & year==2010
by id_sri: replace operative_revenues = operative_revenues[_n-1] + (operative_revenues[_n+2] - operative_revenues[_n-1])*(1/3) if missing(operative_revenues) & impute==1 & year==2009
by id_sri: replace operative_revenues = operative_revenues[_n-1] + (operative_revenues[_n+1] - operative_revenues[_n-2])*(1/3) if missing(operative_revenues) & impute==1 & year==2010
by id_sri: replace material_costs     = material_costs[_n-1]     + (material_costs[_n+2]     - material_costs[_n-1])*(1/3)     if missing(material_costs)     & impute==1 & year==2009
by id_sri: replace material_costs     = material_costs[_n-1]     + (material_costs[_n+1]     - material_costs[_n-2])*(1/3)     if missing(material_costs)     & impute==1 & year==2010
by id_sri: replace labour_costs       = labour_costs[_n-1]       + (labour_costs[_n+2]       - labour_costs[_n-1])*(1/3)       if missing(labour_costs)       & impute==1 & year==2009
by id_sri: replace labour_costs       = labour_costs[_n-1]       + (labour_costs[_n+1]       - labour_costs[_n-2])*(1/3)       if missing(labour_costs)       & impute==1 & year==2010
by id_sri: replace capital_costs      = capital_costs[_n-1]      + (capital_costs[_n+2]      - capital_costs[_n-1])*(1/3)      if missing(capital_costs)      & impute==1 & year==2009
by id_sri: replace capital_costs      = capital_costs[_n-1]      + (capital_costs[_n+1]      - capital_costs[_n-2])*(1/3)      if missing(capital_costs)      & impute==1 & year==2010

* Round quantities
foreach var of varlist fixed_assets operative_revenues material_costs labour_costs capital_costs {
    replace `var' = round(`var')
}
assert !missing(fixed_assets, operative_revenues, material_costs, labour_costs, capital_costs) if impute==1

* Save dataset of expansion set
compress
export delimited $pathCle/output/tax_filings_imputed_expansion.csv, replace




