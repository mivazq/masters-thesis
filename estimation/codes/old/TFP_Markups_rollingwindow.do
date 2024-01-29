***** Rolling Window Estimation *****

foreach cc in ALL{

use "`cc'_20EMPL_mar2018_keep2000.dta",clear

* log values
gen y=ln(OPER_TURN_ppi)
gen cogs=ln(COSTGOOD_ppi)
gen k=ln(TGFIXEDASSETS_piwdi)
drop if k==0
drop if k==.
drop if y==.

gen alpha_cogs_data=COSTGOOD/OPER_TURN
drop if alpha_cogs_data<0 
drop if alpha_cogs_data>=1 
drop if alpha_cogs_data<0.01 

******Add rolling window******
sum YEAR
gen YEAR_r = YEAR
replace YEAR_r = . if YEAR>`r(max)'-4

sort Nace2
vallist Nace2
local sectors `r(list)'
foreach ss of local sectors{
levelsof YEAR_r, local(levels) 
foreach x of local levels {

preserve

keep if Nace2==`ss' & inrange(YEAR,`x',`x'+4) 

///Drop industry-window with less than 50 observations
quietly count if _N>100
  if `r(N)' > 1 {
///Drop industry-window with less than 10 observations for more than 3 years

quietly distinct YEAR
if `r(ndistinct)' > 3 {

sort ID_NUMBER id YEAR Nace2 OPER_TURN_ppi wagebill_ppi y cogs k 
keep CNTRYCDE ID_NUMBER id YEAR Nace2 NACE4 EMPL  wagebill_ppi y cogs k OPER_TURN COSTGOOD OPER_TURN_ppi COSTGOOD_ppi TGFIXEDASSETS TGFIXEDASSETS_piwdi alpha_cogs_data MAT_or_COGS

rename YEAR year


*---Create Variables------------------------------*
* higher order terms on inputs
local M=3
local N=3
forvalues i=1/`M' {
gen cogs`i'=cogs^(`i')
gen k`i'=k^(`i')
*interaction terms
forvalues j=1/`N' {
gen cogs`i'k`j'=cogs^(`i')*k^(`j')

}
}


************************************************************
***** FIRST STAGE  
************************************************************
tab Nace2
xi: reg y cogs* k* i.year 
predict phi
predict epsilon, res
label var phi "phi_it 
label var epsilon "measurement error first stage
sort id year
gen phi_lag=L.phi

gen cogs_lag=L.cogs
gen k_lag=L.k
gen cogs_lag2=cogs_lag^2
gen k_lag2=k_lag^2
gen cogs_lagk_lag=cogs_lag*k_lag
gen cogsk=cogs*k
gen cogs_lagk=cogs_lag*k
*---Compute Corrected Shares---------------------------*
gen y_c=y-epsilon
gen y_c_lev=exp(y_c)

gen alpha_cogs=COSTGOOD_ppi/y_c_lev


drop _I*
sort id year
gen const=1

gen touse=0
replace touse=1 if (y!=. & cogs_lag!=. & k!=. & phi!=. & phi_lag!=.)

*-------COMPUTE MARKUPS --------------------------------------------*
*----------OLS estimates--------------------------------------------*
reg y cogs k i.year
gen beta_cogsols=_b[cogs]
gen beta_kols=_b[k]
gen Markup_ols=_b[cogs]/alpha_cogs
gen Markup_ols_data=_b[cogs]/alpha_cogs_data
*----------ACF estimates--------------------------------------------*
dlw
gen beta_c1=beta_dlw[1,1]
gen beta_cogs1=beta_dlw[1,2]
gen beta_k1=beta_dlw[1,3]

gen Markup_dlw1=beta_cogs1/alpha_cogs
gen Markup_dlw1_data=beta_cogs1/alpha_cogs_data
gen omega_dlw1=phi-beta_cogs1*cogs-beta_k1*k
rename omega_dlw1 TFP_DLW_cogs
*-------------------------------------------------------------------*
dlw_translog
gen betacogs_tl1=beta_dlwtranslog[1,2]
gen betacogs_tl2=beta_dlwtranslog[1,4]
gen betak_tl1=beta_dlwtranslog[1,3]
gen betak_tl2=beta_dlwtranslog[1,5]
gen betacogsk_tl=beta_dlwtranslog[1,6]
gen betacogs_tl=betacogs_tl1+2*betacogs_tl2*cogs+betacogsk_tl*k


gen omega_dlw1tl=phi-betacogs_tl1*cogs-betacogs_tl2*cogs2-betak_tl1*k-betak_tl2*k2-betacogsk_tl*cogsk
rename omega_dlw1tl TFP_DLWTL_cogs
gen Markup_DLWTL=betacogs_tl/alpha_cogs
gen Markup_DLWTL_data=betacogs_tl/alpha_cogs_data
*-------------------------------------------------------------------*

* Collect markup estimates and productivity for analysis:
gen mu_ols_cogs=Markup_ols_data
gen mu_dlw_cogs=Markup_dlw1_data
gen mu_dlwtl_cogs=Markup_DLWTL_data

gen mu_ols_cogs_yc=Markup_ols
gen mu_dlw_cogs_yc=Markup_dlw1
gen mu_dlwtl_cogs_yc=Markup_DLWTL


rename year YEAR

egen start_year=min(YEAR)
egen end_year=max(YEAR)

keep CNTRYCDE ID_NUMBER YEAR  Nace2 NACE4 beta* mu_* TFP_DLW_cogs TFP_DLWTL_cogs COSTGOOD COSTGOOD_ppi OPER_TURN OPER_TURN_ppi TGFIXEDASSETS TGFIXEDASSETS_piwdi alpha_cogs* MAT_or_COGS start_year end_year


save temp_dlw_cogs_TFP_`cc'_`ss'_`x'_feb2018.dta,replace


}
}
restore

}
}

clear
fs temp_dlw_cogs_TFP_`cc'_*.dta
append using `r(files)'

save TFP_DLE_COGS_`cc'_sample1_rolling.dta,replace
!del temp_dlw_cogs_TFP_`cc'_*

}


**Rolling window by using mean beta collapsed by sector-year
use "TFP_DLE_COGS_ALL_sample1_rolling.dta", clear
drop if  beta_cogs1<0 | beta_cogs1>1 | beta_k1<0 | beta_k1>1
rename beta_cogs1 beta_cogs1_rw
rename beta_k1 beta_k1_rw
rename betacogs_tl betacogs_tl_rw
collapse beta_cogs1_rw beta_k1_rw betacogs_tl_rw, by (Nace2 YEAR)
save "DLE_ALL_rolling_to_merge_sample1_mean_beta.dta", replace


