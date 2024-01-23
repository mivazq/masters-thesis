
clear mata
mata:
void GMM_DLW(todo,betas,crit,g,H)
{
	PHI=st_data(.,("phi"),"touse")  
    PHI_LAG=st_data(.,("phi_lag"),"touse")
    Z=st_data(.,("const","cogs","cogs_lag","k"),"touse") 
    X=st_data(.,("const","cogs","k"),"touse") 
    X_lag=st_data(.,("const","cogs_lag","k_lag"),"touse")
    Y=st_data(.,("y"),"touse")
    C=st_data(.,("const"),"touse")

	
	OMEGA=PHI-X*betas'
	OMEGA_lag=PHI_LAG-X_lag*betas'
    OMEGA_lag2=OMEGA_lag:*OMEGA_lag
    OMEGA_lag3=OMEGA_lag2:*OMEGA_lag
    OMEGA_lag_pol=(C,OMEGA_lag,OMEGA_lag2,OMEGA_lag3)
	g_b = invsym(OMEGA_lag_pol'OMEGA_lag_pol)*OMEGA_lag_pol'OMEGA
	XI=OMEGA-OMEGA_lag_pol*g_b
	crit=(Z'XI)'(Z'XI)
	
}

    void GMM_DLW_TL(todo,betas,crit,g,H)
{
	PHI=st_data(.,("phi"),"touse")
	PHI_LAG=st_data(.,("phi_lag"),"touse")
	Z=st_data(.,("const","cogs","cogs_lag","k","cogs_lag2","k2","cogs_lagk"),"touse")
	X=st_data(.,("const","cogs","k","cogs2","k2","cogsk"),"touse")
	X_lag=st_data(.,("const","cogs_lag","k_lag","cogs_lag2","k_lag2","cogs_lagk_lag"),"touse")
	Y=st_data(.,("y"),"touse")
	C=st_data(.,("const"),"touse")

	
	OMEGA=PHI-X*betas'
	OMEGA_lag=PHI_LAG-X_lag*betas'
    OMEGA_lag2=OMEGA_lag:*OMEGA_lag
    OMEGA_lag3=OMEGA_lag2:*OMEGA_lag
    OMEGA_lag_pol=(C,OMEGA_lag,OMEGA_lag2,OMEGA_lag3)
	g_b = invsym(OMEGA_lag_pol'OMEGA_lag_pol)*OMEGA_lag_pol'OMEGA
	XI=OMEGA-OMEGA_lag_pol*g_b
	crit=(Z'XI)'(Z'XI)
}


void DLW()
	{
S=optimize_init()
optimize_init_evaluator(S, &GMM_DLW())
optimize_init_evaluatortype(S,"d0")
optimize_init_technique(S, "nm")
optimize_init_nmsimplexdeltas(S, 0.1)
optimize_init_which(S,"min")
optimize_init_params(S,(1,0.65,0.35))
p=optimize(S)
p
st_matrix("beta_dlw",p)

}


void DLW_TRANSLOG()
	{
S=optimize_init()
optimize_init_evaluator(S, &GMM_DLW_TL())
optimize_init_evaluatortype(S,"d0")
optimize_init_technique(S, "nm")
optimize_init_nmsimplexdeltas(S, 0.1)
optimize_init_which(S,"min")
optimize_init_params(S,(0,0,0,0,0,0))
p=optimize(S)
p
st_matrix("beta_dlwtranslog",p)

}

end


cap program drop dlw
program dlw, rclass
preserve 
sort id year
mata DLW()
end

cap program drop dlw_translog
program dlw_translog, rclass
preserve
sort id year
mata DLW_TRANSLOG()
end


set more off
foreach cc in ALL{
use "`cc'_worldscope_deflated_jul10.dta",clear


keep if (CNTRYCDE=="BE" ) ///
| (CNTRYCDE=="CZ" ) ///
| (CNTRYCDE=="DK" ) ///
| (CNTRYCDE=="DE" ) ///
| (CNTRYCDE=="EE" ) ///
| (CNTRYCDE=="ES" ) ///
| (CNTRYCDE=="FI" ) ///
| (CNTRYCDE=="FR" ) ///
| (CNTRYCDE=="GB" ) ///
| (CNTRYCDE=="GR" ) ///
| (CNTRYCDE=="IT" ) ///
| (CNTRYCDE=="JP" ) ///
| (CNTRYCDE=="LV" ) ///
| (CNTRYCDE=="RO" ) ///
| (CNTRYCDE=="KR" ) ///
| (CNTRYCDE=="RU" ) ///
| (CNTRYCDE=="US" ) ///
| (CNTRYCDE=="AT" ) ///
| (CNTRYCDE=="IE" ) ///
| (CNTRYCDE=="NL" ) ///
| (CNTRYCDE=="PT" ) ///
| (CNTRYCDE=="BG" ) ///
| (CNTRYCDE=="CN" ) ///
| (CNTRYCDE=="HU" ) ///
| (CNTRYCDE=="PL" ) ///
| (CNTRYCDE=="SI" ) ///
| (CNTRYCDE=="SK" ) 

keep if YEAR>=2000
keep if YEAR<=2015


* log values
gen y=ln(netsales_ppi)
gen cogs=ln(costgood_ppi)
gen k=ln(ppegross_piwdi)

***
drop if k==0
drop if k==.
drop if y==.


gen alpha_cogs_data=costgood/netsales

* drop sectors with very few observations 
bys SIC2:egen count_sec2=count(id) 
drop if count_sec2<50 
drop count_sec2

vallist SIC2
local sectors "`r(list)'"
foreach ss of local sectors{

//Select industry for procedure
preserve
keep if SIC2==`ss'

sort ID_NUMBER id YEAR SIC2 netsales_ppi costgood_ppi y cogs k 
keep CNTRYCDE cou ID_NUMBER id YEAR SIC2 SIC4 y cogs k netsales costgood netsales_ppi costgood_ppi ppegross ppegross_piwdi alpha_cogs_data 

rename YEAR year
egen country_year = group(CNTRYCDE year)

*-----Create variables------------------------------------------*
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

gen alpha_cogs=costgood_ppi/y_c_lev

*------------------------------------------------------*
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


keep CNTRYCDE cou ID_NUMBER id YEAR SIC2 SIC4 beta* mu_* TFP_DLW_cogs TFP_DLWTL_cogs netsales costgood netsales_ppi costgood_ppi ppegross ppegross_piwdi alpha_cogs* 


save temp_dlw_cogs_TFP_`cc'_`ss'.dta,replace

restore

}


clear
fs temp_dlw_cogs_TFP_`cc'_*.dta
append using `r(files)'

save TFP_DLE_COGS_`cc'_WS.dta,replace
!del temp_dlw_cogs_TFP_`cc'_*

}

