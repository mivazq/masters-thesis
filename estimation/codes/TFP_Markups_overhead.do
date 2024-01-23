** Adjusting for Overhead cost: Cogs and OOPE as inputs

clear mata
mata:
void GMM_DLW(todo,betas,crit,g,H)
{
	PHI=st_data(.,("phi"),"touse")  
    PHI_LAG=st_data(.,("phi_lag"),"touse")
    Z=st_data(.,("const","m","m_lag","l","l_lag","k"),"touse") 
    X=st_data(.,("const","m","l","k"),"touse") 
    X_lag=st_data(.,("const","m_lag","l_lag","k_lag"),"touse")
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
	Z=st_data(.,("const","m","m_lag","l","l_lag","k","m_lag2","l_lag2","k2","m_lagk","l_lagk","m_lagl_lag","m_lagl_lagk"),"touse")
	X=st_data(.,("const","m","l","k","m2","l2","k2","mk","lk","ml","mlk"),"touse")
	X_lag=st_data(.,("const","m_lag","l_lag","k_lag","m_lag2","l_lag2","k_lag2","m_lagk_lag","l_lagk_lag","m_lagl_lag","m_lagl_lagk_lag"),"touse")
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
optimize_init_params(S,(1,0.35,0.55,0.1)) 
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
optimize_init_params(S,(0,0,0,0,0,0,0,0,0,0,0)) 
p=optimize(S)
p
st_matrix("beta_dlwtranslog",p)

}

end

*********************************************************************
cap program drop dlw
program dlw, rclass
preserve 
sort id year
mata DLW()
end
*********************************************************************
cap program drop dlw_translog
program dlw_translog, rclass
preserve
sort id year
mata DLW_TRANSLOG()
end


set more off
foreach cc in ALL{
use ALL_20EMPL_mar2018_keep2000.dta, clear

drop MATERIAL MATERIAL_ppi wagebill wagebill_ppi
rename COSTGOOD MATERIAL
rename COSTGOOD_ppi MATERIAL_ppi
rename OTHEROPEREXP wagebill
rename OTHEROPEREXP_ppi wagebill_ppi

drop if YEAR<2000 | YEAR>2015


* log values
gen m=ln(MATERIAL_ppi)
gen l=ln(wagebill_ppi)
gen k=ln(TGFIXEDASSETS_piwdi)
drop if k==0
drop if k==.
gen y=ln(OPER_TURN_ppi)
drop if y==.


gen alpha_labor = wagebill/OPER_TURN
drop if alpha_labor <0 | alpha_labor >=1
drop if alpha_labor<0.05

gen alpha_material =MATERIAL/OPER_TURN
drop if alpha_material <0 | alpha_material >=1
drop if alpha_material<0.05

gen COSTGOOD_constructed=wagebill+MATERIAL
gen alpha_cogs_data=COSTGOOD_constructed/OPER_TURN
sum alpha_cogs_data,detail


tempvar temph templ
local varlist alpha_cogs_data 
foreach var of local varlist{
_pctile `var' , percentiles(1 99)
scalar `templ'=r(r1)
scalar `temph'=r(r2)
drop  if `var'<`templ' & `var'!=. 
drop  if `var'>`temph' & `var'!=. 
}

* drop sectors with very few observations.
bys Nace2:egen count_sec2=count(id) 
drop if count_sec2<50 
drop count_sec2


vallist Nace2
local sectors "`r(list)'"
foreach ss of local sectors{

//Select industry for procedure
preserve
keep if Nace2==`ss'

sort ID_NUMBER id YEAR Nace2 VA_mat_ppi wagebill_ppi y l k m
keep ID_NUMBER id YEAR Nace2 NACE4 EMPL VA_mat_ppi VA_mat wagebill_ppi y l k m OPER_TURN_ppi OPER_TURN wagebill MATERIAL MATERIAL_ppi TGFIXEDASSETS TGFIXEDASSETS_piwdi alpha*

rename YEAR year


*------------create variables---------------*
* higher order terms on inputs
local M=3
local N=3
forvalues i=1/`M' {
gen m`i'=m^(`i')
gen l`i'=l^(`i')
gen k`i'=k^(`i')
*interaction terms
forvalues j=1/`N' {
gen m`i'k`j'=m^(`i')*k^(`j')
gen l`i'k`j'=l^(`i')*k^(`j')
gen m`i'l`j'=m^(`i')*l^(`j')
}
}

gen mlk=m*l*k
gen m2l2k2=m2*l2*k2
gen m3l3k3=m3*l3*k3

gen mk=m*k
gen lk=l*k


************************************************************
***** FIRST STAGE  
************************************************************
tab Nace2
xi: reg y m* l* k* i.year 
predict phi
predict epsilon, res
label var phi "phi_it 
label var epsilon "measurement error first stage
sort id year
gen phi_lag=L.phi

gen m_lag=L.m
gen l_lag=L.l
gen k_lag=L.k
gen m_lag2=m_lag^2
gen l_lag2=l_lag^2
gen k_lag2=k_lag^2
gen m_lagk_lag=m_lag*k_lag
gen l_lagk_lag=l_lag*k_lag
gen m_lagl_lag=m_lag*l_lag
gen m_lagl_lagk_lag=m_lag*l_lag*k_lag
gen l_lagk=l_lag*k

*---Compute Corrected Shares---------------------------*
gen y_c=y-epsilon
gen y_c_lev=exp(y_c)


gen alpha_mc=MATERIAL_ppi/y_c_lev
drop if alpha_mc<=0 | alpha_mc>=1

gen alpha_lc=wagebill_ppi/y_c_lev
drop if alpha_lc<=0 | alpha_lc>=1


drop _I*
sort id year
gen const=1


gen touse=0
replace touse=1 if (y!=. & m_lag!=. & l_lag!=. & k!=. & phi!=. & phi_lag!=.)

*-------COMPUTE MARKUPS --------------------------------------------*
*----------OLS estimates--------------------------------------------*
reg y m l k i.year
gen beta_mols=_b[m]
gen beta_lols=_b[l]
gen beta_kols=_b[k]
gen Markup_ols_m=_b[m]/alpha_material
gen Markup_olsc_m=_b[m]/alpha_mc
gen Markup_ols_l=_b[l]/alpha_labor
gen Markup_olsc_l=_b[l]/alpha_lc
*----------ACF estimates--------------------------------------------*
dlw
gen beta_c1=beta_dlw[1,1]
gen beta_m1=beta_dlw[1,2]
gen beta_l1=beta_dlw[1,3]
gen beta_k1=beta_dlw[1,4]

gen Markup_dlw1_m=beta_m1/alpha_material
gen Markup_dlw1c_m=beta_m1/alpha_mc
gen Markup_dlw1_l=beta_l1/alpha_labor
gen Markup_dlw1c_l=beta_l1/alpha_lc

gen omega_dlw1=phi-beta_m1*m-beta_l1*l-beta_k1*k
rename omega_dlw1 TFP_DLW_Y
*-------------------------------------------------------------------*
dlw_translog
gen betam_tl1=beta_dlwtranslog[1,2]
gen betam_tl2=beta_dlwtranslog[1,5]
gen betal_tl1=beta_dlwtranslog[1,3]
gen betal_tl2=beta_dlwtranslog[1,6]
gen betak_tl1=beta_dlwtranslog[1,4]
gen betak_tl2=beta_dlwtranslog[1,7]
gen betamk_tl=beta_dlwtranslog[1,8]
gen betalk_tl=beta_dlwtranslog[1,9]
gen betaml_tl=beta_dlwtranslog[1,10]
gen betamlk_tl=beta_dlwtranslog[1,11]

gen betam_tl=betam_tl1+2*betam_tl2*m+betamk_tl*k+ml*l+(betamlk_tl*l*k)
gen betal_tl=betal_tl1+2*betal_tl2*l+betalk_tl*k+betaml_tl*m+(betamlk_tl*m*k)


gen omega_dlw1tl=phi-betam_tl1*m-betam_tl2*m2 ///
					-betal_tl1*l-betal_tl2*l2 ///
                    -betak_tl1*k-betak_tl2*k2 ///
                    -betamk_tl*mk -betalk_tl*lk ///
					-betaml_tl*ml -betamlk_tl*mlk 
					
rename omega_dlw1tl TFP_DLWTL_Y
gen Markup_DLWTL_m=betam_tl/alpha_material
gen Markup_DLWTLc_m=betam_tl/alpha_mc
gen Markup_DLWTL_l=betal_tl/alpha_labor
gen Markup_DLWTLc_l=betal_tl/alpha_lc
*-------------------------------------------------------------------*

* Collect markup estimates and productivity for analysis:
gen mu_ols_m=Markup_ols_m
gen mu_ols_l=Markup_ols_l
gen mu_dlw_m=Markup_dlw1_m
gen mu_dlw_l=Markup_dlw1_l
gen mu_dlwtl_m=Markup_DLWTL_m
gen mu_dlwtl_l=Markup_DLWTL_l

gen mu_ols_mc=Markup_olsc_m
gen mu_ols_lc=Markup_olsc_l
gen mu_dlw_mc=Markup_dlw1c_m
gen mu_dlw_lc=Markup_dlw1c_l
gen mu_dlwtl_mc=Markup_DLWTLc_m
gen mu_dlwtl_lc=Markup_DLWTLc_l


rename year YEAR


keep ID_NUMBER YEAR  Nace2 NACE4 beta* mu_* TFP_* VA_mat VA_mat_ppi OPER_TURN OPER_TURN_ppi alpha* EMPL wagebill wagebill_ppi MATERIAL MATERIAL_ppi TGFIXEDASSETS TGFIXEDASSETS_piwdi

save temp_dlw_y_llpl_TFP_empl20_`cc'_`ss'.dta,replace

restore

}



clear
fs temp_dlw_y_llpl_TFP_empl20_`cc'_*.dta
append using `r(files)'


save TFP_DLE_COGS_`cc'_CSA.dta,replace
!del temp_dlw_y_llpl_TFP_empl20_`cc'_*

}


