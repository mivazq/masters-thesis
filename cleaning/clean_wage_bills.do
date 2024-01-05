////////////////////////////////////////////////////////////////////////////////
* File name: 		clean_wage_bills.do
* Author:           Miguel Vázquez Vázquez
* Creation date:    20 November 2023
* Description:		This do file cleans the employment (social security) data.
* Input:
*                   $ecuRaw/employment/IESS_`year'_ids
*                   $ecuRaw/employment/employer_employee_`year'
* Output:
*                   $pathEst/input/wage_bills.csv
////////////////////////////////////////////////////////////////////////////////
quietly do "~/data/transactions_ecuador/3_mivazq/Masters_Thesis/setup.do"
////////////////////////////////////////////////////////////////////////////////

* Create folder to store intermediate cleaned files
cap mkdir "$pathEst/input/cleaning_intermediate/SS/"

* 1a) Load and clean IESS data
save $pathEst/input/cleaning_intermediate/SS/IESS.dta, replace emptyok // create empty file
foreach year in 07 08 09 10 11 12 13 14 15 16 17 {
    
    di "Now processing year 20`year' of IESS data"
	use $ecuRaw/employment/IESS_`year'_ids.dta, clear
	
	* Transform ids that are based on SSN into RUC
	replaceID id_employer id_employee
    
    * Merge public oil exporter
    replace id_employer = 129098 if id_employer == 128357
    replace id_employee = 129098 if id_employee == 128357
	
	* Take care of date variables
	gen date = dofm(period)
	format date %d

	* Generate year and month
	gen year  = year(date)
	gen month = month(date)
	drop date period
	
	* Clean (don't drop duplicates, after manual inspection most seem legitimate)
	order year month id_employer id_employee wage
	sort  year month id_employer id_employee
	
    * Assert no missings
    assert !missing(wage)
    	
	* Aggregate up to the month level
	gcollapse (sum) wage, by(year id_employer id_employee)
    
    * Drop aggregate observations with negative/zero total wages
    qui count if wage <  0
    di as result "There are " `r(N)' " negative wage aggregated observations dropped in 20`year'"
    drop if wage <  0
    qui count if wage == 0
    di as result "There are " `r(N)' " zero wage aggregated observations dropped in 20`year'"
	drop if wage == 0

	* Append and save
	compress
	append using $pathEst/input/cleaning_intermediate/SS/IESS.dta
	save $pathEst/input/cleaning_intermediate/SS/IESS.dta, replace
}


* 1b) Load and clean F107 data
save $pathEst/input/cleaning_intermediate/SS/F107.dta, replace emptyok // create empty file
foreach year in 09 10 11 12 13 14 15 16 {
    
    di "Now processing year 20`year' of F107 data"
	use $ecuRaw/employment/employer_employee_20`year'.dta, clear
    
	* Rename variables
	rename (marca       sueldo_iess	 sueldo_f107 sob_suel_com_remu partic_utilidades  decimo_tercero  decimo_cuarto   fondo_reserva id_empleador id_empleado) ///
	       (data_source wages_iess   salary_f107 benefits_f107     participation_f107 thirteenth_f107 fourteenth_f107 reserve_f107  id_employer  id_employee)
    
	* Transform ids that are based on SSN into RUC
	replaceID id_employer id_employee
    
    * Merge public oil exporter
    replace id_employer = 129098 if id_employer == 128357
    replace id_employee = 129098 if id_employee == 128357
    
	* Labor variables
	label define marca           1 "ONLY F107" 2 "ONLY SOCIAL SECURITY" 3 "BOTH", modify
	label var wages_iess         "Wages in social security data"
	label var salary_f107        "Salary in tax withholding data"
	label var benefits_f107      "Bonuses and other compensations"
	label var participation_f107 "Participation in profits"
	label var thirteenth_f107    "Extra month of wage paid in december"
	label var fourteenth_f107    "Extra compensation paid in september"
	label var reserve_f107       "Unemployment reserves" 
    
    * Clean
    local wage_components = "wages_iess salary_f107 benefits_f107 participation_f107 thirteenth_f107 fourteenth_f107 reserve_f107"
	order year id_employer id_employee `wage_components'
	sort  year id_employer id_employee `wage_components'
	qui gduplicates report
    assert _N==`r(unique_value)' // assert no duplicates
    
    * Clearly, salaries below 1 dollar are purely symbolic and they mess up 
    * with later steps, so with replace them with zeros. Besides that, we have 
    * to assume that salary & bonuses are right since we have no way to correct
    * them. We also have to assume that participation to profits and reserve are
    * correct since we don't have a way to correct them. Also, by manual inspection
    * it seems that more often than not it's the 13th/14th payments being wrong 
    * in magnitude rather than the salary+bonuses.
    replace salary_f107   = 0 if salary_f107  <=1
    replace benefits_f107 = 0 if benefits_f107<=1
    
    * On the other hand, we can easily spot mistakes in the 13th and 14th payment
    * based on the salary and bonuses received. In particular, it should be by 
    * construction impossible that the 13th and 14th payments are greater than 
    * salary+bonuses. Indeed, it seems that bonuses are considered when calculating
    * 13th and 14th salaries, and thus are summed up them to the base salary to 
    * construct shares. It's important to note that observations where salary+bonus
    * is equal to zero will not be corrected. After visual inspection they seem
    * reasonable figures and I will simply assume that the salary/bonus components
    * were wrongly reported as zero.
    gen double share_salary_13 = round(thirteenth_f107/(salary_f107+benefits_f107),0.001)
    gen double share_salary_14 = round(fourteenth_f107/(salary_f107+benefits_f107),0.001)
    sum share_salary_13, d
    sum share_salary_14, d
    
    * For every observation where the share value is above 1, we replace the 13th/14th
    * such that the value is below 1 by iteratively dividing by 10 until it's fixed
    gen rescaled_13 = 0 if share_salary_13>1 & !missing(share_salary_13)
    gen rescaled_14 = 0 if share_salary_14>1 & !missing(share_salary_14)
    foreach m in 13 14 {
        local repeat = 1
        if `m'==13 {
            local var thirteenth_f107
        }
        else {
            local var fourteenth_f107
        }
        while `repeat'==1 {
            
            replace rescaled_`m'     = rescaled_`m' + 1    if share_salary_`m'>1 & !missing(share_salary_`m')
            replace `var'            = `var'/10            if share_salary_`m'>1 & !missing(share_salary_`m')
            replace share_salary_`m' = share_salary_`m'/10 if share_salary_`m'>1 & !missing(share_salary_`m')
            
            qui count if share_salary_`m'>1 & !missing(share_salary_`m')
            if r(N)==0 {
                local repeat = 0 // exit loop once all shares are below/equal to 1
            }
        }
    }
    sum share_salary_13, d
    sum share_salary_14, d
    tab rescaled_13 rescaled_14, m
    drop share_salary_* rescaled_13 rescaled_14
    
    * Replace missings with zeroes
    foreach var of varlist `wage_components' {
        replace `var' = 0 if missing(`var')
    }

    * Generate total of F107 items
    egen wages_f107 = rowtotal(*_f107)

	* Aggregate up to the year level
	gcollapse (sum) wages_f107 `wage_components', by(year id_employer id_employee)
    
    * Drop observations with zero total wage
	di "Drop observations with zero total wage (both sum of F107 components and IESS-based)"
    drop if wages_f107==0 & wages_iess==0
    
	* Append and save
	compress
	append using $pathEst/input/cleaning_intermediate/SS/F107.dta
    save $pathEst/input/cleaning_intermediate/SS/F107.dta, replace
}


* 1.c) Merge the two data sources
    * Merge on year-employer-employee
    use $pathEst/input/cleaning_intermediate/SS/IESS.dta, clear
    merge 1:1 year id_employer id_employee using $pathEst/input/cleaning_intermediate/SS/F107.dta, gen(source)
    
    * Look at match only for overlapping years
    tab source if year>=2009 & year<=2016
    
    * Count matched observations (exclude zeros and missings)
    preserve
        drop if missing(wage, wages_iess, salary_f107, wages_f107) | ///
        wage==0 | wages_iess==0 | salary_f107==0 | wages_f107==0
        assert source==3
        
        * Generate variable for F107 salary + bonus
        gen sal_bon = salary_f107 + benefits_f107
        
        * Compare 'wage' and 'wage_iess' (should match often, in theory same source)
        qui count if round(wage)==round(wages_iess) & !missing(wage) & !missing(wages_iess)
        di as result "Share of matched observations where 'wage' == 'wage_iess': " round(r(N)/_N*100)
        qui count if round(wage)>round(wages_iess) & !missing(wage) & !missing(wages_iess)
        di as result "Share of matched observations where 'wage'  > 'wage_iess': " round(r(N)/_N*100)
        qui count if round(wage)<round(wages_iess) & !missing(wage) & !missing(wages_iess)
        di as result "Share of matched observations where 'wage'  < 'wage_iess': " round(r(N)/_N*100)
        // ==> 39% are identical but 59% 'wage' > 'wage_iess', might imply that 'wage'
        // sometimes includes additional compensations/wage components that are 
        // not reported in 'wage_iess'

        * Compare 'wage' and 'salary_f107'
        qui count if round(wage)==round(salary_f107) & !missing(wage) & !missing(salary_f107)
        di as result "Share of matched observations where 'wage' == 'salary_f107': " round(r(N)/_N*100)
        qui count if round(wage)>round(salary_f107) & !missing(wage) & !missing(salary_f107)
        di as result "Share of matched observations where 'wage'  > 'salary_f107': " round(r(N)/_N*100)
        qui count if round(wage)<round(salary_f107) & !missing(wage) & !missing(salary_f107)
        di as result "Share of matched observations where 'wage'  < 'salary_f107': " round(r(N)/_N*100)
        // ==> 42% are identical, 32% 'wage' > 'salary_f107' and 27% 'wage' < 'salary_f107'. 
        // This looks weird, roughly random whether wage' > or < 'salary_f107'
        
        * Compare 'wage' and 'sal_bon'
        qui count if round(wage)==round(sal_bon) & !missing(wage) & !missing(sal_bon)
        di as result "Share of matched observations where 'wage' == 'sal_bon': " round(r(N)/_N*100)
        qui count if round(wage)>round(sal_bon) & !missing(wage) & !missing(sal_bon)
        di as result "Share of matched observations where 'wage'  > 'sal_bon': " round(r(N)/_N*100)
        qui count if round(wage)<round(sal_bon) & !missing(wage) & !missing(sal_bon)
        di as result "Share of matched observations where 'wage'  < 'sal_bon': " round(r(N)/_N*100)
        // ==> 36% are identical, 14% 'wage' > 'sal_bon' and 50% 'wage' < 'sal_bon'. 
        // This looks weird, roughly random whether wage' > or < 'sal_bon'
        
        * Compare 'wage' and 'wages_f107'
        qui count if round(wage)==round(wages_f107) & !missing(wage) & !missing(wages_f107)
        di as result "Share of matched observations where 'wage' == 'wages_f107': " round(r(N)/_N*100)
        qui count if round(wage)>round(wages_f107) & !missing(wage) & !missing(wages_f107)
        di as result "Share of matched observations where 'wage'  > 'wages_f107': " round(r(N)/_N*100)
        qui count if round(wage)<round(wages_f107) & !missing(wage) & !missing(wages_f107)
        di as result "Share of matched observations where 'wage'  < 'wages_f107': " round(r(N)/_N*100)
        // ==> Now 88% 'wage' < 'wages_f107', clearly 'wage' does not include
        // additional compensations
    restore
    
    * First of all let's again set wages to 0 if they are less/equal than 1 dollar, 
    * to avoid problems later (for the final variables)
    replace wage       = 0 if !missing(wage)       & wage <=1
    replace wages_iess = 0 if !missing(wages_iess) & wages_iess <=1
    replace wages_f107 = 0 if !missing(wages_f107) & wages_f107 <=1
    
    * Drop again, after the change, observations with zero total wages in all sources
    assert !missing(wage, wages_f107, wages_iess)  if source==3
    drop if wage==0 & wages_f107==0 & wages_iess==0 & source==3
    assert !missing(wage) &  missing(wages_f107, wages_iess) if source==1
    drop if wage==0                                           & source==1
    assert  missing(wage) & !missing(wages_f107, wages_iess) if source==2
    drop if wages_f107==0 & wages_iess==0                     & source==2
    
    * Let's create a variable with the final wage by combining different sources.
    * F107 wage (including all benefits) is the most reliable and complete source
    * for labor cost. Thus, when available and when F107 wage is not 0 I will use
    * that as primary source.
    gen double final_wage = wages_f107 if !missing(wages_f107) & salary_f107!=0
    
    * When wages_f107 is zero but benefits are not zero we sum benefits with the
    * 'wage_iess', since it doesn't include benefits in theory.
    replace final_wage = wages_f107 + wages_iess if missing(final_wage) & salary_f107==0
    
    * For the remaining informations we have to simply rely on the wage information 
    * of the less reliable data source which also doesn't include benefits.
    assert source==1 if missing(final_wage)
    
    * Thus, we compute ratios of 'final_wage' to 'wage' when both are available 
    * (the cases we matched from both sources) and inspect
    gen benefits_multiplier = final_wage/wage
    sum benefits_multiplier, d // median: 1.121514, mean 1.369555
    
    * I will compute median multipliers for each id_employer and match
    preserve
        collapse (median) benefits_multiplier, by(id_employer)
        tempfile benefits_multiplier
        save `benefits_multiplier', replace
    restore
    
    * Merge with ratios
    drop benefits_multiplier
    merge m:1 id_employer using `benefits_multiplier', nogen assert (master match)
    sum benefits_multiplier, d // median: 1.18193, mean 1.195473
    replace benefits_multiplier = 1 if benefits_multiplier<1 & !missing(benefits_multiplier) // lower cap at 1 (affects ~1% of obs)
    replace benefits_multiplier = 2 if benefits_multiplier>2 & !missing(benefits_multiplier) // upper cap at 2 (affects ~1% of obs)
    sum benefits_multiplier, d // median: 1.18193, mean 1.195473
    
    * As expected, most observations that can be "fixed" are from years where 
    * we don't have the most reliable source (2007, 2008, 2017)
    tab year if missing(final_wage) & !missing(benefits_multiplier)
    sum benefits_multiplier, d // median: 1.232972, mean 1.312767
    
    * Assign missing 'final_wage' as 'wage' times 'benefits_multiplier' when available
    replace final_wage = wage * benefits_multiplier if missing(final_wage)
    
    * For the employers for which we don't have a 'benefits_multiplier' we use
    * the median one
    qui sum benefits_multiplier, d
    replace final_wage = wage * `r(p50)' if missing(final_wage)
    
    * Keep only final wage
    assert !missing(final_wage)
    assert final_wage!=0
    isid year id_employer id_employee
    
    * Collapse at employer year level
    gcollapse (sum) cost_labour = final_wage, by(year id_employer)
    rename id_employer id_sri
    
    * Round and save
    replace cost_labour = round(cost_labour)
    compress
    export delimited $pathEst/input/wage_bills.csv, replace

//     * Delete folder of intermediate files and its contents
//     rm "$pathEst/input/cleaning_intermediate/SS/IESS.dta"
//     rm "$pathEst/input/cleaning_intermediate/SS/F107.dta"
//     rmdir "$pathEst/input/cleaning_intermediate/SS/"
