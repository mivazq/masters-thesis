////////////////////////////////////////////////////////////////////////////////
* File name:        defineF101.ado
* Author:           Miguel Vázquez Vázquez
* Creation date:    20 November 2023
* Description:      This do file labels and generates relevant variables for F101
* Arguments:
*                   -
* Output:
*                   Tax filing positions in form of variables
////////////////////////////////////////////////////////////////////////////////

program define defineF101, rclass
    version 18
    
// Note: since tax forms varied slightly across years not all variables exist 
// in all years. Thus, I put "cap" before every "lab var" statement to capture
// potential errors due to non-existing variables.
//
// Explanation of variable labels:
//  (+) are cells that are added
//  (-) are cells that are subtracted
//  (=) are cells that are a results of calculation based on other cells
//  ( ) are cells that contain informative numbers, not used for any calculation
//  (?) don't know



use "/home/mivazq/data/transactions_ecuador/1_rawdata/F101/F101_2011_jul2012.dta" , clear



* First let's reorder the variables
order c*, seq

////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////


*** TRANSACTIONS WITH RELATED PARTIES ABROAD DURING THE FISCAL PERIOD

* We don't need transactions with parties abroad at all. Drop them all.
drop c110-c160


////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////


*** ASSETS


////////////////////////////////////////////////////////////////////////////////

* CURRENT ASSETS
cap lab var c170  "(+) FUNDS, BANKS ASSETS"
cap lab var c180  "(+) CURRENT INVESTMENTS"
cap lab var c190  "(+) TEMPORARY INVESTMENTS"
cap lab var c200  "(+) ACCOUNTS RECEIVABLE FROM NATURAL PERSONS OR FIRMS - RELATED HOME"
cap lab var c210  "(+) ACCOUNTS RECEIVABLE FROM NATURAL PERSONS OR FIRMS - RELATED ABROAD"
cap lab var c220  "(+) ACCOUNTS RECEIVABLE FROM NATURAL PERSONS OR FIRMS - NOT RELATED HOME"
cap lab var c230  "(+) ACCOUNTS RECEIVABLE FROM NATURAL PERSONS OR FIRMS - NOT RELATED ABROAD"
cap lab var c240  "(+) OTHER ACCOUNTS RECEIVABLE (e.g. FROM EMPLOYEES) - RELATED HOME"
cap lab var c250  "(+) OTHER ACCOUNTS RECEIVABLE (e.g. FROM EMPLOYEES) - RELATED ABROAD"
cap lab var c260  "(+) OTHER ACCOUNTS RECEIVABLE (e.g. FROM EMPLOYEES) - NOT RELATED HOME"
cap lab var c270  "(+) OTHER ACCOUNTS RECEIVABLE (e.g. FROM EMPLOYEES) - NOT RELATED ABROAD"
cap lab var c280  "(=) ACCOUNTS RECEIVABLE FROM NATURAL PERSONS OR FIRMS - RELATED"     // c200 + c210
cap lab var c290  "(=) ACCOUNTS RECEIVABLE FROM NATURAL PERSONS OR FIRMS - NOT RELATED" // c220 + c230
cap lab var c300  "(=) OTHER ACCOUNTS RECEIVABLE (e.g. FROM EMPLOYEES)" // c240 + c250 + c260 + c270
cap lab var c310  "(-) PROVISION FOR UNCOLLECTIBLE ACCOUNTS"
cap lab var c320  "(+) TAX CREDIT IN FAVOR OF THE TAX PAYER (VAT)"
cap lab var c330  "(=) TAX CREDIT IN FAVOR OF THE TAX PAYER (INCOME TAX)" // c340 + c350
cap lab var c340  "(+) TAX CREDIT IN FAVOR OF THE TAX PAYER (INCOME TAX) - PREVIOUS YEARS"
cap lab var c350  "(+) TAX CREDIT IN FAVOR OF THE TAX PAYER (INCOME TAX) - CURRENT YEAR"
cap lab var c360  "(+) INVENTORY OF RAW MATERIAL"
cap lab var c370  "(+) INVENTORY OF GOODS IN PROCESS"
cap lab var c380  "(+) INVENTORY OF SUPPLIES AND MATERIALS"
cap lab var c390  "(+) INVENTORY OF FINISHED GOODS AND MERCHANDISE IN THE STORE"
cap lab var c400  "(+) GOODS IN TRANSIT"
cap lab var c410  "(+) INVENTORY OF SPARE PARTS, TOOLS AND ACCESSORIES"
cap lab var c420  "(=) ASSETS PAID IN ADVANCE" // c430 + c440 + c450
cap lab var c430  "(+) INSURANCE PAID IN ADVANCE"
cap lab var c440  "(+) RENT PAID IN ADVANCE"
cap lab var c450  "(+) DIVIDENDS PAID IN ADVANCE"
cap lab var c460  "(+) OTHER CURRENT ASSETS"
cap lab var c470  "(=) TOTAL CURRENT ASSETS"                                    // Total

* Regenerate sum variables to use instead of single cells when the values of the
* at least one single cell is different than 0. Keep the given sum instead.
recast double c170-c470
replace c280 = cond(c200!=0 | c210!=0, ///
                    c200    + c210,    c280)
replace c290 = cond(c220!=0 | c230!=0, ///
                    c220    + c230,    c290)
replace c300 = cond(c240!=0 | c250!=0 | c260!=0 | c270!=0, ///
                    c240    + c250    + c260    + c270,    c300)
replace c330 = cond(c340!=0 | c350!=0, ///
                    c340    + c350,    c330)
replace c420 = cond(c430!=0 | c440!=0 | c450!=0, ///
                    c430    + c440    + c450,    c420)
drop c200 c210           // considered in c280
drop c220 c230           // considered in c290
drop c240 c250 c260 c270 // considered in c300
drop c340 c350           // considered in c330
drop c430 c440 c450      // considered in c420

* Store final sums (reported and calculated) and drop not needed remaining cells
gen double tot_CA_prov = cond(c310>0, -c310, c310) // in case they get reported with minus
gen double tot_CA      = c470
gen double tot_CA_calc = c170 + c180 + c190 + c280 + c290 + c300 + c320 + c330 + c360 + c370 + c380 + c390 + c400 + c410 + c420 + c460 + tot_CA_prov
format %20.2f tot_*
lab var tot_CA      "(=) TOTAL CURRENT ASSETS - REPORTED"
lab var tot_CA_calc "(=) TOTAL CURRENT ASSETS - CALCULATED"
lab var tot_CA_prov "(-) PROVISIONS ON TOTAL CURRENT ASSETS"
drop c170 c180 c190 c280 c290 c300 c310 c320 c330 c360 c370 c380 c390 c400 c410 c420 c460 c470


////////////////////////////////////////////////////////////////////////////////


* FIXED ASSETS
cap lab var c480  "(+) BUILDINGS"
cap lab var c490  "(+) SHIPS, AIRCRAFT, AND BARGES (ETC.)"
cap lab var c500  "(+) FURNITURE, FURNISHINGS"
cap lab var c510  "(=) MACHINERY, EQUIPMENT, FACILITIES" // c630 + c640
cap lab var c520  "(=) MACHINERY, EQUIPMENT, FACILITIES, FURNITURE, FURNISHINGS" // c500 + c510   OR   c500 + (630 + c640)
cap lab var c530  "(=) MACHINERY, EQUIPMENT, FACILITIES, FURNITURE, FURNISHINGS, BUILDINGS" // c480 + c520   OR   c480 + (c500 + c510)
cap lab var c540  "(+) COMPUTER EQUIPMENT AND SOFTWARE"
cap lab var c550  "(+) VEHICLES AND TRANSPORTATION EQUIPMENT"
cap lab var c560  "(+) OTHER FIXED ASSETS"
cap lab var c570  "(=) TOTAL ACCUMULATED DEPRECIATION OF FIXED ASSETS" // c580 + c660 + c670
cap lab var c580  "(-) ACCUMULATED DEPRECIATION OF FIXED ASSETS - NON ACCELERATED"
cap lab var c590  "(+) LAND"
cap lab var c600  "(=) BUILDINGS + FACILITIES" // c610 + c630
cap lab var c610  "(+) BUILDINGS"
cap lab var c620  "(=) MACHINERY, EQUIPMENT, FURNITURE, FURNISHINGS" // c500 + c640
cap lab var c630  "(+) FACILITIES"
cap lab var c640  "(+) MACHINERY, EQUIPMENT"
cap lab var c650  "(+) NOT FINISHED BUILDINGS"
cap lab var c660  "(-) ACCUMULATED DEPRECIATION OF FIXED ASSETS - ACCELERATED"
cap lab var c670  "(-) ACCUMULATED DEPRECIATION OF VEHICLES AND TRANSPORTATION EQUIPMENT - ACCELERATED"
cap lab var c680  "(=) TOTAL FIXED ASSETS TANGIBLES"                            // Total
cap lab var c690  "(=) TOTAL FIXED ASSETS"                                      // Total (c680+c720) but often confused with c680

* Some people report buildings in c610 some (most) in c480. Let's standardize
recast double c480-c670
assert !(c480>0 & c610>0)
replace c480 = c480 + c610
drop c610

* Also there are a few "hidden" sums which I don't think I need
assert c600==0
assert c620==0
drop c600 c620 // these sums are redundant as they'll be included in another way

* Regenerate sum variables to use instead of single cells when the values of the
* at least one single cell is different than 0. Keep the given sum instead.
replace c510 = cond(c630!=0 | c640!=0, ///
                    c630    + c640,    c510)
replace c520 = cond(c500!=0 | c510!=0, ///
                    c500    + c510,    c520)
replace c530 = cond(c480!=0 | c520!=0, ///
                    c480    + c520,    c530)
replace c570 = cond(c580!=0 | c660!=0 | c670!=0, ///
                    c580    + c660    + c670,    c570)
drop c630 c640      // considered in c510
drop c500 c510      // considered in c520
drop c480 c520      // considered in c530
drop c580 c660 c670 // all these depreciations have been summed up

* Fix totals (firms have reported differently)
recast double c680-c720
replace c680 = cond(c720==0, c690, c690-c720) if c680==0 // replace with diff total - intangible when tangible is 0

* Store final sums (reported and calculated) and drop not needed remaining cells
gen double tot_FA_acdp = cond(c570>0, -c570, c570) // in case they get reported with minus
gen double tot_FA      = c680
gen double tot_FA_calc = c490 + c530 + c540 + c550 + c560 + c590 + c650 + tot_FA_acdp
format %20.2f tot_*
lab var tot_FA      "(=) TOTAL FIXED ASSETS - REPORTED"
lab var tot_FA_calc "(=) TOTAL FIXED ASSETS - CALCULATED"
lab var tot_FA_acdp "(-) ACCUMULATED DEPRECIATION ON TOTAL FIXED ASSETS"
drop c490 c530 c540 c550 c560 c570 c590 c650 c680 c690


////////////////////////////////////////////////////////////////////////////////


* DEFERRED ASSETS
cap lab var c700  "(+) TRADEMARKS, PATENTS, ETC."
cap lab var c710  "(-) ACCUMULATED AMORTIZATION TRADEMARKS, PATENTS, ETC."
cap lab var c720  "(=) TOTAL INTANGIBLE FIXED ASSETS"
cap lab var c730  "(+) ORGANIZATIONAL COSTS"
cap lab var c740  "(+) RESEARCH AND EXPLORATION COSTS"
cap lab var c750  "(+) BALANCE DEBTOR CURRENCY EXCHANGE"
cap lab var c760  "(+) OTHER DEFERRED ASSETS"
cap lab var c770  "(-) ACCUMULATED AMORTIZATION OTHER"
cap lab var c780  "(=) TOTAL DEFERRED ASSETS"                                   // Total

* First drop c720 since it's a subtotal that doesn't make sense and sometimes
* even gets confused for fixed assets total.
* c700 and c710 simply pertain to deferred assets and most firms do it properly
recast double c700-c780
drop c720

* Combine depreciations
count if c710==c770 & c710!=0 // avoid double counting
di as input "There are " r(N) " cases of acc.dep. being double-reported"
replace c770 = cond(c710!=c770, c770 + c710, c770)
drop c710

* Store final sums (reported and calculated) and drop not needed remaining cells
gen double tot_DA_acam = cond(c770>0, -c770, c770) // in case they get reported with minus
gen double tot_DA      = c780
gen double tot_DA_calc = c700 + c730 + c740 + c750 + c760 + tot_DA_acam
format %20.2f tot_*
lab var tot_DA      "(=) TOTAL DEFERRED ASSETS - REPORTED"
lab var tot_DA_calc "(=) TOTAL DEFERRED ASSETS - CALCULATED"
lab var tot_DA_acam "(-) ACCUMULATED AMORTIZATION ON TOTAL DEFERRED ASSETS"
drop c700 c730 c740 c750 c760 c770 c780


////////////////////////////////////////////////////////////////////////////////


* LONG TERM ASSETS
cap lab var c790  "(+) LONG TERM INVESTMENTS - SHARES AND OTHER EQUITY"
cap lab var c800  "(+) LONG TERM INVESTMENTS - OTHER"
cap lab var c810  "(+) LONG TERM ACCOUNTS RECEIVABLE FROM NATURAL FIRMS OR PERSONS - RELATED HOME"
cap lab var c820  "(+) LONG TERM ACCOUNTS RECEIVABLE FROM NATURAL FIRMS OR PERSONS - RELATED ABROAD"
cap lab var c830  "(+) LONG TERM ACCOUNTS RECEIVABLE FROM NATURAL FIRMS OR PERSONS - NOT RELATED HOME"
cap lab var c840  "(+) LONG TERM ACCOUNTS RECEIVABLE FROM NATURAL FIRMS OR PERSONS - NOT RELATED ABROAD"
cap lab var c850  "(=) LONG TERM ACCOUNTS RECEIVABLE FROM NATURAL FIRMS OR PERSONS" // c790 + c800 + c810 + c820 + c830 + c840
cap lab var c860  "(+) OTHER LONG TERM ACCOUNTS RECEIVABLE - RELATED HOME"
cap lab var c870  "(+) OTHER LONG TERM ACCOUNTS RECEIVABLE - RELATED ABROAD"
cap lab var c880  "(+) OTHER LONG TERM ACCOUNTS RECEIVABLE - NOT RELATED HOME"
cap lab var c890  "(+) OTHER LONG TERM ACCOUNTS RECEIVABLE - NOT RELATED ABROAD"
cap lab var c900  "(=) OTHER LONG TERM ACCOUNTS RECEIVABLE"  // c860 + c870 + c880 + c890
cap lab var c910  "(-) PROVISION FOR UNCOLLECTIBLE ACCOUNTS"
cap lab var c920  "(+) OTHER LONG TERM COSTS PAYED IN ADVANCE"
cap lab var c1010 "(+) OTHER LONG TERM ASSETS"
cap lab var c1070 "(=) TOTAL LONG TERM ASSETS"                                  // Total

* Regenerate sum variables to use instead of single cells when the values of the
* at least one single cell is different than 0. Keep the given sum instead.
* Firms seem to have filed the form weirdly (no surprise). So to get things right
* I'll first sum up "OTHER LONG TERM ACCOUNTS RECEIVABLE" and then combine the 
* the rest together. In fact, many people summed up ALL accounts in c850 and not
* only c790 + c800 + c810 + c820 + c830 + c840
recast double c790-c1070
replace c900 = cond(c860!=0 | c870!=0 | c880!=0 | c890!=0, ///
                    c860    + c870    + c880    + c890,    c890)
drop c860 c870 c880 c890 // considered in c900
gen c850_new = cond(c790!=0 | c800!=0 | c810!=0 | c820!=0 | c830!=0 | c840!=0, ///
                    c790    + c800    + c810    + c820    + c830    + c840,    c850)
drop c790 c800 c810 c820 c830 c840 c850 // considered in c850

* This next line assumes that if c850_new and c900 are the same value it must be
* that they reported c900 already in c850. It would be a big coincidence for it 
* to be exactly the same and not be the case of double reporting. If they are 
* different instead, we sum them up because people would not report c900 and then
* also include those values within c850.
gen double lt_assets = cond(round(c850_new)!=round(c900), c850_new + c900, c850_new)

* Store final sums (reported and calculated) and drop not needed remaining cells
gen double tot_LA_prov = cond(c910>0, -c910, c910) // in case they get reported with minus
gen double tot_LA      = c1070
gen double tot_LA_calc = lt_assets + c920 + c1010 + tot_LA_prov
format %20.2f tot_*
lab var tot_LA      "(=) TOTAL LONG TERM ASSETS - REPORTED"
lab var tot_LA_calc "(=) TOTAL LONG TERM ASSETS - CALCULATED"
lab var tot_LA_prov "(-) PROVISIONS ON TOTAL LONG TERM ASSETS"
drop c850_new c900 c910 c920 c1010 c1070 lt_assets


////////////////////////////////////////////////////////////////////////////////


* TOTAL ASSETS
cap lab var c1050 "( ) CONTINGENT ASSETS"
cap lab var c1075 "( ) ASSETS COMING FROM REINVESTMENT OF PROFITS"
cap lab var c1080 "(=) TOTAL ASSETS" // (c470+c690+c780+c1070)
drop c1050 c1075 // no need for informative cells

* Create new sums
gen double tot_A = c1080
gen double tot_A_calc = tot_CA_calc + tot_FA_calc + tot_DA_calc + tot_LA_calc
gen double tot_A_calc_non_neg = cond(tot_CA_calc>0, tot_CA_calc, 0) + ///
                                cond(tot_FA_calc>0, tot_FA_calc, 0) + ///
                                cond(tot_DA_calc>0, tot_DA_calc, 0) + ///
                                cond(tot_LA_calc>0, tot_LA_calc, 0)
format %20.2f tot_*
lab var tot_A              "(=) TOTAL ASSETS - REPORTED"
lab var tot_A_calc         "(=) TOTAL ASSETS - CALCULATED"
lab var tot_A_calc_non_neg "(=) TOTAL ASSETS - CALCULATED (negative totals converted to 0)"
assert tot_A_calc_non_neg>=0
drop c1080


////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////


*** LIABILITIES & EQUITIES

* We don't need liabilities and equity at all. Drop them all.
drop c930-c1790


////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////


*** INCOME
cap lab var c1800 "(+) NET DOMESTIC SALES SUBJECT TO 12% TAX RATE"
cap lab var c1810 "(+) NET DOMESTIC SALES SUBJECT TO 0% TAX RATE"
cap lab var c1820 "(+) NET EXPORTS"
cap lab var c1830 "(+) OTHER INCOME FROM ABROAD"
cap lab var c1840 "(+) FINANCIAL RENTS"
cap lab var c1850 "(+) OTHER TAXABLE INCOME"
cap lab var c1860 "(+) GAIN ON SALE OF FIXED ASSETS"
cap lab var c1870 "(+) DOMESTIC DIVIDENDS RECEIVED"
cap lab var c1880 "(+) EXEMPTED INCOME FROM DONATIONS AND CONTRIBUTIONS - FROM PUBLIC RESOURCES"
cap lab var c1890 "(+) EXEMPTED INCOME FROM DONATIONS AND CONTRIBUTIONS - FROM OTHER ECUADORIAN SOURCES"
cap lab var c1900 "(+) EXEMPTED INCOME FROM DONATIONS AND CONTRIBUTIONS - FROM ABROAD"
cap lab var c1910 "(=) EXEMPTED INCOME FROM DONATIONS AND CONTRIBUTIONS" // c1880 + c1890 + c1900
cap lab var c1920 "(+) OTHER EXEMPTED INCOME"
cap lab var c1930 "(=) TOTAL INCOME"                                            // Total
cap lab var c1940 "( ) NET SALES OF FIXED ASSETS"
cap lab var c1950 "( ) REIMBURSEMENT RECEIVED AS INTERMEDIARY"
drop c1940 c1950 // no need for informative cells


* Regenerate sum variables to use instead of single cells when the values of the
* at least one single cell is different than 0. Keep the given sum instead.
recast double c1800-c1930
replace c1910 = cond(c1880!=0 | c1890!=0 | c1900!=0, ///
                     c1880    + c1890    + c1900,    c1910)
drop c1880 c1890 c1900

* Create new sums
gen double tot_R = c1930
gen double tot_R_calc = c1800 + c1810 + c1820 + c1830 + c1840 + c1850 + c1860 + c1870 + c1910 + c1920
format %20.2f tot_*
lab var tot_R      "(=) TOTAL ASSETS - REPORTED"
lab var tot_R_calc "(=) TOTAL ASSETS - CALCULATED"
drop c1800 c1810 c1820 c1830 c1840 c1850 c1860 c1870 c1910 c1920 c1930


////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////

*** COSTS & EXPENSES

* Recast to double
recast double c1960-c3410

* Drop variables "VND" valor no deducible, no need
foreach var of varlist c1960-c3410 {
    local lab : variable label `var'
    if substr("`lab'",1,3)=="VND" {
        di "Dropping `var' with label `lab'"
        drop `var'
    }
}
cap drop c2745 // wrong label ("VPN") but it's ""

* Drop variables "MAO" (don't know what it stands for, but they're all zeros), no need
foreach var of varlist c1960-c3410 {
    local lab : variable label `var'
    if substr("`lab'",1,3)=="MAO" {
        di "Dropping `var' with label `lab'"
        drop `var'
    }
}

////////////////////////////////////////////////////////////////////////////////

* INVENTORY

* Generate new variables available only in amendments
cap gen c1977 = 0, after(c1970)
cap gen c2274 = 0, after(c2270)
cap gen c2278 = 0, after(c2274)

* Label variables
    * Intermediate goods
    lab var c1960 "(+) INITIAL INVENTORY OF GOODS NOT PROUDCED BY THE FIRM - PRODUCTION COSTS"
    lab var c1970 "(+) NET PURCHASES OF DOMESTIC GOODS NOT PRODUCED BY THE FIRM - PRODUCTION COSTS"
    lab var c1977 "(+) NET PURCHASES OF DOMESTIC GOODS FROM THE FIRM'S INDUSTRY - PRODUCTION COSTS"
    lab var c1980 "(+) IMPORTS NOT PRODUCED BY THE FIRM - PRODUCTION COSTS"
    lab var c1990 "(+) IMPORTS NOT PRODUCED BY THE FIRM - ADMIN EXPENSES"
    lab var c2000 "(-) FINAL INVENTORY OF GOOD NOT PRODUCED BY THE FIRM - PRODUCTION COSTS"
    * Raw materials
    lab var c2010 "(+) INITIAL INVENTORY OF RAW MATERIAL - PRODUCTION COSTS"
    lab var c2020 "(+) NET DOMESTIC PURCHES OF RAW MATERIAL - PRODUCTION COSTS"
    lab var c2030 "(+) IMPORTS OF RAW MATERIAL - PRODUCTION COSTS"
    lab var c2040 "(-) FINAL INVENTORY OF RAW MATERIAL - PRODUCTION COSTS"
    * Products in process
    lab var c2240 "(+) INITIAL INVENTORY OF PRODUCTS IN PROCESS - PRODUCTION COSTS"
    lab var c2250 "(-) FINAL INVENTORY OF PRODUCTS IN PROCESS - PRODUCTION COSTS"
    * Products finished
    lab var c2260 "(+) INITIAL INVENTORY OF FINISHED PRODUCTS - PRODUCTION COSTS"
    lab var c2270 "(-) FINAL INVENTORY OF FINISHED PRODUCTS - PRODUCTION COSTS"
    lab var c2274 "(+) ADJUSTMENT COSTS - PRODUCTION COSTS"
    lab var c2278 "(=) TOTAL COST OF SALES - PRODUCTION COSTS"

* Production costs by category
gen double cost_prod_interm_goods = c1960 + c1970 + c1977 + c1980 + c1990 - c2000
gen double cost_prod_raw_materials = c2010 + c2020 + c2030 - c2040
gen double cost_prod_in_process = c2240 - c2250
gen double cost_prod_finished = c2260 - c2270
gen double cost_prod_adjustment = c2274
gen double cost_prod_total = cost_prod_interm_goods + cost_prod_raw_materials + cost_prod_in_process + cost_prod_finished + cost_prod_adjustment

* Informative variables on imports
gen double info_imports_interm_goods = c1980 + c1990 // already included in "cost_prod_interm_goods", not to be double counted
gen double info_imports_raw_materials = c2030 // already included in "cost_prod_raw_materials", not to be double counted

* Format and keep only total (for now)
format %20.2f cost_prod_* info_imports_*
drop cost_prod_interm_goods cost_prod_raw_materials cost_prod_in_process cost_prod_finished cost_prod_adjustment info*

////////////////////////////////////////////////////////////////////////////////

* LABOUR

* Generate variables available only in normal tax files
cap gen c2320 = 0, after(c2310)
cap gen c2330 = 0, after(c2320)
cap gen c2340 = 0, after(c2330)
cap gen c2350 = 0, after(c2340)

* Generate new variables available only in amendments
cap gen c2752 = 0, after(c2370)
cap gen c2754 = 0, after(c2752)

* c2330 should always be zero, haven't accounted for it not being zero yet
cap assert c2330==0
if _rc {
    di "c2330 [(+) PAYMENTS COMPLETED TO OUTSOURCING] is not zero. What now?"
} 
drop c2330

* Label variables
    * IESS-regulated labour costs
    lab var c2280 "(+) WAGES, SALARIES AND OTHER TAXABLE REMUNERATIONS - PRODUCTION COSTS"
    lab var c2290 "(+) WAGES, SALARIES AND OTHER TAXABLE REMUNERATIONS - ADMIN EXPENSES"
    lab var c2300 "(+) SOCIAL BENEFITS AND OTHER NON-TAXABLE COMPENSATION - PRODUCTION COSTS"
    lab var c2310 "(+) SOCIAL BENEFITS AND OTHER NON-TAXABLE COMPENSATION - ADMIN EXPENSES"
    lab var c2320 "(+) WAGES, SALARIES AND OTHER TAXABLE REMUNERATIONS"
    lab var c2340 "(+) WAGES, SALARIES AND OTHER TAXABLE REMUNERATIONS OUTSOURCING"
    lab var c2350 "(+) OTHER REMUNERATIONS TO OUTSOURCING"
    lab var c2360 "(+) CONTRIBUTION TO SOCIAL SECURITY (INCLUDING RESERVE FUND) - PRODUCTION COSTS"
    lab var c2370 "(+) CONTRIBUTION TO SOCIAL SECURITY (INCLUDING RESERVE FUND) - ADMIN EXPENSES"
    lab var c2752 "(+) OTHER EXPENSES ON EMPLOYEE BENEFITS - PRODUCTION COSTS"
    lab var c2754 "(+) OTHER EXPENSES ON EMPLOYEE BENEFITS - ADMIN EXPENSES"
    * Professional fees for domestic contractors
    lab var c2380 "(+) PROFESSIONAL FEES AND EXPENSES - PRODUCTION COSTS"
    lab var c2390 "(+) PROFESSIONAL FEES AND EXPENSES - ADMIN EXPENSES"
    * Professional fees for foreign contractors
    lab var c2400 "(+) FEES TO FOREIGNERS FOR ONE-TIME EXPENSES - PRODUCTION COSTS"
    lab var c2410 "(+) FEES TO FOREIGNERS FOR ONE-TIME EXPENSES - ADMIN EXPENSES"

* Ensure external wages are included to c2290 when they're not zero
replace c2290 = cond(c2320!=0 | c2340!=0 | c2350!=0, ///
                     c2320    + c2340    + c2350,    c2290)
drop c2320 c2340 c2350 // considered in c2290

* Labour costs by category
gen double cost_labour_iess = c2280 + c2290 + c2300 + c2310 + c2360 + c2370 + c2752 + c2754
gen double cost_labour_prof_fees_dom = c2380 + c2390
gen double cost_labour_prof_fees_imp = c2400 + c2410
gen double cost_labour_total = cost_labour_iess + cost_labour_prof_fees_dom + cost_labour_prof_fees_imp

* Format and keep only total (for now)
format %20.2f cost_labour*
drop cost_labour_iess cost_labour_prof_fees_dom cost_labour_prof_fees_imp

////////////////////////////////////////////////////////////////////////////////

* OTHER OPERATIONAL COSTS

* Generate variables available only in normal tax files
cap gen c2110 = 0, before(c2420)
cap gen c2180 = 0, after(c2110)
cap gen c2190 = 0, after(c2180)
cap gen c2200 = 0, after(c2190)
cap gen c2210 = 0, after(c2200)
cap gen c2220 = 0, after(c2210)
cap gen c2230 = 0, after(c2220)
cap gen c2460 = 0, after(c2230)
cap gen c2470 = 0, after(c2660)

* Generate new variables available only in amendments
cap gen c2772 = 0, after(c2770)
cap gen c2774 = 0, after(c2772)
cap gen c2776 = 0, after(c2774)
cap gen c2785 = 0, after(c2776)
cap gen c2786 = 0, after(c2785)
cap gen c2788 = 0, after(c2786)
cap gen c2801 = 0, after(c2788)
cap gen c2804 = 0, after(c2801)
cap gen c2815 = 0, after(c2804)
cap gen c2803 = 0, after(c2815)

* c2180 & c2190 should always be zero, haven't accounted for them not being zero yet
cap assert c2180==0 & c2190==0
if _rc {
    di "c2180 [(=) COMMERCIAL RENT - HOME] or c2190 [(=) COMMERCIAL RENT - ABROAD] are not zero. What now?"
} 
drop c2180 c2190

* c2460 should already be accounted for
cap assert round(c2460)==round(c2910) if c2460>0
if _rc {
    di "c2460 [(+) COMMISSIONS OF COMPANIES] is not accounted for. What now?"
}
drop c2460

* Label variables
    lab var c2110 "(+) OTHER PRODUCTION EXPENSES"
    * Real estate rent
    lab var c2200 "(+) REAL ESTATE RENT FOR NATURAL PERSONS PROPERTIES - PRODUCTION COST"
    lab var c2210 "(+) REAL ESTATE RENT FOR NATURAL PERSONS PROPERTIES - ADMIN EXPENSES"
    lab var c2220 "(+) REAL ESTATE RENT FOR COMPANIES PROPERTIES - PRODUCTION COST"
    lab var c2230 "(+) REAL ESTATE RENT FOR COMPANIES PROPERTIES - ADMIN EXPENSES"
    lab var c2420 "(=) REAL ESTATE RENT - PRODUCTION COSTS" // c2200 + c2220
    lab var c2430 "(=) REAL ESTATE RENT - ADMIN EXPENSES" // c2210 + c2230
    * Maintenance and repairs
    lab var c2440 "(+) MAINTENANCE AND REPAIRS - PRODUCTION COSTS"
    lab var c2450 "(+) MAINTENANCE AND REPAIRS - ADMIN EXPENSES"
    * Fuel and lubricants
    lab var c2640 "(+) FUEL AND LUBRICANTS - PRODUCTION COSTS"
    lab var c2650 "(=) FUEL AND LUBRICANTS - ADMIN EXPENSES" // c2660 + c2470
    lab var c2660 "(+) FUEL - ADMIN EXPENSES"
    lab var c2470 "(+) LUBRICANTS - ADMIN EXPENSES"
    * Marketing
    lab var c2670 "(+) MARKETING - PRODUCTION COSTS"
    lab var c2680 "(+) MARKETING - ADMIN EXPENSES"
    * Supplies, materials, and replacements
    lab var c2690 "(+) SUPPLIES AND MATERIALS - PRODUCTION COSTS"
    lab var c2700 "(+) SUPPLIES AND MATERIALS - ADMIN EXPENSES"
    * Transportation
    lab var c2710 "(+) TRANSPORTATION - PRODUCTION COSTS"
    lab var c2720 "(+) TRANSPORTATION - ADMIN EXPENSES"
    * Provisions
    lab var c2730 "(+) PROVISIONS - FOR RETIREMENT - PRODUCTION COSTS"
    lab var c2740 "(+) PROVISIONS - FOR RETIREMENT - ADMIN EXPENSES"
    lab var c2750 "(+) PROVISIONS - FOR EVICTION - PRODUCTION COSTS"
    lab var c2760 "(+) PROVISIONS - FOR EVICTION - ADMIN EXPENSES"
    lab var c2770 "(+) PROVISIONS - FOR UNCOLLECTIBLE ACCOUNTS - ADMIN EXPENSES"
    lab var c2774 "(+) PROVISIONS - FOR NET INVENTORY REALIZATION - PRODUCTION COSTS"
    lab var c2772 "(+) PROVISIONS - FOR NET INVENTORY REALIZATION - ADMIN EXPENSES"
    lab var c2788 "(+) PROVISIONS - FOR ASSET VALUE DETERIORATION - PRODUCTION COSTS"
    lab var c2776 "(+) PROVISIONS - FOR ASSET VALUE DETERIORATION - ADMIN EXPENSES"
    lab var c2785 "(+) PROVISIONS - FOR ASSET VALUE DETERIORATION - PRODUCTION COSTS"
    lab var c2786 "(+) PROVISIONS - FOR ASSET VALUE DETERIORATION - ADMIN EXPENSES"
    lab var c2801 "(+) PROVISIONS - FOR ASSET VALUE DETERIORATION - ADMIN EXPENSES"
    lab var c2804 "(+) PROVISIONS - FOR ASSET VALUE DETERIORATION - PRODUCTION COSTS"
    lab var c2815 "(+) PROVISIONS - FOR ASSET VALUE DETERIORATION - ADMIN EXPENSES"
    lab var c2803 "(=) PROVISIONS - FOR ALL ASSET VALUE DETERIORATION"  // c2770 + c2774 + c2772 + c2788 + c2776 + c2785 + c2786 + c2801 + c2804 + c2815
    lab var c2780 "(+) PROVISIONS - OTHER PROVISIONS - PRODUCTION COSTS"
    lab var c2790 "(+) PROVISIONS - OTHER PROVISIONS - ADMIN EXPENSES"
    lab var c2800 "(=) TOTAL PROVISIONS" // c2730 + c2740 + c2750 + c2760 + c2803 + c2780 + c2790
    * Commercial rent
    lab var c2810 "(+) COMMERCIAL RENT - HOME - PRODUCTION COSTS"
    lab var c2820 "(+) COMMERCIAL RENT - HOME - ADMIN EXPENSES"
    lab var c2830 "(+) COMMERCIAL RENT - ABROAD - PRODUCTION COSTS"
    lab var c2840 "(+) COMMERCIAL RENT - ABROAD - ADMIN EXPENSES"
    lab var c2850 "(=) COMMERCIAL RENT - PRODUCTION COSTS" // c2810 + c2830
    lab var c2860 "(=) COMMERCIAL RENT - ADMIN EXPENSES" // c2820 + c2840
    * Commissions
    lab var c2870 "(+) COMMISSIONS - HOME - PRODUCTION COSTS"
    lab var c2880 "(+) COMMISSIONS - HOME - ADMIN EXPENSES"
    lab var c2890 "(+) COMMISSIONS - ABROAD - PRODUCTION COSTS"
    lab var c2900 "(+) COMMISSIONS - ABROAD - ADMIN EXPENSES"
    lab var c2910 "(=) TOTAL COMMISSIONS" // c2870 + c2880 + c2890 + c2900

* Regenerate sum variables to use instead of single cells when the values of the
* at least one single cell is different than 0. Keep the given sum instead.
replace c2420 = cond(c2200!=0 | c2220!=0, ///
                     c2200    + c2220   , c2420)
drop c2200 c2220 // considered in c2420
replace c2430 = cond(c2210!=0 | c2230!=0, ///
                     c2210    + c2230   , c2430)
drop c2210 c2230 // considered in c2430
replace c2650 = cond(c2470!=0 | c2660!=0, ///
                     c2470    + c2660,    c2650)
drop c2470 c2660 // considered in c2650
replace c2803 = cond(c2770!=0 | c2774!=0 | c2772!=0 | c2788!=0 | c2776!=0 | c2785!=0 | c2786!=0 | c2801!=0 | c2804!=0 | c2815!=0, ///
                     c2770    + c2774    + c2772    + c2788    + c2776    + c2785    + c2786    + c2801    + c2804    + c2815   , c2803)
drop c2770 c2774 c2772 c2788 c2776 c2785 c2786 c2801 c2804 c2815 // considered in c2803
replace c2800 = cond(c2730!=0 | c2740!=0 | c2750!=0 | c2760!=0 | c2803!=0 | c2780!=0 | c2790!=0, ///
                     c2730    + c2740    + c2750    + c2760    + c2803    + c2780    + c2790,    c2800)
drop c2730 c2740 c2750 c2760 c2803 c2780 c2790 // considered in c2800
replace c2850 = cond(c2810!=0 | c2830!=0, ///
                     c2810    + c2830,    c2850)
drop c2810 c2830 // considered in c2850
replace c2860 = cond(c2820!=0 | c2840!=0, ///
                     c2820    + c2840,    c2860)
drop c2820 c2840 // considered in c2860
replace c2910 = cond(c2870!=0 | c2880!=0 | c2890!=0 | c2900!=0, ///
                     c2870    + c2880    + c2890    + c2900,    c2910)
drop c2870 c2880 c2890 c2900 // considered in c2910

* Other operational costs by category
gen double cost_ops_rent = c2420 + c2430 + c2850 + c2860
gen double cost_ops_maintenance = c2440 + c2450
gen double cost_ops_fuel = c2640 + c2650
gen double cost_ops_marketing = c2670 + c2680
gen double cost_ops_supplies = c2690 + c2700
gen double cost_ops_transportation = c2710 + c2720
gen double cost_ops_provisions = c2800
gen double cost_ops_commissions = c2910
gen double cost_ops_other = c2110
gen double cost_ops_total = cost_ops_rent + cost_ops_maintenance + cost_ops_fuel + ///
                            cost_ops_marketing + cost_ops_supplies + cost_ops_transportation + ///
                            cost_ops_provisions + cost_ops_commissions + cost_ops_other

* Format and keep only total (for now)
format %20.2f cost_ops*
drop cost_ops_rent cost_ops_maintenance cost_ops_fuel cost_ops_marketing cost_ops_supplies ///
cost_ops_transportation cost_ops_provisions cost_ops_commissions cost_ops_other

////////////////////////////////////////////////////////////////////////////////

* INTERESTS

* Generate variables available only in normal tax files
cap gen c2500 = 0, before(c3040)
cap gen c2510 = 0, after(c2500)

* Label variables
cap gen c2500 = 0
cap gen c2510 = 0
lab var c2920 "(+) INTEREST PAID TO BANKS - HOME - PRODUCTION COSTS"
lab var c2930 "(+) INTEREST PAID TO BANKS - HOME - ADMIN EXPENSES"
lab var c2940 "(+) INTEREST PAID TO BANKS - ABROAD - PRODUCTION COSTS"
lab var c2950 "(+) INTEREST PAID TO BANKS - ABROAD - ADMIN EXPENSES"
lab var c2960 "(+) INTEREST PAID TO OTHERS - RELATED HOME - PRODUCTION COSTS"
lab var c2970 "(+) INTEREST PAID TO OTHERS - RELATED HOME - ADMIN EXPENSES"
lab var c2980 "(+) INTEREST PAID TO OTHERS - RELATED ABROAD - PRODUCTION COSTS"
lab var c2990 "(+) INTEREST PAID TO OTHERS - RELATED ABROAD - ADMIN EXPENSES"
lab var c3000 "(+) INTEREST PAID TO OTHERS - NOT RELATED HOME - PRODUCTION COSTS"
lab var c3010 "(+) INTEREST PAID TO OTHERS - NOT RELATED HOME - ADMIN EXPENSES"
lab var c3020 "(+) INTEREST PAID TO OTHERS - NOT RELATED ABROAD - PRODUCTION COSTS"
lab var c3030 "(+) INTEREST PAID TO OTHERS - NOT RELATED ABROAD - ADMIN EXPENSES"
lab var c2500 "(=) INTEREST PAID TO OTHERS - HOME" // c2960 + c2970 + c3000 + c3010
lab var c2510 "(=) INTEREST PAID TO OTHERS - ABROAD" // c2980 + c2990 + c3020 + c3030
lab var c3040 "(=) TOTAL INTEREST PAID" // c2920 + c2930 + c2940 + c2950 + c2960 + c2970 + c2980 + c2990 + c3000 + c3010 + c3020 + c3030

* Regenerate sum variables to use instead of single cells when the values of the
* at least one single cell is different than 0. Keep the given sum instead.
replace c2500 = cond(c2960!=0 | c2970!=0 | c3000!=0 | c3010!=0, ///
                     c2960    + c2970    + c3000    + c3010,    c2500)
drop c2960 c2970 c3000 c3010 // considered in c2500
replace c2510 = cond(c2980!=0 | c2990!=0 | c3020!=0 | c3030!=0, ///
                     c2980    + c2990    + c3020    + c3030,    c2510)
drop c2980 c2990 c3020 c3030 // considered in c2510
replace c3040 = cond(c2920!=0 | c2930!=0 | c2940!=0 | c2950!=0 | c2500!=0 | c2510!=0, ///
                     c2920    + c2930    + c2940    + c2950    + c2500    + c2510,    c3040)
drop c2920 c2930 c2940 c2950 c2500 c2510 // considered in c3040

* Interest costs
gen double cost_interest_total = c3040

* Format and keep only total (for now)
format %20.2f cost_interest*

////////////////////////////////////////////////////////////////////////////////

* LOSSES

* Generate variables available only in normal tax files
cap gen c2570 = 0, before(c3050)
cap gen c2580 = 0, after(c2570)

* Generate new variables available only in amendments
cap gen c3114 = 0, after(c3110)
cap gen c3116 = 0, after(c3114)

* c2570 & c2580 should always be zero, haven't accounted for them not being zero yet
cap assert round(c2570 + c2580)==round(c3090) if (c2570>0 | c2580>0) 
if _rc {
    di "c2570 [(+) LOSS FROM SALES OF FIXED ASSETS] or c2580 [(+) LOSS FROM SALES OF DEFERRED ASSETS] are not accounted for in c3090. What now?"
} 
drop c2570 c2580

* Label variables
lab var c3050 "(+) LOSS FROM SALE OF ASSETS - RELATED - PRODUCTION COSTS"
lab var c3060 "(+) LOSS FROM SALE OF ASSETS - RELATED - ADMIN EXPENSES"
lab var c3070 "(+) LOSS FROM SALE OF ASSETS - NOT RELATED - PRODUCTION COSTS"
lab var c3080 "(+) LOSS FROM SALE OF ASSETS - NOT RELATED - ADMIN EXPENSES"
lab var c3090 "(=) TOTAL LOSS FROM SALE OF ASSETS" // c3050 + c3060 + c3070 + c3080
lab var c3100 "(+) OTHER LOSSES - PRODUCTION COSTS"
lab var c3110 "(+) OTHER LOSSES - ADMIN EXPENSES"
lab var c3114 "(+) SHRINKAGES - PRODUCTION COSTS"
lab var c3116 "(+) SHRINKAGES - ADMIN EXPENSES"

* Regenerate sum variables to use instead of single cells when the values of the
* at least one single cell is different than 0. Keep the given sum instead.
replace c3090 = cond(c3050!=0 | c3060!=0 | c3070!=0 | c3080!=0, ///
                     c3050    + c3060    + c3070    + c3080,    c3090)
drop c3050 c3060 c3070 c3080 // considered in c3090

* Losses costs
gen double cost_losses_total = c3090 + c3100 + c3110 + c3114 + c3116

* Format and keep only total (for now)
format %20.2f cost_losses*


////////////////////////////////////////////////////////////////////////////////

* DEPRECIATIONS AND AMORTIZATIONS

* Generate variables available only in normal tax files
cap gen c2490 = 0, before(c3270)

* Generate new variables available only in amendments
cap gen c3242 = 0, after(c3250)
cap gen c3254 = 0, after(c3242)
cap gen c3256 = 0, after(c3254)
cap gen c3264 = 0, after(c3256)
cap gen c3266 = 0, after(c3264)
cap gen c3268 = 0, after(c3266)

* Label variables
    * Depreciations
    lab var c3220 "(+) DEPRECIATION OF FIXED ASSETS - ACCELERATED - PRODUCTION COSTS"
    lab var c3230 "(+) DEPRECIATION OF FIXED ASSETS - ACCELERATED - ADMIN EXPENSES"
    lab var c3240 "(+) DEPRECIATION OF FIXED ASSETS - NON ACCELERATED - PRODUCTION COSTS"
    lab var c3250 "(+) DEPRECIATION OF FIXED ASSETS - NON ACCELERATED - ADMIN EXPENSES"
    lab var c3260 "(=) TOTAL DEPRECIATION OF FIXED ASSETS" // c3220 + c3230 + c3240 + c3250
    lab var c3254 "(+) DEPRECIATION OF REVALUATION OF FIXED ASSETS - PRODUCTION COSTS"
    lab var c3256 "(+) DEPRECIATION OF REVALUATION OF FIXED ASSETS - ADMIN EXPENSES"
    lab var c3242 "(=) DEPRECIATION OF REVALUATION OF FIXED ASSETS" // c3254 + c3256
    lab var c3264 "(+) DEPRECIATION OF BIOLOGICAL ASSETS - PRODUCTION COSTS"
    lab var c3266 "(+) DEPRECIATION OF INVESTMENTS PROPERTIES - ADMIN EXPENSES"
    * Amortizations
    lab var c3268 "(+) AMORTIZATION OF RESEARCH AND DEVELOPMENT ASSETS - PRODUCTION COSTS"
    lab var c3270 "(+) AMORTIZATION - PRODUCTION COSTS"
    lab var c3280 "(+) AMORTIZATION - ADMIN EXPENSES"
    lab var c2490 "(+) AMORTIZATION OF BALANCE DEBTOR CURRENCY EXCHANGE"

* Regenerate sum variables to use instead of single cells when the values of the
* at least one single cell is different than 0. Keep the given sum instead.
replace c3260 = cond(c3220!=0 | c3230!=0 | c3240!=0 | c3250!=0, ///
                     c3220    + c3230    + c3240    + c3250,    c3260)
drop c3220 c3230 c3240 c3250 // considered in c3260
replace c3242 = cond(c3254!=0 | c3256!=0, ///
                     c3254    + c3256   , c3242)
drop c3254 c3256 // considered in c3260

* Turns out c3242 and c3260 are the same
gen double dep_FA_net = c3260
replace dep_FA_net = c3242 if c3242!=0
drop c3242 c3260

* Depreciations and amortizations costs
gen double cost_depreciations_total = dep_FA_net + c3264 + c3266
gen double cost_amortizations_total = c3268 + c3270 + c3280 + c2490

* Format and keep only total (for now)
format %20.2f cost_dep* cost_amo*
drop dep_FA_net

////////////////////////////////////////////////////////////////////////////////

* OTHER NON-OPERATIONAL COSTS

* Generate variables available only in normal tax files
cap gen c2480 = 0, before(c3120)
cap gen c2520 = 0, after(c2480)
cap gen c2530 = 0, after(c2520)
cap gen c2540 = 0, after(c2530)
cap gen c2550 = 0, after(c2540)
cap gen c2560 = 0, after(c2550)
cap gen c2590 = 0, after(c2560)
cap gen c2600 = 0, after(c2590)

* Generate new variables available only in amendments
cap gen c2701 = 0, before(c3170)
cap gen c2892 = 0, after(c2701)
cap gen c2893 = 0, after(c2892)
cap gen c2898 = 0, after(c2893)
cap gen c2899 = 0, after(c2898)
cap gen c2902 = 0, after(c2899)
cap gen c3171 = 0, after(c2902)
cap gen c3202 = 0, after(c3171)
cap gen c3212 = 0, after(c3202)
cap gen c3214 = 0, after(c3212)
cap gen c3008 = 0, after(c3214)
cap gen c3012 = 0, after(c3008)

* c2600 should always be zero, haven't accounted for it not being zero yet
cap assert c2600==0
if _rc {
    di "c2600 [(=) TOTAL NON-OPERATIONAL EXPENSES] is not zero. What now?"
} 
drop c2600

* Label variables
    * Random stuff from old forms
    lab var c2480 "(+) NOTARY AND REPRESENTATIVES FEES"
    lab var c2520 "(+) REIMBURSEMENT EXPENSES - HOME"
    lab var c2530 "(+) REIMBURSEMENT EXPENSES - ABROAD"
    lab var c2540 "(+) OTHER EXPENSES - HOME"
    lab var c2550 "(+) OTHER EXPENSES - ABROAD"
    lab var c2560 "(+) OTHER ADMINISTRATIVE SALES-RELATED EXPENSES"
    lab var c2590 "(+) OTHER NON-OPERATIONAL EXPENSES"
    * Random stuff from new forms
    lab var c2701 "(+) UNSURE"
    lab var c2892 "(+) UNSURE - PRODUCTION COSTS"
    lab var c2893 "(+) UNSURE - ADMIN EXPENSES"
    lab var c2898 "(+) UNSURE - PRODUCTION COSTS"
    lab var c2899 "(+) UNSURE - ADMIN EXPENSES"
    lab var c2902 "(=) UNSURE - ADMIN EXPENSES" // c2892 + c2893
    lab var c3202 "(+) UNSURE"
    lab var c3008 "(+) OTHER NON-OPERATIONAL EXPENSES"
    lab var c3012 "(+) UNSURE"
    * Other administrative costs
    lab var c3120 "(+) INSURANCE AND REINSURANCE INTERMEDIARIES - PRODUCTION COSTS"
    lab var c3130 "(+) INSURANCE AND REINSURANCE INTERMEDIARIES - ADMIN EXPENSES"
    lab var c3140 "(+) INDIRECT COSTS INCURRED FROM ABROAD BY RELATED PARTIES - PRODUCTION COSTS"
    lab var c3150 "(+) INDIRECT COSTS INCURRED FROM ABROAD BY RELATED PARTIES - ADMIN EXPENSES"
    lab var c3160 "(+) ADMINISTRATIVE COSTS - ADMIN EXPENSES"
    lab var c3171 "(+) TAXES, CONTRIBUTIONS AND OTHER - PRODUCTION COSTS"
    lab var c3170 "(+) TAXES, CONTRIBUTIONS AND OTHER - ADMIN EXPENSES"
    lab var c3180 "(+) TRAVEL EXPENSES - PRODUCTION COSTS"
    lab var c3190 "(+) TRAVEL EXPENSES - ADMIN EXPENSES"
    lab var c3200 "(+) VAT INCURRED ON COSTS OR EXPENSES - PRODUCTION COSTS"
    lab var c3210 "(+) VAT INCURRED ON COSTS OR EXPENSES - ADMIN EXPENSES"
    lab var c3212 "(+) OTHER EXPENSES - PRODUCTION COSTS"
    lab var c3214 "(+) OTHER EXPENSES - ADMIN EXPENSES"
    * Public and other services/goods
    lab var c3290 "(+) PUBLIC SERVICES - PRODUCTION COSTS"
    lab var c3300 "(+) PUBLIC SERVICES - ADMIN EXPENSES"
    lab var c3310 "(+) PAYMENT FOR OTHER SERVICES - PRODUCTION COSTS"
    lab var c3320 "(+) PAYMENT FOR OTHER SERVICES - ADMIN EXPENSES"
    lab var c3330 "(+) PAYMENT FOR OTHER GOODS - PRODUCTION COSTS"
    lab var c3340 "(+) PAYMENT FOR OTHER GOODS - ADMIN EXPENSES"
    lab var c3350 "(=) OTHER PRODUCTION COSTS" // c3310 + c3330

* Regenerate sum variables to use instead of single cells when the values of the
* at least one single cell is different than 0. Keep the given sum instead.
replace c2902 = cond(c2892!=0 | c2893!=0, ///
                     c2892    + c2893,    c2902)
drop c2892 c2893 // considered in c2902
replace c3350 = cond(c3310!=0 | c3330!=0, ///
                     c3310    + c3330,    c3350)
drop c3310 c3330 // considered in c3350

* Other costs
gen double cost_admin_total = c2480 + c2520 + c2530 + c2540 + c2550 + c2560 + c2590 + ///
                              c2701 + c2898 + c2899 + c2902 + c3202 + c3008 + c3012 + ///
                              c3120 + c3130 + c3140 + c3150 + c3160 + c3171 + ///
                              c3170 + c3180 + c3190 + c3200 + c3210 + c3212 + c3214 + ///
                              c3290 + c3300 + c3320 + c3340 + c3350

////////////////////////////////////////////////////////////////////////////////


* Sums and informative items
cap gen c2610 = 0
cap gen c2620 = 0
cap gen c2630 = 0
lab var c3360 "(=) TOTAL COSTS"                                             // Total - PRODUCTION COSTS
lab var c3370 "(=) TOTAL EXPENDITURES"                                      // Total - ADMIN EXPENSES
lab var c3380 "(=) TOTAL COSTS AND EXPENDITURES" // (c3360+c3370)
lab var c3390 "( ) LOSS OF INVENTORY"
lab var c3400 "( ) PAYMENT FOR REIMBURSEMENT PAID AS REIMBURSER"
lab var c3410 "( ) PAYMENT FOR REIMBURSEMENT PAID AS INTERMEDIARY"
lab var c2610 "(=) NET PROFIT BEFORE 'CCL'" // don't know what CCL is, no need
lab var c2620 "(=) PROFIT BEFORE 'CCL'" // don't know what CCL is, no need
lab var c2630 "(=) PROFIT BEFORE 'CCL'" // don't know what CCL is, no need
drop c2610 c2620 c2630 // no need for intermediate results
drop c3390 c3400 c3410 // no need for informative cells

* Create new sums
gen double tot_C = c3360
gen double tot_C_calc = cost_prod_total + cost_labour_total + cost_ops_total + ///
                            cost_interest_total + cost_losses_total + cost_depreciations_total + ///
                            cost_amortizations_total + cost_admin_total
format %20.2f tot_*
lab var tot_C      "(=) TOTAL COSTS - REPORTED"
lab var tot_C_calc "(=) TOTAL COSTS - CALCULATED"
cap drop c1* 
cap drop c2*
cap drop c3*
cap drop c4*
cap drop c5*
cap drop c6*
cap drop c7*
cap drop c8*
cap drop c9*

 gen same = round(tot_C)==round(tot_C_calc)

tab same

end
