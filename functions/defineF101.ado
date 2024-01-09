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

////////////////////////////////////////////////////////////////////////////////
**#                             1 - LABEL FORM CELLS
////////////////////////////////////////////////////////////////////////////////
use "/home/mivazq/data/transactions_ecuador/1_rawdata/F101/F101_2008_jul2012.dta" , clear
order c*, seq

*** TRANSACTIONS WITH RELATED PARTIES ABROAD DURING THE FISCAL PERIOD
cap lab var c110  "RECORD"
cap lab var c112  "(+) ASSETS WITH RELATED PARTIES ABROAD - TAX HEAVEN"
cap lab var c113  "(+) LIABILITIES WITH RELATED PARTIES ABROAD - TAX HEAVEN"
cap lab var c114  "(+) INCOME WITH RELATED PARTIES ABROAD - TAX HEAVEN"
cap lab var c115  "(+) EXPENDITURE WITH RELATED PARTIES ABROAD - TAX HEAVEN"
cap lab var c120  "(+) ASSETS WITH RELATED PARTIES ABROAD - OTHER REGIME"
cap lab var c130  "(+) LIABILITIES WITH RELATED PARTIES ABROAD - OTHER REGIME"
cap lab var c140  "(+) INCOME WITH RELATED PARTIES ABROAD - OTHER REGIME"
cap lab var c150  "(+) EXPENDITURE WITH RELATED PARTIES ABROAD - OTHER REGIME"
cap lab var c160  "(=) TOTAL OPERATIONS WITH RELATED PARTIES FROM ABROAD"       // Total

* Drop (not needed)
forval i=110/169 {
    cap drop c`i'
}

////////////////////////////////////////////////////////////////////////////////

*** BALANCE SHEET - ASSETS
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

* Regenerate sum variables to use instead of single cells (seems to work better)
recast double c170-c470
replace c280 = cond(c200!=0 | c210!=0, c200 + c210, c280) // keep sum if singles are zero
replace c290 = cond(c220!=0 | c230!=0, c220 + c230, c290) // keep sum if singles are zero
replace c300 = cond(c240!=0 | c250!=0 | c260!=0 | c270!=0, c240 + c250 + c260 + c270, c300) // keep sum if singles are zero
replace c330 = cond(c340!=0 | c350!=0, c340 + c350, c330) // keep sum if singles are zero
replace c420 = cond(c430!=0 | c440!=0 | c450!=0, c430 + c440 + c450, c420) // keep sum if singles are zero
drop c200 c210           // considered in c280
drop c220 c230           // considered in c290
drop c240 c250 c260 c270 // considered in c300
drop c340 c350           // considered in c330
drop c430 c440 c450      // considered in c420

* Store final sums (reported and calculated) and drop not needed remaining cells
gen double tot_CA_prov = cond(c310>0, -c310, c310)
gen double tot_CA      = c470
gen double tot_CA_calc = c170 + c180 + c190 + c280 + c290 + c300 + c320 + c330 + c360 + c370 + c380 + c390 + c400 + c410 + c420 + c460 + tot_CA_prov
format %20.0g tot_*
lab var tot_CA      "(=) TOTAL CURRENT ASSETS - REPORTED"
lab var tot_CA_calc "(=) TOTAL CURRENT ASSETS - CALCULATED"
lab var tot_CA_prov "(-) TOTAL CURRENT ASSETS PROVISIONS"
drop c170 c180 c190 c280 c290 c300 c310 c320 c330 c360 c370 c380 c390 c400 c410 c420 c460 c470

////////////////////////////////////////////////////////////////////////////////

* FIXED ASSETS (TANGIBLE)
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

* Regenerate sum variables to use instead of single cells (seems to work better)
replace c510 = cond(c630!=0 | c640!=0, c630 + c640, c510) // keep sum if singles are zero
replace c520 = cond(c500!=0 | c510!=0, c500 + c510, c520) // keep sum if singles are zero
replace c530 = cond(c480!=0 | c520!=0, c480 + c520, c530) // keep sum if singles are zero
// replace c600 = cond(c480!=0 | c630!=0, c480 + c630, c600) // keep sum if singles are zero    MVV: dropped above
// replace c620 = cond(c500!=0 | c640!=0, c500 + c640, c620) // keep sum if singles are zero    MVV: dropped above
replace c570 = cond(c580!=0 | c660!=0 | c670!=0, c580 + c660 + c670, c570)
drop c630 c640            // considered in c510
drop c480 c500 c510 c520  // considered in c530 (directly or indirectly)
drop c580 c660 c670       // all these depreciations have been summed up

* Fix totals (firms have reported differently)
recast double c680-c720
replace c680 = cond(c720==0, c690, c690-c720) if c680==0 // replace with diff total - intangible when tangible is 0

* Store final sums (reported and calculated) and drop not needed remaining cells
gen double tot_FA_acc_dep = cond(c570>0, -c570, c570)
gen double tot_FA      = c680
gen double tot_FA_calc = c490 + c530 + c540 + c550 + c560 + c590 + c650 + tot_FA_acc_dep
format %20.0g tot_*
lab var tot_FA         "(=) TOTAL FIXED ASSETS - REPORTED"
lab var tot_FA_calc    "(=) TOTAL FIXED ASSETS - CALCULATED"
lab var tot_FA_acc_dep "(-) TOTAL FIXED ASSETS ACCUMULATED DEPRECIATION"
drop c490 c530 c540 c550 c560 c570 c590 c650 c680 c690

////////////////////////////////////////////////////////////////////////////////

* DEFERRED ASSETS (INTANGIBLE)
cap lab var c700  "(+) TRADEMARKS, PATENTS, ETC."
cap lab var c710  "(-) ACCUMULATED DEPRECIATION TRADEMARKS, PATENTS, ETC."
cap lab var c720  "(=) TOTAL INTANGIBLE FIXED ASSETS"
cap lab var c730  "(+) ORGANIZATIONAL COSTS"
cap lab var c740  "(+) RESEARCH AND EXPLORATION COSTS"
cap lab var c750  "(+) BALANCE DEBTOR CURRENCY EXCHANGE"
cap lab var c760  "(+) OTHER DEFERRED ASSETS"
cap lab var c770  "(-) ACCUMULATED DEPRECIATION OTHER"
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
gen double tot_DA_acc_dep = cond(c770>0, -c770, c770)
gen double tot_DA         = c780
gen double tot_DA_calc    = c700 + c730 + c740 + c750 + c760 - c770
format %20.0g tot_*
lab var tot_DA         "(=) TOTAL DEFERRED ASSETS - REPORTED"
lab var tot_DA_calc    "(=) TOTAL DEFERRED ASSETS - CALCULATED"
lab var tot_DA_acc_dep "(-) TOTAL DEFERRED ASSETS ACCUMULATED DEPRECIATION"
drop c700 c730 c740 c750 c760 c770 c780

////////////////////////////////////////////////////////////////////////////////

* LONG TERM ASSETS
cap lab var c790  "(+) LONG TERM INVESTMENTS - SHARES AND OTHER EQUITY"
cap lab var c800  "(+) LONG TERM INVESTMENTS - OTHER"
cap lab var c810  "(+) LONG TERM ACCOUNTS RECEIVABLE FROM NATURAL FIRMS OR PERSONS - RELATED HOME"
cap lab var c820  "(+) LONG TERM ACCOUNTS RECEIVABLE FROM NATURAL FIRMS OR PERSONS - RELATED ABROAD"
cap lab var c830  "(+) LONG TERM ACCOUNTS RECEIVABLE FROM NATURAL FIRMS OR PERSONS - NOT RELATED HOME"
cap lab var c840  "(+) LONG TERM ACCOUNTS RECEIVABLE FROM NATURAL FIRMS OR PERSONS - NOT RELATED ABROAD"
cap lab var c850  "(=) LONG TERM ACCOUNTS RECEIVABLE FROM NATURAL FIRMS OR PERSONS" // c810 + c820 + c830 + c840
cap lab var c860  "(+) OTHER LONG TERM ACCOUNTS RECEIVABLE - RELATED HOME"
cap lab var c870  "(+) OTHER LONG TERM ACCOUNTS RECEIVABLE - RELATED ABROAD"
cap lab var c880  "(+) OTHER LONG TERM ACCOUNTS RECEIVABLE - NOT RELATED HOME"
cap lab var c890  "(+) OTHER LONG TERM ACCOUNTS RECEIVABLE - NOT RELATED ABROAD"
cap lab var c900  "(=) OTHER LONG TERM ACCOUNTS RECEIVABLE"  // c860 + c870 + c880 + c890
cap lab var c910  "(-) PROVISION FOR UNCOLLECTIBLE ACCOUNTS"
cap lab var c1010 "(+) OTHER LONG TERM ASSETS"
cap lab var c1070 "(=) TOTAL LONG TERM ASSETS"                                  // Total

* Regenerate sum variables to use instead of single cells (seems to work better)
recast double c790-c1070

replace c850 = cond(c810!=0 | c820!=0 | c830!=0 | c840!=0, c810 + c820 + c830 + c840, c850) // keep sum if singles are zero
replace c900 = cond(c860!=0 | c870!=0 | c880!=0 | c890!=0, c860 + c870 + c880 + c890, c890) // keep sum if singles are zero
drop c810 c820 c830 c840 // considered in c850
drop c860 c870 c880 c840 // considered in c900




gen same = round(tot_DA,0.01)==round(tot_DA_calc_old,0.01)
gen same_new = round(tot_DA,0.01)==round(tot_DA_calc,0.01)

tab same same_new





* Check for negatives
forval i=70/80 {
    local n = `i'*10
    di "c`n'"
    count if c`n'<0
}








* TOTAL ASSETS
cap lab var c1075 "( ) ASSETS COMING FROM REINVESTMENT OF PROFITS"
cap lab var c1080 "(=) TOTAL ASSETS" // (c470+c690+c780+c1070)
















*** BALANCE SHEET - LIABILITIES & EQUITIES
* CURRENT LIABILITIES
cap lab var c1110 "(+) ACCOUNTS AND NOTES PAYABLE TO CURRENT SUPPLIERS - RELATED HOME"
cap lab var c1120 "(+) ACCOUNTS AND NOTES PAYABLE TO CURRENT SUPPLIERS - RELATED ABROAD"
cap lab var c1130 "(+) ACCOUNTS AND NOTES PAYABLE TO CURRENT SUPPLIERS - NOT RELATED HOME"
cap lab var c1140 "(+) ACCOUNTS AND NOTES PAYABLE TO CURRENT SUPPLIERS - NOT RELATED ABROAD"
cap lab var c1180 "(+) CURRENT OBLIGATIONS WITH FINANCIAL INSTITUTIONS - HOME"
cap lab var c1190 "(+) CURRENT OBLIGATIONS WITH FINANCIAL INSTITUTIONS - ABROAD"
cap lab var c1200 "(+) CURRENT LOANS FROM SHAREHOLDERS - HOME"
cap lab var c1210 "(+) CURRENT LOANS FROM SHAREHOLDERS - ABROAD"
cap lab var c1240 "(+) OTHER CURRENT ACCOUNTS AND NOTES PAYABLE - RELATED HOME"
cap lab var c1250 "(+) OTHER CURRENT ACCOUNTS AND NOTES PAYABLE - RELATED ABROAD"
cap lab var c1260 "(+) OTHER CURRENT ACCOUNTS AND NOTES PAYABLE - NOT RELATED HOME"
cap lab var c1270 "(+) OTHER CURRENT ACCOUNTS AND NOTES PAYABLE - NOT RELATED ABROAD"
cap lab var c1280 "(+) INCOME TAX TO BE PAID IN FISCAL PERIOD"
cap lab var c1290 "(+) WORKERS' PROFITS TO BE PAID IN FISCAL PERIOD"
cap lab var c1300 "(+) TRANSFERS TO THE HEAD OFFICE AND BRANCHES (OVERSEAS)" 
cap lab var c1310 "(+) CURRENT CREDITO A MUTUO"
cap lab var c1320 "(+) SHORT TERM OBLIGATIONS ISSUED"
cap lab var c1330 "(+) OTHER PROVISIONS"
cap lab var c1340 "(=) TOTAL CURRENT LIABILITIES"                               // Total

* LONG TERM LIABILITIES
cap lab var c1350 "(+) LONG TERM ACCOUNTS AND NOTES PAYABLE TO SUPPLIERS - RELATED - HOME"
cap lab var c1360 "(+) LONG TERM ACCOUNTS AND NOTES PAYABLE TO SUPPLIERS - RELATED - ABROAD"
cap lab var c1370 "(+) LONG TERM ACCOUNTS AND NOTES PAYABLE TO SUPPLIERS - NOT RELATED - HOME"
cap lab var c1380 "(+) LONG TERM ACCOUNTS AND NOTES PAYABLE TO SUPPLIERS - NOT RELATED - ABROAD"
cap lab var c1420 "(+) LONG TERM OBLIGATIONS TO FINANCIAL INSTITUTIONS - HOME"
cap lab var c1430 "(+) LONG TERM OBLIGATIONS TO FINANCIAL INSTITUTIONS - ABROAD"
cap lab var c1450 "(+) LONG TERM LOANS FROM SHAREHOLDERS - HOME"
cap lab var c1460 "(+) LONG TERM LOANS FROM SHAREHOLDERS - ABROAD"
cap lab var c1480 "(+) OTHER LONG TERM ACCOUNTS AND NOTES PAYABLE - RELATED HOME"
cap lab var c1490 "(+) OTHER LONG TERM ACCOUNTS AND NOTES PAYABLE - RELATED ABROAD"
cap lab var c1500 "(+) OTHER LONG TERM ACCOUNTS AND NOTES PAYABLE - NOT RELATED HOME"
cap lab var c1510 "(+) OTHER LONG TERM ACCOUNTS AND NOTES PAYABLE - NOT RELATED ABROAD"
cap lab var c1530 "(+) LONG TERM TRANSFERS TO THE HEAD OFFICE AND BRANCHES (OVERSEAS)"
cap lab var c1540 "(+) LONG TERM CREDITO A MUTUO"
cap lab var c1550 "(+) LONG TERM OBLIGATIONS ISSUED"
cap lab var c1560 "(+) PROVISIONS FOR RETIREMENT"
cap lab var c1570 "(+) PROVISIONS FOR LAYOFFS"
cap lab var c1580 "(+) OTHER PROVISIONS (WITH TERMS >1 YEAR)"
cap lab var c1590 "(=) TOTAL LONG TERM LIABILITIES"                             // Total

* TOTAL LIABILITIES
cap lab var c1600 "(+) DEFFERED LIABILITIES"                                    // "Total"
cap lab var c1610 "(+) OTHER LIABILITIES"                                       // "Total"
cap lab var c1620 "(=) TOTAL LIABILITIES" // (c1340+c1590+c1600+c1610)          // Liability Total

* EQUITIES
cap lab var c1630 "(+) SUBSCRIBED AND/OR ISSUED CAPITAL"
cap lab var c1640 "(-) SUBSCRIBED CAPITAL UNPAID, TREASURY SHARES"
cap lab var c1650 "(+) CAPITAL CONTRIBUTIONS BY SHAREHOLDERS OR PARTNERS"
cap lab var c1660 "(+) LEGAL RESERVE"
cap lab var c1720 "(+) OTHER RESERVES"
cap lab var c1740 "(+) PROFIT NOT DISTRIBUTED (REINVESTED) IN PREVIOUS PERIODS"
cap lab var c1750 "(-) ACCUMULATED LOSS FROM PREVIOUS PERIODS"
cap lab var c1760 "(+) NET INCOME (NET OF EMPLOYEE SHARES AND INCOME TAX)"
cap lab var c1770 "(-) LOSS FOR THE PERIOD"

* TOTAL EQUITIES
cap lab var c1780 "(=) TOTAL EQUITY"                                        // Equity Total

* TOTAL LIABILITIES
cap lab var c1790 "(=) TOTAL LIABILITIES AND EQUITY" // (c1620+c1780)


*** INCOME STATEMENT - INCOME
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
cap lab var c1910 "(+) EXEMPTED INCOME FROM DONATIONS AND CONTRIBUTIONS" // (c1880+c1890+c1900)
cap lab var c1920 "(+) OTHER EXEMPTED INCOME"
cap lab var c1930 "(=) TOTAL INCOME"                                            // Total

cap lab var c1940 "( ) NET SALES OF FIXED ASSETS"
cap lab var c1950 "( ) REIMBURSEMENT RECEIVED AS INTERMEDIARY"


*** INCOME STATEMENT - COSTS & EXPENSES
cap lab var c1960 "(+) INITIAL INVENTORY OF GOODS NOT PROUDCED BY THE FIRM - PRODUCTION COSTS"
cap lab var c1970 "(+) NET PURCHASES OF DOMESTIC GOODS NOT PRODUCED BY THE FIRM - PRODUCTION COSTS"
cap lab var c1980 "(+) IMPORTS NOT PRODUCED BY THE FIRM - PRODUCTION COSTS"
cap lab var c1990 "(+) IMPORTS NOT PRODUCED BY THE FIRM - ADMIN EXPENSES"
cap lab var c2000 "(-) FINAL INVENTORY OF GOOD NOT PRODUCED BY THE FIRM - PRODUCTION COSTS"
cap lab var c2010 "(+) INITIAL INVENTORY OF RAW MATERIAL - PRODUCTION COSTS"
cap lab var c2020 "(+) NET DOMESTIC PURCHES OF RAW MATERIAL - PRODUCTION COSTS"
cap lab var c2030 "(+) IMPORTS OF RAW MATERIAL - PRODUCTION COSTS"
cap lab var c2040 "(-) FINAL INVENTORY OF RAW MATERIAL - PRODUCTION COSTS"
cap lab var c2240 "(+) INITIAL INVENTORY OF PRODUCTS IN PROCESS - PRODUCTION COSTS"
cap lab var c2250 "(-) FINAL INVENTORY OF PRODUCTS IN PROCESS - PRODUCTION COSTS"
cap lab var c2260 "(+) INITIAL INVENTORY OF FINISHED PRODUCTS - PRODUCTION COSTS"
cap lab var c2270 "(-) FINAL INVENTORY OF FINISHED PRODUCTS - PRODUCTION COSTS"

cap lab var c2280 "(+) WAGES, SALARIES AND OTHER TAXABLE REMUNERATIONS - PRODUCTION COSTS"
cap lab var c2290 "(+) WAGES, SALARIES AND OTHER TAXABLE REMUNERATIONS - ADMIN EXPENSES"
cap lab var c2300 "(+) SOCIAL BENEFITS AND OTHER NON-TAXABLE COMPENSATION - PRODUCTION COSTS"
cap lab var c2310 "(+) SOCIAL BENEFITS AND OTHER NON-TAXABLE COMPENSATION - ADMIN EXPENSES"
cap lab var c2360 "(+) CONTRIBUTION TO SOCIAL SECURITY (INCLUDING RESERVE FUND) - PRODUCTION COSTS"
cap lab var c2370 "(+) CONTRIBUTION TO SOCIAL SECURITY (INCLUDING RESERVE FUND) - ADMIN EXPENSES"
cap lab var c2380 "(+) PROFESSIONAL FEES AND EXPENSES - PRODUCTION COSTS"
cap lab var c2390 "(+) PROFESSIONAL FEES AND EXPENSES - ADMIN EXPENSES"
cap lab var c2400 "(+) FEES TO FOREIGNERS FOR ONE-TIME EXPENSES - PRODUCTION COSTS"
cap lab var c2410 "(+) FEES TO FOREIGNERS FOR ONE-TIME EXPENSES - ADMIN EXPENSES"
cap lab var c2420 "(+) REAL ESTATE RENT - PRODUCTION COSTS"
cap lab var c2430 "(+) REAL ESTATE RENT - ADMIN EXPENSES"
cap lab var c2440 "(+) MAINTENANCE AND REPAIRS - PRODUCTION COSTS"
cap lab var c2450 "(+) MAINTENANCE AND REPAIRS - ADMIN EXPENSES"
cap lab var c2640 "(+) FUEL - PRODUCTION COSTS"
cap lab var c2650 "(+) FUEL - ADMIN EXPENSES"
cap lab var c2670 "(+) MARKETING - PRODUCTION COSTS"
cap lab var c2680 "(+) MARKETING - ADMIN EXPENSES"
cap lab var c2690 "(+) SUPPLIES AND MATERIALS - PRODUCTION COSTS"
cap lab var c2700 "(+) SUPPLIES AND MATERIALS - ADMIN EXPENSES"
cap lab var c2710 "(+) TRANSPORTATION - PRODUCTION COSTS"
cap lab var c2720 "(+) TRANSPORTATION - ADMIN EXPENSES"

cap lab var c2730 "(+) PROVISIONS - FOR RETIREMENT - PRODUCTION COSTS"
cap lab var c2740 "(+) PROVISIONS - FOR RETIREMENT - ADMIN EXPENSES"
cap lab var c2750 "(+) PROVISIONS - FOR EVICTION - PRODUCTION COSTS"
cap lab var c2760 "(+) PROVISIONS - FOR EVICTION - ADMIN EXPENSES"
cap lab var c2770 "(+) PROVISIONS - FOR UNCOLLECTIBLE ACCOUNTS - ADMIN EXPENSES"
cap lab var c2780 "(+) PROVISIONS - OTHER PROVISIONS - PRODUCTION COSTS"
cap lab var c2790 "(+) PROVISIONS - OTHER PROVISIONS - ADMIN EXPENSES"
cap lab var c2810 "(+) COMMERCIAL LEASING - HOME - PRODUCTION COSTS"
cap lab var c2820 "(+) COMMERCIAL LEASING - HOME - ADMIN EXPENSES"
cap lab var c2830 "(+) COMMERCIAL LEASING - ABROAD - PRODUCTION COSTS"
cap lab var c2840 "(+) COMMERCIAL LEASING - ABROAD - ADMIN EXPENSES"
cap lab var c2870 "(+) COMMISSIONS - HOME - PRODUCTION COSTS"
cap lab var c2880 "(+) COMMISSIONS - HOME - ADMIN EXPENSES"
cap lab var c2890 "(+) COMMISSIONS - ABROAD - PRODUCTION COSTS"
cap lab var c2900 "(+) COMMISSIONS - ABROAD - ADMIN EXPENSES"
cap lab var c2920 "(+) BANK INTEREST - HOME - PRODUCTION COSTS"
cap lab var c2930 "(+) BANK INTEREST - HOME - ADMIN EXPENSES"
cap lab var c2940 "(+) BANK INTEREST - ABROAD - PRODUCTION COSTS"
cap lab var c2950 "(+) BANK INTEREST - ABROAD - ADMIN EXPENSES"
cap lab var c2960 "(+) INTERESES PAGADOS A TERCEROS - RELATED HOME - PRODUCTION COSTS"
cap lab var c2970 "(+) INTERESES PAGADOS A TERCEROS - RELATED HOME - ADMIN EXPENSES"
cap lab var c2980 "(+) INTERESES PAGADOS A TERCEROS - RELATED ABROAD - PRODUCTION COSTS"
cap lab var c2990 "(+) INTERESES PAGADOS A TERCEROS - RELATED ABROAD - ADMIN EXPENSES"
cap lab var c3000 "(+) INTERESES PAGADOS A TERCEROS - NOT RELATED HOME - PRODUCTION COSTS"
cap lab var c3010 "(+) INTERESES PAGADOS A TERCEROS - NOT RELATED HOME - ADMIN EXPENSES"
cap lab var c3020 "(+) INTERESES PAGADOS A TERCEROS - NOT RELATED ABROAD - PRODUCTION COSTS"
cap lab var c3030 "(+) INTERESES PAGADOS A TERCEROS - NOT RELATED ABROAD - ADMIN EXPENSES"
cap lab var c3050 "(+) LOSS FROM SALE OF ASSETS - RELATED - PRODUCTION COSTS"
cap lab var c3060 "(+) LOSS FROM SALE OF ASSETS - RELATED - ADMIN EXPENSES"
cap lab var c3070 "(+) LOSS FROM SALE OF ASSETS - NOT RELATED - PRODUCTION COSTS"
cap lab var c3080 "(+) LOSS FROM SALE OF ASSETS - NOT RELATED - ADMIN EXPENSES"

cap lab var c3100 "(+) OTHER LOSSES - PRODUCTION COSTS"
cap lab var c3110 "(+) OTHER LOSSES - ADMIN EXPENSES"
cap lab var c3120 "(+) INSURANCE AND REINSURANCE INTERMEDIARIES - PRODUCTION COSTS"
cap lab var c3130 "(+) INSURANCE AND REINSURANCE INTERMEDIARIES - ADMIN EXPENSES"
cap lab var c3140 "(+) INDIRECT COSTS INCURRED FROM ABROAD BY RELATED PARTIES - PRODUCTION COSTS"
cap lab var c3150 "(+) INDIRECT COSTS INCURRED FROM ABROAD BY RELATED PARTIES - ADMIN EXPENSES"
cap lab var c3160 "(+) ADMINISTRATIVE COSTS - ADMIN EXPENSES"
cap lab var c3170 "(+) TAXES, CONTRIBUTIONS AND OTHER - ADMIN EXPENSES"
cap lab var c3180 "(+) TRAVEL EXPENSES - PRODUCTION COSTS"
cap lab var c3190 "(+) TRAVEL EXPENSES - ADMIN EXPENSES"
cap lab var c3200 "(+) VAT INCURRED ON COSTS OR EXPENSES - PRODUCTION COSTS"
cap lab var c3210 "(+) VAT INCURRED ON COSTS OR EXPENSES - ADMIN EXPENSES"
cap lab var c3220 "(+) DEPRECIATION OF FIXED ASSETS - ACCELERATED - PRODUCTION COSTS"
cap lab var c3230 "(+) DEPRECIATION OF FIXED ASSETS - ACCELERATED - ADMIN EXPENSES"
cap lab var c3240 "(+) DEPRECIATION OF FIXED ASSETS - NON ACCELERATED - PRODUCTION COSTS"
cap lab var c3250 "(+) DEPRECIATION OF FIXED ASSETS - NON ACCELERATED - ADMIN EXPENSES"
cap lab var c3270 "(+) AMORTIZATION - PRODUCTION COSTS"
cap lab var c3280 "(+) AMORTIZATION - ADMIN EXPENSES"
cap lab var c3290 "(+) PUBLIC SERVICES - PRODUCTION COSTS"
cap lab var c3300 "(+) PUBLIC SERVICES - ADMIN EXPENSES"
cap lab var c3310 "(+) PAYMENT FOR OTHER SERVICES - PRODUCTION COSTS"
cap lab var c3320 "(+) PAYMENT FOR OTHER SERVICES - ADMIN EXPENSES"
cap lab var c3330 "(+) PAYMENT FOR OTHER GOODS - PRODUCTION COSTS"
cap lab var c3340 "(+) PAYMENT FOR OTHER GOODS - ADMIN EXPENSES"

cap lab var c3360 "(=) TOTAL COSTS"                                             // Total - PRODUCTION COSTS
cap lab var c3370 "(=) TOTAL EXPENDITURES"                                      // Total - ADMIN EXPENSES
cap lab var c3380 "(=) TOTAL COSTS AND EXPENDITURES" // (c3360+c3370)

cap lab var c3390 "( ) LOSS OF INVENTORY"
cap lab var c3400 "( ) PAYMENT FOR REIMBURSEMENT PAID AS REIMBURSER"
cap lab var c3410 "( ) PAYMENT FOR REIMBURSEMENT PAID AS INTERMEDIARY"


*** TAX BALANCE
* PROFITS/LOSSES
cap lab var c3420 "(=) PROFIT THIS PERIOD"
cap lab var c3430 "(=) LOSS THIS PERIOD"
cap lab var c3440 "(-) 15% EMPLOYEES' SHARE"
cap lab var c3450 "(-) 100% EXEMPT DIVIDENDS RECEIVED"
cap lab var c3460 "(-) 100% OTHER EXEMPT RENTS"
cap lab var c3470 "(+) NON-DEDUCTIBLE DOMESTIC EXPENSES"
cap lab var c3480 "(+) NON-DEDUCTIBLE EXPENSES FROM ABROAD"
cap lab var c3490 "(+) EXPENSES TO GENERATE EXEMPT INCOME"
cap lab var c3500 "(+) EMPLOYEES' SHARE ATTRIBUTABLE TO EXEMPT INCOME"
cap lab var c3510 "(-) AMORTIZATION OF LOSSES FROM PREVIOUS YEARS"
cap lab var c3520 "(-) DEDUCTIONS FOR SPECIAL LAWS"
cap lab var c3530 "(+) ADJUSTMENT FOR TRANSFER PRICING"
cap lab var c3540 "(-) DEDUCTION FOR NET INCREASE IN EMPLOYEES"
cap lab var c3550 "(-) DEDUCTION FOR PAYMENT TO WORKERS WITH DISABILITIES"
cap lab var c3560 "(=) TAXABLE PROFIT"
cap lab var c3570 "(=) LOSS"

* TAXES
cap lab var c3580 "(-) PROFITS FOR REINVESTMENT AND CAPITALIZATION"
cap lab var c3590 "(=) BALANCE OF TAXABLE INCOME"
cap lab var c3600 "(=) TOTAL TAX INCIDENCE"
cap lab var c3615 "(-) ADVANCE CORRESPONDING TO THE CURRENT FISCAL PERIOD"
cap lab var c3612 "(=) INCOME TAX GREATER THAN THE ONE DETERMINED IN ADVANCE"
cap lab var c3613 "(=) TAX CREDIT ORIGINATED BY ADVANCE (APPLICABLE FOR YEARS BEFORE 2010 ONLY)"
cap lab var c3614 "(+) BALANCE OF ADVANCE PENDING PAYMENT"
cap lab var c3620 "(-) TAX WITHHOLDINGS REALIZED IN THE FISCAL YEAR"
cap lab var c3630 "(-) WITHHOLDINGS FOR ADVANCED DIVIDENDS"
cap lab var c3640 "(-) WITHHOLDINGS FOR INCOME FROM ABROAD WITH THE RIGHT TO A TAX CREDIT"        
cap lab var c3645 "(-) INCOME TAX ADVANCE PAID FOR PUBLIC WORKS"        
cap lab var c3650 "(-) TAX CREDIT FROM PREVIOUS YEARS"
cap lab var c3655 "(-) TAX CREDIT GENERATED FROM TAX ON FOREIGN EXCHANGE OUTFLOWS"
cap lab var c3660 "(-) TAX EXEMPTION AND CREDIT FROM SPECIAL LAWS"
cap lab var c3680 "(=) INCOME TAX TO PAY"
cap lab var c3690 "(=) BALANCE IN FAVOR OF THE TAXPAYER"
cap lab var c3710 "(=) ADVANCE FOR NEXT YEAR"
cap lab var c3692 "( ) ADVANCE TO PAY - FIRST QUOTA""
cap lab var c3694 "( ) ADVANCE TO PAY - SECOND QUOTA""
cap lab var c3696 "( ) ADVANCE TO PAY - BALANCE TO BE SETTLED IN NEXT YEAR'S STATEMENT"



////////////////////////////////////////////////////////////////////////////////
**#                 2 - DEFINE LOCALS WITH ALL COST/REVENUE CELLS
////////////////////////////////////////////////////////////////////////////////

local revenue_F101  c1800+c1810+c1820+c1830+c1840+c1850+c1870+c1880+c1890+c1900+c1920


local costs_F101    c1960 +c1970 +c1980 +c1990 -c2000 +c2010 +c2020 +c2030 ///
                   -c2040 +c2240 -c2250 +c2260 -c2270 +c2280 +c2290 +c2300 ///
                   +c2310 +c2360 +c2370 +c2380 +c2390 +c2400 +c2410 +c2420 ///
                   +c2430 +c2440 +c2450 +c2640 +c2650 +c2670 +c2680 +c2690 ///
                   +c2700 +c2710 +c2720 +c2730 +c2740 +c2750 +c2760 +c2770 ///
                   +c2780 +c2790 +c2810 +c2820 +c2830 +c2840 +c2870 +c2880 ///
                   +c2890 +c2900 +c2920 +c2930 +c2940 +c2950 +c2960 +c2970 ///
                   +c2980 +c2990 +c3000 +c3010 +c3020 +c3030 +c3050 +c3060 ///
                   +c3070 ///
                        +c3080 +c3100 +c3110/*3114   3116*/+c3120 +c3130 +c3140 ///
                        +c3150 +c3160 +c3170/*3171*/+c3180 +c3190 +c3200/*3202*/ ///
                        +c3210/*3212   3214*/+c3220/*3221   3222   3224   3225*/ ///
                       /*3227   3228*/+c3230/*3231   3232   3234   3237   3238*/ ///
                       /*3239*/+c3240 +c3250/*3254   3256   3261   3264   3266*/ ///
                       /*3268*/+c3270/*3271   3272   3274   3275   3277   3278*/ ///
                        +c3280 +c3290 +c3300 +c3310 +c3320 +c3330 +c3340
 

////////////////////////////////////////////////////////////////////////////////
**#                     3 - GENERATE VARIABLES OUT OF CELLS
////////////////////////////////////////////////////////////////////////////////

* Revenue
gen     revenuve = c1930
gen     revenue_calculated = `revenue_F101_08_14' if inlist(year,2008,2009,2010,2011,2012,2013,2014)
replace revenue_calculated = `revenue_F101_15_17' if inlist(year,2015,2016,2017)

* Cost
gen     cost = c3380
gen     cost_calculated = `costs_F101_08_12' if inlist(year,2008,2009,2010,2011,2012)
replace cost_calculated = `costs_F101_13_14' if inlist(year,2013,2014)
replace cost_calculated = `costs_F101_15_17' if inlist(year,2015,2016,2017)

* Profit
gen profit = c3420
gen loss   = c3430
assert c3420==0 if c3430!=0 // check that only one of the two is inputed
assert c3430==0 if c3420!=0 // check that only one of the two is inputed
gen result = profit-loss
gen result_calculated = revenue-cost


/*

*** Cost

        ** Total
        gen      cost = c3380
        gen      cost_calculated = `costs_F101_08_11' if inlist(year,2008,2009,2010,2011)
        replace  cost_calculated = `costs_F101_12'    if inlist(year,2012)
        replace  cost_calculated = `costs_F101_13_14' if inlist(year,2013,2014)
        replace  cost_calculated = `costs_F101_15_17' if inlist(year,2015,2016,2017)

        ** Purchases of domestic good and raw materials
        gen cost_domestic_purchase_goods = c1970
        gen cost_domestic_purchase_raw   = c2020
        //gen cost_domestic_purchase       = cost_domestic_purchase_goods+cost_domestic_purchase_raw
        
        ** Imports of services, goods and raw materials (imports_tf)
        gen cost_imports_goods    = c1980+c1990+c2030        
        gen cost_imports_services = c2400+c2410         
        //gen cost_imports          = imports_goods + imports_services
        
        ** Other production costs
        gen      cost_professional_fee    = c2380+c2390
                        
        gen      cost_rent = c2420+c2430 if inlist(year,2008,2009,2010,2011,2012,2013,2014)
        replace  cost_rent =       c2430 if inlist(year,2015,2016,2017)
        
        gen      cost_maintenance = c2440+c2450
        
        gen      cost_fuel = c2640+c2650
        
        gen      cost_marketing = c2670+c2680 if inlist(year,2008,2009,2010,2011,2012,2013,2014)
        replace  cost_marketing =       c2680 if inlist(year,2015,2016,2017)

        gen      cost_supplies = c2690+c2700
        
        gen      cost_transportation = c2710+c2720 if inlist(year,2008,2009,2010,2011,2012,2013,2014)
        replace  cost_transportation =       c2720 if inlist(year,2015,2016,2017)
        
        gen      cost_commercial_leasing = c2810+c2820+c2830+c2840             if inlist(year,2008,2009,2010,2011,2012,2013,2014)
        replace  cost_commercial_leasing =       c2820      +c2840+c2931+c2933 if inlist(year,2015,2016,2017)
        
        gen      cost_commissions = c2870                                    +c2880            +c2890+c2900 if inlist(year,2008,2009,2010,2011,2012,2013,2014)
        replace  cost_commissions =       c2871+c2872+c2874+c2875+c2877+c2878      +c2881+c2882             if inlist(year,2015,2016,2017)
        
        gen      cost_insurance = c3120+c3130
        
        gen      cost_indirect_thirdparty = c3140+c3150

        gen      cost_administrative = c3160
        
        gen      cost_travel = c3180+c3190 if inlist(year,2008,2009,2010,2011,2012,2013,2014)
        replace  cost_travel =       c3190 if inlist(year,2015,2016,2017)
        
        gen      cost_public_services = c3290+c3300
        
        gen      cost_payment_others =                                                                         c3310+c3320+c3330+c3340 if inlist(year,2008,2009,2010,2011,2012,2013,2014)
        replace  cost_payment_others = c2884+c2886+c2888+c2889+c2892+c2893+c2895+c2896+c2898+c2899+c3212+c3214                         if inlist(year,2015,2016,2017)
        
//         gen      cost_other_production = cost_professional_fee + ///
//                                          cost_rent + ///
//                                          cost_maintenance + ///
//                                          cost_fuel + ///
//                                          cost_marketing + ///
//                                          cost_supplies + ///
//                                          cost_transportation + ///
//                                          cost_commercial_leasing + ///
//                                          cost_commissions + ///
//                                          cost_insurance + ///
//                                          cost_indirect_thirdparty + ///
//                                          cost_administrative + ///
//                                          cost_travel + ///
//                                          cost_public_services + ///
//                                          cost_payment_others + ///
                                         
        ** Financial costs        
        gen      cost_financial        =                                                                                                 c2920+c2930+c2940                     +c2950                           +c2960+c2970+c2980+c2990+c3000             +c3010+c3020+c3030+c3050+c3060+c3070+c3080+c3100+c3110               +c3220                                         +c3230                                           +c3240+c3250                                           +c3270                                     +c3280                                                        if inlist(year,2008,2009,2010,2011,2012)
        replace  cost_financial        =       c2772+c2774+c2776+c2778                                                                  +c2920+c2930+c2940                     +c2950                           +c2960+c2970+c2980+c2990+c3000             +c3010+c3020+c3030+c3050+c3060+c3070+c3080+c3100+c3110+c3114+c3116+c3220                                         +c3230                                           +c3240+c3250+c3254+c3256      +c3264+c3266+c3268+c3270                                     +c3280                                                        if inlist(year,2013,2014)
        replace  cost_financial        = c2701+c2772+c2774            +c2781+c2783+c2785+c2786+c2788+c2789+c2792+c2794+c2797+c2799+c2801      +c2930      +c2942+c2946+c2949+c2950+c2952+c2954+c2957+c2959      +c2970      +c2990      +c3002+c3004+c3010+c3030+c3050+c3060+c3070+c3080                  +c3114+c3116+c3220+c3221+c3222+c3224+c3225+c3227+c3228+c3230+c3231+c3232+c3234+c3237+c3238+c3239+c3240+c3250+c3254+c3256+c3261+c3264                  +c3271+c3272+c3274+c3275+c3277+c3278      +c3281+c3282+c3284+c3285+c3286+c3288+c3289                if inlist(year,2015,2016,2017)


        ** Labor costs
        gen     cost_labor = c2280+c2290+c2300+c2310+c2360+c2370             if inlist(year,2008,2009,2010,2011,2012,2013,2014)
        replace cost_labor = c2280+c2290+c2300+c2310+c2360+c2370+c2752+c2754 if inlist(year,2015,2016,2017)

        //                         *Subitems of labor costs
        //                         generate labor_wages_salaries_tf          = c2280+c2290
        //                         generate labor_social_benefits_tf         = c2300+c2310
        //                         generate labor_contrib_social_security_tf = c2360+c2370

        ** Other costs (inventories, provisions, taxes)

        gen     cost_other = c2730+c2740+c2750+c2760+c2770+c2780+c2790                                                                              +c3170      +c3200      +c3210 if inlist(year,2008,2009,2010,2011,2012)
        replace cost_other = c2730+c2740+c2750+c2760+c2770+c2780+c2790                                                                              +c3170      +c3200      +c3210 if inlist(year,2013,2014)
        replace cost_other = c2730+c2740+c2750+c2760+c2770+c2780+c2790+c2804+c2805+c2807+c2808+c2811+c2813+c2815+c2817+c2819+c2821+c3006+c3008+c3012+c3170+c3171+c3200+c3202+c3210 if inlist(year,2015,2016,2017)

                                        
                *Components of "Other costs" (to test which components was changing)
                
                        *Labor provisions (for retirement)
                        gen     cost_other_labor_provisions = c2730 + c2740                                
                        
                        *Other provisions (rather than labor)
                        gen     cost_other_other_provisions = c2750+c2760+c2770+c2780+c2790                                                             if inlist(year,2008,2009,2010,2011,2012)
                        replace cost_other_other_provisions = c2750+c2760+c2770+c2780+c2790                                                             if inlist(year,2013,2014)
                        replace cost_other_other_provisions = c2750+c2760+c2770+c2780+c2790+c2804+c2805+c2807+c2808+c2811+c2813+c2815+c2817+c2819+c2821 if inlist(year,2015,2016,2017)        
                        
                        *Expenditures to government
                        gen     cost_other_govt = c3170      +c3200      +c3210                if inlist(year,2008,2009,2010,2011,2012,2013,2014)
                        replace cost_other_govt = c3170+c3171+c3200+c3202+c3210                if inlist(year,2015,2016,2017)
                                
                                *Tax expenditures
                                gen     cost_other_govt_taxes        = c3170
                                
        ** Inventories costs
        
                generate cost_inventories        = c1960-c2000 +c2010-c2040 +c2240-c2250 +c2260-c2270                           if inlist(year,2008,2009,2010,2011,2012,2013,2014)
                replace  cost_inventories        = c1960-c2000 +c2010-c2040 +c2240-c2250 +c2260-c2270+c2272+c2274        if inlist(year,2015,2016,2017)        

        ** Assign the discrepancy between "total cost" and the sum of its sub-items to "other costs"
                
                generate cost_disp             = cost - cost_calculated
                replace  cost_other             = cost_other + cost_disp             if cost_disp > 0
                replace  cost_calculated = cost_calculated + cost_disp if cost_disp > 0

                drop cost_disp
                
        ** Check to verify the variables add up to the calculated total costs

                generate cost_calc     = domestic_purchase + imports_tf + cost_other_production + cost_financial + cost_labor + cost_other + cost_inventories
                generate cost_disp     = abs(cost_calc - cost_calculated)
                generate cost_disp_rel = cond(cost_disp==0, 0, abs(cost_calc - cost_calculated) / max(cost_calc, cost_calculated))
                
                qui count if cost_disp_rel > 0.01 & cost_disp > 5
                if (r(N) == 0) {
                        di as result "`form' (cost): calculation correct!"
                }
                else if (r(N) > 0) {
                        di as result "`form' (cost): `r(N)' cases with relative(discrepancy) > 1% & abs(discrepancy) > 5"
                }
                
                drop cost_disp* cost_calc

*** Revenue

        ** Total
                gen revenuve = c1930
                gen      revenue_calculated = `revenue_F101_08_12' if inlist(year,2008,2009,2010,2011,2012)
                replace  revenue_calculated = `revenue_F101_13_14' if inlist(year,2013,2014)
                replace  revenue_calculated = `revenue_F101_15_17' if inlist(year,2015,2016,2017)

        ** Export revenue
                generate exports_tf        = c1820                                if inlist(year,2008,2009,2010,2011,2012,2013,2014)
                replace  exports_tf        = c1820+c1822        if inlist(year,2015,2016,2017)
        
        ** Domestic revenue
                generate revenue_domestic        = c1800      +c1810                  +c1860        if inlist(year,2008,2009,2010,2011,2012,2013,2014)
                replace  revenue_domestic        = c1800+c1802+c1810+c1812+c1826+c1836+c1860 if inlist(year,2015,2016,2017)
        
        ** Financial revenue
                generate revenue_financial = c1840                                                                                                                                                                                                                                                                                             if inlist(year,2008,2009,2010,2011,2012,2013,2014)        
                replace  revenue_financial =       c1843+c1846+c1848+c1851+c1853+c1908+c1911+c5001+c5003+c5005+c5007+c5009+c5011+c5013+c5015+c5017+c5019+c5021+c5023+c5025+c5027+c5029+c5031+c5033+c5035+c5037  if inlist(year,2015,2016,2017)        
        
        ** Dividends revenue
                generate revenue_dividends = c1870
                
        ** Other revenue
                generate revenue_other =       c1830                  +c1850                                                            +c1880                        +c1890                        +c1900                  +c1920                                if inlist(year,2008,2009,2010,2011,2012)
                replace  revenue_other =       c1830                  +c1850                                                            +c1880                        +c1890                        +c1900                                                                          if inlist(year,2013,2014)
                replace  revenue_other = c1821+c1830+c1831+c1838+c1841+c1850+c1856+c1858+c1861+c1863+c1866+c1868+c1871+c1873+c1876+c1878+c1880+c1881+c1883+c1886+c1888+c1890+c1891+c1893+c1896+c1898+c1900+c1901+c1903+c1906      +c5039+c5041  if inlist(year,2015,2016,2017)
                
                *Subitem "Other exempted income"
                generate revenue_other_exempted_income = c1920        if inlist(year,2008,2009,2010,2011,2012)
                replace  revenue_other_exempted_income = 0                        if inlist(year,2013,2014,2015,2016,2017)
                
        ** Assign the discrepancy between "total revenue" and the sum of its sub-items to "other revenue"
                
                generate revenue_disp              = rev - revenue_calculated
                replace  revenue_other      = revenue_other + revenue_disp            if revenue_disp > 0
                replace  revenue_calculated = revenue_calculated + revenue_disp if revenue_disp > 0
                
                drop revenue_disp
                        
        ** Check to verify the variables add up to total revenue
        
                generate revenue_calc     = exports_tf + revenue_domestic + revenue_financial + revenue_dividends + revenue_other
                generate revenue_disp     = abs(revenue_calc - revenue_calculated)
                generate revenue_disp_rel = cond(revenue_disp == 0, 0, abs(revenue_calc - revenue_calculated) / max(revenue_calc, revenue_calculated))
                
                qui count if revenue_disp_rel > 0.01 & revenue_disp > 5
                if (r(N) == 0) {
                        di as result "`form' (revenue): calculation correct!"
                }
                else if (r(N) > 0) {
                        di as result "`form' (revenue): `r(N)' cases with relative(discrepancy) > 1% & abs(discrepancy) > 5"
                }
                
                drop revenue_disp* revenue_calc

*** Profit
        gen profit = c3420
        gen loss   = c3430
        assert c3420==0 if c3430!=0
        assert c3430==0 if c3420!=0
        gen result = profit-loss
        gen result_calculated = revenue-cost
*/

end
