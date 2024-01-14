////////////////////////////////////////////////////////////////////////////////
* File name:        defineF102.ado
* Author:           Miguel Vázquez Vázquez
* Creation date:    20 November 2023
* Description:      This do file labels and generates relevant variables for F102
* Arguments:
*                   -
* Output:
*                   Tax filing positions in form of variables
////////////////////////////////////////////////////////////////////////////////

program define defineF102, rclass
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
/*

*** TRANSACTIONS WITH RELATED PARTIES ABROAD DURING THE FISCAL PERIOD
// No such tax cells for F102


*** BALANCE SHEET - ASSETS
* CURRENT ASSETS
cap lab var c160  "(+) FUNDS, BANKS ASSETS"
cap lab var c170  "(+) CURRENT INVESTMENTS"
cap lab var c180  "(+) ACCOUNTS RECEIVABLE FROM NATURAL PERSONS OR FIRMS - RELATED HOME"
cap lab var c190  "(+) ACCOUNTS RECEIVABLE FROM NATURAL PERSONS OR FIRMS - RELATED ABROAD"
cap lab var c210  "(+) ACCOUNTS RECEIVABLE FROM NATURAL PERSONS OR FIRMS - NOT RELATED HOME"
cap lab var c220  "(+) ACCOUNTS RECEIVABLE FROM NATURAL PERSONS OR FIRMS - NOT RELATED ABROAD" 
cap lab var c230  "(+) OTHER ACCOUNTS RECEIVABLE (e.g. FROM EMPLOYEES) - RELATED HOME" 
cap lab var c240  "(+) OTHER ACCOUNTS RECEIVABLE (e.g. FROM EMPLOYEES) - RELATED ABROAD"
cap lab var c250  "(+) OTHER ACCOUNTS RECEIVABLE (e.g. FROM EMPLOYEES) - NOT RELATED HOME"
cap lab var c260  "(+) OTHER ACCOUNTS RECEIVABLE (e.g. FROM EMPLOYEES) - NOT RELATED ABROAD"
cap lab var c270  "(-) PROVISION FOR UNCOLLECTIBLE ACCOUNTS"
cap lab var c290  "(+) TAX CREDIT IN FAVOR OF THE TAX PAYER (VAT)"
cap lab var c320  "(+) TAX CREDIT IN FAVOR OF THE TAX PAYER (INCOME TAX)"
cap lab var c330  "(+) INVENTORY OF RAW MATERIAL"
cap lab var c340  "(+) INVENTORY OF GOODS IN PROCESS"
cap lab var c350  "(+) INVENTORY OF SUPPLIES AND MATERIALS"
cap lab var c360  "(+) INVENTORY OF FINISHED GOODS AND MERCHANDISE IN THE STORE"
cap lab var c370  "(+) GOODS IN TRANSIT"
cap lab var c380  "(+) INVENTORY OF SPARE PARTS, TOOLS AND ACCESSORIES"
cap lab var c390  "(+) ASSETS PAID IN ADVANCE"
cap lab var c400  "(+) OTHER CURRENT ASSETS"
cap lab var c410  "(=) TOTAL CURRENT ASSETS"                                    // Total

* FIXED ASSETS
cap lab var c420  "(+) PROPERTY (EXCEPT LAND)"
cap lab var c430  "(+) SHIPS, AIRCRAFT, AND BARGES (ETC.)"
cap lab var c440  "(+) FURNITURE/FURNISHINGS"
cap lab var c450  "(+) MACHINERY, EQUIPMENT, AND FACILITIES"
cap lab var c480  "(+) COMPUTER EQUIPMENT AND SOFTWARE"
cap lab var c490  "(+) VEHICLES AND TRANSPORTATION EQUIPMENT"
cap lab var c500  "(+) OTHER FIXED ASSETS"
cap lab var c530  "(-) ACCUMULATED DEPRECIATION OF FIXED ASSETS"
cap lab var c540  "(+) LAND"
cap lab var c550  "(+) NOT FINISHED BUILDINGS"
cap lab var c560  "(=) TOTAL FIXED ASSETS"                                      // Total

* DEFERRED ASSETS (INTANGIBLE)
cap lab var c570  "(+) TRADEMARKS, PATENTS, ETC."
cap lab var c580  "(+) ORGANIZATIONAL COSTS"
cap lab var c600  "(+) RESEARCH AND EXPLORATION COSTS"
cap lab var c620  "(+) OTHER DEFERRED ASSETS"
cap lab var c630  "(-) ACCUMULATED DEPRECIATION"
cap lab var c650  "(=) TOTAL DEFERRED ASSETS"                                   // Total

* LONG TERM ASSETS
cap lab var c670  "(+) LONG TERM INVESTMENTS - SHARES AND OTHER EQUITY"
cap lab var c680  "(+) LONG TERM INVESTMENTS - OTHER"
cap lab var c700  "(+) LONG TERM ACCOUNTS RECEIVABLE FROM NATURAL FIRMS OR PERSONS - RELATED HOME"
cap lab var c710  "(+) LONG TERM ACCOUNTS RECEIVABLE FROM NATURAL FIRMS OR PERSONS - RELATED ABROAD"
cap lab var c720  "(+) LONG TERM ACCOUNTS RECEIVABLE FROM NATURAL FIRMS OR PERSONS - NOT RELATED HOME"
cap lab var c740  "(+) LONG TERM ACCOUNTS RECEIVABLE FROM NATURAL FIRMS OR PERSONS - NOT RELATED ABROAD"
cap lab var c750  "(+) OTHER LONG TERM ACCOUNTS RECEIVABLE - RELATED HOME"
cap lab var c760  "(+) OTHER LONG TERM ACCOUNTS RECEIVABLE - RELATED ABROAD"
cap lab var c770  "(+) OTHER LONG TERM ACCOUNTS RECEIVABLE - NOT RELATED HOME"
cap lab var c780  "(+) OTHER LONG TERM ACCOUNTS RECEIVABLE - NOT RELATED ABROAD"
cap lab var c790  "(-) PROVISION FOR UNCOLLECTIBLE ACCOUNTS"
cap lab var c800  "(+) OTHER LONG TERM ASSETS"
cap lab var c810  "(=) TOTAL LONG TERM ASSETS"                                  // Total

* TOTAL ASSETS
// NO             "( ) ASSETS COMING FROM REINVESTMENT OF PROFITS"              // no profit reinvestment
cap lab var c830  "(=) TOTAL ASSETS" // (c410+c560+c650+c810)


*** BALANCE SHEET - LIABILITIES & EQUITIES
* CURRENT LIABILITIES
cap lab var c850  "(+) ACCOUNTS AND NOTES PAYABLE TO CURRENT SUPPLIERS - RELATED HOME"
cap lab var c860  "(+) ACCOUNTS AND NOTES PAYABLE TO CURRENT SUPPLIERS - RELATED ABROAD"
cap lab var c870  "(+) ACCOUNTS AND NOTES PAYABLE TO CURRENT SUPPLIERS - NOT RELATED HOME"
cap lab var c880  "(+) ACCOUNTS AND NOTES PAYABLE TO CURRENT SUPPLIERS - NOT RELATED ABROAD"
cap lab var c890  "(+) CURRENT OBLIGATIONS WITH FINANCIAL INSTITUTIONS - HOME"
cap lab var c900  "(+) CURRENT OBLIGATIONS WITH FINANCIAL INSTITUTIONS - ABROAD"  
// NO             "(+) CURRENT LOANS FROM SHAREHOLDERS - HOME"                  // no shareholders
// NO             "(+) CURRENT LOANS FROM SHAREHOLDERS - ABROAD"                // no shareholders
cap lab var c960  "(+) OTHER CURRENT ACCOUNTS AND NOTES PAYABLE - RELATED HOME"
cap lab var c970  "(+) OTHER CURRENT ACCOUNTS AND NOTES PAYABLE - RELATED ABROAD"
cap lab var c980  "(+) OTHER CURRENT ACCOUNTS AND NOTES PAYABLE - NOT RELATED HOME"
cap lab var c990  "(+) OTHER CURRENT ACCOUNTS AND NOTES PAYABLE - NOT RELATED ABROAD"
// NO             "(+) INCOME TAX TO BE PAID IN FISCAL PERIOD"
// NO             "(+) WORKERS' PROFITS TO BE PAID IN FISCAL PERIOD"
cap lab var c1000 "(+) TRANSFERS TO THE HEAD OFFICE AND BRANCHES (OVERSEAS)"
cap lab var c1010 "(+) CURRENT CREDITO A MUTUO"
// NO             "(+) SHORT TERM OBLIGATIONS ISSUED"                           // no obbligation issuing possible
cap lab var c1020 "(+) OTHER PROVISIONS"
cap lab var c1030 "(=) TOTAL CURRENT LIABILITIES"                               // Total

* LONG TERM LIABILITIES
cap lab var c1080 "(+) LONG TERM ACCOUNTS AND NOTES PAYABLE TO SUPPLIERS - RELATED HOME"
cap lab var c1090 "(+) LONG TERM ACCOUNTS AND NOTES PAYABLE TO SUPPLIERS - RELATED ABROAD"
cap lab var c1100 "(+) LONG TERM ACCOUNTS AND NOTES PAYABLE TO SUPPLIERS - NOT RELATED HOME"
cap lab var c1110 "(+) LONG TERM ACCOUNTS AND NOTES PAYABLE TO SUPPLIERS - NOT RELATED ABROAD"
cap lab var c1120 "(+) LONG TERM OBLIGATIONS TO FINANCIAL INSTITUTIONS - HOME"
cap lab var c1130 "(+) LONG TERM OBLIGATIONS TO FINANCIAL INSTITUTIONS - ABROAD"
// NO             "(+) LONG TERM LOANS FROM SHAREHOLDERS - HOME"                // no shareholders
// NO             "(+) LONG TERM LOANS FROM SHAREHOLDERS - ABROAD"              // no shareholders
cap lab var c1160 "(+) OTHER LONG TERM ACCOUNTS AND NOTES PAYABLE - RELATED HOME"
cap lab var c1170 "(+) OTHER LONG TERM ACCOUNTS AND NOTES PAYABLE - RELATED ABROAD"
cap lab var c1180 "(+) OTHER LONG TERM ACCOUNTS AND NOTES PAYABLE - NOT RELATED HOME"
cap lab var c1190 "(+) OTHER LONG TERM ACCOUNTS AND NOTES PAYABLE - NOT RELATED ABROAD"
cap lab var c1200 "(+) LONG TERM TRANSFERS TO THE HEAD OFFICE AND BRANCHES (OVERSEAS)"
cap lab var c1210 "(+) LONG TERM CREDITO A MUTUO"
// NO             "(+) LONG TERM OBLIGATIONS ISSUED"                            // no obbligation issuing possible
cap lab var c1220 "(+) PROVISIONS FOR RETIREMENT"
cap lab var c1230 "(+) PROVISIONS FOR LAYOFFS"
cap lab var c1240 "(+) OTHER PROVISIONS (WITH TERMS >1 YEAR)"
cap lab var c1250 "(=) TOTAL LONG TERM LIABILITIES"                             // Total

* TOTAL LIABILITIES
cap lab var c1260 "(+) DEFFERED LIABILITIES"                                    // "Total"
cap lab var c1270 "(+) OTHER LIABILITIES"                                       // "Total"
cap lab var c1310 "(=) TOTAL LIABILITIES" // (c1030+c1250+c1260+c1270)          // Liability Total

* EQUITIES
// No such tax cells for F102

* TOTAL EQUITIES
cap lab var c1330 "(=) TOTAL EQUITY"                                            // "Total"

* TOTAL LIABILITIES
cap lab var c1340 "(=) TOTAL LIABILITIES AND EQUITY" // (c1310+c1330)


*** INCOME STATEMENT - INCOME
cap lab var c1350 "(+) NET DOMESTIC SALES SUBJECT TO 12% TAX RATE"
cap lab var c1360 "(+) NET DOMESTIC SALES SUBJECT TO 0% TAX RATE"
cap lab var c1370 "(+) NET EXPORTS"
cap lab var c1380 "(+) OTHER INCOME FROM ABROAD"
cap lab var c1390 "(+) FINANCIAL RENTS"
cap lab var c1400 "(+) OTHER TAXABLE INCOME"
cap lab var c1410 "(+) GAIN ON SALE OF FIXED ASSETS"
cap lab var c1420 "(+) DOMESTIC DIVIDENDS RECEIVED"
cap lab var c1411 "(+) EXEMPTED INCOME FROM DONATIONS AND CONTRIBUTIONS - FROM PUBLIC RESOURCES"         // later forms only
cap lab var c1413 "(+) EXEMPTED INCOME FROM DONATIONS AND CONTRIBUTIONS - FROM OTHER ECUADORIAN SOURCES" // later forms only
cap lab var c1416 "(+) EXEMPTED INCOME FROM DONATIONS AND CONTRIBUTIONS - FROM ABROAD"                   // later forms only
// NO             "(+) EXEMPTED INCOME FROM DONATIONS AND CONTRIBUTIONS" // (c1411+c1413+c1416), but not existent
cap lab var c1430 "(+) OTHER EXEMPTED INCOME"
cap lab var c1440 "(=) TOTAL INCOME"                                            // Total

cap lab var c1450 "( ) NET SALES OF FIXED ASSETS"
cap lab var c1460 "( ) REIMBURSEMENT RECEIVED AS INTERMEDIARY"


*** INCOME STATEMENT - COSTS & EXPENSES
cap lab var c1470 "(+) INITIAL INVENTORY OF GOODS NOT PROUDCED BY THE FIRM - PRODUCTION COSTS"
cap lab var c1480 "(+) NET PURCHASES OF DOMESTIC GOODS NOT PRODUCED BY THE FIRM - PRODUCTION COSTS"
cap lab var c1490 "(+) IMPORTS NOT PRODUCED BY THE FIRM - PRODUCTION COSTS"
cap lab var c1500 "(+) IMPORTS NOT PRODUCED BY THE FIRM - ADMIN EXPENSES"
cap lab var c1520 "(-) FINAL INVENTORY OF GOOD NOT PRODUCED BY THE FIRM - PRODUCTION COSTS"
cap lab var c1530 "(+) INITIAL INVENTORY OF RAW MATERIAL - PRODUCTION COSTS"
cap lab var c1540 "(+) NET DOMESTIC PURCHES OF RAW MATERIAL - PRODUCTION COSTS"
cap lab var c1550 "(+) IMPORTS OF RAW MATERIAL - PRODUCTION COSTS"
cap lab var c1570 "(-) FINAL INVENTORY OF RAW MATERIAL - PRODUCTION COSTS"
cap lab var c1580 "(+) INITIAL INVENTORY OF PRODUCTS IN PROCESS - PRODUCTION COSTS"
cap lab var c1590 "(-) FINAL INVENTORY OF PRODUCTS IN PROCESS - PRODUCTION COSTS"
cap lab var c1600 "(+) INITIAL INVENTORY OF FINISHED PRODUCTS - PRODUCTION COSTS"
cap lab var c1610 "(-) FINAL INVENTORY OF FINISHED PRODUCTS - PRODUCTION COSTS"

cap lab var c1620 "(+) WAGES, SALARIES AND OTHER TAXABLE REMUNERATIONS - PRODUCTION COSTS"
cap lab var c1630 "(+) WAGES, SALARIES AND OTHER TAXABLE REMUNERATIONS - ADMIN EXPENSES"
cap lab var c1650 "(+) SOCIAL BENEFITS AND OTHER NON-TAXABLE COMPENSATION - PRODUCTION COSTS"
cap lab var c1660 "(+) SOCIAL BENEFITS AND OTHER NON-TAXABLE COMPENSATION - ADMIN EXPENSES"
cap lab var c1670 "(+) CONTRIBUTION TO SOCIAL SECURITY (INCLUDING RESERVE FUND) - PRODUCTION COSTS"
cap lab var c1680 "(+) CONTRIBUTION TO SOCIAL SECURITY (INCLUDING RESERVE FUND) - ADMIN EXPENSES"
cap lab var c1700 "(+) PROFESSIONAL FEES AND EXPENSES - PRODUCTION COSTS"
cap lab var c1710 "(+) PROFESSIONAL FEES AND EXPENSES - ADMIN EXPENSES"
cap lab var c1730 "(+) FEES TO FOREIGNERS FOR ONE-TIME EXPENSES - PRODUCTION COSTS"
cap lab var c1740 "(+) FEES TO FOREIGNERS FOR ONE-TIME EXPENSES - ADMIN EXPENSES"
cap lab var c1750 "(+) REAL ESTATE RENT - PRODUCTION COSTS"
cap lab var c1760 "(+) REAL ESTATE RENT - ADMIN EXPENSES"
cap lab var c1790 "(+) MAINTENANCE AND REPAIRS - PRODUCTION COSTS"
cap lab var c1800 "(+) MAINTENANCE AND REPAIRS - ADMIN EXPENSES"
cap lab var c1810 "(+) FUEL - PRODUCTION COSTS"
cap lab var c1820 "(+) FUEL - ADMIN EXPENSES"
cap lab var c1840 "(+) MARKETING - PRODUCTION COSTS"
cap lab var c1850 "(+) MARKETING - ADMIN EXPENSES"
cap lab var c1860 "(+) SUPPLIES AND MATERIALS - PRODUCTION COSTS"
cap lab var c1870 "(+) SUPPLIES AND MATERIALS - ADMIN EXPENSES"
cap lab var c1880 "(+) TRANSPORTATION - PRODUCTION COSTS"
cap lab var c1890 "(+) TRANSPORTATION - ADMIN EXPENSES"

cap lab var c1900 "(+) PROVISIONS - FOR RETIREMENT - PRODUCTION COSTS"
cap lab var c1910 "(+) PROVISIONS - FOR RETIREMENT - ADMIN EXPENSES"
cap lab var c1930 "(+) PROVISIONS - FOR EVICTION - PRODUCTION COSTS"
cap lab var c1940 "(+) PROVISIONS - FOR EVICTION - ADMIN EXPENSES"
cap lab var C1980 "(+) PROVISIONS - FOR UNCOLLECTIBLE ACCOUNTS - ADMIN EXPENSES"
cap lab var c2000 "(+) PROVISIONS - OTHER PROVISIONS - PRODUCTION COSTS"
cap lab var c2010 "(+) PROVISIONS - OTHER PROVISIONS - ADMIN EXPENSES"
cap lab var c2030 "(+) COMMERCIAL LEASING - HOME - PRODUCTION COSTS"
cap lab var c2040 "(+) COMMERCIAL LEASING - HOME - ADMIN EXPENSES"
cap lab var c2050 "(+) COMMERCIAL LEASING - ABROAD - PRODUCTION COSTS"
cap lab var c2060 "(+) COMMERCIAL LEASING - ABROAD - ADMIN EXPENSES"
cap lab var c2070 "(+) COMMISSIONS - HOME - PRODUCTION COSTS"
cap lab var c2080 "(+) COMMISSIONS - HOME - ADMIN EXPENSES"
cap lab var c2090 "(+) COMMISSIONS - ABROAD - PRODUCTION COSTS"
cap lab var c2100 "(+) COMMISSIONS - ABROAD - ADMIN EXPENSES"
cap lab var c2120 "(+) BANK INTEREST - HOME - PRODUCTION COSTS"
cap lab var c2130 "(+) BANK INTEREST - HOME - ADMIN EXPENSES"
cap lab var c2140 "(+) BANK INTEREST - ABROAD - PRODUCTION COSTS"
cap lab var c2150 "(+) BANK INTEREST - ABROAD - ADMIN EXPENSES"
cap lab var c2230 "(+) INTERESES PAGADOS A TERCEROS - RELATED HOME - PRODUCTION COSTS"
cap lab var c2240 "(+) INTERESES PAGADOS A TERCEROS - RELATED HOME - ADMIN EXPENSES"
cap lab var c2250 "(+) INTERESES PAGADOS A TERCEROS - RELATED ABROAD - PRODUCTION COSTS"
cap lab var c2260 "(+) INTERESES PAGADOS A TERCEROS - RELATED ABROAD - ADMIN EXPENSES"
cap lab var c2270 "(+) INTERESES PAGADOS A TERCEROS - NOT RELATED HOME - PRODUCTION COSTS"
cap lab var c2280 "(+) INTERESES PAGADOS A TERCEROS - NOT RELATED HOME - ADMIN EXPENSES"
cap lab var c2290 "(+) INTERESES PAGADOS A TERCEROS - NOT RELATED ABROAD - PRODUCTION COSTS"
cap lab var c2300 "(+) INTERESES PAGADOS A TERCEROS - NOT RELATED ABROAD - ADMIN EXPENSES"
cap lab var c2320 "(+) LOSS FROM SALE OF ASSETS - RELATED - PRODUCTION COSTS"
cap lab var c2330 "(+) LOSS FROM SALE OF ASSETS - RELATED - ADMIN EXPENSES"
cap lab var c2340 "(+) LOSS FROM SALE OF ASSETS - NOT RELATED - PRODUCTION COSTS"
cap lab var c2350 "(+) LOSS FROM SALE OF ASSETS - NOT RELATED - ADMIN EXPENSES"

cap lab var c2360 "(+) OTHER LOSSES - PRODUCTION COSTS"
cap lab var c2370 "(+) OTHER LOSSES - ADMIN EXPENSES"
cap lab var c2380 "(+) INSURANCE AND REINSURANCE INTERMEDIARIES - PRODUCTION COSTS"
cap lab var c2390 "(+) INSURANCE AND REINSURANCE INTERMEDIARIES - ADMIN EXPENSES"
cap lab var c2400 "(+) INDIRECT COSTS INCURRED FROM ABROAD BY RELATED PARTIES - PRODUCTION COSTS"
cap lab var c2410 "(+) INDIRECT COSTS INCURRED FROM ABROAD BY RELATED PARTIES - ADMIN EXPENSES"
cap lab var c2420 "(+) ADMINISTRATIVE COSTS - ADMIN EXPENSES"
cap lab var c2430 "(+) TAXES, CONTRIBUTIONS AND OTHER - ADMIN EXPENSES"
cap lab var c2440 "(+) TRAVEL EXPENSES - PRODUCTION COSTS"
cap lab var c2450 "(+) TRAVEL EXPENSES - ADMIN EXPENSES"
cap lab var c2470 "(+) VAT INCURRED ON COSTS OR EXPENSES - PRODUCTION COSTS"
cap lab var c2480 "(+) VAT INCURRED ON COSTS OR EXPENSES - ADMIN EXPENSES"
cap lab var c2510 "(+) DEPRECIATION OF FIXED ASSETS - ACCELERATED - PRODUCTION COSTS"
cap lab var c2520 "(+) DEPRECIATION OF FIXED ASSETS - ACCELERATED - ADMIN EXPENSES"
cap lab var c2550 "(+) DEPRECIATION OF FIXED ASSETS - NON ACCELERATED - PRODUCTION COSTS"
cap lab var c2560 "(+) DEPRECIATION OF FIXED ASSETS - NON ACCELERATED - ADMIN EXPENSES"
cap lab var c2570 "(+) AMORTIZATION - PRODUCTION COSTS"
cap lab var c2580 "(+) AMORTIZATION - ADMIN EXPENSES"
cap lab var c2600 "(+) PUBLIC SERVICES - PRODUCTION COSTS"
cap lab var c2610 "(+) PUBLIC SERVICES - ADMIN EXPENSES"
cap lab var c2620 "(+) PAYMENT FOR OTHER SERVICES - PRODUCTION COSTS"
cap lab var c2630 "(+) PAYMENT FOR OTHER SERVICES - ADMIN EXPENSES"
cap lab var c2640 "(+) PAYMENT FOR OTHER GOODS - PRODUCTION COSTS"
cap lab var c2650 "(+) PAYMENT FOR OTHER GOODS - ADMIN EXPENSES"

cap lab var c2660 "(=) TOTAL COSTS"                                             // Total - PRODUCTION COSTS
cap lab var c2750 "(=) TOTAL EXPENDITURES"                                      // Total - ADMIN EXPENSES
cap lab var c2760 "(=) TOTAL COSTS AND EXPENDITURES" // (c2660+c2750)

cap lab var c2770 "( ) LOSS OF INVENTORY"
cap lab var c2780 "( ) PAYMENT FOR REIMBURSEMENT PAID AS REIMBURSER"
cap lab var c2790 "( ) PAYMENT FOR REIMBURSEMENT PAID AS INTERMEDIARY"


*** TAX BALANCE
* PROFITS/LOSSES
cap lab var c2800 "(=) PROFIT THIS PERIOD"
cap lab var c2810 "(=) LOSS THIS PERIOD"
cap lab var c2820 "(-) 15% EMPLOYEES' SHARE"
cap lab var c2830 "(-) 100% EXEMPT DIVIDENDS RECEIVED"
cap lab var c2840 "(-) 100% OTHER EXEMPT RENTS"
cap lab var c2850 "(+) NON-DEDUCTIBLE DOMESTIC EXPENSES"
cap lab var c2860 "(+) NON-DEDUCTIBLE EXPENSES FROM ABROAD"
cap lab var c2870 "(+) EXPENSES TO GENERATE EXEMPT INCOME"
cap lab var c2880 "(+) EMPLOYEES' SHARE ATTRIBUTABLE TO EXEMPT INCOME"            
cap lab var c2910 "(-) AMORTIZATION OF LOSSES FROM PREVIOUS YEARS"
cap lab var c2930 "(-) DEDUCTIONS FOR SPECIAL LAWS"
cap lab var c2940 "(+) ADJUSTMENT FOR TRANSFER PRICING"
cap lab var c2950 "(-) DEDUCTION FOR NET INCREASE IN EMPLOYEES"
cap lab var c2960 "(-) DEDUCTION FOR PAYMENT TO WORKERS WITH DISABILITIES"
cap lab var c2970 "(=) TAXABLE PROFIT"
cap lab var c2980 "(=) LOSS"

* TAXES
// NO             "(-) PROFITS FOR REINVESTMENT AND CAPITALIZATION"
cap lab var c3480 "(=) BALANCE OF TAXABLE INCOME"
cap lab var c3490 "(=) TOTAL TAX INCIDENCE"
cap lab var c3501 "(-) ADVANCE CORRESPONDING TO THE CURRENT FISCAL PERIOD"
cap lab var c3502 "(=) INCOME TAX GREATER THAN THE ONE DETERMINED IN ADVANCE"
cap lab var c3504 "(=) TAX CREDIT ORIGINATED BY ADVANCE (APPLICABLE FOR YEARS BEFORE 2010 ONLY)"
cap lab var c3506 "(+) BALANCE OF ADVANCE PENDING PAYMENT"
cap lab var c3510 "(-) TAX WITHHOLDINGS REALIZED IN THE FISCAL YEAR"
cap lab var c3525 "(-) WITHHOLDINGS FOR ADVANCED DIVIDENDS"
cap lab var c3530 "(-) WITHHOLDINGS FOR INCOME FROM ABROAD WITH THE RIGHT TO A TAX CREDIT"        
cap lab var c3535 "(-) INCOME TAX ADVANCE PAID FOR PUBLIC WORKS"        
cap lab var c3550 "(-) TAX CREDIT FROM PREVIOUS YEARS"
cap lab var c3555 "(-) TAX CREDIT GENERATED FROM TAX ON FOREIGN EXCHANGE OUTFLOWS"
cap lab var c3560 "(-) TAX EXEMPTION AND CREDIT FROM SPECIAL LAWS"
cap lab var c3570 "(=) INCOME TAX TO PAY"
cap lab var c3580 "(=) BALANCE IN FAVOR OF THE TAXPAYER"
cap lab var c3590 "(=) ADVANCE FOR NEXT YEAR"
cap lab var c3592 "( ) ADVANCE TO PAY - FIRST QUOTA""
cap lab var c3594 "( ) ADVANCE TO PAY - SECOND QUOTA""
cap lab var c3596 "( ) ADVANCE TO PAY - BALANCE TO BE SETTLED IN NEXT YEAR'S STATEMENT"

* PERSONAL (STILL TO DO)
cap lab var c2990 "(+) RECEIVED PROFESSIONAL FEEDS"
cap lab var c3010 "(-) RECEIVED PROFESSIONAL FEEDS"
cap lab var c3010 "(+) RECEIVED FEEDS (NON PROFESSIONAL ACTIVITIES)"
cap lab var c3020 "(-) RECEIVED FEEDS (NON PROFESSIONAL ACTIVITIES)"
cap lab var c3040 "(+) RECEIVED REAL STATE RENT"
cap lab var c3050 "(-) RECEIVED REAL STATE RENT"
cap lab var c3080 "(+) RECEIVED OTHERS ASSERTS RENT"
cap lab var c3090 "(-) RECEIVED OTHERS ASSERTS RENT"
cap lab var c3160 "INCOME FROM EGRACULTURA"
cap lab var c3170 "INCOME FROM ROYALTIES"
cap lab var c3180 "OTHER INCOME FROM ABROAD"
cap lab var c3190 "(+) FINANCIAL RENTS"
cap lab var c3192 "(+) DIVIDENDS"
cap lab var c3193 "(+) OTHER TAXABLE INCOME"
cap lab var c3194 "(-) OTHER TAXABLE INCOME"

*/

////////////////////////////////////////////////////////////////////////////////
**#                 2 - DEFINE LOCALS WITH ALL COST/REVENUE CELLS
////////////////////////////////////////////////////////////////////////////////


* Revenue
local revenue_F102_08_11     c1350 +c1360 +c1370/*1371   1373*/+c1380 +c1390 ///
                            +c1400 +c1410/*1411   1413   1416*/+c1420/*1425*/ ///
                            +c1430 +c2990 +c3010 +c3040 +c3080 +c3160 +c3170 ///
                            +c3180 +c3190 +c3192 +c3193

local revenue_F102_12        c1350 +c1360 +c1370/*1371   1373*/+c1380 +c1390 ///
                            +c1400 +c1410/*1411   1413   1416   1420*/+c1425 ///
                            +c1430 +c2990 +c3010 +c3040 +c3080 +c3160 +c3170 ///
                            +c3180 +c3190 +c3192 +c3193

local revenue_F102_13        c1350 +c1360 +c1370/*1371   1373*/+c1380 +c1390 ///
                            +c1400 +c1410/*1411   1413   1416   1420*/+c1425 ///
                           /*1430*/+c2990 +c3010 +c3040 +c3080 +c3160 +c3170 ///
                            +c3180 +c3190 +c3192 +c3193

local revenue_F102_14_17     c1350 +c1360 +c1370 +c1371 +c1373 +c1380 +c1390 ///
                            +c1400 +c1410 +c1411 +c1413 +c1416/*1420*/+c1425 ///
                           /*1430*/+c2990 +c3010 +c3040 +c3080 +c3160 +c3170 ///
                            +c3180 +c3190 +c3192 +c3193

* Cost
local costs_F102_08_11   c1470 +c1480 +c1490 +c1500 -c1520 +c1530 +c1540 ///
                        +c1550 -c1570 +c1580 -c1590 +c1600 -c1610 +c1620 ///
                        +c1630 +c1650 +c1660 +c1670 +c1680 +c1700 +c1710 ///
                        +c1730 +c1740 +c1750 +c1760 +c1790 +c1800 +c1810 ///
                        +c1820 +c1840 +c1850 +c1860 +c1870 +c1880 +c1890 ///
                        +c1900 +c1910 +c1930 +c1940 +c1980 +c2000 +c2010 ///
                        +c2030 +c2040 +c2050 +c2060 +c2070 +c2080 +c2090 ///
                        +c2100 +c2120 +c2130 +c2140 +c2150 +c2230 +c2240 ///
                        +c2250 +c2260 +c2270 +c2280 +c2290 +c2300 +c2320 ///
                        +c2330 +c2340 +c2350 +c2360 +c2370 +c2380 +c2390 ///
                        +c2400 +c2410 +c2420 +c2430 +c2440 +c2450 +c2470 ///
                        +c2480 +c2510 +c2520 +c2550 +c2560 +c2570 +c2580 ///
                        +c2600 +c2610 +c2620 +c2630 +c2640 +c2650 +c3000 ///
                        +c3020 +c3050 +c3090 +c3194 +c3250
                        
local costs_F102_12      c1470 +c1480 +c1490 +c1500 -c1520 +c1530 +c1540 ///
                        +c1550 -c1570 +c1580 -c1590 +c1600 -c1610 +c1620 ///
                        +c1630 +c1650 +c1660 +c1670 +c1680 +c1700 +c1710 ///
                        +c1730 +c1740 +c1750 +c1760 +c1790 +c1800 +c1810 ///
                        +c1820 +c1840 +c1850 +c1860 +c1870 +c1880 +c1890 ///
                        +c1900 +c1910 +c1930 +c1940 +c1980 +c2000 +c2010 ///
                        +c2030 +c2040 +c2050 +c2060 +c2070 +c2080 +c2090 ///
                        +c2100 +c2120 +c2130 +c2140 +c2150 +c2230 +c2240 ///
                        +c2250 +c2260/*2270*/+c2280 +c2290 +c2300 +c2320 ///
                        +c2330 +c2340 +c2350 +c2360 +c2370 +c2380 +c2390 ///
                        +c2400 +c2410 +c2420 +c2430 +c2440 +c2450 +c2470 ///
                        +c2480 +c2510 +c2520 +c2550 +c2560 +c2570 +c2580 ///
                        +c2600 +c2610 +c2620 +c2630 +c2640 +c2650 +c3000 ///
                        +c3020 +c3050 +c3090 +c3162 +c3194 +c3250
                        
local costs_F102_13     `costs_F102_12' +c2374 +c2376 +c2564 +c2566                                  

local costs_F102_14_17  `costs_F102_13' +c1482 +c1981 +c1982 +c1991 +c1992 +c2572 +c2576 +c2582

////////////////////////////////////////////////////////////////////////////////
**#                     3 - GENERATE VARIABLES OUT OF CELLS
////////////////////////////////////////////////////////////////////////////////

* Revenue
gen     revenuve = c1440
gen     revenue_calculated = `revenue_F102_08_11' if inlist(year,2008,2009,2010,2011)
replace revenue_calculated = `revenue_F102_12'    if inlist(year,2012)
replace revenue_calculated = `revenue_F102_13'    if inlist(year,2013)
replace revenue_calculated = `revenue_F102_14_17' if inlist(year,2014,2015,2016,2017)

* Cost
gen     cost = c2760
gen     cost_calculated = `costs_F102_08_11' if inlist(year,2008,2009,2010,2011)
replace cost_calculated = `costs_F102_12'    if inlist(year,2012)
replace cost_calculated = `costs_F102_13'    if inlist(year,2013)
replace cost_calculated = `costs_F102_14_17' if inlist(year,2014,2015,2016,2017)

* Profit
gen profit = c2800
gen loss   = c2810
assert c2800==0 if c2810!=0 // check that only one of the two is inputed
assert c2810==0 if c2800!=0 // check that only one of the two is inputed
gen result = profit-loss
gen result_calculated = revenue-cost


/*

*** Costs

        ** Total costs
        generate cost = c2760                                 
        generate cost_calculated = `costs_F102_08_11' if inlist(year,2008,2009,2010,2011)
        replace  cost_calculated = `costs_F102_12'    if inlist(year,2012)
        replace  cost_calculated = `costs_F102_13'    if inlist(year,2013)
        replace  cost_calculated = `costs_F102_14_17' if inlist(year,2014,2015,2016,2017)

        ** Purchases of domestic good and raw materials
        
                generate domestic_purchase = c1480             +c1540        if inlist(year,2008,2009,2010,2011,2012,2013)
                replace  domestic_purchase = c1480+c1482+c1540        if inlist(year,2014,2015,2016,2017)
                
                        *Subitems of other production costs
                        generate domestic_purchase_goods = c1480                          if inlist(year,2008,2009,2010,2011,2012,2013)
                        replace  domestic_purchase_goods = c1480+c1482        if inlist(year,2014,2015,2016,2017)
                        
                        generate domestic_purchase_raw = c1540
        
        ** Imports of services, goods and raw materials (imports_tf)
                
                generate imports_goods_tf                 = c1490+c1500+c1550        
                generate imports_services_tf = c1730+c1740         
                generate imports_tf                             = imports_goods_tf + imports_services_tf        
                
        ** Other production costs
                        
                generate cost_other_production = c1700+c1710+c1750+c1760+c1790+c1800+c1810+c1820+c1840+c1850+c1860+c1870+c1880+c1890+c2030+c2040+c2050+c2060+c2070+c2080+c2090+c2100+c2380+c2390+c2400+c2410+c2420+c2430+c2440+c2450+c2600+c2610+c2620+c2630+c2640+c2650+c3000+c3020+c3050+c3090                     if inlist(year,2008,2009,2010,2011)
                replace  cost_other_production = c1700+c1710+c1750+c1760+c1790+c1800+c1810+c1820+c1840+c1850+c1860+c1870+c1880+c1890+c2030+c2040+c2050+c2060+c2070+c2080+c2090+c2100+c2380+c2390+c2400+c2410+c2420+c2430+c2440+c2450+c2600+c2610+c2620+c2630+c2640+c2650+c3000+c3020+c3050+c3090+c3162         if inlist(year,2012,2013,2014,2015,2016,2017)
                
                        *Subitems of other production costs
                        generate cost_professional_fee          = c1700+c1710
                        generate cost_maintenance               = c1790+c1800
                        generate cost_fuel                                  = c1810+c1820             
                        generate cost_marketing                              = c1840+c1850                  
                        generate cost_supplies                               = c1860+c1870                  
                        generate cost_transportation                  = c1880+c1890                  
                        generate cost_administrative                  = c2420
                        generate cost_travel                                = c2440+c2450         
                        generate cost_rent                            = c1750+c1760                                                        
                        generate cost_commercial_leasing         = c2030+c2040+c2050+c2060                
                        generate cost_commissions                      = c2070+c2080+c2090+c2100        
                        generate cost_insurance                              = c2380+c2390 
                        generate cost_public_services                  = c2600+c2610
                        generate cost_payment_others                  = c2620+c2630+c2640+c2650
                        generate cost_indirect_thirdparty = c2400+c2410
                        
                        generate cost_other_production_personal = c3000+c3020+c3050+c3090                     if inlist(year,2008,2009,2010,2011)
                        replace  cost_other_production_personal = c3000+c3020+c3050+c3090+c3162         if inlist(year,2012,2013,2014,2015,2016,2017)

        ** Financial costs
                
                generate cost_financial        = c2120+c2130+c2140+c2150+c2230+c2240+c2250+c2260+c2270+c2280+c2290+c2300+c2320+c2330+c2340+c2350+c2360+c2370            +c2510+c2520+c2550+c2560            +c2570            +c2580                    if inlist(year,2008,2009,2010,2011)
                replace  cost_financial        = c2120+c2130+c2140+c2150+c2230+c2240+c2250+c2260      +c2280+c2290+c2300+c2320+c2330+c2340+c2350+c2360+c2370            +c2510+c2520+c2550+c2560            +c2570            +c2580                    if inlist(year,2012)
                replace  cost_financial        = c2120+c2130+c2140+c2150+c2230+c2240+c2250+c2260      +c2280+c2290+c2300+c2320+c2330+c2340+c2350+c2360+c2370+c2374+c2376+c2510+c2520+c2550+c2560+c2564+c2566+c2570            +c2580                    if inlist(year,2013)
                replace  cost_financial        = c2120+c2130+c2140+c2150+c2230+c2240+c2250+c2260      +c2280+c2290+c2300+c2320+c2330+c2340+c2350+c2360+c2370+c2374+c2376+c2510+c2520+c2550+c2560+c2564+c2566+c2570+c2572+c2576+c2580+c2582        if inlist(year,2014,2015,2016,2017)
                                
        ** Labor costs
                
                *Labor costs defined in data_construction
                generate cost_labor        = c1620+c1630+c1650+c1660+c1670+c1680+c3250
                
                        *Subitems of labor costs
                        generate labor_wages_salaries_tf          = c1620+c1630
                        generate labor_social_benefits_tf         = c1650+c1660
                        generate labor_contrib_social_security_tf = c1670+c1680
                        generate cost_labor_personal                                = c3250
                
        ** Other costs (inventories, provisions, taxes)
        
                generate cost_other        = c1900+c1910+c1930+c1940+c1980                        +c2000+c2010+c2470+c2480+c3194 if inlist(year,2008,2009,2010,2011,2012)                
                replace  cost_other        = c1900+c1910+c1930+c1940+c1980+c1981+c1982+c1991+c1992+c2000+c2010+c2470+c2480+c3194 if inlist(year,2013,2014,2015,2016,2017)
                
                *Components of "Other costs" (to test which components was changing)
                
                        *Labor provisions (for retirement)
                        generate cost_other_labor_provisions = c1900+c1910                        
                        
                        *Other provisions (rather than labor)
                        generate cost_other_other_provisions = c1930+c1940+c1980                        +c2000+c2010        if inlist(year,2008,2009,2010,2011,2012)
                        replace  cost_other_other_provisions = c1930+c1940+c1980+c1981+c1982+c1991+c1992+c2000+c2010        if inlist(year,2013,2014,2015,2016,2017)

                        *Expenditures to government
                        generate cost_other_govt                   = c2430+c2470+c2480
                        generate cost_other_govt_taxes = c2430
                        
                        *Personal costs from F102
                        generate cost_other_personal = c3194
                        
        ** Inventories costs
        
                generate cost_inventories        = c1470-c1520 +c1530-c1570 +c1580-c1590 +c1600-c1610
                
        ** Assign the discrepancy between "total cost" and the sum of its sub-items to "other costs"
                
                generate cost_disp             = cost - cost_calculated
                replace  cost_other             = cost_other + cost_disp             if cost_disp > 0
                replace  cost_calculated = cost_calculated + cost_disp if cost_disp > 0

                drop cost_disp        

        ** Check to verify the variables add up to total costs
        
                generate cost_calc     = domestic_purchase + imports_tf + cost_other_production + cost_financial + cost_labor + cost_other + cost_inventories
                generate cost_disp     = abs(cost_calc - cost_calculated)
                generate cost_disp_rel = cond(cost_disp == 0, 0, abs(cost_calc - cost_calculated) / max(cost_calc, cost_calculated))
                        
                qui count if cost_disp_rel > 0.01 & cost_disp > 5
                if (r(N) == 0) {
                        di as result "`form' (cost): calculation correct!"
                }
                else if (r(N) > 0) {
                        di as result "`form' (cost): `r(N)' cases with relative(discrepancy) > 1% & abs(discrepancy) > 5"
                }
                
                drop cost_disp cost_disp_rel cost_calc
        
*** Revenue

        ** Total
                generate rev                        = c1440
                generate revenue_calculated = `revenue_F102_08_11' if inlist(year,2008,2009,2010,2011)
                replace  revenue_calculated = `revenue_F102_12'    if inlist(year,2012)
                replace  revenue_calculated = `revenue_F102_13'    if inlist(year,2013)
                replace  revenue_calculated = `revenue_F102_14_17' if inlist(year,2014,2015,2016,2017)
        
        ** Export revenue
                generate exports_tf        = c1370
        
        ** Domestic revenue
                generate revenue_domestic        = c1350+c1360      +c1410+c2990+c3010+c3040+c3080+c3160        if inlist(year,2008,2009,2010,2011,2012,2013)
                replace  revenue_domestic        = c1350+c1360+c1373+c1410+c2990+c3010+c3040+c3080+c3160        if inlist(year,2014,2015,2016,2017)
                
        ** Financial revenue
                generate revenue_financial = c1390+c3190                                                        
                
        ** Dividends revenue
                generate revenue_dividends = c1420+c3192        if inlist(year,2008,2009,2010,2011)
                replace  revenue_dividends = c1425+c3192        if inlist(year,2012,2013,2014,2015,2016,2017)
                
        ** Other revenue
                generate revenue_other =       c1380+c1400                  +c1430+c3170+c3180+c3193        if inlist(year,2008,2009,2010,2011,2012)
                replace  revenue_other =       c1380+c1400                           +c3170+c3180+c3193                             if inlist(year,2013)
                replace  revenue_other = c1371+c1380+c1400+c1411+c1413+c1416      +c3170+c3180+c3193        if inlist(year,2014,2015,2016,2017)
                
                *Subitem "Other exempted income"
                generate revenue_other_exempted_income = c1430        if inlist(year,2008,2009,2010,2011,2012)
                replace  revenue_other_exempted_income = 0                        if inlist(year,2013,2014,2015,2016,2017)
        
                *Personal wage revenue
                generate revenue_labor_personal = c3240
        
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
                
                drop revenue_disp revenue_disp_rel revenue_calc
                
*** Profit
        generate profit                               = c2800-c2810                                 
        generate profit_calculated = c1440-c2760 


*** Verify that personal revenue/costs are correct

        ** Revenue
        
                generate revenue_personal      = c2990+c3010+c3040+c3080+c3160+c3170+c3180+c3190+c3192+c3193
                generate revenue_personal_disp = c3200 - revenue_personal
                
                *Assign the discrepancy between "total" personal rev and the sum of its sub-items to "other revenue"
                replace revenue_other             = revenue_other    + revenue_personal_disp          if revenue_personal_disp > 0
                replace revenue_personal   = revenue_personal + revenue_personal_disp         if revenue_personal_disp > 0
                replace revenue_calculated = revenue_calculated + revenue_personal_disp if revenue_personal_disp > 0
                
                replace  revenue_personal_disp     = abs(c3200 - revenue_personal)
                generate revenue_personal_disp_rel = cond(revenue_personal_disp==0, 0, revenue_personal_disp / max(c3200, revenue_personal))
                        
                qui count if revenue_personal_disp_rel > 0.01 & revenue_personal_disp > 5
                if (r(N) == 0) {
                        di as result "`form' (personal revenue): calculation correct!"
                }
                else if (r(N) > 0) {
                        di as result "`form' (personal revenue): `r(N)' cases with relative(discrepancy) > 1% & abs(discrepancy) > 5"
                }
        
        ** Cost
        
                generate cost_personal = c3000+c3020+c3050+c3090      +c3194+c3250 if inlist(year,2008,2009,2010,2011)
                replace  cost_personal = c3000+c3020+c3050+c3090+c3162+c3194+c3250 if inlist(year,2012,2013,2014,2015,2016,2017)
                
                generate cost_personal_disp    = c3210+c3250 - cost_personal
                
                *Assign the discrepancy between "total" personal costs and the sum of its sub-items to "other costs"
                replace cost_other            = cost_other    + cost_personal_disp   if cost_personal_disp > 0
                replace cost_personal         = cost_personal + cost_personal_disp   if cost_personal_disp > 0
                replace cost_calculated = cost_calculated + cost_personal_disp if cost_personal_disp > 0

                replace  cost_personal_disp     = abs(c3210+c3250 - cost_personal)
                generate cost_personal_disp_rel = cond(cost_personal_disp==0, 0, cost_personal_disp / max(c3210+c3250, cost_personal))
                        
                qui count if cost_personal_disp_rel > 0.01 & cost_personal_disp > 5
                if (r(N) == 0) {
                        di as result "`form' (personal costs): calculation correct!"
                }
                else if (r(N) > 0) {
                        di as result "`form' (personal costs): `r(N)' cases with relative(discrepancy) > 1% & abs(discrepancy) > 5"
                }
        drop revenue_personal_disp* cost_personal_disp*

*/
        
end
