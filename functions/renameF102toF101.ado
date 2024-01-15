////////////////////////////////////////////////////////////////////////////////
* File name:        renameF102toF101.ado
* Author:           Miguel Vázquez Vázquez
* Creation date:    20 November 2023
* Description:      This do file renames all variables of F102 form to F101 names
* Arguments:
*                   -
* Output:
*                   Tax filing positions in form of variables
////////////////////////////////////////////////////////////////////////////////

program define renameF102toF101, rclass
    version 18

* First let's rename all our variables to free up the right names
rename (c*) (old_c*)

////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////

*** TRANSACTIONS WITH RELATED PARTIES ABROAD DURING THE FISCAL PERIOD
cap gen c110 = 0, before(old_c160)
cap gen c120 = 0, after(c110)
cap gen c130 = 0, after(c120)
cap gen c140 = 0, after(c130)
cap gen c150 = 0, after(c140)
cap gen c160 = 0, after(c150)

////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////

*** ASSETS

////////////////////////////////////////////////////////////////////////////////

* CURRENT ASSETS
rename old_c160 c170 // "(+) FUNDS, BANKS ASSETS"
rename old_c170 c180 // "(+) CURRENT INVESTMENTS"
gen c190 = 0, after(c180) // "(+) TEMPORARY INVESTMENTS"
rename old_c180 c200 // "(+) ACCOUNTS RECEIVABLE FROM NATURAL PERSONS OR FIRMS - RELATED HOME"
rename old_c190 c210 // "(+) ACCOUNTS RECEIVABLE FROM NATURAL PERSONS OR FIRMS - RELATED ABROAD"
rename old_c210 c220 // "(+) ACCOUNTS RECEIVABLE FROM NATURAL PERSONS OR FIRMS - NOT RELATED HOME"
rename old_c220 c230 // "(+) ACCOUNTS RECEIVABLE FROM NATURAL PERSONS OR FIRMS - NOT RELATED ABROAD" 
rename old_c230 c240 // "(+) OTHER ACCOUNTS RECEIVABLE (e.g. FROM EMPLOYEES) - RELATED HOME" 
rename old_c240 c250 // "(+) OTHER ACCOUNTS RECEIVABLE (e.g. FROM EMPLOYEES) - RELATED ABROAD"
rename old_c250 c260 // "(+) OTHER ACCOUNTS RECEIVABLE (e.g. FROM EMPLOYEES) - NOT RELATED HOME"
rename old_c260 c270 // "(+) OTHER ACCOUNTS RECEIVABLE (e.g. FROM EMPLOYEES) - NOT RELATED ABROAD"
cap gen c280 = 0, after(c270) // "(=) ACCOUNTS RECEIVABLE FROM NATURAL PERSONS OR FIRMS - RELATED"     // c200 + c210
cap gen c290 = 0, after(c280) // "(=) ACCOUNTS RECEIVABLE FROM NATURAL PERSONS OR FIRMS - NOT RELATED" // c220 + c230
rename old_c280 c300 // "(=) OTHER ACCOUNTS RECEIVABLE (e.g. FROM EMPLOYEES)" // c240 + c250 + c260 + c270
rename old_c270 c310 // "(-) PROVISION FOR UNCOLLECTIBLE ACCOUNTS"
rename old_c290 c320 // "(+) TAX CREDIT IN FAVOR OF THE TAX PAYER (VAT)"
rename old_c320 c330 // "(+) TAX CREDIT IN FAVOR OF THE TAX PAYER (INCOME TAX)" // c340 + c350
rename old_c300 c340 // "(+) TAX CREDIT IN FAVOR OF THE TAX PAYER (INCOME TAX) - PREVIOUS YEARS"
rename old_c310 c350 // "(+) TAX CREDIT IN FAVOR OF THE TAX PAYER (INCOME TAX) - CURRENT YEAR"
rename old_c330 c360 // "(+) INVENTORY OF RAW MATERIAL"
rename old_c340 c370 // "(+) INVENTORY OF GOODS IN PROCESS"
rename old_c350 c380 // "(+) INVENTORY OF SUPPLIES AND MATERIALS"
rename old_c360 c390 // "(+) INVENTORY OF FINISHED GOODS AND MERCHANDISE IN THE STORE"
rename old_c370 c400 // "(+) GOODS IN TRANSIT"
rename old_c380 c410 // "(+) INVENTORY OF SPARE PARTS, TOOLS AND ACCESSORIES"
rename old_c390 c420 // "(+) ASSETS PAID IN ADVANCE" // c430 + c440 + c450
cap gen c430 = 0, after(c420) // "(+) INSURANCE PAID IN ADVANCE"
cap gen c440 = 0, after(c430) // "(+) RENT PAID IN ADVANCE"
cap gen c450 = 0, after(c440) // "(+) DIVIDENDS PAID IN ADVANCE"
rename old_c400 c460 // "(+) OTHER CURRENT ASSETS"
rename old_c410 c470 // "(=) TOTAL CURRENT ASSETS" // Total

drop old_c200

////////////////////////////////////////////////////////////////////////////////

* FIXED ASSETS
rename old_c420 c480 // "(+) PROPERTY (EXCEPT LAND)"
rename old_c430 c490 // "(+) SHIPS, AIRCRAFT, AND BARGES (ETC.)"
rename old_c440 c500 // "(+) FURNITURE, FURNISHINGS"
rename old_c450 c510 // "(+) MACHINERY, EQUIPMENT, AND FACILITIES" // c630 + c640
rename old_c470 c520 // "(=) MACHINERY, EQUIPMENT, FACILITIES, FURNITURE, FURNISHINGS" // c500 + c510   OR   c500 + (630 + c640)
cap gen c530 = 0, after(c520) // "(=) MACHINERY, EQUIPMENT, FACILITIES, FURNITURE, FURNISHINGS, BUILDINGS" // c480 + c520   OR   c480 + (c500 + c510)
rename old_c480 c540 // "(+) COMPUTER EQUIPMENT AND SOFTWARE"
rename old_c490 c550 // "(+) VEHICLES AND TRANSPORTATION EQUIPMENT"
rename old_c500 c560 // "(+) OTHER FIXED ASSETS"
rename old_c530 c570 // "(=) TOTAL ACCUMULATED DEPRECIATION OF FIXED ASSETS" // c580 + c660 + c670
cap gen c580 = 0, after(c570) // "(-) ACCUMULATED DEPRECIATION OF FIXED ASSETS - NON ACCELERATED"
rename old_c540 c590 // "(+) LAND"
cap gen c600 = 0, after(c590) // "(=) BUILDINGS + FACILITIES" // c610 + c630
cap gen c610 = 0, after(c600) // "(+) BUILDINGS"
cap gen c620 = 0, after(c610) // "(=) MACHINERY, EQUIPMENT, FURNITURE, FURNISHINGS" // c500 + c640
cap gen c630 = 0, after(c620) // "(+) FACILITIES"
cap gen c640 = 0, after(c630) // "(+) MACHINERY, EQUIPMENT"
rename old_c550 c650 // "(+) NOT FINISHED BUILDINGS"
rename old_c510 c660 // "(-) ACCUMULATED DEPRECIATION OF FIXED ASSETS - ACCELERATED"
rename old_c520 c670 // "(-) ACCUMULATED DEPRECIATION OF VEHICLES AND TRANSPORTATION EQUIPMENT - ACCELERATED"
cap gen c680 = 0, after(c670) // "(=) TOTAL FIXED ASSETS TANGIBLES"
rename old_c560 c690 // "(=) TOTAL FIXED ASSETS" // Total

* There is in F102 a special cell that sums c480 and c490. In the cases where it's
* not zero and the two other are zero we assign this to c480.
replace c480 = old_c460 if old_c460!=0 & c480==0 & c490==0
drop old_c460

////////////////////////////////////////////////////////////////////////////////

* DEFERRED ASSETS
rename old_c570 c700 // "(+) TRADEMARKS, PATENTS, ETC."
cap gen c710 = 0, after(c700) // "(-) ACCUMULATED AMORTIZATION TRADEMARKS, PATENTS, ETC."
rename old_c640 c720 // "(=) TOTAL INTANGIBLE FIXED ASSETS"
rename old_c580 c730 // "(+) ORGANIZATIONAL COSTS"
rename old_c600 c740 // "(+) RESEARCH AND EXPLORATION COSTS"
cap gen c750 = 0, after(c740) // "(+) BALANCE DEBTOR CURRENCY EXCHANGE"
rename old_c620 c760 // "(+) OTHER DEFERRED ASSETS"
rename old_c630 c770 // "(-) ACCUMULATED DEPRECIATION"
rename old_c650 c780 // "(=) TOTAL DEFERRED ASSETS" // Total

drop old_c660

////////////////////////////////////////////////////////////////////////////////

* LONG TERM ASSETS
rename old_c670 c790 // "(+) LONG TERM INVESTMENTS - SHARES AND OTHER EQUITY"
rename old_c680 c800 // "(+) LONG TERM INVESTMENTS - OTHER"
rename old_c700 c810 // "(+) LONG TERM ACCOUNTS RECEIVABLE FROM NATURAL FIRMS OR PERSONS - RELATED HOME"
rename old_c710 c820 // "(+) LONG TERM ACCOUNTS RECEIVABLE FROM NATURAL FIRMS OR PERSONS - RELATED ABROAD"
rename old_c720 c830 // "(+) LONG TERM ACCOUNTS RECEIVABLE FROM NATURAL FIRMS OR PERSONS - NOT RELATED HOME"
rename old_c740 c840 // "(+) LONG TERM ACCOUNTS RECEIVABLE FROM NATURAL FIRMS OR PERSONS - NOT RELATED ABROAD"
rename old_c590 c850 // "(=) LONG TERM ACCOUNTS RECEIVABLE FROM NATURAL FIRMS OR PERSONS" // c790 + c800 + c810 + c820 + c830 + c840
rename old_c750 c860 // "(+) OTHER LONG TERM ACCOUNTS RECEIVABLE - RELATED HOME"
rename old_c760 c870 // "(+) OTHER LONG TERM ACCOUNTS RECEIVABLE - RELATED ABROAD"
rename old_c770 c880 // "(+) OTHER LONG TERM ACCOUNTS RECEIVABLE - NOT RELATED HOME"
rename old_c780 c890 // "(+) OTHER LONG TERM ACCOUNTS RECEIVABLE - NOT RELATED ABROAD"
cap gen c900 = 0, after(c890) // "(=) OTHER LONG TERM ACCOUNTS RECEIVABLE"  // c860 + c870 + c880 + c890
rename old_c790 c910 // "(-) PROVISION FOR UNCOLLECTIBLE ACCOUNTS"
rename old_c690 c920 // "(+) OTHER LONG TERM COSTS PAYED IN ADVANCE"
rename old_c800 c1010 // "(+) OTHER LONG TERM ASSETS"
rename old_c810 c1070 // "(=) TOTAL LONG TERM ASSETS" // Total

replace c850 = old_c730 if old_c730!=0 & c850==0
drop old_c730

////////////////////////////////////////////////////////////////////////////////

* TOTAL ASSETS
rename old_c840 c1050 // "( ) CONTINGENT ASSETS"
cap gen c1075 = 0, after(c1050) // "( ) ASSETS COMING FROM REINVESTMENT OF PROFITS"
rename old_c830 c1080 // "(=) TOTAL ASSETS" // (c470+c690+c780+c1070)


////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////


*** LIABILITIES & EQUITIES

* We don't need liabilities and equity at all. Drop them all.
drop old_c610 old_c820 old_c850-old_c1340


////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////


*** INCOME
rename old_c1350 c1800 // "(+) NET DOMESTIC SALES SUBJECT TO 12% TAX RATE"
rename old_c1360 c1810 // "(+) NET DOMESTIC SALES SUBJECT TO 0% TAX RATE"
rename old_c1370 c1820 // "(+) NET EXPORTS"
rename old_c1380 c1830 // "(+) OTHER INCOME FROM ABROAD"
rename old_c1390 c1840 // "(+) FINANCIAL RENTS"
rename old_c1400 c1850 // "(+) OTHER TAXABLE INCOME"
rename old_c1410 c1860 // "(+) GAIN ON SALE OF FIXED ASSETS"
rename old_c1420 c1870 // "(+) DOMESTIC DIVIDENDS RECEIVED"
cap rename old_c1411 c1880 // "(+) EXEMPTED INCOME FROM DONATIONS AND CONTRIBUTIONS - FROM PUBLIC RESOURCES"         // later forms only
cap rename old_c1413 c1890 // "(+) EXEMPTED INCOME FROM DONATIONS AND CONTRIBUTIONS - FROM OTHER ECUADORIAN SOURCES" // later forms only
cap rename old_c1416 c1900 // "(+) EXEMPTED INCOME FROM DONATIONS AND CONTRIBUTIONS - FROM ABROAD"                   // later forms only
cap gen c1880 = 0, after(c1870) // "(+) EXEMPTED INCOME FROM DONATIONS AND CONTRIBUTIONS - FROM PUBLIC RESOURCES"
cap gen c1890 = 0, after(c1880) // "(+) EXEMPTED INCOME FROM DONATIONS AND CONTRIBUTIONS - FROM OTHER ECUADORIAN SOURCES"
cap gen c1900 = 0, after(c1890) // "(+) EXEMPTED INCOME FROM DONATIONS AND CONTRIBUTIONS - FROM ABROAD"
cap gen c1910 = 0, after(c1900) // "(+) EXEMPTED INCOME FROM DONATIONS AND CONTRIBUTIONS" // (c1411+c1413+c1416), but not existent
rename old_c1430 c1920 // "(+) OTHER EXEMPTED INCOME"
rename old_c1440 c1930 // "(=) TOTAL INCOME"                                            // Total
rename old_c1450 c1940 // "( ) NET SALES OF FIXED ASSETS"
rename old_c1460 c1950 // "( ) REIMBURSEMENT RECEIVED AS INTERMEDIARY"

cap replace c1870 = old_c1425 if old_c1425!=0 & c1870==0
cap drop old_c1425

////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////

*** COSTS & EXPENSES

////////////////////////////////////////////////////////////////////////////////

* INVENTORY
rename old_c1470 c1960 // "(+) INITIAL INVENTORY OF GOODS NOT PROUDCED BY THE FIRM - PRODUCTION COSTS"
rename old_c1480 c1970 // "(+) NET PURCHASES OF DOMESTIC GOODS NOT PRODUCED BY THE FIRM - PRODUCTION COSTS"
rename old_c1490 c1980 // "(+) IMPORTS NOT PRODUCED BY THE FIRM - PRODUCTION COSTS"
rename old_c1500 c1990 // "(+) IMPORTS NOT PRODUCED BY THE FIRM - ADMIN EXPENSES"
rename old_c1520 c2000 // "(-) FINAL INVENTORY OF GOOD NOT PRODUCED BY THE FIRM - PRODUCTION COSTS"
rename old_c1530 c2010 // "(+) INITIAL INVENTORY OF RAW MATERIAL - PRODUCTION COSTS"
rename old_c1540 c2020 // "(+) NET DOMESTIC PURCHES OF RAW MATERIAL - PRODUCTION COSTS"
rename old_c1550 c2030 // "(+) IMPORTS OF RAW MATERIAL - PRODUCTION COSTS"
rename old_c1570 c2040 // "(-) FINAL INVENTORY OF RAW MATERIAL - PRODUCTION COSTS"
rename old_c1580 c2240 // "(+) INITIAL INVENTORY OF PRODUCTS IN PROCESS - PRODUCTION COSTS"
rename old_c1590 c2250 // "(-) FINAL INVENTORY OF PRODUCTS IN PROCESS - PRODUCTION COSTS"
rename old_c1600 c2260 // "(+) INITIAL INVENTORY OF FINISHED PRODUCTS - PRODUCTION COSTS"
rename old_c1610 c2270 // "(-) FINAL INVENTORY OF FINISHED PRODUCTS - PRODUCTION COSTS"

drop old_c1510 old_c1560

////////////////////////////////////////////////////////////////////////////////

* LABOUR
rename old_c1620 c2280 // "(+) WAGES, SALARIES AND OTHER TAXABLE REMUNERATIONS - PRODUCTION COSTS"
cap gen c2290 = 0, after(c2280) // "(+) WAGES, SALARIES AND OTHER TAXABLE REMUNERATIONS - ADMIN EXPENSES"
rename old_c1650 c2300 // "(+) SOCIAL BENEFITS AND OTHER NON-TAXABLE COMPENSATION - PRODUCTION COSTS"
rename old_c1660 c2310 // "(+) SOCIAL BENEFITS AND OTHER NON-TAXABLE COMPENSATION - ADMIN EXPENSES"
rename old_c1630 c2320 // "(+) WAGES, SALARIES AND OTHER TAXABLE REMUNERATIONS"
cap gen c2330 = 0, after(c2320) // "(+) PAYMENTS COMPLETED TO OUTSOURCING"
rename old_c1640 c2340 // "(+) WAGES, SALARIES AND OTHER TAXABLE REMUNERATIONS OUTSOURCING"
rename old_c1720 c2350 // "(+) OTHER REMUNERATIONS TO OUTSOURCING"
rename old_c1670 c2360 // "(+) CONTRIBUTION TO SOCIAL SECURITY (INCLUDING RESERVE FUND) - PRODUCTION COSTS"
rename old_c1680 c2370 // "(+) CONTRIBUTION TO SOCIAL SECURITY (INCLUDING RESERVE FUND) - ADMIN EXPENSES"
rename old_c1700 c2380 // "(+) PROFESSIONAL FEES AND EXPENSES - PRODUCTION COSTS"
rename old_c1710 c2390 // "(+) PROFESSIONAL FEES AND EXPENSES - ADMIN EXPENSES"
rename old_c1730 c2400 // "(+) FEES TO FOREIGNERS FOR ONE-TIME EXPENSES - PRODUCTION COSTS"
rename old_c1740 c2410 // "(+) FEES TO FOREIGNERS FOR ONE-TIME EXPENSES - ADMIN EXPENSES"

drop old_c1690


////////////////////////////////////////////////////////////////////////////////

* OTHER OPERATIONAL COSTS

* Label variables
rename old_c1920 c2110 // "(+) OTHER PRODUCTION EXPENSES"
cap gen c2200 = 0, after(c2110) // "(+) REAL ESTATE RENT FOR NATURAL PERSONS PROPERTIES - PRODUCTION COST"
rename old_c1950 c2210 // "(+) REAL ESTATE RENT FOR NATURAL PERSONS PROPERTIES - ADMIN EXPENSES"
cap gen c2220 = 0, after(c2110) // "(+) REAL ESTATE RENT FOR COMPANIES PROPERTIES - PRODUCTION COST"
rename old_c1960 c2230 // "(+) REAL ESTATE RENT FOR COMPANIES PROPERTIES - ADMIN EXPENSES"
rename old_c1750 c2420 // "(=) REAL ESTATE RENT - PRODUCTION COSTS" // c2200 + c2220
rename old_c1760 c2430 // "(=) REAL ESTATE RENT - ADMIN EXPENSES" // c2210 + c2230
rename old_c1790 c2440 // "(+) MAINTENANCE AND REPAIRS - PRODUCTION COSTS"
rename old_c1800 c2450 // "(+) MAINTENANCE AND REPAIRS - ADMIN EXPENSES"
rename old_c1810 c2640 // "(+) FUEL AND LUBRICANTS - PRODUCTION COSTS"
rename old_c1820 c2650 // "(=) FUEL AND LUBRICANTS - ADMIN EXPENSES" // c2660 + c2470
cap gen c2660 = 0, after(c2650) // "(+) FUEL - ADMIN EXPENSES"
rename old_c1830 c2470 // "(+) LUBRICANTS - ADMIN EXPENSES"
rename old_c1840 c2670 // "(+) MARKETING - PRODUCTION COSTS"
rename old_c1850 c2680 // "(+) MARKETING - ADMIN EXPENSES"
rename old_c1860 c2690 // "(+) SUPPLIES AND MATERIALS - PRODUCTION COSTS"
rename old_c1870 c2700 // "(+) SUPPLIES AND MATERIALS - ADMIN EXPENSES"
rename old_c1880 c2710 // "(+) TRANSPORTATION - PRODUCTION COSTS"
rename old_c1890 c2720 // "(+) TRANSPORTATION - ADMIN EXPENSES"
rename old_c1900 c2730 // "(+) PROVISIONS - FOR RETIREMENT - PRODUCTION COSTS"
rename old_c1910 c2740 // "(+) PROVISIONS - FOR RETIREMENT - ADMIN EXPENSES"
rename old_c1930 c2750 // "(+) PROVISIONS - FOR EVICTION - PRODUCTION COSTS"
rename old_c1940 c2760 // "(+) PROVISIONS - FOR EVICTION - ADMIN EXPENSES"
rename old_c1980 c2770 // "(+) PROVISIONS - FOR UNCOLLECTIBLE ACCOUNTS - ADMIN EXPENSES"
rename old_c2000 c2780 // "(+) PROVISIONS - OTHER PROVISIONS - PRODUCTION COSTS"
rename old_c2010 c2790 // "(+) PROVISIONS - OTHER PROVISIONS - ADMIN EXPENSES"
rename old_c2020 c2800 // "(=) TOTAL PROVISIONS" // c2730 + c2740 + c2750 + c2760 + c2770 + c2780 + c2790
rename old_c2030 c2810 // "(+) COMMERCIAL RENT - HOME - PRODUCTION COSTS"
rename old_c2040 c2820 // "(+) COMMERCIAL RENT - HOME - ADMIN EXPENSES"
rename old_c2050 c2830 // "(+) COMMERCIAL RENT - ABROAD - PRODUCTION COSTS"
rename old_c2060 c2840 // "(+) COMMERCIAL RENT - ABROAD - ADMIN EXPENSES"
cap gen c2850 = 0, after(c2840) // "(=) COMMERCIAL RENT - PRODUCTION COSTS" // c2810 + c2830
cap gen c2860 = 0, after(c2850) // "(=) COMMERCIAL RENT - ADMIN EXPENSES" // c2820 + c2840
cap gen c2180 = 0, after(c2860) // "(=) COMMERCIAL RENT - HOME"
cap gen c2190 = 0, after(c2180) // "(=) COMMERCIAL RENT - ABROAD"
rename old_c2070 c2870 // "(+) COMMISSIONS - HOME - PRODUCTION COSTS"
rename old_c2080 c2880 // "(+) COMMISSIONS - HOME - ADMIN EXPENSES"
rename old_c2090 c2890 // "(+) COMMISSIONS - ABROAD - PRODUCTION COSTS"
rename old_c2100 c2900 // "(+) COMMISSIONS - ABROAD - ADMIN EXPENSES"
rename old_c2110 c2910 // "(=) TOTAL COMMISSIONS" // c2870 + c2880 + c2890 + c2900
rename old_c1970 c2460 // "(+) COMMISSIONS OF COMPANIES"

drop old_c1770 old_c1780


////////////////////////////////////////////////////////////////////////////////

* INTERESTS
rename old_c2120 c2920 // "(+) INTEREST PAID TO BANKS - HOME - PRODUCTION COSTS"
rename old_c2130 c2930 // "(+) INTEREST PAID TO BANKS - HOME - ADMIN EXPENSES"
rename old_c2140 c2940 // "(+) INTEREST PAID TO BANKS - ABROAD - PRODUCTION COSTS"
rename old_c2150 c2950 // "(+) INTEREST PAID TO BANKS - ABROAD - ADMIN EXPENSES"
rename old_c2230 c2960 // "(+) INTEREST PAID TO OTHERS - RELATED HOME - PRODUCTION COSTS"
rename old_c2240 c2970 // "(+) INTEREST PAID TO OTHERS - RELATED HOME - ADMIN EXPENSES"
rename old_c2250 c2980 // "(+) INTEREST PAID TO OTHERS - RELATED ABROAD - PRODUCTION COSTS"
rename old_c2260 c2990 // "(+) INTEREST PAID TO OTHERS - RELATED ABROAD - ADMIN EXPENSES"
rename old_c2270 c3000 // "(+) INTEREST PAID TO OTHERS - NOT RELATED HOME - PRODUCTION COSTS"
rename old_c2280 c3010 // "(+) INTEREST PAID TO OTHERS - NOT RELATED HOME - ADMIN EXPENSES"
rename old_c2290 c3020 // "(+) INTEREST PAID TO OTHERS - NOT RELATED ABROAD - PRODUCTION COSTS"
rename old_c2300 c3030 // "(+) INTEREST PAID TO OTHERS - NOT RELATED ABROAD - ADMIN EXPENSES"
rename old_c2160 c2500 // "(=) INTEREST PAID TO OTHERS - HOME" // c2960 + c2970 + c3000 + c3010
rename old_c2170 c2510 // "(=) INTEREST PAID TO OTHERS - ABROAD" // c2980 + c2990 + c3020 + c3030
rename old_c2310 c3040 // "(=) TOTAL INTEREST PAID" // c2920 + c2930 + c2940 + c2950 + c2500 + c2510

////////////////////////////////////////////////////////////////////////////////

* LOSSES
rename old_c2320 c3050 // "(+) LOSS FROM SALE OF ASSETS - RELATED - PRODUCTION COSTS"
rename old_c2330 c3060 // "(+) LOSS FROM SALE OF ASSETS - RELATED - ADMIN EXPENSES"
rename old_c2340 c3070 // "(+) LOSS FROM SALE OF ASSETS - NOT RELATED - PRODUCTION COSTS"
rename old_c2350 c3080 // "(+) LOSS FROM SALE OF ASSETS - NOT RELATED - ADMIN EXPENSES"
rename old_c2720 c3090 // "(=) TOTAL LOSS FROM SALE OF ASSETS" // c3050 + c3060 + c3070 + c3080
rename old_c2490 c2570 // "(+) LOSS FROM SALES OF FIXED ASSETS"
rename old_c2500 c2580 // "(+) LOSS FROM SALES OF DEFERRED ASSETS"
rename old_c2360 c3100 // "(+) OTHER LOSSES - PRODUCTION COSTS"
rename old_c2370 c3110 // "(+) OTHER LOSSES - ADMIN EXPENSES"

////////////////////////////////////////////////////////////////////////////////

* DEPRECIATIONS AND AMORTIZATIONS
rename old_c2510 c3220 // "(+) DEPRECIATION OF FIXED ASSETS - ACCELERATED - PRODUCTION COSTS"
rename old_c2520 c3230 // "(+) DEPRECIATION OF FIXED ASSETS - ACCELERATED - ADMIN EXPENSES"
rename old_c2550 c3240 // "(+) DEPRECIATION OF FIXED ASSETS - NON ACCELERATED - PRODUCTION COSTS"
rename old_c2560 c3250 // "(+) DEPRECIATION OF FIXED ASSETS - NON ACCELERATED - ADMIN EXPENSES"
rename old_c2540 c3260 // "(=) TOTAL DEPRECIATION OF FIXED ASSETS" // c3220 + c3230 + c3240 + c3250
rename old_c2570 c3270 // "(+) AMORTIZATION - PRODUCTION COSTS"
rename old_c2580 c3280 // "(+) AMORTIZATION - ADMIN EXPENSES"
rename old_c1990 c2490 // "(+) AMORTIZATION OF BALANCE DEBTOR CURRENCY EXCHANGE"

replace c3270 = old_c2590 if c3270==0 & c3280==0 & old_c2590!=0
drop old_c2590

////////////////////////////////////////////////////////////////////////////////

* OTHER NON-OPERATIONAL COSTS

cap gen c2600 = 0, before(old_c2460) // "(=) TOTAL NON-OPERATIONAL EXPENSES"
rename old_c2460 c2480 // "(+) NOTARY AND REPRESENTATIVES FEES"
rename old_c2180 c2520 // "(+) REIMBURSEMENT EXPENSES - HOME"
rename old_c2190 c2530 // "(+) REIMBURSEMENT EXPENSES - ABROAD"
rename old_c2210 c2540 // "(+) OTHER EXPENSES - HOME"
rename old_c2220 c2550 // "(+) OTHER EXPENSES - ABROAD"
rename old_c2710 c2560 // "(+) OTHER ADMINISTRATIVE SALES-RELATED EXPENSES"
rename old_c2730 c2590 // "(+) OTHER NON-OPERATIONAL EXPENSES"
rename old_c2380 c3120 // "(+) INSURANCE AND REINSURANCE INTERMEDIARIES - PRODUCTION COSTS"
rename old_c2390 c3130 // "(+) INSURANCE AND REINSURANCE INTERMEDIARIES - ADMIN EXPENSES"
rename old_c2400 c3140 // "(+) INDIRECT COSTS INCURRED FROM ABROAD BY RELATED PARTIES - PRODUCTION COSTS"
rename old_c2410 c3150 // "(+) INDIRECT COSTS INCURRED FROM ABROAD BY RELATED PARTIES - ADMIN EXPENSES"
rename old_c2420 c3160 // "(+) ADMINISTRATIVE COSTS - ADMIN EXPENSES"
rename old_c2430 c3170 // "(+) TAXES, CONTRIBUTIONS AND OTHER - ADMIN EXPENSES"
rename old_c2440 c3180 // "(+) TRAVEL EXPENSES - PRODUCTION COSTS"
rename old_c2450 c3190 // "(+) TRAVEL EXPENSES - ADMIN EXPENSES"
rename old_c2470 c3200 // "(+) VAT INCURRED ON COSTS OR EXPENSES - PRODUCTION COSTS"
rename old_c2480 c3210 // "(+) VAT INCURRED ON COSTS OR EXPENSES - ADMIN EXPENSES"
rename old_c2600 c3290 // "(+) PUBLIC SERVICES - PRODUCTION COSTS"
rename old_c2610 c3300 // "(+) PUBLIC SERVICES - ADMIN EXPENSES"
rename old_c2620 c3310 // "(+) PAYMENT FOR OTHER SERVICES - PRODUCTION COSTS"
rename old_c2630 c3320 // "(+) PAYMENT FOR OTHER SERVICES - ADMIN EXPENSES"
rename old_c2640 c3330 // "(+) PAYMENT FOR OTHER GOODS - PRODUCTION COSTS"
rename old_c2650 c3340 // "(+) PAYMENT FOR OTHER GOODS - ADMIN EXPENSES"
cap gen c3350 = 0, after(c3340) // "(=) OTHER PRODUCTION COSTS" // c3310 + c3330
rename old_c2660 c3360 // "(=) TOTAL COSTS" // Total - PRODUCTION COSTS
rename old_c2750 c3370 // "(=) TOTAL EXPENDITURES" // Total - ADMIN EXPENSES
rename old_c2760 c3380 // "(=) TOTAL COSTS AND EXPENDITURES" // (c2660+c2750)
rename old_c2770 c3390 // "( ) LOSS OF INVENTORY"
rename old_c2780 c3400 // "( ) PAYMENT FOR REIMBURSEMENT PAID AS REIMBURSER"
rename old_c2790 c3410 // "( ) PAYMENT FOR REIMBURSEMENT PAID AS INTERMEDIARY"

replace c2540 = old_c2700 if old_c2700!=0 & c2540==0 & c2550==0

drop old_*

end
