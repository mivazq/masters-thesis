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

use "/home/mivazq/data/transactions_ecuador/1_rawdata/F102/F102_2008_jul2012.dta" , clear

* First let's reorder the variables
order c*, seq

* Then, let's rename all our variables to free up the right names
rename (c*) (old_c*)

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
drop old_c610-old_c1340


////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////


*** INCOME
rename old_c1350 c1800 // "(+) NET DOMESTIC SALES SUBJECT TO 12% TAX RATE"
...
