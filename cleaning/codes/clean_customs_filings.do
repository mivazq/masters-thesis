////////////////////////////////////////////////////////////////////////////////
* File name:        clean_customs_filings.do
* Author:           Miguel Vázquez Vázquez
* Creation date:    03 January 2024
* Description:		This file cleans the customs data of export and import
* Input:
*					$ecuRaw/customsData/Exports_08_12_Final.dta
* 					$ecuRaw/customsData/Imports_08_12_Final.dta
* Output:
*					$pathCle/output/exports.csv
*					$pathCle/output/imports.csv
////////////////////////////////////////////////////////////////////////////////
quietly do "~/data/transactions_ecuador/3_mivazq/Masters_Thesis/setup.do"
////////////////////////////////////////////////////////////////////////////////
**#                             1 - EXPORTS
////////////////////////////////////////////////////////////////////////////////

* Load export data and rename variables
use $ecuRaw/customsData/Exports_08_12_Final.dta, clear
rename (Value_usd_FOB Country id) (exports country id_sri)

* Transform SSN ids into RUCs and merge public oil firm
replaceID id_sri
replace id_sri = 129098 if id_sri == 128357

* Generate year/month variables from shipment date
tostring ShipmentDate, replace
gen date  = date(ShipmentDate,"YMD")
format date %td
gen year  = yofd(date)

* Keep only relevant years
keep if year>=2008 & year<=2011

* Label variables
label var id_sri  "Exporter ID"
label var year    "Year of transaction"
label var exports "Export Value (FOB,US$)"
label var country "Destination Country"
label var nandina "10-digit NANDINA code"

* Collapse data
gcollapse (sum) exports, by(year id_sri)

* Round and drop negatives and zeros
// replace exports = round(exports)
drop if round(exports)<=0

* Save
format %20.0g exports
export delimited "$pathCle/output/exports.csv", replace

////////////////////////////////////////////////////////////////////////////////
**#                             2 - IMPORTS
////////////////////////////////////////////////////////////////////////////////

* Load import data and rename variables
use $ecuRaw/customsData/Imports_08_12_Final.dta, clear
rename (value_cif Country id) (imports country id_sri)

* Generate fees
egen fees = rowtotal(advalorem_duty other_fees ice_tax va_tax other_duties)

* Transform SSN ids into RUCs and merge public oil firm
replaceID id_sri
replace id_sri = 129098 if id_sri == 128357

* Drop zero entries
drop if imports==0 & fees==0

* Replace negative VAT in one observations
replace va_tax = 0 if va_tax < 0

* Generate rate of fees to import value
gen rate = fees/imports

* Fix fees in case they are too high 
* To be conservative, I believe fees higher than 10 times the value must be errors
replace fees = fees/1000 if rate>10
replace rate = fees/imports // recalculate rates now

* Keep only relevant years
keep if year>=2008 & year<=2011

* Label variables
label var id_sri  "Importer ID"
label var year    "Year of transaction"
label var imports "Import Value (CIF,US$)"
label var country "Source Country"
label var nandina "10-digit NANDINA code"

* Collapse data and save
gcollapse (sum) imports fees, by(year id_sri)

* Round and drop negatives and zeros
// replace imports = round(imports)
drop if round(imports)<=0

* Save
format %20.0g imports
format %20.0g fees
export delimited "$pathCle/output/imports.csv", replace

