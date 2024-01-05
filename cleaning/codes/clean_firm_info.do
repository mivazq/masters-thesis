////////////////////////////////////////////////////////////////////////////////
* File name:        clean_firm_info.do
* Author:           Miguel Vázquez Vázquez
* Creation date:    20 November 2023
* Description:		This do file renames, cleans and labels the data about firm 
*					characteristics.
* Input:
*                   $ecuRaw/characteristics/characteristicsOld.dta
* Output:
*                   $pathCle/output/firm_info.csv
////////////////////////////////////////////////////////////////////////////////
quietly do "~/data/transactions_ecuador/3_mivazq/Masters_Thesis/setup.do"
////////////////////////////////////////////////////////////////////////////////

* Load data
use $ecuRaw/characteristics/characteristicsOld, clear
order id_sri *

* Replace all SSN firms with their RUC counterparts
note: some firms may have both an SSN and an RUC ID. To ensure that firms use 			///
            the same ID across different datasets, we always use the RUC ID. However,	///
            the data come from the firm registry, so it makes sense that only RUC IDs	///
            were used in the first place and this command makes 0 changes.
replaceID id_sri

* Rename variables
rename prov             province
rename clase            class
rename obligado         mandatory
rename tipo_1           type1
rename tipo_2           type2
rename tipo_3           type3
rename tipo_4           type4
rename artesano         artisan_type
rename estado           status
rename fecha_obligado   BooksStartDate
rename start_date       startdate
rename fecha_alta       SpecialStartDate
rename fecha_baja       SpecialEndDate
rename ciiu_1           indcode1
rename ciiu_2           indcode2
rename ciiu_3           indcode3
rename ciiu_4           indcode4
rename ciiu_5           indcode5
rename ciiu_6           indcode6

* Fix missing industry codes
forvalues l = 1/6 {
    replace indcode`l' = "" if (indcode6 == "999999" | indcode6 == "NO TIE")
}

* Fill in start_date in the few cases missings, assume 1st January 2007 (don't "invent" new entries)
replace startdate = date("01/01/2007", "DMY") if missing(startdate)
 /// 111 cases

* Drop useless industry codes
drop indcode2 // Simply doesn't exist, doesn't have any meaning 
drop indcode6 // Only Ecuador specific (CIIU), doesn't exist in ISIC Rev 3.1
rename (indcode1 indcode3 indcode4 indcode5) (isic_section isic_division isic_group isic_class)

* Indicator of public firms
gen byte soe = (type2 == 22)

* Indicator of firms who keep accounting books
gen byte keeps_books = (mandatory == "S") if mandatory != ""

* Indicator of active firms
gen byte active = (status == "ACT") if status != ""

* Fix provinces with accents or Spanish characters
replace province = "CAÑAR" if regexm(province,"CA.AR")

* Define tax administration regions (Quito and Guayaquil have own regions but can't distinguish here)
gen region_tax = ""
replace region_tax = "ZONA 1" ///
    if inlist(province, "CARCHI", "ESMERALDAS", "IMBABURA", "SUCUMBIOS")
replace region_tax = "ZONA 2 + ZONA 9" ///
    if inlist(province, "NAPO", "ORELLANA", "PICHINCHA") // Quito is in Pichincha
replace region_tax = "ZONA 3" ///
    if inlist(province, "CHIMBORAZO", "COTOPAXI", "PASTAZA", "TUNGURAHUA")
replace region_tax = "ZONA 4" ///
    if inlist(province, "MANABI", "SANTO DOMINGO DE LOS TSACHILAS")
replace region_tax = "ZONA 5 + ZONA 8" ///
    if inlist(province, "BOLIVAR", "GALAPAGOS", "GUAYAS", "LOS RIOS", "SANTA ELENA") // Guayaquil is in Guayas
replace region_tax = "ZONA 6" ///
    if inlist(province, "AZUAY", "CAÑAR", "MORONA SANTIAGO")
replace region_tax = "ZONA 7" ///
    if inlist(province, "EL ORO", "LOJA", "ZAMORA CHINCHIPE")

* Define geographical regions
gen region_geo = ""
replace region_geo = "INSULAR" if inlist(province, "GALAPAGOS")
replace region_geo = "LITORAL" ///
    if inlist(province, "EL ORO", "ESMERALDAS", "GUAYAS", "LOS RIOS", "MANABI", ///
    "SANTA ELENA", "SANTO DOMINGO DE LOS TSACHILAS")
replace region_geo = "INTERANDINA" ///
    if inlist(province, "AZUAY", "BOLIVAR", "CAÑAR", "CARCHI", "CHIMBORAZO", ///
    "COTOPAXI", "IMBABURA", "LOJA", "PICHINCHA") ///
    | inlist(province,  "TUNGURAHUA")
replace region_geo = "AMAZONICA" ///
    if inlist(province, "MORONA SANTIAGO", "NAPO", "ORELLANA", "PASTAZA", ///
    "SUCUMBIOS", "ZAMORA CHINCHIPE")

* Label all variables
lab var id_sri          "Standard SRI ID"
lab var startdate       "Start date of the firm"
lab var soe             "=1 if the firm is state-owned enterprise"
lab var active          "=1 if the firm is active" 				
lab var keeps_books     "=1 if the taxpayer has accounting books"
lab var province        "Province"
lab var region_tax      "Region of tax administration under which firm is operating"
lab var region_geo      "Broad geographical regions"
lab var isic_section    "ISIC Rev.3.1 - Section"
lab var isic_division   "ISIC Rev.3.1 - Division"
lab var isic_group      "ISIC Rev.3.1 - Group"
lab var isic_class      "ISIC Rev.3.1 - Class"

* Save data
drop class active keeps_books mandatory artisan_type status BooksStartDate SpecialStartDate SpecialEndDate type1 type2 type3 type4
order id_sri startdate soe province region_tax region_geo isic_section isic_division isic_group isic_class
sort id_sri
compress
export delimited $pathCle/output/firm_info.csv, replace // Import with encoding(UTF8)!

