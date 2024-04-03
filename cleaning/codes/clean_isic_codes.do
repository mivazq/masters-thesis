////////////////////////////////////////////////////////////////////////////////
* File name:        clean_isic_codes.do
* Author:           Miguel Vázquez Vázquez
* Creation date:    20 November 2023
* Description:		This do file prepares the ISIC codes table for future merging
*                   "https://unstats.un.org/unsd/classifications/Econ/Download/
*                   In%20Text/ISIC_Rev_3_1_english_structure.txt".
* Input:
*                   $ecuRaw/nomenclatures/ISIC_codes.csv
* Output:
*                   $pathCle/output/isic_codes.csv
////////////////////////////////////////////////////////////////////////////////
quietly do "~/data/transactions_ecuador/3_mivazq/Masters_Thesis/setup.do"
////////////////////////////////////////////////////////////////////////////////

* Load data 
import delimited $ecuRaw/nomenclatures/ISIC_codes.csv, encoding(UTF8) varnames(1) clear

* Create section variables
destring code, gen(temp) force
gen isic_section = code if missing(temp)
gen isic_section_desc = description if missing(temp)
drop temp
carryforward isic_section isic_section_desc, replace // sectors are already sorted
drop if code == isic_section // drop rows now that we have columns

* Create division variables
gen isic_division = code if strlen(code)==2
gen isic_division_desc = description if strlen(code)==2
carryforward isic_division isic_division_desc, replace // sectors are already sorted
drop if code == isic_division // drop rows now that we have columns

* Create group variables
gen isic_group = code if strlen(code)==3
gen isic_group_desc = description if strlen(code)==3
carryforward isic_group isic_group_desc, replace // sectors are already sorted
drop if code == isic_group // drop rows now that we have columns

* Create class variables
rename (code description) (isic_class isic_class_desc)
order isic_class isic_class_desc, last

* Label variables
label variable isic_section         "ISIC Rev.3.1 - Section"
label variable isic_division        "ISIC Rev.3.1 - Division"
label variable isic_group           "ISIC Rev.3.1 - Group"
label variable isic_class           "ISIC Rev.3.1 - Class"
label variable isic_section_desc    "ISIC Rev.3.1 - Section (Description)"
label variable isic_division_desc   "ISIC Rev.3.1 - Division (Description)"
label variable isic_group_desc      "ISIC Rev.3.1 - Group (Description)"
label variable isic_class_desc      "ISIC Rev.3.1 - Class (Description)"

* Add section letter to divisions, groups, and classes (to match with firm info data)
foreach var of varlist isic_division isic_group isic_class {
    replace `var' = isic_section+`var'
}

* Create and save datasets to merge descriptions at different levels
    * Section level
    preserve
        keep isic_section isic_section_desc
        order(isic_section), first
        gduplicates drop
        assert _N==17 // There are 17 Sections in ISIC Rev.3.1
        export delimited $pathCle/output/isic_codes_section.csv, replace
    restore
    
    * Division level
    preserve
        keep isic_division isic_section_desc isic_division_desc
        order(isic_division), first
        gduplicates drop
        assert _N==62 // There are 62 Divisions in ISIC Rev.3.1
        export delimited $pathCle/output/isic_codes_division.csv, replace
    restore
    
    * Group level
    preserve
        keep isic_group isic_section_desc isic_division_desc isic_group_desc
        order(isic_group), first
        gduplicates drop
        assert _N==161 // There are 161 Groups in ISIC Rev.3.1
        export delimited $pathCle/output/isic_codes_group.csv, replace
    restore
    
    * Class level
    preserve
        keep isic_class isic_section_desc isic_division_desc isic_group_desc isic_class_desc
        order(isic_class), first
        gduplicates drop
        assert _N==298 // There are 298 Classes in ISIC Rev.3.1
        export delimited $pathCle/output/isic_codes_class.csv, replace
    restore
