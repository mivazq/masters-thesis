////////////////////////////////////////////////////////////////////////////////
* File name:        clean_IDs.do
* Author:           Miguel Vázquez Vázquez
* Creation date:    20 November 2023
* Description:		This file cleans the file containing information on all the
*                   existing IDs across all datasets, assigning them to entities.
*                   The goal is to have a unified list with all existing IDs and 
*                   a clear cut definition for who is an incorporated firm, who 
*                   an unincorporated firm (invidual-firm), and who is a pure 
*                   individual. Additionally creates a crosswalk for ID replacement.
* Input:
*                   $ecuRaw/characteristics/id_charac_mar19.dta
* Output:
*                   $pathFun/replaceID.dta
*                   $pathEst/input/entities.csv
////////////////////////////////////////////////////////////////////////////////
quietly do "~/data/transactions_ecuador/3_mivazq/Masters_Thesis/setup.do"
////////////////////////////////////////////////////////////////////////////////
* VARIABLE DESCRIPTIONS:
*
* 	id_sri- The SSN or RUC id of the entity/individual
*
* 	ssn_s - An indicator for whether the id_sri is an individual (1 for individuals, 
*       0 if not)
*
* 	ruc_s - An indicator for whether the id_sri is an RUC ID (1 if yes, 0 if not)
*
* 	final_consumer - An indicator for final consumers that equals 1 if the 
*		record is a final consumer and 0 otherwise. The ID stored is a fake ID
*		that represents any and all final consumers. Thus there is only 1 ID for 
*		which final_consumer == 1, which is used whenever a firm reports a sale 
*		to a final consumer. This ID only appears in sales annexes.
*
* 	person_report - An indicator that only exists for individuals (i.e. people) 
*		and is 1 if id_sri is an SSN and 2 if id_sri is an RUC
*
* 	id_person_diff_report - If the entity is an individual with both an SSN and 
*		RUC ID, then this variable contins the ID that is not listed in id_sri
////////////////////////////////////////////////////////////////////////////////
**#                 1 - CREATE DATASET FOR ID REPLACEMENT
////////////////////////////////////////////////////////////////////////////////

* Load
use $ecuRaw/characteristics/id_charac_mar19.dta, clear

* Rename variables such that they make sense
ren id_sri                  id_sri_1
ren id_person_diff_report   id_sri_2
ren ssn_s                   is_resident
ren final_consumer          is_final_consumer
ren ruc_s                   is_id_sri_1_ruc
ren person_report           id_sri_1_ruc_or_ssn

* Label variables
order id_sri_1 id_sri_2 is_resident is_final_consumer is_id_sri_1_ruc id_sri_1_ruc_or_ssn
lab var id_sri_1            "ID from SRI 1 (can be SSN or RUC)"
lab var id_sri_2            "ID from SRI 2 (can be SSN or RUC)"
lab var is_resident         "=1 if entity is an Ecuadorian resident, =0 if entity is a legal person (firm) or is a non-resident"
lab var is_final_consumer   "=1 if entity is 'the final consumer', =0 if not"
lab var is_id_sri_1_ruc     "=1 if ID from SRI 1 is a RUC ID, =0 if not"
lab var id_sri_1_ruc_or_ssn "=1 if ID from SRI 1 is a SSN ID, =2 if 0 if ID from SRI 1 is a RUC ID"

* Correctness checks
    * If the entity is NOT an Ecuadorian resident, then it for sure cannot have 
    * a second ID (ID SRI 2) becuase only Ecuadorian residents can have both a 
    * RUC (tax ID) and a SSN (social security ID). Thus...
        * ... 'id_sri_1_ruc_or_ssn' should *NEVER* be missing for Ecuadorian 
        * resident, as it tells whether the ID SRI 1 is SSN or RUC
        assert !missing(id_sri_1_ruc_or_ssn) if is_resident == 1
        * ... 'id_sri_1_ruc_or_ssn' should *ALWAYS* be missing for not Ecuadorian 
        * residents, as they can't have two IDs.
        assert  missing(id_sri_1_ruc_or_ssn) if is_resident == 0
    
    * Note, though, that 'id_sri_2' can be missing for Ecuadorian residents too, 
    * as entities will only have two SRI IDs if they sometimes appeared in the 
    * data with their SSN and sometimes with RUC. If they consistently only used 
    * one of the two, 'id_sri_2' will be missing. 
    gen is_id_sri_2_missing = missing(id_sri_2)
    tab is_id_sri_2_missing if is_resident == 1
    * On the other hand, for not Ecuadorian residents 'id_sri_2' will always be 
    * missing (due to the point above)
    tab is_id_sri_2_missing   if is_resident == 0
    assert  missing(id_sri_2) if is_resident == 0
    drop is_id_sri_2_missing
    
    * Check that 'is_id_sri_1_ruc' and 'id_sri_1_ruc_or_ssn' are consistent for 
    * residents (non-residents/firms have missing 'id_sri_1_ruc_or_ssn')
    tab is_id_sri_1_ruc id_sri_1_ruc_or_ssn, nolabel missing
    tab is_id_sri_1_ruc id_sri_1_ruc_or_ssn if is_resident==1, nolabel missing
    
    * There should only be one final ID for final consumers
    qui count if is_final_consumer == 1
    assert r(N) == 1

* Create new final version of id_sri which prioritizes RUC since we mosly care
* about firms
    * Create empty variable
    gen double id_sri = .
    lab var id_sri "ID from SRI (final, RUC-priority)"
    * Fill with ID SRI 1 if that's the only ID we have
    replace id_sri = id_sri_1 if missing(id_sri_2)
    * When we have two IDs, use the RUC one
    replace id_sri = id_sri_1 if !missing(id_sri_2) & id_sri_1_ruc_or_ssn==2
    replace id_sri = id_sri_2 if !missing(id_sri_2) & id_sri_1_ruc_or_ssn==1
    assert !missing(id_sri) // every entity must have a new ID

* Create crosswalk table for ID replacing
    preserve
        * Keep only the old unique ID variable (contains all existing IDs) and the final one
        keep id_sri_1 id_sri
        rename id_sri_1 id_sri_unique
        isid id_sri_unique
        * Keep only observations that need replacement and store table in functions folder
        keep if id_sri_unique!=id_sri
        save $pathFun/replaceID.dta, replace
    restore


////////////////////////////////////////////////////////////////////////////////
**#                    2 - CREATE DATASET WITH ENTITY TYPES
////////////////////////////////////////////////////////////////////////////////

* First create dummies indicating what type of ID is the final ID
gen     is_ruc = 0
replace is_ruc = 1 if is_id_sri_1_ruc==1 | (id_sri_1_ruc_or_ssn==1 & !missing(id_sri_2))
gen     is_ssn = 0
replace is_ssn = 1 if id_sri_1_ruc_or_ssn==1 & missing(id_sri_2)
gen     is_unk = 0
replace is_unk = 1 if (is_ruc==0 & is_ssn==0)
lab var is_ruc "=1 if final ID is RUC"
lab var is_ssn "=1 if final ID is SSN"
lab var is_unk "=1 if final ID is Unknown (NOT RUC!)" // most likely passport number
assert is_ruc+is_ssn+is_unk==1

* Drop now useless variables and duplicate observations
drop id_sri_1 id_sri_2 is_id_sri_1_ruc id_sri_1_ruc_or_ssn
order id_sri is_resident is_final_consumer is_ruc is_ssn is_unk
gsort id_sri
gduplicates drop id_sri, force

* Merge in with differnt dataset to get maximum information possible and assign
* identity correctly
    * Firm registry
    preserve
        use $ecuRaw/characteristics/characteristicsOld.dta, clear
        replaceID id_sri // replaceIDs where possible using the above crosswalk and a custom function
        assert !missing(tipo_1)
        gen byte inc_firm = (tipo_1==2) // define firm indicators base on type (=1 natural person, =2 legal person)
        keep id_sri inc_firm
        gduplicates drop // new duplicates introduced by ID replacement (there should be no conflict, don't use 'force')
        gen match_firm = 1
        tempfile firm
        save `firm'
    restore
    merge 1:1 id_sri using `firm', nogen assert(master match)
    
    * Civil registry
    preserve
        use $ecuRaw/civilRegistry/cc_fakeid.dta, clear
        rename id id_sri
        replaceID id_sri // replaceIDs where possible using the above crosswalk and a custom function
        keep id_sri
        gduplicates drop // new duplicates introduced by ID replacement
        gen match_civil = 1
        tempfile civil
        save `civil'
    restore
    merge 1:1 id_sri using `civil', nogen assert(master match)
    
    * Employees list
    preserve
        use $ecuRaw/employment/IDs/ids_employees.dta, clear
        replaceID id_sri // replaceIDs where possible using the above crosswalk and a custom function
        keep id_sri
        gduplicates drop // new duplicates introduced by ID replacement
        gen match_employees = 1
        tempfile employees
        save `employees'
    restore
    merge 1:1 id_sri using `employees', nogen assert(master match)
    
    * Employers list
    preserve
        use $ecuRaw/employment/IDs/ids_employers.dta, clear
        replaceID id_sri // replaceIDs where possible using the above crosswalk and a custom function
        keep id_sri
        gduplicates drop // new duplicates introduced by ID replacement
        gen match_employers = 1
        tempfile employers
        save `employers'
    restore
    merge 1:1 id_sri using `employers', nogen assert(master match)
    
    * Buyers/Sellers list
    preserve
        use $ecuRaw/purchaseRecords/IDs/ids_buyers_sellers.dta, clear
        replaceID id_sri // replaceIDs where possible using the above crosswalk and a custom function
        keep id_sri
        gduplicates drop // new duplicates introduced by ID replacement
        gen match_transactions = 1
        tempfile transactions
        save `transactions'
    restore
    merge 1:1 id_sri using `transactions', nogen assert(master match)
    
    * F102 filers
    preserve
        use $ecuRaw/F102/IDs/ids_F102_filers.dta, clear
        replaceID id_sri // replaceIDs where possible using the above crosswalk and a custom function
        keep id_sri
        gduplicates drop // new duplicates introduced by ID replacement
        gen match_F102 = 1
        tempfile F102
        save `F102'
    restore
    merge 1:1 id_sri using `F102', nogen assert(master match)
    
    * F101 filers
    preserve
        use $ecuRaw/F101/IDs/ids_F101_filers.dta, clear
        replaceID id_sri // replaceIDs where possible using the above crosswalk and a custom function
        keep id_sri
        gduplicates drop // new duplicates introduced by ID replacement
        gen match_F101 = 1
        tempfile F101
        save `F101'
    restore
    qui sum id_sri
    local highest_id_sri = `r(max)'
    merge 1:1 id_sri using `F101', gen(mergeF101) // Here there will be 2017 IDs that are not present
    
    * Ensure that the unmatched from "F101 using" are indeed later IDs, assign as RUC IDs
    assert id_sri>`highest_id_sri' if mergeF101==2
    replace is_ruc = 1 if mergeF101==2
    replace is_ssn = 0 if mergeF101==2
    replace is_unk = 0 if mergeF101==2
    replace is_resident = 0 if mergeF101==2
    replace is_final_consumer = 0 if mergeF101==2
    drop mergeF101
    
    * Replace missing matches with zeros for all match variables
    foreach var of varlist match_* {
        replace `var' = 0 if missing(`var')
    }


* Fix is_ruc/is_ssn/is_unk
    * Firm registry is RUC based, so if it's matched there then it must hold is_ruc==1
    count if match_firm==1 & is_ruc==0 // 5 matches
    replace is_ruc = 1 if (match_firm==1 & is_ruc==0) // 5 replacements
    replace is_ssn = 0 if (is_ruc==1 & is_ssn==1) // 2 replacements
    replace is_unk = 0 if (is_ruc==1 & is_unk==1) // 3 replacements
    
    * Assert Unknown IDs are not controversial
    local match_both ((match_F101==1 | match_F102==1 | match_firm==1 | match_employers==1) & (match_civil==1 | match_employees==1))
    count if is_unk==1 & `match_both' // 10 cases
    di as input "`r(N)' unknown IDs are matched both to strict RUC-based datasets and civil registry/employees. Assume SSN based" 
    replace is_ruc = 1 if is_unk==1 & `match_both' // Set RUC to 1
    replace is_unk = 0 if is_unk==1 & `match_both' // Set UNK to 0
    
    * Fix Unknown IDs to be RUC if matched to strict RUC-based datasets
    count if is_unk==1 & (match_F101==1 | match_F102==1 | match_firm==1 | match_employers==1) // 12,513 cases
    replace is_ruc = 1 if is_unk==1 & (match_F101==1 | match_F102==1 | match_firm==1 | match_employers==1) // Set RUC to 1
    replace is_unk = 0 if is_unk==1 & (match_F101==1 | match_F102==1 | match_firm==1 | match_employers==1) // Set UNK to 0

    * Fix Unknown IDs to be SSN if matched to strict SSN-based datasets
    count if is_unk==1 & (match_civil==1 | match_employees==1) // 173,284 cases
    replace is_ssn = 1 if is_unk==1 & (match_civil==1 | match_employees==1) // Set SSN to 1
    replace is_unk = 0 if is_unk==1 & (match_civil==1 | match_employees==1) // Set UNK to 0
    
    * Assume that if Unknown appears in transactions data it's a RUC
    count if is_unk==1 & match_transactions==1 // 260,728 cases
    replace is_ruc = 1 if is_unk==1 & match_transactions==1 // Set RUC to 1
    replace is_unk = 0 if is_unk==1 & match_transactions==1 // Set UNK to 0
    
    * Count remaining Unknown, assert identities are still correct
    count if is_unk==1 // 574,983 remaining
    assert is_ruc+is_ssn+is_unk==1
    
    * Assert that the remaining unknowns are not matched anywhere
    foreach match of varlist match* {
        assert `match'==0 if is_unk==1
    }

* Create entity dummies
    * Persons
    gen is_person = 0
    replace is_person = 1 if match_civil==1 // if matched civil registry, surely a person
    replace is_person = 1 if match_employees==1 // if matched employees, surely a person
    replace is_person = 1 if is_ssn==1 // if has SSN ID, surely a person
    * Firms
    gen is_firm = 0
    replace is_firm = 1 if match_firm==1 // if matched firm registry, surely a firm/independent worker
    replace is_firm = 1 if match_F101==1 | match_F102==1 // if matched F10X, surely a firm/independent worker
    replace is_firm = 1 if match_employers==1 // if matched employers, surely a firm/independent worker
    replace is_firm = 1 if match_transactions==1 // if matched transactions, surely a firm/independent worker
    replace is_firm = 1 if is_ruc==1 // if has RUC ID, surely a firm/independent worker
    * Unknowns
    assert is_unk==1 if is_person==0 & is_firm==0 // if no firm nor person, must be unknown
    
* Generate entity
gen     entity = ""
replace entity = "Company"              if is_firm==1 & is_person==0
replace entity = "Sole proprietorship"  if is_firm==1 & is_person==1
replace entity = "Person"               if is_firm==0 & is_person==1
replace entity = "Unknown"              if is_firm==0 & is_person==0
drop if is_final_consumer==1 // not needed, I don't use sales annexes

* Tabulate final entities
tab entity
tab entity if match_F101==1 | match_F102==1

* Save the final entities
keep  id_sri entity
gsort id_sri
compress
export delimited $pathEst/input/entities.csv, replace
