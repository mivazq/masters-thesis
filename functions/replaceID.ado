////////////////////////////////////////////////////////////////////////////////
* File name:        clean_id_sri.do
* Author:           Miguel Vázquez Vázquez
* Creation date:    20 November 2023
* Description:		This function unifies the IDs given by SRI by always using 
*                   the RUC ID instead of the SSN ID for entities which have 
*                   provided both interchangeably.
* Input:
*                   $pathFun/replaceID.dta
*                   Current dataset when running function
* Output:
*                   Current dataset when running function but with updated IDs
////////////////////////////////////////////////////////////////////////////////

program define replaceID
	syntax varlist [if] [in] [,]
    
	foreach id_entity in `varlist'{
        
        * Rename variable to match with IDs dataset
		rename `id_entity' id_sri_unique
        
        * Perform merge
		qui merge m:1 id_sri_unique using $pathFun/replaceID.dta, ///
        keep(master match) keepusing(id_sri) nogen
        
        * Replace the SSN id_sri variable in the current dataset with the RUC one
        di as result "When available, replace SSN IDs with RUC IDs for variable `id_entity'"
		replace id_sri_unique = id_sri if !missing(id_sri)
        
        * Drop merged variables and revert variable name changes
		drop id_sri
		rename id_sri_unique `id_entity'
	}
    
end
