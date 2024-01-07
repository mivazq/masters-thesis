////////////////////////////////////////////////////////////////////////////////
* File name:        setup.do
* Author:           Miguel Vázquez Vázquez
* Creation date:    20 November 2023
* Description:		This file installs packages, defines paths, loads functions
////////////////////////////////////////////////////////////////////////////////
**#                         1 - PACKAGES AND OPTIONS
////////////////////////////////////////////////////////////////////////////////

* Set version
version 18

* Install packages
foreach file in gtools ftools cfvars mdesc carryforward distinct {
	capture findfile `file'.ado
	if _rc == 601 {
        ssc install `file'
	}
}

* Clear all objects
clear all
cap log close _all
macro drop _all

* Set important options
set more off, permanently
set varabbrev off, permanently

* Set date
global sysdate : di %tdCCYY-NN-DD date(c(current_date),"DMY")							

////////////////////////////////////////////////////////////////////////////////
**#                             2 - PATHS
////////////////////////////////////////////////////////////////////////////////

* Main paths
global ecuRaw  "~/data/transactions_ecuador/1_rawdata"
global ecuAll  "~/data/transactions_ecuador/2_shared"
global ecuFCT  "$ecuAll/factorContentTrade"
global ecuMine "~/data/transactions_ecuador/3_mivazq"

* Set working directory
global pathWD "$ecuMine/Masters_Thesis"
cd $pathWD

* Project sub-folders
global pathCle "$pathWD/cleaning"
global pathEst "$pathWD/estimation"
global pathFun "$pathWD/functions"

* Output files
global pathFig "$pathWD/results/figures"
global pathTab "$pathWD/results/tables"
global pathItx "$pathWD/results/intext"

////////////////////////////////////////////////////////////////////////////////
**#                             3 - FUNCTIONS
////////////////////////////////////////////////////////////////////////////////

* Add path for custom ado files
adopath + $pathFun
