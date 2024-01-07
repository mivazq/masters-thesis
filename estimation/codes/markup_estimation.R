#///////////////////////////////////////////////////////////////////////////////
# File name:		markup_estimation.R
# Author:			Miguel Vázquez Vázquez
# Creation date:    20 November 2023
# Description:      This file estimates markups for all firms, year to year.
# Input:            
#                   $pathEst/input/panel.Rdata
#                   $pathCle/output/firm_info.csv
#                   $pathCle/output/tax_filings.csv
#                   $pathCle/output/wage_bills.csv
#                   $pathCle/output/exports.csv
#                   $pathCle/output/imports.csv
#                   $pathCle/output/cost_intermediate.csv
#                   $pathCle/output/revenue_intermediate.csv
#                   $pathCle/output/deflators.csv
#                   $pathCle/output/isic_codes_section.csv
#                   $pathCle/output/isic_codes_division.csv
#                   $pathCle/output/isic_codes_group.csv
#                   $pathCle/output/isic_codes_class.csv
# Output:           
#                   -
#///////////////////////////////////////////////////////////////////////////////
source('~/data/transactions_ecuador/3_mivazq/Masters_Thesis/setup.R')
#///////////////////////////////////////////////////////////////////////////////
#----             1 - LOAD SAMPLE PANEL AND MERGE OTHER SOURCES             ----
#///////////////////////////////////////////////////////////////////////////////

# Load panel of firms in our sample
load(paste0(pathEst, "input/panel.Rdata"))

# Load data about tax filings and firm information
# df_firm_info   <- fread(file=paste0(pathCle, "output/firm_info.csv"), na.strings="")
df_tax_filings <- fread(file=paste0(pathCle, "output/tax_filings.csv"), na.strings="")
df_wage_bills  <- fread(file=paste0(pathCle, "output/wage_bills.csv"), na.strings="")
df_exports     <- fread(file=paste0(pathCle, "output/exports.csv"), na.strings="")
df_imports     <- fread(file=paste0(pathCle, "output/imports.csv"), na.strings="")
df_interm_c    <- fread(file=paste0(pathCle, "output/intermediate_cost.csv"), na.strings="")
df_interm_r    <- fread(file=paste0(pathCle, "output/intermediate_revenue.csv"), na.strings="")
# df_deflators   <- fread(file=paste0(pathCle, "output/deflators.csv"), na.strings="")
# df_isic_sec    <- fread(file=paste0(pathCle, "output/isic_codes_section.csv"), na.strings="")
# df_isic_div    <- fread(file=paste0(pathCle, "output/isic_codes_division.csv"), na.strings="")
# df_isic_gro    <- fread(file=paste0(pathCle, "output/isic_codes_group.csv"), na.strings="")
# df_isic_cla    <- fread(file=paste0(pathCle, "output/isic_codes_class.csv"), na.strings="")

# In general, committing a filing mistake in the tax form is as likely is any
# cell as in any other. Thus, I will use "total" cells, and not try to recalculate 
# the total myself by summing cells as this might increase the probability of 
# including mistakes.

# Merge yearly data sources
panel <- merge.data.table(panel, df_tax_filings[,.(year,id_sri,asset_fix_total,revenue_total, cost_total, cost_labor)], by=c("year","id_sri"), all.x=T)
panel <- merge.data.table(panel, df_wage_bills,  by=c("year","id_sri"), all.x=T)
panel <- merge.data.table(panel, df_exports,     by=c("year","id_sri"), all.x=T)
panel <- merge.data.table(panel, df_imports,     by=c("year","id_sri"), all.x=T)
panel <- merge.data.table(panel, df_interm_c,    by=c("year","id_sri"), all.x=T)
panel <- merge.data.table(panel, df_interm_r,    by=c("year","id_sri"), all.x=T)
setorder(panel, "id_sri", "year")

# Set to missing external values when we labeled a firm as not active based on 
# tax filings
if ( nrow(panel[active==0 & !is.na(cost_total) & !is.na(revenue_total)]) != 0 ) {
    stop("Something is wrong. Tax form variables should already be missing for non-active firms!")
}
panel[active==0, c("wages", "exports", "imports", "cost_transactions", "revenue_transactions") := NA]

#///////////////////////////////////////////////////////////////////////////////
#----                           2 - INVESTMENTS                             ----
#///////////////////////////////////////////////////////////////////////////////

# First differences of change in fixed assets accumulated depreciation
panel[, asset_fix_acdep_lag := shift(.SD, type="lag"), .SDcols="asset_fix_acdep", by=id_sri]
panel[, d_asset_fix_acdep := asset_fix_acdep - asset_fix_acdep_lag]

# Unfortunately no info on investments/disinvestments. Compute net investment as
# the first difference of net fixed assets (account for current-period depreciation)

# How I think the tax form should be filled is that "cost_asset_fix_dep_produ"
# takes the value of the depreciation per se (which should be identical to the
# change in accumulated depreciation, barring disinvestments) whereas 
# "cost_asset_fix_dep_admin" should report the administrative costs of calculating/
# booking the depreciation and thus should not decrease the value of the assets.
# It seems though that only a small majority of filers filled the two fields 
# accordingly. I'll keep "produ" only when it matches roughly, "admin" when it 
# matches roughly. And a sum of both when it matches roughly. Roughly due to round.
# It seems that mostly the admin one or the sum is correct, as most cases where
# "produ" is right it's due to it being 0 and admin being 0 as well. Thus, 
# combined with the fact that "produ" is mostly 0 anyway, we will keep the sum 
# as correct when none of the values matches.
# TOO MUCH FUCKING DETAIL STO WAQSTING TIME
#         panel[, sum_dep := rowSums(.SD,na.rm=T), .SDcols = c("cost_asset_fix_dep_produ", 
#                                                              "cost_asset_fix_dep_admin")]
#         cat("Case 1:", nrow(panel[round(cost_asset_fix_dep_produ,-1)==round(-d_asset_fix_acdep,-1)]),"\n")
#         panel[round(cost_asset_fix_dep_produ,-1)==round(-d_asset_fix_acdep,-1), 
#               cost_asset_fix_dep := cost_asset_fix_dep_produ]
#         cat("Case 2:", nrow(panel[round(cost_asset_fix_dep_admin,-1)==round(-d_asset_fix_acdep,-1)]),"\n")
#         panel[round(cost_asset_fix_dep_admin,-1)==round(-d_asset_fix_acdep,-1), 
#               cost_asset_fix_dep := cost_asset_fix_dep_admin]
#         cat("Case 3:", nrow(panel[round(sum_dep,-1)==round(-d_asset_fix_acdep,-1)]),"\n")
#         panel[round(sum_dep,-1)==round(-d_asset_fix_acdep,-1), 
#               cost_asset_fix_dep := sum_dep]
#         panel[is.na(cost_asset_fix_dep) & !is.na(form), cost_asset_fix_dep := sum_dep]
#         panel[, c("cost_asset_fix_dep_produ", "cost_asset_fix_dep_admin", "sum_dep") := NULL]

# Now, given that the account "accumulated depreciation" can only move for two 
# reasons: # (1) add depreciation, (2) sell a fixed asset and remove it's acc. dep.,
# if we see that the difference in accumulated depreciation and the current-period
# depreciation don't overlap it implies there was disinvestment (we can't quantify it
# though, since we only see the depreciated part of the sold assets).
panel[, disinvested := ifelse(round(cost_asset_fix_dep,-1)==round(-d_asset_fix_acdep,-1), 0, 1)]

# Generate net investment (investment - disinvestment) as: asset_fix_tot_t1 - asset_fix_tot_t0 + depr_t1
panel[, asset_fix_total_lag := shift(.SD, type="lag"), .SDcols="asset_fix_total", by=id_sri]
panel[, net_investment := asset_fix_total - asset_fix_total_lag + cost_asset_fix_dep]





