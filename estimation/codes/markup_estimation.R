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
#----               1 - LOAD DATA AND MERGE WITH SAMPLE OF FIRMS            ----
#///////////////////////////////////////////////////////////////////////////////

# Load panel of firms in our sample
load(paste0(pathEst, "input/panel.Rdata"))

# Load data about tax filings and firm information
df_firm_info   <- fread(file=paste0(pathCle, "output/firm_info.csv"), na.strings="")
df_tax_filings <- fread(file=paste0(pathCle, "output/tax_filings.csv"), na.strings="")
df_wage_bills  <- fread(file=paste0(pathCle, "output/wage_bills.csv"), na.strings="")
df_exports     <- fread(file=paste0(pathCle, "output/exports.csv"), na.strings="")
df_imports     <- fread(file=paste0(pathCle, "output/imports.csv"), na.strings="")
df_interm_c    <- fread(file=paste0(pathCle, "output/intermediate_cost.csv"), na.strings="")
df_interm_r    <- fread(file=paste0(pathCle, "output/intermediate_revenue.csv"), na.strings="")
df_deflators   <- fread(file=paste0(pathCle, "output/deflators.csv"), na.strings="")
df_isic_sec    <- fread(file=paste0(pathCle, "output/isic_codes_section.csv"), na.strings="")
df_isic_div    <- fread(file=paste0(pathCle, "output/isic_codes_division.csv"), na.strings="")
df_isic_gro    <- fread(file=paste0(pathCle, "output/isic_codes_group.csv"), na.strings="")
df_isic_cla    <- fread(file=paste0(pathCle, "output/isic_codes_class.csv"), na.strings="")





# Merge time-less data sources from firm registry
firm_sample <- merge.data.table(firm_sample, df_firm_info[,.(id_sri, startyear)], by="id_sri", all.x=T)

# Merge yearly data sources
panel <- merge.data.table(panel, df_tax_filings, by=c("year", "id_sri"), all.x=T)
panel <- merge.data.table(panel, df_wage_bills,  by=c("year", "id_sri"), all.x=T)
panel <- merge.data.table(panel, df_exports,     by=c("year", "id_sri"), all.x=T)
panel <- merge.data.table(panel, df_imports,     by=c("year", "id_sri"), all.x=T)
setorder(panel, cols ="year","id_sri")

# Merge intermediate costs and revenues to the panel
panel <- merge.data.table(panel, df_cost_intermediate, by=c("id_sri","year"), all.x=T)
panel <- merge.data.table(panel, df_revenue_intermediate, by=c("id_sri","year"), all.x=T)

#### ACCOUNTING

# Generate disinvestment as: accum_depr_t1 - accum_depr_t0 + depr_t1
# Balances:
# accum_depr_t0 = -50
# accum_depr_t1 = -100
# Example:   0 disinvestment and 50 depreciation. Disinvestment = -100 - (-50) + 50 =   0 (correct!)
# Balances:
# accum_depr_t0 = -250
# accum_depr_t1 = -100
# Example: 150 disinvestment and  0 depreciation. Disinvestment = -100 - (-250) +  0 = 150 (correct!)
# Example: 185 disinvestment and 35 depreciation. Disinvestment = -100 - (-250) + 35 = 185 (correct!)

# Generate net investment (investment - disinvestment) as: asset_fix_tot_t1 - asset_fix_tot_t0 + depr_t1
# Balances:
# asset_fix_tot_t0 = 200
# asset_fix_tot_t1 = 170
# Example: 0 investment, 0 disinvestment and 30 depreciation. Net investment = 170 - 200 + 30 =   0 (correct!)

# Generate investment as: net investment + disinvestment (both calculated above)
