#///////////////////////////////////////////////////////////////////////////////
# File name:		markup_estimation.R
# Author:			Miguel Vázquez Vázquez
# Creation date:    20 November 2023
# Description:      This file estimates markups for all firms, year to year.
# Input:            
#                   $pathEst/input/firm_info.csv
#                   $pathEst/input/tax_filings.csv
#                   $pathEst/input/purchase_annexes.csv
#                   $pathEst/input/isic_codes_section.csv
#                   $pathEst/input/isic_codes_division.csv
#                   $pathEst/input/isic_codes_group.csv
#                   $pathEst/input/isic_codes_class.csv
# Output:           
#                   -
#///////////////////////////////////////////////////////////////////////////////
source('~/data/transactions_ecuador/3_mivazq/Masters_Thesis/setup.R')
#///////////////////////////////////////////////////////////////////////////////
#----               1 - LOAD DATA AND MERGE WITH SAMPLE OF FIRMS            ----
#///////////////////////////////////////////////////////////////////////////////

# Load sample of firms
load(paste0(pathEst, "input/firm_sample.Rdata")) # load sample of firm IDs we are interested in

# Load data about tax filings and firm information
df_tax_filings <- fread(file=paste0(pathEst, "input/tax_filings.csv"), na.strings="")
df_firm_info   <- fread(file=paste0(pathEst, "input/firm_info.csv"), na.strings="")
df_wage_bills  <- fread(file=paste0(pathEst, "input/wage_bills.csv"), na.strings="")
df_exports <- fread(file=paste0(pathEst, "input/exports.csv"), na.strings="")
df_imports <- fread(file=paste0(pathEst, "input/imports.csv"), na.strings="")

# Convert string date to format date
df_firm_info[, startdate := as.Date(as.POSIXct(startdate, format = "%d%b%Y"))]
df_firm_info[, startyear := year(startdate)]

# Merge time-less data sources from firm registry
firm_sample <- merge.data.table(firm_sample, df_firm_info[,.(id_sri, startyear)], by="id_sri", all.x=T)


# Merge yearly data sources
panel <- merge.data.table(panel, df_tax_filings, by=c("year", "id_sri"), all.x=T)
panel <- merge.data.table(panel, df_wage_bills,  by=c("year", "id_sri"), all.x=T)
panel <- merge.data.table(panel, df_exports,     by=c("year", "id_sri"), all.x=T)
panel <- merge.data.table(panel, df_imports,     by=c("year", "id_sri"), all.x=T)
setorder(panel, cols ="year","id_sri")


# Load purchase annexes
df_cost_intermediate <- fread(file=paste0(pathEst, "input/cost_intermediate.csv"),
                              na.strings="")
df_revenue_intermediate <- fread(file=paste0(pathEst, "input/revenue_intermediate.csv"),
                                 na.strings="")

# Merge intermediate costs and revenues to the panel
panel <- merge.data.table(panel, df_cost_intermediate, by=c("id_sri","year"), all.x=T)
panel <- merge.data.table(panel, df_revenue_intermediate, by=c("id_sri","year"), all.x=T)






df_deflators <- fread(file=paste0(pathEst, "input/deflators.csv"), na.strings="")







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






#///////////////////////////////////////////////////////////////////////////////
#----                       2 - SECTION 2                      ----
#///////////////////////////////////////////////////////////////////////////////

df_isic_sections    <- fread(file=paste0(pathEst, "input/isic_codes_section.csv"),
                             na.strings="")
df_isic_divisions   <- fread(file=paste0(pathEst, "input/isic_codes_division.csv"),
                             na.strings="")
df_isic_groups      <- fread(file=paste0(pathEst, "input/isic_codes_group.csv"),
                             na.strings="")
df_isic_classes     <- fread(file=paste0(pathEst, "input/isic_codes_class.csv"),
                             na.strings="")

