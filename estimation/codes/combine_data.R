#///////////////////////////////////////////////////////////////////////////////
# File name:		combine_data.R
# Author:			Miguel Vázquez Vázquez
# Creation date:    20 November 2023
# Description:      This file combines data from different source the create the 
#                   final panel which will be used for markup estimation.
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
panel <- merge.data.table(panel, df_tax_filings, by=c("year","id_sri"), all.x=T) #[,.(year,id_sri,asset_fix_total,revenue_total, cost_total, cost_labor)]
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
panel[active==0, c("wages", "wages_adj", "exports", "imports", "fees", "cost_transactions", "revenue_transactions") := NA]

# For now, simply exclude all the firms which in theory should be active but in
# practice did not file a tax form in at least one of their active years
further_drop <- unique(panel[active==1 & is.na(cost_total)]$id_sri) # lose around 45K firms out of 140K
panel <- panel[id_sri %nin% further_drop]

# Drop variables I don't need
panel[, c("missing_year_sample","form") := NULL]

#///////////////////////////////////////////////////////////////////////////////
#----                           2 - FIXED ASSETS                            ----
#///////////////////////////////////////////////////////////////////////////////

# TOTAL FIXED ASSETS
    # In the cases where the total assets variable is reported as zero but by summing
    # the single components we get a positive number, we will consider this instead of 0
    panel[, sum_asset_fix := rowSums(.SD,na.rm=T), 
          .SDcols = c("asset_fix_prope", "asset_fix_ships", "asset_fix_furni", 
                      "asset_fix_machi", "asset_fix_compu", "asset_fix_vehic", 
                      "asset_fix_other", "asset_fix_lands", "asset_fix_unfin")]
    panel[, sum_asset_fix := sum_asset_fix - asset_fix_acdep] # subtract accumulated depreciation
    cat("This affects",nrow(panel[asset_fix_total==0 & sum_asset_fix>0]),"observations\n")
    panel[asset_fix_total==0 & sum_asset_fix>0, asset_fix_total := 0]
    
    # Also, it seems that sometimes firms don't report their assets since it's hard
    # to believe that a firm who operated in previous years with assets can operate 
    # without assets. Thus, if the assets of a given year are 0 and were >0 in the 
    # previous I will carry forward the value. I need to be careful with the imputation 
    # because often times firms will have 0 assets in their exit year, which makes sense.
    # I repeat this exercise, for non-exiting firms, 3 times (2008->2009, 2009->2010, 2010->2011)
    for (i in seq(1,3,1)) {
        panel[, asset_fix_total_lag := shift(.SD, type="lag"), .SDcols="asset_fix_total", by=id_sri]
        cat("In iteration",i,"this affects",nrow(panel[asset_fix_total==0 & asset_fix_total_lag!=0 & exit==0]),"observations\n")
        panel[asset_fix_total==0 & asset_fix_total_lag!=0 & exit==0, asset_fix_total := asset_fix_total_lag]
    }
    rm(i)
    
    # Recompute one last time, for entering firms we can set the lag to 0
    panel[, asset_fix_total_lag := shift(.SD, type="lag"), .SDcols="asset_fix_total", by=id_sri]
    panel[entry==1, asset_fix_total_lag := 0]


# CURRENT PERIOD DEPRECIATION
    # First construct the change in fixed assets accumulated depreciation. Similarly
    # to above, I'll carry forward values when it makes sense.
    for (i in seq(1,3,1)) {
        panel[, asset_fix_acdep_lag := shift(.SD, type="lag"), .SDcols="asset_fix_acdep", by=id_sri]
        cat("In iteration",i,"this affects",nrow(panel[asset_fix_acdep==0 & asset_fix_acdep_lag!=0 & exit==0]),"observations\n")
        panel[asset_fix_acdep==0 & asset_fix_acdep_lag!=0 & exit==0, asset_fix_acdep := asset_fix_acdep_lag]
    }
    rm(i)
    
    # For entering firms we can set the lag to 0
    panel[entry==1, asset_fix_acdep_lag := 0]
    
    # Construct first difference in fixed assets accumulated depreciation
    panel[, d_asset_fix_acdep := asset_fix_acdep - asset_fix_acdep_lag]
    
    # How I think the tax form should be filled is that "cost_asset_fix_dep_produ"
    # takes the value of the depreciation per se (which should be identical to the
    # change in accumulated depreciation, barring disinvestments) whereas
    # "cost_asset_fix_dep_admin" should report the administrative costs of calculating/
    # booking the depreciation and thus should not decrease the value of the assets.
    # It seems though that only a small minority of filers filled the two fields
    # accordingly. First I'll see if any of "produ", "admin" or "sum" matches perfectly.
    panel[, sum_dep := rowSums(.SD,na.rm=T), .SDcols = c("cost_asset_fix_dep_produ",
                                                         "cost_asset_fix_dep_admin")]
    cat("Case 1.1 - Exact match \"both\"  and change in acc. dep.:", nrow(panel[d_asset_fix_acdep==sum_dep]),"observations\n")
    cat("Case 1.2 - Exact match \"admin\" and change in acc. dep.:", nrow(panel[d_asset_fix_acdep==cost_asset_fix_dep_admin]),"observations\n")
    cat("Case 1.3 - Exact match \"produ\" and change in acc. dep.:", nrow(panel[d_asset_fix_acdep==cost_asset_fix_dep_produ]),"observations\n")
    panel[d_asset_fix_acdep==sum_dep,                  cost_asset_fix_dep := sum_dep]
    panel[d_asset_fix_acdep==cost_asset_fix_dep_admin, cost_asset_fix_dep := cost_asset_fix_dep_admin]
    panel[d_asset_fix_acdep==cost_asset_fix_dep_produ, cost_asset_fix_dep := cost_asset_fix_dep_produ]
    
    # For the remaining cases I'll use rough matching since there was rounding involved.
    cat("Case 2.1 - Rough match \"both\"  and change in acc. dep.:", nrow(panel[is.na(cost_asset_fix_dep) & round(d_asset_fix_acdep,-1)==round(sum_dep,-1)]),"\n")
    cat("Case 2.2 - Rough match \"admin\" and change in acc. dep.:", nrow(panel[is.na(cost_asset_fix_dep) & round(d_asset_fix_acdep,-1)==round(cost_asset_fix_dep_admin,-1)]),"\n")
    cat("Case 2.3 - Rough match \"produ\" and change in acc. dep.:", nrow(panel[is.na(cost_asset_fix_dep) & round(d_asset_fix_acdep,-1)==round(cost_asset_fix_dep_produ,-1)]),"\n")
    panel[is.na(cost_asset_fix_dep) & round(d_asset_fix_acdep,-1)==round(sum_dep,-1),                  cost_asset_fix_dep := sum_dep]
    panel[is.na(cost_asset_fix_dep) & round(d_asset_fix_acdep,-1)==round(cost_asset_fix_dep_admin,-1), cost_asset_fix_dep := cost_asset_fix_dep_admin]
    panel[is.na(cost_asset_fix_dep) & round(d_asset_fix_acdep,-1)==round(cost_asset_fix_dep_produ,-1), cost_asset_fix_dep := cost_asset_fix_dep_produ]
    
    # It seems that mostly the admin one or the sum is correct, as most cases where
    # "produ" is right it's due to it being 0 and admin being 0 as well. Thus,
    # combined with the fact that "produ" is mostly 0 anyway, we will keep the sum
    # as correct when none of the values matches.
    cat("Case 3 - No match at all:", nrow(panel[is.na(cost_asset_fix_dep) & !is.na(cost_total)]),"\n")
    panel[is.na(cost_asset_fix_dep) & !is.na(cost_total), cost_asset_fix_dep := sum_dep]


# NET INVESTMENTS
    # Finally I construct the net investments by first differencing changes in 
    # total fixed assets and adding the current period depreciations (which 
    # would otherwise be implicitly considered a disinvestment)
    panel[, net_investment := asset_fix_total - asset_fix_total_lag + cost_asset_fix_dep]
    
    # Check distribution of investments for entering, exiting, inside firms
    summary(panel[entry==1]$net_investment)
    summary(panel[exit==1 ]$net_investment)
    summary(panel[entry==0 & exit==0]$net_investment)
    
    # Drop fixed assets single accounts
    panel[, c("cost_asset_fix_dep_produ", "cost_asset_fix_dep_admin", "sum_dep",
              "asset_fix_prope", "asset_fix_ships", "asset_fix_furni", 
              "asset_fix_machi", "asset_fix_compu", "asset_fix_vehic", 
              "asset_fix_other", "asset_fix_lands", "asset_fix_unfin",
              "asset_fix_acdep", "asset_fix_acdep_lag", "d_asset_fix_acdep", 
              "sum_asset_fix") := NULL]
    
    # Move columns relating to assets to the front
    front_names <- c("year", "id_sri", "entity", "isic_section", "active", "entry", 
                     "exit", "asset_fix_total", "asset_fix_total_lag", "net_investment")
    setcolorder(panel, front_names) # the non-mentioned columns will be after
    rm(front_names)
    
    # Set lag variables to NA for non-active entries 
    panel[active==0, c("asset_fix_total_lag") := NA]


#///////////////////////////////////////////////////////////////////////////////
#----                           3 - EXPORTS & IMPORTS                       ----
#///////////////////////////////////////////////////////////////////////////////

# IMPORTS
    # First fill missings with zeros for active observations
    panel[active==1 & is.na(imports), imports := 0]
    panel[active==1 & is.na(fees), fees := 0]
    panel[, import_n_fees := imports+fees]
    
    # Compare reported imports in tax filings with the customs data
    panel[, cost_imports_goods := cost_imports_goods_produ + cost_imports_goods_admin + cost_imports_rawmat]
    panel[, cost_imports_services := cost_imports_services_produ + cost_imports_services_admin]
    panel[, cost_imports_all := cost_imports_goods + cost_imports_services]
    
    # Look at distributions of different variables
    summary(panel$cost_imports_all)
    summary(panel$cost_imports_goods)
    summary(panel$cost_imports_services)
    summary(panel$imports)
    summary(panel$import_n_fees)
    table(panel$cost_imports_goods>0, panel$imports>0)
    
    # Imports from customs are only for HS products (so services are not included),
    # and it seems that the distributions of imported goods from tax filing and 
    # the ones from customs (without fees) are the most similar. Thus, I will use 
    # information from customs (without fees) when this is available (as I deem it
    # as the most reliable), and use imported goods from tax filings when customs
    # report zero. At the end, I add imported services on top of it.
    panel[, cost_imports := imports]
    panel[cost_imports==0 & cost_imports_goods>0, cost_imports := cost_imports_goods]
    panel[, cost_imports := cost_imports + cost_imports_services]
    
    # Delete no longer needed variables
    panel[, c("cost_imports_goods",    "cost_imports_goods_admin",    "cost_imports_goods_produ",
              "cost_imports_services", "cost_imports_services_admin", "cost_imports_services_produ",
              "cost_imports_rawmat",   "cost_imports_all", "imports", "fees", "import_n_fees") := NULL]


# EXPORTS
    # First fill missings with zeros for active observations
    panel[active==1 & is.na(exports), exports := 0]
    
    # Compare reported exports in tax filings with the customs data
    panel[, revenue_exports_all := revenue_export + revenue_other_abroad]
    
    # Look at distributions of different variables
    summary(panel$revenue_export)
    summary(panel$revenue_other_abroad)
    summary(panel$revenue_exports_all)
    summary(panel$exports)
    table(panel$revenue_export>0, panel$exports>0)
    
    # Exports from customs are only for HS products (so services are not included),
    # and it seems that the distributions of exported goods from tax filing and 
    # the ones from customs are the most similar. Thus, I will use information 
    # from customs when this is available (as I deem it as the most reliable), 
    # and use exported goods from tax filings when customs report zero. At the
    # end, I add other revenue from abroad (probably services) on top of it.
    panel[, revenue_exports := exports]
    panel[revenue_exports==0 & revenue_export>0, revenue_exports := revenue_export]
    panel[, revenue_exports := revenue_exports + revenue_other_abroad]
    
    # Delete no longer needed variables
    panel[, c("revenue_export", "revenue_other_abroad", "revenue_exports_all", "exports") := NULL]


#///////////////////////////////////////////////////////////////////////////////
#----                           4 - PURCHASES                               ----
#///////////////////////////////////////////////////////////////////////////////

# COSTS
    # First fill missings with zeros for active observations
    panel[active==1 & is.na(cost_transactions), cost_transactions := 0]

# REVENUES
    # First fill missings with zeros for active observations
    panel[active==1 & is.na(revenue_transactions), revenue_transactions := 0]


#///////////////////////////////////////////////////////////////////////////////
#----                               5 - WAGES                               ----
#///////////////////////////////////////////////////////////////////////////////

# First fill missings with zeros for active observations
panel[active==1 & is.na(wages),     wages := 0]
panel[active==1 & is.na(wages_adj), wages_adj := 0]

# Look at distributions of different variables
summary(panel$cost_labor)
summary(panel$wages)
summary(panel$wages_adj)
table(panel$cost_labor>0, panel$wages>0)

# Take wages from SS data when available, use those from tax filings when zero
panel[, cost_employees := wages]
panel[cost_employees==0 & cost_labor>0, cost_employees := cost_labor]

# Delete no longer needed variables and reorder
panel[, c("cost_labor", "wages", "wages_adj") := NULL]

# There is a lot of firms which seem to never have cost of labour, which is 
# impossible. I further exclude these firms firm the panel.
panel[, total_cost_labour := sum(cost_employees), by=id_sri]
further_drop <- unique(panel[total_cost_labour==0]$id_sri)
panel <- panel[id_sri %nin% further_drop]

# For the remaining cases, I fill the zeros using the firm-specific mean wage 
# ratio over total costs. As first source for total cost I use the sum of the
# costs from external sources. As second the total cost reported. As third a 
# reconstructed version of the reported total cost.
panel[, new_cost_total := cost_employees + cost_transactions + cost_asset_fix_dep + cost_imports]
panel[                    cost_employees!=0 & new_cost_total!=0, wage_ratio := cost_employees/new_cost_total]
panel[is.na(wage_ratio) & cost_employees!=0 & cost_total!=0,     wage_ratio := cost_employees/cost_total]
panel[cost_total!=0, rev_to_cost := revenue_total/cost_total, by=id_sri]
panel[, mean_rev_to_cost := mean(rev_to_cost, na.rm=T), by=id_sri]
panel[, reconstr_cost_total := revenue_total/avg_rev_to_cost]




panel[, mean_wage_ratio := mean(wage_ratio,na.rm=T), by=id_sri]
panel[active==1 & is.na(mean_wage_ratio), mean_wage_ratio := 0]
panel[cost_employees==0, cost_employees := (cost_employees + cost_transactions + cost_asset_fix_dep + cost_imports)*mean_wage_ratio]

# There are still some few cases where the cost of labour is zero if a firm in 
# a given year appears to not have purchased anything nor depreciated. In these
# cases I will use the "cost_total" variable from the tax filing to compute the
# ratios instead
panel[cost_employees==0, cost_employees := (cost_total)*mean_wage_ratio]

# For the final remaining cases (when cost_total is also 0) I will impute it by 
# first reconstructing cost_total and then repeating the above
panel[cost_employees==0, cost_employees := revenue_total/avg_rev_to_cost*mean_wage_ratio]




