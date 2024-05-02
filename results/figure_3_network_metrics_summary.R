#///////////////////////////////////////////////////////////////////////////////
# File name:		figure_3_network_metrics_summary.R
# Author:			Miguel Vázquez Vázquez
# Creation date:    1 May 2024
# Description:      
#
# Input:            
#                   -

# Output:           
#                   -
#///////////////////////////////////////////////////////////////////////////////
source('~/data/transactions_ecuador/3_mivazq/Masters_Thesis/setup.R')
#///////////////////////////////////////////////////////////////////////////////

#///////////////////////////////////////////////////////////////////////////////
#### PREPARE DATA ####
#///////////////////////////////////////////////////////////////////////////////

library(PerformanceAnalytics)

# Load data and merge
load(file=paste0(pathEst, "output/firm_markups_V.Rdata"))
load(file=paste0(pathEst, "output/firm_markups_ML.Rdata"))
markups <- merge(markups_ML, markups_V, by=c("id","year","ind"))
markups <- markups[!is.na(mu_v_dlw_cd)]
rm(markups_ML, markups_V)
load(file=paste0(pathEst, "output/network_metrics.Rdata"))

# Merge
network_metrics <- merge(network_metrics, markups[, .(id_seller=id, year, match = 1)], by=c("id_seller","year"), all.x = T)
unique_ids_all_years <- unique(markups$id)
rm(markups)
network_metrics[is.na(match) & id_seller %in% unique_ids_all_years, match := 0] # label id-year pairs that we don't estimate markups for
network_metrics <- network_metrics[!is.na(match)]

# Generate variable for industry group
network_metrics[, ind_group := ifelse(substr(seller_sec,1,1) %in% c("A","B","C","D","E","F"), "AF", ifelse(substr(seller_sec,1,1)=="G", "G", "HQ"))]

# Check correlations
# cor(network_metrics[match==1, .SD, .SDcols = !c("id_seller", "year", "seller_sec", "ind_group", "match")])
chart.Correlation(network_metrics[match==1, .SD, .SDcols = !c("id_seller", "year", "seller_sec", "ind_group", "match")],
                  method="pearson",
                  histogram=TRUE,
                  pch=16)



