#///////////////////////////////////////////////////////////////////////////////
# File name:		dotheycorrelate.R
# Author:			Miguel Vázquez Vázquez
# Creation date:    19 March 2023
# Description:      This file checks whether markups correlate with different 
#                   network metrics.
# Input:            
#                   -
# Output:           
#                   -
#///////////////////////////////////////////////////////////////////////////////
source('~/data/transactions_ecuador/3_mivazq/Masters_Thesis/setup.R')
#///////////////////////////////////////////////////////////////////////////////
#----                       1 - LOAD AND PREPARE DATA                       ----
#///////////////////////////////////////////////////////////////////////////////

markups <- fread(paste0(pathEst, "output/markups_2008.csv"))
load(file = paste0(pathEst,'output/network_metrics.Rdata'))


match <- merge(markups, network_metrics[year==2008], by.x="id", by.y="id_seller")

summary(lm(mu_dlwcd ~ wsi_i, data=match))
summary(lm(mu_dlwcd ~ wsi_iw, data=match))
summary(lm(mu_dlwcd ~ ti_i, data=match))
summary(lm(mu_dlwcd ~ ti_iw, data=match))
summary(lm(mu_dlwcd ~ tv_i, data=match))
summary(lm(mu_dlwcd ~ ctv_i, data=match))
summary(lm(mu_dlwcd ~ sr_i, data=match))
summary(lm(mu_dlwcd ~ sr_iw, data=match))
summary(lm(mu_dlwcd ~ KC_i, data=match))
