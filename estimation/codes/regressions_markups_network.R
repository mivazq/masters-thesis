#///////////////////////////////////////////////////////////////////////////////
# File name:		regressions_markups_network.R
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

load(file = paste0(pathEst, "output/firm_markups_V.Rdata"))
load(file = paste0(pathEst, "output/firm_markups_ML.Rdata"))
load(file = paste0(pathEst, "output/network_metrics.Rdata"))
markups <- merge(markups_ML, markups_V, by=c("id","year","ind"))
rm(markups_ML, markups_V)

match <- merge(markups, network_metrics, by.x=c("id","year"), by.y=c("id_seller","year"))
match <- match[substr(ind,1,1)=="G"]

summary(lm(mu_m_dlw_cd ~ wsi_i, data=match))
# summary(lm(mu_m_dlw_cd ~ wsi_iw, data=match))
summary(lm(mu_m_dlw_cd ~ ti_i, data=match))
# summary(lm(mu_m_dlw_cd ~ ti_iw, data=match))
# summary(lm(mu_m_dlw_cd ~ tv_i, data=match))
summary(lm(mu_m_dlw_cd ~ ctv_i, data=match))
summary(lm(mu_m_dlw_cd ~ sr_i, data=match))
# summary(lm(mu_m_dlw_cd ~ sr_iw, data=match))
summary(lm(mu_m_dlw_cd ~ KC_i, data=match))
