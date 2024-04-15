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

# Load data
load(file = paste0(pathEst, "output/firm_markups_V.Rdata"))
load(file = paste0(pathEst, "output/firm_markups_ML.Rdata"))
load(file = paste0(pathEst, "output/network_metrics.Rdata"))
markups <- merge(markups_ML, markups_V, by=c("id","year","ind"))
markups <- markups[!is.na(V)] # exclude non-active observations
rm(markups_ML, markups_V)

# Generate variable for industry group
markups[, ind_group := ifelse(substr(ind,1,1) %in% c("A","B","C","D","E","F"), "AF", ifelse(substr(ind,1,1)=="G", "G", "HQ"))]

# Match markups estimation sample and network sample
match <- merge(markups, network_metrics, by.x=c("id","year","ind"), by.y=c("id_seller","year","seller_sec"))

# Adjust markups and network measure to be percent deviations from mean/median, for ease of interpretation
match[, wsi_i  := (wsi_i  - mean(wsi_i))/mean(wsi_i),   by = c("year", "ind")]
match[, wsi_iw := (wsi_iw - mean(wsi_iw))/mean(wsi_iw), by = c("year", "ind")]
match[, ti_i   := (ti_i   - mean(ti_i))/mean(ti_i),     by = c("year", "ind")]
match[, ti_iw  := (ti_iw  - mean(ti_iw))/mean(ti_iw),   by = c("year", "ind")]
match[, sr_i   := (sr_i   - mean(sr_i))/mean(sr_i),     by = c("year", "ind")]
match[, sr_iw  := (sr_iw  - mean(sr_iw))/mean(sr_iw),   by = c("year", "ind")]
match[, ctv_i  := (ctv_i  - mean(ctv_i))/mean(ctv_i),   by = c("year", "ind")]
match[, kc_i   := (kc_i   - mean(kc_i))/mean(kc_i),     by = c("year", "ind")]
match[, mu_v_dlw_cd  := (mu_v_dlw_cd  - mean(mu_v_dlw_cd))/mean(mu_v_dlw_cd),  by = c("year", "ind")]



#///////////////////////////////////////////////////////////////////////////////
#----                           1 - RUN REGRESSIONS                         ----
#///////////////////////////////////////////////////////////////////////////////

# Run regressions
reg01 <- feols(mu_v_dlw_cd ~ wsi_i | id + year, data=match, subset = match$ind_group=="AF", panel.id = c("id","year"))
reg02 <- feols(mu_v_dlw_cd ~ wsi_i | id + year, data=match, subset = match$ind_group=="G", panel.id = c("id","year"))
reg03 <- feols(mu_v_dlw_cd ~ wsi_i | id + year, data=match, subset = match$ind_group=="HQ", panel.id = c("id","year"))
reg04 <- feols(mu_v_dlw_cd ~ wsi_i | id + year, data=match, panel.id = c("id","year"))

share_AF <- reg01$nobs/reg04$nobs
share_G  <- reg02$nobs/reg04$nobs
share_HQ <- reg03$nobs/reg04$nobs

# Put results into table by industry group
sink(paste0(pathTab,sysdate,"_table_5_regression_results_muV_WSI.tex"))
cat("\\begin{table}[!htbp]\\centering \n")
cat("\\caption{\\label{tab:EstimatedResultsV} Regression Results} \n")
cat("\\begin{adjustbox}{width=\\columnwidth,center} \n")
cat("\\begin{tabular}{lcccc}")
cat("\\toprule \n")
cat(" & \\multicolumn{1}{c}{A01--F45} & \\multicolumn{1}{c}{G50--G52} & \\multicolumn{1}{c}{H55--Q99} & \\multicolumn{1}{c}{All industries}\\\\ \n")
cat("\\addlinespace \\hline \\addlinespace \n")
cat("$WSI_i$ & ", 
    fp(reg01$coefficients, 4), sign_stars(reg01$coeftable[, 4])," & ", 
    fp(reg02$coefficients, 4), sign_stars(reg02$coeftable[, 4])," & ", 
    fp(reg03$coefficients, 4), sign_stars(reg03$coeftable[, 4])," & ", 
    fp(reg04$coefficients, 4), sign_stars(reg04$coeftable[, 4])," \\\\ \n")
cat(" & ",        fpt(reg01$se, 4),          " & ", fpt(reg02$se, 4),          " & ", fpt(reg03$se, 4),          " & ", fpt(reg04$se, 4),          " \\\\ \n")
cat("\\addlinespace \\hline \\addlinespace \n")
cat("Firm FE & \\checkmark & \\checkmark & \\checkmark & \\checkmark \\\\ \n")
cat("Year FE & \\checkmark & \\checkmark & \\checkmark & \\checkmark \\\\ \n")
cat("Observations & ",reg01$nobs," & ",reg02$nobs," & ",reg03$nobs," & ",reg04$nobs," \\\\ \n")
cat("Share of observations & ", fpp(share_AF,1), " & ", fpp(share_G,1), " & ", fpp(share_HQ,1), " & 100\\% \\\\  \n")
cat("\\bottomrule \n")
cat("\\end{tabular} \n")
cat("\\end{adjustbox} \n")
cat("\\justify \\footnotesize \\emph{Notes:} This table reports results. Standard errors clustered at the firm-level reported in parentheses. Significance levels given by: *** p $<$ 0.001, ** p $<$ 0.01, * p $<$ 0.05. \n")
cat("\\end{table} \n")
sink()




# reg11 <- feols(mu_v_dlw_cd ~ wsi_i  | id + year, data=match, panel.id = c("id","year"))
# reg12 <- feols(mu_v_dlw_cd ~ wsi_iw | id + year, data=match, panel.id = c("id","year"))
# reg13 <- feols(mu_v_dlw_cd ~ ti_i   | id + year, data=match, panel.id = c("id","year"))
# reg14 <- feols(mu_v_dlw_cd ~ ti_iw  | id + year, data=match, panel.id = c("id","year"))
# reg15 <- feols(mu_v_dlw_cd ~ ctv_i  | id + year, data=match, panel.id = c("id","year"))
# reg16 <- feols(mu_v_dlw_cd ~ sr_i   | id + year, data=match, panel.id = c("id","year"))
# reg17 <- feols(mu_v_dlw_cd ~ sr_iw  | id + year, data=match, panel.id = c("id","year"))
# reg18 <- feols(mu_v_dlw_cd ~ kc_i   | id + year, data=match, panel.id = c("id","year"))
reg11 <- feols(mu_v_dlw_cd ~ wsi_i  | ind + year, data=match, panel.id = c("id","year"))
reg12 <- feols(mu_v_dlw_cd ~ wsi_iw | ind + year, data=match, panel.id = c("id","year"))
reg13 <- feols(mu_v_dlw_cd ~ ti_i   | ind + year, data=match, panel.id = c("id","year"))
reg14 <- feols(mu_v_dlw_cd ~ ti_iw  | ind + year, data=match, panel.id = c("id","year"))
reg15 <- feols(mu_v_dlw_cd ~ ctv_i  | ind + year, data=match, panel.id = c("id","year"))
reg16 <- feols(mu_v_dlw_cd ~ sr_i   | ind + year, data=match, panel.id = c("id","year"))
reg17 <- feols(mu_v_dlw_cd ~ sr_iw  | ind + year, data=match, panel.id = c("id","year"))
reg18 <- feols(mu_v_dlw_cd ~ kc_i   | ind + year, data=match, panel.id = c("id","year"))


# Put results into table by measure
sink(paste0(pathTab,sysdate,"_table_5_regression_results_muV.tex"))
cat("\\begin{table}[!htbp]\\centering \n")
cat("\\caption{\\label{tab:EstimatedResultsV} Regression Results} \n")
cat("\\begin{adjustbox}{width=\\columnwidth,center} \n")
cat("\\begin{tabular}{lcccccccc}")
cat("\\toprule \n")
cat(" & (1) & (2) & (3) & (4) & (5) & (6) & (7) & (8) \\\\ \n")
cat("\\addlinespace \\hline \\addlinespace \n")
cat("$WSI_i$   & ", fp(reg11$coefficients, 4), sign_stars(reg11$coeftable[, 4])," & & & & & & & \\\\ \n")
cat("          & ", fpt(reg11$se, 4)," & & & & & & & \\\\ \n")
cat("$WSI^W_i$ & & ", fp(reg12$coefficients, 4), sign_stars(reg12$coeftable[, 4])," & & & & & & \\\\ \n")
cat("          & & ", fpt(reg12$se, 4)," & & & & & & \\\\ \n")
cat("$TI_i$    & & & ", fp(reg13$coefficients, 4), sign_stars(reg13$coeftable[, 4])," & & & & & \\\\ \n")
cat("          & & & ", fpt(reg13$se, 4)," & & & & & \\\\ \n")
cat("$TI^W_i$  & & & & ", fp(reg14$coefficients, 4), sign_stars(reg14$coeftable[, 4])," & & & & \\\\ \n")
cat("          & & & & ", fpt(reg14$se, 4)," & & & & \\\\ \n")
cat("$CTV_i$   & & & & & ", fp(reg15$coefficients, 4), sign_stars(reg15$coeftable[, 4])," & & & \\\\ \n")
cat("          & & & & & ", fpt(reg15$se, 4)," & & & \\\\ \n")
cat("$SR_i$    & & & & & & ", fp(reg16$coefficients, 4), sign_stars(reg16$coeftable[, 4])," & & \\\\ \n")
cat("          & & & & & & ", fpt(reg16$se, 4)," & & \\\\ \n")
cat("$SR^W_i$  & & & & & & & ", fp(reg17$coefficients, 4), sign_stars(reg17$coeftable[, 4])," & \\\\ \n")
cat("          & & & & & & & ", fpt(reg17$se, 4)," & \\\\ \n")
cat("$KC_i$    & & & & & & & & ", fp(reg18$coefficients, 4), sign_stars(reg18$coeftable[, 4])," \\\\ \n")
cat("          & & & & & & & & ", fpt(reg18$se, 4)," \\\\ \n")
cat("\\addlinespace \\hline \\addlinespace \n")
# cat("Standard deviation & ",sd(match$wsi_i)," & ",sd(match$wsi_iw)," & ",sd(match$ti_i)," & ",sd(match$ti_iw)," & ",sd(match$ctv_i)," & ",sd(match$sr_i)," & ",sd(match$sr_iw)," & ",sd(match$kc_i)," \\\\ \n")
cat("Firm FE & \\checkmark & \\checkmark & \\checkmark & \\checkmark & \\checkmark & \\checkmark & \\checkmark & \\checkmark \\\\ \n")
cat("Year FE & \\checkmark & \\checkmark & \\checkmark & \\checkmark & \\checkmark & \\checkmark & \\checkmark & \\checkmark \\\\ \n")
cat("Observations & ",reg11$nobs," & ",reg12$nobs," & ",reg13$nobs," & ",reg14$nobs," & ",reg15$nobs," & ",reg16$nobs," & ",reg17$nobs," & ",reg18$nobs," \\\\ \n")
cat("\\bottomrule \n")
cat("\\end{tabular} \n")
cat("\\end{adjustbox} \n")
cat("\\justify \\footnotesize \\emph{Notes:} This table reports results. Standard errors clustered at the firm-level reported in parentheses. Significance levels given by: *** p $<$ 0.001, ** p $<$ 0.01, * p $<$ 0.05. \n")
cat("\\end{table} \n")
sink()


