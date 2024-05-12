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

# Load data and merge
load(file=paste0(pathEst, "output/firm_markups_V.Rdata"))
load(file=paste0(pathEst, "output/firm_markups_ML.Rdata"))
markups <- merge(markups_ML, markups_V, by=c("id","year","ind"))
rm(markups_ML, markups_V)
load(file=paste0(pathEst, "output/network_metrics.Rdata"))

# Generate variable for industry group
markups[, ind_group := ifelse(substr(ind,1,1) %in% c("A","B","C","D","E","F"), "AF", ifelse(substr(ind,1,1)=="G", "G", "HQ"))]

# Drop OLS markups, L markups, and missing markups
markups <- markups[, .SD, .SDcols = !(grep("_ols_", colnames(markups)))]
markups <- markups[, .SD, .SDcols = !(grep("_l_", colnames(markups)))]
markups <- markups[!is.na(mu_v_dlw_cd)] # exclude missing obs

# Create dummy for firm size expansion (based on cost)
markups[, V_lag := shift(V), by="id"]
markups[, firm_size_expansion := ifelse(V>V_lag, 1, 0)]


# Since we are only focused on are main specification, drop all other variables
markups[, c("mu_m_dlw_cd", "mu_m_dlw_tl", "mu_v_dlw_tl", "M", "L", "V", "V_lag", "alpha_v") := NULL]
setnames(markups, "mu_v_dlw_cd", "mu")

                    # Fix markups of industries H55 and O91 in baseline spec
                    medgroup = median(markups[ind_group=="HQ" & ind %nin% c("H55","O91")]$mu) # median for industry group H55-Q99 (excluding these two industries)
                    fix_H55 = medgroup - median(markups[ind=="H55"]$mu) # median deviation from group median
                    fix_O91 = medgroup - median(markups[ind=="O91"]$mu) # median deviation from group median
                    markups[ind=="H55", mu := mu + fix_H55] # fix H55
                    markups[ind=="O91", mu := mu + fix_O91] # fix O91

# Match markups estimation sample and network sample
match <- merge(markups, network_metrics, by.x=c("id","year","ind"), by.y=c("id_seller","year","seller_sec"))

# Calculate IQR for network metrics
IQR <- dcast(data = match[,.SD,.SDcols = !c("id", "year", "ind", "ind_group")],
             formula = .~.,
             fun = IQR,
             value.var=colnames(match[,.SD,.SDcols = !c("id", "year", "ind", "ind_group")]))


#///////////////////////////////////////////////////////////////////////////////
#----                 2 - RUN REGRESSIONS ON IMPORTANCE                ----
#///////////////////////////////////////////////////////////////////////////////

# Run regressions for network variables relating to idea of diversification
reg_imp_1 <- feols(mu ~ wsvi_i_c | ind + year, data=match, panel.id = c("id","year"), cluster = "id")
reg_imp_2 <- feols(mu ~ wsvi_i_w | ind + year, data=match, panel.id = c("id","year"), cluster = "id")
reg_imp_3 <- feols(mu ~ bvi_i_c  | ind + year, data=match, panel.id = c("id","year"), cluster = "id")
reg_imp_4 <- feols(mu ~ bvi_i_w  | ind + year, data=match, panel.id = c("id","year"), cluster = "id")
# reg_imp_5 <- feols(mu ~ wsfi_i_c | ind + year, data=match, panel.id = c("id","year"), cluster = "id")
# reg_imp_6 <- feols(mu ~ wsfi_i_w | ind + year, data=match, panel.id = c("id","year"), cluster = "id")
# reg_imp_7 <- feols(mu ~ bfi_i_c  | ind + year, data=match, panel.id = c("id","year"), cluster = "id")
# reg_imp_8 <- feols(mu ~ bfi_i_w  | ind + year, data=match, panel.id = c("id","year"), cluster = "id")
reg_imp_5 <- feols(mu ~ wsvi_i_u | ind + year, data=match, panel.id = c("id","year"), cluster = "id")
reg_imp_6 <- feols(mu ~ bvi_i_u  | ind + year, data=match, panel.id = c("id","year"), cluster = "id")
reg_imp_7 <- feols(mu ~ wsvi_i_c | ind + year, data=match, weights = match$seller_weight, panel.id = c("id","year"), cluster = "id")
reg_imp_8 <- feols(mu ~ bvi_i_c  | ind + year, data=match, weights = match$seller_weight, panel.id = c("id","year"), cluster = "id")

sink(paste0(pathTab,sysdate,"_table_6_regression_results_importance.tex"))
cat("\\begin{table}[!htbp]\\centering \n")
cat("\\caption{\\label{tab:EstimatedResults_Importance} Regression Results Importance} \n")
cat("\\begin{adjustbox}{width=\\columnwidth,center} \n")
cat("\\begin{tabular}{lcccccccc}")
cat("\\toprule \n")
cat(" & (1) & (2) & (3) & (4) & (5) & (6) & (7) & (8) \\\\ \n")
cat("\\addlinespace \\hline \\addlinespace \n")
cat("Within Sector Value Importance (\\%)     & ", fp(reg_imp_1$coefficients, 4), sign_stars(reg_imp_1$coeftable[, 4])," & ",fp(reg_imp_2$coefficients, 4), sign_stars(reg_imp_2$coeftable[, 4])," & & & & & & \\\\ \n")
cat("                                         & ", fpt(reg_imp_1$se, 4)," & ",fpt(reg_imp_2$se, 4)," & & & & & & \\\\ \n")
cat("Broad Value Importance (\\%)             & & & ", fp(reg_imp_3$coefficients, 4), sign_stars(reg_imp_3$coeftable[, 4])," & ",fp(reg_imp_4$coefficients, 4), sign_stars(reg_imp_4$coeftable[, 4])," & & & & \\\\ \n")
cat("                                         & & & ", fpt(reg_imp_3$se, 4)," & ",fpt(reg_imp_4$se, 4)," & & & & \\\\ \n")
cat("Within Sector Frequency Importance (\\%) & & & & & ", fp(reg_imp_5$coefficients, 4), sign_stars(reg_imp_5$coeftable[, 4])," & ",fp(reg_imp_6$coefficients, 4), sign_stars(reg_imp_6$coeftable[, 4])," & & \\\\ \n")
cat("                                         & & & & & ", fpt(reg_imp_5$se, 4)," & ",fpt(reg_imp_6$se, 4)," & & \\\\ \n")
cat("Broad Frequency Importance (\\%)         & & & & & & & ", fp(reg_imp_7$coefficients, 4), sign_stars(reg_imp_7$coeftable[, 4])," & ",fp(reg_imp_8$coefficients, 4), sign_stars(reg_imp_8$coeftable[, 4])," \\\\ \n")
cat("                                         & & & & & & & ", fpt(reg_imp_7$se, 4)," & ",fpt(reg_imp_8$se, 4)," \\\\ \n")
cat("\\addlinespace \\hline \\addlinespace \n")
# cat("Standard deviation & ",sd(match$wsi_i)," & ",sd(match$wsi_iw)," & ",sd(match$ti_i)," & ",sd(match$ti_iw)," & ",sd(match$ctv_i)," & ",sd(match$sr_i)," & ",sd(match$sr_iw)," & ",sd(match$kc_i)," \\\\ \n")
cat("Industry FE & \\checkmark & \\checkmark & \\checkmark & \\checkmark & \\checkmark & \\checkmark & \\checkmark & \\checkmark \\\\ \n")
cat("Year FE     & \\checkmark & \\checkmark & \\checkmark & \\checkmark & \\checkmark & \\checkmark & \\checkmark & \\checkmark \\\\ \n")
cat(" Network metric weighted & & \\multirow{2}{*}{\\checkmark} & & \\multirow{2}{*}{\\checkmark} & & \\multirow{2}{*}{\\checkmark} & & \\multirow{2}{*}{\\checkmark} \\\\ \n")
cat(" by transaction value    & & & & & & & & \\\\ \n")
cat("\\addlinespace \\hline \\addlinespace \n")
cat("IQR & ",fp(IQR$wsvi_i_c,2)," & ",fp(IQR$wsvi_i_w,2)," & ",fp(IQR$bvi_i_c,2)," & ",fp(IQR$bvi_i_w,2)," & ",fp(IQR$wsfi_i_c,2)," & ",fp(IQR$wsfi_i_w,2)," & ",fp(IQR$bfi_i_c,2)," & ",fp(IQR$bfi_i_w,2)," \\\\ \n")
cat("Observations & ",reg_imp_1$nobs," & ",reg_imp_2$nobs," & ",reg_imp_3$nobs," & ",reg_imp_4$nobs," & ",reg_imp_5$nobs," & ",reg_imp_6$nobs," & ",reg_imp_7$nobs," & ",reg_imp_8$nobs," \\\\ \n")
cat("\\bottomrule \n")
cat("\\end{tabular} \n")
cat("\\end{adjustbox} \n")
cat("\\justify \\footnotesize \\emph{Notes:} This table reports results. Standard errors clustered at the firm-level reported in parentheses. Significance levels given by: *** p $<$ 0.001, ** p $<$ 0.01, * p $<$ 0.05. \n")
cat("\\end{table} \n")
sink()


#///////////////////////////////////////////////////////////////////////////////
#----                 2 - RUN REGRESSIONS ON DIVERSIFICATION                ----
#///////////////////////////////////////////////////////////////////////////////

# Run regressions for network variables relating to idea of diversification
reg_div_1 <- feols(mu ~ log(act_n_buyers)    | ind + year, data=match, panel.id = c("id","year"), cluster = "id")
reg_div_2 <- feols(mu ~ log(act_n_sectors)   | ind + year, data=match, panel.id = c("id","year"), cluster = "id")
reg_div_3 <- feols(mu ~ log(act_n_provinces) | ind + year, data=match, panel.id = c("id","year"), cluster = "id")
reg_div_4 <- feols(mu ~ lo_i_c               | ind + year, data=match, panel.id = c("id","year"), cluster = "id")
reg_div_5 <- feols(mu ~ lo_i_w               | ind + year, data=match, panel.id = c("id","year"), cluster = "id")
reg_div_6 <- feols(mu ~ ho_i_c               | ind + year, data=match, panel.id = c("id","year"), cluster = "id")
reg_div_7 <- feols(mu ~ ho_i_w               | ind + year, data=match, panel.id = c("id","year"), cluster = "id")

sink(paste0(pathTab,sysdate,"_table_6_regression_results_diversification.tex"))
cat("\\begin{table}[!htbp]\\centering \n")
cat("\\caption{\\label{tab:EstimatedResults_Diversification} Regression Results Diversification} \n")
cat("\\begin{adjustbox}{width=\\columnwidth,center} \n")
cat("\\begin{tabular}{lccccccc}")
cat("\\toprule \n")
cat(" & (1) & (2) & (3) & (4) & (5) & (6) & (7) \\\\ \n")
cat("\\addlinespace \\hline \\addlinespace \n")
cat("log(Nr. of buyers)             & ", fp(reg_div_1$coefficients, 4), sign_stars(reg_div_1$coeftable[, 4])," & & & & & & \\\\ \n")
cat("                               & ", fpt(reg_div_1$se, 4)," & & & & & & \\\\ \n")
cat("log(Nr. of sectors)            & & ", fp(reg_div_2$coefficients, 4), sign_stars(reg_div_2$coeftable[, 4])," & & & & & \\\\ \n")
cat("                               & & ", fpt(reg_div_2$se, 4)," & & & & & \\\\ \n")
cat("log(Nr. of provinces)          & & & ", fp(reg_div_3$coefficients, 4), sign_stars(reg_div_3$coeftable[, 4])," & & & & \\\\ \n")
cat("                               & & & ", fpt(reg_div_3$se, 4)," & & & & \\\\ \n")
cat("Buyers in other industry (\\%) & & & & ", fp(reg_div_4$coefficients, 4), sign_stars(reg_div_4$coeftable[, 4])," & ",fp(reg_div_5$coefficients, 4), sign_stars(reg_div_5$coeftable[, 4])," & & \\\\ \n")
cat("                               & & & & ", fpt(reg_div_4$se, 4)," & ",fpt(reg_div_5$se, 4)," & & \\\\ \n")
cat("Buyers in other province (\\%) & & & & & & ", fp(reg_div_6$coefficients, 4), sign_stars(reg_div_6$coeftable[, 4])," & ",fp(reg_div_7$coefficients, 4), sign_stars(reg_div_7$coeftable[, 4])," \\\\ \n")
cat("                               & & & & & & ", fpt(reg_div_6$se, 4)," & ",fpt(reg_div_7$se, 4)," \\\\ \n")
cat("\\addlinespace \\hline \\addlinespace \n")
# cat("Standard deviation & ",sd(match$wsi_i)," & ",sd(match$wsi_iw)," & ",sd(match$ti_i)," & ",sd(match$ti_iw)," & ",sd(match$ctv_i)," & ",sd(match$sr_i)," & ",sd(match$sr_iw)," & ",sd(match$kc_i)," \\\\ \n")
cat("Industry FE & \\checkmark & \\checkmark & \\checkmark & \\checkmark & \\checkmark & \\checkmark & \\checkmark \\\\ \n")
cat("Year FE     & \\checkmark & \\checkmark & \\checkmark & \\checkmark & \\checkmark & \\checkmark & \\checkmark \\\\ \n")
cat(" Network metric weighted & & & & & \\multirow{2}{*}{\\checkmark} & & \\multirow{2}{*}{\\checkmark} \\\\ \n")
cat(" by transaction value    & & & & & & & \\\\ \n")
cat("\\addlinespace \\hline \\addlinespace \n")
cat("IQR & ",fp(IQR$act_n_buyers,0)," & ",fp(IQR$act_n_sectors,0)," & ",fp(IQR$act_n_provinces,0)," & ",fp(IQR$lo_i_c,2)," & ",fp(IQR$lo_i_w,2)," & ",fp(IQR$ho_i_c,2)," & ",fp(IQR$ho_i_w,2)," \\\\ \n")
cat("Observations & ",reg_div_1$nobs," & ",reg_div_2$nobs," & ",reg_div_3$nobs," & ",reg_div_4$nobs," & ",reg_div_5$nobs," & ",reg_div_6$nobs," & ",reg_div_7$nobs," \\\\ \n")
cat("\\bottomrule \n")
cat("\\end{tabular} \n")
cat("\\end{adjustbox} \n")
cat("\\justify \\footnotesize \\emph{Notes:} This table reports results. Standard errors clustered at the firm-level reported in parentheses. Significance levels given by: *** p $<$ 0.001, ** p $<$ 0.01, * p $<$ 0.05. \n")
cat("\\end{table} \n")
sink()


























#///////////////////////////////////////////////////////////////////////////////
#----               OLD REGS                ----
#///////////////////////////////////////////////////////////////////////////////

# Run regressions
reg01 <- feols(mu_v_dlw_cd ~ wsi_i | year, data=match, subset = match$ind_group=="AF", panel.id = c("id","year"), cluster = "id")
reg02 <- feols(mu_v_dlw_cd ~ wsi_i | year, data=match, subset = match$ind_group=="G", panel.id = c("id","year"), cluster = "id")
reg03 <- feols(mu_v_dlw_cd ~ wsi_i | year, data=match, subset = match$ind_group=="HQ", panel.id = c("id","year"), cluster = "id")
reg04 <- feols(mu_v_dlw_cd ~ wsi_i | year, data=match, panel.id = c("id","year"), cluster = "id")

share_AF <- reg01$nobs/reg04$nobs
share_G  <- reg02$nobs/reg04$nobs
share_HQ <- reg03$nobs/reg04$nobs

# Put results into table by industry group
sink(paste0(pathTab,sysdate,"_table_5_regression_results_muV_WSI.tex"))
cat("\\begin{table}[!htbp]\\centering \n")
cat("\\caption{\\label{tab:EstimatedResultsV_WSI_IndustryGroup} Regression Results} \n")
cat("\\begin{adjustbox}{width=\\columnwidth,center} \n")
cat("\\begin{tabular}{lcccc}")
cat("\\toprule \n")
cat(" & (1) & (2) & (3) & (4) \\\\ \n")
cat("\\addlinespace \\hline \\addlinespace \n")
cat("$WSI_i$ & ", 
    fp(reg01$coefficients, 4), sign_stars(reg01$coeftable[, 4])," & ", 
    fp(reg02$coefficients, 4), sign_stars(reg02$coeftable[, 4])," & ", 
    fp(reg03$coefficients, 4), sign_stars(reg03$coeftable[, 4])," & ", 
    fp(reg04$coefficients, 4), sign_stars(reg04$coeftable[, 4])," \\\\ \n")
cat(" & ",        fpt(reg01$se, 4),          " & ", fpt(reg02$se, 4),          " & ", fpt(reg03$se, 4),          " & ", fpt(reg04$se, 4),          " \\\\ \n")
cat("\\addlinespace \\hline \\addlinespace \n")
cat("Year FE             & \\checkmark & \\checkmark & \\checkmark & \\checkmark \\\\ \n")
cat("Industries A01--F45 & \\checkmark &             &             & \\checkmark \\\\ \n")
cat("Industries G50--G52 &             & \\checkmark &             & \\checkmark \\\\ \n")
cat("Industries H55--Q99 &             &             & \\checkmark & \\checkmark \\\\ \n")
cat("Observations & ",reg01$nobs," & ",reg02$nobs," & ",reg03$nobs," & ",reg04$nobs," \\\\ \n")
cat("Share of observations & ", fpp(share_AF,1), " & ", fpp(share_G,1), " & ", fpp(share_HQ,1), " & 100\\% \\\\  \n")
cat("\\bottomrule \n")
cat("\\end{tabular} \n")
cat("\\end{adjustbox} \n")
cat("\\justify \\footnotesize \\emph{Notes:} This table reports results. Standard errors clustered at the firm-level reported in parentheses. Significance levels given by: *** p $<$ 0.001, ** p $<$ 0.01, * p $<$ 0.05. \n")
cat("\\end{table} \n")
sink()

reg05 <- feols(mu_v_dlw_cd ~ wsi_i,                   data=match, panel.id = c("id","year"), cluster = "id")
reg06 <- feols(mu_v_dlw_cd ~ wsi_i | year,            data=match, panel.id = c("id","year"), cluster = "id")
reg07 <- feols(mu_v_dlw_cd ~ wsi_i | year + ind,      data=match, panel.id = c("id","year"), cluster = "id")
reg08 <- feols(mu_v_dlw_cd ~ wsi_i | year + ind + id, data=match, panel.id = c("id","year"), cluster = "id")


# Put results into table with different FEs
sink(paste0(pathTab,sysdate,"_table_5_regression_results_muV_WSI_FEs.tex"))
cat("\\begin{table}[!htbp]\\centering \n")
cat("\\caption{\\label{tab:EstimatedResultsV_WSI_FE} Regression Results} \n")
cat("\\begin{adjustbox}{width=\\columnwidth,center} \n")
cat("\\begin{tabular}{lcccc}")
cat("\\toprule \n")
cat(" & (1) & (2) & (3) & (4) \\\\ \n")
cat("\\addlinespace \\hline \\addlinespace \n")
cat("$WSI_i$ & ", 
    fp(reg05$coefficients[2], 4), sign_stars(reg05$coeftable[2, 4])," & ", 
    fp(reg06$coefficients, 4), sign_stars(reg06$coeftable[, 4])," & ", 
    fp(reg07$coefficients, 4), sign_stars(reg07$coeftable[, 4])," & ", 
    fp(reg08$coefficients, 4), sign_stars(reg08$coeftable[, 4])," \\\\ \n")
cat(" & ",        fpt(reg05$se[2], 4),          " & ", fpt(reg06$se, 4),          " & ", fpt(reg07$se, 4),          " & ", fpt(reg08$se, 4),          " \\\\ \n")
cat("\\addlinespace \\hline \\addlinespace \n")
cat("Year FE     &  & \\checkmark & \\checkmark & \\checkmark \\\\ \n")
cat("Industry FE &  &             & \\checkmark & \\checkmark \\\\ \n")
cat("Firm FE     &  &             &             & \\checkmark \\\\ \n")
cat("Observations & ",reg05$nobs," & ",reg06$nobs," & ",reg07$nobs," & ",reg08$nobs," \\\\ \n")
cat("\\bottomrule \n")
cat("\\end{tabular} \n")
cat("\\end{adjustbox} \n")
cat("\\justify \\footnotesize \\emph{Notes:} This table reports results. Standard errors clustered at the firm-level reported in parentheses. Significance levels given by: *** p $<$ 0.001, ** p $<$ 0.01, * p $<$ 0.05. \n")
cat("\\end{table} \n")
sink()


ggplot(data=match, aes(x=wsi_i, y=mu_v_dlw_cd)) + 
    geom_point(aes(colour=ind_group), alpha=0.5, size=0.1) + scale_y_continuous(limits=c(0.75,2)) +
    geom_smooth(method="lm", formula=y~x, color="black") +
    geom_smooth(aes(colour=ind_group), method="lm", formula=y~x)


# reg11 <- feols(mu_v_dlw_cd ~ wsi_i  | id + year, data=match, panel.id = c("id","year"), cluster = "id")
# reg12 <- feols(mu_v_dlw_cd ~ wsi_iw | id + year, data=match, panel.id = c("id","year"), cluster = "id")
# reg13 <- feols(mu_v_dlw_cd ~ ti_i   | id + year, data=match, panel.id = c("id","year"), cluster = "id")
# reg14 <- feols(mu_v_dlw_cd ~ ti_iw  | id + year, data=match, panel.id = c("id","year"), cluster = "id")
# reg15 <- feols(mu_v_dlw_cd ~ ctv_i  | id + year, data=match, panel.id = c("id","year"), cluster = "id")
# reg16 <- feols(mu_v_dlw_cd ~ sr_i   | id + year, data=match, panel.id = c("id","year"), cluster = "id")
# reg17 <- feols(mu_v_dlw_cd ~ sr_iw  | id + year, data=match, panel.id = c("id","year"), cluster = "id")
# reg18 <- feols(mu_v_dlw_cd ~ kc_i   | id + year, data=match, panel.id = c("id","year"), cluster = "id")
reg11 <- feols(mu_v_dlw_cd ~ wsi_i  | ind + year, data=match, panel.id = c("id","year"), cluster = "id")
reg12 <- feols(mu_v_dlw_cd ~ wsi_iw | ind + year, data=match, panel.id = c("id","year"), cluster = "id")
reg13 <- feols(mu_v_dlw_cd ~ ti_i   | ind + year, data=match, panel.id = c("id","year"), cluster = "id")
reg14 <- feols(mu_v_dlw_cd ~ ti_iw  | ind + year, data=match, panel.id = c("id","year"), cluster = "id")
reg15 <- feols(mu_v_dlw_cd ~ ctv_i  | ind + year, data=match, panel.id = c("id","year"), cluster = "id")
reg16 <- feols(mu_v_dlw_cd ~ sr_i   | ind + year, data=match, panel.id = c("id","year"), cluster = "id")
reg17 <- feols(mu_v_dlw_cd ~ sr_iw  | ind + year, data=match, panel.id = c("id","year"), cluster = "id")
reg18 <- feols(mu_v_dlw_cd ~ kc_i   | ind + year, data=match, panel.id = c("id","year"), cluster = "id")


# Put results into table by measure
sink(paste0(pathTab,sysdate,"_table_5_regression_results_muV.tex"))
cat("\\begin{table}[!htbp]\\centering \n")
cat("\\caption{\\label{tab:EstimatedResultsV_all} Regression Results} \n")
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


