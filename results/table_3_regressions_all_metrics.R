#///////////////////////////////////////////////////////////////////////////////
# File name:		table_3_regressions_all_metrics.R
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
markups <- markups[!is.na(mu_v_dlw_tl)] # exclude missing obs

# Create dummy for firm size expansion (based on cost)
markups[, V_lag := shift(V), by="id"]
markups[, firm_size_expansion := V-V_lag]
markups[, firm_size_expansion_dummy := ifelse(firm_size_expansion>0, 1, 0)]

# Since we are only focused on are main specification, drop all other variables
markups[, c("mu_m_dlw_cd", "mu_m_dlw_tl", "mu_v_dlw_cd", "M", "L", "V_lag") := NULL]
setnames(markups, "mu_v_dlw_tl", "mu")

# Discard negative markups
cat("There are",nrow(markups[mu<0]),"negative markups")
markups <- markups[mu>=0]

# Match markups estimation sample and network sample
match <- merge(markups, network_metrics, by.x=c("id","year","ind"), by.y=c("id_seller","year","seller_sec"))

# Calculate IQR for network metrics
IQR <- dcast(data = match[,.SD,.SDcols = !c("id", "year", "ind", "ind_group", "firm_size_expansion", "firm_size_expansion_dummy", "mu", "alpha_v", "V", "K", "seller_weight")],
             formula = .~.,
             fun = IQR,
             value.var=colnames(match[,.SD,.SDcols = !c("id", "year", "ind", "ind_group", "firm_size_expansion", "firm_size_expansion_dummy", "mu", "alpha_v", "V", "K", "seller_weight")]))

# Reshape data to allow loops
match_melt <- melt(data = match,
                   id.vars = c("id","year","ind","ind_group","mu","alpha_v","V","K","firm_size_expansion","firm_size_expansion_dummy","seller_weight"))
IQR_melt   <- melt(data = IQR,
                   id.vars = c("."))
IQR_melt[, . := NULL]
IQR[, . := NULL]

# Create change in WII
# match[, wii_lag := shift(wsvi_i_c), by="id"]
# match[, delta_wii := wsvi_i_c-wii_lag]
# ggplot(data=match, aes(x=firm_size_expansion, y=delta_wii)) + geom_point() + scale_x_continuous(limits=c(-100000,100000)) + geom_smooth()
# summary(lm(delta_wii~firm_size_expansion, data=match))

#///////////////////////////////////////////////////////////////////////////////
#----                   RUN REGRESSIONS ON ALL METRICS                      ----
#///////////////////////////////////////////////////////////////////////////////

# library(quantreg)
# model <- rq(mu ~ wsvi_i_c, data = match, tau = 0.5)

# Run regressions for network variables relating to idea of diversification
reg_impo_1 <- feols(mu ~ wsvi_i_c + V + K | ind + year, data=match, panel.id = c("id","year"), cluster = "id")
reg_impo_2 <- feols(mu ~ wsvi_i_w + V + K | ind + year, data=match, panel.id = c("id","year"), cluster = "id")
reg_impo_3 <- feols(mu ~ bvi_i_c  + V + K | ind + year, data=match, panel.id = c("id","year"), cluster = "id")
reg_impo_4 <- feols(mu ~ bvi_i_w  + V + K | ind + year, data=match, panel.id = c("id","year"), cluster = "id")
reg_impo_1_FFE <- feols(mu ~ wsvi_i_c + V + K | ind + year + id, data=match, panel.id = c("id","year"), cluster = "id")
reg_impo_2_FFE <- feols(mu ~ wsvi_i_w + V + K | ind + year + id, data=match, panel.id = c("id","year"), cluster = "id")
reg_impo_3_FFE <- feols(mu ~ bvi_i_c  + V + K | ind + year + id, data=match, panel.id = c("id","year"), cluster = "id")
reg_impo_4_FFE <- feols(mu ~ bvi_i_w  + V + K | ind + year + id, data=match, panel.id = c("id","year"), cluster = "id")

reg_qual_1 <- feols(mu ~     sr_i_c  + V + K | ind + year, data=match, panel.id = c("id","year"), cluster = "id")
reg_qual_2 <- feols(mu ~     sr_i_w  + V + K | ind + year, data=match, panel.id = c("id","year"), cluster = "id")
reg_qual_3 <- feols(mu ~     ri_i_c  + V + K | ind + year, data=match, panel.id = c("id","year"), cluster = "id")
reg_qual_4 <- feols(mu ~     ri_i_w  + V + K | ind + year, data=match, panel.id = c("id","year"), cluster = "id")
reg_qual_5 <- feols(mu ~ log(sf_i_c) + V + K | ind + year, data=match, panel.id = c("id","year"), cluster = "id")
reg_qual_6 <- feols(mu ~ log(sf_i_w) + V + K | ind + year, data=match, panel.id = c("id","year"), cluster = "id")
reg_qual_1_FFE <- feols(mu ~     sr_i_c  + V + K | ind + year + id, data=match, panel.id = c("id","year"), cluster = "id")
reg_qual_2_FFE <- feols(mu ~     sr_i_w  + V + K | ind + year + id, data=match, panel.id = c("id","year"), cluster = "id")
reg_qual_3_FFE <- feols(mu ~     ri_i_c  + V + K | ind + year + id, data=match, panel.id = c("id","year"), cluster = "id")
reg_qual_4_FFE <- feols(mu ~     ri_i_w  + V + K | ind + year + id, data=match, panel.id = c("id","year"), cluster = "id")
reg_qual_5_FFE <- feols(mu ~ log(sf_i_c) + V + K | ind + year + id, data=match, panel.id = c("id","year"), cluster = "id")
reg_qual_6_FFE <- feols(mu ~ log(sf_i_w) + V + K | ind + year + id, data=match, panel.id = c("id","year"), cluster = "id")

reg_comp_1 <- feols(mu ~ us_i_c + V + K | ind + year, data=match, panel.id = c("id","year"), cluster = "id")
reg_comp_2 <- feols(mu ~ us_i_w + V + K | ind + year, data=match, panel.id = c("id","year"), cluster = "id")
reg_comp_3 <- feols(mu ~ ci_i_c + V + K | ind + year, data=match, panel.id = c("id","year"), cluster = "id")
reg_comp_4 <- feols(mu ~ ci_i_w + V + K | ind + year, data=match, panel.id = c("id","year"), cluster = "id")
reg_comp_1_FFE <- feols(mu ~ us_i_c + V + K | ind + year + id, data=match, panel.id = c("id","year"), cluster = "id")
reg_comp_2_FFE <- feols(mu ~ us_i_w + V + K | ind + year + id, data=match, panel.id = c("id","year"), cluster = "id")
reg_comp_3_FFE <- feols(mu ~ ci_i_c + V + K | ind + year + id, data=match, panel.id = c("id","year"), cluster = "id")
reg_comp_4_FFE <- feols(mu ~ ci_i_w + V + K | ind + year + id, data=match, panel.id = c("id","year"), cluster = "id")

reg_targ_1 <- feols(mu ~ lo_i_c + V + K | ind + year, data=match, panel.id = c("id","year"), cluster = "id")
reg_targ_2 <- feols(mu ~ lo_i_w + V + K | ind + year, data=match, panel.id = c("id","year"), cluster = "id")
reg_targ_3 <- feols(mu ~ vt_i_c + V + K | ind + year, data=match, panel.id = c("id","year"), cluster = "id")
reg_targ_4 <- feols(mu ~ vt_i_w + V + K | ind + year, data=match, panel.id = c("id","year"), cluster = "id")
reg_targ_5 <- feols(mu ~ sp_i_c + V + K | ind + year, data=match, panel.id = c("id","year"), cluster = "id")
reg_targ_6 <- feols(mu ~ sp_i_w + V + K | ind + year, data=match, panel.id = c("id","year"), cluster = "id")
reg_targ_1_FFE <- feols(mu ~ lo_i_c + V + K | ind + year + id, data=match, panel.id = c("id","year"), cluster = "id")
reg_targ_2_FFE <- feols(mu ~ lo_i_w + V + K | ind + year + id, data=match, panel.id = c("id","year"), cluster = "id")
reg_targ_3_FFE <- feols(mu ~ vt_i_c + V + K | ind + year + id, data=match, panel.id = c("id","year"), cluster = "id")
reg_targ_4_FFE <- feols(mu ~ vt_i_w + V + K | ind + year + id, data=match, panel.id = c("id","year"), cluster = "id")
reg_targ_5_FFE <- feols(mu ~ sp_i_c + V + K | ind + year + id, data=match, panel.id = c("id","year"), cluster = "id")
reg_targ_6_FFE <- feols(mu ~ sp_i_w + V + K | ind + year + id, data=match, panel.id = c("id","year"), cluster = "id")

reg_dive_1 <- feols(mu ~ log(act_n_buyers)    + V + K | ind + year, data=match, panel.id = c("id","year"), cluster = "id")
reg_dive_2 <- feols(mu ~ log(act_n_sectors)   + V + K | ind + year, data=match, panel.id = c("id","year"), cluster = "id")
reg_dive_3 <- feols(mu ~ log(act_n_provinces) + V + K | ind + year, data=match, panel.id = c("id","year"), cluster = "id")
reg_dive_1_FFE <- feols(mu ~ log(act_n_buyers)    + V + K | ind + year + id, data=match, panel.id = c("id","year"), cluster = "id")
reg_dive_2_FFE <- feols(mu ~ log(act_n_sectors)   + V + K | ind + year + id, data=match, panel.id = c("id","year"), cluster = "id")
reg_dive_3_FFE <- feols(mu ~ log(act_n_provinces) + V + K | ind + year + id, data=match, panel.id = c("id","year"), cluster = "id")

#///////////////////////////////////////////////////////////////////////////////
#----                           OUTPUT TABLE                                ----
#///////////////////////////////////////////////////////////////////////////////

sink(paste0(pathTab,sysdate,"_table_3_regression_results_all_metrics.tex"))
cat("\\begin{table}[!htbp]\\centering \n")
cat("\\caption{\\label{tab:EstimatedResults_All} Regression Results for All Network Metrics} \n")
cat("\\begin{adjustbox}{height=0.4\\textheight,center} \n")
cat("\\begin{tabular}{llccc}")
cat("\\toprule \n")
cat(" & & IQR & (1) & (2) \\\\ \n")
cat("\\addlinespace \\hline \\addlinespace \n")

cat("\\multicolumn{1}{l|}{\\multirow{5}{0.2\\textwidth}{Importance for buyer}} & Within Industry & \\multirow{2}{*}{",fp(IQR$wsvi_i_c,2),"} & ", 
    fp(reg_impo_1$coefficients[1], 4), sign_stars(reg_impo_1$coeftable[1, 4])," & ", 
    fp(reg_impo_1_FFE$coefficients[1], 4), sign_stars(reg_impo_1_FFE$coeftable[1, 4])," \\\\ \n")
cat("\\multicolumn{1}{l|}{ } & Importance & & ", 
    fpt(reg_impo_1$se[1], 4)," & ",
    fpt(reg_impo_1_FFE$se[1], 4)," \\\\ \n")
cat("\\addlinespace \\cline{2-5} \\addlinespace \n")
cat("\\multicolumn{1}{l|}{ } & Across Industry & \\multirow{2}{*}{",fp(IQR$bvi_i_c,2),"} & ", 
    fp(reg_impo_3$coefficients[1], 4), sign_stars(reg_impo_3$coeftable[1, 4])," & ", 
    fp(reg_impo_3_FFE$coefficients[1], 4), sign_stars(reg_impo_3_FFE$coeftable[1, 4])," \\\\ \n")
cat("\\multicolumn{1}{l|}{ } & Importance & & ", 
    fpt(reg_impo_3$se[1], 4)," & ",
    fpt(reg_impo_3_FFE$se[1], 4)," \\\\ \n")
cat("\\addlinespace \\hline \\addlinespace \n")

cat("\\multicolumn{1}{l|}{\\multirow{8}{0.15\\textwidth}{Relationship quality}} & Seller & \\multirow{2}{*}{",fp(IQR$sr_i_c,2),"} & ", 
    fp(reg_qual_1$coefficients[1], 4), sign_stars(reg_qual_1$coeftable[1, 4])," & ", 
    fp(reg_qual_1_FFE$coefficients[1], 4), sign_stars(reg_qual_1_FFE$coeftable[1, 4])," \\\\ \n")
cat("\\multicolumn{1}{l|}{ } & Reciprocity & & ", 
    fpt(reg_qual_1$se[1], 4)," & ",
    fpt(reg_qual_1_FFE$se[1], 4)," \\\\ \n")
cat("\\addlinespace \\cline{2-5} \\addlinespace \n")
cat("\\multicolumn{1}{l|}{ } & Reciprocity & \\multirow{2}{*}{",fp(IQR$ri_i_c,2),"} & ", 
    fp(reg_qual_3$coefficients[1], 4), sign_stars(reg_qual_3$coeftable[1, 4])," & ", 
    fp(reg_qual_3_FFE$coefficients[1], 4), sign_stars(reg_qual_3_FFE$coeftable[1, 4])," \\\\ \n")
cat("\\multicolumn{1}{l|}{ } & Degree & & ", 
    fpt(reg_qual_3$se[1], 4)," & ",
    fpt(reg_qual_3_FFE$se[1], 4)," \\\\ \n")
cat("\\addlinespace \\cline{2-5} \\addlinespace \n")
cat("\\multicolumn{1}{l|}{ } & log(Selling & \\multirow{2}{*}{",fp(IQR$sf_i_c,2),"} & ", 
    fp(reg_qual_5$coefficients[1], 4), sign_stars(reg_qual_5$coeftable[1, 4])," & ", 
    fp(reg_qual_5_FFE$coefficients[1], 4), sign_stars(reg_qual_5_FFE$coeftable[1, 4])," \\\\ \n")
cat("\\multicolumn{1}{l|}{ } & Frequency) & & ", 
    fpt(reg_qual_5$se[1], 4)," & ",
    fpt(reg_qual_5_FFE$se[1], 4)," \\\\ \n")
cat("\\addlinespace \\hline \\addlinespace \n")

cat("\\multicolumn{1}{l|}{\\multirow{5}{0.15\\textwidth}{Competition}} & Unique & \\multirow{2}{*}{",fp(IQR$us_i_c,2),"} & ", 
    fp(reg_comp_1$coefficients[1], 4), sign_stars(reg_comp_1$coeftable[1, 4])," & ", 
    fp(reg_comp_1_FFE$coefficients[1], 4), sign_stars(reg_comp_1_FFE$coeftable[1, 4])," \\\\ \n")
cat("\\multicolumn{1}{l|}{ } & Seller & & ", 
    fpt(reg_comp_1$se[1], 4)," & ",
    fpt(reg_comp_1_FFE$se[1], 4)," \\\\ \n")
cat("\\addlinespace \\cline{2-5} \\addlinespace \n")
cat("\\multicolumn{1}{l|}{ } & Competition & \\multirow{2}{*}{",fp(IQR$ci_i_c,2),"} & ", 
    fp(reg_comp_3$coefficients[1], 4), sign_stars(reg_comp_3$coeftable[1, 4])," & ", 
    fp(reg_comp_3_FFE$coefficients[1], 4), sign_stars(reg_comp_3_FFE$coeftable[1, 4])," \\\\ \n")
cat("\\multicolumn{1}{l|}{ } & Intensity & & ", 
    fpt(reg_comp_3$se[1], 4)," & ",
    fpt(reg_comp_3_FFE$se[1], 4)," \\\\ \n")
cat("\\addlinespace \\hline \\addlinespace \n")

cat("\\multicolumn{1}{l|}{\\multirow{8}{0.15\\textwidth}{Market targeting}} & Local & \\multirow{2}{*}{",fp(IQR$lo_i_c,2),"} & ", 
    fp(reg_targ_1$coefficients[1], 4), sign_stars(reg_targ_1$coeftable[1, 4])," & ", 
    fp(reg_targ_1_FFE$coefficients[1], 4), sign_stars(reg_targ_1_FFE$coeftable[1, 4])," \\\\ \n")
cat("\\multicolumn{1}{l|}{ } & Focus  & & ", 
    fpt(reg_targ_1$se[1], 4)," & ",
    fpt(reg_targ_1_FFE$se[1], 4)," \\\\ \n")
cat("\\addlinespace \\cline{2-5} \\addlinespace \n")
cat("\\multicolumn{1}{l|}{ } & Vertical & \\multirow{2}{*}{",fp(IQR$vt_i_c,2),"} & ", 
    fp(reg_targ_3$coefficients[1], 4), sign_stars(reg_targ_3$coeftable[1, 4])," & ", 
    fp(reg_targ_3_FFE$coefficients[1], 4), sign_stars(reg_targ_3_FFE$coeftable[1, 4])," \\\\ \n")
cat("\\multicolumn{1}{l|}{ } & Focus & & ", 
    fpt(reg_targ_3$se[1], 4)," & ",
    fpt(reg_targ_3_FFE$se[1], 4)," \\\\ \n")
cat("\\addlinespace \\cline{2-5} \\addlinespace \n")
cat("\\multicolumn{1}{l|}{ } & Buyer & \\multirow{2}{*}{",fp(IQR$sp_i_c,2),"} & ", 
    fp(reg_targ_5$coefficients[1], 4), sign_stars(reg_targ_5$coeftable[1, 4])," & ", 
    fp(reg_targ_5_FFE$coefficients[1], 4), sign_stars(reg_targ_5_FFE$coeftable[1, 4])," \\\\ \n")
cat("\\multicolumn{1}{l|}{ } & Specialisation & & ", 
    fpt(reg_targ_5$se[1], 4)," & ",
    fpt(reg_targ_5_FFE$se[1], 4)," \\\\ \n")
cat("\\addlinespace \\hline \\addlinespace \n")

cat("\\multicolumn{1}{l|}{\\multirow{8}{0.15\\textwidth}{Diversification}} & log(Unique & \\multirow{2}{*}{",fp(IQR$act_n_buyers,0),"} & ", 
    fp(reg_dive_1$coefficients[1], 4), sign_stars(reg_dive_1$coeftable[1, 4])," & ", 
    fp(reg_dive_1_FFE$coefficients[1], 4), sign_stars(reg_dive_1_FFE$coeftable[1, 4]),"  \\\\ \n")
cat("\\multicolumn{1}{l|}{ } & Buyer Count) & & ", 
    fpt(reg_dive_1$se[1], 4)," & ", 
    fpt(reg_dive_1_FFE$se[1], 4)," \\\\ \n")
cat("\\addlinespace \\cline{2-5} \\addlinespace \n")
cat("\\multicolumn{1}{l|}{ } & log(Unique & \\multirow{2}{*}{",fp(IQR$act_n_sectors,0),"} & ", 
    fp(reg_dive_2$coefficients[1], 4), sign_stars(reg_dive_2$coeftable[1, 4])," & ", 
    fp(reg_dive_2_FFE$coefficients[1], 4), sign_stars(reg_dive_2_FFE$coeftable[1, 4])," \\\\ \n")
cat("\\multicolumn{1}{l|}{ } & Industry Count) & & ", 
    fpt(reg_dive_2$se[1], 4)," & ", 
    fpt(reg_dive_2_FFE$se[1], 4)," \\\\ \n")
cat("\\addlinespace \\cline{2-5} \\addlinespace \n")
cat("\\multicolumn{1}{l|}{ } & log(Unique & \\multirow{2}{*}{",fp(IQR$act_n_provinces,0),"} & ", 
    fp(reg_dive_3$coefficients[1], 4), sign_stars(reg_dive_3$coeftable[1, 4])," & ", 
    fp(reg_dive_3_FFE$coefficients[1], 4), sign_stars(reg_dive_3_FFE$coeftable[1, 4])," \\\\ \n")
cat("\\multicolumn{1}{l|}{ } & Province Count) & & ", 
    fpt(reg_dive_3$se[1], 4)," & ", 
    fpt(reg_dive_3_FFE$se[1], 4)," \\\\ \n")

cat("\\addlinespace \\hline \\addlinespace \n")
cat("\\multicolumn{2}{l}{Year FE}     & & \\checkmark & \\checkmark \\\\ \n")
cat("\\multicolumn{2}{l}{Industry FE} & & \\checkmark & \\checkmark \\\\ \n")
cat("\\multicolumn{2}{l}{Input usage controls} & & \\checkmark & \\checkmark \\\\ \n")
cat("\\multicolumn{2}{l}{Firm FE}     & &             & \\checkmark \\\\ \n")
cat("\\addlinespace \\hline \\addlinespace \n")
cat("\\multicolumn{2}{l}{Observations} & & ",fp(nrow(match))," & ",fp(nrow(match))," \\\\ \n")
cat("\\bottomrule \n")
cat("\\end{tabular} \n")
cat("\\end{adjustbox} \n")
cat("\\justify \\footnotesize \\emph{Notes:} This table reports results of regressing different network-related metrics on markups. Each row captures an independent regression, i.e. the respective variable is regressed alone on markups, with the inclusion of the respective fixed effects and controls reported at the bottom. Concretely, Column (2) differs from Column (1) only through the inclusion of firm fixed effects. Interquartile ranges (IQR) are reported in levels even for the variables regressed in logs. Standard errors clustered at the firm level reported in parentheses. Significance levels given by: *** p $<$ 0.001, ** p $<$ 0.01, * p $<$ 0.05. \n")
cat("\\end{table} \n")
sink()




sink(paste0(pathTab,sysdate,"_table_3_regression_results_all_metrics_weighted.tex"))
cat("\\begin{table}[!htbp]\\centering \n")
cat("\\caption{\\label{tab:EstimatedResults_All_weighted} Regression Results for All Network Metrics} \n")
cat("\\begin{adjustbox}{width=0.7\\textwidth,center} \n")
cat("\\begin{tabular}{llccc}")
cat("\\toprule \n")
cat(" & & IQR & (1) & (2) \\\\ \n")
cat("\\addlinespace \\hline \\addlinespace \n")

cat("\\multicolumn{1}{l|}{\\multirow{5}{0.2\\textwidth}{Importance for buyer}} & Within Industry & \\multirow{2}{*}{",fp(IQR$wsvi_i_w,2),"} & ", 
    fp(reg_impo_2$coefficients[1], 4), sign_stars(reg_impo_2$coeftable[1, 4])," & ", 
    fp(reg_impo_2_FFE$coefficients[1], 4), sign_stars(reg_impo_2_FFE$coeftable[1, 4])," \\\\ \n")
cat("\\multicolumn{1}{l|}{ } & Importance & & ", 
    fpt(reg_impo_2$se[1], 4)," & ",
    fpt(reg_impo_2_FFE$se[1], 4)," \\\\ \n")
cat("\\addlinespace \\cline{2-5} \\addlinespace \n")
cat("\\multicolumn{1}{l|}{ } & Across Industry & \\multirow{2}{*}{",fp(IQR$bvi_i_w,2),"} & ", 
    fp(reg_impo_4$coefficients[1], 4), sign_stars(reg_impo_4$coeftable[1, 4])," & ", 
    fp(reg_impo_4_FFE$coefficients[1], 4), sign_stars(reg_impo_4_FFE$coeftable[1, 4])," \\\\ \n")
cat("\\multicolumn{1}{l|}{ } & Importance & & ", 
    fpt(reg_impo_4$se[1], 4)," & ",
    fpt(reg_impo_4_FFE$se[1], 4)," \\\\ \n")
cat("\\addlinespace \\hline \\addlinespace \n")

cat("\\multicolumn{1}{l|}{\\multirow{8}{0.15\\textwidth}{Relationship quality}} & Seller & \\multirow{2}{*}{",fp(IQR$sr_i_w,2),"} & ", 
    fp(reg_qual_2$coefficients[1], 4), sign_stars(reg_qual_2$coeftable[1, 4])," & ", 
    fp(reg_qual_2_FFE$coefficients[1], 4), sign_stars(reg_qual_2_FFE$coeftable[1, 4])," \\\\ \n")
cat("\\multicolumn{1}{l|}{ } & Reciprocity & & ", 
    fpt(reg_qual_2$se[1], 4)," & ",
    fpt(reg_qual_2_FFE$se[1], 4)," \\\\ \n")
cat("\\addlinespace \\cline{2-5} \\addlinespace \n")
cat("\\multicolumn{1}{l|}{ } & Reciprocity & \\multirow{2}{*}{",fp(IQR$ri_i_w,2),"} & ", 
    fp(reg_qual_4$coefficients[1], 4), sign_stars(reg_qual_4$coeftable[1, 4])," & ", 
    fp(reg_qual_4_FFE$coefficients[1], 4), sign_stars(reg_qual_4_FFE$coeftable[1, 4])," \\\\ \n")
cat("\\multicolumn{1}{l|}{ } & Degree & & ", 
    fpt(reg_qual_4$se[1], 4)," & ",
    fpt(reg_qual_4_FFE$se[1], 4)," \\\\ \n")
cat("\\addlinespace \\cline{2-5} \\addlinespace \n")
cat("\\multicolumn{1}{l|}{ } & log(Selling & \\multirow{2}{*}{",fp(IQR$sf_i_w,2),"} & ", 
    fp(reg_qual_6$coefficients[1], 4), sign_stars(reg_qual_6$coeftable[1, 4])," & ", 
    fp(reg_qual_6_FFE$coefficients[1], 4), sign_stars(reg_qual_6_FFE$coeftable[1, 4])," \\\\ \n")
cat("\\multicolumn{1}{l|}{ } & Frequency) & & ", 
    fpt(reg_qual_6$se[1], 4)," & ",
    fpt(reg_qual_6_FFE$se[1], 4)," \\\\ \n")
cat("\\addlinespace \\hline \\addlinespace \n")

cat("\\multicolumn{1}{l|}{\\multirow{5}{0.15\\textwidth}{Competition}} & Unique & \\multirow{2}{*}{",fp(IQR$us_i_w,2),"} & ", 
    fp(reg_comp_2$coefficients[1], 4), sign_stars(reg_comp_2$coeftable[1, 4])," & ", 
    fp(reg_comp_2_FFE$coefficients[1], 4), sign_stars(reg_comp_2_FFE$coeftable[1, 4])," \\\\ \n")
cat("\\multicolumn{1}{l|}{ } & Seller & & ", 
    fpt(reg_comp_2$se[1], 4)," & ",
    fpt(reg_comp_2_FFE$se[1], 4)," \\\\ \n")
cat("\\addlinespace \\cline{2-5} \\addlinespace \n")
cat("\\multicolumn{1}{l|}{ } & Competition & \\multirow{2}{*}{",fp(IQR$ci_i_w,2),"} & ", 
    fp(reg_comp_4$coefficients[1], 4), sign_stars(reg_comp_4$coeftable[1, 4])," & ", 
    fp(reg_comp_4_FFE$coefficients[1], 4), sign_stars(reg_comp_4_FFE$coeftable[1, 4])," \\\\ \n")
cat("\\multicolumn{1}{l|}{ } & Intensity & & ", 
    fpt(reg_comp_4$se[1], 4)," & ",
    fpt(reg_comp_4_FFE$se[1], 4)," \\\\ \n")
cat("\\addlinespace \\hline \\addlinespace \n")

cat("\\multicolumn{1}{l|}{\\multirow{8}{0.15\\textwidth}{Market targeting}} & Local & \\multirow{2}{*}{",fp(IQR$lo_i_w,2),"} & ", 
    fp(reg_targ_2$coefficients[1], 4), sign_stars(reg_targ_2$coeftable[1, 4])," & ", 
    fp(reg_targ_2_FFE$coefficients[1], 4), sign_stars(reg_targ_2_FFE$coeftable[1, 4])," \\\\ \n")
cat("\\multicolumn{1}{l|}{ } & Focus  & & ", 
    fpt(reg_targ_2$se[1], 4)," & ",
    fpt(reg_targ_2_FFE$se[1], 4)," \\\\ \n")
cat("\\addlinespace \\cline{2-5} \\addlinespace \n")
cat("\\multicolumn{1}{l|}{ } & Vertical & \\multirow{2}{*}{",fp(IQR$vt_i_w,2),"} & ", 
    fp(reg_targ_4$coefficients[1], 4), sign_stars(reg_targ_4$coeftable[1, 4])," & ", 
    fp(reg_targ_4_FFE$coefficients[1], 4), sign_stars(reg_targ_4_FFE$coeftable[1, 4])," \\\\ \n")
cat("\\multicolumn{1}{l|}{ } & Focus & & ", 
    fpt(reg_targ_4$se[1], 4)," & ",
    fpt(reg_targ_4_FFE$se[1], 4)," \\\\ \n")
cat("\\addlinespace \\cline{2-5} \\addlinespace \n")
cat("\\multicolumn{1}{l|}{ } & Buyer & \\multirow{2}{*}{",fp(IQR$sp_i_w,2),"} & ", 
    fp(reg_targ_6$coefficients[1], 4), sign_stars(reg_targ_6$coeftable[1, 4])," & ", 
    fp(reg_targ_6_FFE$coefficients[1], 4), sign_stars(reg_targ_6_FFE$coeftable[1, 4])," \\\\ \n")
cat("\\multicolumn{1}{l|}{ } & Specialisation & & ", 
    fpt(reg_targ_6$se[1], 4)," & ",
    fpt(reg_targ_6_FFE$se[1], 4)," \\\\ \n")
cat("\\addlinespace \\hline \\addlinespace \n")

cat("\\multicolumn{1}{l|}{\\multirow{8}{0.15\\textwidth}{Diversification}} & log(Unique & \\multirow{2}{*}{",fp(IQR$act_n_buyers,0),"} & ", 
    fp(reg_dive_1$coefficients[1], 4), sign_stars(reg_dive_1$coeftable[1, 4])," & ", 
    fp(reg_dive_1_FFE$coefficients[1], 4), sign_stars(reg_dive_1_FFE$coeftable[1, 4]),"  \\\\ \n")
cat("\\multicolumn{1}{l|}{ } & Buyer Count) & & ", 
    fpt(reg_dive_1$se[1], 4)," & ", 
    fpt(reg_dive_1_FFE$se[1], 4)," \\\\ \n")
cat("\\addlinespace \\cline{2-5} \\addlinespace \n")
cat("\\multicolumn{1}{l|}{ } & log(Unique & \\multirow{2}{*}{",fp(IQR$act_n_sectors,0),"} & ", 
    fp(reg_dive_2$coefficients[1], 4), sign_stars(reg_dive_2$coeftable[1, 4])," & ", 
    fp(reg_dive_2_FFE$coefficients[1], 4), sign_stars(reg_dive_2_FFE$coeftable[1, 4])," \\\\ \n")
cat("\\multicolumn{1}{l|}{ } & Industry Count) & & ", 
    fpt(reg_dive_2$se[1], 4)," & ", 
    fpt(reg_dive_2_FFE$se[1], 4)," \\\\ \n")
cat("\\addlinespace \\cline{2-5} \\addlinespace \n")
cat("\\multicolumn{1}{l|}{ } & log(Unique & \\multirow{2}{*}{",fp(IQR$act_n_provinces,0),"} & ", 
    fp(reg_dive_3$coefficients[1], 4), sign_stars(reg_dive_3$coeftable[1, 4])," & ", 
    fp(reg_dive_3_FFE$coefficients[1], 4), sign_stars(reg_dive_3_FFE$coeftable[1, 4])," \\\\ \n")
cat("\\multicolumn{1}{l|}{ } & Province Count) & & ", 
    fpt(reg_dive_3$se[1], 4)," & ", 
    fpt(reg_dive_3_FFE$se[1], 4)," \\\\ \n")

cat("\\addlinespace \\hline \\addlinespace \n")
cat("\\multicolumn{2}{l}{Year FE}     & & \\checkmark & \\checkmark \\\\ \n")
cat("\\multicolumn{2}{l}{Industry FE} & & \\checkmark & \\checkmark \\\\ \n")
cat("\\multicolumn{2}{l}{Input usage controls} & & \\checkmark & \\checkmark \\\\ \n")
cat("\\multicolumn{2}{l}{Firm FE}     & &             & \\checkmark \\\\ \n")
cat("\\addlinespace \\hline \\addlinespace \n")
cat("\\multicolumn{2}{l}{Observations} & & ",fp(nrow(match))," & ",fp(nrow(match))," \\\\ \n")
cat("\\bottomrule \n")
cat("\\end{tabular} \n")
cat("\\end{adjustbox} \n")
cat("\\justify \\footnotesize \\emph{Notes:} This table reports results of regressing different network-related metrics on markups. Each row captures an independent regression, i.e. the respective variable is regressed alone on markups, with the inclusion of the respective fixed effects and controls reported at the bottom. Concretely, Column (2) differs from Column (1) only through the inclusion of firm fixed effects. Interquartile ranges (IQR) are reported in levels even for the variables regressed in logs. Standard errors clustered at the firm level reported in parentheses. Significance levels given by: *** p $<$ 0.001, ** p $<$ 0.01, * p $<$ 0.05. \n")
cat("\\end{table} \n")
sink()

