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
markups <- markups[!is.na(mu_v_dlw_tl)] # exclude missing obs

# Create dummy for firm size expansion (based on cost)
markups[, V_lag := shift(V), by="id"]
markups[, firm_size_expansion := ifelse(V>V_lag, 1, 0)]

# Since we are only focused on are main specification, drop all other variables
markups[, c("mu_m_dlw_cd", "mu_m_dlw_tl", "mu_v_dlw_cd", "M", "L", "V_lag") := NULL]
setnames(markups, "mu_v_dlw_tl", "mu")

# Match markups estimation sample and network sample
match <- merge(markups, network_metrics, by.x=c("id","year","ind"), by.y=c("id_seller","year","seller_sec"))

# Calculate IQR for network metrics
IQR <- dcast(data = match[,.SD,.SDcols = !c("id", "year", "ind", "ind_group", "firm_size_expansion", "mu", "V", "alpha_v", "seller_weight", "pot_n_buyers")],
             formula = .~.,
             fun = IQR,
             value.var=colnames(match[,.SD,.SDcols = !c("id", "year", "ind", "ind_group", "firm_size_expansion", "mu", "V", "alpha_v", "seller_weight", "pot_n_buyers")]))

# Reshape data to allow loops
match_melt <- melt(data = match[,.SD,.SDcols=!c("pot_n_buyers")],
                   id.vars = c("id","year","ind","ind_group","mu","V","firm_size_expansion","seller_weight"))
IQR_melt   <- melt(data = IQR,
                   id.vars = c("."))
IQR_melt[, . := NULL]
IQR[, . := NULL]

#///////////////////////////////////////////////////////////////////////////////
#----                 1 - EFFECT OF FIXED EFFECTS                ----
#///////////////////////////////////////////////////////////////////////////////

# Iterate for each network variable
for (var in levels(match_melt$variable)) {
    
    reg_fe_1 <- feols(mu ~ value,                   data=match_melt[variable==var], panel.id = c("id","year"), cluster = "id")
    reg_fe_2 <- feols(mu ~ value | year,            data=match_melt[variable==var], panel.id = c("id","year"), cluster = "id")
    reg_fe_3 <- feols(mu ~ value | year + ind,      data=match_melt[variable==var], panel.id = c("id","year"), cluster = "id")
    reg_fe_4 <- feols(mu ~ value | year + ind + id, data=match_melt[variable==var], panel.id = c("id","year"), cluster = "id")
    
    # Put results into table with different FEs
    sink(paste0(pathTab,sysdate,"_table_3_regression_results_",var,"_FEs.tex"))
    cat("\\begin{table}[!htbp]\\centering \n")
    cat("\\caption{\\label{tab:EstimatedResults_",var,"_FE} Regression Results Decomposition} \n")
    cat("\\begin{adjustbox}{width=0.5\\columnwidth,center} \n")
    cat("\\begin{tabular}{lrrrr}")
    cat("\\toprule \n")
    cat(" & \\multicolumn{1}{c}{(1)} & \\multicolumn{1}{c}{(2)} & \\multicolumn{1}{c}{(3)} & \\multicolumn{1}{c}{(4)} \\\\ \n")
    cat("\\addlinespace \\hline \\addlinespace \n")
    cat("Network Metric & ", 
        fp(reg_fe_1$coefficients[2], 4), sign_stars(reg_fe_1$coeftable[2, 4])," & ", 
        fp(reg_fe_2$coefficients, 4), sign_stars(reg_fe_2$coeftable[, 4])," & ", 
        fp(reg_fe_3$coefficients, 4), sign_stars(reg_fe_3$coeftable[, 4])," & ", 
        fp(reg_fe_4$coefficients, 4), sign_stars(reg_fe_4$coeftable[, 4])," \\\\ \n", sep = "")
    cat(" & ",        fpt(reg_fe_1$se[2], 4),          " & ", fpt(reg_fe_2$se, 4),          " & ", fpt(reg_fe_3$se, 4),          " & ", fpt(reg_fe_4$se, 4),          " \\\\ \n")
    cat("\\addlinespace \\hline \\addlinespace \n")
    cat("Year FE     & \\textbf{\\sffamily X} & ",fp(reg_fe_2$fixef_sizes["year"])," & ",fp(reg_fe_3$fixef_sizes["year"])," & ",fp(reg_fe_4$fixef_sizes["year"])," \\\\ \n")
    cat("Industry FE & \\textbf{\\sffamily X} & \\textbf{\\sffamily X}               & ",fp(reg_fe_3$fixef_sizes["ind"]),"  & ",fp(reg_fe_4$fixef_sizes["ind"]),"  \\\\ \n")
    cat("Firm FE     & \\textbf{\\sffamily X} & \\textbf{\\sffamily X}               & \\textbf{\\sffamily X}               & ",fp(reg_fe_4$fixef_sizes["id"]),"   \\\\ \n")
    cat("\\addlinespace \\hline \\addlinespace \n")
    cat("Network Metric IQR & ",fp(IQR_melt[variable==var]$value,2)," & ",fp(IQR_melt[variable==var]$value,2)," & ",fp(IQR_melt[variable==var]$value,2)," & ",fp(IQR_melt[variable==var]$value,2)," \\\\ \n")
    cat("Observations & ",fp(reg_fe_1$nobs)," & ",fp(reg_fe_2$nobs)," & ",fp(reg_fe_3$nobs)," & ",fp(reg_fe_4$nobs)," \\\\ \n")
    cat("Adjusted R-squared & ",fp(r2(reg_fe_1)["ar2"],2)," & ",fp(r2(reg_fe_2)["ar2"],2)," & ",fp(r2(reg_fe_3)["ar2"],2)," & ",fp(r2(reg_fe_4)["ar2"],2)," \\\\ \n")
    cat("\\bottomrule \n")
    cat("\\end{tabular} \n")
    cat("\\end{adjustbox} \n")
    cat("\\justify \\footnotesize \\emph{Notes:} This table reports regression results of the XXX NETWORK METRIC XXX on markups. The relationship is decomposed by incrementally increasing controls. Standard errors clustered at the firm-level reported in parentheses. Significance levels given by: *** p $<$ 0.001, ** p $<$ 0.01, * p $<$ 0.05. \n")
    cat("\\end{table} \n")
    sink()
    
}

# Create table for main text
reg_fe_1 <- feols(mu ~ wsvi_i_c,                   data=match, panel.id = c("id","year"), cluster = "id")
reg_fe_2 <- feols(mu ~ wsvi_i_c | year,            data=match, panel.id = c("id","year"), cluster = "id")
reg_fe_3 <- feols(mu ~ wsvi_i_c | year + ind,      data=match, panel.id = c("id","year"), cluster = "id")
reg_fe_4 <- feols(mu ~ wsvi_i_c | year + ind + id, data=match, panel.id = c("id","year"), cluster = "id")

# Put results into table with different FEs
sink(paste0(pathTab,sysdate,"_table_3_regression_results_main_FEs.tex"))
cat("\\begin{table}[!htbp]\\centering \n")
cat("\\caption{\\label{tab:EstimatedResults_main_FE} Regression Results Decomposition} \n")
cat("\\begin{tabular}{lrrrr}")
cat("\\toprule \n")
cat(" & \\multicolumn{1}{c}{(1)} & \\multicolumn{1}{c}{(2)} & \\multicolumn{1}{c}{(3)} & \\multicolumn{1}{c}{(4)} \\\\ \n")
cat("\\addlinespace \\hline \\addlinespace \n")
cat("Within-Industry & ", 
    fp(reg_fe_1$coefficients[2], 4), sign_stars(reg_fe_1$coeftable[2, 4])," & ", 
    fp(reg_fe_2$coefficients, 4), sign_stars(reg_fe_2$coeftable[, 4])," & ", 
    fp(reg_fe_3$coefficients, 4), sign_stars(reg_fe_3$coeftable[, 4])," & ", 
    fp(reg_fe_4$coefficients, 4), sign_stars(reg_fe_4$coeftable[, 4])," \\\\ \n", sep = "")
cat("Importance & ",        fpt(reg_fe_1$se[2], 4),          " & ", fpt(reg_fe_2$se, 4),          " & ", fpt(reg_fe_3$se, 4),          " & ", fpt(reg_fe_4$se, 4),          " \\\\ \n")
cat("\\addlinespace \\hline \\addlinespace \n")
cat("Year FE     & \\textbf{\\sffamily X} & ",fp(reg_fe_2$fixef_sizes["year"])," & ",fp(reg_fe_3$fixef_sizes["year"])," & ",fp(reg_fe_4$fixef_sizes["year"])," \\\\ \n")
cat("Industry FE & \\textbf{\\sffamily X} & \\textbf{\\sffamily X}               & ",fp(reg_fe_3$fixef_sizes["ind"]),"  & ",fp(reg_fe_4$fixef_sizes["ind"]),"  \\\\ \n")
cat("Firm FE     & \\textbf{\\sffamily X} & \\textbf{\\sffamily X}               & \\textbf{\\sffamily X}               & ",fp(reg_fe_4$fixef_sizes["id"]),"   \\\\ \n")
cat("\\addlinespace \\hline \\addlinespace \n")
# cat("Network Metric IQR & ",fp(IQR$wsvi_i_c,2)," & ",fp(IQR$wsvi_i_c,2)," & ",fp(IQR$wsvi_i_c,2)," & ",fp(IQR$wsvi_i_c,2)," \\\\ \n")
cat("Observations & ",fp(reg_fe_1$nobs)," & ",fp(reg_fe_2$nobs)," & ",fp(reg_fe_3$nobs)," & ",fp(reg_fe_4$nobs)," \\\\ \n")
cat("Adjusted R-squared & ",fp(r2(reg_fe_1)["ar2"],2)," & ",fp(r2(reg_fe_2)["ar2"],2)," & ",fp(r2(reg_fe_3)["ar2"],2)," & ",fp(r2(reg_fe_4)["ar2"],2)," \\\\ \n")
cat("\\bottomrule \n")
cat("\\end{tabular} \n")
cat("\\justify \\footnotesize \\emph{Notes:} This table reports regression results of the conditional unweighted within-industry importance on markups. The relationship is decomposed by incrementally increasing controls. Standard errors clustered at the firm-level reported in parentheses. Significance levels given by: *** p $<$ 0.001, ** p $<$ 0.01, * p $<$ 0.05. \n")
cat("\\end{table} \n")
sink()


#///////////////////////////////////////////////////////////////////////////////
#----                 2 - RUN REGRESSIONS ON IMPORTANCE                ----
#///////////////////////////////////////////////////////////////////////////////

# Run regressions for network variables relating to idea of diversification
reg_impo_1 <- feols(mu ~ wsvi_i_c + V | ind + year, data=match, panel.id = c("id","year"), cluster = "id")
reg_impo_2 <- feols(mu ~ wsvi_i_w + V | ind + year, data=match, panel.id = c("id","year"), cluster = "id")
reg_impo_3 <- feols(mu ~ wsvi_i_u + V | ind + year, data=match, panel.id = c("id","year"), cluster = "id")
reg_impo_4 <- feols(mu ~ bvi_i_c  + V | ind + year, data=match, panel.id = c("id","year"), cluster = "id")
reg_impo_5 <- feols(mu ~ bvi_i_w  + V | ind + year, data=match, panel.id = c("id","year"), cluster = "id")
reg_impo_6 <- feols(mu ~ bvi_i_u  + V | ind + year, data=match, panel.id = c("id","year"), cluster = "id")

reg_qual_1 <- feols(mu ~ sr_i_c + V | ind + year, data=match, panel.id = c("id","year"), cluster = "id")
reg_qual_2 <- feols(mu ~ sr_i_w + V | ind + year, data=match, panel.id = c("id","year"), cluster = "id")
reg_qual_3 <- feols(mu ~ sr_i_u + V | ind + year, data=match, panel.id = c("id","year"), cluster = "id")
reg_qual_4 <- feols(mu ~ log(ri_i_c) + V | ind + year, data=match, panel.id = c("id","year"), cluster = "id")
reg_qual_5 <- feols(mu ~ log(ri_i_w) + V | ind + year, data=match, panel.id = c("id","year"), cluster = "id")
reg_qual_6 <- feols(mu ~ log(ri_i_u) + V | ind + year, data=match, panel.id = c("id","year"), cluster = "id")

reg_comp_1 <- feols(mu ~ us_i_c + V | ind + year, data=match, panel.id = c("id","year"), cluster = "id")
reg_comp_2 <- feols(mu ~ us_i_w + V | ind + year, data=match, panel.id = c("id","year"), cluster = "id")
reg_comp_3 <- feols(mu ~ us_i_u + V | ind + year, data=match, panel.id = c("id","year"), cluster = "id")
reg_comp_4 <- feols(mu ~ ci_i_c + V | ind + year, data=match, panel.id = c("id","year"), cluster = "id")
reg_comp_5 <- feols(mu ~ ci_i_w + V | ind + year, data=match, panel.id = c("id","year"), cluster = "id")
reg_comp_6 <- feols(mu ~ ci_i_u + V | ind + year, data=match, panel.id = c("id","year"), cluster = "id")

reg_focu_1 <- feols(mu ~ lo_i_c + V | ind + year, data=match, panel.id = c("id","year"), cluster = "id")
reg_focu_2 <- feols(mu ~ lo_i_w + V | ind + year, data=match, panel.id = c("id","year"), cluster = "id")
reg_focu_3 <- feols(mu ~ ho_i_c + V | ind + year, data=match, panel.id = c("id","year"), cluster = "id")
reg_focu_4 <- feols(mu ~ ho_i_w + V | ind + year, data=match, panel.id = c("id","year"), cluster = "id")

reg_dive_1 <- feols(mu ~ log(act_n_buyers)    + V | ind + year, data=match, panel.id = c("id","year"), cluster = "id")
reg_dive_2 <- feols(mu ~ log(act_n_sectors)   + V | ind + year, data=match, panel.id = c("id","year"), cluster = "id")
reg_dive_3 <- feols(mu ~ log(act_n_provinces) + V | ind + year, data=match, panel.id = c("id","year"), cluster = "id")

sink(paste0(pathTab,sysdate,"_table_4_regression_results_all_metrics.tex"))
cat("\\begin{table}[!htbp]\\centering \n")
cat("\\caption{\\label{tab:EstimatedResults_All} Regression Results for All Network Metrics} \n")
cat("\\begin{adjustbox}{height=0.4\\textheight,center} \n")
cat("\\begin{tabular}{llccc}")
cat("\\toprule \n")
cat(" & & (1) & (2) & (3) \\\\ \n")
cat("\\addlinespace \\hline \\addlinespace \n")

cat("\\multicolumn{1}{l|}{\\multirow{5}{0.2\\textwidth}{Importance for buyer}} & Within-Industry & ", 
    fp(reg_impo_1$coefficients[1], 4), sign_stars(reg_impo_1$coeftable[1, 4])," & ", 
    fp(reg_impo_2$coefficients[1], 4), sign_stars(reg_impo_2$coeftable[1, 4])," & ", 
    fp(reg_impo_3$coefficients[1], 4), sign_stars(reg_impo_3$coeftable[1, 4])," \\\\ \n")
cat("\\multicolumn{1}{l|}{ } & Importance (\\%) & ", 
    fpt(reg_impo_1$se[1], 4)," & ",
    fpt(reg_impo_2$se[1], 4)," & ",
    fpt(reg_impo_3$se[1], 4)," \\\\ \n")
cat("\\addlinespace \\cline{2-5} \\addlinespace \n")
cat("\\multicolumn{1}{l|}{ } & Across-Industry & ", 
    fp(reg_impo_4$coefficients[1], 4), sign_stars(reg_impo_4$coeftable[1, 4])," & ", 
    fp(reg_impo_5$coefficients[1], 4), sign_stars(reg_impo_5$coeftable[1, 4])," & ", 
    fp(reg_impo_6$coefficients[1], 4), sign_stars(reg_impo_6$coeftable[1, 4])," \\\\ \n")
cat("\\multicolumn{1}{l|}{ } & Importance (\\%) & ", 
    fpt(reg_impo_4$se[1], 4)," & ",
    fpt(reg_impo_5$se[1], 4)," & ",
    fpt(reg_impo_6$se[1], 4)," \\\\ \n")
cat("\\addlinespace \\hline \\addlinespace \n")

cat("\\multicolumn{1}{l|}{\\multirow{5}{0.15\\textwidth}{Relationship quality}} & Seller & ", 
    fp(reg_qual_1$coefficients[1], 4), sign_stars(reg_qual_1$coeftable[1, 4])," & ", 
    fp(reg_qual_2$coefficients[1], 4), sign_stars(reg_qual_2$coeftable[1, 4])," & ", 
    fp(reg_qual_3$coefficients[1], 4), sign_stars(reg_qual_3$coeftable[1, 4])," \\\\ \n")
cat("\\multicolumn{1}{l|}{ } & Reciprocity (\\%) & ", 
    fpt(reg_qual_1$se[1], 4)," & ",
    fpt(reg_qual_2$se[1], 4)," & ",
    fpt(reg_qual_3$se[1], 4)," \\\\ \n")
cat("\\addlinespace \\cline{2-5} \\addlinespace \n")
cat("\\multicolumn{1}{l|}{ } & Selling & ", 
    fp(reg_qual_4$coefficients[1], 4), sign_stars(reg_qual_4$coeftable[1, 4])," & ", 
    fp(reg_qual_5$coefficients[1], 4), sign_stars(reg_qual_5$coeftable[1, 4])," & ", 
    fp(reg_qual_6$coefficients[1], 4), sign_stars(reg_qual_6$coeftable[1, 4])," \\\\ \n")
cat("\\multicolumn{1}{l|}{ } & Intensity (log) & ", 
    fpt(reg_qual_4$se[1], 4)," & ",
    fpt(reg_qual_5$se[1], 4)," & ",
    fpt(reg_qual_6$se[1], 4)," \\\\ \n")
cat("\\addlinespace \\hline \\addlinespace \n")

cat("\\multicolumn{1}{l|}{\\multirow{5}{0.15\\textwidth}{Competition}} & Unique & ", 
    fp(reg_comp_1$coefficients[1], 4), sign_stars(reg_comp_1$coeftable[1, 4])," & ", 
    fp(reg_comp_2$coefficients[1], 4), sign_stars(reg_comp_2$coeftable[1, 4])," & ", 
    fp(reg_comp_3$coefficients[1], 4), sign_stars(reg_comp_3$coeftable[1, 4])," \\\\ \n")
cat("\\multicolumn{1}{l|}{ } & Seller (\\%) & ", 
    fpt(reg_comp_1$se[1], 4)," & ",
    fpt(reg_comp_2$se[1], 4)," & ",
    fpt(reg_comp_3$se[1], 4)," \\\\ \n")
cat("\\addlinespace \\cline{2-5} \\addlinespace \n")
cat("\\multicolumn{1}{l|}{ } & Competition & ", 
    fp(reg_comp_4$coefficients[1], 4), sign_stars(reg_comp_4$coeftable[1, 4])," & ", 
    fp(reg_comp_5$coefficients[1], 4), sign_stars(reg_comp_5$coeftable[1, 4])," & ", 
    fp(reg_comp_6$coefficients[1], 4), sign_stars(reg_comp_6$coeftable[1, 4])," \\\\ \n")
cat("\\multicolumn{1}{l|}{ } & Intensity (\\%) & ", 
    fpt(reg_comp_4$se[1], 4)," & ",
    fpt(reg_comp_5$se[1], 4)," & ",
    fpt(reg_comp_6$se[1], 4)," \\\\ \n")
cat("\\addlinespace \\hline \\addlinespace \n")

cat("\\multicolumn{1}{l|}{\\multirow{5}{0.15\\textwidth}{Clientele focus}} & Non-Local & ", 
    fp(reg_focu_1$coefficients[1], 4), sign_stars(reg_focu_1$coeftable[1, 4])," & ", 
    fp(reg_focu_2$coefficients[1], 4), sign_stars(reg_focu_2$coeftable[1, 4])," & \\\\ \n")
cat("\\multicolumn{1}{l|}{ } & Orientation (\\%) & ", 
    fpt(reg_focu_1$se[1], 4)," & ",
    fpt(reg_focu_2$se[1], 4)," & \\\\ \n")
cat("\\addlinespace \\cline{2-5} \\addlinespace \n")
cat("\\multicolumn{1}{l|}{ } & Non-Horizontal & ", 
    fp(reg_focu_3$coefficients[1], 4), sign_stars(reg_focu_3$coeftable[1, 4])," & ", 
    fp(reg_focu_4$coefficients[1], 4), sign_stars(reg_focu_4$coeftable[1, 4])," & \\\\ \n")
cat("\\multicolumn{1}{l|}{ } & Orientation (\\%) & ", 
    fpt(reg_focu_3$se[1], 4)," & ",
    fpt(reg_focu_4$se[1], 4)," & \\\\ \n")
cat("\\addlinespace \\hline \\addlinespace \n")

cat("\\multicolumn{1}{l|}{\\multirow{8}{0.15\\textwidth}{Diversification}} & Unique & ", 
    fp(reg_dive_1$coefficients[1], 4), sign_stars(reg_dive_1$coeftable[1, 4])," & & \\\\ \n")
cat("\\multicolumn{1}{l|}{ } & buyer count (log) & ", 
    fpt(reg_dive_1$se[1], 4)," & & \\\\ \n")
cat("\\addlinespace \\cline{2-5} \\addlinespace \n")
cat("\\multicolumn{1}{l|}{ } & Unique & ", 
    fp(reg_dive_2$coefficients[1], 4), sign_stars(reg_dive_2$coeftable[1, 4])," & & \\\\ \n")
cat("\\multicolumn{1}{l|}{ } & industry count (log) & ", 
    fpt(reg_dive_2$se[1], 4)," & & \\\\ \n")
cat("\\addlinespace \\cline{2-5} \\addlinespace \n")
cat("\\multicolumn{1}{l|}{ } & Unique & ", 
    fp(reg_dive_3$coefficients[1], 4), sign_stars(reg_dive_3$coeftable[1, 4])," & & \\\\ \n")
cat("\\multicolumn{1}{l|}{ } & province count (log) & ", 
    fpt(reg_dive_3$se[1], 4)," & & \\\\ \n")

cat("\\addlinespace \\hline \\addlinespace \n")
cat("\\multicolumn{2}{l}{Year FE}     & \\checkmark & \\checkmark & \\checkmark \\\\ \n")
cat("\\multicolumn{2}{l}{Industry FE} & \\checkmark & \\checkmark & \\checkmark \\\\ \n")
cat("\\multicolumn{2}{l}{Input usage control} & \\checkmark & \\checkmark & \\checkmark \\\\ \n")
cat("\\multicolumn{2}{l}{Metric construction:} & & & \\\\ \n")
cat("\\multicolumn{2}{l}{\\quad Conditional on selling} & \\checkmark & \\checkmark & \\\\ \n")
cat("\\multicolumn{2}{l}{\\quad Weighted by sale value} &             & \\checkmark & \\\\ \n")
cat("\\addlinespace \\hline \\addlinespace \n")
cat("\\multicolumn{2}{l}{Observations} & ",fp(nrow(match))," & ",fp(nrow(match))," & ",fp(nrow(match))," \\\\ \n")
cat("\\bottomrule \n")
cat("\\end{tabular} \n")
cat("\\end{adjustbox} \n")
cat("\\justify \\footnotesize \\emph{Notes:} This table reports results of regressing different network-related metrics on markups. Each row captures an independent regression, i.e. the respective variable is regressed alone on markups, with the inclusion of year and industry fixed effects. Column (1) specifications use the baseline definition of the constructed network metrics, i.e. conditional on selling and unweighted. Column (2) weights each seller-buyer interaction by the value of the sale when averaging the metrics at the seller level. Column (3) constructs an unconditional version by considering all the potential buyers of a given seller instead of only the ones with whom a transaction actually takes place. Standard errors clustered at the firm-level reported in parentheses. Significance levels given by: *** p $<$ 0.001, ** p $<$ 0.01, * p $<$ 0.05. \n")
cat("\\end{table} \n")
sink()
























#///////////////////////////////////////////////////////////////////////////////
#----                 2 - PLOT                  ----
#///////////////////////////////////////////////////////////////////////////////

ggplot(data=match, aes(x=wsvi_i_c, y=mu)) + 
    geom_point(aes(colour=ind_group), alpha=0.5, size=0.1) + scale_y_continuous(limits=c(0.75,2)) +
    geom_smooth(method="lm", formula=y~x, color="black") +
    geom_smooth(aes(colour=ind_group), method="lm", formula=y~x)

