#///////////////////////////////////////////////////////////////////////////////
# File name:		table_2_regressions_FEs.R
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

reg_fe_1 <- list()
reg_fe_2 <- list()
reg_fe_3 <- list()
reg_fe_4 <- list()
reg_fe_5 <- list()

# Iterate for each network variable
for (var in levels(match_melt$variable)) {
    
    cat("Now estimating regression for variable:", var,"...\n")
    
    if (var %in% c("sf_i_c","sf_i_w","act_n_buyers","act_n_sectors","act_n_provinces")) {
        reg_fe_1[[var]] <- feols(mu ~ log(value),                           data=match_melt[variable==var], panel.id = c("id","year"), cluster = "id")
        reg_fe_2[[var]] <- feols(mu ~ log(value) | year,                    data=match_melt[variable==var], panel.id = c("id","year"), cluster = "id")
        reg_fe_3[[var]] <- feols(mu ~ log(value) | year + ind,              data=match_melt[variable==var], panel.id = c("id","year"), cluster = "id")
        reg_fe_4[[var]] <- feols(mu ~ log(value) + V + K | year + ind,      data=match_melt[variable==var], panel.id = c("id","year"), cluster = "id")
        reg_fe_5[[var]] <- feols(mu ~ log(value) + V + K | year + ind + id, data=match_melt[variable==var], panel.id = c("id","year"), cluster = "id")
    } else {
        reg_fe_1[[var]] <- feols(mu ~ value,                           data=match_melt[variable==var], panel.id = c("id","year"), cluster = "id")
        reg_fe_2[[var]] <- feols(mu ~ value | year,                    data=match_melt[variable==var], panel.id = c("id","year"), cluster = "id")
        reg_fe_3[[var]] <- feols(mu ~ value | year + ind,              data=match_melt[variable==var], panel.id = c("id","year"), cluster = "id")
        reg_fe_4[[var]] <- feols(mu ~ value + V + K | year + ind,      data=match_melt[variable==var], panel.id = c("id","year"), cluster = "id")
        reg_fe_5[[var]] <- feols(mu ~ value + V + K | year + ind + id, data=match_melt[variable==var], panel.id = c("id","year"), cluster = "id")
    }

}

#///////////////////////////////////////////////////////////////////////////////
#----                           OUTPUT TABLE                                ----
#///////////////////////////////////////////////////////////////////////////////

# Put results into table with different FEs
sink(paste0(pathTab,sysdate,"_table_2_regression_results_main_FEs.tex"))
cat("\\begin{table}[!htbp]\\centering \n")
cat("\\caption{\\label{tab:EstimatedResults_main_FE} Regression Results Decomposition} \n")
cat("\\begin{adjustbox}{width=\\textwidth,center} \n")
cat("\\begin{tabular}{lcrrrrr}")
cat("\\toprule \n")
cat(" & IQR & \\multicolumn{1}{c}{(1)} & \\multicolumn{1}{c}{(2)} & \\multicolumn{1}{c}{(3)} & \\multicolumn{1}{c}{(4)} & \\multicolumn{1}{c}{(5)} \\\\ \n")
cat("\\addlinespace \\hline \\addlinespace \n")
cat("Within Industry & \\multirow{2}{*}{",fp(IQR$wsvi_i_c,2),"} & ", 
    fp(reg_fe_1[["wsvi_i_c"]]$coefficients[2], 4), sign_stars(reg_fe_1[["wsvi_i_c"]]$coeftable[2, 4])," & ", 
    fp(reg_fe_2[["wsvi_i_c"]]$coefficients[1], 4), sign_stars(reg_fe_2[["wsvi_i_c"]]$coeftable[1, 4])," & ", 
    fp(reg_fe_3[["wsvi_i_c"]]$coefficients[1], 4), sign_stars(reg_fe_3[["wsvi_i_c"]]$coeftable[1, 4])," & ", 
    fp(reg_fe_4[["wsvi_i_c"]]$coefficients[1], 4), sign_stars(reg_fe_4[["wsvi_i_c"]]$coeftable[1, 4])," & ", 
    fp(reg_fe_5[["wsvi_i_c"]]$coefficients[1], 4), sign_stars(reg_fe_5[["wsvi_i_c"]]$coeftable[1, 4])," \\\\ \n", sep = "")
cat("Importance & & ",
    fpt(reg_fe_1[["wsvi_i_c"]]$se[2], 4)," & ", 
    fpt(reg_fe_2[["wsvi_i_c"]]$se[1], 4)," & ", 
    fpt(reg_fe_3[["wsvi_i_c"]]$se[1], 4)," & ", 
    fpt(reg_fe_4[["wsvi_i_c"]]$se[1], 4)," & ", 
    fpt(reg_fe_5[["wsvi_i_c"]]$se[1], 4)," \\\\ \n")
cat("\\addlinespace \\hline \\addlinespace \n")
cat("Year FE              & & \\textbf{\\sffamily X} & ",fp(reg_fe_2[["wsvi_i_c"]]$fixef_sizes["year"])," & ",fp(reg_fe_3[["wsvi_i_c"]]$fixef_sizes["year"])," & ",fp(reg_fe_4[["wsvi_i_c"]]$fixef_sizes["year"])," & ",fp(reg_fe_5[["wsvi_i_c"]]$fixef_sizes["year"])," \\\\ \n")
cat("Industry FE          & & \\textbf{\\sffamily X} & \\textbf{\\sffamily X}                             & ",fp(reg_fe_3[["wsvi_i_c"]]$fixef_sizes["ind"]),"  & ",fp(reg_fe_4[["wsvi_i_c"]]$fixef_sizes["ind"]),"  & ",fp(reg_fe_5[["wsvi_i_c"]]$fixef_sizes["ind"]),"  \\\\ \n")
cat("Input usage controls & & \\textbf{\\sffamily X} & \\textbf{\\sffamily X} & \\textbf{\\sffamily X} & \\checkmark  & \\checkmark \\\\ \n")
cat("Firm FE              & & \\textbf{\\sffamily X} & \\textbf{\\sffamily X} & \\textbf{\\sffamily X} & \\textbf{\\sffamily X} & ",fp(reg_fe_5[["wsvi_i_c"]]$fixef_sizes["id"]),"   \\\\ \n")
cat("\\addlinespace \\hline \\addlinespace \n")
cat("Observations & & ",fp(nrow(match))," & ",fp(nrow(match))," & ",fp(nrow(match))," & ",fp(nrow(match))," & ",fp(nrow(match))," \\\\ \n")
cat("Adjusted R-squared & & ",fp(r2(reg_fe_1[["wsvi_i_c"]])["ar2"],2)," & ",fp(r2(reg_fe_2[["wsvi_i_c"]])["ar2"],2)," & ",fp(r2(reg_fe_3[["wsvi_i_c"]])["ar2"],2)," & ",fp(r2(reg_fe_4[["wsvi_i_c"]])["ar2"],2)," & ",fp(r2(reg_fe_5[["wsvi_i_c"]])["ar2"],2)," \\\\ \n")
cat("\\bottomrule \n")
cat("\\end{tabular} \n")
cat("\\end{adjustbox} \n")
cat("\\justify \\footnotesize \\emph{Notes:} This table reports regression results of the unweighted within industry importance on markups. The relationship is decomposed by incrementally increasing controls. For fixed effects, I report the number of groups. Standard errors clustered at the firm level reported in parentheses. Significance levels given by: *** p $<$ 0.001, ** p $<$ 0.01, * p $<$ 0.05. \n")
cat("\\end{table} \n")
sink()

labs = c("Within Industry Importance",
         "Across Industry Importance",
         "Seller Reciprocity",
         "Reciprocity Degree",
         "log(Selling Frequency)",
         "Unique Seller",
         "Competition Intensity",
         "Local Focus",
         "Vertical Focus",
         "Buyer Specialisation", 
         "log(Unique Buyer Count)", 
         "log(Unique Industry Count)",
         "log(Unique Province Count)")

# Appendix table - UNWEIGHTED
sink(paste0(pathTab,sysdate,"_table_2_regression_results_appendix_FEs_unweighted.tex"))
cat("\\begin{table}[!htbp]\\centering \n")
cat("\\caption{\\label{tab:EstimatedResults_appendix_FE_unweighted} Regression Results Decomposition -- Unweighted Metrics} \n")
cat("\\begin{adjustbox}{width=\\textwidth,center} \n")
cat("\\begin{tabular}{lcrrrrr}")
cat("\\toprule \n")
cat(" & IQR & \\multicolumn{1}{c}{(1)} & \\multicolumn{1}{c}{(2)} & \\multicolumn{1}{c}{(3)} & \\multicolumn{1}{c}{(4)} & \\multicolumn{1}{c}{(5)} \\\\ \n")
cat("\\addlinespace \\hline \\addlinespace \n")
i = 1
vars = levels(match_melt$variable)[levels(match_melt$variable) %in% c("wsvi_i_c","bvi_i_c","sr_i_c","ri_i_c","sf_i_c","us_i_c","ci_i_c","lo_i_c","vt_i_c","sp_i_c","act_n_buyers","act_n_sectors","act_n_provinces")]
for (var in vars) {
    cat(labs[i], " & \\multirow{2}{*}{",fp(IQR_melt[variable==var]$value,2),"} & ", 
        fp(reg_fe_1[[var]]$coefficients[2], 4), sign_stars(reg_fe_1[[var]]$coeftable[2, 4])," & ", 
        fp(reg_fe_2[[var]]$coefficients[1], 4), sign_stars(reg_fe_2[[var]]$coeftable[1, 4])," & ", 
        fp(reg_fe_3[[var]]$coefficients[1], 4), sign_stars(reg_fe_3[[var]]$coeftable[1, 4])," & ", 
        fp(reg_fe_4[[var]]$coefficients[1], 4), sign_stars(reg_fe_4[[var]]$coeftable[1, 4])," & ", 
        fp(reg_fe_5[[var]]$coefficients[1], 4), sign_stars(reg_fe_5[[var]]$coeftable[1, 4])," \\\\ \n", sep = "")
    cat(" & & ",
        fpt(reg_fe_1[[var]]$se[2], 4)," & ", 
        fpt(reg_fe_2[[var]]$se[1], 4)," & ", 
        fpt(reg_fe_3[[var]]$se[1], 4)," & ", 
        fpt(reg_fe_4[[var]]$se[1], 4)," & ", 
        fpt(reg_fe_5[[var]]$se[1], 4)," \\\\ \n")
    cat("\\addlinespace \\hline \\addlinespace \n")
    i = i+1
}
cat("\\addlinespace \\hline \\addlinespace \n")
cat("Year FE              & & \\textbf{\\sffamily X} & ",fp(reg_fe_2[["wsvi_i_c"]]$fixef_sizes["year"])," & ",fp(reg_fe_3[["wsvi_i_c"]]$fixef_sizes["year"])," & ",fp(reg_fe_4[["wsvi_i_c"]]$fixef_sizes["year"])," & ",fp(reg_fe_5[["wsvi_i_c"]]$fixef_sizes["year"])," \\\\ \n")
cat("Industry FE          & & \\textbf{\\sffamily X} & \\textbf{\\sffamily X}                             & ",fp(reg_fe_3[["wsvi_i_c"]]$fixef_sizes["ind"]),"  & ",fp(reg_fe_4[["wsvi_i_c"]]$fixef_sizes["ind"]),"  & ",fp(reg_fe_5[["wsvi_i_c"]]$fixef_sizes["ind"]),"  \\\\ \n")
cat("Input usage controls & & \\textbf{\\sffamily X} & \\textbf{\\sffamily X} & \\textbf{\\sffamily X} & \\checkmark  & \\checkmark \\\\ \n")
cat("Firm FE              & & \\textbf{\\sffamily X} & \\textbf{\\sffamily X} & \\textbf{\\sffamily X} & \\textbf{\\sffamily X} & ",fp(reg_fe_5[["wsvi_i_c"]]$fixef_sizes["id"]),"   \\\\ \n")
cat("Observations & & ",fp(nrow(match))," & ",fp(nrow(match))," & ",fp(nrow(match))," & ",fp(nrow(match))," & ",fp(nrow(match))," \\\\ \n")
cat("\\bottomrule \n")
cat("\\end{tabular} \n")
cat("\\end{adjustbox} \n")
cat("\\justify \\footnotesize \\emph{Notes:} This table reports regression results of the unweighted network metrics on markups, decomposing the relationships by incrementally increasing controls. Each row captures an independent regression, i.e. the respective variable is regressed alone on markups, with the inclusion of the respective fixed effects and controls reported at the bottom. For fixed effects, I report the number of groups. Interquartile ranges (IQR) are reported in levels even for the variables regressed in logs. Standard errors clustered at the firm level reported in parentheses. Significance levels given by: *** p $<$ 0.001, ** p $<$ 0.01, * p $<$ 0.05. \n")
cat("\\end{table} \n")
sink()

# Appendix table - WEIGHTED
sink(paste0(pathTab,sysdate,"_table_2_regression_results_appendix_FEs_weighted.tex"))
cat("\\begin{table}[!htbp]\\centering \n")
cat("\\caption{\\label{tab:EstimatedResults_appendix_FE_weighted} Regression Results Decomposition -- Weighted Metrics} \n")
cat("\\begin{adjustbox}{width=\\textwidth,center} \n")
cat("\\begin{tabular}{lcrrrrr}")
cat("\\toprule \n")
cat(" & IQR & \\multicolumn{1}{c}{(1)} & \\multicolumn{1}{c}{(2)} & \\multicolumn{1}{c}{(3)} & \\multicolumn{1}{c}{(4)} & \\multicolumn{1}{c}{(5)} \\\\ \n")
cat("\\addlinespace \\hline \\addlinespace \n")
i = 1
vars = levels(match_melt$variable)[levels(match_melt$variable) %in% c("wsvi_i_w","bvi_i_w","sr_i_w","ri_i_w","sf_i_w","us_i_w","ci_i_w","lo_i_w","vt_i_w","sp_i_w","act_n_buyers","act_n_sectors","act_n_provinces")]
for (var in vars) {
    cat(labs[i], " & \\multirow{2}{*}{",fp(IQR_melt[variable==var]$value,2),"} & ", 
        fp(reg_fe_1[[var]]$coefficients[2], 4), sign_stars(reg_fe_1[[var]]$coeftable[2, 4])," & ", 
        fp(reg_fe_2[[var]]$coefficients[1], 4), sign_stars(reg_fe_2[[var]]$coeftable[1, 4])," & ", 
        fp(reg_fe_3[[var]]$coefficients[1], 4), sign_stars(reg_fe_3[[var]]$coeftable[1, 4])," & ", 
        fp(reg_fe_4[[var]]$coefficients[1], 4), sign_stars(reg_fe_4[[var]]$coeftable[1, 4])," & ", 
        fp(reg_fe_5[[var]]$coefficients[1], 4), sign_stars(reg_fe_5[[var]]$coeftable[1, 4])," \\\\ \n", sep = "")
    cat(" & & ",
        fpt(reg_fe_1[[var]]$se[2], 4)," & ", 
        fpt(reg_fe_2[[var]]$se[1], 4)," & ", 
        fpt(reg_fe_3[[var]]$se[1], 4)," & ", 
        fpt(reg_fe_4[[var]]$se[1], 4)," & ", 
        fpt(reg_fe_5[[var]]$se[1], 4)," \\\\ \n")
    cat("\\addlinespace \\hline \\addlinespace \n")
    i = i+1
}
cat("Year FE              & & \\textbf{\\sffamily X} & ",fp(reg_fe_2[["wsvi_i_w"]]$fixef_sizes["year"])," & ",fp(reg_fe_3[["wsvi_i_w"]]$fixef_sizes["year"])," & ",fp(reg_fe_4[["wsvi_i_w"]]$fixef_sizes["year"])," & ",fp(reg_fe_5[["wsvi_i_w"]]$fixef_sizes["year"])," \\\\ \n")
cat("Industry FE          & & \\textbf{\\sffamily X} & \\textbf{\\sffamily X}                             & ",fp(reg_fe_3[["wsvi_i_w"]]$fixef_sizes["ind"]),"  & ",fp(reg_fe_4[["wsvi_i_w"]]$fixef_sizes["ind"]),"  & ",fp(reg_fe_5[["wsvi_i_w"]]$fixef_sizes["ind"]),"  \\\\ \n")
cat("Input usage controls & & \\textbf{\\sffamily X} & \\textbf{\\sffamily X} & \\textbf{\\sffamily X} & \\checkmark  & \\checkmark \\\\ \n")
cat("Firm FE              & & \\textbf{\\sffamily X} & \\textbf{\\sffamily X} & \\textbf{\\sffamily X} & \\textbf{\\sffamily X} & ",fp(reg_fe_5[["wsvi_i_w"]]$fixef_sizes["id"]),"   \\\\ \n")
cat("\\addlinespace \\hline \\addlinespace \n")
cat("Observations & & ",fp(nrow(match))," & ",fp(nrow(match))," & ",fp(nrow(match))," & ",fp(nrow(match))," & ",fp(nrow(match))," \\\\ \n")
cat("\\bottomrule \n")
cat("\\end{tabular} \n")
cat("\\end{adjustbox} \n")
cat("\\justify \\footnotesize \\emph{Notes:} This table reports regression results of the weighted network metrics on markups, decomposing the relationships by incrementally increasing controls. Each row captures an independent regression, i.e. the respective variable is regressed alone on markups, with the inclusion of the respective fixed effects and controls reported at the bottom. For fixed effects, I report the number of groups. Interquartile ranges (IQR) are reported in levels even for the variables regressed in logs. Standard errors clustered at the firm level reported in parentheses. Significance levels given by: *** p $<$ 0.001, ** p $<$ 0.01, * p $<$ 0.05. \n")
cat("\\end{table} \n")
sink()


