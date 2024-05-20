#///////////////////////////////////////////////////////////////////////////////
# File name:		table_5_regressions_industry.R
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
markups <- markups[mu>=0]

# Match markups estimation sample and network sample
match <- merge(markups, network_metrics, by.x=c("id","year","ind"), by.y=c("id_seller","year","seller_sec"))

# Calculate IQR for network metrics
IQR <- dcast(data = match[,.SD,.SDcols = !c("id", "year", "ind", "ind_group", "firm_size_expansion", "firm_size_expansion_dummy", "mu", "V", "alpha_v", "seller_weight", "pot_n_buyers")],
             formula = .~.,
             fun = IQR,
             value.var=colnames(match[,.SD,.SDcols = !c("id", "year", "ind", "ind_group", "firm_size_expansion", "firm_size_expansion_dummy", "mu", "V", "alpha_v", "seller_weight", "pot_n_buyers")]))

# Reshape data to allow loops
match_melt <- melt(data = match[,.SD,.SDcols=!c("pot_n_buyers")],
                   id.vars = c("id","year","ind","ind_group","mu","V","K","firm_size_expansion","firm_size_expansion_dummy","seller_weight"))
IQR_melt   <- melt(data = IQR,
                   id.vars = c("."))
IQR_melt[, . := NULL]
IQR[, . := NULL]

#///////////////////////////////////////////////////////////////////////////////
#----                       REGRESSIONS BY INDUSTRY GROUP                   ----
#///////////////////////////////////////////////////////////////////////////////

# Store coefficients for each industry
coef_by_ind = data.table(ind = sort(unique(match$ind)))
for (indu in sort(unique(match$ind))) {
    mod = feols(mu ~ wsvi_i_c + V + K | year, data=match, subset = match$ind==indu, panel.id = c("id","year"), cluster = "id")
    coef_by_ind[ind==indu, coef := mod$coefficients["wsvi_i_c"]]
    coef_by_ind[ind==indu, se := mod$se["wsvi_i_c"]]
}
coef_by_ind[, lwr := coef - 1.96*se]
coef_by_ind[, upr := coef + 1.96*se]
ggplot(data=coef_by_ind, aes(x=factor(ind), y=coef)) + 
    geom_point() + geom_hline(yintercept = 0) +
    geom_errorbar(aes(ymin = lwr, ymax=upr))

# Store coefficients for each industry group
coef_by_ind_group = data.table(ind_group = sort(unique(match$ind_group)))
for (indug in sort(unique(match$ind_group))) {
    mod = feols(mu ~ wsvi_i_c + V + K | year, data=match, subset = match$ind_group==indug, panel.id = c("id","year"), cluster = "id")
    coef_by_ind_group[ind_group==indug, coef := mod$coefficients["wsvi_i_c"]]
    coef_by_ind_group[ind_group==indug, se := mod$se["wsvi_i_c"]]
}
coef_by_ind_group[, lwr := coef - 1.96*se]
coef_by_ind_group[, upr := coef + 1.96*se]
ggplot(data=coef_by_ind_group, aes(x=factor(ind_group), y=coef)) + 
    geom_point() + geom_hline(yintercept = 0) +
    geom_errorbar(aes(ymin = lwr, ymax=upr), width=0.1)

# Store coefficients for each ISIC section
match[, isic_sec := substr(ind,1,1)]
coef_by_isic_section = data.table(isic_sec = sort(unique(match$isic_sec)))
for (indus in sort(unique(match$isic_sec))) {
    mod = feols(mu ~ wsvi_i_c + V + K | year, data=match, subset = match$isic_sec==indus, panel.id = c("id","year"), cluster = "id")
    coef_by_isic_section[isic_sec==indus, coef := mod$coefficients["wsvi_i_c"]]
    coef_by_isic_section[isic_sec==indus, se := mod$se["wsvi_i_c"]]
}
coef_by_isic_section[, lwr := coef - 1.96*se]
coef_by_isic_section[, upr := coef + 1.96*se]
ggplot(data=coef_by_isic_section, aes(x=factor(isic_sec), y=coef)) + 
    geom_point() + geom_hline(yintercept = 0) +
    geom_errorbar(aes(ymin = lwr, ymax=upr), width=0.1)


# Run regressions
reg01 <- feols(mu ~ wsvi_i_c + V + K | year, data=match, subset = match$ind_group=="AF", panel.id = c("id","year"), cluster = "id")

reg02 <- feols(mu ~ wsvi_i_c | year, data=match, subset = match$ind_group=="G",  panel.id = c("id","year"), cluster = "id")
reg03 <- feols(mu ~ wsvi_i_c | year, data=match, subset = match$ind_group=="HQ", panel.id = c("id","year"), cluster = "id")
reg04 <- feols(mu ~ wsvi_i_c | year, data=match, panel.id = c("id","year"), cluster = "id")

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


#///////////////////////////////////////////////////////////////////////////////
#----                 2 - PLOT                  ----
#///////////////////////////////////////////////////////////////////////////////

ggplot(data=match[mu>=0], aes(x=wsvi_i_c, y=mu)) + 
    geom_point(aes(colour=ind), alpha=0.5, size=0.1) + scale_y_continuous(limits=c(0,3)) +
    geom_smooth(method="lm", formula=y~x, color="black") +
    geom_smooth(aes(colour=ind), method="lm", formula=y~x)


