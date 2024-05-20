#///////////////////////////////////////////////////////////////////////////////
# File name:		table_4_summary_parameters.R
# Author:			Miguel Vázquez Vázquez
# Creation date:    31 March 2024
# Description:      This file produces table with summary statistics on markups
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

# Load data and merge
elasticities_V  <- as.data.table(read_xlsx(path=paste0(pathEst, "output/CD_elasticities_V_by_industry.xlsx")))
elasticities_ML <- as.data.table(read_xlsx(path=paste0(pathEst, "output/CD_elasticities_ML_by_industry.xlsx")))

# Create table for different subsets of industries (A-F, G, H-Q, *ALL*)
elasticities_V_AF  <- elasticities_V[substr(ind,1,1) %in% c("A","B","C","D","E","F")]
elasticities_V_G   <- elasticities_V[substr(ind,1,1) %in% c("G")]
elasticities_V_HQ  <- elasticities_V[substr(ind,1,1) %in% c("H","I","J","K","L","M","N","O","P","Q")]
elasticities_ML_AF <- elasticities_ML[substr(ind,1,1) %in% c("A","B","C","D","E","F")]
elasticities_ML_G  <- elasticities_ML[substr(ind,1,1) %in% c("G")]
elasticities_ML_HQ <- elasticities_ML[substr(ind,1,1) %in% c("H","I","J","K","L","M","N","O","P","Q")]

# Count number of industries by industry group
ind_nr_AF <- length(elasticities_V_AF$ind)
ind_nr_G  <- length(elasticities_V_G$ind)
ind_nr_HQ <- length(elasticities_V_HQ$ind)
ind_nr_tot <- length(elasticities_V$ind)

# Compute number of observations by industry group
nrobs_AF  <- sum(elasticities_V_AF$n)
nrobs_G   <- sum(elasticities_V_G$n)
nrobs_HQ  <- sum(elasticities_V_HQ$n)
nrobs_tot <- sum(elasticities_V$n)

# Construct shares (might be different than for other tables due to only 2nd stage obs)
share_AF  <- nrobs_AF /nrobs_tot
share_G   <- nrobs_G  /nrobs_tot
share_HQ  <- nrobs_HQ /nrobs_tot
share_tot <- nrobs_tot/nrobs_tot

# Compute weighted averages (value=elasticity, weight=industry count)
varnames = names(elasticities_V)[4:length(names(elasticities_V))]
elasticities_V[,    as.vector(varnames) := lapply(.SD, function(x) x*elasticities_V[["n"]]),    .SDcols = varnames] # multiply by weight (n)
elasticities_V_AF[, as.vector(varnames) := lapply(.SD, function(x) x*elasticities_V_AF[["n"]]), .SDcols = varnames] # multiply by weight (n)
elasticities_V_G[,  as.vector(varnames) := lapply(.SD, function(x) x*elasticities_V_G[["n"]]),  .SDcols = varnames] # multiply by weight (n)
elasticities_V_HQ[, as.vector(varnames) := lapply(.SD, function(x) x*elasticities_V_HQ[["n"]]), .SDcols = varnames] # multiply by weight (n)
elasticities_V    <- dcast(data = elasticities_V,    formula = .~., fun.aggregate = sum, value.var = c("n", varnames)) # sum weighted elasticities
elasticities_V_AF <- dcast(data = elasticities_V_AF, formula = .~., fun.aggregate = sum, value.var = c("n", varnames)) # sum weighted elasticities
elasticities_V_G  <- dcast(data = elasticities_V_G,  formula = .~., fun.aggregate = sum, value.var = c("n", varnames)) # sum weighted elasticities
elasticities_V_HQ <- dcast(data = elasticities_V_HQ, formula = .~., fun.aggregate = sum, value.var = c("n", varnames)) # sum weighted elasticities
elasticities_V[,    as.vector(varnames) := lapply(.SD, function(x) x/elasticities_V[["n"]]),    .SDcols = varnames] # divide by total weight (n)
elasticities_V_AF[, as.vector(varnames) := lapply(.SD, function(x) x/elasticities_V_AF[["n"]]), .SDcols = varnames] # divide by total weight (n)
elasticities_V_G[,  as.vector(varnames) := lapply(.SD, function(x) x/elasticities_V_G[["n"]]),  .SDcols = varnames] # divide by total weight (n)
elasticities_V_HQ[, as.vector(varnames) := lapply(.SD, function(x) x/elasticities_V_HQ[["n"]]), .SDcols = varnames] # divide by total weight (n)
varnames = names(elasticities_ML)[4:length(names(elasticities_ML))]
elasticities_ML[,    as.vector(varnames) := lapply(.SD, function(x) x*elasticities_ML[["n"]]),    .SDcols = varnames] # multiply by weight (n)
elasticities_ML_AF[, as.vector(varnames) := lapply(.SD, function(x) x*elasticities_ML_AF[["n"]]), .SDcols = varnames] # multiply by weight (n)
elasticities_ML_G[,  as.vector(varnames) := lapply(.SD, function(x) x*elasticities_ML_G[["n"]]),  .SDcols = varnames] # multiply by weight (n)
elasticities_ML_HQ[, as.vector(varnames) := lapply(.SD, function(x) x*elasticities_ML_HQ[["n"]]), .SDcols = varnames] # multiply by weight (n)
elasticities_ML    <- dcast(data = elasticities_ML,    formula = .~., fun.aggregate = sum, value.var = c("n", varnames)) # sum weighted elasticities
elasticities_ML_AF <- dcast(data = elasticities_ML_AF, formula = .~., fun.aggregate = sum, value.var = c("n", varnames)) # sum weighted elasticities
elasticities_ML_G  <- dcast(data = elasticities_ML_G,  formula = .~., fun.aggregate = sum, value.var = c("n", varnames)) # sum weighted elasticities
elasticities_ML_HQ <- dcast(data = elasticities_ML_HQ, formula = .~., fun.aggregate = sum, value.var = c("n", varnames)) # sum weighted elasticities
elasticities_ML[,    as.vector(varnames) := lapply(.SD, function(x) x/elasticities_ML[["n"]]),    .SDcols = varnames] # divide by total weight (n)
elasticities_ML_AF[, as.vector(varnames) := lapply(.SD, function(x) x/elasticities_ML_AF[["n"]]), .SDcols = varnames] # divide by total weight (n)
elasticities_ML_G[,  as.vector(varnames) := lapply(.SD, function(x) x/elasticities_ML_G[["n"]]),  .SDcols = varnames] # divide by total weight (n)
elasticities_ML_HQ[, as.vector(varnames) := lapply(.SD, function(x) x/elasticities_ML_HQ[["n"]]), .SDcols = varnames] # divide by total weight (n)

# Rename variables, assign industry group
elasticities_V_AF[,  . := "AF"]
elasticities_V_G[,   . := "G"]
elasticities_V_HQ[,  . := "HQ"]
elasticities_ML_AF[, . := "AF"]
elasticities_ML_G[,  . := "G"]
elasticities_ML_HQ[, . := "HQ"]
setnames(elasticities_V_AF,   ".", "ind_group")
setnames(elasticities_V_G,    ".", "ind_group")
setnames(elasticities_V_HQ,   ".", "ind_group")
setnames(elasticities_ML_AF,  ".", "ind_group")
setnames(elasticities_ML_G,   ".", "ind_group")
setnames(elasticities_ML_HQ,  ".", "ind_group")
elasticities_V[, . := NULL]
elasticities_ML[, . := NULL]

#///////////////////////////////////////////////////////////////////////////////
#### OUTPUT TABLES ####
#///////////////////////////////////////////////////////////////////////////////

# Main table - Cobb Douglas - V
sink(paste0(pathTab,sysdate,"_table_2_estimates_parameters_main.tex"))
cat("\\begin{table}[!htbp]\\centering \n")
cat("\\caption{\\label{tab:EstimatedParametersV} Output Elasticities by Estimation Method} \n")
cat("\\begin{tabular}{lccc}")
cat("\\toprule \n")
cat(" & FOC & OLS & DLW \\\\ \n")
cat("\\addlinespace \\hline \\addlinespace \n")
cat("Variable ($V$) & ", fp(elasticities_V$FOC_v,3), " & ", fp(elasticities_V$OLS_v,3), " & ", fp(elasticities_V$DLW_v,3), "  \\\\ \n")
cat("Capital ($K$) & ", fp(elasticities_V$FOC_k,3), " & ", fp(elasticities_V$OLS_k,3), " & ", fp(elasticities_V$DLW_k,3), " \\\\ \n")
cat("\\addlinespace \\hline \\addlinespace \n")
cat("Returns to scale & ", fp(elasticities_V$FOC_tot,3), " & ", fp(elasticities_V$OLS_tot,3), " & ", fp(elasticities_V$DLW_tot,3), " \\\\ \n")
cat("\\bottomrule \n")
cat("\\end{tabular} \n")
cat("\\justify \\footnotesize \\emph{Notes:} This table reports the weighted average of the Cobb-Douglas techonology parameters and thus output elasticities. For each row, the specific industry $s$ parameter $\\hat{\\theta}_s$ is weighted by the number of observations used to estimate it. The first column reports the technology parameter implied by the aggregate First Order Condition (FOC) approach, the second column reports the value obtained via an Ordinary Least Squares (OLS) regression with time controls, while the third reports the parameters estimated with the \\citeauthor{DeLoecker2012} (DLW) \\citeyear{DeLoecker2012} methodology. For comparability reasons the FOC and OLS esimates are computed on the same sample as DLW, i.e. 2\\ts{nd} stage observations. Returns to scale are constructed by summing up all inputs' output elasticities and might slightly differ from the sum of the values reported in the table due to rounding errors.\n")
cat("\\end{table} \n")
sink()


# Appendix table - Cobb Douglas - V
sink(paste0(pathTab,sysdate,"_table_2_estimates_parameters_appendix.tex"))
cat("\\begin{table}[!htbp]\\centering \n")
cat("\\caption{\\label{tab:EstimatedParametersIndustryGroup} Output Elasticities by Industry Group and Estimation Method} \n")
cat("\\begin{adjustbox}{width=\\columnwidth,center} \n")
cat("\\begin{tabular}{p{5pt}lccccccccccccccc}")
cat("\\toprule \n")
cat(" & & \\multicolumn{15}{c}{Industry group} \\\\ \n")
cat("\\addlinespace \\cline{3-17} \\addlinespace \n")
cat(" & & \\multicolumn{3}{c}{A01--F45} & & \\multicolumn{3}{c}{G50--G52} & & \\multicolumn{3}{c}{H55--Q99} & & \\multicolumn{3}{c}{All industries}\\\\ \n")
cat("\\addlinespace \\cline{3-5} \\cline{7-9} \\cline{11-13} \\cline{15-17} \\addlinespace \n")
cat(" & & FOC & OLS & DLW & & FOC & OLS & DLW & & FOC & OLS & DLW & & FOC & OLS & DLW \\\\ \n")
cat("\\addlinespace \\hline \\addlinespace \n")
cat("\\multicolumn{2}{l}{Combined variable inputs:} & & & & & & & & & & & & & & & \\\\ \n")
cat("\\addlinespace \n")
cat("& Variable ($V$) & ",
    fp(elasticities_V_AF$FOC_v,3), " & ", fp(elasticities_V_AF$OLS_v,3), " & ", fp(elasticities_V_AF$DLW_v,3), " & & ",
    fp(elasticities_V_G$FOC_v,3),  " & ", fp(elasticities_V_G$OLS_v,3),  " & ", fp(elasticities_V_G$DLW_v,3),  " & & ",
    fp(elasticities_V_HQ$FOC_v,3), " & ", fp(elasticities_V_HQ$OLS_v,3), " & ", fp(elasticities_V_HQ$DLW_v,3), " & & ",
    fp(elasticities_V$FOC_v,3),    " & ", fp(elasticities_V$OLS_v,3),    " & ", fp(elasticities_V$DLW_v,3),    " \\\\ \n")
cat("& Capital ($K$) & ",
    fp(elasticities_V_AF$FOC_k,3), " & ", fp(elasticities_V_AF$OLS_k,3), " & ", fp(elasticities_V_AF$DLW_k,3), " & & ",
    fp(elasticities_V_G$FOC_k,3),  " & ", fp(elasticities_V_G$OLS_k,3),  " & ", fp(elasticities_V_G$DLW_k,3),  " & & ",
    fp(elasticities_V_HQ$FOC_k,3), " & ", fp(elasticities_V_HQ$OLS_k,3), " & ", fp(elasticities_V_HQ$DLW_k,3), " & & ",
    fp(elasticities_V$FOC_k,3),    " & ", fp(elasticities_V$OLS_k,3),    " & ", fp(elasticities_V$DLW_k,3),    " \\\\ \n")
cat("\\addlinespace \\cline{2-17} \\addlinespace \n")
cat("& Returns to scale & ",
    fp(elasticities_V_AF$FOC_tot,3), " & ", fp(elasticities_V_AF$OLS_tot,3), " & ", fp(elasticities_V_AF$DLW_tot,3), " & & ",
    fp(elasticities_V_G$FOC_tot,3),  " & ", fp(elasticities_V_G$OLS_tot,3),  " & ", fp(elasticities_V_G$DLW_tot,3),  " & & ",
    fp(elasticities_V_HQ$FOC_tot,3), " & ", fp(elasticities_V_HQ$OLS_tot,3), " & ", fp(elasticities_V_HQ$DLW_tot,3), " & & ",
    fp(elasticities_V$FOC_tot,3),    " & ", fp(elasticities_V$OLS_tot,3),    " & ", fp(elasticities_V$DLW_tot,3),    " \\\\ \n")
cat("\\addlinespace \\midrule \\addlinespace \n")
cat("\\multicolumn{2}{l}{Separated variable inputs:} & & & & & & & & & & & & & & & \\\\ \n")
cat("\\addlinespace \n")
cat("& Materials ($M$)  & ",
    fp(elasticities_ML_AF$FOC_m,3), " & ", fp(elasticities_ML_AF$OLS_m,3), " & ", fp(elasticities_ML_AF$DLW_m,3), " & & ",
    fp(elasticities_ML_G$FOC_m,3),  " & ", fp(elasticities_ML_G$OLS_m,3),  " & ", fp(elasticities_ML_G$DLW_m,3),  " & & ",
    fp(elasticities_ML_HQ$FOC_m,3), " & ", fp(elasticities_ML_HQ$OLS_m,3), " & ", fp(elasticities_ML_HQ$DLW_m,3), " & & ",
    fp(elasticities_ML$FOC_m,3),    " & ", fp(elasticities_ML$OLS_m,3),    " & ", fp(elasticities_ML$DLW_m,3),    " \\\\ \n")
cat("& Labour ($L$) & ",
    fp(elasticities_ML_AF$FOC_l,3), " & ", fp(elasticities_ML_AF$OLS_l,3), " & ", fp(elasticities_ML_AF$DLW_l,3), " & & ",
    fp(elasticities_ML_G$FOC_l,3),  " & ", fp(elasticities_ML_G$OLS_l,3),  " & ", fp(elasticities_ML_G$DLW_l,3),  " & & ",
    fp(elasticities_ML_HQ$FOC_l,3), " & ", fp(elasticities_ML_HQ$OLS_l,3), " & ", fp(elasticities_ML_HQ$DLW_l,3), " & & ",
    fp(elasticities_ML$FOC_l,3),    " & ", fp(elasticities_ML$OLS_l,3),    " & ", fp(elasticities_ML$DLW_l,3),    " \\\\ \n")
cat("& Capital ($K$) & ",
    fp(elasticities_ML_AF$FOC_k,3), " & ", fp(elasticities_ML_AF$OLS_k,3), " & ", fp(elasticities_ML_AF$DLW_k,3), " & & ",
    fp(elasticities_ML_G$FOC_k,3),  " & ", fp(elasticities_ML_G$OLS_k,3),  " & ", fp(elasticities_ML_G$DLW_k,3),  " & & ",
    fp(elasticities_ML_HQ$FOC_k,3), " & ", fp(elasticities_ML_HQ$OLS_k,3), " & ", fp(elasticities_ML_HQ$DLW_k,3), " & & ",
    fp(elasticities_ML$FOC_k,3),    " & ", fp(elasticities_ML$OLS_k,3),    " & ", fp(elasticities_ML$DLW_k,3),    " \\\\ \n")
cat("\\addlinespace \\cline{2-17} \\addlinespace \n")
cat("& Returns to scale & ",
    fp(elasticities_ML_AF$FOC_tot,3), " & ", fp(elasticities_ML_AF$OLS_tot,3), " & ", fp(elasticities_ML_AF$DLW_tot,3), " & & ",
    fp(elasticities_ML_G$FOC_tot,3),  " & ", fp(elasticities_ML_G$OLS_tot,3),  " & ", fp(elasticities_ML_G$DLW_tot,3),  " & & ",
    fp(elasticities_ML_HQ$FOC_tot,3), " & ", fp(elasticities_ML_HQ$OLS_tot,3), " & ", fp(elasticities_ML_HQ$DLW_tot,3), " & & ",
    fp(elasticities_ML$FOC_tot,3),    " & ", fp(elasticities_ML$OLS_tot,3),    " & ", fp(elasticities_ML$DLW_tot,3),    " \\\\ \n")
cat("\\addlinespace \\hline \\addlinespace \n")
cat("\\multicolumn{2}{l}{Number of industries}  & \\multicolumn{3}{c}{", 
    fp(ind_nr_AF),  "} & & \\multicolumn{3}{c}{", 
    fp(ind_nr_G),   "} & & \\multicolumn{3}{c}{", 
    fp(ind_nr_HQ),  "} & & \\multicolumn{3}{c}{", 
    fp(ind_nr_tot), "} \\\\  \n")
cat("\\multicolumn{2}{l}{Number of observations} & \\multicolumn{3}{c}{", 
    fp(nrobs_AF),  "} & & \\multicolumn{3}{c}{", 
    fp(nrobs_G),   "} & & \\multicolumn{3}{c}{", 
    fp(nrobs_HQ),  "} & & \\multicolumn{3}{c}{", 
    fp(nrobs_tot), "} \\\\  \n")
cat("\\multicolumn{2}{l}{Share of observations} & \\multicolumn{3}{c}{", 
    fpp(share_AF,1),  "} & & \\multicolumn{3}{c}{", 
    fpp(share_G,1),   "} & & \\multicolumn{3}{c}{", 
    fpp(share_HQ,1),  "} & & \\multicolumn{3}{c}{",
    fpp(share_tot,1), "} \\\\  \n")
cat("\\bottomrule \n")
cat("\\end{tabular} \n")
cat("\\end{adjustbox} \n")
cat("\\justify \\footnotesize \\emph{Notes:} This table reports the weighted average of the Cobb-Douglas techonology parameters and thus output elasticities. For each row, the specific industry $s$ parameter $\\hat{\\theta}_s$ is weighted by the number of observations used to estimate it. For each industry group, the first column reports the technology parameter implied by the aggregate First Order Condition (FOC) approach, the second column reports the value obtained via an Ordinary Least Squares (OLS) regression with time controls, while the third reports the parameters estimated with the \\citeauthor{DeLoecker2012} (DLW) \\citeyear{DeLoecker2012} methodology. For comparability reasons the FOC and OLS esimates are computed on the same sample as DLW, i.e. 2\\ts{nd} stage observations. Returns to scale are constructed by summing up all inputs' output elasticities and might slightly differ from the sum of the values reported in the table due to rounding errors.\n")
cat("\\end{table} \n")
sink()




