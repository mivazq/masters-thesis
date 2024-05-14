#///////////////////////////////////////////////////////////////////////////////
# File name:		table_1_summary_markups.R
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

# Load data and merge
load(file=paste0(pathEst, "output/firm_markups_V.Rdata"))
load(file=paste0(pathEst, "output/firm_markups_ML.Rdata"))
markups <- merge(markups_ML, markups_V, by=c("id","year", "ind"))
rm(markups_ML, markups_V)

# Reshape dataset
markups <- melt(markups, measure.vars = measure(value.name, input, est, pf, sep = "_"))
markups[, weight := ifelse(input=="m", M, ifelse(input=="l", L, V))]
markups[, c("M","L","V") := NULL]

# Table by industry group, for main
sink(paste0(pathTab,sysdate,"_table_1_estimates_markups_main.tex"))
cat("\\begin{table}[!htbp]\\centering \n")
cat("\\caption{\\label{tab:EstimatedMarkups} Estimated Markups} \n")
# cat("\\begin{adjustbox}{width=\\columnwidth,center} \n")
cat("\\begin{tabular}{llc
    S[table-format=1.2]@{\\hskip-.4cm}S[table-format=3.3,parse-numbers=false]}")
cat("\\toprule \n")
cat("Production & Variable & & \\multicolumn{2}{l}{\\multirow{2}{*}{\\hspace{-8pt}Median markup}} \\\\ \n")
cat("function   & input    & & \\\\ \n")
cat("\\addlinespace \\hline \\addlinespace \n")
cat("Cobb-Douglas & $V$ & & ", 
    fp(median(markups[input=="v" & est=="dlw" & pf=="cd"]$mu, na.rm=T), 2), " & ", fpt(sd(markups[input=="v" & est=="dlw" & pf=="cd"]$mu, na.rm=T),2), " \\\\  \n")
cat("Translog & $V$ & & ", 
    fp(median(markups[input=="v" & est=="dlw" & pf=="tl"]$mu, na.rm=T), 2), " & ", fpt(sd(markups[input=="v" & est=="dlw" & pf=="tl"]$mu, na.rm=T),2), " \\\\  \n")
cat("Cobb-Douglas & $M$ & & ", 
    fp(median(markups[input=="m" & est=="dlw" & pf=="cd"]$mu, na.rm=T), 2), " & ", fpt(sd(markups[input=="m" & est=="dlw" & pf=="cd"]$mu, na.rm=T),2), " \\\\  \n")
cat("Translog & $M$ & & ", 
    fp(median(markups[input=="m" & est=="dlw" & pf=="tl"]$mu, na.rm=T), 2), " & ", fpt(sd(markups[input=="m" & est=="dlw" & pf=="tl"]$mu, na.rm=T),2), " \\\\  \n")
cat("Cobb-Douglas & $L$ & & ", 
    fp(median(markups[input=="l" & est=="dlw" & pf=="cd"]$mu, na.rm=T), 2), " & ", fpt(sd(markups[input=="l" & est=="dlw" & pf=="cd"]$mu, na.rm=T),2), " \\\\  \n")
cat("Translog & $L$ & & ", 
    fp(median(markups[input=="l" & est=="dlw" & pf=="tl"]$mu, na.rm=T), 2), " & ", fpt(sd(markups[input=="l" & est=="dlw" & pf=="tl"]$mu, na.rm=T),2), " \\\\  \n")
cat("\\bottomrule \n")
cat("\\end{tabular} \n")
# cat("\\end{adjustbox} \n")
cat("\\justify \\footnotesize \\emph{Notes:} This table reports the median estimated firm markup, across all firms and years, by specification. The different specifications are a combination of assuming different production functions (Cobb-Douglas or Translog) and using different variable inputs to recover markups ($M$ being material inputs, $L$ being labour, and $V$ being the sum of both). Standard deviations reported in parentheses as per \\cite{DeLoecker2012}. \n")
cat("\\end{table} \n")
sink()

# Create table for different subsets of industries (A-F, G, H-Q, *ALL*)
markups_AF <- markups[substr(ind,1,1) %in% c("A","B","C","D","E","F")]
table(substr(markups_AF$ind,1,1))
markups_G  <- markups[substr(ind,1,1) %in% c("G")]
table(substr(markups_G$ind,1,1))
markups_HQ <- markups[substr(ind,1,1) %in% c("H","I","J","K","L","M","N","O","P","Q")]
table(substr(markups_HQ$ind,1,1))

# Compute shares of observations by industry group
share_AF <- nrow(markups_AF[!is.na(mu)])/nrow(markups[!is.na(mu)])
share_G  <- nrow(markups_G[!is.na(mu)]) /nrow(markups[!is.na(mu)])
share_HQ <- nrow(markups_HQ[!is.na(mu)])/nrow(markups[!is.na(mu)])

# Compute number of observations by industry group
nrobs_AF  <- nrow(markups_AF[!is.na(mu)])
nrobs_G   <- nrow(markups_G[!is.na(mu)])
nrobs_HQ  <- nrow(markups_HQ[!is.na(mu)])
nrobs_tot <- nrow(markups[!is.na(mu)])

# Table by industry group, for appendix
sink(paste0(pathTab,sysdate,"_table_1_estimates_markups_appendix.tex"))
cat("\\begin{table}[!htbp]\\centering \n")
cat("\\caption{\\label{tab:EstimatedMarkupsIndustryGroup} Estimated Markups by Industry Group} \n")
cat("\\begin{adjustbox}{width=\\columnwidth,center} \n")
cat("\\begin{tabular}{ll
    S[table-format=1.2]@{\\hskip.1cm}S[table-format=3.3,parse-numbers=false]
    S[table-format=1.2]@{\\hskip.1cm}S[table-format=3.3,parse-numbers=false]
    S[table-format=1.2]@{\\hskip.1cm}S[table-format=3.3,parse-numbers=false]
    S[table-format=1.2]@{\\hskip.1cm}S[table-format=3.3,parse-numbers=false]}")
cat("\\toprule \n")
cat("\\multicolumn{1}{l}{\\multirow{2}{*}{Production}} & \\multicolumn{1}{l}{\\multirow{2}{*}{Variable}} & \\multicolumn{8}{c}{Industry group} \\\\ \n")
cat("\\addlinespace \\cline{3-10} \\addlinespace \n")
cat("function & input & \\multicolumn{2}{l}{\\hspace{7pt}A01--F45} & \\multicolumn{2}{l}{\\hspace{6pt}G50--G52} & \\multicolumn{2}{l}{\\hspace{6pt}H55--Q99} & \\multicolumn{2}{l}{\\hspace{-5pt}All industries} \\\\ \n")
cat("\\addlinespace \\hline \\addlinespace \n")
cat("Cobb-Douglas & $V$ & ", 
    fp(median(markups_AF[input=="v" & est=="dlw" & pf=="cd"]$mu, na.rm=T), 2), " & ", fpt(sd(markups_AF[input=="v" & est=="dlw" & pf=="cd"]$mu, na.rm=T),2), " & ", 
    fp(median(markups_G[input=="v" & est=="dlw" & pf=="cd"]$mu, na.rm=T), 2), " & ", fpt(sd(markups_G[input=="v" & est=="dlw" & pf=="cd"]$mu, na.rm=T),2), " & ",
    fp(median(markups_HQ[input=="v" & est=="dlw" & pf=="cd"]$mu, na.rm=T), 2), " & ", fpt(sd(markups_HQ[input=="v" & est=="dlw" & pf=="cd"]$mu, na.rm=T),2), " & ",
    fp(median(markups[input=="v" & est=="dlw" & pf=="cd"]$mu, na.rm=T), 2), " & ", fpt(sd(markups[input=="v" & est=="dlw" & pf=="cd"]$mu, na.rm=T),2), " \\\\  \n")
cat("Translog & $V$ & ", 
    fp(median(markups_AF[input=="v" & est=="dlw" & pf=="tl"]$mu, na.rm=T), 2), " & ", fpt(sd(markups_AF[input=="v" & est=="dlw" & pf=="tl"]$mu, na.rm=T),2), " & ", 
    fp(median(markups_G[input=="v" & est=="dlw" & pf=="tl"]$mu, na.rm=T), 2), " & ", fpt(sd(markups_G[input=="v" & est=="dlw" & pf=="tl"]$mu, na.rm=T),2), " & ",
    fp(median(markups_HQ[input=="v" & est=="dlw" & pf=="tl"]$mu, na.rm=T), 2), " & ", fpt(sd(markups_HQ[input=="v" & est=="dlw" & pf=="tl"]$mu, na.rm=T),2), " & ",
    fp(median(markups[input=="v" & est=="dlw" & pf=="tl"]$mu, na.rm=T), 2), " & ", fpt(sd(markups[input=="v" & est=="dlw" & pf=="tl"]$mu, na.rm=T),2), " \\\\  \n")
cat("Cobb-Douglas & $M$ & ", 
    fp(median(markups_AF[input=="m" & est=="dlw" & pf=="cd"]$mu, na.rm=T), 2), " & ", fpt(sd(markups_AF[input=="m" & est=="dlw" & pf=="cd"]$mu, na.rm=T),2), " & ", 
    fp(median(markups_G[input=="m" & est=="dlw" & pf=="cd"]$mu, na.rm=T), 2), " & ", fpt(sd(markups_G[input=="m" & est=="dlw" & pf=="cd"]$mu, na.rm=T),2), " & ",
    fp(median(markups_HQ[input=="m" & est=="dlw" & pf=="cd"]$mu, na.rm=T), 2), " & ", fpt(sd(markups_HQ[input=="m" & est=="dlw" & pf=="cd"]$mu, na.rm=T),2), " & ",
    fp(median(markups[input=="m" & est=="dlw" & pf=="cd"]$mu, na.rm=T), 2), " & ", fpt(sd(markups[input=="m" & est=="dlw" & pf=="cd"]$mu, na.rm=T),2), " \\\\  \n")
cat("Translog & $M$ & ", 
    fp(median(markups_AF[input=="m" & est=="dlw" & pf=="tl"]$mu, na.rm=T), 2), " & ", fpt(sd(markups_AF[input=="m" & est=="dlw" & pf=="tl"]$mu, na.rm=T),2), " & ", 
    fp(median(markups_G[input=="m" & est=="dlw" & pf=="tl"]$mu, na.rm=T), 2), " & ", fpt(sd(markups_G[input=="m" & est=="dlw" & pf=="tl"]$mu, na.rm=T),2), " & ",
    fp(median(markups_HQ[input=="m" & est=="dlw" & pf=="tl"]$mu, na.rm=T), 2), " & ", fpt(sd(markups_HQ[input=="m" & est=="dlw" & pf=="tl"]$mu, na.rm=T),2), " & ",
    fp(median(markups[input=="m" & est=="dlw" & pf=="tl"]$mu, na.rm=T), 2), " & ", fpt(sd(markups[input=="m" & est=="dlw" & pf=="tl"]$mu, na.rm=T),2), " \\\\  \n")
cat("Cobb-Douglas & $L$ & ", 
    fp(median(markups_AF[input=="l" & est=="dlw" & pf=="cd"]$mu, na.rm=T), 2), " & ", fpt(sd(markups_AF[input=="l" & est=="dlw" & pf=="cd"]$mu, na.rm=T),2), " & ", 
    fp(median(markups_G[input=="l" & est=="dlw" & pf=="cd"]$mu, na.rm=T), 2), " & ", fpt(sd(markups_G[input=="l" & est=="dlw" & pf=="cd"]$mu, na.rm=T),2), " & ",
    fp(median(markups_HQ[input=="l" & est=="dlw" & pf=="cd"]$mu, na.rm=T), 2), " & ", fpt(sd(markups_HQ[input=="l" & est=="dlw" & pf=="cd"]$mu, na.rm=T),2), " & ",
    fp(median(markups[input=="l" & est=="dlw" & pf=="cd"]$mu, na.rm=T), 2), " & ", fpt(sd(markups[input=="l" & est=="dlw" & pf=="cd"]$mu, na.rm=T),2), " \\\\  \n")
cat("Translog & $L$ & ", 
    fp(median(markups_AF[input=="l" & est=="dlw" & pf=="tl"]$mu, na.rm=T), 2), " & ", fpt(sd(markups_AF[input=="l" & est=="dlw" & pf=="tl"]$mu, na.rm=T),2), " & ", 
    fp(median(markups_G[input=="l" & est=="dlw" & pf=="tl"]$mu, na.rm=T), 2), " & ", fpt(sd(markups_G[input=="l" & est=="dlw" & pf=="tl"]$mu, na.rm=T),2), " & ",
    fp(median(markups_HQ[input=="l" & est=="dlw" & pf=="tl"]$mu, na.rm=T), 2), " & ", fpt(sd(markups_HQ[input=="l" & est=="dlw" & pf=="tl"]$mu, na.rm=T),2), " & ",
    fp(median(markups[input=="l" & est=="dlw" & pf=="tl"]$mu, na.rm=T), 2), " & ", fpt(sd(markups[input=="l" & est=="dlw" & pf=="tl"]$mu, na.rm=T),2), " \\\\  \n")
cat("\\addlinespace \\hline \\addlinespace \n")
cat("\\multicolumn{2}{l}{Number of observations} & \\multicolumn{2}{c}{", fp(nrobs_AF),    "} & \\multicolumn{2}{c}{", fp(nrobs_G),    "} & \\multicolumn{2}{c}{", fp(nrobs_HQ),    "} & \\multicolumn{2}{c}{", fp(nrobs_tot), "} \\\\  \n")
cat("\\multicolumn{2}{l}{Share of observations}  & \\multicolumn{2}{c}{", fpp(share_AF,1), "} & \\multicolumn{2}{c}{", fpp(share_G,1), "} & \\multicolumn{2}{c}{", fpp(share_HQ,1), "} & \\multicolumn{2}{c}{", "100\\%",      "} \\\\  \n")
cat("\\bottomrule \n")
cat("\\end{tabular} \n")
cat("\\end{adjustbox} \n")
cat("\\justify \\footnotesize \\emph{Notes:} This table reports the median estimated firm markup, across all firms and years, by specification and industry group. The different specifications are a combination of assuming different production functions (Cobb-Douglas or Translog) and using different variable inputs to recover markups ($M$ being material inputs, $L$ being labour, and $V$ being the sum of both). Standard deviations reported in parentheses as per \\cite{DeLoecker2012}. \n")
cat("\\end{table} \n")
sink()



