#///////////////////////////////////////////////////////////////////////////////
# File name:		table_3_summary_markups.R
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

sink(paste0(pathTab,sysdate,"_table_3_estimates_markups.tex"))
cat("\\begin{table}[!htbp]\\centering \n")
cat("\\caption{\\label{tab:EstimatedMarkups} Estimated Markups} \n")
# cat("\\begin{adjustbox}{width=\\columnwidth,center} \n")
cat("\\begin{tabular}{lcccc}")
cat("\\toprule \n")
cat("\\multicolumn{1}{l}{\\multirow{3}{*}{Specification}} & \\multicolumn{4}{c}{Median markup ($\\mu$) by industry group} \\\\ \n")
cat("\\addlinespace \\cline{2-5} \\addlinespace \n")
cat(" & A01--F45 & G50--G52 & H55--Q99 & All industries \\\\ \n")
cat("\\addlinespace \\hline \\addlinespace \n")
    cat("$V$ --- Cobb-Douglas & ", 
        fp(median(markups_AF[input=="v" & est=="dlw" & pf=="cd"]$mu, na.rm=T), 2), fpt(sd(markups_AF[input=="v" & est=="dlw" & pf=="cd"]$mu, na.rm=T),2), " & ", 
        fp(median(markups_G[input=="v" & est=="dlw" & pf=="cd"]$mu, na.rm=T), 2), fpt(sd(markups_G[input=="v" & est=="dlw" & pf=="cd"]$mu, na.rm=T),2), " & ",
        fp(median(markups_HQ[input=="v" & est=="dlw" & pf=="cd"]$mu, na.rm=T), 2), fpt(sd(markups_HQ[input=="v" & est=="dlw" & pf=="cd"]$mu, na.rm=T),2), " & ",
        fp(median(markups[input=="v" & est=="dlw" & pf=="cd"]$mu, na.rm=T), 2), fpt(sd(markups[input=="v" & est=="dlw" & pf=="cd"]$mu, na.rm=T),2), " \\\\  \n")
    cat("$V$ --- Translog & ", 
        fp(median(markups_AF[input=="v" & est=="dlw" & pf=="tl"]$mu, na.rm=T), 2), fpt(sd(markups_AF[input=="v" & est=="dlw" & pf=="tl"]$mu, na.rm=T),2), " & ", 
        fp(median(markups_G[input=="v" & est=="dlw" & pf=="tl"]$mu, na.rm=T), 2), fpt(sd(markups_G[input=="v" & est=="dlw" & pf=="tl"]$mu, na.rm=T),2), " & ",
        fp(median(markups_HQ[input=="v" & est=="dlw" & pf=="tl"]$mu, na.rm=T), 2), fpt(sd(markups_HQ[input=="v" & est=="dlw" & pf=="tl"]$mu, na.rm=T),2), " & ",
        fp(median(markups[input=="v" & est=="dlw" & pf=="tl"]$mu, na.rm=T), 2), fpt(sd(markups[input=="v" & est=="dlw" & pf=="tl"]$mu, na.rm=T),2), " \\\\  \n")
    cat("$M$ --- Cobb-Douglas & ", 
        fp(median(markups_AF[input=="m" & est=="dlw" & pf=="cd"]$mu, na.rm=T), 2), fpt(sd(markups_AF[input=="m" & est=="dlw" & pf=="cd"]$mu, na.rm=T),2), " & ", 
        fp(median(markups_G[input=="m" & est=="dlw" & pf=="cd"]$mu, na.rm=T), 2), fpt(sd(markups_G[input=="m" & est=="dlw" & pf=="cd"]$mu, na.rm=T),2), " & ",
        fp(median(markups_HQ[input=="m" & est=="dlw" & pf=="cd"]$mu, na.rm=T), 2), fpt(sd(markups_HQ[input=="m" & est=="dlw" & pf=="cd"]$mu, na.rm=T),2), " & ",
        fp(median(markups[input=="m" & est=="dlw" & pf=="cd"]$mu, na.rm=T), 2), fpt(sd(markups[input=="m" & est=="dlw" & pf=="cd"]$mu, na.rm=T),2), " \\\\  \n")
    cat("$M$ --- Translog & ", 
        fp(median(markups_AF[input=="m" & est=="dlw" & pf=="tl"]$mu, na.rm=T), 2), fpt(sd(markups_AF[input=="m" & est=="dlw" & pf=="tl"]$mu, na.rm=T),2), " & ", 
        fp(median(markups_G[input=="m" & est=="dlw" & pf=="tl"]$mu, na.rm=T), 2), fpt(sd(markups_G[input=="m" & est=="dlw" & pf=="tl"]$mu, na.rm=T),2), " & ",
        fp(median(markups_HQ[input=="m" & est=="dlw" & pf=="tl"]$mu, na.rm=T), 2), fpt(sd(markups_HQ[input=="m" & est=="dlw" & pf=="tl"]$mu, na.rm=T),2), " & ",
        fp(median(markups[input=="m" & est=="dlw" & pf=="tl"]$mu, na.rm=T), 2), fpt(sd(markups[input=="m" & est=="dlw" & pf=="tl"]$mu, na.rm=T),2), " \\\\  \n")
    cat("$L$ --- Cobb-Douglas & ", 
        fp(median(markups_AF[input=="l" & est=="dlw" & pf=="cd"]$mu, na.rm=T), 2), fpt(sd(markups_AF[input=="l" & est=="dlw" & pf=="cd"]$mu, na.rm=T),2), " & ", 
        fp(median(markups_G[input=="l" & est=="dlw" & pf=="cd"]$mu, na.rm=T), 2), fpt(sd(markups_G[input=="l" & est=="dlw" & pf=="cd"]$mu, na.rm=T),2), " & ",
        fp(median(markups_HQ[input=="l" & est=="dlw" & pf=="cd"]$mu, na.rm=T), 2), fpt(sd(markups_HQ[input=="l" & est=="dlw" & pf=="cd"]$mu, na.rm=T),2), " & ",
        fp(median(markups[input=="l" & est=="dlw" & pf=="cd"]$mu, na.rm=T), 2), fpt(sd(markups[input=="l" & est=="dlw" & pf=="cd"]$mu, na.rm=T),2), " \\\\  \n")
    cat("$L$ --- Translog & ", 
        fp(median(markups_AF[input=="l" & est=="dlw" & pf=="tl"]$mu, na.rm=T), 2), fpt(sd(markups_AF[input=="l" & est=="dlw" & pf=="tl"]$mu, na.rm=T),2), " & ", 
        fp(median(markups_G[input=="l" & est=="dlw" & pf=="tl"]$mu, na.rm=T), 2), fpt(sd(markups_G[input=="l" & est=="dlw" & pf=="tl"]$mu, na.rm=T),2), " & ",
        fp(median(markups_HQ[input=="l" & est=="dlw" & pf=="tl"]$mu, na.rm=T), 2), fpt(sd(markups_HQ[input=="l" & est=="dlw" & pf=="tl"]$mu, na.rm=T),2), " & ",
        fp(median(markups[input=="l" & est=="dlw" & pf=="tl"]$mu, na.rm=T), 2), fpt(sd(markups[input=="l" & est=="dlw" & pf=="tl"]$mu, na.rm=T),2), " \\\\  \n")
cat("\\addlinespace \\hline \\addlinespace \n")
cat("Share of observations & ", fpp(share_AF,1), " & ", fpp(share_G,1), " & ", fpp(share_HQ,1), " & 100\\% \\\\  \n")
cat("\\bottomrule \n")
cat("\\end{tabular} \n")
# cat("\\end{adjustbox} \n")
cat("\\justify \\footnotesize \\emph{Notes:} This table reports the median estimated markup, across all firms and years, by industry group. The different specifications are a combination of assuming different production functions (Cobb-Douglas or Translog) and using different input variables to extract markups ($M$ being material costs, $L$ being labour costs, and $V$ being the sum of both). Standard deviations reported in parentheses. \n")
cat("\\end{table} \n")
sink()

