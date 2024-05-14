#///////////////////////////////////////////////////////////////////////////////
# File name:		appendixC_table_network_summary_sellers.R
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
load(file = paste0(pathEst, "output/sum_stats_sellers.Rdata"))
markups <- merge(markups_ML, markups_V, by=c("id","year","ind"))
markups <- markups[!is.na(V)] # exclude non-active observations
rm(markups_ML, markups_V)

# Generate variable for industry group
markups[, ind_group := ifelse(substr(ind,1,1) %in% c("A","B","C","D","E","F"), "AF", ifelse(substr(ind,1,1)=="G", "G", "HQ"))]

# Match markups estimation sample and network sample
match <- merge(markups[,.(id,year,mu=1)], sum_stats_sellers, by.x=c("id","year"), by.y=c("id_sri","year"), all.y=T)

# Create category variable (either matched, or network only, or network only but missing sector info)
match[, category := ifelse(!is.na(mu), "intersection", "network")]
table(match$category)

# Summarize statistics by category (pool all years, not really much difference over time)
custom_fun <- function(x) {
    return(median(as.double(x)))
}
stats <- dcast(match,
               category ~.,
               # fun = custom_fun,
               fun = mean,
               value.var=c("unique_buyers","unique_industries","trans_freq","trans_val","avg_trans_amount","also_buyer"))

# Produce table
sink(paste0(pathTab,sysdate,"_appendixC_table_network_summary_sellers.tex"))
cat("\\begin{table}[!htbp]\\centering \n")
cat("\\caption{\\label{tab:NetworkSellers} Summary Statistics of Sellers in Network} \n")
cat("\\begin{adjustbox}{width=\\columnwidth,center} \n")
cat("\\begin{tabular}{lcc}")
cat("\\toprule \n")
cat("Summary statistic & Intersection set & Network-only set \\\\ \n")
cat("\\addlinespace \\hline \\addlinespace \n")
cat("Dummy: Files purchase annexes & ", 
    fp(stats[category=="intersection"]$also_buyer,3), " & ", 
    fp(stats[category=="network"]$also_buyer,3),      "\\\\  \n")
cat("Number of buyers supplied & ", 
    fp(stats[category=="intersection"]$unique_buyers,1), " & ", 
    fp(stats[category=="network"]$unique_buyers,1),      "\\\\  \n")
cat("Number of industries supplied & ", 
    fp(stats[category=="intersection"]$unique_industries,1), " & ", 
    fp(stats[category=="network"]$unique_industries,1),      "\\\\  \n")
cat("Transaction frequency & ", 
    fp(stats[category=="intersection"]$trans_freq,1), " & ", 
    fp(stats[category=="network"]$trans_freq,1),      "\\\\  \n")
cat("Transaction value & ", 
    fp(stats[category=="intersection"]$trans_val,0), " & ", 
    fp(stats[category=="network"]$trans_val,0),      "\\\\  \n")
cat("Average amount per transaction & ", 
    fp(stats[category=="intersection"]$avg_trans_amount,0), " & ", 
    fp(stats[category=="network"]$avg_trans_amount,0),      "\\\\  \n")
cat("\\midrule \n")
cat("Number of unique sellers & ", 
    fp(uniqueN(match[category=="intersection"]$id),0), " & ", 
    fp(uniqueN(match[category=="network"]$id),0),      "\\\\  \n")
cat("Number of total single transactions & ", 
    fp(sum(match[category=="intersection"]$trans_freq),0), " & ", 
    fp(sum(match[category=="network"]$trans_freq),0),      "\\\\  \n")
cat("Sum of total transactions value & ", 
    fp(sum(match[category=="intersection"]$trans_val),0), " & ", 
    fp(sum(match[category=="network"]$trans_val),0),      "\\\\  \n")
cat("\\bottomrule \n")
cat("\\end{tabular} \n")
cat("\\end{adjustbox} \n")
cat("\\justify \\footnotesize \\emph{Notes:} The reported values are averages across all sellers and all years for each category. The decision of pooling all years together stems from the minimal variance of these summary statistics over time. \n")
cat("\\end{table} \n")
sink()


