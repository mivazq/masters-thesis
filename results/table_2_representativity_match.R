#///////////////////////////////////////////////////////////////////////////////
# File name:		table_2_representativity_match.R
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
match <- merge(markups, network_metrics, by.x=c("id","year"), by.y=c("id_seller","year"))

# Representativity of match for MARKUP SAMPLE
table(match$year)/table(markups$year) # by year
table(match$ind_group)/table(markups$ind_group) # by industry group
table(match$ind_group, match$year)/table(markups$ind_group, markups$year) # by year and industry group
df_represent1 <- as.data.table(table(match$ind_group, match$year)/table(markups$ind_group, markups$year))
setnames(df_represent1, c("V1","V2","N"), c("grp","year","val"))
df_represent1_all <- as.data.table(table(match$year)/table(markups$year))
setnames(df_represent1_all, c("V1","N"), c("year","val"))

# Representativity of match for NETWORK SAMPLE
network_metrics[, ind_group := ifelse(substr(seller_sec,1,1) %in% c("A","B","C","D","E","F"), "AF", ifelse(substr(seller_sec,1,1)=="G", "G", "HQ"))]
table(match$year)/table(network_metrics$year) # by year
table(match$ind_group)/table(network_metrics$ind_group) # by industry group
table(match$ind_group, match$year)/table(network_metrics$ind_group, network_metrics$year) # by year and industry group
df_represent2 <- as.data.table(table(match$ind_group, match$year)/table(network_metrics$ind_group, network_metrics$year))
setnames(df_represent2, c("V1","V2","N"), c("grp","year","val"))
df_represent2_all <- as.data.table(table(match$year)/table(network_metrics$year))
setnames(df_represent2_all, c("V1","N"), c("year","val"))

# Produce table
sink(paste0(pathTab,sysdate,"_table_2_representativity_match.tex"))
cat("\\begin{table}[!htbp]\\centering \n")
cat("\\caption{\\label{tab:RepresentativityMatch} Representativity of Matched Observations} \n")
cat("\\begin{adjustbox}{width=\\columnwidth,center} \n")
cat("\\begin{tabular}{lccccccccc}")
cat("\\toprule \n")
cat("\\multicolumn{1}{l}{\\multirow{3}{*}{Industry group}} & \\multicolumn{4}{c}{As share of markup sample} & & \\multicolumn{4}{c}{As share of network sample} \\\\ \n")
cat("\\addlinespace \\cline{2-5} \\cline{7-10} \\addlinespace \n")
cat(" & 2008 & 2009 & 2010 & 2011 & & 2008 & 2009 & 2010 & 2011 \\\\ \n")
cat("\\addlinespace \\hline \\addlinespace \n")
cat("A01--F45 & ", 
    fpp(df_represent1[grp=="AF" & year==2008]$val, 1), " & ", fpp(df_represent1[grp=="AF" & year==2009]$val, 1), " & ", 
    fpp(df_represent1[grp=="AF" & year==2010]$val, 1), " & ", fpp(df_represent1[grp=="AF" & year==2011]$val, 1), " & & ", 
    fpp(df_represent2[grp=="AF" & year==2008]$val, 1), " & ", fpp(df_represent2[grp=="AF" & year==2009]$val, 1), " & ", 
    fpp(df_represent2[grp=="AF" & year==2010]$val, 1), " & ", fpp(df_represent2[grp=="AF" & year==2011]$val, 1), "\\\\  \n")
cat("G50--G52 & ", 
    fpp(df_represent1[grp=="G" & year==2008]$val, 1), " & ", fpp(df_represent1[grp=="G" & year==2009]$val, 1), " & ", 
    fpp(df_represent1[grp=="G" & year==2010]$val, 1), " & ", fpp(df_represent1[grp=="G" & year==2011]$val, 1), " & & ", 
    fpp(df_represent2[grp=="G" & year==2008]$val, 1), " & ", fpp(df_represent2[grp=="G" & year==2009]$val, 1), " & ", 
    fpp(df_represent2[grp=="G" & year==2010]$val, 1), " & ", fpp(df_represent2[grp=="G" & year==2011]$val, 1), "\\\\  \n")
cat("H55--Q99 & ", 
    fpp(df_represent1[grp=="HQ" & year==2008]$val, 1), " & ", fpp(df_represent1[grp=="HQ" & year==2009]$val, 1), " & ", 
    fpp(df_represent1[grp=="HQ" & year==2010]$val, 1), " & ", fpp(df_represent1[grp=="HQ" & year==2011]$val, 1), " & & ", 
    fpp(df_represent2[grp=="HQ" & year==2008]$val, 1), " & ", fpp(df_represent2[grp=="HQ" & year==2009]$val, 1), " & ", 
    fpp(df_represent2[grp=="HQ" & year==2010]$val, 1), " & ", fpp(df_represent2[grp=="HQ" & year==2011]$val, 1), "\\\\  \n")
cat("All industries & ", 
    fpp(df_represent1_all[year==2008]$val, 1), " & ", fpp(df_represent1_all[year==2009]$val, 1), " & ", 
    fpp(df_represent1_all[year==2010]$val, 1), " & ", fpp(df_represent1_all[year==2011]$val, 1), " & & ", 
    fpp(df_represent2_all[year==2008]$val, 1), " & ", fpp(df_represent2_all[year==2009]$val, 1), " & ", 
    fpp(df_represent2_all[year==2010]$val, 1), " & ", fpp(df_represent2_all[year==2011]$val, 1), " \\\\  \n")
cat("\\bottomrule \n")
cat("\\end{tabular} \n")
cat("\\end{adjustbox} \n")
cat("\\justify \\footnotesize \\emph{Notes:} This table reports the shares of observations from the markup estimation sample that appear in the transaction data as sellers and for which I can thus construct network metrics. \n")
cat("\\end{table} \n")
sink()


