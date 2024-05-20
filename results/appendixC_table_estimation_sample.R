#///////////////////////////////////////////////////////////////////////////////
# File name:		appendixC_table_estimation_sample.R
# Author:			Miguel Vázquez Vázquez
# Creation date:    31 March 2024
# Description:      In this file we produce Table 1 (about sample)
#
# Input:
#                   $pathEst/input/firm_sample.Rdata
#                   $pathEst/input/firm_sample_drop.Rdata
#                   $pathEst/input/isic_codes_section.csv
# Output:
#                   $pathTab/$sysdate_sector_distribution.tex
#                   -
#///////////////////////////////////////////////////////////////////////////////
source('~/data/transactions_ecuador/3_mivazq/Masters_Thesis/setup.R')
#///////////////////////////////////////////////////////////////////////////////
#----                               LOAD DATA                               ----
#///////////////////////////////////////////////////////////////////////////////

# Load table by industry obs
df_table <- as.data.table(read_excel(paste0(pathEst, "output/sample_by_industry_pre_exclusion.xlsx")))
df_table$desc <- gsub('organizations', 'organisations', df_table$desc) # change to british english

# Load markup and network data, merge, prepare to join with table
load(file=paste0(pathEst, "output/firm_markups_V.Rdata"))
load(file=paste0(pathEst, "output/network_metrics.Rdata"))
match <- merge(markups_V[!is.na(mu_v_dlw_tl)], network_metrics, by.x=c("id","year","ind"), by.y=c("id_seller","year","seller_sec"))
rm(markups_V, network_metrics)
match <- match[, .(id,year,ind)]
match <- dcast(match, ind~., fun.aggregate = list(length, uniqueN), value.var = "id")

# Create columns with shares of observations and firms matched in network data
df_table <- merge(df_table, match, all.x=T)
df_table[, id_length  := id_length/obs_1st_stage]
df_table[, id_uniqueN := id_uniqueN/ids_1st_stage]

# "Emphasize" latex function
emph <- function(value) {
    if (value=="NA\\%") {
        return(paste0("\\textcolor{red}{\\emph{-}}"))
    } else {
        return(paste0("\\textcolor{red}{\\emph{", value,"}}"))
    }
}

sink(paste0(pathTab,sysdate,"_appendixC_table_estimation_sample.tex"))
cat("\\begin{table}[!htbp]\\centering \n")
cat("\\caption{\\label{tab:IndustryDistribution} Industry Distribution of Markup Estimation Sample and Match with Network} \n")
cat("\\begin{adjustbox}{width=\\columnwidth,center} \n")
cat("\\begin{tabular}{lp{1\\textwidth}rrcrrcrr}")
cat("\\toprule \n")
cat("\\multicolumn{1}{l}{\\multirow{3}{*}{Code}} & \\multicolumn{1}{l}{\\multirow{3}{*}{Industry Description}} & \\multicolumn{2}{c}{1st Stage} & & \\multicolumn{2}{c}{2nd Stage} & & \\multicolumn{2}{c}{Share Matched in Network} \\\\ \n")
cat("\\addlinespace \\cline{3-4} \\cline{6-7} \\cline{9-10} \\addlinespace \n")
cat("& & \\multicolumn{1}{c}{Observations} & \\multicolumn{1}{c}{Unique firms} & & \\multicolumn{1}{c}{Observations} & \\multicolumn{1}{c}{Unique firms} & & \\multicolumn{1}{c}{Observations} & \\multicolumn{1}{c}{Unique firms} \\\\ \n")
cat("\\addlinespace \\hline \\addlinespace \n")
for (industry in sort(df_table$ind)){
    if (df_table[ind==industry]$obs_1st_stage>=100) {
        cat(df_table[ind==industry]$ind,           " & ", df_table[ind==industry]$desc, " & ",
            fp(df_table[ind==industry]$obs_1st_stage), " & ", fp(df_table[ind==industry]$ids_1st_stage), " & & ",
            fp(df_table[ind==industry]$obs_2nd_stage), " & ", fp(df_table[ind==industry]$ids_2nd_stage), " & & ",
            fpp(df_table[ind==industry]$id_length,1),   " & ", fpp(df_table[ind==industry]$id_uniqueN,1),  " \\\\  \n")
    }
    else {
        cat(emph(df_table[ind==industry]$ind),           " & ", emph(df_table[ind==industry]$desc), " & ",
            emph(fp(df_table[ind==industry]$obs_1st_stage)), " & ", emph(fp(df_table[ind==industry]$ids_1st_stage)), " & & ",
            emph(fp(df_table[ind==industry]$obs_2nd_stage)), " & ", emph(fp(df_table[ind==industry]$ids_2nd_stage)), " & & ",
            emph(fpp(df_table[ind==industry]$id_length,1)),   " & ", emph(fpp(df_table[ind==industry]$id_uniqueN,1)),  " \\\\  \n")
    }
}
cat("\\bottomrule \n")
cat("\\end{tabular} \n")
cat("\\end{adjustbox} \n")
cat("\\justify \\footnotesize \\emph{Notes:} Industries are defined according to ISIC Revision 3.1 divisions (2-digits level, where the prefix letter indicates the corresponding ISIC Revision 3.1 section). For the 1\\ts{st} stage I consider all valid tax filings as described in Section \\ref{sec:data} in the main text. For the 2\\ts{nd} stage all valid observations are considered for which a valid observation is also available in the previous year. Finally, for the markup estimation procedure I exclude all industries (formatted in \\textcolor{red}{\\emph{red cursive}}) where the number of observations available for the 1\\ts{st} stage is below 100. The last two columns report the shares of observations and unique firms that find a match in the transaction network out of the 1\\ts{st} stage values. Obviously, excluded industries are not matched. Note that further observations may be excluded later on, for example in the case of negative markups in a given specification. \n")
cat("\\end{table} \n")
sink()
