#///////////////////////////////////////////////////////////////////////////////
# File name:		table_1_sample.R
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

df_table <- as.data.table(read_excel(paste0(pathEst, "output/sample_by_industry_pre_exclusion.xlsx")))
df_table$desc <- gsub('organizations', 'organisations', df_table$desc) # change to british english
emph <- function(value) {
    return(paste0("\\textcolor{red}{\\emph{", value,"}}"))
}

filename = paste0(pathTab,sysdate,"_table_1_estimation_sample.tex")
if (file.exists(filename)) { file.remove(filename) }
sink(filename)
# cat("\\documentclass{article} \n")
# cat("\\usepackage{siunitx} \n")
# cat("\\usepackage{fullpage} \n")
# cat("\\usepackage{booktabs} \n")
# cat("\\usepackage{adjustbox} \n")
# cat("\\usepackage{ragged2e} \n")
# cat("\\usepackage{multirow} \n")
# cat("\\begin{document} \n")
cat("\\begin{table}[!htbp]\\centering \n")
cat("\\caption{\\label{tab:IndustryDistribution} Industry Distribution of Markup Estimation Sample} \n")
cat("\\begin{adjustbox}{width=\\columnwidth,center} \n")
cat("\\begin{tabular}{lp{1\\textwidth}rrcrr}")
cat("\\toprule \n")
cat("\\multicolumn{1}{l}{\\multirow{3}{*}{Code}} & \\multicolumn{1}{l}{\\multirow{3}{*}{Industry description}} & \\multicolumn{2}{c}{1st stage} & & \\multicolumn{2}{c}{2nd stage} \\\\ \n")
cat("\\addlinespace \\cline{3-4} \\cline{6-7} \\addlinespace \n")
cat("& & \\multicolumn{1}{c}{Observations} & \\multicolumn{1}{c}{Unique firms} & & \\multicolumn{1}{c}{Observations} & \\multicolumn{1}{c}{Unique firms} \\\\ \n")
cat("\\addlinespace \\hline \\addlinespace \n")
for (industry in sort(df_table$ind)){
    if (df_table[ind==industry]$obs_1st_stage>=100) {
        cat(df_table[ind==industry]$ind,           " & ", df_table[ind==industry]$desc, " & ",
            fp(df_table[ind==industry]$obs_1st_stage), " & ", fp(df_table[ind==industry]$ids_1st_stage), " & & ",
            fp(df_table[ind==industry]$obs_2nd_stage), " & ", fp(df_table[ind==industry]$ids_2nd_stage), " \\\\  \n")
    }
    else {
        cat(emph(df_table[ind==industry]$ind),           " & ", emph(df_table[ind==industry]$desc), " & ",
            emph(fp(df_table[ind==industry]$obs_1st_stage)), " & ", emph(fp(df_table[ind==industry]$ids_1st_stage)), " & & ",
            emph(fp(df_table[ind==industry]$obs_2nd_stage)), " & ", emph(fp(df_table[ind==industry]$ids_2nd_stage)), " \\\\  \n")
    }
}
cat("\\bottomrule \n")
cat("\\end{tabular} \n")
cat("\\end{adjustbox} \n")
cat("\\justify \\footnotesize \\emph{Notes:} Industries are defined according to ISIC Revision 3.1 divisions (2-digits level, where the prefix letter indicates the corresponding ISIC Revision 3.1 section). For the 1\\ts{st} stage I consider all valid tax filings as described in Section \\ref{sec:data} in the main text. For the 2\\ts{nd} stage all valid observations are considered for which a valid observation is also available in the previous year. Finally, for the markup estimation procedure I exclude all industries (formatted in \\textcolor{red}{\\emph{red cursive}}) where the number of observations available for the 1\\ts{st} stage is below 100.\n")
cat("\\end{table} \n")
# cat("\\end{document} \n")
sink()
