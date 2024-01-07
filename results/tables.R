#///////////////////////////////////////////////////////////////////////////////
# File name:		tables.R
# Author:			Miguel Vázquez Vázquez
# Creation date:    03 January 2024
# Description:      In this file we produce all tables of the paper's appendix
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

# Sector Distribution
sections <- fread(paste0(pathCle, "output/isic_codes_section.csv"))
sections <- sections[isic_section %nin% c("P", "Q", "R", "S", "T")]
load(file = paste0(pathEst, "input/firm_sample.Rdata"))
load(file = paste0(pathEst, "input/firm_sample_drop.Rdata"))

#///////////////////////////////////////////////////////////////////////////////
#### Table 1: Sector Distribution                               # Section ? ####
#///////////////////////////////////////////////////////////////////////////////

df_table <- sections
C1 <- as.data.table(table(firm_sample$isic_section))
C2 <- as.data.table(table(firm_sample$isic_section)/nrow(firm_sample))
C3 <- as.data.table(table(firm_sample[entity=="Company"]$isic_section)/table(firm_sample$isic_section))
C4 <- as.data.table(table(firm_sample[entity=="Sole proprietorship"]$isic_section)/table(firm_sample$isic_section))
df_table <- merge(df_table, C1[,.(isic_section=V1, C1=N)], by="isic_section")
df_table <- merge(df_table, C2[,.(isic_section=V1, C2=N)], by="isic_section")
df_table <- merge(df_table, C3[,.(isic_section=V1, C3=N)], by="isic_section")
df_table <- merge(df_table, C4[,.(isic_section=V1, C4=N)], by="isic_section")
setnames(df_table, c("isic_section", "isic_section_desc"), c("sec","desc"))

sink(paste0(pathTab,sysdate,"_sector_distribution.tex"))
# cat("\\documentclass{article} \n")
# cat("\\usepackage{siunitx} \n")
# cat("\\usepackage{fullpage} \n")
# cat("\\usepackage{booktabs} \n")
# cat("\\usepackage{adjustbox} \n")
# cat("\\usepackage{ragged2e} \n")
# cat("\\usepackage{multirow} \n")
# cat("\\begin{document} \n")
cat("\\begin{table}[!htbp]\\centering \n")
cat("\\caption{\\label{SectorDistribution} ISIC Revision 3.1 Section Distribution of the Sample Firms} \n")
cat("\\begin{adjustbox}{width=\\columnwidth,center} \n")
cat("\\begin{tabular}{p{0.4\\textwidth}crS[table-format=3.2]S[table-format=3.2]S[table-format=3.2]} \n")
cat("\\toprule \n")
cat("            & \\multicolumn{1}{c}{ISIC} & \\multicolumn{1}{c}{Number} & \\multicolumn{1}{c}{Share} & \\multicolumn{2}{c}{Entity type} \\\\ \n")
cat("\\addlinespace \\cline{5-6} \\addlinespace \n")
cat("Description & \\multicolumn{1}{c}{Section} & \\multicolumn{1}{c}{of firms} & \\multicolumn{1}{c}{of total} & \\multicolumn{1}{c}{Companies} & \\multicolumn{1}{c}{Sole-proprietorships} \\\\ \n")
cat("\\addlinespace \\hline \\addlinespace \n")
for (sect in sort(df_table$sec)){
    cat(df_table[sec==sect]$desc, " & ", df_table[sec==sect]$sec, " & ", fp(df_table[sec==sect]$C1), " & ", fpp(df_table[sec==sect]$C2,2), " & ", 
        fpp(df_table[sec==sect]$C3,2), " & ", fpp(df_table[sec==sect]$C4,2), " \\\\  \n")
}
cat("\\bottomrule \n")
cat("\\end{tabular} \n")
cat("\\end{adjustbox} \n")
cat("\\justify \\footnotesize \\emph{Notes:} Due to the low relevance for this work and low sample size, I exclude from my potential sample firms belonging to ISIC Rev.3.1 sections P (\\textit{Activities of private households as employers and undifferentiated production activities of private households},",fp(nrow(firm_sample_drop[isic_section=="P"])),"firms) and Q (\\textit{Extraterritorial organizations and bodies},",fp(nrow(firm_sample_drop[isic_section=="Q"])),"firms), as well as Ecuador's CIIU Rev.3.1 specific sections R (\\textit{Private sector salaried work},",fp(nrow(firm_sample_drop[isic_section=="R"])),"firms), S (\\textit{Public sector salaried work},",fp(nrow(firm_sample_drop[isic_section=="S"])),"firms), and T (\\textit{Without economic activity - CIIU},",fp(nrow(firm_sample_drop[isic_section=="T"])),"firms). Additionally, I exclude further",fp(nrow(firm_sample_drop[is.na(isic_section)])),"firms which would have counted towards my sample but for which I do not have industry information. After the additional exclusion of SOE my final sample consists of",nrow(firm_sample),"firms.\n")
cat("\\end{table} \n")
# cat("\\end{document} \n")
sink()

#///////////////////////////////////////////////////////////////////////////////
#### Table ?  # Section ? ####
#///////////////////////////////////////////////////////////////////////////////
