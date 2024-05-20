#///////////////////////////////////////////////////////////////////////////////
# File name:		appendixB_figure_inputs_variability.R
# Author:			Miguel Vázquez Vázquez
# Creation date:    16 April 2024
# Description:      This file estimates markups for all firms, year to year.
# Input:            
#                   -

# Output:           
#                   -
#///////////////////////////////////////////////////////////////////////////////
source('~/data/transactions_ecuador/3_mivazq/Masters_Thesis/setup.R')
#///////////////////////////////////////////////////////////////////////////////
#----                       1 - CONSTRUCT METRICS                           ----
#///////////////////////////////////////////////////////////////////////////////

# Load data
df_tax_filings <- fread(file=paste0(pathCle, "output/tax_filings.csv"), na.strings="")
df_firm_info   <- fread(file=paste0(pathCle, "output/firm_info.csv"), na.strings="")
df_deflators   <- fread(file=paste0(pathCle, "output/deflators.csv"), na.strings="")
df_def_assets  <- fread(file=paste0(pathCle, "output/assets_deflators.csv"), na.strings="")

# Merge tax filings panel with firm industry information
panel <- merge(df_tax_filings, df_firm_info, by="id_sri")

# Merge price deflators into panel (try to match on class, if not on group)
# Due to how the deflators file is constructed even if a specific group would 
# have no deflator it inherits from its division or section. Thus, no need to 
# merge further than group.
panel <- merge(panel, df_deflators[,.(year,code,def1=deflator)], 
               by.x=c("year","isic_class"), by.y=c("year","code"), all.x=T)
panel <- merge(panel, df_deflators[,.(year,code,def2=deflator)], 
               by.x=c("year","isic_group"), by.y=c("year","code"), all.x=T)
panel[, deflator := ifelse(!is.na(def1), def1, def2)]
panel[, c("def1","def2") := NULL]

# Merge assets deflators
panel <- merge(panel, df_def_assets[, .(year, capital_deflator = value)], by="year", all.x=T)

# Deflate assets and cost of capital using 
panel[, c("fixed_assets", "capital_costs") := lapply(.SD, function(x) x/panel[["capital_deflator"]]*100), .SDcols = c("fixed_assets", "capital_costs")]

# Deflate other monetary variables
panel[, c("operative_revenues", "material_costs", "labour_costs") := lapply(.SD, function(x) x/panel[["deflator"]]*100), .SDcols = c("operative_revenues", "material_costs", "labour_costs")]

# Generate variable that sums up material and labour costs := "variable_costs"
panel[, variable_costs := material_costs + labour_costs]

# Get lag of labour, materials
setorder(panel, "id_sri", "year")
panel[, material_costs_lag := shift(material_costs, type="lag"), by = id_sri]
panel[, labour_costs_lag   := shift(labour_costs,   type="lag"), by = id_sri]
panel[, variable_costs_lag := shift(variable_costs, type="lag"), by = id_sri]
panel[, fixed_assets_lag   := shift(fixed_assets,   type="lag"), by = id_sri]

# Construct Davis-Haltiwanger Growth Rate
panel[, M_gr := (material_costs - material_costs_lag) / ( (1/2)*(material_costs + material_costs_lag) )]
panel[, L_gr := (labour_costs   - labour_costs_lag  ) / ( (1/2)*(labour_costs   + labour_costs_lag  ) )]
panel[, V_gr := (variable_costs - variable_costs_lag) / ( (1/2)*(variable_costs + variable_costs_lag) )]
panel[, K_gr := (fixed_assets   - fixed_assets_lag)   / ( (1/2)*(fixed_assets   + fixed_assets_lag)   )]

# Prepare data for plot
variability <- panel[!is.na(M_gr), .(id_sri, year, M_gr, L_gr, V_gr, K_gr)]
variability <- melt(variability,
                    id.vars = c("id_sri","year"),
                    measure.vars = c("M_gr", "L_gr", "V_gr", "K_gr"))
# variability <- variability[variable != "V_gr"] # remove "non-natural" input, no need to plot

# Functions to compute growth rates
# dhgr <- function(t, t1) {
#     return( (t - t1) / ((1/2)*(t + t1)) )
# }
# gr <- function(t, t1) {
#     return( (t - t1) / t1 )
# }

labs = c(expression("Material ("*italic("M")*")"),
         expression("Labour ("*italic("L")*")"),
         expression("Variable ("*italic("V")*")"),
         expression("Capital ("*italic("K")*")"))
         
# Plot densities by industry for main
ggplot(variability, aes(x = value, fill = variable)) + 
    geom_density(data=variability[variable!="K_gr"], alpha = 0.3) + 
    geom_density(data=variability[variable=="K_gr"], aes(x = value, y = -(after_stat(density))), alpha = 1) +
    scale_fill_discrete(name = "Industry Group",  labels = labs, type = c("green2","purple","yellow","black")) + 
    scale_y_continuous(limits = c(-2.1,2.1), breaks = c(-2,-1,0,1,2), labels=c("2","1","0","1","2")) + ylab("Density  ") +
    # scale_x_continuous(limits = c(-2,2), breaks = c(-2,-1,0,1,2), labels=c("-2\n(-1)\n","-1\n(-0.66)","0\n(0%)","1\n(2)","2\n(\u221E)")) + xlab("Davis-Haltiwanger growth rate\n(Standard growth rate)") +
    scale_x_continuous(limits = c(-2,2), breaks = c(-2,-1,0,1,2), labels=c("-2","-1","0","1","2")) + xlab(expression(italic("gr"["it"]^"DH"))) +
    theme_bw() + 
    theme(legend.position = "bottom", text = element_text(family = "Palatino"), 
          legend.text = element_text(size = 25, margin = margin(r = 20)), legend.key.size = unit(10, "mm"), 
          legend.title = element_blank(), 
          legend.box.background = element_rect(colour = "black"), axis.text = element_text(size = 25, color = "black"), 
          axis.title.y = element_text(angle = 0, vjust = 0.5), axis.title = element_text(size = 25, color = "black"))
ggsave(paste0(pathFig,sysdate,"_appendixB_figure_inputs_variability.pdf"), width = 15, height = 10, device=cairo_pdf)

# Check correlations (note that spearman is identical lin and log, due to log being a monotone transformation)
correlations <- data.table(input=c("M", "L", "V", "K"))
M_array <- panel[active==1]$material_costs
L_array <- panel[active==1]$labour_costs
V_array <- panel[active==1]$variable_costs
K_array <- panel[active==1]$fixed_assets
R_array <- panel[active==1]$operative_revenues
correlations[input=="M", pearson_lin := cor(M_array,      R_array, method="pearson")]
correlations[input=="L", pearson_lin := cor(L_array,      R_array, method="pearson")]
correlations[input=="V", pearson_lin := cor(V_array,      R_array, method="pearson")]
correlations[input=="K", pearson_lin := cor(K_array,      R_array, method="pearson")]
correlations[input=="M", spearman    := cor(M_array,      R_array, method="spearman")]
correlations[input=="L", spearman    := cor(L_array,      R_array, method="spearman")]
correlations[input=="V", spearman    := cor(V_array,      R_array, method="spearman")]
correlations[input=="K", spearman    := cor(K_array,      R_array, method="spearman")]
correlations[input=="M", pearson_log := cor(log(M_array), log(R_array), method="pearson")]
correlations[input=="L", pearson_log := cor(log(L_array), log(R_array), method="pearson")]
correlations[input=="V", pearson_log := cor(log(V_array), log(R_array), method="pearson")]
correlations[input=="K", pearson_log := cor(log(K_array), log(R_array), method="pearson")]
correlations


sink(paste0(pathTab,sysdate,"_appendixB_correlations_inputs_rev.tex"))
cat("\\begin{table}[!htbp]\\centering \n")
cat("\\caption{\\label{tab:CorrelationsRevenue} Correlations Between Different Inputs and Revenue} \n")
cat("\\begin{tabular}{lccccc}")
cat("\\toprule \n")
cat("\\multirow{3}{*}{Input} & \\multicolumn{2}{c}{Pearson's $r$} & & \\multicolumn{2}{c}{Spearman's $\\rho$} \\\\ \n")
cat("\\addlinespace \\cline{2-3} \\cline{5-6} \\addlinespace \n")
cat("                        & Linear & Log & & Linear & Log \\\\ \n")
cat("\\addlinespace \\hline \\addlinespace \n")
cat(" Materials ($M$) & ", fp(correlations[input=="M"]$pearson_lin,3), " & ", fp(correlations[input=="M"]$pearson_log,3), " & & \\multicolumn{2}{c}{",fp(correlations[input=="M"]$spearman,3),"} \\\\ \n")
cat(" Labour ($L$) & ", fp(correlations[input=="L"]$pearson_lin,3), " & ", fp(correlations[input=="L"]$pearson_log,3), " & & \\multicolumn{2}{c}{",fp(correlations[input=="L"]$spearman,3),"} \\\\ \n")
cat(" Variable ($V$) & ", fp(correlations[input=="V"]$pearson_lin,3), " & ", fp(correlations[input=="V"]$pearson_log,3), " & & \\multicolumn{2}{c}{",fp(correlations[input=="V"]$spearman,3),"} \\\\ \n")
cat("\\addlinespace \\hline \\addlinespace \n")
cat(" Capital ($K$) & ", fp(correlations[input=="K"]$pearson_lin,3), " & ", fp(correlations[input=="K"]$pearson_log,3), " & & \\multicolumn{2}{c}{",fp(correlations[input=="K"]$spearman,3),"} \\\\ \n")
cat("\\bottomrule \n")
cat("\\end{tabular} \n")
cat("\\justify \\footnotesize \\emph{Notes:} The variable input $V$ is given by summing $M$ and $L$. Capital is included just as proof of concept and does not belong to the possible variable inputs. Spearman's $\\rho$ is identical for linear and log variables due to the log being a monotonic transformation. \n")
cat("\\end{table} \n")
sink()


