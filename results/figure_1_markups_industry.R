#///////////////////////////////////////////////////////////////////////////////
# File name:		figure_1_markups_industry.R
# Author:			Miguel Vázquez Vázquez
# Creation date:    11 April 2024
# Description:      
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
markups <- merge(markups_ML, markups_V, by=c("id","year","ind"))
markups <- markups[!is.na(mu_v_dlw_cd)]
rm(markups_ML, markups_V)

# Drop OLS markups
ols_cols = colnames(markups)[grep("_ols", colnames(markups))]
markups <- markups[, !ols_cols, with=FALSE]

# Drop missing markup rows
markups <- markups[!is.na(mu_v_dlw_cd)]

# Check correlations between markups
mu_cols = colnames(markups)[grep("mu_", colnames(markups))]
cor(markups[, ..mu_cols], method="pearson")  # check linear correlation
cor(markups[, ..mu_cols], method="spearman") # check rank correlation

# Generate variable for industry group
markups[, ind_group := ifelse(substr(ind,1,1) %in% c("A","B","C","D","E","F"), "AF", ifelse(substr(ind,1,1)=="G", "G", "HQ"))]

# Reshape dataset
markups <- melt(markups, measure.vars = measure(value.name, input, est, pf, sep = "_"))
markups[, est := NULL] # useless column since OLS were dropped

# Define legend labels (\u2013 is unicode en-dash)
lgndlab1 <- "A01\u2013F45"
lgndlab2 <- "G50\u2013G52"
lgndlab3 <- "H55\u2013Q99"

# Plot densities by industry for main
ggplot(markups[input=="v" & pf=="cd" & mu<3 & mu>0], aes(x = mu, fill = ind_group)) + 
    geom_density(alpha = 0.2) + geom_vline(xintercept=1, alpha=0.5) +
    scale_fill_discrete(name="Industry group", labels = c(lgndlab1, lgndlab2, lgndlab3)) + 
    theme_bw() + ylab("Density") + xlab(expression(italic("\u03BC"["it"]^"V"))) +
    theme(legend.position = c(0.865,0.783), text = element_text(family = "Palatino"), 
      legend.text = element_text(size = 25, margin = margin(r = 20)), legend.key.size = unit(10, "mm"), 
      legend.title = element_blank(),
      legend.box.background = element_rect(colour = "black"), axis.text = element_text(size = 25, color = "black"), 
      axis.title.y = element_text(angle = 0, vjust = 0.5), axis.title = element_text(size = 25))
ggsave(paste0(pathFig,sysdate,"_industry_markup_main.pdf"), width = 15, height = 10, device=cairo_pdf)

# Plot densities by industry for appendix
ggplot(markups[mu<3 & mu>0], aes(x = mu, fill = ind_group)) + 
    geom_density(alpha = 0.2) + geom_vline(xintercept=1, alpha=0.5) +
    facet_grid(rows=vars(input),cols=vars(pf), scales = "free_y") +
    scale_fill_discrete(name="Industry group", labels = c(lgndlab1, lgndlab2, lgndlab3)) + 
    theme_bw() + ylab("Density") + xlab(expression(italic("\u03BC"["it"]^"V"))) +
    theme(legend.position = c(0.865,0.783), text = element_text(family = "Palatino"), 
          legend.text = element_text(size = 25, margin = margin(r = 20)), legend.key.size = unit(10, "mm"), 
          legend.title = element_blank(),
          legend.box.background = element_rect(colour = "black"), axis.text = element_text(size = 25, color = "black"), 
          axis.title.y = element_text(angle = 0, vjust = 0.5), axis.title = element_text(size = 25))
ggsave(paste0(pathFig,sysdate,"_industry_markup_appendix.pdf"), width = 15, height = 10, device=cairo_pdf)

