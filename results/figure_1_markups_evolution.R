#///////////////////////////////////////////////////////////////////////////////
# File name:		figure_1_markups_evolution.R
# Author:			Miguel Vázquez Vázquez
# Creation date:    10 January 2023
# Description:      This file estimates markups for all firms, year to year. The
#                   estimation is performed on an unbalanced panel of firms.
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

# Generate variable for industry group
markups[, ind_group := ifelse(substr(ind,1,1) %in% c("A","B","C","D","E","F"), "AF", ifelse(substr(ind,1,1)=="G", "G", "HQ"))]

# Drop OLS markups, not interested
markups <- markups[est=="dlw"]
markups[, est := NULL]

# Set up table for aggregate markups
agg_mu <- data.table(input     = c("m", "l", "v"),
                     ind_group = c(rep("AF",3), rep("G", 3), rep("HQ", 3), rep("Z_ALL", 3)),
                     pf        = c(rep("cd",12), rep("tl", 12)),
                     year      = c(rep(2008,24), rep(2009,24), rep(2010,24), rep(2011,24)))

# Calculate weighted averages (using input shares as weight)
for (i_year in 2008:2011) {
    for (i_pf in c("cd","tl")) {
        for (i_input in c("m","l","v")) {
            for (i_ind_group in c("AF","G","HQ")) {
                agg_mu[input==i_input & ind_group==i_ind_group & pf==i_pf & year==i_year, 
                       value := weighted.mean(x = markups[input==i_input & ind_group==i_ind_group & pf==i_pf & year==i_year]$mu, 
                                              w = markups[input==i_input & ind_group==i_ind_group & pf==i_pf & year==i_year]$weight, 
                                              na.rm = T)]
            }
            agg_mu[input==i_input & ind_group=="Z_ALL" & pf==i_pf & year==i_year, 
                   value := weighted.mean(x = markups[input==i_input & pf==i_pf & year==i_year]$mu, 
                                          w = markups[input==i_input & pf==i_pf & year==i_year]$weight, 
                                          na.rm = T)]
        }
    }
}
rm(i_input, i_ind_group, i_pf, i_year)

# Define titles for facets (\u03BC is unicode mu)
agg_mu[, facets := paste0("italic(\u03BC^",toupper(input),")")]
agg_mu$facets <- factor(agg_mu$facets, levels=c("italic(\u03BC^M)", "italic(\u03BC^L)", "italic(\u03BC^V)"))

# Define legend labels (\u2013 is unicode en-dash)
lgndlab1 <- "A01\u2013F45"
lgndlab2 <- "G50\u2013G52"
lgndlab3 <- "H55\u2013Q99"
lgndlab4 <- "All industries"


# Plot evolution
for (i_pf in c("cd","tl")) {
    
    # Minimum should be at least 0 on y-axis
    min_val = ifelse(min(agg_mu[pf==i_pf]$value)>0, 0, NA)
    
    ggplot(agg_mu[pf==i_pf], aes(x = year, y = value, color = ind_group)) +
        geom_line() + geom_hline(yintercept=1) +
        geom_point() + facet_wrap(~facets, nrow = 1, ncol = 3, scales = 'free_y', labeller=label_parsed) +
        scale_color_discrete(labels = c(lgndlab1, lgndlab2, lgndlab3, lgndlab4)) +
        scale_y_continuous(limits = c(min_val,NA)) +
        labs(x = "Year", y = "", color="") +
        theme_bw() + 
        theme(legend.position = "bottom", text = element_text(family = "Palatino"), 
              strip.background=element_blank(), strip.text=element_text(size=20),
              legend.text = element_text(size = 20), legend.key.size = unit(20, "mm"), 
              axis.text = element_text(size = 20, color = "black"),
              axis.title = element_text(size = 20))
    ggsave(paste0(pathFig,sysdate,"_",toupper(i_pf),"_aggregate_markup.pdf"), width = 15, height = 10, device=cairo_pdf)
}



# 
# for (i_input in c("m","l","v")) {
#     for (i_pf in c("cd","tl")) {
#         if (i_input=="m") {
#             ggplot(agg_mu[input==i_input & pf==i_pf], aes(x = year, y = value, color = ind_group)) +
#                 geom_line() +
#                 geom_point() +
#                 labs(x = "Year", y = "Aggregate Markup", color = "Industry group") +
#                 theme_bw() + 
#                 theme(legend.position = "bottom", text = element_text(family = "Palatino"))
#             ggsave(paste0(pathFig,sysdate,"_",toupper(i_pf),"_aggregate_markup_",toupper(i_input),".pdf"))
#         }
#         else if (i_input=="l") {
#             
#         }
#         else {
#             ggplot(agg_mu[input==i_input & pf==i_pf], aes(x = year, y = value, color = ind_group)) +
#                 geom_line() +
#                 geom_point() +
#                 labs(x = "Year", y = "", color = "Industry group") +
#                 theme_bw() + 
#                 theme(legend.position = "bottom", text = element_text(family = "Palatino"))
#             ggsave(paste0(pathFig,sysdate,"_",toupper(i_pf),"_aggregate_markup_",toupper(i_input),".pdf"))
#         }
#     }
# }
# rm(i_input, i_pf)
