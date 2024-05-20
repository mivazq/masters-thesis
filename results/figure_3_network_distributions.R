#///////////////////////////////////////////////////////////////////////////////
# File name:		figure_3_network_distributions.R
# Author:			Miguel Vázquez Vázquez
# Creation date:    1 May 2024
# Description:      
#
# Input:            
#                   -

# Output:           
#                   -
#///////////////////////////////////////////////////////////////////////////////
source('~/data/transactions_ecuador/3_mivazq/Masters_Thesis/setup.R')
#///////////////////////////////////////////////////////////////////////////////

#///////////////////////////////////////////////////////////////////////////////
#### PREPARE DATA ####
#///////////////////////////////////////////////////////////////////////////////

# Load data and merge
load(file=paste0(pathEst, "output/firm_markups_V.Rdata"))
load(file=paste0(pathEst, "output/firm_markups_ML.Rdata"))
markups <- merge(markups_ML, markups_V, by=c("id","year","ind"))
markups <- markups[!is.na(mu_v_dlw_cd)]
rm(markups_ML, markups_V)
load(file=paste0(pathEst, "output/network_metrics.Rdata"))

# Merge
network_metrics <- merge(network_metrics, markups[, .(id_seller=id, year, match = 1)], by=c("id_seller","year"), all.x = T)
unique_ids_all_years <- unique(markups$id)
rm(markups)
# network_metrics[is.na(match) & id_seller %in% unique_ids_all_years, match := 0] # label id-year pairs that we don't estimate markups for
network_metrics <- network_metrics[!is.na(match)]
network_metrics[, match := NULL]

# Generate variable for industry group
network_metrics[, ind_group := ifelse(substr(seller_sec,1,1) %in% c("A","B","C","D","E","F"), "AF", ifelse(substr(seller_sec,1,1)=="G", "G", "HQ"))]

#///////////////////////////////////////////////////////////////////////////////
#### CORRELATION MATRIX FIGURE ####
#///////////////////////////////////////////////////////////////////////////////

# melt data
netmet <- melt(network_metrics,
               id.vars = c("id_seller", "year", "seller_sec", "ind_group", "seller_weight"))

# scale variables that are not naturally bounded between 0 and 1
netmet[, max_value := max(value), by = variable]
netmet[, min_value := min(value), by = variable]
netmet[variable %in% c("sf_i_c", "sf_i_w", "act_n_buyers", "act_n_sectors", "act_n_provinces"), value := (value - min_value) / (max_value - min_value)]
netmet[, c("max_value", "min_value") := NULL]

# keep only conditional
netmet_unweighted <- netmet[variable %in% c("wsvi_i_c", "bvi_i_c", 
                                            "sr_i_c", "ri_i_c", "sf_i_c",
                                            "us_i_c", "ci_i_c", 
                                            "lo_i_c", "vt_i_c", 
                                            "sp_i_c", "act_n_buyers", "act_n_sectors", "act_n_provinces")]
netmet_weighted <- netmet[variable %in% c("wsvi_i_w", "bvi_i_w", 
                                          "sr_i_w", "ri_i_w", "sf_i_w",
                                          "us_i_w", "ci_i_w", 
                                          "lo_i_w", "vt_i_w", 
                                          "sp_i_w", "act_n_buyers", "act_n_sectors", "act_n_provinces")]

# plot
labs = c("Within Industry Importance",
         "Across Industry Importance",
         "Seller Reciprocity",
         "Reciprocity Degree",
         "* Selling Frequency",
         "Unique Seller",
         "Competition Intensity",
         "Local Focus",
         "Vertical Focus",
         "Buyer Specialisation", 
         "* Unique Buyer Count", 
         "* Unique Industry Count",
         "* Unique Province Count")


ggplot(netmet_unweighted, aes(x=variable, y=value)) + 
    geom_violin(scale="width", fill="gray", draw_quantiles = c(0.25, 0.75), linetype = "dashed", colour="red") + 
    geom_violin(scale="width", fill="transparent", draw_quantiles = c(0.5), linetype = "solid", linewidth=0.6) +
    scale_x_discrete(limits=rev, labels=rev(labs)) + coord_flip() +
    scale_y_continuous(breaks=c(0, 1/4, 1/3, 1/2, 2/3, 3/4, 1), labels=c("0","0.25","0.33","0.5","0.66","0.75","1"), expand = c(0,0)) + 
    ylab("") + xlab("") + theme_bw() +
    theme(text = element_text(family = "Palatino"),
          axis.text = element_text(size = 12, color = "black"),
          panel.grid.minor = element_blank())
ggsave(paste0(pathFig,sysdate,"_figure_3_network_metrics_dist_unweighted.pdf"), width = 10, height = 15, device=cairo_pdf)

ggplot(netmet_weighted, aes(x=variable, y=value)) + 
    geom_violin(scale="width", fill="gray", draw_quantiles = c(0.25, 0.75), linetype = "dashed", colour="red") + 
    geom_violin(scale="width", fill="transparent", draw_quantiles = c(0.5), linetype = "solid", linewidth=0.6) +
    scale_x_discrete(limits=rev, labels=rev(labs)) + coord_flip() +
    scale_y_continuous(breaks=c(0, 1/4, 1/3, 1/2, 2/3, 3/4, 1), labels=c("0","0.25","0.33","0.5","0.66","0.75","1"), expand = c(0,0)) + 
    ylab("") + xlab("") + theme_bw() +
    theme(text = element_text(family = "Palatino"),
          axis.text = element_text(size = 12, color = "black"),
          panel.grid.minor = element_blank())
ggsave(paste0(pathFig,sysdate,"_figure_3_network_metrics_dist_weighted.pdf"), width = 10, height = 15, device=cairo_pdf)

