#///////////////////////////////////////////////////////////////////////////////
# File name:		figure_5_network_metrics_corrmat.R
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

# Check correlations
spearman = cor(network_metrics[, .SD, .SDcols = !c("id_seller", "year", "seller_sec", "ind_group", "seller_weight")], method = "spearman")
pearson  = cor(network_metrics[, .SD, .SDcols = !c("id_seller", "year", "seller_sec", "ind_group", "seller_weight")], method = "pearson")
spearman[upper.tri(spearman, diag=T)] <- 0
pearson[ lower.tri(spearman, diag=F)] <- 0
combined = spearman + pearson
corrs = as.data.table(combined)
namesss = as.list(colnames(corrs))
corrs[, vars := namesss]
corrs <- melt(corrs, id.vars=c("vars"))
setnames(corrs, c("y","x","corr"))
corrs[, x := factor(as.character(x), levels=namesss)]
corrs[, y := factor(as.character(y), levels=namesss)]
corrs[x==y, corr := NA]

# Labels
labs <- c(expression("Within Industry Importance"),
         expression("Within Industry Importance"^"W"),
         expression("Across Industry Importance"),
         expression("Across Industry Importance"^"W"),
         expression("Seller Reciprocity"),
         expression("Seller Reciprocity"^"W"),
         expression("Reciprocity Degree"),
         expression("Reciprocity Degree"^"W"),
         expression("Selling Frequency"),
         expression("Selling Frequency"^"W"),
         expression("Unique Seller"),
         expression("Unique Seller"^"W"),
         expression("Competition Intensity"),
         expression("Competition Intensity"^"W"),
         expression("Local Focus"),
         expression("Local Focus"^"W"),
         expression("Vertical Focus"),
         expression("Vertical Focus"^"W"),
         expression("Buyer Specialisation"),
         expression("Buyer Specialisation"^"W"),
         expression("Unique Buyer Count"), 
         expression("Unique Industry Count"),
         expression("Unique Province Count"))

# Plot Spearman Correlation Coefficients
ggplot(corrs, aes(x = x, y = y, fill = corr)) +
    geom_tile() +
    scale_fill_gradient2(low = "red3", mid = "white", high = "green4", na.value="black", limits=c(-1,1)) +
    labs(title = "", x = "", y = "", fill = "") + 
    geom_vline(xintercept = c(2.5,4.5,6.5,8.5,10.5,12.5,14.5,16.5,18.5,20.5)) +
    geom_vline(xintercept = c(4.5, 10.5, 14.5, 20.5), linewidth=2) +
    geom_hline(yintercept = 24-c(2.5,4.5,6.5,8.5,10.5,12.5,14.5,16.5,18.5,20.5)) +
    geom_hline(yintercept = 24-c(4.5, 10.5, 14.5, 20.5), linewidth=2) +
    scale_y_discrete(limits=rev, labels = rev(labs), expand = c(0,0)) + scale_x_discrete(position="top", labels = labs, expand = c(0,0)) + 
    theme_bw() +
    theme(legend.position = "bottom", text = element_text(family = "Palatino"),
          axis.text = element_text(size = 12, color = "black"), 
          legend.text = element_text(size = 12, color = "black"), 
          legend.key.width = unit(3, "cm"),
          axis.text.x = element_text(angle=45, hjust=0),
          plot.margin = margin(-20,100,0,0, "points"))
ggsave(paste0(pathFig,sysdate,"_figure_5_corr_matrix_network_metrics.pdf"), width = 11, height = 10, device=cairo_pdf)
