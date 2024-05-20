#///////////////////////////////////////////////////////////////////////////////
# File name:		figure_4_network_metrics_evolution.R
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
#### EVOLUTION OVER TIME: AGGREGATE AND MEDIAN ####
#///////////////////////////////////////////////////////////////////////////////

# Aggregate
agg <- copy(network_metrics)
agg[, agg_wsvi_i_c := weighted.mean(wsvi_i_c, w=seller_weight), by=c("year","ind_group")]
agg[, agg_bvi_i_c  := weighted.mean(bvi_i_c,  w=seller_weight), by=c("year","ind_group")]
agg[, agg_sr_i_c   := weighted.mean(sr_i_c,   w=seller_weight), by=c("year","ind_group")]
agg[, agg_ri_i_c   := weighted.mean(ri_i_c,   w=seller_weight), by=c("year","ind_group")]
agg[, agg_sf_i_c   := weighted.mean(sf_i_c,   w=seller_weight), by=c("year","ind_group")]
agg[, agg_us_i_c   := weighted.mean(us_i_c,   w=seller_weight), by=c("year","ind_group")]
agg[, agg_ci_i_c   := weighted.mean(ci_i_c,   w=seller_weight), by=c("year","ind_group")]
agg[, agg_lo_i_c   := weighted.mean(lo_i_c,   w=seller_weight), by=c("year","ind_group")]
agg[, agg_vt_i_c   := weighted.mean(vt_i_c,   w=seller_weight), by=c("year","ind_group")]
agg[, agg_sp_i_c   := weighted.mean(sp_i_c,   w=seller_weight), by=c("year","ind_group")]
agg[, agg_n_buy    := weighted.mean(log(act_n_buyers),    w=seller_weight), by=c("year","ind_group")]
agg[, agg_n_ind    := weighted.mean(log(act_n_sectors),   w=seller_weight), by=c("year","ind_group")]
agg[, agg_n_pro    := weighted.mean(log(act_n_provinces), w=seller_weight), by=c("year","ind_group")]
agg_all <- copy(network_metrics)
agg_all[, agg_wsvi_i_c := weighted.mean(wsvi_i_c, w=seller_weight), by=c("year")]
agg_all[, agg_bvi_i_c  := weighted.mean(bvi_i_c,  w=seller_weight), by=c("year")]
agg_all[, agg_sr_i_c   := weighted.mean(sr_i_c,   w=seller_weight), by=c("year")]
agg_all[, agg_ri_i_c   := weighted.mean(ri_i_c,   w=seller_weight), by=c("year")]
agg_all[, agg_sf_i_c   := weighted.mean(sf_i_c,   w=seller_weight), by=c("year")]
agg_all[, agg_us_i_c   := weighted.mean(us_i_c,   w=seller_weight), by=c("year")]
agg_all[, agg_ci_i_c   := weighted.mean(ci_i_c,   w=seller_weight), by=c("year")]
agg_all[, agg_lo_i_c   := weighted.mean(lo_i_c,   w=seller_weight), by=c("year")]
agg_all[, agg_vt_i_c   := weighted.mean(vt_i_c,   w=seller_weight), by=c("year")]
agg_all[, agg_sp_i_c   := weighted.mean(sp_i_c,   w=seller_weight), by=c("year")]
agg_all[, agg_n_buy    := weighted.mean(log(act_n_buyers),    w=seller_weight), by=c("year")]
agg_all[, agg_n_ind    := weighted.mean(log(act_n_sectors),   w=seller_weight), by=c("year")]
agg_all[, agg_n_pro    := weighted.mean(log(act_n_provinces), w=seller_weight), by=c("year")]

agg <- dcast(data = agg,
             formula = year + ind_group ~ .,
             fun = mean,
             value.var = colnames(agg)[grepl("agg_",colnames(agg))])
agg_all <- dcast(data = agg_all,
                 formula = year ~ .,
                 fun = mean,
                 value.var = colnames(agg_all)[grepl("agg_",colnames(agg_all))])
agg_all[, ind_group := "Z_ALL"]
agg <- rbind(agg, agg_all)
rm(agg_all)
agg <- melt(agg, id.vars=c("year","ind_group"))

# Median
med <- copy(network_metrics)
med[, med_wsvi_i_c := median(wsvi_i_c), by=c("year","ind_group")]
med[, med_bvi_i_c  := median(bvi_i_c), by=c("year","ind_group")]
med[, med_sr_i_c   := median(sr_i_c), by=c("year","ind_group")]
med[, med_ri_i_c   := median(ri_i_c), by=c("year","ind_group")]
med[, med_sf_i_c   := median(sf_i_c), by=c("year","ind_group")]
med[, med_us_i_c   := median(us_i_c), by=c("year","ind_group")]
med[, med_ci_i_c   := median(ci_i_c), by=c("year","ind_group")]
med[, med_lo_i_c   := median(lo_i_c), by=c("year","ind_group")]
med[, med_vt_i_c   := median(vt_i_c), by=c("year","ind_group")]
med[, med_sp_i_c   := median(sp_i_c), by=c("year","ind_group")]
med[, med_n_buy    := median(act_n_buyers), by=c("year","ind_group")]
med[, med_n_ind    := median(act_n_sectors), by=c("year","ind_group")]
med[, med_n_pro    := median(act_n_provinces), by=c("year","ind_group")]
med_all <- copy(network_metrics)
med_all[, med_wsvi_i_c := median(wsvi_i_c), by=c("year")]
med_all[, med_bvi_i_c  := median(bvi_i_c), by=c("year")]
med_all[, med_sr_i_c   := median(sr_i_c), by=c("year")]
med_all[, med_ri_i_c   := median(ri_i_c), by=c("year")]
med_all[, med_sf_i_c   := median(sf_i_c), by=c("year")]
med_all[, med_us_i_c   := median(us_i_c), by=c("year")]
med_all[, med_ci_i_c   := median(ci_i_c), by=c("year")]
med_all[, med_lo_i_c   := median(lo_i_c), by=c("year")]
med_all[, med_vt_i_c   := median(vt_i_c), by=c("year")]
med_all[, med_sp_i_c   := median(sp_i_c), by=c("year")]
med_all[, med_n_buy    := median(act_n_buyers), by=c("year")]
med_all[, med_n_ind    := median(act_n_sectors), by=c("year")]
med_all[, med_n_pro    := median(act_n_provinces), by=c("year")]

med <- dcast(data = med,
             formula = year + ind_group ~ .,
             fun = mean,
             value.var = colnames(med)[grepl("med_",colnames(med))])
med_all <- dcast(data = med_all,
                 formula = year ~ .,
                 fun = mean,
                 value.var = colnames(med_all)[grepl("med_",colnames(med_all))])
med_all[, ind_group := "Z_ALL"]
med <- rbind(med, med_all)
rm(med_all)
med <- melt(med, id.vars=c("year","ind_group"))


# Rename variables for plot
agg[variable=="agg_wsvi_i_c", var := "Within Industry Importance"]
agg[variable=="agg_bvi_i_c",  var := "Across Industry Importance"]
agg[variable=="agg_sr_i_c",   var := "Seller Reciprocity"]
agg[variable=="agg_ri_i_c",   var := "Reciprocity Degree"]
agg[variable=="agg_sf_i_c",   var := "Selling Frequency"]
agg[variable=="agg_us_i_c",   var := "Unique Seller"]
agg[variable=="agg_ci_i_c",   var := "Competition Intensity"]
agg[variable=="agg_lo_i_c",   var := "Local Focus"]
agg[variable=="agg_vt_i_c",   var := "Vertical Focus"]
agg[variable=="agg_sp_i_c",   var := "Buyer Specialisation"]
agg[variable=="agg_n_buy",    var := "Unique Buyer Count"]
agg[variable=="agg_n_ind",    var := "Unique Industry Count"]
agg[variable=="agg_n_pro",    var := "Unique Province Count"]
agg[, var := factor(var, levels=c("Within Industry Importance",
                                  "Across Industry Importance",
                                  "Seller Reciprocity",
                                  "Reciprocity Degree",
                                  "Selling Frequency",
                                  "Unique Seller",
                                  "Competition Intensity",
                                  "Local Focus",
                                  "Vertical Focus",
                                  "Buyer Specialisation",
                                  "Unique Buyer Count",
                                  "Unique Industry Count",
                                  "Unique Province Count"))]
med[variable=="med_wsvi_i_c", var := "Within Industry Importance"]
med[variable=="med_bvi_i_c",  var := "Across Industry Importance"]
med[variable=="med_sr_i_c",   var := "Seller Reciprocity"]
med[variable=="med_ri_i_c",   var := "Reciprocity Degree"]
med[variable=="med_sf_i_c",   var := "Selling Frequency"]
med[variable=="med_us_i_c",   var := "Unique Seller"]
med[variable=="med_ci_i_c",   var := "Competition Intensity"]
med[variable=="med_lo_i_c",   var := "Local Focus"]
med[variable=="med_vt_i_c",   var := "Vertical Focus"]
med[variable=="med_sp_i_c",   var := "Buyer Specialisation"]
med[variable=="med_n_buy",    var := "Unique Buyer Count"]
med[variable=="med_n_ind",    var := "Unique Industry Count"]
med[variable=="med_n_pro",    var := "Unique Province Count"]
med[, var := factor(var, levels=c("Within Industry Importance",
                                  "Across Industry Importance",
                                  "Seller Reciprocity",
                                  "Reciprocity Degree",
                                  "Selling Frequency",
                                  "Unique Seller",
                                  "Competition Intensity",
                                  "Local Focus",
                                  "Vertical Focus",
                                  "Buyer Specialisation",
                                  "Unique Buyer Count",
                                  "Unique Industry Count",
                                  "Unique Province Count"))]

# Define legend labels
lgndlab1 <- "Produce"
lgndlab2 <- "Commerce"
lgndlab3 <- "Service"
lgndlab4 <- "All industries"

# Plot
ggplot(med, aes(x = year, y = value, color = ind_group)) +
    geom_line(linewidth=0.5) + geom_point(size=1) +
    scale_color_discrete(labels = c(lgndlab1, lgndlab2, lgndlab3, lgndlab4), type = c("#f8766d","#00ba38","#619cff","#a3a500")) + 
    scale_y_continuous(limits=c(0,NA)) + facet_wrap(~var, scales = "free_y") +
    xlab("Year") + ylab("") +
    theme_bw() + guides(linetype="none", colour = guide_legend(nrow = 1)) +
    theme(legend.position = c(0.6,0.09), text = element_text(family = "Palatino"),
          strip.background=element_blank(), strip.text=element_text(size=20), legend.title = element_blank(), 
          legend.text = element_text(size = 25, margin = margin(r = 20)), legend.key.size = unit(20, "mm"), 
          axis.text = element_text(size = 25, color = "black"), axis.title.y = element_text(angle = 0, vjust = 0.5),
          axis.title = element_text(size = 25, color = "black"), axis.title.x = element_blank())

ggsave(filename=paste0(pathFig,sysdate,"_figure_4_medians_network.pdf"), width = 20, height = 10, device=cairo_pdf)






# #///////////////////////////////////////////////////////////////////////////////
# #### EVOLUTION OVER TIME: DISPERSION ####
# #///////////////////////////////////////////////////////////////////////////////
# 
# std <- dcast(network_metrics,
#              formula = year + ind_group ~ .,
#              fun = sd,
#              value.var = c("wsvi_i_c", "wsvi_i_w", "wsvi_i_u",
#                            "bvi_i_c", "bvi_i_w", "bvi_i_u",
#                            "sr_i_c", "sr_i_w", "sr_i_u",
#                            "sf_i_c", "sf_i_w", "sf_i_u",
#                            "us_i_c", "us_i_w", "us_i_u",
#                            "ci_i_c", "ci_i_w", "ci_i_u",
#                            "lo_i_c", "lo_i_w", 
#                            "vt_i_c", "vt_i_w", 
#                            "act_n_buyers", "act_n_sectors", "act_n_provinces"))
# std <- melt(data = std,
#             id.vars = c("year", "ind_group"))
# 
# # Add rows for all industries combined
# for (i_year in 2008:2011) {
#     for (var in c("wsvi_i_c", "wsvi_i_w", "wsvi_i_u",
#                   "bvi_i_c", "bvi_i_w", "bvi_i_u",
#                   "sr_i_c", "sr_i_w", "sr_i_u",
#                   "us_i_c", "us_i_w", "us_i_u",
#                   "ci_i_c", "ci_i_w", "ci_i_u",
#                   "lo_i_c", "lo_i_w", 
#                   "vt_i_c", "vt_i_w")) {
#         std <- rbind(std, list(i_year, "Z_ALL", var, sd(as.vector(network_metrics[year==i_year, .SD, .SDcols=var])[[1]])))
#     }
#     for (var in c("sf_i_c", "sf_i_w", "sf_i_u","act_n_buyers", "act_n_sectors", "act_n_provinces")) {
#         std <- rbind(std, list(i_year, "Z_ALL", var, sd(log(as.vector(network_metrics[year==i_year, .SD, .SDcols=var])[[1]]) )))
#     }
# }
# 
# 
# 
# 
# 
# 
# std[, var_group := ifelse(variable %in% c("wsvi_i_u", "bvi_i_u", "sr_i_u", "sf_i_u", "us_i_u", "ci_i_u"), "unconditional", 
#                           ifelse(variable %in% c("wsvi_i_w", "bvi_i_w", "sr_i_w", "sf_i_w", "us_i_w", "ci_i_w", "lo_i_w", "vt_i_w"), "weighted", "conditional"))]
# 
# 
# # Plot evolution by production function and input (for main)
# # lgndlab1 <- "Produce"
# # lgndlab2 <- "Commerce"
# # lgndlab3 <- "Service"
# # lgndlab4 <- "All industries"
# ggplot(std[var_group=="conditional" & ind_group=="Z_ALL"], aes(x = year, y = value, color = variable)) +
#     geom_line(linewidth=1.5) + geom_point(size=3) +
#     # scale_color_discrete(labels = c(lgndlab1, lgndlab2, lgndlab3, lgndlab4), type = c("#f8766d","#00ba38","#619cff","#a3a500")) +
#     scale_y_continuous(labels = function(x) fp(x, dig=2), limits = c(0,NA)) +
#     xlab("Year") + ylab(expression(italic("\u03C3"["\u03BC"["it"]^"V"]))) +
#     theme_bw() + 
#     theme(legend.position = "bottom", text = element_text(family = "Palatino"), 
#           legend.title = element_blank(), 
#           strip.background=element_blank(), strip.text=element_text(size=25, color = "black"),
#           legend.text = element_text(size = 25, margin = margin(r = 20)), legend.key.size = unit(20, "mm"), 
#           axis.text = element_text(size = 25, color = "black"), 
#           axis.title.y = element_text(angle = 0, vjust = 0.5, size = 25, color = "black"), 
#           axis.title.x = element_blank())
# ggsave(paste0(pathFig,sysdate,"_std_network_metrics.pdf"), width = 15, height = 10, device=cairo_pdf)
