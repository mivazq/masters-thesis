#///////////////////////////////////////////////////////////////////////////////
# File name:		figure_3_network_metrics_summary.R
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

# library(PerformanceAnalytics)
library(ggpubr)

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

# Exclude potential number of buyers (not really well defined)
network_metrics[, pot_n_buyers := NULL]

# Exclude other vars
network_metrics[, c("wsfi_i_c", "wsfi_i_w", "bfi_i_c", "bfi_i_w") := NULL]

# Reorder variables (exclude pot_n_buyers)
network_metrics <- network_metrics[, .(id_seller, year, seller_sec, ind_group, seller_weight,
                                       wsvi_i_c, wsvi_i_w, wsvi_i_u,
                                       bvi_i_c, bvi_i_w, bvi_i_u,
                                       sr_i_c, sr_i_w, sr_i_u,
                                       ri_i_c, ri_i_w, ri_i_u,
                                       us_i_c, us_i_w, us_i_u,
                                       ci_i_c, ci_i_w, ci_i_u,
                                       lo_i_c, lo_i_w, 
                                       ho_i_c, ho_i_w, 
                                       act_n_buyers, act_n_sectors, act_n_provinces)]

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
labs = c(expression(italic("Within Industry Importance"["i"]^"C")),
         expression(italic("Within Industry Importance"["i"]^"W")),
         expression(italic("Within Industry Importance"["i"]^"U")),
         expression(italic("Across Industry Importance"["i"]^"C")),
         expression(italic("Across Industry Importance"["i"]^"W")),
         expression(italic("Across Industry Importance"["i"]^"U")),
         expression(italic("Seller Reciprocity"["i"]^"C")),
         expression(italic("Seller Reciprocity"["i"]^"W")),
         expression(italic("Seller Reciprocity"["i"]^"U")),
         expression(italic("Selling Intensity"["i"]^"C")),
         expression(italic("Selling Intensity"["i"]^"W")),
         expression(italic("Selling Intensity"["i"]^"U")),
         expression(italic("Unique Seller"["i"]^"C")),
         expression(italic("Unique Seller"["i"]^"W")),
         expression(italic("Unique Seller"["i"]^"U")),
         expression(italic("Competition Intensity"["i"]^"C")),
         expression(italic("Competition Intensity"["i"]^"W")),
         expression(italic("Competition Intensity"["i"]^"U")),
         expression(italic("Non-Local Orientation"["i"]^"C")),
         expression(italic("Non-Local Orientation"["i"]^"W")),
         expression(italic("Non-Horizontal Orientation"["i"]^"C")),
         expression(italic("Non-Horizontal Orientation"["i"]^"W")),
         expression(italic("Unique Buyer Count"["i"])), 
         expression(italic("Unique Industry Count"["i"])),
         expression(italic("Unique Province Count"["i"])))

# Plot Spearman Correlation Coefficients
ggplot(corrs, aes(x = x, y = y, fill = corr)) +
    geom_tile() +
    scale_fill_gradient2(low = "red3", mid = "white", high = "green4", na.value="black", limits=c(-1,1)) +
    labs(title = "", x = "", y = "", fill = "") + 
    geom_vline(xintercept = c(3.5,6.5,9.5,12.5,15.5,18.5,20.5,22.5)) +
    geom_vline(xintercept = c(6.5, 12.5, 18.5), linewidth=2) +
    geom_hline(yintercept = 26-c(3.5,6.5,9.5,12.5,15.5,18.5,20.5,22.5)) +
    geom_hline(yintercept = 26-c(6.5, 12.5, 18.5), linewidth=2) +
    scale_y_discrete(limits=rev, labels = rev(labs), expand = c(0,0)) + scale_x_discrete(position="top", labels = labs, expand = c(0,0)) + 
    theme_bw() +
    theme(legend.position = "bottom", text = element_text(family = "Palatino"),
          axis.text = element_text(size = 12, color = "black"), 
          legend.text = element_text(size = 12, color = "black"), 
          legend.key.width = unit(3, "cm"),
          axis.text.x = element_text(angle=45, hjust=0),
          plot.margin = margin(-20,100,0,0, "points"))
ggsave(paste0(pathFig,sysdate,"_corr_matrix_network_metrics.pdf"), width = 11, height = 10, device=cairo_pdf)



# Create table for dispersion at industry-year level, per specification
agg <- copy(network_metrics)
agg[, agg_wsvi_i_c := weighted.mean(wsvi_i_c, w=seller_weight), by=c("year","ind_group")]
agg[, agg_bvi_i_c  := weighted.mean(bvi_i_c,  w=seller_weight), by=c("year","ind_group")]
agg[, agg_sr_i_c   := weighted.mean(sr_i_c,   w=seller_weight), by=c("year","ind_group")]
agg[, agg_ri_i_c   := weighted.mean(ri_i_c,   w=seller_weight), by=c("year","ind_group")]
agg[, agg_us_i_c   := weighted.mean(us_i_c,   w=seller_weight), by=c("year","ind_group")]
agg[, agg_ci_i_c   := weighted.mean(ci_i_c,   w=seller_weight), by=c("year","ind_group")]
agg[, agg_lo_i_c   := weighted.mean(lo_i_c,   w=seller_weight), by=c("year","ind_group")]
agg[, agg_ho_i_c   := weighted.mean(ho_i_c,   w=seller_weight), by=c("year","ind_group")]
agg[, agg_n_buy    := weighted.mean(log(act_n_buyers),    w=seller_weight), by=c("year","ind_group")]
agg[, agg_n_ind    := weighted.mean(log(act_n_sectors),   w=seller_weight), by=c("year","ind_group")]
agg[, agg_n_pro    := weighted.mean(log(act_n_provinces), w=seller_weight), by=c("year","ind_group")]
# agg[, agg_wsvi_i_c := median(wsvi_i_c), by=c("year","ind_group")]
# agg[, agg_bvi_i_c  := median(bvi_i_c), by=c("year","ind_group")]
# agg[, agg_sr_i_c   := median(sr_i_c), by=c("year","ind_group")]
# agg[, agg_ri_i_c   := median(ri_i_c), by=c("year","ind_group")]
# agg[, agg_us_i_c   := median(us_i_c), by=c("year","ind_group")]
# agg[, agg_ci_i_c   := median(ci_i_c), by=c("year","ind_group")]
# agg[, agg_lo_i_c   := median(lo_i_c), by=c("year","ind_group")]
# agg[, agg_ho_i_c   := median(ho_i_c), by=c("year","ind_group")]
# agg[, agg_n_buy    := median(log(act_n_buyers)), by=c("year","ind_group")]
# agg[, agg_n_ind    := median(log(act_n_sectors)), by=c("year","ind_group")]
# agg[, agg_n_pro    := median(log(act_n_provinces)), by=c("year","ind_group")]

agg <- dcast(data = agg,
             formula = year + ind_group ~ .,
             fun = mean,
             value.var = colnames(agg)[grepl("agg_",colnames(agg))])

agg_all <- copy(network_metrics)
agg_all[, agg_wsvi_i_c := weighted.mean(wsvi_i_c, w=seller_weight), by=c("year")]
agg_all[, agg_bvi_i_c  := weighted.mean(bvi_i_c,  w=seller_weight), by=c("year")]
agg_all[, agg_sr_i_c   := weighted.mean(sr_i_c,   w=seller_weight), by=c("year")]
agg_all[, agg_ri_i_c   := weighted.mean(ri_i_c,   w=seller_weight), by=c("year")]
agg_all[, agg_us_i_c   := weighted.mean(us_i_c,   w=seller_weight), by=c("year")]
agg_all[, agg_ci_i_c   := weighted.mean(ci_i_c,   w=seller_weight), by=c("year")]
agg_all[, agg_lo_i_c   := weighted.mean(lo_i_c,   w=seller_weight), by=c("year")]
agg_all[, agg_ho_i_c   := weighted.mean(ho_i_c,   w=seller_weight), by=c("year")]
agg_all[, agg_n_buy    := weighted.mean(log(act_n_buyers),    w=seller_weight), by=c("year")]
agg_all[, agg_n_ind    := weighted.mean(log(act_n_sectors),   w=seller_weight), by=c("year")]
agg_all[, agg_n_pro    := weighted.mean(log(act_n_provinces), w=seller_weight), by=c("year")]
# agg_all[, agg_wsvi_i_c := median(wsvi_i_c), by=c("year")]
# agg_all[, agg_bvi_i_c  := median(bvi_i_c), by=c("year")]
# agg_all[, agg_sr_i_c   := median(sr_i_c), by=c("year")]
# agg_all[, agg_ri_i_c   := median(ri_i_c), by=c("year")]
# agg_all[, agg_us_i_c   := median(us_i_c), by=c("year")]
# agg_all[, agg_ci_i_c   := median(ci_i_c), by=c("year")]
# agg_all[, agg_lo_i_c   := median(lo_i_c), by=c("year")]
# agg_all[, agg_ho_i_c   := median(ho_i_c), by=c("year")]
# agg_all[, agg_n_buy    := median(log(act_n_buyers)), by=c("year")]
# agg_all[, agg_n_ind    := median(log(act_n_sectors)), by=c("year")]
# agg_all[, agg_n_pro    := median(log(act_n_provinces)), by=c("year")]

agg_all <- dcast(data = agg_all,
                 formula = year ~ .,
                 fun = mean,
                 value.var = colnames(agg_all)[grepl("agg_",colnames(agg_all))])
agg_all[, ind_group := "Z_ALL"]

agg <- rbind(agg, agg_all)


agg <- melt(agg,
            id.vars=c("year","ind_group"))


lgndlab1 <- "Produce"
lgndlab2 <- "Commerce"
lgndlab3 <- "Service"
lgndlab4 <- "All industries"

blankdata = data.table(x=2008:2011,  y=c(1,1,1,1), ind_group=NA)

# rename variables
agg[variable=="agg_wsvi_i_c", var := as.character(expression(italic("Within Industry Importance"["i"]^"C")))]
agg[variable=="agg_bvi_i_c",  var := as.character(expression(italic("Across Industry Importance"["i"]^"C")))]
agg[variable=="agg_sr_i_c",   var := as.character(expression(italic("Seller Reciprocity"["i"]^"C")))]
agg[variable=="agg_ri_i_c",   var := as.character(expression(italic("Selling Intensity"["i"]^"C")))]
agg[variable=="agg_us_i_c",   var := as.character(expression(italic("Unique Seller"["i"]^"C")))]
agg[variable=="agg_ci_i_c",   var := as.character(expression(italic("Competition Intensity"["i"]^"C")))]
agg[variable=="agg_lo_i_c",   var := as.character(expression(italic("Non-Local Orientation"["i"]^"C")))]
agg[variable=="agg_ho_i_c",   var := as.character(expression(italic("Non-Horizontal Orientation"["i"]^"C")))]
agg[variable=="agg_n_buy",    var := as.character(expression("log("*italic("Unique Buyer Count"["i"])*")"))]
agg[variable=="agg_n_ind",    var := as.character(expression("log("*italic("Unique Industry Count"["i"])*")"))]
agg[variable=="agg_n_pro",    var := as.character(expression("log("*italic("Unique Province Count"["i"])*")"))]
agg[, var := factor(var, levels=c(as.character(expression(italic("Within Industry Importance"["i"]^"C"))),
                                  as.character(expression(italic("Across Industry Importance"["i"]^"C"))),
                                  as.character(expression(italic("Seller Reciprocity"["i"]^"C"))),
                                  as.character(expression(italic("Selling Intensity"["i"]^"C"))),
                                  as.character(expression(italic("Unique Seller"["i"]^"C"))),
                                  as.character(expression(italic("Competition Intensity"["i"]^"C"))),
                                  as.character(expression(italic("Non-Local Orientation"["i"]^"C"))),
                                  as.character(expression(italic("Non-Horizontal Orientation"["i"]^"C"))),
                                  as.character(expression("log("*italic("Unique Buyer Count"["i"])*")")),
                                  as.character(expression("log("*italic("Unique Industry Count"["i"])*")")),
                                  as.character(expression("log("*italic("Unique Province Count"["i"])*")"))
                                  ))]

#  Plot only CD for input V (for main text)
fig <- ggplot(agg, aes(x = year, y = value, color = ind_group)) +
    geom_line(linewidth=0.5) + geom_point(size=1) + #geom_blank(data=blankdata, aes(x=x,y=y)) +
    # scale_linetype_manual(values= c(1,2), labels=c("Aggregate","Median")) +
    scale_color_discrete(labels = c(lgndlab1, lgndlab2, lgndlab3, lgndlab4), type = c("#f8766d","#00ba38","#619cff","#a3a500"))+ #, name=var) +
    # scale_y_continuous(labels = function(x) fp(x, dig=1)) +
    scale_y_continuous(limits=c(0,NA)) + facet_wrap(~var, scales = "free_y", labeller = label_parsed) +
    xlab("Year") + ylab("") +
    theme_bw() + guides(linetype="none", color = guide_legend(byrow = TRUE)) +
    theme(legend.position = c(0.9,0.12), text = element_text(family = "Palatino"), legend.spacing.y = unit(0, 'pt'),
          strip.background=element_blank(), strip.text=element_text(size=20), legend.title = element_blank(), #legend.title = element_text(size = 25), 
          legend.text = element_text(size = 25, margin = margin(r = 20)), legend.key.size = unit(20, "mm"), 
          axis.text = element_text(size = 25, color = "black"), axis.title.y = element_text(angle = 0, vjust = 0.5),
          axis.title = element_text(size = 25, color = "black"), axis.title.x = element_blank())
fig
ggsave(fig, filename=paste0(pathFig,sysdate,"_aggregate_network.pdf"), width = 20, height = 10, device=cairo_pdf)

















std <- dcast(network_metrics,
             formula = year + ind_group ~ .,
             fun = sd,
             value.var = c("wsvi_i_c", "wsvi_i_w", "wsvi_i_u",
                           "bvi_i_c", "bvi_i_w", "bvi_i_u",
                           "sr_i_c", "sr_i_w", "sr_i_u",
                           "ri_i_c", "ri_i_w", "ri_i_u",
                           "us_i_c", "us_i_w", "us_i_u",
                           "ci_i_c", "ci_i_w", "ci_i_u",
                           "lo_i_c", "lo_i_w", 
                           "ho_i_c", "ho_i_w", 
                           "act_n_buyers", "act_n_sectors", "act_n_provinces"))
std <- melt(data = std,
            id.vars = c("year", "ind_group"))

# Add rows for all industries combined
for (i_year in 2008:2011) {
    for (var in c("wsvi_i_c", "wsvi_i_w", "wsvi_i_u",
                  "bvi_i_c", "bvi_i_w", "bvi_i_u",
                  "sr_i_c", "sr_i_w", "sr_i_u",
                  "us_i_c", "us_i_w", "us_i_u",
                  "ci_i_c", "ci_i_w", "ci_i_u",
                  "lo_i_c", "lo_i_w", 
                  "ho_i_c", "ho_i_w")) {
        std <- rbind(std, list(i_year, "Z_ALL", var, sd(as.vector(network_metrics[year==i_year, .SD, .SDcols=var])[[1]])))
    }
    for (var in c("ri_i_c", "ri_i_w", "ri_i_u","act_n_buyers", "act_n_sectors", "act_n_provinces")) {
        std <- rbind(std, list(i_year, "Z_ALL", var, sd(log(as.vector(network_metrics[year==i_year, .SD, .SDcols=var])[[1]]) )))
    }
}






std[, var_group := ifelse(variable %in% c("wsvi_i_u", "bvi_i_u", "sr_i_u", "ri_i_u", "us_i_u", "ci_i_u"), "unconditional", 
                          ifelse(variable %in% c("wsvi_i_w", "bvi_i_w", "sr_i_w", "ri_i_w", "us_i_w", "ci_i_w", "lo_i_w", "ho_i_w"), "weighted", "conditional"))]


# Plot evolution by production function and input (for main)
# lgndlab1 <- "Produce"
# lgndlab2 <- "Commerce"
# lgndlab3 <- "Service"
# lgndlab4 <- "All industries"
ggplot(std[var_group=="conditional" & ind_group=="Z_ALL"], aes(x = year, y = value, color = variable)) +
    geom_line(linewidth=1.5) + geom_point(size=3) +
    # scale_color_discrete(labels = c(lgndlab1, lgndlab2, lgndlab3, lgndlab4), type = c("#f8766d","#00ba38","#619cff","#a3a500")) +
    scale_y_continuous(labels = function(x) fp(x, dig=2), limits = c(0,NA)) +
    xlab("Year") + ylab(expression(italic("\u03C3"["\u03BC"["it"]^"V"]))) +
    theme_bw() + 
    theme(legend.position = "bottom", text = element_text(family = "Palatino"), 
          legend.title = element_blank(), 
          strip.background=element_blank(), strip.text=element_text(size=25, color = "black"),
          legend.text = element_text(size = 25, margin = margin(r = 20)), legend.key.size = unit(20, "mm"), 
          axis.text = element_text(size = 25, color = "black"), 
          axis.title.y = element_text(angle = 0, vjust = 0.5, size = 25, color = "black"), 
          axis.title.x = element_blank())
ggsave(paste0(pathFig,sysdate,"_std_network_metrics.pdf"), width = 15, height = 10, device=cairo_pdf)

setorder(std, variable, ind_group)









# chart.Correlation(network_metrics[match==1, .SD, .SDcols = !c("id_seller", "year", "seller_sec", "ind_group", "match")],
                  # method="pearson",
                  # histogram=TRUE,
                  # pch=16)

# network_metrics <- melt(network_metrics[, .SD, .SDcols=!c("pot_n_buyers", "match")],
                        # id.vars=c("id_seller", "year", "seller_sec", "ind_group"))

levels(network_metrics$variable)




plot0101 <- ggplot(network_metrics[variable=="act_n_buyers"], aes(x=value)) + geom_density() + scale_x_continuous(trans="log", breaks=c(1,2,3,4,5,10,20,100,1000,3000)) + theme_bw()
plot0202 <- ggplot(network_metrics[variable=="act_n_sectors"], aes(x=as.factor(value))) + geom_bar(col = "black") + scale_x_discrete(limits=factor(1:60), breaks=c(1,10,20,30,40,50,60)) + theme_bw()
plot0303 <- ggplot(network_metrics[variable=="act_n_provinces"], aes(x=as.factor(value))) + geom_bar(col = "black") + scale_x_discrete(breaks=c(1,6,12,18,24)) + theme_bw()
plot0101
plot0202
plot0303


plot0404 <- ggplot(network_metrics[variable=="wsvi_i_c"], aes(x=value)) + geom_density() + scale_x_continuous(trans="log", breaks=c(0.025,0.05,0.1,0.25,0.5,1)) + theme_bw()
plot0505 <- ggplot(network_metrics[variable=="wsvi_i_w"], aes(x=value)) + geom_density() + scale_x_continuous(trans="log", breaks=c(0.025,0.05,0.1,0.25,0.5,1)) + theme_bw()
plot0606 <- ggplot(network_metrics[variable=="wsvi_i_u"], aes(x=value)) + geom_density() + scale_x_continuous(trans="log", breaks=c(0.0000000025,0.00000025,0.000025,0.0025,0.25), labels = label_scientific()) + theme_bw()
plot0404
plot0505
plot0606

plot45 <- 


plot0707 <- ggplot(network_metrics[variable=="bvi_i_c"], aes(x=value)) + geom_density() + scale_x_continuous(trans="log", breaks=c(0.025,0.05,0.1,0.25,0.5,1)) + theme_bw()
plot0808 <- ggplot(network_metrics[variable=="bvi_i_w"], aes(x=value)) + geom_density() + scale_x_continuous(trans="log", breaks=c(0.025,0.05,0.1,0.25,0.5,1)) + theme_bw()
plot0909 <- ggplot(network_metrics[variable=="bvi_i_u"], aes(x=value)) + geom_density() + scale_x_continuous(trans="log", breaks=c(0.0000000025,0.00000025,0.000025,0.0025,0.25), labels = label_scientific()) + theme_bw()
plot0707
plot0808
plot0909


# there are zeros here
plot1010 <- ggplot(network_metrics[variable=="us_i_c"], aes(x=value)) + geom_density() + scale_x_continuous(trans="log", breaks=c(0.025,0.05,0.1,0.25,0.5,1)) + theme_bw()
plot1111 <- ggplot(network_metrics[variable=="us_i_w"], aes(x=value)) + geom_density() + scale_x_continuous(trans="log", breaks=c(0.025,0.05,0.1,0.25,0.5,1)) + theme_bw()
plot1212 <- ggplot(network_metrics[variable=="us_i_u"], aes(x=value)) + geom_density() + scale_x_continuous(trans="log", breaks=c(0.0000000025,0.00000025,0.000025,0.0025,0.25), labels = label_scientific()) + theme_bw()
plot1010
plot1111
plot1212


# there are zeros here
plot1313 <- ggplot(network_metrics[variable=="sr_i_c"], aes(x=value)) + geom_density() + scale_x_continuous(trans="log", breaks=c(0.025,0.05,0.1,0.25,0.5,1)) + theme_bw()
plot1414 <- ggplot(network_metrics[variable=="sr_i_w"], aes(x=value)) + geom_density() + scale_x_continuous(trans="log", breaks=c(0.025,0.05,0.1,0.25,0.5,1)) + theme_bw()
plot1515 <- ggplot(network_metrics[variable=="sr_i_u"], aes(x=value)) + geom_density() + scale_x_continuous(trans="log", breaks=c(0.0000000025,0.00000025,0.000025,0.0025,0.25), labels = label_scientific()) + theme_bw()
plot1313
plot1414
plot1515


plot1616 <- ggplot(network_metrics[variable=="ri_i_c"], aes(x=value)) + geom_density() + scale_x_continuous(trans="log", breaks=c(1,2,5,10,100,1000)) + theme_bw()
plot1717 <- ggplot(network_metrics[variable=="ri_i_w"], aes(x=value)) + geom_density() + scale_x_continuous(trans="log", breaks=c(1,2,5,10,100,1000)) + theme_bw()
plot1818 <- ggplot(network_metrics[variable=="ri_i_u"], aes(x=value)) + geom_density() + scale_x_continuous(trans="log", breaks=c(0.000025,0.0025,0.25), labels = label_scientific()) + theme_bw()
plot1616
plot1717
plot1818





plot1616 <- ggplot(network_metrics[variable=="ri_i_c"], aes(x=value)) + geom_density() + scale_x_continuous(trans="log", breaks=c(1,2,5,10,100,1000)) + theme_bw()
plot1717 <- ggplot(network_metrics[variable=="ri_i_w"], aes(x=value)) + geom_density() + scale_x_continuous(trans="log", breaks=c(1,2,5,10,100,1000)) + theme_bw()
plot1818 <- ggplot(network_metrics[variable=="ri_i_u"], aes(x=value)) + geom_density() + scale_x_continuous(trans="log", breaks=c(0.000025,0.0025,0.25), labels = label_scientific()) + theme_bw()
plot1616
plot1717
plot1818


ggarrange(plot0101, plot0202, plot0303, plot0404, plot0505, plot0606, plot0707, plot0808, plot0909, plot1010, plot1111, plot1212, plot1313, plot1414, plot1515, plot1616, plot1717, plot1818, ncol = 24, nrow = 24)


