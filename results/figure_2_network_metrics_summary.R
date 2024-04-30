#///////////////////////////////////////////////////////////////////////////////
# File name:		figure_2_network_metrics_summary.R
# Author:			Miguel Vázquez Vázquez
# Creation date:    03 April 2024
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

# # Generate variable for industry group
# markups[, ind_group := ifelse(substr(ind,1,1) %in% c("A","B","C","D","E","F"), "AF", ifelse(substr(ind,1,1)=="G", "G", "HQ"))]
# 
# # Fix markups of industries H55 AND O91
# medgroup = median(markups[ind_group=="HQ" & ind %nin% c("H55","O91")]$mu_v_dlw_cd) # median for industry group H55-Q99 (excluding these two industries)
# fix_H55 = medgroup - median(markups[ind=="H55"]$mu_v_dlw_cd) # median deviation from group median
# fix_O91 = medgroup - median(markups[ind=="O91"]$mu_v_dlw_cd) # median deviation from group median
# markups[ind=="H55", mu_v_dlw_cd := mu_v_dlw_cd + fix_H55] # fix H55
# markups[ind=="O91", mu_v_dlw_cd := mu_v_dlw_cd + fix_O91] # fix O91

# Merge
network_metrics <- merge(network_metrics, markups[, .(id_seller=id, year, match = 1)], by=c("id_seller","year"), all.x = T)
network_metrics[is.na(match), match := 0]
rm(markups)

# Reshape data
network_metrics <- melt(network_metrics, id.vars = c("year","id_seller","seller_sec","match"))
matched <- network_metrics[match==1]
matched[, sample := "match"]
network_metrics[, sample := "all"]
network_metrics <- rbind(network_metrics, matched)
network_metrics[, match := NULL]
rm(matched)

# Winsorize values for CTV and KC (top 0.1%)
# threshold <- p999(network_metrics[variable=="ctv_i" & sample=="all"]$value)
# network_metrics[variable=="ctv_i" & value>threshold, value := threshold]
# threshold <- p999(network_metrics[variable=="kc_i" & sample=="all"]$value)
# network_metrics[variable=="kc_i" & value>threshold, value := threshold]

# # Center data
# network_metrics_centered <- copy(network_metrics)
# network_metrics_centered[, value  := (value - mean(value))/mean(value), by = c("variable", "year", "seller_sec")]

# Generate qq-data
qqpoints = seq(0,1,0.01)
qqplot_data = data.table(quantiles = rep(qqpoints, 2),
                         axis      = c(rep("x", length(qqpoints)), 
                                       rep("y", length(qqpoints))))
for (var in levels(network_metrics$variable)) {
    for (smpl in c("all","match")) {
        dist = tanh(network_metrics[variable==var & sample==smpl]$value) # take logs
        # dist = network_metrics[variable==var & sample==smpl]$value # take logs
        dist = dist # remove non-finite values
        vals = quantile(dist, probs=qqpoints, na.rm=T)
        qqplot_data[axis==ifelse(smpl=="all","x","y"), value := vals]
    }
    setnames(qqplot_data, "value", var)
}
rm(var, smpl, vals, qqpoints)
qqplot_data <- melt(qqplot_data, 
                    id.vars       = c('quantiles', 'axis'), 
                    measure.vars  = levels(network_metrics$variable))
qqplot_data <- dcast(qqplot_data,
                     quantiles + variable ~ axis,
                     fun = mean,
                     value.var="value")

#///////////////////////////////////////////////////////////////////////////////
#### OUPUT PLOTS ####
#///////////////////////////////////////////////////////////////////////////////

# geom_density_2d ???
# ggplot(qqplot_data[variable %nin% c("ctv_i","kc_i")], aes(x = x, y = y)) +
ggplot(qqplot_data, aes(x = x, y = y)) + 
    geom_point(shape="circle open") + geom_segment(aes(x=0,xend=1,y=0,yend=1), colour='red', linewidth=0.1) + 
    facet_wrap(~variable, ncol=2, scales="free")
    
qqplot(y=network_metrics[variable=="kc_i" & sample=="match"]$value, x=network_metrics[variable=="kc_i" & sample=="all"]$value)


# Non-centered measures
# VWSI_i (0 impossible)
ggplot(network_metrics[variable=="v_wsi_i"], aes(x = value, fill=sample)) + 
    geom_density(alpha=0.5) + scale_x_continuous(trans="log") +
    ylab("Density") + xlab(expression("log("*italic("VWSI"["i"])*")")) +
    scale_fill_manual(values=c("#619cff", "#f8766d")) + 
    annotate("text", x=0.00001, y=0.25, label = "Share of zeros for all: 0",
             col="#619cff", size=8, family = "Palatino", hjust="left") + 
    annotate("text", x=0.00001, y=0.2, label = "Share of zeros for match: 0",
             col="#f8766d", size=8, family = "Palatino", hjust="left") + 
    theme_bw() + theme(legend.position = "bottom", text = element_text(family = "Palatino"))
ggsave(filename = paste0(pathFig,sysdate,'_figure_2_distribution_network_vwsi.pdf'), width = 10, height = 10)

# VWSI_i^W (0 impossible)
ggplot(network_metrics[variable=="v_wsi_iw"], aes(x = value, fill=sample)) + 
    geom_density(alpha=0.5) + scale_x_continuous(trans="log") + 
    ylab("Density") + xlab(expression("log("*italic("VWSI"["i"]^"W")*")")) +
    scale_fill_manual(values=c("#619cff", "#f8766d")) + 
    annotate("text", x=0.00001, y=0.25, label = "Share of zeros for all: 0",
             col="#619cff", size=8, family = "Palatino", hjust="left") + 
    annotate("text", x=0.00001, y=0.2, label = "Share of zeros for match: 0",
             col="#f8766d", size=8, family = "Palatino", hjust="left") + 
    theme_bw() + theme(legend.position = "bottom", text = element_text(family = "Palatino"))
ggsave(filename = paste0(pathFig,sysdate,'_figure_2_distribution_network_vwsi_w.pdf'), width = 10, height = 10)

# FWSI_i (0 impossible)
ggplot(network_metrics[variable=="f_wsi_i"], aes(x = value, fill=sample)) + 
    geom_density(alpha=0.5) + scale_x_continuous(trans="log") +
    ylab("Density") + xlab(expression("log("*italic("FWSI"["i"])*")")) +
    scale_fill_manual(values=c("#619cff", "#f8766d")) + 
    annotate("text", x=0.00001, y=0.25, label = "Share of zeros for all: 0",
             col="#619cff", size=8, family = "Palatino", hjust="left") + 
    annotate("text", x=0.00001, y=0.2, label = "Share of zeros for match: 0",
             col="#f8766d", size=8, family = "Palatino", hjust="left") + 
    theme_bw() + theme(legend.position = "bottom", text = element_text(family = "Palatino"))
ggsave(filename = paste0(pathFig,sysdate,'_figure_2_distribution_network_fwsi.pdf'), width = 10, height = 10)

# FWSI_i^W (0 impossible)
ggplot(network_metrics[variable=="f_wsi_iw"], aes(x = value, fill=sample)) + 
    geom_density(alpha=0.5) + scale_x_continuous(trans="log") + 
    ylab("Density") + xlab(expression("log("*italic("FWSI"["i"]^"W")*")")) +
    scale_fill_manual(values=c("#619cff", "#f8766d")) + 
    annotate("text", x=0.00001, y=0.25, label = "Share of zeros for all: 0",
             col="#619cff", size=8, family = "Palatino", hjust="left") + 
    annotate("text", x=0.00001, y=0.2, label = "Share of zeros for match: 0",
             col="#f8766d", size=8, family = "Palatino", hjust="left") + 
    theme_bw() + theme(legend.position = "bottom", text = element_text(family = "Palatino"))
ggsave(filename = paste0(pathFig,sysdate,'_figure_2_distribution_network_fwsi_w.pdf'), width = 10, height = 10)

# VBI_i
ggplot(network_metrics[variable=="v_bi_i"], aes(x = value, fill=sample)) + 
    geom_density(alpha=0.5) + scale_x_continuous(trans="log") +
    ylab("Density") + xlab(expression("log("*italic("VBI"["i"])*")")) +
    scale_fill_manual(values=c("#619cff", "#f8766d")) + 
    annotate("text", x=0.0000001, y=0.25, label = "Share of zeros for all: 0",
             col="#619cff", size=8, family = "Palatino", hjust="left") + 
    annotate("text", x=0.0000001, y=0.2, label = "Share of zeros for match: 0",
             col="#f8766d", size=8, family = "Palatino", hjust="left") + 
    theme_bw() + theme(legend.position = "bottom", text = element_text(family = "Palatino"))
ggsave(filename = paste0(pathFig,sysdate,'_figure_2_distribution_network_vbi.pdf'), width = 10, height = 10)

# VBI_i^W
ggplot(network_metrics[variable=="v_bi_iw"], aes(x = value, fill=sample)) + 
    geom_density(alpha=0.5) + scale_x_continuous(trans="log") + 
    ylab("Density") + xlab(expression("log("*italic("VBI"["i"]^"W")*")")) +
    scale_fill_manual(values=c("#619cff", "#f8766d")) + 
    annotate("text", x=0.0000001, y=0.25, label = "Share of zeros for all: 0",
             col="#619cff", size=8, family = "Palatino", hjust="left") + 
    annotate("text", x=0.0000001, y=0.2, label = "Share of zeros for match: 0",
             col="#f8766d", size=8, family = "Palatino", hjust="left") +  
    theme_bw() + theme(legend.position = "bottom", text = element_text(family = "Palatino"))
ggsave(filename = paste0(pathFig,sysdate,'_figure_2_distribution_network_vbi_w.pdf'), width = 10, height = 10)

# FBI_i
ggplot(network_metrics[variable=="f_bi_i"], aes(x = value, fill=sample)) + 
    geom_density(alpha=0.5) + scale_x_continuous(trans="log") +
    ylab("Density") + xlab(expression("log("*italic("FBI"["i"])*")")) +
    scale_fill_manual(values=c("#619cff", "#f8766d")) + 
    annotate("text", x=0.0000001, y=0.25, label = "Share of zeros for all: 0",
             col="#619cff", size=8, family = "Palatino", hjust="left") + 
    annotate("text", x=0.0000001, y=0.2, label = "Share of zeros for match: 0",
             col="#f8766d", size=8, family = "Palatino", hjust="left") + 
    theme_bw() + theme(legend.position = "bottom", text = element_text(family = "Palatino"))
ggsave(filename = paste0(pathFig,sysdate,'_figure_2_distribution_network_fbi.pdf'), width = 10, height = 10)

# FBI_i^W
ggplot(network_metrics[variable=="f_bi_iw"], aes(x = value, fill=sample)) + 
    geom_density(alpha=0.5) + scale_x_continuous(trans="log") + 
    ylab("Density") + xlab(expression("log("*italic("FBI"["i"]^"W")*")")) +
    scale_fill_manual(values=c("#619cff", "#f8766d")) + 
    annotate("text", x=0.0000001, y=0.25, label = "Share of zeros for all: 0",
             col="#619cff", size=8, family = "Palatino", hjust="left") + 
    annotate("text", x=0.0000001, y=0.2, label = "Share of zeros for match: 0",
             col="#f8766d", size=8, family = "Palatino", hjust="left") +  
    theme_bw() + theme(legend.position = "bottom", text = element_text(family = "Palatino"))
ggsave(filename = paste0(pathFig,sysdate,'_figure_2_distribution_network_fbi_w.pdf'), width = 10, height = 10)

# SR_i
zeros = table(network_metrics[variable=="sr_i"]$value==0,network_metrics[variable=="sr_i"]$sample)[2,]/colSums(table(network_metrics[variable=="sr_i"]$value==0,network_metrics[variable=="sr_i"]$sample))
ggplot(network_metrics[variable=="sr_i"], aes(x = value, fill=sample)) + 
    geom_density(alpha=0.5) + scale_x_continuous(trans="log") +
    ylab("Density") + xlab(expression("log("*italic("SR"["i"])*")")) +
    scale_fill_manual(values=c("#619cff", "#f8766d")) + 
    annotate("text", x=0.0001, y=0.3, label = paste0("Share of zeros for all: ",round(zeros["all"],2)),
             col="#619cff", size=8, family = "Palatino", hjust="left") + 
    annotate("text", x=0.0001, y=0.25, label = paste0("Share of zeros for match: ",round(zeros["match"],2)),
             col="#f8766d", size=8, family = "Palatino", hjust="left") + 
    theme_bw() + theme(legend.position = "bottom", text = element_text(family = "Palatino"))
ggsave(filename = paste0(pathFig,sysdate,'_figure_2_distribution_network_sr.pdf'), width = 10, height = 10)

# SR_i^W
zeros = table(network_metrics[variable=="sr_iw"]$value==0,network_metrics[variable=="sr_iw"]$sample)[2,]/colSums(table(network_metrics[variable=="sr_iw"]$value==0,network_metrics[variable=="sr_iw"]$sample))
ggplot(network_metrics[variable=="sr_iw"], aes(x = value, fill=sample)) + 
    geom_density(alpha=0.5) + scale_x_continuous(trans="log") + 
    ylab("Density") + xlab(expression("log("*italic("SR"["i"]^"W")*")")) +
    scale_fill_manual(values=c("#619cff", "#f8766d")) + 
    annotate("text", x=0.0001, y=0.3, label = paste0("Share of zeros for all: ",round(zeros["all"],2)),
             col="#619cff", size=8, family = "Palatino", hjust="left") + 
    annotate("text", x=0.0001, y=0.25, label = paste0("Share of zeros for match: ",round(zeros["match"],2)),
             col="#f8766d", size=8, family = "Palatino", hjust="left") + 
    theme_bw() + theme(legend.position = "bottom", text = element_text(family = "Palatino"))
ggsave(filename = paste0(pathFig,sysdate,'_figure_2_distribution_network_sr_w.pdf'), width = 10, height = 10)

# KC
# zeros = table(network_metrics[variable=="kc_i"]$value==0,network_metrics[variable=="kc_i"]$sample)[2,]/colSums(table(network_metrics[variable=="kc_i"]$value==0,network_metrics[variable=="kc_i"]$sample))
# ones = table(network_metrics[variable=="kc_i"]$value==1,network_metrics[variable=="kc_i"]$sample)[2,]/colSums(table(network_metrics[variable=="kc_i"]$value==1,network_metrics[variable=="kc_i"]$sample))
ggplot(network_metrics[variable=="kc_i"], aes(x = value, fill=sample)) + 
    geom_density(alpha=0.5) + scale_x_continuous(trans="log") + 
    ylab("Density") + xlab(expression("log("*italic("KC"["i"])*")")) +
    scale_fill_manual(values=c("#619cff", "#f8766d")) + 
    # annotate("text", x=0.0001, y=0.3, label = paste0("Share of ones for all: ",round(ones["all"],2)),
    #          col="#619cff", size=8, family = "Palatino", hjust="left") + 
    # annotate("text", x=0.0001, y=0.25, label = paste0("Share of ones for match: ",round(ones["match"],2)),
    #          col="#f8766d", size=8, family = "Palatino", hjust="left") +
    annotate("text", x=0.0001, y=0.3, label = "Share of ones for all: 0",
             col="#619cff", size=8, family = "Palatino", hjust="left") + 
    annotate("text", x=0.0001, y=0.25, label = "Share of ones for match: 0",
             col="#f8766d", size=8, family = "Palatino", hjust="left") + 
    theme_bw() + theme(legend.position = "bottom", text = element_text(family = "Palatino"))
ggsave(filename = paste0(pathFig,sysdate,'_figure_2_distribution_network_kc.pdf'), width = 10, height = 10)



# 
# 
# 
# ggplot(network_metrics[variable=="wsi_i"], aes(x = value, fill=sample)) + 
#     geom_density(alpha=0.5) + scale_x_continuous(trans="log")
# 
# 
# + scale_y_continuous(trans="log")
# 
# + facet_wrap(~variable, ncol = 1, scales="free")
# 
# 
# + 
#     scale_x_continuous(trans='log') + theme_bw() + theme(legend.position = "bottom")
# ggsave(figure, filename = paste0(pathFig,sysdate,'_figure_2_distribution_network.pdf'), width = 10, height = 10)
# 
# 
# ggplot(network_metrics, aes(x = value)) + 
#     geom_qq(aes(sample=sample)) + facet_wrap(~variable)
# 
# + 
#     scale_x_continuous(trans='log') + theme_bw() + theme(legend.position = "bottom")
# 
# 
# # Centered measures
# figure = ggplot(network_metrics_centered, aes(x = value)) + 
#     geom_density(aes(color=sample)) + facet_wrap(~variable, ncol = 1, scales="free") + 
#     scale_x_continuous(trans='log') + theme_bw() + theme(legend.position = "bottom")
# ggsave(figure, filename = paste0(pathFig,sysdate,'_figure_2_distribution_network_centered.pdf'), width = 10, height = 10)



# VI_i
ggplot(network_metrics[variable=="vi_i"], aes(x = value, fill=sample)) + 
    geom_density(alpha=0.5) + scale_x_continuous(trans="log") +
    ylab("Density") + xlab(expression("log("*italic("VI"["i"])*")")) +
    scale_fill_manual(values=c("#619cff", "#f8766d")) + 
    annotate("text", x=0.0001, y=0.3, label = "Share of zeros for all: 0",
             col="#619cff", size=8, family = "Palatino", hjust="left") + 
    annotate("text", x=0.0001, y=0.25, label = "Share of zeros for match: 0",
             col="#f8766d", size=8, family = "Palatino", hjust="left") + 
    theme_bw() + theme(legend.position = "bottom", text = element_text(family = "Palatino"))
ggsave(filename = paste0(pathFig,sysdate,'_figure_2_distribution_network_vi.pdf'), width = 10, height = 10)

# VI_i^W
ggplot(network_metrics[variable=="vi_iw"], aes(x = value, fill=sample)) + 
    geom_density(alpha=0.5) + scale_x_continuous(trans="log") + 
    ylab("Density") + xlab(expression("log("*italic("VI"["i"]^"W")*")")) +
    scale_fill_manual(values=c("#619cff", "#f8766d")) + 
    annotate("text", x=0.0001, y=0.3, label = "Share of zeros for all: 0",
             col="#619cff", size=8, family = "Palatino", hjust="left") + 
    annotate("text", x=0.0001, y=0.25, label = "Share of zeros for match: 0",
             col="#f8766d", size=8, family = "Palatino", hjust="left") + 
    theme_bw() + theme(legend.position = "bottom", text = element_text(family = "Palatino"))
ggsave(filename = paste0(pathFig,sysdate,'_figure_2_distribution_network_vi_w.pdf'), width = 10, height = 10)


# FI_i
ggplot(network_metrics[variable=="fi_i"], aes(x = value, fill=sample)) + 
    geom_density(alpha=0.5) + scale_x_continuous(trans="log") +
    ylab("Density") + xlab(expression("log("*italic("FI"["i"])*")")) +
    scale_fill_manual(values=c("#619cff", "#f8766d")) + 
    annotate("text", x=0.0001, y=0.3, label = "Share of zeros for all: 0",
             col="#619cff", size=8, family = "Palatino", hjust="left") + 
    annotate("text", x=0.0001, y=0.25, label = "Share of zeros for match: 0",
             col="#f8766d", size=8, family = "Palatino", hjust="left") + 
    theme_bw() + theme(legend.position = "bottom", text = element_text(family = "Palatino"))
ggsave(filename = paste0(pathFig,sysdate,'_figure_2_distribution_network_fi.pdf'), width = 10, height = 10)

# FI_i^W
ggplot(network_metrics[variable=="fi_iw"], aes(x = value, fill=sample)) + 
    geom_density(alpha=0.5) + scale_x_continuous(trans="log") + 
    ylab("Density") + xlab(expression("log("*italic("FI"["i"]^"W")*")")) +
    scale_fill_manual(values=c("#619cff", "#f8766d")) + 
    annotate("text", x=0.0001, y=0.3, label = "Share of zeros for all: 0",
             col="#619cff", size=8, family = "Palatino", hjust="left") + 
    annotate("text", x=0.0001, y=0.25, label = "Share of zeros for match: 0",
             col="#f8766d", size=8, family = "Palatino", hjust="left") + 
    theme_bw() + theme(legend.position = "bottom", text = element_text(family = "Palatino"))
ggsave(filename = paste0(pathFig,sysdate,'_figure_2_distribution_network_fi_w.pdf'), width = 10, height = 10)

