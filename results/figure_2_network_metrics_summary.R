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

# Merge
network_metrics <- merge(network_metrics, markups[, .(id_seller=id, year, match = mu_v_dlw_cd)], by=c("id_seller","year"), all.x = T)
network_metrics[, match := ifelse(is.na(match),0,1)]
rm(markups)

# Reshape data
network_metrics[, c("tv_i") := NULL]
network_metrics <- melt(network_metrics, id.vars = c("year","id_seller","seller_sec","match"))
matched <- network_metrics[match==1]
matched[, sample := "match"]
network_metrics[, sample := "all"]
network_metrics <- rbind(network_metrics, matched)
network_metrics[, match := NULL]
rm(matched)

# Change order of factors
network_metrics$variable <- factor(network_metrics$variable, levels=c("wsi_i", "wsi_iw", "ti_i", "ti_iw", "sr_i", "sr_iw", "ctv_i", "kc_i"))

# Winsorize values for CTV and KC (top 0.1%)
threshold <- p999(network_metrics[variable=="ctv_i" & sample=="all"]$value)
network_metrics[variable=="ctv_i" & value>threshold, value := threshold]
threshold <- p999(network_metrics[variable=="kc_i" & sample=="all"]$value)
network_metrics[variable=="kc_i" & value>threshold, value := threshold]

# Center data
network_metrics_centered <- copy(network_metrics)
network_metrics_centered[, value  := (value - mean(value))/mean(value), by = c("variable", "year", "seller_sec")]

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

geom_density_2d?


# ggplot(qqplot_data[variable %nin% c("ctv_i","kc_i")], aes(x = x, y = y)) +
ggplot(qqplot_data, aes(x = x, y = y)) + 
    geom_point(shape="circle open") + geom_segment(aes(x=0,xend=1,y=0,yend=1), colour='red', linewidth=0.1) + 
    facet_wrap(~variable, ncol=2, scales="free")
    
qqplot(y=network_metrics[variable=="kc_i" & sample=="match"]$value, x=network_metrics[variable=="kc_i" & sample=="all"]$value)


# Non-centered measures
ggplot(network_metrics[variable=="sr_i"], aes(x = value, fill=sample)) + 
    geom_density(alpha=0.5)

+ facet_wrap(~variable, ncol = 1, scales="free")


+ 
    scale_x_continuous(trans='log') + theme_bw() + theme(legend.position = "bottom")
ggsave(figure, filename = paste0(pathFig,sysdate,'_figure_2_distribution_network.pdf'), width = 10, height = 10)


ggplot(network_metrics, aes(x = value)) + 
    geom_qq(aes(sample=sample)) + facet_wrap(~variable)

+ 
    scale_x_continuous(trans='log') + theme_bw() + theme(legend.position = "bottom")


# Centered measures
figure = ggplot(network_metrics_centered, aes(x = value)) + 
    geom_density(aes(color=sample)) + facet_wrap(~variable, ncol = 1, scales="free") + 
    scale_x_continuous(trans='log') + theme_bw() + theme(legend.position = "bottom")
ggsave(figure, filename = paste0(pathFig,sysdate,'_figure_2_distribution_network_centered.pdf'), width = 10, height = 10)

