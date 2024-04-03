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

# Load data and merge
load(file=paste0(pathEst, "output/firm_markups_V.Rdata"))
load(file=paste0(pathEst, "output/firm_markups_ML.Rdata"))
markups <- merge(markups_ML, markups_V, by=c("id","year","ind"))
rm(markups_ML, markups_V)
load(file=paste0(pathEst, "output/network_metrics.Rdata"))

# Merge
network_metrics <- merge(network_metrics, markups[, .(id_seller=id, year, match = mu_v_dlw_cd)])
network_metrics[, match := ifelse(is.na(match),0,1)]
rm(markups)



#### CREATE GRID OF PLOTS WITH 2 COLUMNS (LEFT IS ALL NETWORK SAMPLE, RIGHT IS MATCHED OBS ONLY); EACH ROW IS ONE NETWORK METRIC





# CTV
figure <- ggplot(network_metrics, aes(x = log(ctv_i))) +
    geom_histogram() +
    # stat_function(fun = dlnorm, args = list(mean = log(mean(network_metrics$ctv_i)), sd = sd(network_metrics$ctv_i))) +
    facet_wrap(~year, nrow = 2, ncol = 2, scales = 'fixed') +
    ylab("Frequency") +
    xlab(expression("log("~italic(CTV[i])~")")) +
    scale_x_continuous(breaks = c(log(10^-5),log(10^-2),log(10^1)), labels = c("10^-5","10^-2","10^1")) +
    theme_bw()+
    theme(legend.position = c(0.75, 0.1), legend.title = element_blank(), axis.title = element_text(size = 20),
          axis.text.x = element_text(vjust = unit(0.1, "mm")), axis.title.x = element_text(vjust = unit(0.1, "mm")),
          legend.key.size = unit(12,"mm"), axis.text = element_text(size = 15, color = "black"),
          legend.text = element_text(size = 20), strip.background = element_blank(), strip.text = element_text(size = 20),
          panel.spacing = unit(1,"cm"), legend.background = element_blank(), legend.box.background = element_rect(colour = "black", fill = "white"),
          axis.title.y = element_text(angle = 0, vjust = 0.5), legend.text.align = 0,
          text = element_text(family = "serif"))
ggsave(figure, filename = paste0(pathFig,sysdate,'_CTV_distribution.pdf'), width = 10, height = 10)

# WSI
figure <- ggplot(network_metrics, aes(x = wsi_i)) +
    geom_histogram() +
    # stat_function(fun = dlnorm, args = list(mean = log(mean(network_metrics$ctv_i)), sd = sd(network_metrics$ctv_i))) +
    facet_wrap(~year, nrow = 2, ncol = 2, scales = 'fixed') +
    ylab("Frequency") +
    xlab(expression(italic(WSI[i]))) +
    # scale_x_continuous(breaks = c(log(10^-5),log(10^-2),log(10^1)), labels = c("10^-5","10^-2","10^1")) +
    theme_bw()+
    theme(legend.position = c(0.75, 0.1), legend.title = element_blank(), axis.title = element_text(size = 20),
          axis.text.x = element_text(vjust = unit(0.1, "mm")), axis.title.x = element_text(vjust = unit(0.1, "mm")),
          legend.key.size = unit(12,"mm"), axis.text = element_text(size = 15, color = "black"),
          legend.text = element_text(size = 20), strip.background = element_blank(), strip.text = element_text(size = 20),
          panel.spacing = unit(1,"cm"), legend.background = element_blank(), legend.box.background = element_rect(colour = "black", fill = "white"),
          axis.title.y = element_text(angle = 0, vjust = 0.5), legend.text.align = 0,
          text = element_text(family = "serif"))
ggsave(figure, filename = paste0(pathFig,sysdate,'_WSI_distribution.pdf'), width = 10, height = 10)


# TI
figure <- ggplot(network_metrics, aes(x = log(ti_i))) +
    geom_histogram() +
    # stat_function(fun = dlnorm, args = list(mean = log(mean(network_metrics$ctv_i)), sd = sd(network_metrics$ctv_i))) +
    facet_wrap(~year, nrow = 2, ncol = 2, scales = 'fixed') +
    ylab("Frequency") +
    xlab(expression("log("~italic(TI[i])~")")) +
    # scale_x_continuous(breaks = c(log(10^-8),log(10^-6),log(10^-4),log(10^-2),log(10^0)), labels = c("10^-8","10^-6","10^-4","10^-2","10^0")) +
    theme_bw()+
    theme(legend.position = c(0.75, 0.1), legend.title = element_blank(), axis.title = element_text(size = 20),
          axis.text.x = element_text(vjust = unit(0.1, "mm")), axis.title.x = element_text(vjust = unit(0.1, "mm")),
          legend.key.size = unit(12,"mm"), axis.text = element_text(size = 15, color = "black"),
          legend.text = element_text(size = 20), strip.background = element_blank(), strip.text = element_text(size = 20),
          panel.spacing = unit(1,"cm"), legend.background = element_blank(), legend.box.background = element_rect(colour = "black", fill = "white"),
          axis.title.y = element_text(angle = 0, vjust = 0.5), legend.text.align = 0,
          text = element_text(family = "serif"))
ggsave(figure, filename = paste0(pathFig,sysdate,'_TI_distribution.pdf'), width = 10, height = 10)


# SR
figure <- ggplot(network_metrics[sr_i!=0], aes(x = log(sr_i))) +
    geom_histogram() +
    # stat_function(fun = dlnorm, args = list(mean = log(mean(network_metrics$ctv_i)), sd = sd(network_metrics$ctv_i))) +
    facet_wrap(~year, nrow = 2, ncol = 2, scales = 'fixed') +
    ylab("Frequency") +
    xlab(expression(italic(SR[i]))) +
    scale_x_continuous(breaks = c(log(10^-8),log(10^-6),log(10^-4),log(10^-2),log(10^0)), labels = c("10^-8","10^-6","10^-4","10^-2","10^0")) +
    theme_bw()+
    theme(legend.position = c(0.75, 0.1), legend.title = element_blank(), axis.title = element_text(size = 20),
          axis.text.x = element_text(vjust = unit(0.1, "mm")), axis.title.x = element_text(vjust = unit(0.1, "mm")),
          legend.key.size = unit(12,"mm"), axis.text = element_text(size = 15, color = "black"),
          legend.text = element_text(size = 20), strip.background = element_blank(), strip.text = element_text(size = 20),
          panel.spacing = unit(1,"cm"), legend.background = element_blank(), legend.box.background = element_rect(colour = "black", fill = "white"),
          axis.title.y = element_text(angle = 0, vjust = 0.5), legend.text.align = 0,
          text = element_text(family = "serif"))
ggsave(figure, filename = paste0(pathFig,sysdate,'_SR_distribution.pdf'), width = 10, height = 10)



# KC
figure <- ggplot(network_metrics, aes(x = log(KC_i))) +
    geom_histogram() +
    # stat_function(fun = dlnorm, args = list(mean = log(mean(network_metrics$ctv_i)), sd = sd(network_metrics$ctv_i))) +
    facet_wrap(~year, nrow = 2, ncol = 2, scales = 'fixed') +
    ylab("Frequency") +
    xlab(expression(italic(KC[i]))) +
    # scale_x_continuous(breaks = c(log(10^-8),log(10^-6),log(10^-4),log(10^-2),log(10^0)), labels = c("10^-8","10^-6","10^-4","10^-2","10^0")) +
    theme_bw()+
    theme(legend.position = c(0.75, 0.1), legend.title = element_blank(), axis.title = element_text(size = 20),
          axis.text.x = element_text(vjust = unit(0.1, "mm")), axis.title.x = element_text(vjust = unit(0.1, "mm")),
          legend.key.size = unit(12,"mm"), axis.text = element_text(size = 15, color = "black"),
          legend.text = element_text(size = 20), strip.background = element_blank(), strip.text = element_text(size = 20),
          panel.spacing = unit(1,"cm"), legend.background = element_blank(), legend.box.background = element_rect(colour = "black", fill = "white"),
          axis.title.y = element_text(angle = 0, vjust = 0.5), legend.text.align = 0,
          text = element_text(family = "serif"))
ggsave(figure, filename = paste0(pathFig,sysdate,'_KC_distribution.pdf'), width = 10, height = 10)



