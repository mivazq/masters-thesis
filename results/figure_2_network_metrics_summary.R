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

# Winsorize values for CTV and KC (top 0.1%)
threshold <- p999(network_metrics[variable=="ctv_i" & sample=="all"]$value)
network_metrics[variable=="ctv_i" & value>threshold, value := threshold]
threshold <- p999(network_metrics[variable=="kc_i" & sample=="all"]$value)
network_metrics[variable=="kc_i" & value>threshold, value := threshold]

# # Create factor variable
# network_metrics[, fact := interaction(variable,sample)]
# network_metrics[, fact := factor(fact, levels = c("wsi_i.all", "wsi_i.match", 
#                                                   "ti_i.all", "ti_i.match", 
#                                                   "ctv_i.all", "ctv_i.match", 
#                                                   "sr_i.all", "sr_i.match", 
#                                                   "KC_i.all", "KC_i.match"))]


figure = ggplot(network_metrics, aes(x = value)) + 
    geom_density(aes(color=sample)) + facet_wrap(~variable, ncol = 1, scales="free") + 
    scale_x_continuous(trans='log') + theme_bw() + theme(legend.position = "bottom")
ggsave(figure, filename = paste0(pathFig,sysdate,'_figure_2_distribution_network.pdf'), width = 10, height = 10)


network_metrics[, value  := (value  - mean(value))/mean(value),   by = c("variable", "year", "seller_sec")]



figure = ggplot(network_metrics, aes(x = value)) + 
    geom_density(aes(color=sample)) + facet_wrap(~variable, ncol = 1, scales="free") + 
    scale_x_continuous(trans='log') + theme_bw() + theme(legend.position = "bottom")
ggsave(figure, filename = paste0(pathFig,sysdate,'_figure_2_distribution_network_centered.pdf'), width = 10, height = 10)


# 
# 
# # CTV
# for (smpl in c("all","match")) {
#     
#     if (smpl=="all") {
#         data = network_metrics
#     } else {
#         data = network_metrics[match==1]
#     }
#     
#     fig <- ggplot(data, aes(x = log(ctv_i))) +
#         geom_density() +
#         ylab("Frequency") +
#         xlab(expression("log("*italic(CTV[i])*")")) +
#         scale_x_continuous(breaks = c(log(10^-5),log(10^-2),log(10^1)), labels = c(expression(10^-5),expression(10^-2),expression(10^1))) +
#         theme_bw()+
#         theme(legend.position = c(0.75, 0.1), legend.title = element_blank(), axis.title = element_text(size = 20),
#               axis.text.x = element_text(vjust = unit(0.1, "mm")), axis.title.x = element_text(vjust = unit(0.1, "mm")),
#               legend.key.size = unit(12,"mm"), axis.text = element_text(size = 15, color = "black"),
#               legend.text = element_text(size = 20), strip.background = element_blank(), strip.text = element_text(size = 20),
#               panel.spacing = unit(1,"cm"), legend.background = element_blank(), legend.box.background = element_rect(colour = "black", fill = "white"),
#               axis.title.y = element_text(angle = 0, vjust = 0.5), legend.text.align = 0,
#               text = element_text(family = "Palatino"))
#     
#     assign(paste0("figure_ctv_",smpl), fig)
# }
# 
# figure <- ggarrange(figure_ctv_all, figure_ctv_match, ncol=2, nrow=1, align="h")
# ggsave(figure, filename = paste0(pathFig,sysdate,'_CTV_distribution.pdf'), width = 10, height = 10)
# 
# 
# 
# 
# # WSI
# figure <- ggplot(network_metrics, aes(x = wsi_i)) +
#     geom_histogram() +
#     # stat_function(fun = dlnorm, args = list(mean = log(mean(network_metrics$ctv_i)), sd = sd(network_metrics$ctv_i))) +
#     facet_wrap(~year, nrow = 2, ncol = 2, scales = 'fixed') +
#     ylab("Frequency") +
#     xlab(expression(italic(WSI[i]))) +
#     # scale_x_continuous(breaks = c(log(10^-5),log(10^-2),log(10^1)), labels = c("10^-5","10^-2","10^1")) +
#     theme_bw()+
#     theme(legend.position = c(0.75, 0.1), legend.title = element_blank(), axis.title = element_text(size = 20),
#           axis.text.x = element_text(vjust = unit(0.1, "mm")), axis.title.x = element_text(vjust = unit(0.1, "mm")),
#           legend.key.size = unit(12,"mm"), axis.text = element_text(size = 15, color = "black"),
#           legend.text = element_text(size = 20), strip.background = element_blank(), strip.text = element_text(size = 20),
#           panel.spacing = unit(1,"cm"), legend.background = element_blank(), legend.box.background = element_rect(colour = "black", fill = "white"),
#           axis.title.y = element_text(angle = 0, vjust = 0.5), legend.text.align = 0,
#           text = element_text(family = "serif"))
# ggsave(figure, filename = paste0(pathFig,sysdate,'_WSI_distribution.pdf'), width = 10, height = 10)
# 
# 
# # TI
# figure <- ggplot(network_metrics, aes(x = log(ti_i))) +
#     geom_histogram() +
#     # stat_function(fun = dlnorm, args = list(mean = log(mean(network_metrics$ctv_i)), sd = sd(network_metrics$ctv_i))) +
#     facet_wrap(~year, nrow = 2, ncol = 2, scales = 'fixed') +
#     ylab("Frequency") +
#     xlab(expression("log("~italic(TI[i])~")")) +
#     # scale_x_continuous(breaks = c(log(10^-8),log(10^-6),log(10^-4),log(10^-2),log(10^0)), labels = c("10^-8","10^-6","10^-4","10^-2","10^0")) +
#     theme_bw()+
#     theme(legend.position = c(0.75, 0.1), legend.title = element_blank(), axis.title = element_text(size = 20),
#           axis.text.x = element_text(vjust = unit(0.1, "mm")), axis.title.x = element_text(vjust = unit(0.1, "mm")),
#           legend.key.size = unit(12,"mm"), axis.text = element_text(size = 15, color = "black"),
#           legend.text = element_text(size = 20), strip.background = element_blank(), strip.text = element_text(size = 20),
#           panel.spacing = unit(1,"cm"), legend.background = element_blank(), legend.box.background = element_rect(colour = "black", fill = "white"),
#           axis.title.y = element_text(angle = 0, vjust = 0.5), legend.text.align = 0,
#           text = element_text(family = "serif"))
# ggsave(figure, filename = paste0(pathFig,sysdate,'_TI_distribution.pdf'), width = 10, height = 10)
# 
# 
# # SR
# figure <- ggplot(network_metrics[sr_i!=0], aes(x = log(sr_i))) +
#     geom_histogram() +
#     # stat_function(fun = dlnorm, args = list(mean = log(mean(network_metrics$ctv_i)), sd = sd(network_metrics$ctv_i))) +
#     facet_wrap(~year, nrow = 2, ncol = 2, scales = 'fixed') +
#     ylab("Frequency") +
#     xlab(expression(italic(SR[i]))) +
#     scale_x_continuous(breaks = c(log(10^-8),log(10^-6),log(10^-4),log(10^-2),log(10^0)), labels = c("10^-8","10^-6","10^-4","10^-2","10^0")) +
#     theme_bw()+
#     theme(legend.position = c(0.75, 0.1), legend.title = element_blank(), axis.title = element_text(size = 20),
#           axis.text.x = element_text(vjust = unit(0.1, "mm")), axis.title.x = element_text(vjust = unit(0.1, "mm")),
#           legend.key.size = unit(12,"mm"), axis.text = element_text(size = 15, color = "black"),
#           legend.text = element_text(size = 20), strip.background = element_blank(), strip.text = element_text(size = 20),
#           panel.spacing = unit(1,"cm"), legend.background = element_blank(), legend.box.background = element_rect(colour = "black", fill = "white"),
#           axis.title.y = element_text(angle = 0, vjust = 0.5), legend.text.align = 0,
#           text = element_text(family = "serif"))
# ggsave(figure, filename = paste0(pathFig,sysdate,'_SR_distribution.pdf'), width = 10, height = 10)
# 
# 
# 
# # KC
# figure <- ggplot(network_metrics, aes(x = log(KC_i))) +
#     geom_histogram() +
#     # stat_function(fun = dlnorm, args = list(mean = log(mean(network_metrics$ctv_i)), sd = sd(network_metrics$ctv_i))) +
#     facet_wrap(~year, nrow = 2, ncol = 2, scales = 'fixed') +
#     ylab("Frequency") +
#     xlab(expression(italic(KC[i]))) +
#     # scale_x_continuous(breaks = c(log(10^-8),log(10^-6),log(10^-4),log(10^-2),log(10^0)), labels = c("10^-8","10^-6","10^-4","10^-2","10^0")) +
#     theme_bw()+
#     theme(legend.position = c(0.75, 0.1), legend.title = element_blank(), axis.title = element_text(size = 20),
#           axis.text.x = element_text(vjust = unit(0.1, "mm")), axis.title.x = element_text(vjust = unit(0.1, "mm")),
#           legend.key.size = unit(12,"mm"), axis.text = element_text(size = 15, color = "black"),
#           legend.text = element_text(size = 20), strip.background = element_blank(), strip.text = element_text(size = 20),
#           panel.spacing = unit(1,"cm"), legend.background = element_blank(), legend.box.background = element_rect(colour = "black", fill = "white"),
#           axis.title.y = element_text(angle = 0, vjust = 0.5), legend.text.align = 0,
#           text = element_text(family = "serif"))
# ggsave(figure, filename = paste0(pathFig,sysdate,'_KC_distribution.pdf'), width = 10, height = 10)
# 
# 
# 
