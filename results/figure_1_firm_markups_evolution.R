#///////////////////////////////////////////////////////////////////////////////
# File name:		figure_1_firm_markups_evolution.R
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
rm(markups_ML, markups_V)

# Generate variable for industry group
markups[, ind_group := ifelse(substr(ind,1,1) %in% c("A","B","C","D","E","F"), "AF", ifelse(substr(ind,1,1)=="G", "G", "HQ"))]

# Reshape dataset
markups <- melt(markups, measure.vars = measure(value.name, input, est, pf, sep = "_"))
markups[, weight := ifelse(input=="m", M, ifelse(input=="l", L, V))]
markups[, c("M","L","V") := NULL]

# Drop OLS and L markups, not interested
markups <- markups[est=="dlw" & input!="l"]
markups[, est := NULL]

# Fix markups of industries H55 AND O91
medgroup = median(markups[ind_group=="HQ" & ind %nin% c("H55","O91") & input=="v" & pf=="cd"]$mu) # median for industry group H55-Q99 (excluding these two industries)
fix_H55 = medgroup - median(markups[ind=="H55" & input=="v" & pf=="cd"]$mu) # median deviation from group median
fix_O91 = medgroup - median(markups[ind=="O91" & input=="v" & pf=="cd"]$mu) # median deviation from group median
markups[ind=="H55" & input=="v" & pf=="cd", mu := mu + fix_H55] # fix H55
markups[ind=="O91" & input=="v" & pf=="cd", mu := mu + fix_O91] # fix O91

# Compute change in markups
setorder(markups, "input", "pf", "id", "year")
markups[, mu_lag     := shift(mu,     type="lag"), by = c("id", "input", "pf")]
markups[, weight_lag := shift(weight, type="lag"), by = c("id", "input", "pf")]
markups[, delta_mu   := mu - mu_lag]
markups[, delta_w    := weight - weight_lag]

# Compute change in within-industry deciles
markups[, dcl_mu := .bincode(mu, breaks = quantile(mu,seq(0,1,by = 0.1), na.rm=T),
                             include.lowest = T), by = c("input", "pf", "year", "ind")]
setorder(markups, "input", "pf", "id", "year")
markups[, dcl_mu_lag := shift(dcl_mu, type="lag"), by = c("id", "input", "pf")]
markups[, delta_dcl_mu := dcl_mu - dcl_mu_lag]
markups[, prev_year := year-1]

# Create compact dataset for plotting
compact <- dcast(markups[input=="v" & pf=="tl" & !is.na(dcl_mu) & !is.na(dcl_mu_lag)],
                 prev_year + year + dcl_mu_lag + dcl_mu ~.,
                 fun = length,
                 value.var="dcl_mu")
setnames(compact, ".", "size")

# Compute share of all year for linewidth
compact[, sum_size := sum(size), by="year"]
compact[, size := size/sum_size]

# Aggregate all years by taking means since there is very little variation across years
compact <- dcast(data=compact,
                 formula=dcl_mu_lag + dcl_mu ~.,
                 fun = mean,
                 value.var="size")
setnames(compact, ".", "size")

# Plot with Line Widths Based on Count (ALL INDUSTRY GROUPS) # , labels=c("1st","2nd","3rd","4th","5th","6th","7th","8th","9th","10th")
ggplot(compact, aes(x = as.factor(dcl_mu_lag), y = as.factor(0), 
                    xend = as.factor(dcl_mu), yend = as.factor(1), 
                    linewidth = size)) +
    geom_segment(aes(color = ifelse(dcl_mu > dcl_mu_lag, "up", 
                                    ifelse(dcl_mu < dcl_mu_lag, "down", "same"))), 
                 alpha = 1, show.legend = c(linewidth = FALSE, colour = TRUE)) +
    scale_color_manual(values = c("up" = "green", "down" = "red", "same" = "gray"), labels=c("Downwards", "No movement", "Upwards")) +
    scale_x_discrete(breaks=1:10) + scale_y_discrete(breaks=c(0,1), labels=c(expression(italic("t-1")), expression(italic("t"))), expand = c(0.05,0.05)) +
    scale_linewidth_continuous(range = c(1,10)) +
    labs(x = "Decile within industry", y = "") +
    theme_bw() + 
    theme(legend.position = "bottom", text = element_text(family = "Palatino"), 
          strip.background=element_blank(), strip.text=element_text(size=20), legend.title = element_blank(),
          legend.text = element_text(size = 25, margin = margin(r = 20)), legend.key.size = unit(20, "mm"),
          axis.text = element_text(size = 25, color = "black"), axis.title.y = element_blank(),
          axis.title = element_text(size = 25, color = "black"), axis.text.y = element_text(hjust=0.5))
ggsave(paste0(pathFig,sysdate,"_firmlevel_markup_movement.pdf"), width = 15, height = 10, device=cairo_pdf)

# Compute some statistics for the text
compact_stats <- copy(compact)
compact_stats[, dw_more_deciles := dcl_mu <  dcl_mu_lag-1]
compact_stats[, dw_one_decile   := dcl_mu == dcl_mu_lag-1]
compact_stats[, same            := dcl_mu == dcl_mu_lag]
compact_stats[, up_one_decile   := dcl_mu == dcl_mu_lag+1]
compact_stats[, up_more_deciles := dcl_mu >  dcl_mu_lag+1]
unique(rowSums(compact_stats[,.SD,.SDcols = c("dw_more_deciles", "dw_one_decile", "same", "up_one_decile", "up_more_deciles")]))==1 # SHOULD BE TRUE (ONLY ONE CATEGORY ALLOWED)

# Multiply size by dummy variables to get the number in the correct row
compact_stats[, c("dw_more_deciles", "dw_one_decile", "same", "up_one_decile", "up_more_deciles") := 
                  lapply(.SD, function (x) x*size), .SDcols = c("dw_more_deciles", "dw_one_decile", "same", "up_one_decile", "up_more_deciles")]

# Share averaged over the years
compact_stats <- dcast(data=compact_stats,
                       formula=.~ .,
                       fun = sum,
                       value.var=c("dw_more_deciles", "dw_one_decile", "same", "up_one_decile", "up_more_deciles"))
