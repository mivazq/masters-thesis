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


# Compute within-industry rank
markups[, rank_mu := rank(mu, na.last="keep", ties.method="min"), by = c("input", "pf", "year", "ind")]
markups[, rank_mu_max := max(rank_mu, na.rm=T) , by = c("input", "pf", "year", "ind")]
markups[, rank_mu_pctl := rank_mu/rank_mu_max]
setorder(markups, "input", "pf", "id", "year")
markups[, rank_mu_pctl_lag := shift(rank_mu_pctl, type="lag"), by = c("id", "input", "pf")]
markups[, delta_rank_mu_pctl := rank_mu_pctl - rank_mu_pctl_lag]


# Compute within-industry rank (in vigintile terms)
markups[, dcl_mu := .bincode(mu,
                             breaks         = quantile(mu,seq(0,1,by = 0.1), na.rm=T),
                             include.lowest = T), by = c("input", "pf", "year", "ind")]
setorder(markups, "input", "pf", "id", "year")
markups[, dcl_mu_lag := shift(dcl_mu, type="lag"), by = c("id", "input", "pf")]
markups[, delta_dcl_mu := dcl_mu - dcl_mu_lag]


# markups[, dir := shift(delta_mu, type="lead"), by = c("id", "input", "pf")]
# markups[, dir := shift(delta_rank_mu_pctl, type="lead"), by = c("id", "input", "pf")]
markups[, dir := shift(delta_dcl_mu, type="lead"), by = c("id", "input", "pf")]
markups[, dir := ifelse(dir>0, "up", 
                        ifelse(dir<0, "down",
                               ifelse(dir==0, "same", NA)))]



markups[, prev_year := year-1]

compact <- dcast(markups[input=="v" & pf=="cd" & !is.na(dcl_mu) & !is.na(dcl_mu_lag)],
                 prev_year + year + dcl_mu_lag + dcl_mu ~.,
                 fun = length,
                 value.var="dcl_mu")
setnames(compact, ".", "size")

# Plot with Line Widths Based on Count
ggplot(compact, aes(x = prev_year, y = as.factor(dcl_mu_lag), 
                    xend = year, yend = as.factor(dcl_mu), 
                    linewidth = size)) +
    geom_segment(aes(color = ifelse(dcl_mu > dcl_mu_lag, "up", 
                                    ifelse(dcl_mu < dcl_mu_lag, "down", "same"))), 
                 alpha = 0.3, show.legend = F) +
    scale_color_manual(values = c("up" = "green", "down" = "red", "same" = "gray")) +
    scale_y_discrete(breaks=1:10) +
    theme_bw() +
    labs(x = "Year", y = "Decile within industry", color = "Direction", size = "Count") +
    theme(legend.position = "top")



# Plot grid (\u2192 is an right arrow)
lab <- as_labeller(c(`2009` = "2009 \u2192 2010", `2010` = "2010 \u2192 2011", `2011` = "2011 \u2192 2012", "AF" = "A01-F45", "G" = "G50-G52", "HQ" = "H55-Q99"))

xlab <- expression(italic("\u0394\u03BC=\u03BC"["it"]*"-\u03BC"["it-1"]))


ggplot(markups[input=="v" & pf=="cd" & delta_mu >= -0.5 & delta_mu<=0.5 & !is.na(delta_mu)], aes(x=delta_mu, y=mu_lag)) +
    geom_point(aes(colour=ind_group), alpha=0.2) + facet_wrap(~year, labeller=lab) +
    geom_smooth(aes(colour=ind_group), method="lm", formula=y~poly(x,2)) + 
    scale_y_continuous(limits=c(NA,3)) +
    xlab(xlab) + ylab(expression(italic("\u03BC"["it-1"])))





# ggplot(markups[input=="v" & pf=="cd"], aes(x=year, y=mu, group=id)) +
#     geom_line(aes(colour=dir), alpha=0.025, show.legend = F) +
#     geom_point(alpha=0.025) + scale_y_continuous(limits=c(NA,5)) +
#     geom_hline(yintercept = 1) +
#     scale_color_manual(values = c("up" = "green", "down" = "red")) +
#     theme_bw()



# ggplot(markups[input=="v" & pf=="cd"], aes(x=year, y=dcl_mu, group=id)) +
#     geom_line(aes(colour=dir), alpha=0.01, show.legend = F) + 
#     scale_color_manual(values = c("up" = "green", "down" = "red")) +
#     theme_bw()

ggplot(compact, aes(x=year, y=dcl_mu, group=dcl_mu)) +
    geom_line(aes(colour=dir, linewidth=size), show.legend = F) + 
    scale_color_manual(values = c("up" = "green", "down" = "red")) +
    theme_bw()




# Labeller
ggplot(markups[input=="v" & pf=="cd" & delta_mu >= -0.5 & delta_mu<=0.5 & !is.na(delta_mu)], aes(x=delta_mu)) +
    geom_histogram(binwidth = 0.025, boundary = 0, col = "black", fill = "red") + facet_grid(ind_group ~ year, labeller = lab) +
    xlab("Change in markup (bounded at [-0.5,0.5])") + geom_vline(xintercept = 0)
ggsave(paste0(pathFig,sysdate,'_figure_firmlevel_markup_changes.pdf'), width = 10, height = 10)

# Plot grid 
figure <- ggplot(markups[input=="v" & pf=="cd" & !is.na(delta_rank_mu_pctl)], aes(x=delta_rank_mu_pctl*100)) +
    geom_histogram(binwidth = 5, boundary = 0, col = "black", fill = "red") + facet_grid(ind_group ~ year, labeller = lab) +
    xlab("Percentile rank change") + geom_vline(xintercept = 0)
ggsave(figure, filename = paste0(pathFig,sysdate,'_figure_firmlevel_markup_percentile_changes.pdf'), width = 10, height = 10)






# Compute mean percentile for each percentile of previous year
markups[!is.na(dcl_mu_lag), mean_dcl_mu := mean(dcl_mu,na.rm=T), by = c("input", "pf", "year", "ind", "dcl_mu_lag")]

test <- unique(markups[!is.na(dcl_mu_lag), .(input, pf, year, ind, ind_group, dcl_mu_lag, mean_dcl_mu)])
setorder(test, "input", "pf", "ind", "ind_group", "year", "dcl_mu_lag")
test[, mean_dcl_mu_ind_group := mean(mean_dcl_mu, na.rm=T), by = c("input", "pf", "year", "ind_group", "dcl_mu_lag")]
test2 <- unique(test[, .(input, pf, year, ind_group, dcl_mu_lag, mean_dcl_mu_ind_group)])


ggplot(test2[input=="v" & pf=="cd"], aes(x=dcl_mu_lag, y= mean_dcl_mu_ind_group, colour=as.factor(year))) +
    geom_point() + geom_line() + facet_wrap(~ind_group) + geom_segment(x=0,xend=11,y=0,yend=11, color="black") +
    xlab(expression(italic("t-1"))) + ylab(expression(italic("t"))) +
    scale_x_continuous(breaks = 1:10, limits = c(1,10)) + scale_y_continuous(breaks = 1:10, limits = c(1,10))


ggplot(test[input=="v" & pf=="cd"], aes(x=dcl_mu_lag, y= mean_dcl_mu, linewidth=as.factor(ind), colour=as.factor(year))) +
    geom_line(alpha=0.2) + geom_segment(x=0,xend=11,y=0,yend=11, color="black") +
    scale_linewidth_manual(values=rep(0.5,42)) +
    xlab(expression(italic("t-1"))) + ylab(expression(italic("t"))) +
    scale_x_continuous(breaks = 1:10, limits = c(1,10)) + scale_y_continuous(breaks = 1:10, limits = c(1,10))




