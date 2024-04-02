#///////////////////////////////////////////////////////////////////////////////
# File name:		figure_1_markups_evolution.R
# Author:			Miguel Vázquez Vázquez
# Creation date:    10 January 2023
# Description:      This file estimates markups for all firms, year to year. The
#                   estimation is performed on an unbalanced panel of firms.
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
markups <- merge(markups_ML, markups_V, by=c("id","year", "ind"))
rm(markups_ML, markups_V)

# Reshape dataset
markups <- melt(markups, measure.vars = measure(value.name, input, est, pf, sep = "_"))
markups[, weight := ifelse(input=="m", M, ifelse(input=="l", L, V))]
markups[, c("M","L","V") := NULL]

# Drop OLS markups, not interested
markups <- markups[est=="dlw"]
markups[, est := NULL]

# Set up table for aggregate markups
agg_mu <- data.table(input = c("m", "l", "v"),
                     pf    = c(rep("cd",3), rep("tl", 3)),
                     est   = c(rep("ols",6), rep("dlw", 6)),
                     year  = c(rep(2008,12), rep(2009,12), rep(2010,12), rep(2011,12)))

# markups <- markups[substr(ind,1,1)%in%c("A","B","D","F","G")]

# Calculate weighted averages (using input shares as weight)
for (i_input in c("m","l","v")) {
for (i_pf in c("cd","tl")) {
for (i_est in c("ols","dlw")) {
for (i_year in 2008:2011) {
    agg_mu[input==i_input & pf==i_pf & est==i_est & year==i_year, 
           value := weighted.mean(x = markups[input==i_input & pf==i_pf & est==i_est & year==i_year]$mu, 
                                  w = markups[input==i_input & pf==i_pf & est==i_est & year==i_year]$weight, 
                                  na.rm = T)]
}
} 
}
}
rm(i_input, i_pf, i_est, i_year)

# Plot evolution
for (i_input in c("m","l","v")) {
    
    ggplot(agg_mu[input==i_input], aes(x = year, y = value, color = est, linetype = pf)) +
        geom_line() +
        geom_point() +
        labs(x = "Year", y = "Aggregate μ", color = "Estimation Method", linetype = "Production Function") +
        scale_linetype_manual(values = c("solid", "dashed")) +
        theme_bw()
    ggsave(paste0(pathFig, sysdate, "_aggregate_markup_",toupper(i_input),".pdf"))
}









