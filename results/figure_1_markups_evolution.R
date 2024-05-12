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
markups <- merge(markups_ML, markups_V, by=c("id","year","ind"))
rm(markups_ML, markups_V)

# Reshape dataset
markups <- melt(markups, measure.vars = measure(value.name, input, est, pf, sep = "_"))
markups[, weight := ifelse(input=="m", M, ifelse(input=="l", L, V))]
markups[, c("M","L","V") := NULL]

# Generate variable for industry group
markups[, ind_group := ifelse(substr(ind,1,1) %in% c("A","B","C","D","E","F"), "AF", ifelse(substr(ind,1,1)=="G", "G", "HQ"))]

# Drop OLS markups, not interested
markups <- markups[est=="dlw"]
markups[, est := NULL]

# Drop missing markups
markups <- markups[!is.na(mu)]

# Set up table for aggregate markups
agg_mu <- data.table(input     = c("m", "l", "v"),
                     ind_group = c(rep("AF",3), rep("G", 3), rep("HQ", 3), rep("Z_ALL", 3)),
                     pf        = c(rep("cd",12), rep("tl", 12)),
                     year      = c(rep(2008,24), rep(2009,24), rep(2010,24), rep(2011,24)))

# Replicate for different variables
agg_mu <- rbind(copy(agg_mu)[, variable := "agg"], 
                copy(agg_mu)[, variable := "median"],
                copy(agg_mu)[, variable := "sd"])

# Calculate weighted averages (using input shares as weight)
for (i_year in 2008:2011) {
    for (i_pf in c("cd","tl")) {
        for (i_input in c("v","m","l")) {
            for (i_ind_group in c("AF","G","HQ")) {
                agg_mu[input==i_input & ind_group==i_ind_group & pf==i_pf & year==i_year & variable=="agg", 
                       value := weighted.mean(x = markups[input==i_input & ind_group==i_ind_group & pf==i_pf & year==i_year]$mu, 
                                              w = markups[input==i_input & ind_group==i_ind_group & pf==i_pf & year==i_year]$weight, na.rm = T)]
                agg_mu[input==i_input & ind_group==i_ind_group & pf==i_pf & year==i_year & variable=="median", 
                       value := median(x = markups[input==i_input & ind_group==i_ind_group & pf==i_pf & year==i_year]$mu, na.rm = T)]
                agg_mu[input==i_input & ind_group==i_ind_group & pf==i_pf & year==i_year & variable=="sd", 
                       value := sd(x = markups[input==i_input & ind_group==i_ind_group & pf==i_pf & year==i_year]$mu, na.rm = T)]
            }
            agg_mu[input==i_input & ind_group=="Z_ALL" & pf==i_pf & year==i_year & variable=="agg", 
                   value := weighted.mean(x = markups[input==i_input & pf==i_pf & year==i_year]$mu, 
                                          w = markups[input==i_input & pf==i_pf & year==i_year]$weight, na.rm = T)]
            agg_mu[input==i_input & ind_group=="Z_ALL" & pf==i_pf & year==i_year & variable=="median", 
                   value := median(x = markups[input==i_input & pf==i_pf & year==i_year]$mu, na.rm = T)]
            agg_mu[input==i_input & ind_group=="Z_ALL" & pf==i_pf & year==i_year & variable=="sd", 
                   value := sd(x = markups[input==i_input & pf==i_pf & year==i_year]$mu, na.rm = T)]
        }
    }
}
rm(i_input, i_ind_group, i_pf, i_year)

# Remove labour since too wrong
agg_mu <- agg_mu[input!="l"]
agg_mu[, pf := ifelse(pf=="cd", "Cobb-Douglas Production", "Translog Production")] # make nice

# Define legend labels (\u2013 is unicode en-dash)
# lgndlab1 <- "A01\u2013F45"
# lgndlab2 <- "G50\u2013G52"
# lgndlab3 <- "H55\u2013Q99"
lgndlab1 <- "Produce"
lgndlab2 <- "Commerce"
lgndlab3 <- "Service"
lgndlab4 <- "All industries"

# Plot evolution by production function and input (for appendix)
for (i_input in c("v", "m")) {
    
    ylab = ifelse(i_input=="v", 
                  expression(italic("\u03BC"["t"]^"V")),
                  expression(italic("\u03BC"["t"]^"M")))
    
    ggplot(agg_mu[input==i_input & variable %nin% c("sd")], aes(x = year, y = value, color = ind_group, linetype=variable)) +
        geom_line(linewidth=1.5) + geom_point(size=3) + geom_hline(yintercept=1) + 
        scale_linetype_manual(values= c(1,2), labels=c("Aggregate","Median")) +
        scale_color_discrete(labels = c(lgndlab1, lgndlab2, lgndlab3, lgndlab4), type = c("#f8766d","#00ba38","#619cff","#a3a500")) +
        scale_y_continuous(labels = function(x) fp(x, dig=2)) + facet_wrap(~pf) +
        xlab("Year") + ylab(ylab) +
        theme_bw() + guides(linetype="none") + 
        theme(legend.position = "bottom", text = element_text(family = "Palatino"), 
              legend.title = element_blank(), 
              strip.background=element_blank(), strip.text=element_text(size=25, color = "black"),
              legend.text = element_text(size = 25, margin = margin(r = 20)), legend.key.size = unit(20, "mm"), 
              axis.text = element_text(size = 25, color = "black"), 
              axis.title.y = element_text(angle = 0, vjust = 0.5, size = 25, color = "black"), 
              axis.title.x = element_blank())
    ggsave(paste0(pathFig,sysdate,"_aggregate_markup_",toupper(i_input),"_appendix.pdf"), width = 15, height = 10, device=cairo_pdf)
}

# Fix markups of industries H55 AND O91 (in new variable)
medgroup = median(markups[ind_group=="HQ" & ind %nin% c("H55","O91") & input=="v" & pf=="cd"]$mu) # median for industry group H55-Q99 (excluding these two industries)
fix_H55 = medgroup - median(markups[ind=="H55" & input=="v" & pf=="cd"]$mu) # median deviation from group median
fix_O91 = medgroup - median(markups[ind=="O91" & input=="v" & pf=="cd"]$mu) # median deviation from group median
markups[ind=="H55" & input=="v" & pf=="cd", mu := mu + fix_H55] # fix H55
markups[ind=="O91" & input=="v" & pf=="cd", mu := mu + fix_O91] # fix O91

# Set up table for aggregate markups
agg_mu <- data.table(input     = c("m", "l", "v"),
                     ind_group = c(rep("AF",3), rep("G", 3), rep("HQ", 3), rep("Z_ALL", 3)),
                     pf        = c(rep("cd",12), rep("tl", 12)),
                     year      = c(rep(2008,24), rep(2009,24), rep(2010,24), rep(2011,24)))

# Replicate for different variables
agg_mu <- rbind(copy(agg_mu)[, variable := "agg"], 
                copy(agg_mu)[, variable := "median"],
                copy(agg_mu)[, variable := "sd"])

# Calculate weighted averages (using input shares as weight)
for (i_year in 2008:2011) {
    for (i_pf in c("cd","tl")) {
        for (i_input in c("v","m","l")) {
            for (i_ind_group in c("AF","G","HQ")) {
                agg_mu[input==i_input & ind_group==i_ind_group & pf==i_pf & year==i_year & variable=="agg", 
                       value := weighted.mean(x = markups[input==i_input & ind_group==i_ind_group & pf==i_pf & year==i_year]$mu, 
                                              w = markups[input==i_input & ind_group==i_ind_group & pf==i_pf & year==i_year]$weight, na.rm = T)]
                agg_mu[input==i_input & ind_group==i_ind_group & pf==i_pf & year==i_year & variable=="median", 
                       value := median(x = markups[input==i_input & ind_group==i_ind_group & pf==i_pf & year==i_year]$mu, na.rm = T)]
                agg_mu[input==i_input & ind_group==i_ind_group & pf==i_pf & year==i_year & variable=="sd", 
                       value := sd(x = markups[input==i_input & ind_group==i_ind_group & pf==i_pf & year==i_year]$mu, na.rm = T)]
            }
            agg_mu[input==i_input & ind_group=="Z_ALL" & pf==i_pf & year==i_year & variable=="agg", 
                   value := weighted.mean(x = markups[input==i_input & pf==i_pf & year==i_year]$mu, 
                                          w = markups[input==i_input & pf==i_pf & year==i_year]$weight, na.rm = T)]
            agg_mu[input==i_input & ind_group=="Z_ALL" & pf==i_pf & year==i_year & variable=="median", 
                   value := median(x = markups[input==i_input & pf==i_pf & year==i_year]$mu, na.rm = T)]
            agg_mu[input==i_input & ind_group=="Z_ALL" & pf==i_pf & year==i_year & variable=="sd", 
                   value := sd(x = markups[input==i_input & pf==i_pf & year==i_year]$mu, na.rm = T)]
        }
    }
}
rm(i_input, i_ind_group, i_pf, i_year)

# Remove labour since too wrong
agg_mu <- agg_mu[input!="l"]
agg_mu[, pf := ifelse(pf=="cd", "Cobb-Douglas Production", "Translog Production")] # make nice

# Define legend labels (\u2013 is unicode en-dash)
# lgndlab1 <- "A01\u2013F45"
# lgndlab2 <- "G50\u2013G52"
# lgndlab3 <- "H55\u2013Q99"
lgndlab1 <- "Produce"
lgndlab2 <- "Commerce"
lgndlab3 <- "Service"
lgndlab4 <- "All industries"

#  Plot only CD for input V (for main text)
ggplot(agg_mu[pf=="Cobb-Douglas Production" & input=="v" & variable %nin% c("sd")], aes(x = year, y = value, color = ind_group, linetype=variable)) +
    geom_line(linewidth=1.5) + geom_point(size=3) + geom_hline(yintercept=1) + 
    scale_linetype_manual(values= c(1,2), labels=c("Aggregate","Median")) +
    scale_color_discrete(labels = c(lgndlab1, lgndlab2, lgndlab3, lgndlab4), type = c("#f8766d","#00ba38","#619cff","#a3a500")) +
    scale_y_continuous(labels = function(x) fp(x, dig=1)) +
    xlab("Year") + ylab(expression(italic("\u03BC"["t"]^"V"))) +
    theme_bw() + guides(linetype="none") +
    theme(legend.position = "bottom", text = element_text(family = "Palatino"), 
          strip.background=element_blank(), strip.text=element_text(size=20), legend.title = element_blank(),
          legend.text = element_text(size = 25, margin = margin(r = 20)), legend.key.size = unit(20, "mm"), 
          axis.text = element_text(size = 25, color = "black"), axis.title.y = element_text(angle = 0, vjust = 0.5),
          axis.title = element_text(size = 25, color = "black"), axis.title.x = element_blank())
ggsave(paste0(pathFig,sysdate,"_aggregate_markup_main.pdf"), width = 15, height = 10, device=cairo_pdf)


#///////////////////////////////////////////////////////////////////////////////
#### Decomposition ####
#///////////////////////////////////////////////////////////////////////////////
load(file=paste0(pathEst, "output/firm_markups_V.Rdata"))
load(file=paste0(pathEst, "output/firm_markups_ML.Rdata"))
df <- merge(markups_ML, markups_V, by=c("id","year","ind"))
rm(markups_ML, markups_V)
df <- melt(df, measure.vars = measure(value.name, input, est, pf, sep = "_"))
df <- df[pf=="cd" & input=="v" & est=="dlw", .(id, year, ind, alpha_v, mu, w = V)]

# Weights
df[, w_it := w/sum(w, na.rm=T), by=year]
df[, lag_w_it := shift(w_it), by=id]
df[, delta_w_it := w_it - lag_w_it]

# Markups
df[, mu_it := mu]
df[, lag_mu_it := shift(mu_it), by=id]
df[, bar_mu_t := weighted.mean(mu_it, w_it, na.rm=T), by=year]
df[, lag_bar_mu_t := shift(bar_mu_t), by=id]
df[, delta_mu_it := mu_it - lag_mu_it]
df[, hat_mu_it   := mu_it - lag_bar_mu_t]
df[, tilde_mu_it := mu_it - bar_mu_t]
df[, lag_tilde_mu_it := shift(tilde_mu_it), by=id]


# Effects
df[, within  := lag_w_it*delta_mu_it]
df[, between := delta_w_it*lag_tilde_mu_it]
df[, cross   := delta_w_it*delta_mu_it]
df[, entry   := w_it*hat_mu_it]
df[, exit    := lag_w_it*lag_tilde_mu_it]
df[, overall := bar_mu_t - lag_bar_mu_t]
df[, level   := bar_mu_t]
df[!is.na(within), c("entry","exit") := NA] # no entry/exit effect for firms staying

# Aggregate at yearly level
agg_df <- dcast(df,
                year~.,
                fun = list(sum,sum,sum,sum,sum,mean,mean),
                na.rm=T,
                value.var=list("within","between","cross","entry","exit","overall","level"))
setnames(agg_df,c("year","within","between","cross","entry","exit","overall","level"))

# Check correctness
agg_df[, sum_comp := within + between + cross + entry - exit] # exit has to be subtracted!
agg_df[year==2008, overall := 0]
all.equal(agg_df$sum_comp,agg_df$overall) # should print TRUE
agg_df[, sum_comp := NULL]
agg_df[, exit := -exit] # make negative so that "summing" bars make sense

# Plot
agg_df <- melt(agg_df,
               id.var="year")
ggplot(agg_df[variable %nin% c("overall","level")], aes(x = year, y = value)) +
    geom_bar(aes(fill = variable), width = 0.5, size = 0.25, color = "black",
             stat = "identity", position = position_stack()) +
    geom_line(data=agg_df[variable=="overall"], aes(x=year, y=value)) +
    scale_fill_manual(values = c("#AC92EB", "#4FC1E8", "#FFCE54", "#A0D568", "#ED5564"),
                      labels = c("Within","Between","Cross","Entry","Exit")) +
    scale_y_continuous(name = "Delta µ") +
    xlab("") + 
    theme_bw() +
    theme(legend.position = c(0.1,0.8), text = element_text(family = "Palatino"),
          legend.title = element_blank())
ggsave(paste0(pathFig,sysdate,"_aggregate_markup_main.pdf"), width = 15, height = 10, device=cairo_pdf)








### PLOT EVOLUTION FOR HQ INDUSTRES TO UNDERSTAND WHERE PEAK COMES FROM IN 2010
# 
# # Set up table for aggregate markups
# agg_mu <- data.table(ind   = sort(unique(markups[ind_group=="HQ"]$ind)),
#                      input = c(rep("m",15), rep("v",15)),
#                      pf    = c(rep("cd",30), rep("tl", 30)),
#                      year  = c(rep(2008,60), rep(2009,60), rep(2010,60), rep(2011,60)))
# 
# # Calculate weighted averages (using input shares as weight)
# for (i_year in 2008:2011) {
#     for (i_pf in c("cd","tl")) {
#         for (i_input in c("v","m")) {
#             for (i_ind in sort(unique(markups[ind_group=="HQ"]$ind))) {
#                 agg_mu[input==i_input & ind==i_ind & pf==i_pf & year==i_year, 
#                        value := weighted.mean(x = markups[input==i_input & ind==i_ind & pf==i_pf & year==i_year]$mu, 
#                                               w = markups[input==i_input & ind==i_ind & pf==i_pf & year==i_year]$weight, 
#                                               na.rm = T)]
#             }
#         }
#     }
# }
# rm(i_input, i_ind, i_pf, i_year)
# 
# # Remove labour since too wrong
# agg_mu[, pf := ifelse(pf=="cd", "Cobb-Douglas Production", "Translog Production")] # make nice
# 
# ggplot(agg_mu[pf=="Cobb-Douglas Production" & input=="v"], aes(x = year, y = value, color = ind)) +
#     geom_line(linewidth=1.5) + geom_hline(yintercept=1) + geom_point() +
#     scale_y_continuous(labels = function(x) fp(x, dig=2)) +
#     xlab("Year") + ylab(expression(italic("\u03BC"["t"]^"V"))) +
#     theme_bw() + 
#     theme(legend.position = "bottom", text = element_text(family = "Palatino"), 
#           strip.background=element_blank(), strip.text=element_text(size=20), legend.title = element_blank(),
#           legend.text = element_text(size = 25, margin = margin(r = 20)), legend.key.size = unit(20, "mm"), 
#           axis.text = element_text(size = 25, color = "black"), axis.title.y = element_text(angle = 0, vjust = 0.5),
#           axis.title = element_text(size = 25, color = "black"), axis.title.x = element_blank())
#

