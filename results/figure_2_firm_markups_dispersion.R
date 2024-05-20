#///////////////////////////////////////////////////////////////////////////////
# File name:		figure_2_markups_dispersion.R
# Author:			Miguel Vázquez Vázquez
# Creation date:    28 April 2024
# Description:      This file plots markup dispersion over time
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

# Drop labour markups, missing markups, OLS markups
markups <- markups[input!="l"]
markups <- markups[!is.na(mu)]
markups <- markups[est=="dlw"]
markups[, est := NULL]

# Generate variable for industry group
markups[, ind_group := ifelse(substr(ind,1,1) %in% c("A","B","C","D","E","F"), "AF", ifelse(substr(ind,1,1)=="G", "G", "HQ"))]

# Collapse at industry-year level, per specification
std_mu <- dcast(markups,
                formula = input + pf + year + ind_group ~ .,
                fun = sd,
                value.var = "mu")

# Add rows for all industries combined
for (i_year in 2008:2011) {
    for (i_pf in c("cd","tl")) {
        for (i_input in c("v","m")) {
            std_mu <- rbind(std_mu, list(i_input, i_pf, i_year, "Z_ALL", sd(markups[input==i_input & pf==i_pf & year==i_year]$mu)))
        }
    }
}
setnames(std_mu, ".", "value")

# Define legend labels (\u2013 is unicode en-dash)
lgndlab1 <- "Produce"
lgndlab2 <- "Commerce"
lgndlab3 <- "Service"
lgndlab4 <- "All industries"
std_mu[, pf := ifelse(pf=="cd", "Cobb-Douglas Production", "Translog Production")] # make nice

# Plot evolution by production function and input (for appendix)
for (i_input in c("v", "m")) {
    
    ylab = ifelse(i_input=="v", 
                  expression(italic("\u03C3"["\u03BC"["it"]^"V"])),
                  expression(italic("\u03C3"["\u03BC"["it"]^"M"])))
    
    ggplot(std_mu[input==i_input], aes(x = year, y = value, color = ind_group)) +
        geom_line(linewidth=1.5) + geom_point(size=3) +
        facet_wrap(~pf) +
        scale_color_discrete(labels = c(lgndlab1, lgndlab2, lgndlab3, lgndlab4), type = c("#f8766d","#00ba38","#619cff","#a3a500")) +
        scale_y_continuous(labels = function(x) fp(x, dig=2), limits = c(0,NA)) +
        xlab("Year") + ylab(ylab) +
        theme_bw() + 
        theme(legend.position = "bottom", text = element_text(family = "Palatino"), 
              legend.title = element_blank(), 
              strip.background=element_blank(), strip.text=element_text(size=25, color = "black"),
              legend.text = element_text(size = 25, margin = margin(r = 20)), legend.key.size = unit(20, "mm"), 
              axis.text = element_text(size = 25, color = "black"), 
              axis.title.y = element_text(angle = 0, vjust = 0.5, size = 25, color = "black"), 
              axis.title.x = element_blank())
    ggsave(paste0(pathFig,sysdate,"_figure_2_std_markup_",toupper(i_input),"_appendix.pdf"), width = 15, height = 10, device=cairo_pdf)
}

# Plot evolution by production function and input (for main)
ggplot(std_mu[input=="v" & pf=="Translog Production"], aes(x = year, y = value, color = ind_group)) +
    geom_line(linewidth=1.5) + geom_point(size=3) +
    scale_color_discrete(labels = c(lgndlab1, lgndlab2, lgndlab3, lgndlab4), type = c("#f8766d","#00ba38","#619cff","#a3a500")) +
    scale_y_continuous(labels = function(x) fp(x, dig=2), limits = c(0,NA)) +
    xlab("Year") + ylab(expression(italic("\u03C3"["\u03BC"["it"]]))) +
    theme_bw() + 
    theme(legend.position = "bottom", text = element_text(family = "Palatino"), legend.title = element_blank(), 
          legend.text = element_text(size = 25, margin = margin(r = 20)), legend.key.size = unit(20, "mm"), 
          axis.text = element_text(size = 25, color = "black"), axis.title.y = element_text(angle = 0, vjust = 0.5), 
          axis.title = element_text(size = 25, color = "black"), axis.title.x = element_blank())
ggsave(paste0(pathFig,sysdate,"_figure_2_std_markup_v_main.pdf"), width = 15, height = 10, device=cairo_pdf)

