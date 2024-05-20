#///////////////////////////////////////////////////////////////////////////////
# File name:		figure_1_markups_industry.R
# Author:			Miguel Vázquez Vázquez
# Creation date:    11 April 2024
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

# Drop OLS markups
ols_cols = colnames(markups)[grep("_ols", colnames(markups))]
markups <- markups[, !ols_cols, with=FALSE]

# Drop missing markup rows
markups <- markups[!is.na(mu_v_dlw_tl)]

# Check correlations between markups
mu_cols = colnames(markups)[grep("mu_", colnames(markups))]
cor(markups[, ..mu_cols], method="pearson")  # check linear correlation
cor(markups[, ..mu_cols], method="spearman") # check rank correlation

# Generate variable for industry group
markups[, ind_group := ifelse(substr(ind,1,1) %in% c("A","B","C","D","E","F"), "AF", ifelse(substr(ind,1,1)=="G", "G", "HQ"))]

# Reshape dataset
markups <- melt(markups, measure.vars = measure(value.name, input, est, pf, sep = "_"))
markups[, est := NULL] # useless column since OLS were dropped

# Drop labour markups
markups <- markups[input!="l"]

# Define legend labels (\u2013 is unicode en-dash)
# lgndlab1 <- "A01\u2013F45"
# lgndlab2 <- "G50\u2013G52"
# lgndlab3 <- "H55\u2013Q99"
lgndlab1 <- "Produce"
lgndlab2 <- "Commerce"
lgndlab3 <- "Service"

# Calculate densities at median for plotting reasons
data_lines <- unique(markups[, .(ind_group, input, pf)])
data_lines[, c("x", "y", "xend", "yend") := 0]
for (i_pf in c("cd", "tl")) {
    for (i_input in c("v", "m")) {
        for (i_ind_group in c("AF", "G", "HQ")) {
            d <- density(markups[input==i_input & pf==i_pf & ind_group==i_ind_group]$mu, n = 2^11)
            apx <- approx(d$x, d$y, xout = c(median(markups[input==i_input & pf==i_pf & ind_group==i_ind_group]$mu)))
            data_lines[input==i_input & pf==i_pf & ind_group==i_ind_group, x := apx$x]
            data_lines[input==i_input & pf==i_pf & ind_group==i_ind_group, y := 0]
            data_lines[input==i_input & pf==i_pf & ind_group==i_ind_group, xend := apx$x]
            data_lines[input==i_input & pf==i_pf & ind_group==i_ind_group, yend := apx$y]
        }
    }
}
rm(d, apx)

# Manual fixes since some density values are not precise
data_lines[pf=="cd" & input=="m" & ind_group!="G", yend := yend+0.05]
data_lines[pf=="tl" & input=="m" & ind_group=="AF", yend := yend+0.06]
data_lines[pf=="tl" & input=="m" & ind_group=="HQ", yend := yend+0.10]

# Rename for nice plotting
markups[, pf := ifelse(pf=="cd", "Cobb-Douglas Production", "Translog Production")]
data_lines[, pf := ifelse(pf=="cd", "Cobb-Douglas Production", "Translog Production")]

# Plot densities by industry group for appendix
for (i_input in c("v", "m")) {
    
    xlab <- ifelse(i_input=="v", 
                   expression(italic("\u03BC"["it"]^"V")),
                   expression(italic("\u03BC"["it"]^"M")))
    
    ggplot(markups[input==i_input], aes(x = mu, fill = ind_group)) + 
        geom_segment(data=data_lines[input==i_input], aes(x=x, y=y, xend=xend, yend=yend, color=ind_group), alpha=0.5, linewidth=1.5, show.legend = F) +
        geom_density(alpha = 0.2, n = 2^11) + geom_vline(xintercept=1) +
        facet_wrap(~pf) +
        scale_fill_discrete(name="Industry Group",  labels = c(lgndlab1, lgndlab2, lgndlab3), type = c("#f8766d","#00ba38","#619cff")) + 
        scale_color_discrete(name="Industry Group", labels = c(lgndlab1, lgndlab2, lgndlab3), type = c("#f8766d","#00ba38","#619cff")) + 
        scale_x_continuous(limits = c(0,3)) +
        theme_bw() + ylab("Density") + xlab(xlab) +
        theme(legend.position = c(0.5,0.543), text = element_text(family = "Palatino"), 
              legend.text = element_text(size = 25, margin = margin(r = 20)), legend.key.size = unit(10, "mm"), 
              legend.title = element_blank(), 
              strip.background=element_blank(), strip.text=element_text(size=25, color = "black"),
              legend.box.background = element_rect(colour = "black"), axis.text = element_text(size = 25, color = "black"), 
              axis.title.y = element_text(angle = 0, vjust = 0.5), axis.title = element_text(size = 25, color = "black"))
    ggsave(paste0(pathFig,sysdate,"_figure_1_industry_markup_",toupper(i_input),"_appendix.pdf"), width = 15, height = 10, device=cairo_pdf)
}

# Plot densities by industry group for main
ggplot(markups[input=="v" & pf=="Translog Production"], aes(x = mu, fill = ind_group)) + 
    geom_segment(data=data_lines[input=="v" & pf=="Translog Production"], aes(x=x, y=y, xend=xend, yend=yend, color=ind_group), alpha=0.5, linewidth=1.5, show.legend = F) +
    geom_density(alpha = 0.2, n = 2^11) + geom_vline(xintercept=1) + 
    scale_fill_discrete(name="Industry Group",  labels = c(lgndlab1, lgndlab2, lgndlab3), type = c("#f8766d","#00ba38","#619cff")) + 
    scale_color_discrete(name="Industry Group", labels = c(lgndlab1, lgndlab2, lgndlab3), type = c("#f8766d","#00ba38","#619cff")) + 
    scale_x_continuous(limits = c(0,3)) +
    theme_bw() + ylab("Density") + xlab(expression(italic("\u03BC"["it"]))) +
    theme(legend.position = c(0.865,0.783), text = element_text(family = "Palatino"), 
          legend.text = element_text(size = 25, margin = margin(r = 20)), legend.key.size = unit(10, "mm"), 
          legend.title = element_blank(),
          legend.box.background = element_rect(colour = "black"), axis.text = element_text(size = 25, color = "black"), 
          axis.title.y = element_text(angle = 0, vjust = 0.5), axis.title = element_text(size = 25, color = "black"))
ggsave(paste0(pathFig,sysdate,"_figure_1_industry_markup_main.pdf"), width = 15, height = 10, device=cairo_pdf)

# Check median markup of manufacturing firms in 2010 using materials as variable input and translog production function to compare with Rodriguez
median(markups[input=="m" & pf=="Translog Production" & year==2010 & substr(ind,1,1)=="D"]$mu)
median(markups[input=="v" & pf=="Translog Production" & year==2010 & substr(ind,1,1)=="D"]$mu)
median(markups[input=="m" & pf=="Cobb-Douglas Production" & year==2010 & substr(ind,1,1)=="D"]$mu)

# Median markups by industry
for (industry in sort(unique(markups$ind))) {
    cat(industry, "median markup: ", median(markups[input=="v" & pf=="Translog Production" & ind==industry]$mu), "\n")
}
# Median markups by industry group
for (industry in sort(unique(markups$ind_group))) {
    cat(industry, "median markup: ", median(markups[input=="v" & pf=="Translog Production" & ind_group==industry]$mu), "\n")
}
