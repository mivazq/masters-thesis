#///////////////////////////////////////////////////////////////////////////////
# File name:		figures.R
# Author:			Miguel Vázquez Vázquez
# Creation date:    03 January 2024
# Description:      In this file we produce all figures of the paper's appendix
#
# Input:
#                   $pathEst/input/firm_sample.Rdata
# Output:
#                   $pathFig/$sysdate_entry_year.png
#                   $pathFig/$sysdate_exit_year.png
#                   -
#///////////////////////////////////////////////////////////////////////////////
source('~/data/transactions_ecuador/3_mivazq/Masters_Thesis/setup.R')
#///////////////////////////////////////////////////////////////////////////////
#----                               LOAD DATA                               ----
#///////////////////////////////////////////////////////////////////////////////

# Firm sample with entry/exit year
load(file = paste0(pathEst, "input/firm_sample.Rdata"))

#///////////////////////////////////////////////////////////////////////////////
#### Figure 1: Entry and Exit of Entering/Exiting Firms         # Section ? ####
#///////////////////////////////////////////////////////////////////////////////

# Manipulate data for entry plot
df_plot <- dcast(data      = firm_sample[entry_year %in% seq(2008,2011,1)], 
                 formula   = entry_year ~ ., 
                 fun       = length)
setnames(df_plot, ".", "count")

# Plot entry and save
plot <- ggplot(df_plot, aes(x=entry_year,y=count)) + 
    geom_col() +
    scale_y_continuous(limits = c(0, 10000)) +
    xlab("Year") + ylab("Entries") + 
    ggtitle("Entry year of entering firms in sample") + 
    theme_bw() + theme(axis.title.y = element_text(angle=0, vjust=0.5))
ggsave(plot, filename = paste0(pathFig,sysdate,'_entry_year.png'),width = 12, height = 7)
rm(df_plot, plot)

# Manipulate data for exit plot
df_plot <- dcast(data      = firm_sample[exit_year %in% seq(2008,2011,1)], 
                 formula   = exit_year ~ ., 
                 fun       = length)
setnames(df_plot, ".", "count")

# Plot exit and save
plot <- ggplot(df_plot, aes(x=exit_year,y=count)) + 
    geom_col() +
    scale_y_continuous(limits = c(0, 10000)) +
    xlab("Year") + ylab("Exits  ") + 
    ggtitle("Exit year of exiting firms in sample") + 
    theme_bw() + theme(axis.title.y = element_text(angle=0, vjust=0.5))
ggsave(plot, filename = paste0(pathFig,sysdate,'_exit_year.png'),width = 12, height = 7)
rm(df_plot, plot)

#///////////////////////////////////////////////////////////////////////////////
#### Figure ?  # Section ? ####
#///////////////////////////////////////////////////////////////////////////////

