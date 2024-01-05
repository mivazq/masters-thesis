#///////////////////////////////////////////////////////////////////////////////
# File name:		meeting_prep.R
# Author:			Miguel Vázquez Vázquez
# Creation date:    16 December 2023
# Description:      This file prepares stuff for meeting
# Input:            
#                   $pathEst/input/purchase_annexes.csv
# Output:           
#                   -
#///////////////////////////////////////////////////////////////////////////////
source('~/data/transactions_ecuador/3_mivazq/Masters_Thesis/setup.R')
#///////////////////////////////////////////////////////////////////////////////
#----                           1 - LOAD AND PREPARE                        ----
#///////////////////////////////////////////////////////////////////////////////

df_firm_info    <- fread(file=paste0(pathEst, "input/firm_info.csv"), na.strings="")
df_firm_info[, startdate := as.Date(strptime(startdate, format = "%d%b%Y"))]

# plot histogram to show entry based on firm registry
png(paste0(pathWD,"meeting prep/start_date.png"), width=1000, height=700, bg = "white")
hist(df_firm_info[startdate>="2008-01-01"]$startdate, freq=T, right=F, xlab=paste("Note: max date is",as.character(max(df_firm_info$startdate))),
     breaks=c(as.Date("2008-01-01"), as.Date("2009-01-01"), as.Date("2010-01-01"), 
              as.Date("2011-01-01"), as.Date("2012-01-01"), as.Date("2013-01-01")),
     main="Histogram of start date in firm registry (starting 2008-01-01)")
dev.off()

