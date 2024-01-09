#///////////////////////////////////////////////////////////////////////////////
# File name:		sample_selection.R
# Author:			Miguel Vázquez Vázquez
# Creation date:    20 November 2023
# Description:      This file selects the sample of firms for the analysis.
# Input:            
#                   $pathCle/output/firm_info.csv
#                   $pathCle/output/tax_filings.csv
#                   $pathCle/output/entities.csv
# Output:           
#                   $pathEst/firm_sample.Rdata
#///////////////////////////////////////////////////////////////////////////////
source('~/data/transactions_ecuador/3_mivazq/Masters_Thesis/setup.R')
#///////////////////////////////////////////////////////////////////////////////
#----                       1 - CREATE SAMPLE OF FIRMS                      ----
#///////////////////////////////////////////////////////////////////////////////

# Load data about tax filings and firm information
df_active    <- fread(file=paste0(pathCle, "output/active_firms.csv"), na.strings="")
df_firm_info <- fread(file=paste0(pathCle, "output/firm_info.csv"), na.strings="")
df_entities  <- fread(file=paste0(pathCle, "output/entities.csv"), na.strings="")

# Only keeping 2008-2011 years for now
if (!all.equal(unique(df_active$year), c(2008,2009,2010,2011))) {
    stop("There other years outside 2008-2011 range")
}

# Merge data sources
firm_sample <- unique(df_active[, .(id_sri)])
firm_sample <- merge.data.table(firm_sample, df_entities, by="id_sri", all.x=T)
firm_sample <- merge.data.table(firm_sample, df_firm_info[, .(id_sri, soe, isic_section)], by="id_sri", all.x=T)
setorder(firm_sample, cols ="id_sri")

# First, I exclude firms in ISIC sections P and Q ("Activities of private 
# households as employers and undifferentiated production activities of private 
# households" and "Extraterritorial organizations and bodies", respectively), as 
# well as the 3 special Ecuador sections (R, S, T) and also firms without industry
firm_sample_drop <- firm_sample[isic_section %in% c("P","Q","R","S","T") | is.na(isic_section)]
firm_sample <- firm_sample[!( isic_section %in% c("P","Q","R","S","T") | is.na(isic_section) )]

# Check distribution of exclusions
table(firm_sample_drop$entity, firm_sample_drop$isic_section, useNA="ifany")

# Check distribution of state-owned enterprises (SOE) and ISIC sections
table(firm_sample$soe, firm_sample$isic_section)

# Exclude SOE firms since their filings are quite unreliable
firm_sample_drop <- rbind(firm_sample_drop, firm_sample[soe==1])
firm_sample <- firm_sample[soe==0]

# Additionally exclude sections E and L ("Electricity, gas and water supply" and
# "Public administration and defence; compulsory social security", respectively)
# due to high share (>15%) of SOE firms in section
# firm_sample_drop <- rbind(firm_sample_drop, firm_sample[isic_section %in% c("E","L")])
# firm_sample <- firm_sample[!( isic_section %in% c("E","L") )]

# Check final distribution of sections
table(firm_sample$isic_section)
round(table(firm_sample$isic_section)/nrow(firm_sample)*100,2)

#///////////////////////////////////////////////////////////////////////////////
#----                   3 - DEFINE FINAL ENTRY AND EXIT YEAR                ----
#///////////////////////////////////////////////////////////////////////////////

# Convert string date to format date
df_firm_info[, startdate := as.Date(as.POSIXct(startdate, format = "%d%b%Y"))]
df_firm_info[, startyear := year(startdate)]

# For entry, take firm registry as first input and adapt in contradicting cases
firm_sample <- merge(firm_sample, df_firm_info[,.(id_sri,startyear)], by="id_sri", all.x = T)
firm_sample <- merge(firm_sample, unique(df_active[,.(id_sri,first_filing,last_filing,missing_year_sample)]), by="id_sri", all.x = T)
colSums(is.na(firm_sample)) # check that there are no missing values
warning("There are ",nrow(firm_sample[first_filing<startyear])," firms in our sample where their 'first filing' < 'start date' in the firm registry")
firm_sample[, entry_year := startyear]
firm_sample[first_filing<startyear, entry_year := first_filing]
if (sum(is.na(firm_sample$entrydate))!=0) {
    stop("There are firms with missing entry year")
}
firm_sample[, c("startyear","first_filing") := NULL]

# For exit, we don't see anything in firm registry: take last filing date
firm_sample[, exit_year := last_filing]
if (sum(is.na(firm_sample$exit_year))!=0) {
    stop("There are firms with missing exit year")
}
firm_sample[, last_filing := NULL]

# Store final sample of firms
setorder(firm_sample, id_sri)
setorder(firm_sample_drop, id_sri)
save(firm_sample, file = paste0(pathEst, "input/firm_sample.Rdata"))
save(firm_sample_drop, file = paste0(pathEst, "input/firm_sample_drop.Rdata"))

#///////////////////////////////////////////////////////////////////////////////
#----                           4 - CREATE PANEL                            ----
#///////////////////////////////////////////////////////////////////////////////

# Create full panel
panel <- data.table()
for (yyyy in c(2008, 2009, 2010, 2011)) {
    panel <- rbind(panel, firm_sample[, year := yyyy])
}
if (nrow(panel)!=nrow(firm_sample)*4) {
    stop("Wrong number of observations in panel")
}

# Reorganise variables
panel <- panel[, .(year, id_sri, entity, soe, isic_section, entry_year, exit_year, missing_year_sample)]
setorder(panel, "id_sri","year")

# Create dummies for activity/entry/exit
panel[, active := ifelse(year>=entry_year & year<=exit_year, 1, 0)]
panel[, entry  := ifelse(year==entry_year, 1, 0)]
panel[, exit   := ifelse(year==exit_year,  1, 0)]
panel[, c("entry_year", "exit_year") := NULL]

# Save panel
panel[, soe := NULL]
save(panel, file = paste0(pathEst, "input/panel.Rdata"))
