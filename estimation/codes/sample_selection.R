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
df_tax_filings <- fread(file=paste0(pathCle, "output/tax_filings.csv"), na.strings="")
df_firm_info   <- fread(file=paste0(pathCle, "output/firm_info.csv"), na.strings="")
df_entities    <- fread(file=paste0(pathCle, "output/entities.csv"), na.strings="")

# Only keeping 2008-2011 years for now
if (!all.equal(unique(df_tax_filings$year), c(2008,2009,2010,2011))) {
    stop("There other years outside 2008-2011 range")
}

# Merge data sources
firm_sample <- unique(df_tax_filings[, .(id_sri, form)])
firm_sample <- merge.data.table(firm_sample, df_entities, by="id_sri", all.x=T)
firm_sample <- merge.data.table(firm_sample, df_firm_info[, .(id_sri, soe, isic_section)], by="id_sri", all.x=T)
setorder(firm_sample, cols ="id_sri")

# How do taxform type filing and firm indicator compare? Note that from 'tax_filings'
# I only kept tax filings with non-zero entries in the long form. Short-form 
# filers (people) are excluded
table(firm_sample$form, firm_sample$entity, useNA = 'always') # looks pretty good
firm_sample <- unique(firm_sample[, .(id_sri, entity, soe, isic_section)])

# For now I keep only entities that I labeled as "Company", regardless of which 
# taxform they actually filed (vast majority filed F101 though)
# firm_sample <- firm_sample[entity=="Company"] # CHANGED: keep both companies and book-keeping-obliged sole-proprietorships

# Additionally, I exclude firms in ISIC sections P and Q ("Activities of private 
# households as employers and undifferentiated production activities of private 
# households" and "Extraterritorial organizations and bodies", respectively), as 
# well as the 3 special Ecuador sections (R, S, T) and also firms without industry
firm_sample_drop <- firm_sample[isic_section %in% c("P","Q","R","S","T") | is.na(isic_section)]
firm_sample <- firm_sample[!( isic_section %in% c("P","Q","R","S","T") | is.na(isic_section) )]

# Check distribution of exclusions
table(firm_sample_drop$entity, firm_sample_drop$isic_section, useNA="ifany")

# Check distribution of state-owned enterprises (SOE) and ISIC sections
table(firm_sample$soe, firm_sample$isic_section)

# Check final distribution of sections
table(firm_sample$isic_section)
round(table(firm_sample$isic_section)/nrow(firm_sample)*100,2)

# Additionally exclude sections E and L ("Electricity, gas and water supply" and
# "Public administration and defence; compulsory social security", respectively)
# due to high share (>15%) of SOE firms in section
# firm_sample <- firm_sample[isic_section %nin% c("E","L")]

# Exclude SOE firms
# ...

#///////////////////////////////////////////////////////////////////////////////
#----       2 - IMPORT ENTRY AND EXIT YEARS FROM OTHER SOURCES, COMBINE     ----
#///////////////////////////////////////////////////////////////////////////////

## MVV: maybe add other sources e.g. PA & exports/imports? Would not be bad to use
# all sources that I actually take data from in the project


# Generate entry/exit dates based on tax filings and social security
# Note that this datasets include *ALL* IDs that appear in the data, regardless
# of whether they filed e.g. an empty tax form
filing_IDs_F101 <- as.data.table(read_dta(paste0(ecuRaw, "F101/IDs/ids_F101_filers.dta")))
filing_IDs_F102 <- as.data.table(read_dta(paste0(ecuRaw, "F102/IDs/ids_F102_filers.dta")))
filing_IDs_SS   <- as.data.table(read_dta(paste0(ecuRaw, "employment/IDs/ids_employers.dta")))
replaceIDs      <- as.data.table(read_dta(paste0(pathFun, "replaceID.dta")))

# Replace IDs to keep RUCs where possible
setnames(filing_IDs_F101, 'id_sri', 'id_sri_unique')
setnames(filing_IDs_F102, 'id_sri', 'id_sri_unique')
setnames(filing_IDs_SS,   'id_sri', 'id_sri_unique')
filing_IDs_F101 <- merge(filing_IDs_F101, replaceIDs, by="id_sri_unique", all.x = T)
filing_IDs_F101[!is.na(id_sri), id_sri_unique := id_sri]
filing_IDs_F101[, id_sri := NULL]
filing_IDs_F102 <- merge(filing_IDs_F102, replaceIDs, by="id_sri_unique", all.x = T)
filing_IDs_F102[!is.na(id_sri), id_sri_unique := id_sri]
filing_IDs_F102[, id_sri := NULL]
filing_IDs_SS <- merge(filing_IDs_SS, replaceIDs, by="id_sri_unique", all.x = T)
filing_IDs_SS[!is.na(id_sri), id_sri_unique := id_sri]
filing_IDs_SS[, id_sri := NULL]
setnames(filing_IDs_F101, 'id_sri_unique', 'id_sri')
setnames(filing_IDs_F102, 'id_sri_unique', 'id_sri')
setnames(filing_IDs_SS,   'id_sri_unique', 'id_sri')

# Re-collapse at id_sri level after fixing IDs (where needed)
if (nrow(filing_IDs_F101) != length(unique(filing_IDs_F101$id_sri))) {
    stop("NEED TO FILL THIS PART FOR F101 DATA AS WELL, SOME IDS ARE DUPLICATE")
}
if (nrow(filing_IDs_F102) != length(unique(filing_IDs_F102$id_sri))) {
    # Collapse at id_sri level by taking max values for single-year dummies,
    # min for first_filing and max for last_filing
    filing_IDs_F102 <- dcast(data      = filing_IDs_F102,
                             formula   = id_sri ~ .,
                             fun       = list(max,       max,       max,       
                                              max,       max,       max,       
                                              max,       max,       max,       
                                              max,       
                                              min,            max,
                                              sum,            sum),
                             value.var = list("in_2008", "in_2009", "in_2010", 
                                              "in_2011", "in_2012", "in_2013", 
                                              "in_2014", "in_2015", "in_2016", 
                                              "in_2017", 
                                              "first_filing", "last_filing",
                                              "filings_done", "filings_due"))
    
    # Fix variable name changes
    new_col_names <- colnames(filing_IDs_F102)
    new_col_names <- gsub("_max$", "", new_col_names)
    new_col_names <- gsub("_min$", "", new_col_names)
    new_col_names <- gsub("_sum$", "", new_col_names)
    setnames(filing_IDs_F102, colnames(filing_IDs_F102), new_col_names)
    
    # Adjust filings_done and filings_due ex-post
    filing_IDs_F102[, filings_done := rowSums(.SD), .SDcols = c("in_2008", "in_2009", "in_2010", 
                                                                "in_2011", "in_2012", "in_2013", 
                                                                "in_2014", "in_2015", "in_2016")]
    filing_IDs_F102[, filings_due := last_filing-first_filing+1]
    
}
if (nrow(filing_IDs_SS) != length(unique(filing_IDs_SS$id_sri))) {
    cat("NEED TO FILL THIS PART FOR SS DATA AS WELL, SOME IDS ARE DUPLICATE")
}

# Combine the three external sources
external <- merge(filing_IDs_F101[,.(id_sri, first_filing, last_filing)], 
                  filing_IDs_F102[,.(id_sri, first_filing, last_filing)], 
                  by="id_sri", all=T, suffixes = c(".F101", ".F102"))
external <- merge(external, 
                  filing_IDs_SS[,.(id_sri, first_filing.SS=first_filing, last_filing.SS=last_filing)], 
                  by="id_sri", all=T)
external[, first_filing := pmin(first_filing.F101, first_filing.F102, first_filing.SS, na.rm = T)]
external[, last_filing  := pmax(last_filing.F101,  last_filing.F102,  last_filing.SS , na.rm = T)]

rm(filing_IDs_F101,filing_IDs_F102,filing_IDs_SS,replaceIDs)

#///////////////////////////////////////////////////////////////////////////////
#----                   3 - DEFINE FINAL ENTRY AND EXIT YEAR                ----
#///////////////////////////////////////////////////////////////////////////////

# Convert string date to format date
df_firm_info[, startdate := as.Date(as.POSIXct(startdate, format = "%d%b%Y"))]
df_firm_info[, startyear := year(startdate)]

# For entry, take firm registry as first input and adapt in contradicting cases
firm_sample <- merge(firm_sample, df_firm_info[,.(id_sri,startyear)], by="id_sri", all.x = T)
firm_sample <- merge(firm_sample, external[,.(id_sri,first_filing,last_filing)], by="id_sri", all.x = T)
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

#///////////////////////////////////////////////////////////////////////////////
#----                   4 - PANELISE AND STORE FINAL SAMPLE                 ----
#///////////////////////////////////////////////////////////////////////////////

# Store final sample of firms
setorder(firm_sample, id_sri)
setorder(firm_sample_drop, id_sri)
save(firm_sample, file = paste0(pathEst, "input/firm_sample.Rdata"))
save(firm_sample_drop, file = paste0(pathEst, "input/firm_sample_drop.Rdata"))

# Create full panel
panel <- data.table()
for (yyyy in c(2008, 2009, 2010, 2011)) {
    panel <- rbind(panel, firm_sample[, year := yyyy])
}
if (nrow(panel)!=nrow(firm_sample)*4) {
    stop("Wrong number of observations in panel")
}

# Reorganise variables and save panel
panel <- panel[, .(year, id_sri, entity, soe, isic_section, entry_year, exit_year)]
setorder(panel, "id_sri","year")
save(panel, file = paste0(pathEst, "input/panel.Rdata"))

