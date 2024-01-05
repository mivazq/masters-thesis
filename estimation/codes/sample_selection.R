#///////////////////////////////////////////////////////////////////////////////
# File name:		sample_selection.R
# Author:			Miguel Vázquez Vázquez
# Creation date:    20 November 2023
# Description:      This file selects the sample of firms for the analysis.
# Input:            
#                   $pathEst/input/firm_info.csv
#                   $pathEst/input/tax_filings.csv
#                   $pathEst/input/entities.csv
# Output:           
#                   -
#///////////////////////////////////////////////////////////////////////////////
source('~/data/transactions_ecuador/3_mivazq/Masters_Thesis/setup.R')
#///////////////////////////////////////////////////////////////////////////////
#----                       1 - CREATE SAMPLE OF FIRMS                      ----
#///////////////////////////////////////////////////////////////////////////////

# Load data about tax filings and firm information
df_tax_filings <- fread(file=paste0(pathEst, "input/tax_filings.csv"), na.strings="")
df_firm_info   <- fread(file=paste0(pathEst, "input/firm_info.csv"), na.strings="")
df_entities    <- fread(file=paste0(pathEst, "input/entities.csv"), na.strings="")

# Only keeping 2008-2011 years for now
df_tax_filings <- df_tax_filings[year %in% c(2008,2009,2010,2011)]

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

# Store final sample of firms
setorder(firm_sample, id_sri)
setorder(firm_sample_drop, id_sri)
save(firm_sample, file = paste0(pathEst, "input/firm_sample.Rdata"))
save(firm_sample_drop, file = paste0(pathEst, "input/firm_sample_drop.Rdata"))

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

# We take firm registry information as first input and adapt in contradicting cases
firm_sample <- merge(firm_sample, df_firm_info[,.(id_sri,startyear)], by="id_sri", all.x = T)
firm_sample <- merge(firm_sample, external[,.(id_sri,first_filing,last_filing)], by="id_sri", all.x = T)
colSums(is.na(firm_sample)) # check that there are no missing values
warning("There are ",nrow(firm_sample[first_filing<startyear])," firms in our sample where their 'first filing' < 'start date' in the firm registry")
firm_sample[, entryyear := startyear]
firm_sample[first_filing<startyear, entryyear := first_filing]




# Generate entry dates
    # For firms found in registry take their "start date"
    panel[, entrydate := year(startdate)]

    # Adjust when entry date later than first filing of F101 form
    panel[, first_filing := min(year), by=id_sri]
    cat("There are", nrow(panel[entrydate>first_filing]), "such cases\n")
    panel[entrydate>first_filing, entrydate := first_filing]
    
    # Adjust when entry date later than first filing of social security data
    entry_emp <- copy(df_wage_bills)
    entry_emp[, entrydate := min(year), by=id_sri]
    entry_emp <- unique(entry_emp[,.(id_sri, entry_emp=entrydate)])
    panel <- merge.data.table(panel, entry_emp, by="id_sri", all.x=T)
    cat("There are further", nrow(panel[entrydate>entry_emp]), "such cases\n")
    panel[entrydate>entry_emp, entrydate := entry_emp]
    
    # Generate entry date for firms not found in registry
    panel[, imputed := is.na(startdate)] # create var to label imputed entries
    cat("There are", nrow(panel[imputed==T]), "such cases\n")
    panel[imputed==T, entrydate := first_filing] # based on tax filing
    panel[entrydate>entry_emp, entrydate := entry_emp] # adjust with employment data

    # Finally, simply cut off entry dates to 2007 (1 year before our 2008-2015 period)
    panel[entrydate<2007, entrydate := 2007]
    panel[, startdate := NULL] # drop start date var
    cat("Assert every firm has an entry date, must print TRUE:\n",
        sum(is.na(panel$entrydate))==0,"\n")
    rm(entry_emp)
    
# Generate exit dates
    # Based on last date they fill a form
    panel[, exitdate := max(year), by=id_sri]
    
    # Adjust when exit date later than last filing of F101 form
    # Note: it might make sense that firms appear in social security but don't
    # file a tax form if, e.g., they go bankrupt mid-year
    exit_emp <- copy(df_wage_bills)
    exit_emp[, exitdate := max(year), by=id_sri]
    exit_emp <- unique(exit_emp[,.(id_sri, exit_emp=exitdate)])
    panel <- merge.data.table(panel, exit_emp, by="id_sri", all.x=T)
    cat("There are", nrow(panel[exitdate<exit_emp]), "such cases\n")
    panel[exitdate<exit_emp, exitdate := exit_emp]
    
    # Finally, simply cut off entry dates to 2016 (1 year after our 2008-2015 period)
    panel[exitdate>=2016, exitdate := 2016] # no exit if last form in 2017
    cat("Assert every firm has an exit date, must print TRUE:\n",
        sum(is.na(panel$exitdate))==0,"\n")
    rm(exit_emp)
    
# Create plot to show effect of imputation of missing entry year
df_plot <- panel[, .(id_sri, imputed, entrydate, count=1)]
df_plot <- unique(df_plot) # keep only unique entries, don't double count firms
cat("Assert we have one observation for firm, must print TRUE:\n",
    nrow(df_plot)==length(firm_ids),"\n")
df_plot <- aggregate(df_plot[entrydate %in% seq(2008,2015,1)], count ~ entrydate + imputed, FUN=length)
plot <- ggplot(df_plot, aes(x=entrydate,y=count)) + 
    geom_col(aes(fill=imputed), position='stack') +
    scale_x_continuous(breaks = 2008:2015, 
                       labels = as.character(2008:2015)) +
    scale_y_continuous(limits = c(0, 10000)) +
    xlab("Year") + ylab("Entries") + 
    ggtitle("Entry of companies") + 
    theme_bw() + theme(axis.title.y = element_text(angle=0, vjust=0.5))
ggsave(plot, filename = paste0(pathFig,sysdate,'_entry_date.png'),width = 12, height = 7)
rm(df_plot, plot)

# Create plot to see exits
df_plot <- panel[, .(id_sri, exitdate, count=1)]
df_plot <- unique(df_plot) # keep only unique entries, don't double count firms
cat("Assert we have one observation for firm, must print TRUE:\n",
    nrow(df_plot)==length(firm_ids),"\n")
df_plot <- aggregate(df_plot[exitdate %in% seq(2008,2015,1)], count ~ exitdate, FUN=length)
plot <- ggplot(df_plot, aes(x=exitdate,y=count)) + 
    geom_col() +
    scale_x_continuous(breaks = 2008:2015, 
                       labels = as.character(2008:2015)) +
    scale_y_continuous(limits = c(0, 10000)) +
    xlab("Year") + ylab("Exits") + 
    ggtitle("Exits of companies") + 
    theme_bw() + theme(axis.title.y = element_text(angle=0, vjust=0.5))
ggsave(plot, filename = paste0(pathFig,sysdate,'_exit_date.png'),width = 12, height = 7)
rm(df_plot, plot)

# Check how many cases we have of firms not filing even though still in market
panel[is.na(exitdate), exitdate := 2015] # no exit if last form in 2015
panel[, filing_years_due := exitdate-ifelse(entrydate>=2008, entrydate, 2008)+1]



#///////////////////////////////////////////////////////////////////////////////






















# Create full panel
panel <- data.table()
for (yyyy in c(2008, 2009, 2010, 2011)) {
    panel <- rbind(panel, firm_sample[, year := yyyy])
}
cat("Assert correct number of observations, must print TRUE:\n",
    nrow(panel)==nrow(firm_sample)*4,"\n")