#///////////////////////////////////////////////////////////////////////////////
# File name:		sector_prediction.R
# Author:			Miguel Vázquez Vázquez
# Creation date:    20 November 2023
# Description:      This file predicts the industry of a firm based on its sale
#                   and purchase behaviours.
# Input:            
#                   $pathEst/input/firm_ids.Rdata
#                   $pathEst/input/intermediate_transactions.csv
#                   $pathEst/input/firm_info.csv
#                   $pathEst/input/isic_codes_sections.csv
#                   $pathEst/input/isic_codes_division.csv
# Output:           
#                   -
#///////////////////////////////////////////////////////////////////////////////
source('~/data/transactions_ecuador/3_mivazq/Masters_Thesis/setup.R')
#///////////////////////////////////////////////////////////////////////////////
#----                           1 - LOAD AND PREPARE                        ----
#///////////////////////////////////////////////////////////////////////////////

# Load sample of firms
load(paste0(pathEst, "input/firm_ids.Rdata")) # load sample of firm IDs we are interested in

# Load transactions and firm information
df_transactions <- fread(file=paste0(pathEst, "input/intermediate_transactions.csv"), 
                         na.strings="", colClasses = c(transaction_value = "double"))
df_firm_info    <- fread(file=paste0(pathEst, "input/firm_info.csv"), na.strings="")
df_firm_info[, startdate := as.Date(strptime(startdate, format = "%d%b%Y"))]

# Let's first try with sections only!!!
keep_sectors = c("A","B","C","D","E","F","G","H","I","J","K","L","M","N","O")
nr_sectors = length(keep_sectors) # 15 sections in ISIC Rev 3.1 (excluding P and Q), also exclude 3 special Ecuador (R, S, T)

# Load ISIC divisions
df_isic_divisions <- fread(file=paste0(pathEst, "input/isic_codes_division.csv"), na.strings="")
keep_divisions = df_isic_divisions$isic_division[substr(df_isic_divisions$isic_division,1,1) %in% keep_sectors]
nr_divisions = length(keep_divisions)


# Create data frame with firm IDs, known sector, predicted sector
panel <- data.table(id_sri = firm_ids)
panel <- merge.data.table(panel, df_firm_info[,.(id_sri, isic_section)], by="id_sri", all.x=T)
# panel <- merge.data.table(panel, df_firm_info[,.(id_sri, isic_division)], by="id_sri", all.x=T)
setnames(panel, "isic_section", "isic_section_known")
# setnames(panel, "isic_division", "isic_section_known")
paste0(round(sum(is.na(panel$isic_section_known))/nrow(panel)*100,2), "% of the firm IDs don't have information on industry")

# Merge information on sectors for all firms in transaction data
df_transactions <- merge.data.table(df_transactions, df_firm_info[,.(id_sri, buyer_isic_section  = isic_section)], by.x="id_buyer",  by.y="id_sri", all.x=T)
df_transactions <- merge.data.table(df_transactions, df_firm_info[,.(id_sri, seller_isic_section = isic_section)], by.x="id_seller", by.y="id_sri", all.x=T)
# df_transactions <- merge.data.table(df_transactions, df_firm_info[,.(id_sri, buyer_isic_section  = isic_division)], by.x="id_buyer",  by.y="id_sri", all.x=T)
# df_transactions <- merge.data.table(df_transactions, df_firm_info[,.(id_sri, seller_isic_section = isic_division)], by.x="id_seller", by.y="id_sri", all.x=T)

# Function to perform L2 normalization
l2_normalize <- function(x) {
    as.numeric(x) / sqrt(sum(as.numeric(x)^2, na.rm = T))
}

#///////////////////////////////////////////////////////////////////////////////
#----               2 - CREATE AGGREGATE PURCHASE SHARES VECTORS            ----
#///////////////////////////////////////////////////////////////////////////////

# To construct the representative embeddings for each sector I try here as first
# attempt to simply sum all purchases for every sector-sector combination and 
# then construct shares. I will also keep "NA" as additional setting in the 
# embedding but it is not a value we will predict, since I need to assign a 
# specific sector to each firm.

### FILTER
df_transactions_filtered <- df_transactions
# df_transactions_filtered <- df_transactions[(id_seller %in% firm_ids & id_buyer %in% firm_ids)]
paste0("Now remain ",round(nrow(df_transactions_filtered)/nrow(df_transactions)*100),"% of transactions")

# Collapse information at sector-sector level by summing transaction values
df_transactions_sectors <- dcast.data.table(data = df_transactions_filtered,
                                            # formula = id_buyer + buyer_isic_section + seller_isic_section ~ .,
                                            formula = buyer_isic_section + seller_isic_section ~ .,
                                            # fun.aggregate = mean,
                                            fun.aggregate = sum,
                                            value.var = "transaction_value")
setnames(df_transactions_sectors, ".", "transaction_value")

        # df_transactions_sectors <- dcast.data.table(data = df_transactions_sectors,
        #                                             formula = buyer_isic_section + seller_isic_section ~ .,
        #                                             fun.aggregate = mean,
        #                                             value.var = "transaction_value")
        # setnames(df_transactions_sectors, ".", "transaction_value")



### BUYERS

# Create table for buyers
buyer_vectors <- df_transactions_sectors[!is.na(buyer_isic_section)] # exclude NAs from buyers
buyer_vectors <- df_transactions_sectors[!is.na(seller_isic_section)] # exclude NAs from sellers
buyer_vectors <- as.data.table(complete(buyer_vectors,buyer_isic_section,seller_isic_section,fill=list(transaction_value=0))) # fill in missing combinations
if (nrow(buyer_vectors)==nr_sectors*(nr_sectors+1)) { cat("True\n") }
# if (nrow(buyer_vectors)==nr_sectors*(nr_divisions+1)) { cat("True\n") }

# Create purchases from each sector (including NA) for each sector
buyer_vectors <- dcast.data.table(data = buyer_vectors,
                                  formula = seller_isic_section ~ buyer_isic_section,
                                  fun.aggregate = sum,
                                  value.var = "transaction_value",
                                  fill = 0) # reshape















# Collapse information at sector-sector level by summing transaction values
df_transactions_sectors <- dcast.data.table(data = df_transactions_filtered,
                                            # formula = id_seller + seller_isic_section + buyer_isic_section ~ .,
                                            formula = buyer_isic_section + seller_isic_section ~ .,
                                            # fun.aggregate = mean,
                                            fun.aggregate = sum,
                                            value.var = "transaction_value")
setnames(df_transactions_sectors, ".", "transaction_value")

                # df_transactions_sectors <- dcast.data.table(data = df_transactions_sectors,
                #                                             formula = seller_isic_section + buyer_isic_section ~ .,
                #                                             fun.aggregate = mean,
                #                                             value.var = "transaction_value")
                # setnames(df_transactions_sectors, ".", "transaction_value")



### SELLER

# Create table for buyers
seller_vectors <- df_transactions_sectors[!is.na(seller_isic_section)] # exclude NAs from sellers
seller_vectors <- df_transactions_sectors[!is.na(buyer_isic_section)] # exclude NAs from buyers
seller_vectors <- as.data.table(complete(seller_vectors,buyer_isic_section,seller_isic_section,fill=list(transaction_value=0))) # fill in missing combinations
# if (nrow(seller_vectors)==nr_sectors*(nr_sectors+1)) { cat("True\n") }
if (nrow(seller_vectors)==nr_sectors*(nr_divisions+1)) { cat("True\n") }

# Create purchases from each sector (including NA) for each sector
seller_vectors <- dcast.data.table(data = seller_vectors,
                                   formula = buyer_isic_section ~ seller_isic_section,
                                   fun.aggregate = sum,
                                   value.var = "transaction_value",
                                   fill = 0) # reshape



#### COMBINE
rownames <- c(paste0("seller_",buyer_vectors$seller_isic_section), 
              paste0("buyer_",seller_vectors$buyer_isic_section))
buyer_vectors <- buyer_vectors[, -c("seller_isic_section")]
seller_vectors <- seller_vectors[, -c("buyer_isic_section")]
aggregate_vectors <- rbind(buyer_vectors, seller_vectors, use.names=T)
colnames <- colnames(aggregate_vectors)
aggregate_vectors[, (colnames) := lapply(.SD, l2_normalize), .SDcols = colnames]



#///////////////////////////////////////////////////////////////////////////////
#----           3 - CREATE PURCHASE SHARES VECTORS FOR ALL BUYERS           ----
#///////////////////////////////////////////////////////////////////////////////

### BUYERS

# Collapse information at sector-sector level by summing transaction values
df_transactions_buyers_sectors <- dcast.data.table(data = df_transactions_filtered,
                                                   formula = id_buyer + seller_isic_section ~ .,
                                                   fun.aggregate = sum,
                                                   value.var = "transaction_value")
setnames(df_transactions_buyers_sectors, ".", "transaction_value")

# Create table for buyers by keeping only firms in our panel
single_buyer_vectors <- df_transactions_buyers_sectors[!is.na(seller_isic_section)] # exclude NAs from sellers
single_buyer_vectors <- merge.data.table(panel[,.(id_sri)], df_transactions_buyers_sectors, by.x="id_sri", by.y="id_buyer", all.x=T)
single_buyer_vectors <- as.data.table(complete(single_buyer_vectors,id_sri,seller_isic_section,fill=list(transaction_value=0))) # fill in missing combinations
# if (nrow(single_buyer_vectors)==length(firm_ids)*(nr_sectors+1)) { cat("True\n") }
if (nrow(single_buyer_vectors)==length(firm_ids)*(nr_divisions+1)) { cat("True\n") }

# Create purchases from each sector (including NA) for each sector
single_buyer_vectors <- dcast.data.table(data = single_buyer_vectors,
                                         formula = seller_isic_section ~ id_sri,
                                         fun.aggregate = sum,
                                         value.var = "transaction_value") # reshape
# rownames <- single_buyer_vectors$seller_isic_section
# single_buyer_vectors <- single_buyer_vectors[, -c("seller_isic_section")]
# colnames <- colnames(single_buyer_vectors)
# single_buyer_vectors[, (colnames) := lapply(.SD, l2_normalize), .SDcols = colnames]


### SELLERS

# Collapse information at sector-sector level by summing transaction values
df_transactions_sellers_sectors <- dcast.data.table(data = df_transactions_filtered,
                                                    formula = id_seller + buyer_isic_section ~ .,
                                                    fun.aggregate = sum,
                                                    value.var = "transaction_value")
setnames(df_transactions_sellers_sectors, ".", "transaction_value")

# Create table for sellers by keeping only firms in our panel
single_seller_vectors <- df_transactions_sellers_sectors[!is.na(buyer_isic_section)] # exclude NAs from sellers
single_seller_vectors <- merge.data.table(panel[,.(id_sri)], df_transactions_sellers_sectors, by.x="id_sri", by.y="id_seller", all.x=T)
single_seller_vectors <- as.data.table(complete(single_seller_vectors,id_sri,buyer_isic_section,fill=list(transaction_value=0))) # fill in missing combinations
# if (nrow(single_seller_vectors)==length(firm_ids)*(nr_sectors+1)) { cat("True\n") }
if (nrow(single_seller_vectors)==length(firm_ids)*(nr_divisions+1)) { cat("True\n") }

# Create purchases from each sector (including NA) for each sector
single_seller_vectors <- dcast.data.table(data = single_seller_vectors,
                                          formula = buyer_isic_section ~ id_sri,
                                          fun.aggregate = sum,
                                          value.var = "transaction_value") # reshape
# rownames <- single_seller_vectors$buyer_isic_section
# single_seller_vectors <- single_seller_vectors[, -c("buyer_isic_section")]
# colnames <- colnames(single_seller_vectors)
# single_seller_vectors[, (colnames) := lapply(.SD, l2_normalize), .SDcols = colnames]

# # Split the original data.table into a list of smaller data.tables
# chunk_size <- 100000
# col_batches <- split(colnames, ceiling(seq_along(colnames) / chunk_size))
# normalized_tables <- list()
# i = 1
# # Process each chunk independently
# for (batch in col_batches) {
#     result <- system.time({
#     # Create a temporary data.table with the selected columns
#     temp_dt <- single_seller_vectors[, (batch), with = FALSE]
#     
#     # Normalize the selected columns
#     temp_dt[, (batch) := lapply(.SD, l2_normalize), .SDcols = batch]
#     
#     # Add the normalized data.table to the list
#     normalized_tables[[length(normalized_tables) + 1]] <- temp_dt
#     })
#     elapsed_time <- result["elapsed"]
#     cat("Elapsed Time for iteration",i,":", elapsed_time, "seconds\n")
#     i = i + 1
# }
# 
# # Combine the normalized data.tables into a single data.table
# single_seller_vectors <- do.call(cbind, normalized_tables)



#### COMBINE
rownames <- c(paste0("seller_",single_buyer_vectors$seller_isic_section), 
              paste0("buyer_",single_seller_vectors$buyer_isic_section))
single_buyer_vectors <- single_buyer_vectors[, -c("seller_isic_section")]
single_seller_vectors <- single_seller_vectors[, -c("buyer_isic_section")]
individual_vectors <- rbind(single_buyer_vectors, single_seller_vectors, use.names=T)
colnames <- colnames(individual_vectors)
individual_vectors[, (colnames) := lapply(.SD, l2_normalize), .SDcols = colnames]






# PREDICT ####

# Keep only needed vectors (i.e. for the sectors we want to predict)
aggregate_vectors <- aggregate_vectors[, keep_sectors, with=FALSE]
# aggregate_vectors <- aggregate_vectors[, keep_divisions, with=FALSE]

# Construct cosine similarity matrix
# cos_sim_buy <- as.data.table(t(as.matrix(single_buyer_vectors)) %*% as.matrix(buyer_vectors))
# cos_sim_sel <- as.data.table(t(as.matrix(single_seller_vectors)) %*% as.matrix(seller_vectors))
cos_sim <- as.data.table(t(as.matrix(individual_vectors)) %*% as.matrix(aggregate_vectors))

# Find the column name with the maximum value for each row
# cos_sim_buy[, pred_sector := colnames(.SD)[max.col(.SD, ties.method = "first")]]
# cos_sim_sel[, pred_sector := colnames(.SD)[max.col(.SD, ties.method = "first")]]
cos_sim[, pred_sector := colnames(.SD)[max.col(.SD, ties.method = "first")]]

# Match with panel to check accuracy
# cos_sim_buy[, id_sri := as.numeric(colnames(single_buyer_vectors))]
# cos_sim_sel[, id_sri := as.numeric(colnames(single_seller_vectors))]
# panel <- merge.data.table(panel, cos_sim_buy[, .(id_sri, pred_sector_buy = pred_sector)], by="id_sri", all.x=T)
# panel <- merge.data.table(panel, cos_sim_sel[, .(id_sri, pred_sector_sel = pred_sector)], by="id_sri", all.x=T)
cos_sim[, id_sri := as.numeric(colnames(individual_vectors))]
panel <- merge.data.table(panel, cos_sim[, .(id_sri, pred_sector)], by="id_sri", all.x=T)


# Check
# nrow(panel[!is.na(isic_section_known)]) # number of firms with non-missing sector
# nrow(panel[!is.na(isic_section_known) & !is.na(pred_sector_buy)]) # number of firms with non-missing sector & predicted based on purchases
# nrow(panel[!is.na(isic_section_known) & !is.na(pred_sector_sel)]) # number of firms with non-missing sector & predicted based on sales
# nrow(panel[!is.na(isic_section_known) & !is.na(pred_sector_buy) & isic_section_known==pred_sector_buy])
# nrow(panel[!is.na(isic_section_known) & !is.na(pred_sector_sel) & isic_section_known==pred_sector_sel])
# 
# nrow(panel[!is.na(pred_sector_buy) & !is.na(pred_sector_sel) & pred_sector_buy==pred_sector_sel])

nrow(panel[!is.na(isic_section_known)]) # number of firms with non-missing sector
nrow(panel[!is.na(isic_section_known) & !is.na(pred_sector)]) # number of firms with non-missing sector & predicted based on purchases+sales
nrow(panel[!is.na(isic_section_known) & !is.na(pred_sector) & isic_section_known==pred_sector])
nrow(panel[!is.na(isic_section_known) & !is.na(pred_sector) & isic_section_known==pred_sector])/nrow(panel[!is.na(isic_section_known) & !is.na(pred_sector)])
nrow(panel[!is.na(isic_section_known) & !is.na(pred_sector) & substr(isic_section_known,1,1) %in% c("A","B","C","D") & isic_section_known==pred_sector])/nrow(panel[!is.na(isic_section_known) & !is.na(pred_sector) & substr(isic_section_known,1,1) %in% c("A","B","C","D")])

panel[,pred_sector:=NULL]

#///////////////////////////////////////////////////////////////////////////////
#----           4 - AUTOMATIC FUNCTION           ----
#///////////////////////////////////////////////////////////////////////////////

# Compute cosine similarity for each buyer vector with each buyer sector vector
predict_sector <- function(firm_vectors, sector_vectors) {
    
    # Create empty table to store results
    n_sect <- nrow(sector_vectors)
    n_firm <- nrow(firm_vectors)
    predicted_sectors <- data.table(id_sri      = rep(rownames(firm_vectors), n_sect),
                                    sector      = lapply(rownames(sector_vectors), function(x) rep(x, n_firm)),
                                    pred_sector = NA)
    

    return(predicted_sectors)
}




