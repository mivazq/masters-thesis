#///////////////////////////////////////////////////////////////////////////////
# File name:		network_metrics_regional.R
# Author:			Miguel Vázquez Vázquez
# Creation date:    30 April 2024
# Description:      This file estimates markups for all firms, year to year.
# Input:            
#                   -

# Output:           
#                   -
#///////////////////////////////////////////////////////////////////////////////
source('~/data/transactions_ecuador/3_mivazq/Masters_Thesis/setup.R')
#///////////////////////////////////////////////////////////////////////////////
#----                       1 - CONSTRUCT METRICS                           ----
#///////////////////////////////////////////////////////////////////////////////

# Load transactions and firm information
df_transactions <- fread(file=paste0(pathCle, "output/intermediate_transactions.csv"), 
                         na.strings="", colClasses = c(transaction_value = "double"))
df_firm_info    <- fread(file=paste0(pathCle, "output/firm_info.csv"), na.strings="")

# Generate new variable
df_firm_info[, geo := paste(region_geo, "-", province)]

# Merge firm sectors
df_transactions <- merge.data.table(df_transactions, df_firm_info[,.(id_sri, buyer_sec  = isic_division, buyer_province  = geo)], by.x="id_buyer",  by.y="id_sri", all.x=T)
df_transactions <- merge.data.table(df_transactions, df_firm_info[,.(id_sri, seller_sec = isic_division, seller_province = geo)], by.x="id_seller", by.y="id_sri", all.x=T)

# Exclude firms for which no sector information is available (mostly due to sellers)
cat("There are", 
    fp(nrow(df_transactions)),"transactions,", 
    fp(length(unique(df_transactions$id_seller))), "unique sellers and", 
    fp(length(unique(df_transactions$id_buyer))),"unique buyers before removal of missing sector firms.",
    "The total value across all transactions is",fp(sum(df_transactions$transaction_value)),
    "and the total number of unique transactions (frequencies) is",fp(sum(df_transactions$transaction_volume)))

df_transactions <- df_transactions[!is.na(buyer_sec) & !is.na(seller_sec)]

cat("There are", 
    fp(nrow(df_transactions)),"transactions,", 
    fp(length(unique(df_transactions$id_seller))), "unique sellers and", 
    fp(length(unique(df_transactions$id_buyer))),"unique buyers after removal of missing sector firms.",
    "The total value across all transactions is",fp(sum(df_transactions$transaction_value)),
    "and the total number of unique transactions (frequencies) is",fp(sum(df_transactions$transaction_volume)))


# Check how much firms trade across regions
regional_trade <- dcast(data = df_transactions,
                        formula = seller_province + buyer_province ~ .,
                        fun = sum,
                        value.var=c("transaction_value","transaction_volume"))
regional_trade[, tot_val := sum(transaction_value), by = "buyer_province"]
regional_trade[, tot_vol := sum(transaction_volume), by = "buyer_province"]
regional_trade[, transaction_value := transaction_value/tot_val]
regional_trade[, transaction_volume := transaction_volume/tot_vol]
regional_trade[, buyer_province := factor(buyer_province)]
regional_trade[, seller_province := factor(seller_province, levels=rev(levels(regional_trade$buyer_province)))]

ggplot(regional_trade, aes(x = buyer_province, y = seller_province)) +
    geom_tile(aes(fill = transaction_value)) +
    scale_fill_gradient(low = "white", high = "black") +
    labs(title = "Transaction Value Across Regions Heatmap (as share of province's total purchases' value)", x = "Province of buyer", y = "Province of seller") +
    theme(axis.text.x = element_text(angle=90))

ggplot(regional_trade, aes(x = buyer_province, y = seller_province)) +
    geom_tile(aes(fill = transaction_volume)) +
    scale_fill_gradient(low = "white", high = "black") +
    labs(title = "Transaction Counts Across Regions Heatmap (as share of province's total purchases' count)", x = "Province of buyer", y = "Province of seller") +
    theme(axis.text.x = element_text(angle=90))

# How is this picture different for our markup sample?
regional_trade_mu <- dcast(data = df_transactions[id_seller %in% markups_V$id],
                           formula = seller_province + buyer_province ~ .,
                           fun = sum,
                           value.var=c("transaction_value","transaction_volume"))
regional_trade_mu <- merge(regional_trade_mu, regional_trade[,.(seller_province,buyer_province)], all.y = T) # ensure all combinations exist
regional_trade_mu[is.na(transaction_value), c("transaction_value", "transaction_volume") := 0] # assign zeros to previously missing combinations
regional_trade_mu[, tot_val := sum(transaction_value), by = "buyer_province"]
regional_trade_mu[, tot_vol := sum(transaction_volume), by = "buyer_province"]
regional_trade_mu[, transaction_value := transaction_value/tot_val]
regional_trade_mu[, transaction_volume := transaction_volume/tot_vol]
regional_trade_mu[, buyer_province := factor(buyer_province)]
regional_trade_mu[, seller_province := factor(seller_province, levels=rev(levels(regional_trade_mu$buyer_province)))]


ggplot(regional_trade_mu, aes(x = buyer_province, y = seller_province)) +
    geom_tile(aes(fill = transaction_value)) +
    scale_fill_gradient(low = "white", high = "black") +
    labs(title = "Transaction Value Across Regions Heatmap (as share of province's total purchases' value)", x = "Province of buyer", y = "Province of seller") +
    theme(axis.text.x = element_text(angle=90))

ggplot(regional_trade_mu, aes(x = buyer_province, y = seller_province)) +
    geom_tile(aes(fill = transaction_volume)) +
    scale_fill_gradient(low = "white", high = "black") +
    labs(title = "Transaction Counts Across Regions Heatmap (as share of province's total purchases' count)", x = "Province of buyer", y = "Province of seller") +
    theme(axis.text.x = element_text(angle=90))
