#///////////////////////////////////////////////////////////////////////////////
# File name:		network_metrics.R
# Author:			Miguel Vázquez Vázquez
# Creation date:    21 February 2024
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
load(file = paste0(pathEst, "output/firm_markups_V.Rdata"))

# Merge firm sectors
df_transactions <- merge.data.table(df_transactions, df_firm_info[,.(id_sri, buyer_sec  = isic_division)], by.x="id_buyer",  by.y="id_sri", all.x=T)
df_transactions <- merge.data.table(df_transactions, df_firm_info[,.(id_sri, seller_sec = isic_division)], by.x="id_seller", by.y="id_sri", all.x=T)

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




# Get some summary statistics on transactions for sellers
distinct <- function(x) {
    return(length(unique(x)))    
}
sum_stats_sellers <- dcast.data.table(df_transactions,
                                      year + id_seller + seller_sec ~.,
                                      fun = list(length, distinct, sum, sum),
                                      value.var=list("id_buyer","buyer_sec","transaction_volume","transaction_value"))
setnames(sum_stats_sellers, "id_seller", "id_sri")
sum_stats_sellers <- merge(sum_stats_sellers, unique(df_transactions[,.(year,id_sri=id_buyer,one=1)]), by=c("year","id_sri"),all.x=T)
sum_stats_sellers[is.na(one), one := 0]
setnames(sum_stats_sellers, 
         c("id_buyer_length", "buyer_sec_distinct", "transaction_volume_sum", "transaction_value_sum","one"),
         c("unique_buyers",   "unique_industries",  "trans_freq",             "trans_val",            "also_buyer"))
sum_stats_sellers[, avg_trans_amount := trans_val/trans_freq]
save(sum_stats_sellers, file = paste0(pathEst,'output/sum_stats_sellers.Rdata'))




# Check correlation between average transaction size and frequency of transactions
# df_transactions[, avg_trans_size := transaction_value/transaction_volume]
# ggplot(data=df_transactions, aes(x=transaction_volume, y=avg_trans_size)) + geom_point() # need to cluster points somehow, too many points graph wont load

# # Print information on number of buyers, sellers, transactions per year
# sss = 0; bbb = 0; ttt = 0;
# for (yyy in 2008:2011) {
#     cat("In",yyy,"there were",fp(length(unique(df_transactions[year==yyy]$id_seller))),
#         "unique sellers,",fp(length(unique(df_transactions[year==yyy]$id_buyer))), 
#         "unique buyers, and",fp(nrow(df_transactions[year==yyy])),"yearly aggregated transactions.\n")
#     sss = sss + length(unique(df_transactions[year==yyy]$id_seller))
#     bbb = bbb + length(unique(df_transactions[year==yyy]$id_buyer))
#     ttt = ttt + nrow(df_transactions[year==yyy])
# }
# cat("Average unique sellers:", fp(sss/4))
# cat("Average unique buyers:", fp(bbb/4))
# cat("Average yearly transactions:", fp(ttt/4))
# 
# # Print information on density of network
# ddd = 0;
# for (yyy in 2008:2011) {
#     sss = unique(df_transactions[year==yyy]$id_seller) 
#     bbb = unique(df_transactions[year==yyy]$id_buyer)
#     aaa = length(unique(c(sss, bbb)))
#     ttt_max = ( aaa*(aaa-1) ) / 2
#     ttt = nrow(df_transactions[year==yyy])
#     cat("In",yyy,"there were",fp(aaa),"unique agents. Thus, the number of potential connections is",fp(ttt_max)," giving us a density of",ttt/ttt_max,"\n")
#     ddd = ddd + ttt/ttt_max
# }
# cat("Average density:", fp(ddd/4,7))

# # Frequency Intensity by seller [I]
# df_transactions[, fi_i := mean(transaction_volume), by = c("year", "id_seller")]
# df_transactions[, fi_iw := weighted.mean(transaction_volume, w = transaction_value),  by = c("year", "id_seller")] # weighted mean
# # Value Intensity by seller [I]
# df_transactions[, vi_i := mean(transaction_value), by = c("year", "id_seller")]
# df_transactions[, vi_iw := weighted.mean(transaction_value, w = transaction_volume),  by = c("year", "id_seller")] # weighted mean

