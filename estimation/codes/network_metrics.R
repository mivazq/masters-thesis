#///////////////////////////////////////////////////////////////////////////////
# File name:		
# Author:			Miguel Vázquez Vázquez
# Creation date:    21 February 2023
# Description:      This file estimates markups for all firms, year to year.
# Input:            
#                   -

# Output:           
#                   -
#///////////////////////////////////////////////////////////////////////////////
source('~/data/transactions_ecuador/3_mivazq/Masters_Thesis/setup.R')
#///////////////////////////////////////////////////////////////////////////////
#----                       1 - LOAD AND PREPARE DATA                       ----
#///////////////////////////////////////////////////////////////////////////////

# Load transactions and firm information
df_transactions <- fread(file=paste0(pathCle, "output/intermediate_transactions.csv"), 
                         na.strings="", colClasses = c(transaction_value = "double"))
df_firm_info    <- fread(file=paste0(pathCle, "output/firm_info.csv"), na.strings="")

# Merge firm sectors
df_transactions <- merge.data.table(df_transactions, df_firm_info[,.(id_sri, buyer_sec  = isic_division)], by.x="id_buyer",  by.y="id_sri", all.x=T)
df_transactions <- merge.data.table(df_transactions, df_firm_info[,.(id_sri, seller_sec = isic_division)], by.x="id_seller", by.y="id_sri", all.x=T)

# Create dataset with unique sellers
network_sample  <- unique(df_transactions[, .(year, id_seller, seller_sec)])

# Purchases per buyer & buyer and seller sector
df_transactions[, sec_buy := sum(transaction_value), by = c("year", "id_buyer", "seller_sec")]
df_transactions[is.na(seller_sec), sec_buy := NA]
df_transactions[, all_buy := sum(transaction_value), by = c("year", "id_buyer")]

# Within Sector Importance, Broad Sector Importance, Total Importance [I,J]
df_transactions[, wsi_ij := transaction_value/sec_buy]
df_transactions[, bsi_kj := sec_buy/all_buy]
df_transactions[, ti_ij  := wsi_ij*bsi_kj]

# Take averages by seller [I]
df_transactions[, wsi_i  := mean(wsi_ij), by = c("year", "id_seller")]
df_transactions[, wsi_iw := sum(wsi_ij),  by = c("year", "id_seller")] # weighted sum
df_transactions[, ti_i   := mean(ti_ij),  by = c("year", "id_seller")]
df_transactions[, ti_iw  := sum(ti_i),    by = c("year", "id_seller")] # weighted sum

# (Corrected) Transaction Volume
df_transactions[, tv_i := sum(transaction_volume), by = c("year", "id_seller")]
df_transactions[, tv_k := mean(tv_i), by = c("year", "seller_sec")]
df_transactions[is.na(seller_sec), tv_k := NA]
df_transactions[, ctv_i := tv_i/tv_k]

# Match transactions with their opposite direction
reciprocity <- df_transactions[, .(year, id_seller, id_buyer, transaction_value)]
reciprocity_reversed <- reciprocity[, .(year, id_seller=id_buyer, id_buyer=id_seller, transaction_value)]
reciprocity <- merge(reciprocity, reciprocity_reversed, by=c("year", "id_seller", "id_buyer"), all.x = T)
rm(reciprocity_reversed)

# Seller Reciprocity
reciprocity[, sr_ij := ifelse(is.na(transaction_value.y), 0, 1)] # check if bilateral relationship
reciprocity[, sr_i  := mean(sr_ij), by = c("year", "id_seller")]
reciprocity[, sr_iw := sum(sr_ij),  by = c("year", "id_seller")] # weighted sum

# Iterate over years to construct adjacency matrices
KC_dt <- data.table()
for (yyy in 2008:2011) {
    
    # Create matrix of transactions Z (sellers on rows, buyers on columns)
    id_firm <- sort(unique(c(df_transactions[year==yyy]$id_seller, df_transactions[year==yyy]$id_buyer)))
    i <- match(df_transactions[year==yyy]$id_seller, id_firm)
    j <- match(df_transactions[year==yyy]$id_buyer,  id_firm)
    Z <- sparseMatrix(i    = i,
                      j    = j,
                      x    = df_transactions[year==yyy]$transaction_value,
                      dims = c(length(id_firm),
                               length(id_firm)))

    # Create adjacency matrix A by creating cost shares
    cost <- colSums(Z)
    diag_inv_C <- .sparseDiagonal(n = length(cost),
                                  x = ifelse(cost==0, 0, 1/cost))
    A <- Z %*% diag_inv_C
    
    # Katz Centrality
    n = dim(A)[1] # number of sellers
    alpha = 0.5 # attenuation factor
    d = 50 # number of steps to approximate inverse (too much memory required)
    KC <- data.table(year      = yyy,
                     id_seller = id_firm,
                     KC_i      = as.vector(leontief_degree(l = alpha*A, 
                                                           m = rep(1, n),
                                                           degree = d)))
    KC_dt <- rbind(KC_dt, KC)
}

# Combine all metrics in a single table with all sellers
network_sample <- merge(network_sample, 
                        unique(df_transactions[, .(year, id_seller, wsi_i, wsi_iw, ti_i, ti_iw, tv_i, ctv_i)]), 
                        by=c("year", "id_seller"), all.x=T)
network_sample <- merge(network_sample, 
                        unique(reciprocity[, .(year, id_seller, sr_i, sr_iw)]), 
                        by=c("year", "id_seller"), all.x=T)
network_sample <- merge(network_sample, 
                        unique(KC_dt[, .(year, id_seller, KC_i)]), 
                        by=c("year", "id_seller"), all.x=T)

for (element in KC_list) {
    
}
network_sample[is.na(year), KC_i := NA]

# store file containing network metrics
network_metrics <- filtered
save(network_metrics, file = paste0(pathEst,'output/network_metrics.Rdata'))


# trans_2008 <- df_transactions[year==2008]
# trans_2008[, sec_buy := sum(transaction_value), by = c("id_buyer", "seller_sec")]
# trans_2008[is.na(seller_sec), sec_buy := NA]
# trans_2008[, all_buy := sum(transaction_value), by = c("id_buyer")]
# trans_2008[, wsi_ij := transaction_value/sec_buy]
# trans_2008[, bsi_kj := sec_buy/all_buy]
# trans_2008[, ti_ij  := wsi_ij*bsi_kj]
# 
# # averages
# trans_2008[, wsi_i := mean(wsi_ij), by = c("id_seller")]
# trans_2008[, wsi_iw := sum(wsi_ij), by = c("id_seller")]
# trans_2008[, ti_i  := mean(ti_ij), by = c("id_seller")]
# trans_2008[, ti_iw := sum(ti_i), by = c("id_seller")]

# filter for IDs we have markups for
# filtered <- merge(panel[year==2008, .(id_sri, isic_division)], unique(trans_2008[, .(id_seller, year, wsi_i, wsi_iw, ti_i, ti_iw)]), by.x="id_sri", by.y="id_seller", all.x=T)
# hist(filtered$wsi_i)
# hist(filtered$wsi_iw)
# hist(filtered$ti_i)
# hist(filtered$ti_iw)

# # reciprocity
# reciprocity <- df_transactions[year==2008, .(id_seller, id_buyer, transaction_value)]
# reciprocity_reversed <- reciprocity[, .(id_seller=id_buyer, id_buyer=id_seller, transaction_value)]
# reciprocity <- merge(reciprocity, reciprocity_reversed, by=c("id_seller", "id_buyer"), all.x = T)
# reciprocity[, sr_ij := ifelse(is.na(transaction_value.y), 0, 1)]
# reciprocity[, c("transaction_value.x", "transaction_value.y") := NULL]
# rm(reciprocity_reversed)
# reciprocity[, sr_i := mean(sr_ij), by="id_seller"]
# reciprocity[, sr_iw := sum(sr_ij), by="id_seller"]

# # merge to filtered
# filtered <- merge(filtered, unique(reciprocity[, .(id_seller, sr_i, sr_iw)]), by.x="id_sri", by.y="id_seller", all.x=T)
# hist(filtered$sr_i)
# hist(filtered$sr_iw)

# # transaction volume
# trans_2008[, tv_i := sum(transaction_volume), by = c("id_seller")]
# trans_2008[, tv_k := mean(tv_i), by = c("seller_sec")]
# trans_2008[, ctv_i := tv_i/tv_k]
# 
# # merge to filtered
# filtered <- merge(filtered, unique(trans_2008[, .(id_seller, tv_i, ctv_i)]), by.x="id_sri", by.y="id_seller", all.x=T)
# hist(filtered$tv_i)
# hist(filtered$ctv_i)


# # Katz Centrality
# id_firm <- sort(unique(c(df_transactions[year==2008]$id_seller, df_transactions[year==2008]$id_buyer)))
# i <- match(df_transactions[year==2008]$id_seller, id_firm)
# j <- match(df_transactions[year==2008]$id_buyer,  id_firm)
# Z <- sparseMatrix(i    = i,
#                   j    = j,
#                   x    = df_transactions[year==2008]$transaction_value,
#                   dims = c(length(id_firm),
#                            length(id_firm)))
# rm(i,j)
# cost <- colSums(Z)
# diag_inv_C <- .sparseDiagonal(n = length(cost),
#                               x = ifelse(cost==0, 0, 1/cost))
# A <- Z %*% diag_inv_C
# n = dim(A)[1]
# KC_dt <- data.table(id_seller = id_firm,
#                     KC_i      = as.vector(leontief_degree(l = 0.5*A, 
#                                                           m = rep(1, n),
#                                                           degree = 50)))


# # merge to filtered
# filtered <- merge(filtered, unique(KC_dt[, .(id_seller, KC_i)]), by.x="id_sri", by.y="id_seller", all.x=T)
# filtered[is.na(year), KC_i := NA]
# hist(filtered$KC_i)

# filtered[!is.na(KC_i), rank := rank(-KC_i)]
# filtered[!is.na(KC_i), logKC := log(KC_i)]
# filtered[!is.na(KC_i), logrank := log(rank)]
# ggplot(filtered[!is.na(KC_i)], aes(x = logKC, y =logrank)) + geom_point() + geom_smooth(method='lm', formula= y~x)


# # store file containing network metrics
# network_metrics <- filtered
# save(network_metrics, file = paste0(pathEst,'output/network_metrics.Rdata'))



