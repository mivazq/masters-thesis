#///////////////////////////////////////////////////////////////////////////////
# File name:		network_metrics.R
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
#----                       1 - CONSTRUCT METRICS                           ----
#///////////////////////////////////////////////////////////////////////////////

# Load transactions and firm information
df_transactions <- fread(file=paste0(pathCle, "output/intermediate_transactions.csv"), 
                         na.strings="", colClasses = c(transaction_value = "double"))
df_firm_info    <- fread(file=paste0(pathCle, "output/firm_info.csv"), na.strings="")
load(file = paste0(pathEst, "output/firm_markups_V.Rdata"))

# Generate new variable
df_firm_info[, geo := paste(region_geo,"-", province)]

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


# # Check how much firms trade across regions
# regional_trade <- dcast(data = df_transactions,
#                         formula = seller_province + buyer_province ~ .,
#                         fun = sum,
#                         value.var=c("transaction_value","transaction_volume"))
# regional_trade[, tot_val := sum(transaction_value), by = "buyer_province"]
# regional_trade[, tot_vol := sum(transaction_volume), by = "buyer_province"]
# regional_trade[, transaction_value := transaction_value/tot_val]
# regional_trade[, transaction_volume := transaction_volume/tot_vol]
# regional_trade[, buyer_province := factor(buyer_province)]
# regional_trade[, seller_province := factor(seller_province, levels=rev(levels(regional_trade$buyer_province)))]
# 
# ggplot(regional_trade, aes(x = buyer_province, y = seller_province)) +
#     geom_tile(aes(fill = transaction_value)) +
#     scale_fill_gradient(low = "white", high = "black") +
#     labs(title = "Transaction Value Across Regions Heatmap (as share of province's total purchases' value)", x = "Province of buyer", y = "Province of seller") +
#     theme(axis.text.x = element_text(angle=90))
# 
# ggplot(regional_trade, aes(x = buyer_province, y = seller_province)) +
#     geom_tile(aes(fill = transaction_volume)) +
#     scale_fill_gradient(low = "white", high = "black") +
#     labs(title = "Transaction Counts Across Regions Heatmap (as share of province's total purchases' count)", x = "Province of buyer", y = "Province of seller") +
#     theme(axis.text.x = element_text(angle=90))
# 
# # How is this picture different for our markup sample?
# regional_trade_mu <- dcast(data = df_transactions[id_seller %in% markups_V$id],
#                            formula = seller_province + buyer_province ~ .,
#                            fun = sum,
#                            value.var=c("transaction_value","transaction_volume"))
# regional_trade_mu <- merge(regional_trade_mu, regional_trade[,.(seller_province,buyer_province)], all.y = T) # ensure all combinations exist
# regional_trade_mu[is.na(transaction_value), c("transaction_value", "transaction_volume") := 0] # assign zeros to previously missing combinations
# regional_trade_mu[, tot_val := sum(transaction_value), by = "buyer_province"]
# regional_trade_mu[, tot_vol := sum(transaction_volume), by = "buyer_province"]
# regional_trade_mu[, transaction_value := transaction_value/tot_val]
# regional_trade_mu[, transaction_volume := transaction_volume/tot_vol]
# regional_trade_mu[, buyer_province := factor(buyer_province)]
# regional_trade_mu[, seller_province := factor(seller_province, levels=rev(levels(regional_trade_mu$buyer_province)))]
# 
# 
# ggplot(regional_trade_mu, aes(x = buyer_province, y = seller_province)) +
#     geom_tile(aes(fill = transaction_value)) +
#     scale_fill_gradient(low = "white", high = "black") +
#     labs(title = "Transaction Value Across Regions Heatmap (as share of province's total purchases' value)", x = "Province of buyer", y = "Province of seller") +
#     theme(axis.text.x = element_text(angle=90))
# 
# ggplot(regional_trade_mu, aes(x = buyer_province, y = seller_province)) +
#     geom_tile(aes(fill = transaction_volume)) +
#     scale_fill_gradient(low = "white", high = "black") +
#     labs(title = "Transaction Counts Across Regions Heatmap (as share of province's total purchases' count)", x = "Province of buyer", y = "Province of seller") +
#     theme(axis.text.x = element_text(angle=90))

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

# df_transactions_all <- copy(df_transactions)
# df_transactions <- df_transactions[year==2008]
# 
# 
# # Create sparse matrices for transaction values and frequencies
# id_firm <- sort(unique(c(df_transactions$id_seller, df_transactions$id_buyer)))
# i <- match(df_transactions$id_seller, id_firm)
# j <- match(df_transactions$id_buyer,  id_firm)
# mat_V <- sparseMatrix(i    = i,
#                       j    = j,
#                       x    = df_transactions$transaction_value,
#                       dims = c(length(id_firm),
#                                length(id_firm)),
#                       dimnames = list(id_firm,id_firm))
# mat_F <- sparseMatrix(i    = i,
#                       j    = j,
#                       x    = df_transactions$transaction_volume,
#                       dims = c(length(id_firm),
#                                length(id_firm)),
#                       dimnames = list(id_firm,id_firm))
# 
# # Create sparse matrices for sector belonging of firms
# id_sectors <- levels(factor(df_firm_info$isic_division)) # 60 because no firm in sectors P96, P97
# df_sectors <- df_firm_info[id_sri %in% id_firm] # by construction all firms will be listed since I excluded missing-sector firms
# i <- match(df_sectors$id_sri, id_firm)
# j <- match(df_sectors$isic_division, id_sectors)
# mat_S <- sparseMatrix(i    = i,
#                       j    = j,
#                       x    = 1,
#                       dims = c(length(id_firm),
#                                length(id_sectors)),
#                       dimnames = list(id_firm,id_sectors))
# 
# 
# dim(mat_V)
# dim(mat_S)
# 
# sales_by_buyer_sector      =   mat_V  %*% mat_S # matrix of sales     by buyer  sector
# purchases_by_seller_sector = t(mat_V) %*% mat_S # matrix of purchases by seller sector
# 
# 
# summary <- df_transactions[, .(
#     var = sum(transaction_value, na.rm = TRUE)
# ), by = .(id_buyer, seller_sec)]
# setorder(summary, id_buyer, seller_sec)
# head(summary)
# purchases_by_seller_sector["1","A01"]
# purchases_by_seller_sector["2","D22"]
# purchases_by_seller_sector["2","D24"]
# purchases_by_seller_sector["2","D32"]
# dim(purchases_by_seller_sector)
# 
# 
# 
# v_wsi_ij = mat_V   transaction_value/v_sec_buy
# 
# 
# # Step 2: Construct a matrix with matching dimensions for division
# # Each column will represent the corresponding sector total for each buyer
# # buyer_sector_total <- mat_S %*% t(purchases_by_seller_sector)  # Total purchases by buyer for each sector
# 
# # Step 1: Calculate purchases by buyer-sector using a loop to avoid large intermediate matrices
# buyer_sector_total <- Matrix(0, nrow = nrow(mat_V), ncol = ncol(mat_V))  # initialize empty sparse matrix
# 
# # Define a chunk size for iteration
# chunk_size <- 1000  # Adjust based on available memory and performance needs
# 
# # Loop through sectors in chunks to calculate buyer_sector_total
# for (i in seq(1, ncol(purchases_by_seller_sector), by = chunk_size)) {
#     # Get the current chunk of sectors
#     chunk <- seq(i, min(i + chunk_size - 1, ncol(purchases_by_seller_sector)))
#     
#     # Update the buyer_sector_total for the current chunk
#     buyer_sector_total <- buyer_sector_total + mat_S[, chunk] %*% t(purchases_by_seller_sector[, chunk])
# }
# 
# 
# # Step 3: Compute v_wsi_ij
# v_wsi_ij <- mat_V / buyer_sector_total  # Division to get the ratio of transaction value to total sector
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# # Compute value and frequency by sector
# v_sec_buy_sparse <- rowSums(values_sparse, na.rm = TRUE)
# f_sec_buy_sparse <- rowSums(volumes_sparse, na.rm = TRUE)
# 
# # Compute value and frequency by buyer
# v_all_buy_sparse <- rep(sum(df_transactions$transaction_value, na.rm = TRUE), length(v_sec_buy_sparse))
# f_all_buy_sparse <- rep(sum(df_transactions$transaction_volume, na.rm = TRUE), length(f_sec_buy_sparse))
# 
# # Calculate derived metrics
# v_wsi_ij <- values_sparse / v_sec_buy_sparse
# v_bsi_kj <- v_sec_buy_sparse / v_all_buy_sparse
# v_bi_ij <- v_wsi_ij * v_bsi_kj
# 
# f_wsi_ij <- volumes_sparse / f_sec_buy_sparse
# f_bsi_kj <- f_sec_buy_sparse / f_all_buy_sparse
# f_bi_ij <- f_wsi_ij * f_bsi_kj
# 
# # Optionally, convert back to data.table or another desired format for further analysis
# # The conversion from sparse matrices to data.table can be done based on the non-zero indices of sparse matrices
# result_data <- data.table(
#     id_buyer = rep(rownames(v_wsi_ij), ncol(v_wsi_ij)),
#     seller_sec = rep(colnames(v_wsi_ij), each = nrow(v_wsi_ij)),
#     v_wsi_ij = as.vector(v_wsi_ij),
#     v_bsi_kj = as.vector(v_bsi_kj),
#     v_bi_ij = as.vector(v_bi_ij),
#     f_wsi_ij = as.vector(f_wsi_ij),
#     f_bsi_kj = as.vector(f_bsi_kj),
#     f_bi_ij = as.vector(f_bi_ij)
# )
# 
# 
# 
# 
# # Let's do per year, start with 2008 as test
# values <- df_transactions[year==2008]
# 
# # Create matrix of transactions Z (sellers on rows, buyers on columns)
# id_firm <- sort(unique(c(values$id_seller, values$id_buyer)))
# i <- match(values$id_seller, id_firm)
# j <- match(values$id_buyer,  id_firm)
# Z <- sparseMatrix(i    = i,
#                   j    = j,
#                   x    = values$transaction_value,
#                   dims = c(length(id_firm),
#                            length(id_firm)))
# 
# # Create matrices for sector identifiers

# Value of purchases per buyer & buyer and seller sector
df_transactions[, v_sec_buy := sum(transaction_value), by = c("year", "id_buyer", "seller_sec")]
df_transactions[, v_all_buy := sum(transaction_value), by = c("year", "id_buyer")]

# Frequency of purchases per buyer & buyer and seller sector
df_transactions[, f_sec_buy := sum(transaction_volume), by = c("year", "id_buyer", "seller_sec")]
df_transactions[, f_all_buy := sum(transaction_volume), by = c("year", "id_buyer")]

# Within Sector Value Importance, Broad Sector Value Importance, Broad Value Importance [I,J]
df_transactions[, wsvi_ij := transaction_value/v_sec_buy]
df_transactions[, bsvi_kj := v_sec_buy/v_all_buy]
df_transactions[, bvi_ij  := wsvi_ij*bsvi_kj]

# Within Sector Frequency Importance, Broad Sector Frequency Importance, Frequency Broad Importance [I,J]
df_transactions[, wsfi_ij := transaction_volume/f_sec_buy]
df_transactions[, bsfi_kj := f_sec_buy/f_all_buy]
df_transactions[, bfi_ij  := wsfi_ij*bsfi_kj]

# Compute actual buyers vs. potential buyers
df_transactions[, act_n_buyers  := length(id_buyer), by = c("year", "id_seller")]
potential <- dcast(data = df_transactions,
                   formula = year + seller_sec ~.,
                   fun = length, 
                   value.var="id_buyer")
setnames(potential, ".", "pot_n_buyers")
df_transactions <- merge(df_transactions, potential, by = c("year", "seller_sec"), all.x = T)

# Take averages by seller [I] (conditional, conditional (weighted), and unconditional) - Note that unconditional weighted cannot exist, because all v_wsi_ij=0 would have weight 0, it just reverts to conditional weighted
df_transactions[, wsvi_i_c := sum(wsvi_ij)/act_n_buyers,                             by = c("year", "id_seller")] # conditional on selling (actual buyers only) - unweighted
df_transactions[, wsvi_i_w := sum(wsvi_ij*transaction_value)/sum(transaction_value), by = c("year", "id_seller")] # conditional on selling (actual buyers only) - weighted
df_transactions[, wsvi_i_u := sum(wsvi_ij)/pot_n_buyers,                             by = c("year", "id_seller")] # unconditional (all potential buyers) - unweighted



df_transactions[, v_bi_i   := mean(v_bi_ij),  by = c("year", "id_seller")]
df_transactions[, v_bi_iw  := weighted.mean(v_bi_ij, w = transaction_value),  by = c("year", "id_seller")] # weighted mean
df_transactions[, f_wsi_i  := mean(f_wsi_ij), by = c("year", "id_seller")]
df_transactions[, f_wsi_iw := weighted.mean(f_wsi_ij, w = transaction_volume), by = c("year", "id_seller")] # weighted mean
df_transactions[, f_bi_i   := mean(f_bi_ij),  by = c("year", "id_seller")]
df_transactions[, f_bi_iw  := weighted.mean(f_bi_ij, w = transaction_volume),  by = c("year", "id_seller")] # weighted mean

# Match transactions with their opposite direction
reciprocity <- df_transactions[, .(year, id_seller, id_buyer, transaction_value)]
reciprocity_reversed <- reciprocity[, .(year, id_seller=id_buyer, id_buyer=id_seller, transaction_value)]
reciprocity <- merge(reciprocity, reciprocity_reversed, by=c("year", "id_seller", "id_buyer"), all.x = T)
reciprocity[, bilateral_transaction := transaction_value.x + transaction_value.y] # will be NA if not bilateral (i.e. if transaction_value.y is NA)
rm(reciprocity_reversed)

# Seller Reciprocity
reciprocity[, sr_ij := ifelse(is.na(bilateral_transaction), 0, 1)]
reciprocity[, sr_i  := mean(sr_ij), by = c("year", "id_seller")]
reciprocity[, sr_iw := weighted.mean(sr_ij, w = transaction_value.x),  by = c("year", "id_seller")] # weighted mean (by sales)

# # Iterate over years to construct adjacency matrices
# KC_dt <- data.table()
# for (yyy in 2008:2011) {
#     
#     # Create matrix of transactions Z (sellers on rows, buyers on columns)
#     id_firm <- sort(unique(c(df_transactions[year==yyy]$id_seller, df_transactions[year==yyy]$id_buyer)))
#     i <- match(df_transactions[year==yyy]$id_seller, id_firm)
#     j <- match(df_transactions[year==yyy]$id_buyer,  id_firm)
#     Z <- sparseMatrix(i    = i,
#                       j    = j,
#                       x    = df_transactions[year==yyy]$transaction_value,
#                       dims = c(length(id_firm),
#                                length(id_firm)))
#     
#     # Create adjacency matrix A by creating cost shares
#     cost <- colSums(Z)
#     diag_inv_C <- .sparseDiagonal(n = length(cost),
#                                   x = ifelse(cost==0, 0, 1/cost))
#     A <- Z %*% diag_inv_C
#     
#     # Katz Centrality
#     n = dim(A)[1] # number of sellers
#     alpha = 0.5 # attenuation factor
#     d = 50 # number of steps to approximate inverse (too much memory required)
#     KC <- data.table(year      = yyy,
#                      id_seller = id_firm,
#                      kc_i      = as.vector(leontief_degree(l = alpha*A, 
#                                                            m = rep(1, n),
#                                                            degree = d)))
#     KC_dt <- rbind(KC_dt, KC)
# }

# Combine all metrics in a single table with all sellers (must have sector info)
network_metrics <- unique(df_transactions[!is.na(seller_sec), .(year, id_seller, seller_sec)])
network_metrics <- merge(network_metrics, 
                         unique(df_transactions[, .(year, id_seller, v_wsi_i, v_wsi_iw, v_bi_i, v_bi_iw, f_wsi_i, f_wsi_iw, f_bi_i, f_bi_iw)]), 
                         by=c("year", "id_seller"), all.x=T)
network_metrics <- merge(network_metrics, 
                         unique(reciprocity[, .(year, id_seller, sr_i, sr_iw)]), 
                         by=c("year", "id_seller"), all.x=T)
network_metrics <- merge(network_metrics, 
                         unique(KC_dt[, .(year, id_seller, kc_i)]), 
                         by=c("year", "id_seller"), all.x=T)

# Store file containing network metrics
save(network_metrics, file = paste0(pathEst,'output/network_metrics.Rdata'))




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

