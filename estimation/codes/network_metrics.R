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
#----                             1 - LOAD DATA                             ----
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


#///////////////////////////////////////////////////////////////////////////////
#----                       2 - CONSTRUCT METRICS                           ----
#///////////////////////////////////////////////////////////////////////////////

# Value/Frequency of purchases per buyer & buyer and seller sector
df_transactions[, v_sec_buy := sum(transaction_value),  by = c("year", "id_buyer", "seller_sec")]
df_transactions[, v_all_buy := sum(transaction_value),  by = c("year", "id_buyer")]
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
potential <- dcast(data = unique(df_transactions[, .(year, seller_sec, id_buyer)]),
                   formula = year + seller_sec ~.,
                   fun = length, 
                   value.var="id_buyer")
setnames(potential, ".", "pot_n_buyers")
df_transactions <- merge(df_transactions, potential, by = c("year", "seller_sec"), all.x = T)
rm(potential)

# Take averages by seller [I] (conditional, conditional (weighted), and unconditional) - Note that unconditional weighted cannot exist, because all v_wsi_ij=0 would have weight 0, it just reverts to conditional weighted
df_transactions[, wsvi_i_c := sum(wsvi_ij)/act_n_buyers,                             by = c("year", "id_seller")] # conditional on selling (actual buyers only) - unweighted
df_transactions[, wsvi_i_w := sum(wsvi_ij*transaction_value)/sum(transaction_value), by = c("year", "id_seller")] # conditional on selling (actual buyers only) - weighted
df_transactions[, wsvi_i_u := sum(wsvi_ij)/pot_n_buyers,                             by = c("year", "id_seller")] # unconditional (all potential buyers) - unweighted

df_transactions[, bvi_i_c := sum(bvi_ij)/act_n_buyers,                             by = c("year", "id_seller")] # conditional on selling (actual buyers only) - unweighted
df_transactions[, bvi_i_w := sum(bvi_ij*transaction_value)/sum(transaction_value), by = c("year", "id_seller")] # conditional on selling (actual buyers only) - weighted
df_transactions[, bvi_i_u := sum(bvi_ij)/pot_n_buyers,                             by = c("year", "id_seller")] # unconditional (all potential buyers) - unweighted

df_transactions[, wsfi_i_c := sum(wsfi_ij)/act_n_buyers,                               by = c("year", "id_seller")] # conditional on selling (actual buyers only) - unweighted
df_transactions[, wsfi_i_w := sum(wsfi_ij*transaction_volume)/sum(transaction_volume), by = c("year", "id_seller")] # conditional on selling (actual buyers only) - weighted
df_transactions[, wsfi_i_u := sum(wsfi_ij)/pot_n_buyers,                               by = c("year", "id_seller")] # unconditional (all potential buyers) - unweighted

df_transactions[, bfi_i_c := sum(bfi_ij)/act_n_buyers,                               by = c("year", "id_seller")] # conditional on selling (actual buyers only) - unweighted
df_transactions[, bfi_i_w := sum(bfi_ij*transaction_volume)/sum(transaction_volume), by = c("year", "id_seller")] # conditional on selling (actual buyers only) - weighted
df_transactions[, bfi_i_u := sum(bfi_ij)/pot_n_buyers,                               by = c("year", "id_seller")] # unconditional (all potential buyers) - unweighted

# Seller Reciprocity (first match transactions with their opposite direction)
reciprocity <- df_transactions[, .(year, id_seller, id_buyer, transaction_value)]
reciprocity_reversed <- reciprocity[, .(year, id_seller=id_buyer, id_buyer=id_seller, transaction_value)]
reciprocity <- merge(reciprocity, reciprocity_reversed, by=c("year", "id_seller", "id_buyer"), all.x = T)
reciprocity[, bilateral_transaction := transaction_value.x + transaction_value.y] # will be NA if not bilateral (i.e. if transaction_value.y is NA)
rm(reciprocity_reversed)
reciprocity[, sr_ij := ifelse(is.na(bilateral_transaction), 0, 1)]
df_transactions <- merge(df_transactions, reciprocity[, .(year, id_seller, id_buyer, sr_ij)], by=c("year", "id_seller", "id_buyer"))

df_transactions[, sr_i_c := sum(sr_ij)/act_n_buyers,                             by = c("year", "id_seller")] # conditional on selling (actual buyers only) - unweighted
df_transactions[, sr_i_w := sum(sr_ij*transaction_value)/sum(transaction_value), by = c("year", "id_seller")] # conditional on selling (actual buyers only) - weighted
df_transactions[, sr_i_u := sum(sr_ij)/pot_n_buyers,                             by = c("year", "id_seller")] # unconditional (all potential buyers) - unweighted

# Count average number of transactions per buyer (relation intensity)
df_transactions[, ri_i_c := sum(transaction_volume)/act_n_buyers,                             by = c("year", "id_seller")] # conditional on selling (actual buyers only) - unweighted
df_transactions[, ri_i_w := sum(transaction_volume*transaction_value)/sum(transaction_value), by = c("year", "id_seller")] # conditional on selling (actual buyers only) - weighted
df_transactions[, ri_i_u := sum(transaction_volume)/pot_n_buyers,                             by = c("year", "id_seller")] # unconditional (all potential buyers) - unweighted

# Compute average number of competitors
df_transactions[, act_n_sellers := length(id_seller), by = c("year", "id_buyer", "seller_sec")]
df_transactions[, pot_n_sellers := length(id_seller), by = c("year", "seller_sec")] # count all sellers in your sector

# Count how many times you are the unique seller in your sector
df_transactions[, us_i_c := sum( act_n_sellers==1)/act_n_buyers,                              by = c("year", "id_seller")] # conditional on selling (actual buyers only) - unweighted => essentially share of buyers for which you are the unique seller in your sector
df_transactions[, us_i_w := sum((act_n_sellers==1)*transaction_value)/sum(transaction_value), by = c("year", "id_seller")] # conditional on selling (actual buyers only) - weighted
df_transactions[, us_i_u := sum( act_n_sellers==1)/pot_n_buyers,                              by = c("year", "id_seller")] # unconditional (all potential buyers) - unweighted

# Define competition intensity
df_transactions[, n_sellers := act_n_sellers/pot_n_sellers] # this will limit the value between 0 and 1
df_transactions[, ci_i_c := sum(n_sellers)/act_n_buyers,                     by = c("year", "id_seller")] # competition intensity conditional
df_transactions[, ci_i_w := sum(n_sellers*transaction_value)/act_n_buyers,,  by = c("year", "id_seller")] # competition intensity weighted
df_transactions[, ci_i_u := sum(n_sellers)/pot_n_buyers,                     by = c("year", "id_seller")] # competition intensity conditional
# ci_i_u is not correctly define, should somehow consider competition also for buyers with which I dont interact

# Check how many time you often you sell to NOT your own province (i.e. NOT local orientation)
df_transactions[, local := seller_province!=buyer_province]
df_transactions[, lo_i_c := sum( local==1)/act_n_buyers,                              by = c("year", "id_seller")] # conditional on selling (actual buyers only) - unweighted => essentially share of buyers that are in your own province
df_transactions[, lo_i_w := sum((local==1)*transaction_value)/sum(transaction_value), by = c("year", "id_seller")] # conditional on selling (actual buyers only) - weighted

# Check how many time you often you sell to NOT your own sector (i.e. NOT horizontal orientation)
df_transactions[, horizontal := seller_sec!=buyer_sec]
df_transactions[, ho_i_c := sum( horizontal==1)/act_n_buyers,                              by = c("year", "id_seller")] # conditional on selling (actual buyers only) - unweighted => essentially share of buyers that are in your own sector
df_transactions[, ho_i_w := sum((horizontal==1)*transaction_value)/sum(transaction_value), by = c("year", "id_seller")] # conditional on selling (actual buyers only) - weighted

# Count number of sectors you serve (keep absolute so that interpretation is easy)
buyers_sec <- dcast(data = unique(df_transactions[, .(year, buyer_sec, id_seller)]),
                    formula = year + id_seller ~.,
                    fun = length, 
                    value.var="buyer_sec")
setnames(buyers_sec, ".", "act_n_sectors")
df_transactions <- merge(df_transactions, buyers_sec, by = c("year", "id_seller"), all.x = T)
rm(buyers_sec)

# Count to how many different provinces a sellers sells to
buyers_prov <- dcast(data = unique(df_transactions[, .(year, buyer_province, id_seller)]),
                     formula = year + id_seller ~.,
                     fun = length, 
                     value.var="buyer_province")
setnames(buyers_prov, ".", "act_n_provinces")
df_transactions <- merge(df_transactions, buyers_prov, by = c("year", "id_seller"), all.x = T)
rm(buyers_prov)

# # INTERESTING CONCEPT, COME BACK TO IT
#
# Note that after a point the vectors are extremely high correlated (especially at the top)
#
# > cor(KC10$kc_i,KC3$kc_i, method="spearman")
# [1] 0.9999999
# > cor(KC10$kc_i,KC2$kc_i, method="spearman")
# [1] 0.9999972
# > cor(KC10$kc_i,KC$kc_i, method="spearman")
# [1] 0.9998437
# 
# # Create matrix of transactions Z (sellers on rows, buyers on columns)
#     id_firm <- sort(unique(c(df_transactions[year==2008]$id_seller, df_transactions[year==2008]$id_buyer)))
#     i <- match(df_transactions[year==2008]$id_seller, id_firm)
#     j <- match(df_transactions[year==2008]$id_buyer,  id_firm)
#     Z <- sparseMatrix(i    = i,
#                       j    = j,
#                       x    = df_transactions[year==2008]$transaction_value,
#                       dims = c(length(id_firm),
#                                length(id_firm)))
# 
#     # Create adjacency matrix A by creating cost shares
#     cost <- colSums(Z)
#     diag_inv_C <- .sparseDiagonal(n = length(cost),
#                                   x = ifelse(cost==0, 0, 1/cost))
#     A <- Z %*% diag_inv_C
#     
#     # Vector of initial VSWI_i
#     vector = data.table(id_seller = id_firm)
#     vector <- merge(vector, unique(df_transactions[year==2008, .(id_seller, wsvi_i_c)]), all.x = T)
#     vector[ is.na(wsvi_i_c), wsvi_i_c := 0]
# 
#     # Katz Centrality
#     alpha = 1 # attenuation factor
#     d = 10 # number of steps to approximate inverse (too much memory required)
#     KC10 <- data.table(year      = 2008,
#                      id_seller = id_firm,
#                      kc_i      = as.vector(leontief_degree(l = alpha*A,
#                                                            m = vector$wsvi_i_c,
#                                                            degree = 10*d)))


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
# df_transactions <- merge(df_transactions, KC_dt, by=c("year", "id_seller"), all.x=T)




#///////////////////////////////////////////////////////////////////////////////
#----               3 - COLLECT SELLER-LEVEL METRICS AND SAVE               ----
#///////////////////////////////////////////////////////////////////////////////

# Combine all metrics in a single table with all unique sellers
df_transactions[, seller_weight := sum(transaction_value), by=c("year", "id_seller")]
network_metrics <- unique(df_transactions[, .(year, id_seller, seller_sec, seller_weight,
                                              act_n_buyers, pot_n_buyers, act_n_sectors, act_n_provinces,
                                              wsvi_i_c, wsvi_i_w, wsvi_i_u, 
                                              bvi_i_c,  bvi_i_w,  bvi_i_u, 
                                              wsfi_i_c, wsfi_i_w, wsfi_i_u, 
                                              bfi_i_c,  bfi_i_w,  bfi_i_u, 
                                              us_i_c,   us_i_w,   us_i_u,
                                              sr_i_c,   sr_i_w,   sr_i_u,
                                              ri_i_c,   ri_i_w,   ri_i_u,
                                              ci_i_c,   ci_i_w,   ci_i_u,
                                              lo_i_c,   lo_i_w,
                                              ho_i_c,   ho_i_w)])
setorder(network_metrics, year, id_seller)

# Store file containing network metrics
save(network_metrics, file = paste0(pathEst,'output/network_metrics.Rdata'))
