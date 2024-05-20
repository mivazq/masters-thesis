#///////////////////////////////////////////////////////////////////////////////
# File name:		markup_estimation_M_L.R
# Author:			Miguel Vázquez Vázquez
# Creation date:    10 January 2023
# Description:      This file estimates markups for all firms, year to year. The
#                   estimation is performed on an unbalanced panel of firms.
#
# The estimation routine combines two stages, all repeated for each industry separately.
#
# 1) In the first we run the observed output y against an expected output thought 
# to be "calculated" by the firm based on a polynomial function Phi (Ф) of inputs, 
# known productivity and potentially other firm characteristics: Ф(l,k,m,z). The
# residuals of this regression are epsilon (ϵ) and include both measurement error
# in observed output as well as unexpected shocks to production. It is important
# to retrieve ϵ because to get the right mark-ups we need to consider the expected 
# output that the firm decided to produce and not the effectively realised one.
#
# 2) In the second stage we estimate unobserved total factor productivity (TFP) 
# by relying on the law of motion of log productivity (ω): ω_t = g(w_t-1) + ξ
# where we can use additional lags in the law of motion and ξ represents the 
# innovation to productivity, i.e. the changes to productivity due to decisions 
# made by the firm in the *current* period. We do this by using generalized method 
# of moments (GMM). In particular, we can compute log TFP (ω) by subtracting from
# the estimated expected output Ф the input usages times their corresponding 
# output-elasticities. With GMM we can optimise these output-elasticities (β) such
# that we match moments that we assume to be true and innovation to productivity (ξ)
# does not correlate with variables that are chosen one period ahead. The functions
# are stored in a separate file DLW.R and loaded with the setup.R file.
#
# Input:            
#                   -

# Output:           
#                   -
#///////////////////////////////////////////////////////////////////////////////
source('~/data/transactions_ecuador/3_mivazq/Masters_Thesis/setup.R')
#///////////////////////////////////////////////////////////////////////////////
#----                       1 - LOAD AND PREPARE DATA                       ----
#///////////////////////////////////////////////////////////////////////////////

# Load data
df_tax_filings <- fread(file=paste0(pathCle, "output/tax_filings.csv"), na.strings="")
df_firm_info   <- fread(file=paste0(pathCle, "output/firm_info.csv"), na.strings="")
df_deflators   <- fread(file=paste0(pathCle, "output/deflators.csv"), na.strings="")
df_def_assets  <- fread(file=paste0(pathCle, "output/assets_deflators.csv"), na.strings="")
df_industries  <- fread(file=paste0(pathCle, "output/isic_codes_division.csv"), na.strings="")

# Merge tax filings panel with firm industry information
panel <- merge(df_tax_filings, df_firm_info, by="id_sri")

# Merge price deflators into panel (try to match on class, if not on group)
# Due to how the deflators file is constructed even if a specific group would 
# have no deflator it inherits from its division or section. Thus, no need to 
# merge further than group.
panel <- merge(panel, df_deflators[,.(year,code,def1=deflator)], 
               by.x=c("year","isic_class"), by.y=c("year","code"), all.x=T)
panel <- merge(panel, df_deflators[,.(year,code,def2=deflator)], 
               by.x=c("year","isic_group"), by.y=c("year","code"), all.x=T)
panel[, deflator := ifelse(!is.na(def1), def1, def2)]
panel[, c("def1","def2") := NULL]

# Merge assets deflators
panel <- merge(panel, df_def_assets[, .(year, capital_deflator = value)], by="year", all.x=T)

# Deflate assets and cost of capital using 
panel[, c("fixed_assets", "capital_costs") := lapply(.SD, function(x) x/panel[["capital_deflator"]]*100), .SDcols = c("fixed_assets", "capital_costs")]

# Deflate other monetary variables
panel[, c("operative_revenues", "material_costs", "labour_costs") := lapply(.SD, function(x) x/panel[["deflator"]]*100), .SDcols = c("operative_revenues", "material_costs", "labour_costs")]


#///////////////////////////////////////////////////////////////////////////////
#----       2 - OUTPUT OBSERVATIONS TABLE AND EXCLUDE SMALL INDUSTRIES      ----
#///////////////////////////////////////////////////////////////////////////////

# Sort data table and create selectors for 1st and 2nd stage
setorder(panel, "id_sri", "year")
panel[, active_lag := shift(active, type="lag"), by = id_sri]
panel[is.na(active_lag), active_lag := 0]
use_1st <- panel$active==1 # Active in current period
use_2nd <- panel$active==1 & panel$active_lag==1 # Active in current and previous periods

# Table with number of observations/firms per industry used for 1st and 2nd stage
table_sample_nobs <- df_industries[, .(desc=isic_division_desc, ind=isic_division)]
table_sample_nobs[, obs_1st_stage := as.vector(table(factor(       panel[use_1st                         ]$isic_division, levels = table_sample_nobs$ind)))]
table_sample_nobs[, ids_1st_stage := as.vector(table(factor(unique(panel[use_1st,.(id_sri,isic_division)])$isic_division, levels = table_sample_nobs$ind)))]
table_sample_nobs[, obs_2nd_stage := as.vector(table(factor(       panel[use_2nd                         ]$isic_division, levels = table_sample_nobs$ind)))]
table_sample_nobs[, ids_2nd_stage := as.vector(table(factor(unique(panel[use_2nd,.(id_sri,isic_division)])$isic_division, levels = table_sample_nobs$ind)))]
setorder(table_sample_nobs, "ind")
write_xlsx(table_sample_nobs, paste0(pathEst, "output/sample_by_industry_pre_exclusion.xlsx"))

# Exclude too small industries (<100 observations for 1st stage) and output new table
exclusions <- table_sample_nobs[obs_1st_stage<100]$ind
table_sample_nobs <- table_sample_nobs[ind %nin% exclusions]
write_xlsx(table_sample_nobs, paste0(pathEst, "output/sample_by_industry_post_exclusion.xlsx"))

# Perform exclusions in the panel and 1st/2nd stage selectors
use_1st <- use_1st[panel$isic_division %nin% exclusions]
use_2nd <- use_2nd[panel$isic_division %nin% exclusions]
panel   <- panel[  panel$isic_division %nin% exclusions]

# Store vector containing kept industries
industries <- sort(unique(panel$isic_division))


#///////////////////////////////////////////////////////////////////////////////
#----                 3 - FIRST AND SECOND STAGE ESTIMATIONS                ----
#///////////////////////////////////////////////////////////////////////////////

# Create estimation data table
dt_est <- panel[, .(id = id_sri, 
                    year = year, 
                    ind = isic_division,
                    cons = 1,
                    y = log(operative_revenues), 
                    k = log(fixed_assets),
                    m = log(material_costs), 
                    l = log(labour_costs))]

# Create new variables needed for Translog production function
dt_est[, `:=` (m2  = m^2,
               l2  = l^2,
               k2  = k^2,
               mk  = m * k,
               lk  = l * k,
               ml  = m * l,
               mlk = m * l * k)]

# Create table to store OLS estimates by industry which will be used as initial values
# for GMM.
dt_init <- data.table(ind = rep(industries,2),
                      pf  = c(rep("CD",length(industries)), rep("TL",length(industries))))

# Iterate over industries 
for (industry in industries) {
    
    # Logical vector for industry selection
    ind_sel <- dt_est$ind==industry
    
    message("ESTIMATING FIRST STAGE FOR DIVISION ", industry)
    
    ### FIRST STAGE
        # Generate polynomial terms on inputs variables for Phi_hat estimation (prefix: "fs")
        degree <- 3 # degree of polynomial expansion
        for (iter1 in 1:degree) {
            dt_est[, paste0("fs_", "m", iter1) := m^iter1]
            dt_est[, paste0("fs_", "l", iter1) := l^iter1]
            dt_est[, paste0("fs_", "k", iter1) := k^iter1]
            
            for (iter2 in 1:degree) {
                dt_est[, paste0("fs_", "m", iter1, "k", iter2) := m^iter1 * k^iter2]
                dt_est[, paste0("fs_", "l", iter1, "k", iter2) := l^iter1 * k^iter2]
                dt_est[, paste0("fs_", "m", iter1, "l", iter2) := m^iter1 * l^iter2]
            }
            
            dt_est[, paste0("fs_", "m", iter1, "l", iter1, "k", iter1) := m^iter1 * l^iter1 * k^iter1]
        }
        rm(iter1, iter2, degree) # remove useless values
        fs_vars <- colnames(dt_est)[grep("^fs_", colnames(dt_est))] # list all "fs_" vars
        
        # Run first-stage estimation
        frml <- paste("y ~",paste(fs_vars, collapse = " + "),"| year") # formula
        fs_model <- feols(as.formula(frml), data = dt_est, subset = use_1st & ind_sel, panel.id=c("id","year"))
        
        # Store estimates of expected output (phi_hat) and measurement error (epsilon_hat) 
        dt_est[use_1st & ind_sel, `:=` (Phi_hat     = predict(fs_model), 
                                        epsilon_hat = residuals(fs_model))]
        dt_est[, Phi_hat_lag := shift(Phi_hat, type="lag"), by = id]
        
        # Drop first stage variables (no longer needed)
        dt_est[, (fs_vars) := NULL]
        rm(fs_vars, frml)
    
    
    message("ESTIMATING SECOND STAGE FOR DIVISION ", industry)
    
    ### SECOND STAGE
        # Create new variables needed for productivity estimation
        dt_est[, `:=` (m_lag = shift(m, 1),
                       l_lag = shift(l, 1),
                       k_lag = shift(k, 1)), 
               by = id]
        dt_est[, `:=` (m_lag2          = m_lag^2,
                       l_lag2          = l_lag^2,
                       k_lag2          = k_lag^2,
                       m_lagk_lag      = m_lag * k_lag,
                       l_lagk_lag      = l_lag * k_lag,
                       m_lagl_lag      = m_lag * l_lag,
                       m_lagl_lagk_lag = m_lag * l_lag * k_lag,
                       m_lagk          = m_lag * k,
                       l_lagk          = l_lag * k,
                       m_lagl          = m_lag * l,            # new (for L as dynamic)
                       m_laglk         = m_lag * l * k,        # new (for L as dynamic)
                       m_lagl_lagk     = m_lag * l_lag * k)]
        
        # OLS estimates on Cobb Douglas production function
        olscd_model <- feols(y ~ m + l + k | year, 
                             data = dt_est, subset = use_2nd & ind_sel, panel.id=c("id","year"))
        dt_est[ind_sel, olscd := list(olscd_model$coefficients)]
        dt_init[ind==industry & pf=="CD", names(as.list(olscd_model$coefficients)) := as.list(olscd_model$coefficients)]
        
        # OLS estimates on Translog production function
        olstl_model <- feols(y ~ m + l + k + m2 + l2 + k2 + mk + lk + ml + mlk | year, 
                             data = dt_est, subset = use_2nd & ind_sel, panel.id=c("id","year"))
        dt_est[ind_sel, olstl := list(olstl_model$coefficients)]
        dt_init[ind==industry & pf=="TL", names(as.list(olstl_model$coefficients)) := as.list(olstl_model$coefficients)]
        
        # ACF estimates using DLW method on Cobb Douglas production function
        dt_est[ind_sel, dlwcd := list(DLW_CD_ML(init_par = c(1, dt_init[ind==industry & pf=="CD"]$m, dt_init[ind==industry & pf=="CD"]$l, dt_init[ind==industry & pf=="CD"]$k)))]

        # ACF estimates using DLW method on Translog production function
        dt_est[ind_sel, dlwtl := list(DLW_TL_ML(init_par = c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)))]
        
    message("\n")
}

rm(fs_model, olscd_model, olstl_model, ind_sel, industry)


#///////////////////////////////////////////////////////////////////////////////
#----               4 - COMPUTE MARK-UPS AND PRODUCTIVITIES                 ----
#///////////////////////////////////////////////////////////////////////////////

# Extract individual betas from coefficient lists
dt_est[, beta_m_ols_cd := sapply(olscd, function(x) x[["m"]])]
dt_est[, beta_l_ols_cd := sapply(olscd, function(x) x[["l"]])]
dt_est[, beta_k_ols_cd := sapply(olscd, function(x) x[["k"]])]
dt_est[, olscd := NULL]
dt_est[, beta_m_dlw_cd := sapply(dlwcd, function(x) x[["m"]])]
dt_est[, beta_l_dlw_cd := sapply(dlwcd, function(x) x[["l"]])]
dt_est[, beta_k_dlw_cd := sapply(dlwcd, function(x) x[["k"]])]
dt_est[, dlwcd := NULL]
dt_est[, beta_m1_ols_tl := sapply(olstl, function(x) x[["m"]])]
dt_est[, beta_l1_ols_tl := sapply(olstl, function(x) x[["l"]])]
dt_est[, beta_k1_ols_tl := sapply(olstl, function(x) x[["k"]])]
dt_est[, beta_m2_ols_tl := sapply(olstl, function(x) x[["m2"]])]
dt_est[, beta_l2_ols_tl := sapply(olstl, function(x) x[["l2"]])]
dt_est[, beta_k2_ols_tl := sapply(olstl, function(x) x[["k2"]])]
dt_est[, beta_mk_ols_tl := sapply(olstl, function(x) x[["mk"]])]
dt_est[, beta_lk_ols_tl := sapply(olstl, function(x) x[["lk"]])]
dt_est[, beta_ml_ols_tl := sapply(olstl, function(x) x[["ml"]])]
dt_est[, beta_mlk_ols_tl := sapply(olstl, function(x) x[["mlk"]])]
dt_est[, olstl := NULL]
dt_est[, beta_m1_dlw_tl := sapply(dlwtl, function(x) x[["m"]])]
dt_est[, beta_l1_dlw_tl := sapply(dlwtl, function(x) x[["l"]])]
dt_est[, beta_k1_dlw_tl := sapply(dlwtl, function(x) x[["k"]])]
dt_est[, beta_m2_dlw_tl := sapply(dlwtl, function(x) x[["m2"]])]
dt_est[, beta_l2_dlw_tl := sapply(dlwtl, function(x) x[["l2"]])]
dt_est[, beta_k2_dlw_tl := sapply(dlwtl, function(x) x[["k2"]])]
dt_est[, beta_mk_dlw_tl := sapply(dlwtl, function(x) x[["mk"]])]
dt_est[, beta_lk_dlw_tl := sapply(dlwtl, function(x) x[["lk"]])]
dt_est[, beta_ml_dlw_tl := sapply(dlwtl, function(x) x[["ml"]])]
dt_est[, beta_mlk_dlw_tl := sapply(dlwtl, function(x) x[["mlk"]])]
dt_est[, dlwtl := NULL]

# Compute log TFP
dt_est[, tfp_ols_cd := y      - beta_m_ols_cd*m   - beta_l_ols_cd*l   - beta_k_ols_cd*k]
dt_est[, tfp_ols_tl := y      - beta_m1_ols_tl*m  - beta_l1_ols_tl*l  - beta_k1_ols_tl*k 
                              - beta_m2_ols_tl*m2 - beta_l2_ols_tl*l2 - beta_k2_ols_tl*k2
                              - beta_mk_ols_tl*mk - beta_lk_ols_tl*lk - beta_ml_ols_tl*ml - beta_mlk_ols_tl*mlk]
dt_est[, tfp_dlw_cd := Phi_hat - beta_m_dlw_cd*m   - beta_l_dlw_cd*l   - beta_k_dlw_cd*k]
dt_est[, tfp_dlw_tl := Phi_hat - beta_m1_dlw_tl*m  - beta_l1_dlw_tl*l  - beta_k1_dlw_tl*k 
                               - beta_m2_dlw_tl*m2 - beta_l2_dlw_tl*l2 - beta_k2_dlw_tl*k2
                               - beta_mk_dlw_tl*mk - beta_lk_dlw_tl*lk - beta_ml_dlw_tl*ml - beta_mlk_dlw_tl*mlk]

# Compute mark-ups
dt_est[, alpha_m  := exp(m) / exp(Phi_hat)] # corrected share of material inputs (using expected output Ф instead of realised y)
dt_est[, alpha_l  := exp(l) / exp(Phi_hat)] # corrected share of labour inputs (using expected output Ф instead of realised y)
dt_est[, beta_m_ols_tl := (1*beta_m1_ols_tl + 2*m*beta_m2_ols_tl + k*beta_mk_ols_tl + l*beta_ml_ols_tl + l*k*beta_mlk_ols_tl)] # Translog total output elasticity of material inputs
dt_est[, beta_l_ols_tl := (1*beta_l1_ols_tl + 2*l*beta_l2_ols_tl + k*beta_lk_ols_tl + m*beta_ml_ols_tl + m*k*beta_mlk_ols_tl)] # Translog total output elasticity of labour inputs
dt_est[, beta_m_dlw_tl := (1*beta_m1_dlw_tl + 2*m*beta_m2_dlw_tl + k*beta_mk_dlw_tl + l*beta_ml_dlw_tl + l*k*beta_mlk_dlw_tl)] # Translog total output elasticity of material inputs
dt_est[, beta_l_dlw_tl := (1*beta_l1_dlw_tl + 2*l*beta_l2_dlw_tl + k*beta_lk_dlw_tl + m*beta_ml_dlw_tl + m*k*beta_mlk_dlw_tl)] # Translog total output elasticity of labour inputs
dt_est[, mu_m_ols_cd := beta_m_ols_cd / alpha_m]
dt_est[, mu_m_dlw_cd := beta_m_dlw_cd / alpha_m]
dt_est[, mu_m_ols_tl := beta_m_ols_tl / alpha_m]
dt_est[, mu_m_dlw_tl := beta_m_dlw_tl / alpha_m]
dt_est[, mu_l_ols_cd := beta_l_ols_cd / alpha_l]
dt_est[, mu_l_dlw_cd := beta_l_dlw_cd / alpha_l]
dt_est[, mu_l_ols_tl := beta_l_ols_tl / alpha_l]
dt_est[, mu_l_dlw_tl := beta_l_dlw_tl / alpha_l]


#///////////////////////////////////////////////////////////////////////////////
#----               5 - OUTPUT TABLE WITH COMPARED CD ELASTICITIES          ----
#///////////////////////////////////////////////////////////////////////////////

# Compute elasticities based on aggregate FOCs
shares <- dcast(data      = panel[use_2nd],
                formula   = isic_division ~ .,
                fun       = sum,
                na.rm     = T,
                value.var = c("material_costs", "labour_costs", "capital_costs", "operative_revenues"))
shares[, c("material_costs", "labour_costs", "capital_costs") := 
           lapply(.SD, function(x) x/shares[["operative_revenues"]]), .SDcols = 
           c("material_costs", "labour_costs", "capital_costs")]
shares[, operative_revenues := NULL]
setnames(shares, c("isic_division", "material_costs", "labour_costs", "capital_costs"), 
         c("ind", "M_share", "L_share", "K_share"))
setorder(shares, "ind")

# Get final elasticities from DLW method
results <- unique(dt_est[,.(ind, m = beta_m_dlw_cd, l = beta_l_dlw_cd, k = beta_k_dlw_cd)])
setorder(results, "ind")

# Table with elasticities
table_elast_cd <- df_industries[isic_division %in% industries, .(desc=isic_division_desc, ind=isic_division)]
table_elast_cd <- merge(table_elast_cd, shares[, .(ind, FOC_m = M_share, FOC_l = L_share, FOC_k = K_share)], by="ind")
table_elast_cd <- merge(table_elast_cd, dt_init[pf=="CD", .(ind, OLS_m = m, OLS_l = l, OLS_k = k)], by="ind")
table_elast_cd <- merge(table_elast_cd, results[, .(ind, DLW_m = m, DLW_l = l, DLW_k = k)], by="ind")
table_elast_cd <- merge(table_elast_cd, table_sample_nobs[, .(ind, n = obs_2nd_stage)], by="ind")
table_elast_cd <- table_elast_cd[, .(desc, ind, n, FOC_m, OLS_m, DLW_m, FOC_l, OLS_l, DLW_l, FOC_k, OLS_k, DLW_k)]
table_elast_cd <- table_elast_cd[, c("FOC_tot", "OLS_tot", "DLW_tot") := list(FOC_m+FOC_l+FOC_k, OLS_m+OLS_l+OLS_k, DLW_m+DLW_l+DLW_k)]
write_xlsx(table_elast_cd, paste0(pathEst, "output/CD_elasticities_ML_by_industry_new.xlsx"))

# Remove no longer needed objects
rm(dt_init, results, shares)


#///////////////////////////////////////////////////////////////////////////////
#----           6 - OUTPUT TABLE WITH CD AND TL MARKUPS DISTRIBUTIONS       ----
#///////////////////////////////////////////////////////////////////////////////

# Iterate for M and L
for (var in c("M","L")) {
# Iterate for Cobb-Douglas and Translog
for (pf in c("CD", "TL")) {
    ### OLS
    table_dist_mu_ols <- dcast.data.table(data = dt_est[use_1st], 
                                          formula = ind ~.,
                                          fun = list(mean, sd, min, p10, p25, p50, p75, p90, max),
                                          na.rm = T,
                                          value.var = paste0("mu_",tolower(var),"_ols_",tolower(pf)))
    setnames(table_dist_mu_ols, 2:10, c("mean","sd","min","p10","p25","p50","p75","p90","max"))
    table_dist_mu_ols[, est := "OLS"]
    
    ### DLW
    table_dist_mu_dlw <- dcast.data.table(data = dt_est[use_1st], 
                                          formula = ind ~.,
                                          fun = list(mean, sd, min, p10, p25, p50, p75, p90, max),
                                          na.rm = T,
                                          value.var = paste0("mu_",tolower(var),"_dlw_",tolower(pf)))
    setnames(table_dist_mu_dlw, 2:10, c("mean","sd","min","p10","p25","p50","p75","p90","max"))
    table_dist_mu_dlw[, est := "DLW"]
    
    # Combine OLS and DLW, sort, write to Excel
    table_dist_mu <- rbind(table_dist_mu_ols, table_dist_mu_dlw)
    setorder(table_dist_mu, "ind", -"est")
    table_dist_mu <- merge(table_dist_mu, df_industries[, .(desc=isic_division_desc, ind=isic_division)], by="ind", all.x=T)
    table_dist_mu <- merge(table_dist_mu, table_sample_nobs[, .(ind, n=obs_1st_stage)], by="ind")
    table_dist_mu <- table_dist_mu[, .(desc, ind, n, est, mean, sd, min, p10, p25, p50, p75, p90, max)]
    write_xlsx(table_dist_mu, paste0(pathEst, "output/",pf,"_firm_markup_",var,"_distribution_by_industry_new.xlsx"))
}
}

rm(table_dist_mu_ols, table_dist_mu_dlw)


#///////////////////////////////////////////////////////////////////////////////
#----                           7 - EXPORT FIRM MARKUPS                     ----
#///////////////////////////////////////////////////////////////////////////////

markups_ML <- subset(dt_est, select=c("id", "year", "ind", "m", "l", colnames(dt_est)[grep("mu_", colnames(dt_est))]))
markups_ML[, c("M", "L") := list(exp(m), exp(l))]
markups_ML[, c("m", "l") := NULL]
save(markups_ML, file=paste0(pathEst, "output/firm_markups_ML.Rdata"))

# Average output elasticity
paste0("Average firm-level output elasticity for CD prod. and using input M: ", fp(mean(dt_est[!is.na(y)]$beta_m_dlw_cd),3))
paste0("Average firm-level output elasticity for TL prod. and using input M: ", fp(mean(dt_est[!is.na(y)]$beta_m_dlw_tl),3))
paste0("Average firm-level output elasticity for CD prod. and using input L: ", fp(mean(dt_est[!is.na(y)]$beta_l_dlw_cd),3))
paste0("Average firm-level output elasticity for TL prod. and using input L: ", fp(mean(dt_est[!is.na(y)]$beta_l_dlw_tl),3))

# Save data about elasticities (set to missing for inactive)
elast_ML <- subset(dt_est, select=c("id", "year", "ind", "m", "alpha_m", "l", "alpha_l", 
                                   "beta_m_ols_cd", "beta_m_dlw_cd", "beta_m_ols_tl", "beta_m_dlw_tl", 
                                   "beta_l_ols_cd", "beta_l_dlw_cd", "beta_l_ols_tl", "beta_l_dlw_tl"))
elast_ML[is.na(m), c("beta_m_ols_cd", "beta_m_dlw_cd", "beta_l_ols_cd", "beta_l_dlw_cd") := NA] # Due to not using input intensity elasticities were defined for all obs (even inactive)
save(elast_ML, file=paste0(pathEst, "output/firm_elasticities_ML.Rdata"))





