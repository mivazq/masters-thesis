#///////////////////////////////////////////////////////////////////////////////
# File name:		markup_estimation_V.R
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
df_industries  <- df_industries[isic_division %nin% c("R20", "S25", "T03")]

# Merge tax filings panel with firm industry information immediately excluding
# Ecuador's special sectors (very few observations anyway) and missings
panel <- merge(df_tax_filings, df_firm_info, by="id_sri", all.x=T)
panel <- panel[!is.na(isic_division) & isic_division %nin% c("R20", "S25", "T03")]

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

# Generate variable that sums up material and labour costs := "variable_costs"
panel[, variable_costs := material_costs + labour_costs]


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
# write_xlsx(table_sample_nobs, paste0(pathEst, "output/sample_by_industry_pre_exclusion.xlsx")) # SAME AS FOR M,L

# Exclude too small industries (<50 observations for 2nd stage) and output new table
exclusions <- table_sample_nobs[obs_2nd_stage<50]$ind
table_sample_nobs <- table_sample_nobs[ind %nin% exclusions]
# write_xlsx(table_sample_nobs, paste0(pathEst, "output/sample_by_industry_post_exclusion.xlsx")) # SAME AS FOR M,L

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
                    l = log(labour_costs),
                    v = log(variable_costs))]

# Create new variables needed for Translog production function
dt_est[, `:=` (v2 = v^2,
               k2 = k^2,
               vk = v * k)]

# Create table to store OLS estimates by industry which will be used as initial values
# for GMM.
dt_init <- data.table(ind = rep(industries,2),
                      pf  = c(rep("CD",length(industries)), rep("TL",length(industries))))

# Iterate over industries 
for (industry in industries) {
    
    # Logical vector for industry selection and first stage selection
    ind_sel <- dt_est$ind==industry

    message("ESTIMATING FIRST STAGE FOR DIVISION ", industry)
    
    ### FIRST STAGE
        # Generate polynomial terms on inputs variables for Phi_hat estimation (prefix: "fs")
        degree <- 3 # degree of polynomial expansion
        for (iter1 in 1:degree) {
            dt_est[, paste0("fs_", "v", iter1) := v^iter1]
            dt_est[, paste0("fs_", "k", iter1) := k^iter1]
            
            for (iter2 in 1:degree) {
                dt_est[, paste0("fs_", "v", iter1, "k", iter2) := v^iter1 * k^iter2]
            }
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
        dt_est[, `:=` (v_lag = shift(v, 1),
                       k_lag = shift(k, 1)), 
               by = id]
        dt_est[, `:=` (v_lag2     = v_lag^2,
                       k_lag2     = k_lag^2,
                       v_lagk_lag = v_lag * k_lag,
                       v_lagk     = v_lag * k)]
        
        # OLS estimates on Cobb Douglas production function
        olscd_model <- feols(y ~ v + k | year, 
                             data = dt_est, subset = use_2nd & ind_sel, panel.id=c("id","year"))
        dt_est[ind_sel, olscd := list(olscd_model$coefficients)]
        dt_init[ind==industry & pf=="CD", names(as.list(olscd_model$coefficients)) := as.list(olscd_model$coefficients)]
        
        # OLS estimates on Translog production function
        olstl_model <- feols(y ~ v + k + v2 + k2 + vk | year, 
                             data = dt_est, subset = use_2nd & ind_sel, panel.id=c("id","year"))
        dt_est[ind_sel, olstl := list(olstl_model$coefficients)]
        dt_init[ind==industry & pf=="TL", names(as.list(olstl_model$coefficients)) := as.list(olstl_model$coefficients)]

        # ACF estimates using DLW method on Cobb Douglas production function
        dt_est[ind_sel, dlwcd := list(DLW_CD_V(init_par = c(1, dt_init[ind==industry & pf=="CD"]$v, dt_init[ind==industry & pf=="CD"]$k)))]
        
        # ACF estimates using DLW method on Translog production function
        dt_est[ind_sel, dlwtl := list(DLW_TL_V(init_par = c(0, 0, 0, 0, 0, 0)))]
        
    message("\n")
}

rm(fs_model, olscd_model, olstl_model, ind_sel, industry)

#///////////////////////////////////////////////////////////////////////////////
#----               4 - COMPUTE MARK-UPS AND PRODUCTIVITIES                 ----
#///////////////////////////////////////////////////////////////////////////////

# Extract individual betas from coefficient lists
dt_est[, beta_v_ols_cd := sapply(olscd, function(x) x[["v"]])]
dt_est[, beta_k_ols_cd := sapply(olscd, function(x) x[["k"]])]
dt_est[, olscd := NULL]
dt_est[, beta_v_dlw_cd := sapply(dlwcd, function(x) x[["v"]])]
dt_est[, beta_k_dlw_cd := sapply(dlwcd, function(x) x[["k"]])]
dt_est[, dlwcd := NULL]
dt_est[, beta_v1_ols_tl := sapply(olstl, function(x) x[["v"]])]
dt_est[, beta_k1_ols_tl := sapply(olstl, function(x) x[["k"]])]
dt_est[, beta_v2_ols_tl := sapply(olstl, function(x) x[["v2"]])]
dt_est[, beta_k2_ols_tl := sapply(olstl, function(x) x[["k2"]])]
dt_est[, beta_vk_ols_tl := sapply(olstl, function(x) x[["vk"]])]
dt_est[, olstl := NULL]
dt_est[, beta_v1_dlw_tl := sapply(dlwtl, function(x) x[["v"]])]
dt_est[, beta_k1_dlw_tl := sapply(dlwtl, function(x) x[["k"]])]
dt_est[, beta_v2_dlw_tl := sapply(dlwtl, function(x) x[["v2"]])]
dt_est[, beta_k2_dlw_tl := sapply(dlwtl, function(x) x[["k2"]])]
dt_est[, beta_vk_dlw_tl := sapply(dlwtl, function(x) x[["vk"]])]
dt_est[, dlwtl := NULL]

# Compute log TFP
dt_est[, tfp_ols_cd := y       - beta_v_ols_cd*v  - beta_k_ols_cd*k]
dt_est[, tfp_ols_tl := y       - beta_v1_ols_tl*v - beta_k1_ols_tl*k - beta_v2_ols_tl*v2 - beta_k2_ols_tl*k2 - beta_vk_ols_tl*vk]
dt_est[, tfp_dlw_cd := Phi_hat - beta_v_dlw_cd*v  - beta_k_dlw_cd*k]
dt_est[, tfp_dlw_tl := Phi_hat - beta_v1_dlw_tl*v - beta_k1_dlw_tl*k - beta_v2_dlw_tl*v2 - beta_k2_dlw_tl*k2 - beta_vk_dlw_tl*vk]

# Compute mark-ups
dt_est[, alpha_v  := exp(v) / exp(Phi_hat)] # corrected share of variable inputs (using expected output Ф instead of realised y)
dt_est[, beta_v_ols_tl := (1*beta_v1_ols_tl + 2*v*beta_v2_ols_tl + k*beta_vk_ols_tl)] # Translog total output elasticity of variable inputs
dt_est[, beta_v_dlw_tl := (1*beta_v1_dlw_tl + 2*v*beta_v2_dlw_tl + k*beta_vk_dlw_tl)] # Translog total output elasticity of variable inputs
dt_est[, mu_v_ols_cd := beta_v_ols_cd / alpha_v]
dt_est[, mu_v_dlw_cd := beta_v_dlw_cd / alpha_v]
dt_est[, mu_v_ols_tl := beta_v_ols_tl / alpha_v]
dt_est[, mu_v_dlw_tl := beta_v_dlw_tl / alpha_v]


#///////////////////////////////////////////////////////////////////////////////
#----               5 - OUTPUT TABLE WITH COMPARED CD ELASTICITIES          ----
#///////////////////////////////////////////////////////////////////////////////

# Compute elasticities based on aggregate FOCs
shares <- dcast(data      = panel[use_2nd],
                formula   = isic_division ~ .,
                fun       = sum,
                na.rm     = T,
                value.var = c("variable_costs", "capital_costs", "operative_revenues"))
shares[, c("variable_costs", "capital_costs") := 
           lapply(.SD, function(x) x/shares[["operative_revenues"]]), .SDcols = 
           c("variable_costs", "capital_costs")]
shares[, operative_revenues := NULL]
setnames(shares, c("isic_division", "variable_costs", "capital_costs"), 
                 c("ind", "V_share", "K_share"))
setorder(shares, "ind")

# Get final elasticities from DLW method
results <- unique(dt_est[,.(ind, v = beta_v_dlw_cd, k = beta_k_dlw_cd)])
setorder(results, "ind")

# Table with elasticities
table_elast_cd <- df_industries[isic_division %in% industries, .(desc=isic_division_desc, ind=isic_division)]
table_elast_cd <- merge(table_elast_cd, shares[, .(ind, FOC_v = V_share, FOC_k = K_share)], by="ind")
table_elast_cd <- merge(table_elast_cd, dt_init[pf=="CD", .(ind, OLS_v = v, OLS_k = k)], by="ind")
table_elast_cd <- merge(table_elast_cd, results[, .(ind, DLW_v = v, DLW_k = k)], by="ind")
table_elast_cd <- merge(table_elast_cd, table_sample_nobs[, .(ind, n = obs_2nd_stage)], by="ind")
table_elast_cd <- table_elast_cd[, .(desc, ind, n, FOC_v, OLS_v, DLW_v, FOC_k, OLS_k, DLW_k)]
table_elast_cd <- table_elast_cd[, c("FOC_tot", "OLS_tot", "DLW_tot") := list(FOC_v+FOC_k, OLS_v+OLS_k, DLW_v+DLW_k)]
write_xlsx(table_elast_cd, paste0(pathEst, "output/CD_elasticities_V_by_industry.xlsx"))

# Remove no longer needed objects
rm(dt_init, results, shares)


#///////////////////////////////////////////////////////////////////////////////
#----           6 - OUTPUT TABLE WITH CD AND TL MARKUPS DISTRIBUTIONS       ----
#///////////////////////////////////////////////////////////////////////////////

# Iterate for Cobb-Douglas and Translog
for (pf in c("CD", "TL")) {
    ### OLS
    table_dist_mu_ols <- dcast.data.table(data = dt_est[use_1st], 
                                          formula = ind ~.,
                                          fun = list(mean, sd, min, p10, p25, p50, p75, p90, max),
                                          na.rm = T,
                                          value.var = paste0("mu_v_ols_",tolower(pf)))
    setnames(table_dist_mu_ols, 2:10, c("mean","sd","min","p10","p25","p50","p75","p90","max"))
    table_dist_mu_ols[, est := "OLS"]
    
    ### DLW
    table_dist_mu_dlw <- dcast.data.table(data = dt_est[use_1st], 
                                          formula = ind ~.,
                                          fun = list(mean, sd, min, p10, p25, p50, p75, p90, max),
                                          na.rm = T,
                                          value.var = paste0("mu_v_dlw_",tolower(pf)))
    setnames(table_dist_mu_dlw, 2:10, c("mean","sd","min","p10","p25","p50","p75","p90","max"))
    table_dist_mu_dlw[, est := "DLW"]
    
    # Combine OLS and DLW, sort, write to Excel
    table_dist_mu <- rbind(table_dist_mu_ols, table_dist_mu_dlw)
    setorder(table_dist_mu, "ind", -"est")
    table_dist_mu <- merge(table_dist_mu, df_industries[, .(desc=isic_division_desc, ind=isic_division)], by="ind", all.x=T)
    table_dist_mu <- merge(table_dist_mu, table_sample_nobs[, .(ind, n=obs_1st_stage)], by="ind")
    table_dist_mu <- table_dist_mu[, .(desc, ind, n, est, mean, sd, min, p10, p25, p50, p75, p90, max)]
    write_xlsx(table_dist_mu, paste0(pathEst, "output/",pf,"_firm_markup_V_distribution_by_industry.xlsx"))
}

rm(table_dist_mu_ols, table_dist_mu_dlw)


#///////////////////////////////////////////////////////////////////////////////
#----                           7 - EXPORT FIRM MARKUPS                     ----
#///////////////////////////////////////////////////////////////////////////////

markups_V <- subset(dt_est, select=c("id", "year", "ind", "v", colnames(dt_est)[grep("mu_", colnames(dt_est))]))
markups_V[, V := exp(v)]
markups_V[, v := NULL]
save(markups_V, file=paste0(pathEst, "output/firm_markups_V.Rdata"))
