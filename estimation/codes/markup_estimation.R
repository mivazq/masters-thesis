#///////////////////////////////////////////////////////////////////////////////
# File name:		markup_estimation.R
# Author:			Miguel Vázquez Vázquez
# Creation date:    10 January 2023
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

# Load data
df_tax_filings <- fread(file=paste0(pathCle, "output/tax_filings.csv"), na.strings="")
df_firm_info   <- fread(file=paste0(pathCle, "output/firm_info.csv"), na.strings="")
df_deflators   <- fread(file=paste0(pathCle, "output/deflators.csv"), na.strings="")
df_cpi_assets  <- fread(file=paste0(pathCle, "output/assets_deflators.csv"), na.strings="")

# Merge tax filings panel with firm industry information
panel <- merge(df_firm_info, df_tax_filings, by="id_sri", all.y=T)
panel <- panel[isic_section %nin% c("P","Q","R","S","T")]

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

# Merge Consumer Price Indexes to deflate assets
panel <- merge(panel, df_cpi_assets, by="year", all.x=T)

# Resort data to original state
setorder(panel, "id_sri", "year")

# Deflate assets
panel[, fixed_assets := fixed_assets/cpi]

# Deflate other monetary variables
panel[, c("operative_revenues", "material_costs", "labour_costs") := lapply(.SD, function(x) x/panel[["deflator"]]), .SDcols = c("operative_revenues", "material_costs", "labour_costs")]

# Generate variable that sums up material and labour costs := "sales_costs"
panel[, sales_costs := material_costs + labour_costs]

# Convert start date to start year from firm register
panel[, startdate := NULL] # can't be used, differs way too much from first/last filing


#///////////////////////////////////////////////////////////////////////////////
#----                           2 - CHECKS                                  ----
#///////////////////////////////////////////////////////////////////////////////

# Plots
plot(panel$material_costs, panel$operative_revenues)
plot(panel$labour_costs,   panel$operative_revenues)
plot(panel$sales_costs,    panel$operative_revenues)
plot(panel$material_costs, panel$operative_revenues, log="xy")
plot(panel$labour_costs,   panel$operative_revenues, log="xy")
plot(panel$sales_costs,    panel$operative_revenues, log="xy")

# Correlations
cor(panel$material_costs, panel$operative_revenues, use="pairwise")
cor(panel$labour_costs,   panel$operative_revenues, use="pairwise")
cor(panel$sales_costs,    panel$operative_revenues, use="pairwise")
cor(log(panel$material_costs), log(panel$operative_revenues), use="pairwise")
cor(log(panel$labour_costs),   log(panel$operative_revenues), use="pairwise")
cor(log(panel$sales_costs),    log(panel$operative_revenues), use="pairwise")


#///////////////////////////////////////////////////////////////////////////////
#----                 3 - FIRST AND SECOND STAGE ESTIMATIONS                ----
#///////////////////////////////////////////////////////////////////////////////

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

# Create panel datatable (v = simply l + m)
dt_est <- panel[, .(id = id_sri, 
                year = year, 
                sec = isic_section,
                # div = isic_division,
                # e = is_exporter,
                y = log(operative_revenues), 
                k = log(fixed_assets), 
                l = log(labour_costs),
                m = log(material_costs),
                v = log(sales_costs))]

# Iterate over industries 
for (ind in sort(unique(dt_est$sec))) {
    
    # Logical vector for sector selection
    dt_sel <- dt_est$sec==ind
    
    message(paste("ESTIMATING FIRST STAGE FOR SECTION "), ind)
    
    ### FIRST STAGE
        # Generate polynomial terms on inputs variables for Phi_hat estimation (prefix: "fs")
        degree <- 3 # degree of polynomial expansion
        for (iter1 in 1:degree) {
            dt_est[, paste0("fs_","v", iter1) := v^iter1]
            dt_est[, paste0("fs_","k", iter1) := k^iter1]
            
            for (iter2 in 1:degree) {
                dt_est[, paste0("fs_","v", iter1, "k", iter2) := v^iter2 * k^iter1]
            }
        }
        rm(iter1, iter2, degree) # remove useless values
        fs_vars <- colnames(dt_est)[grep("^fs_", colnames(dt_est))] # list all "fs_" vars
        
        # Run first-stage estimation
        frml <- paste("y ~",paste(fs_vars, collapse = " + "),"| year") # formula
        fs_model <- feols(as.formula(frml), data = dt_est, subset = dt_sel & !is.na(dt$y), panel.id=c("id","year"))
        
        # Store estimates of expected output (phi_hat) and measurement error (epsilon_hat) 
        dt_est[!is.na(y) & dt_sel, `:=` (Phi_hat     = predict(fs_model), 
                                         epsilon_hat = residuals(fs_model))]
        dt_est[, Phi_hat_lag := shift(Phi_hat, type="lag"), by = id]
        
        # Drop first stage variables (no longer needed)
        dt_est[, (fs_vars) := NULL]
        rm(fs_vars, frml)
    
    
    message(paste("ESTIMATING SECOND STAGE FOR SECTION "), ind)
    
    ### SECOND STAGE 
        # Create new variables needed for translog production function
        dt_est[, `:=` (v2         = v^2,
                       k2         = k^2,
                       vk         = v * k)]
        # Create new variables needed for productivity estimation
        dt_est[, `:=` (v_lag = shift(v, 1),
                       k_lag = shift(k, 1)), 
               by = id]
        dt_est[, `:=` (v_lag2     = v_lag^2,
                       k_lag2     = k_lag^2,
                       v_lagk_lag = v_lag * k_lag,
                       v_lagk     = v_lag * k)]
        
        # Generate constant term and indicator for which observations are to be used
        # i.e., we need current period inputs & phi, and lagged inputs & phi
        # we also select for the sector here.
        dt_est[, cons := 1]
        dt_est[!is.na(v) & !is.na(v_lag) & !is.na(Phi_hat) & !is.na(Phi_hat_lag), use := 1]
        dt_est[is.na(use), use := 0]
        
        # OLS estimates
        ols_model <- feols(y ~ v + k | year, data = dt_est[use==1 & dt_sel], panel.id=c("id","year"))
        dt_est[dt_sel, ols := list(ols_model$coefficients)]
        
        
        # ACF estimates using DLW method on Cobb Douglas production function
        dt_est[dt_sel, dlwcd := list(DLW_CD(init_par = c(1, ols_model$coefficients["v"], ols_model$coefficients["k"])))]
        
        # ACF estimates using DLW method on Translog production function
        dt_est[dt_sel, dlwtl := list(DLW_TL(init_par = c(0, 0, 0, 0, 0, 0)))]
    
    message("\n")
}

rm(fs_model, ols_model, dt_sel, ind)

#///////////////////////////////////////////////////////////////////////////////
#----               4 - COMPUTE MARK-UPS AND PRODUCTIVITIES                 ----
#///////////////////////////////////////////////////////////////////////////////

# Extract individual betas from coefficient lists
dt_est[, beta_v_ols := sapply(ols, function(x) x[["v"]])]
dt_est[, beta_k_ols := sapply(ols, function(x) x[["k"]])]
dt_est[, ols := NULL]
dt_est[, beta_v_dlwcd := sapply(dlwcd, function(x) x[["v"]])]
dt_est[, beta_k_dlwcd := sapply(dlwcd, function(x) x[["k"]])]
dt_est[, dlwcd := NULL]
dt_est[, beta_v1_dlwtl := sapply(dlwtl, function(x) x[["v"]])]
dt_est[, beta_k1_dlwtl := sapply(dlwtl, function(x) x[["k"]])]
dt_est[, beta_v2_dlwtl := sapply(dlwtl, function(x) x[["v2"]])]
dt_est[, beta_k2_dlwtl := sapply(dlwtl, function(x) x[["k2"]])]
dt_est[, beta_vk_dlwtl := sapply(dlwtl, function(x) x[["vk"]])]
dt_est[, dlwtl := NULL]

# Compute log TFP
dt_est[, tfp_ols   := y       - beta_v_ols*v    - beta_k_ols*k]
dt_est[, tfp_dlwcd := Phi_hat - beta_v_dlwcd*v  - beta_k_dlwcd*k]
dt_est[, tfp_dlwtl := Phi_hat - beta_v1_dlwtl*v - beta_k1_dlwtl*k - beta_v2_dlwtl*v2 - beta_k2_dlwtl*k2 - beta_vk_dlwtl*vk]

# Compute mark-ups
dt_est[, alpha_v  := exp(v) / exp(Phi_hat)] # corrected share of variable inputs (using expected output Ф instead of realised y)
dt_est[, beta_v_dlwtl := (beta_v1_dlwtl + 2*beta_v2_dlwtl*v + beta_vk_dlwtl*k)] # translog total output elasticity of variable inputs
dt_est[, mu_ols   := beta_v_ols   / alpha_v]
dt_est[, mu_dlwcd := beta_v_dlwcd / alpha_v]
dt_est[, mu_dlwtl := beta_v_dlwtl / alpha_v]




