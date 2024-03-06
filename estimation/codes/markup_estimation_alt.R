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
df_tax_filings_imp_exp <- fread(file=paste0(pathCle, "output/tax_filings_imputed_expansion.csv"), na.strings="")
df_firm_info   <- fread(file=paste0(pathCle, "output/firm_info.csv"), na.strings="")
df_deflators   <- fread(file=paste0(pathCle, "output/deflators.csv"), na.strings="")
df_def_assets  <- fread(file=paste0(pathCle, "output/assets_deflators.csv"), na.strings="")

# Append baseline filings with expansion set thanks to imputation
# df_tax_filings <- rbind(df_tax_filings[, impute := 0], df_tax_filings_imp_exp)

# Merge tax filings panel with firm industry information
panel <- merge(df_firm_info, df_tax_filings, by="id_sri", all.y=T)

# Keep only relevant industries
panel <- panel[isic_section %in% c("A","B","D","F","G")]

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

# Resort data to original state
setorder(panel, "id_sri", "year")

# Deflate assets and cost of capital using 
panel[, c("fixed_assets", "capital_costs") := lapply(.SD, function(x) x/panel[["capital_deflator"]]*100), .SDcols = c("fixed_assets", "capital_costs")]

# Deflate other monetary variables
panel[, c("operative_revenues", "material_costs", "labour_costs") := lapply(.SD, function(x) x/panel[["deflator"]]*100), .SDcols = c("operative_revenues", "material_costs", "labour_costs")]

# Generate variable that sums up material and labour costs := "variable_costs"
panel[, variable_costs := material_costs + labour_costs]

# # 2008 distribution of start dates
# for (y in 2008:2011) {
#     png(file = paste0(pathFig,sysdate,'_dist_stdart_year_',y,'.png'))
#     hist(as.numeric(substr(panel[entry==1 & year==y]$startdate,6,10))[as.numeric(substr(panel[entry==1 & year==y]$startdate,6,10))>=1961], 
#          xlim = range(1961,2011), ylim = range(0,2500), breaks=1961:2011, 
#          xlab = "Start of activity according to firm registry", 
#          main = paste0(y))
#     dev.off()
# }

# panel[, startdate := NULL] # can't be used, differs way too much from first/last filing

#///////////////////////////////////////////////////////////////////////////////
#----                       ... - MEETING 2                                 ----
#///////////////////////////////////////////////////////////////////////////////
# 
# ### ENTRY DATE
# 
# # Check entry year under current specification (first filing)
# cur <- ggplot(panel[entry==1], aes(x=year)) + geom_bar() + theme_bw()
# ggsave(cur, filename = paste0(pathFig, format(Sys.Date()),"_entry_current.png"))
# 
# # Check entry year under alternative specification (firm registry + correction)
# panel[, alt := ifelse(entry==1, as.integer(substr(startdate,6,10)), NA)] # get firm registry date
# panel[year<alt, alt := year] # correct in cases where first_filing < registry date (2 cases)
# alt <- ggplot(panel[entry==1 & alt>=2008], aes(x=alt)) + geom_bar() + theme_bw()
# ggsave(alt, filename = paste0(pathFig, format(Sys.Date()),"_entry_alternative.png"))
# alt_excl <- ggplot(panel[entry==1 & alt>=2008 & !(alt<year & alt>=2008)], aes(x=alt)) + geom_bar() + theme_bw()
# ggsave(alt_excl, filename = paste0(pathFig, format(Sys.Date()),"_entry_alternative_exclusions.png"))
# 
# # Alternative 2
# panel[, alt2 := alt] # new version
# panel[(alt2<year & alt2>=2008), alt2 := year] # adjust firm registry when first filing is after registry date and registry date >= 2008
# alt2 <- ggplot(panel[entry==1 & alt2>=2008], aes(x=alt2)) + geom_bar() + theme_bw()
# ggsave(alt, filename = paste0(pathFig, format(Sys.Date()),"_entry_alternative2.png"))
# 
# # Look at entry across entire firm registry
# fr <- ggplot(df_firm_info[as.numeric(substr(startdate,6,10))>1995], aes(x=substr(startdate,6,10))) + geom_bar() + theme_bw()
# ggsave(fr, filename = paste0(pathFig, format(Sys.Date()),"_entry_all_firm_registry.png"))
# 
# # Convert start date to start year from firm register
# panel[, c("alt","alt2", "startdate") := NULL] # can't be used, differs way too much from first/last filing
# rm(cur,alt,alt_excl,alt2,fr,y)
# 
# 
# 
# 
# ### LABOUR COST
# df_wage_bills <- fread(file=paste0(pathCle, "output/wage_bills.csv"), na.strings="")
# compare_wages <- merge(panel[active==1, .(id_sri,year,labour_costs)], df_wage_bills, by=c("id_sri","year"), all.x = T)
# compare_wages[, rel_diff := (wages-labour_costs)/labour_costs*100]
# above = nrow(compare_wages[rel_diff>100])/nrow(compare_wages)*100 # share of observations where difference is more than 100%
# miss = nrow(compare_wages[is.na(rel_diff)])/nrow(compare_wages)*100 # share of observations where SS data is missing
# png(file = paste0(pathFig,sysdate,'_compare_wages.png'))
# hist(compare_wages[rel_diff<=100]$rel_diff, freq=F, xlab="% difference between social security wages compared to tax filings",
#      main = paste0("(SS_wages - TF_wages)/TF_wages
#                    [capped at 100%, but ",round(above,2),"% of cases are above 100%]
#                    [and ",round(miss,2),"% of cases have missing SS data]"))
# dev.off()
# 
# 
# 
# 
# 
# ### TRANSACTION COSTS
# panel[, investment := fixed_assets-0.9*shift(fixed_assets), by=id_sri] # assume 10% depr. rate
# 
# df_purchases <- fread(file=paste0(pathCle, "output/intermediate_cost.csv"), na.strings="")
# compare_trans <- merge(panel[active==1, .(id_sri,year,material_costs,investment)], df_purchases, by=c("id_sri","year"), all.x = T)
# compare_trans[, rel_diff := (cost_transactions-material_costs)/material_costs*100]
# above = nrow(compare_trans[rel_diff>100])/nrow(compare_trans)*100 # share of observations where difference is more than 100%
# miss = nrow(compare_trans[is.na(rel_diff)])/nrow(compare_trans)*100 # share of observations where SS data is missing
# png(file = paste0(pathFig,sysdate,'_compare_trans.png'))
# hist(compare_trans[rel_diff<=100]$rel_diff, freq=F, xlab="% difference between transaction annex purchases compared material costs",
#      main = paste0("(TA_purchases - TF_materials)/TF_materials
#                    [capped at 100%, but ",round(above,2),"% of cases are above 100%]
#                    [and ",round(miss,2),"% of cases have missing TA data]"))
# dev.off()
# 
# panel[, investment := NULL]
# 
# rm(compare_wages, compare_trans, above, miss)

#///////////////////////////////////////////////////////////////////////////////
#----                           2 - CHECKS                                  ----
#///////////////////////////////////////////////////////////////////////////////

# # Plots
# plot(panel$material_costs, panel$operative_revenues)
# plot(panel$labour_costs,   panel$operative_revenues)
# plot(panel$variable_costs, panel$operative_revenues)
# plot(panel$material_costs, panel$operative_revenues, log="xy")
# plot(panel$labour_costs,   panel$operative_revenues, log="xy")
# plot(panel$variable_costs, panel$operative_revenues, log="xy")
# 
# # Correlations
# cor(panel$material_costs, panel$operative_revenues, use="pairwise")
# cor(panel$labour_costs,   panel$operative_revenues, use="pairwise")
# cor(panel$variable_costs, panel$operative_revenues, use="pairwise")
# 
# cor(log(panel$material_costs), log(panel$operative_revenues), use="pairwise")
# cor(log(panel$labour_costs),   log(panel$operative_revenues), use="pairwise")
# cor(log(panel$variable_costs), log(panel$operative_revenues), use="pairwise") 


#///////////////////////////////////////////////////////////////////////////////
#----                 3 - COMPUTE LABOR/MATERIALS/CAPITAL SHARES            ----
#///////////////////////////////////////////////////////////////////////////////

shares <- dcast(data      = panel,
                formula   = isic_division ~ .,
                fun       = sum,
                na.rm     = T,
                value.var = c("material_costs", "labour_costs", "variable_costs", "capital_costs", "operative_revenues"))
shares[, c("material_costs", "labour_costs", "variable_costs", "capital_costs") := 
           lapply(.SD, function(x) x/shares[["operative_revenues"]]), .SDcols = 
           c("material_costs", "labour_costs", "variable_costs", "capital_costs")]
shares[, operative_revenues := NULL]
setnames(shares, c("material_costs", "labour_costs", "variable_costs", "capital_costs"), c("M_share", "L_share", "V_share", "K_share"))
shares[, tot_share_v_k := V_share + K_share]

VA_shares <- dcast(data      = panel,
                   formula   = isic_division ~ .,
                   fun       = sum,
                   na.rm     = T,
                   value.var = c("material_costs", "labour_costs", "capital_costs", "operative_revenues"))
VA_shares[, VA := operative_revenues - material_costs]
VA_shares[, c("operative_revenues", "material_costs") := NULL]
VA_shares[, c("labour_costs", "capital_costs") := 
              lapply(.SD, function(x) x/VA_shares[["VA"]]), .SDcols = 
              c("labour_costs", "capital_costs")]
VA_shares[, c("VA") := NULL]
setnames(VA_shares, c("labour_costs", "capital_costs"), c("L_share", "K_share"))
VA_shares[, tot_share_l_k := L_share + K_share]


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


# source alternative functions
source(paste0(pathFun, "DLW_alt.R"))

# Create panel datatable (v = simply l + m)
dt_est <- panel[, .(id = id_sri, 
                year = year, 
                sec = isic_division,
                # div = isic_division,
                # e = is_exporter,
                y = log(operative_revenues), 
                k = log(fixed_assets), 
                l = log(labour_costs),
                m = log(material_costs),
                v = log(variable_costs))]

# Create new variables needed for translog production function
dt_est[, `:=` (l2  = l^2,
               m2  = m^2,
               k2  = k^2,
               lk  = l * k,
               mk  = m * k,
               lm  = l * m,
               lmk = l * m * k)]

# Create table to store OLS estimates by sector which will be used as initial values
# for GMM.
sectors <- sort(unique(dt_est$sec))
dt_init <- data.table(sec = rep(sectors,2),
                      pf  = c(rep("CD",length(sectors)), rep("TL",length(sectors))))

# Iterate over industries 
for (ind in sort(unique(dt_est$sec))) {
    
    if (ind %in% c("C10", "C12", "D16", "D30")) {
        message("ESTIMATION FOR DIVISION ", ind, " SKIPPED, UNSUFFICIENT OBSERVATIONS\n")
        next
    }
    
    # Logical vector for sector selection and first stage selection
    sec_sel <- dt_est$sec==ind
    use_1st  <- !is.na(dt_est$y)
    
    message("ESTIMATING FIRST STAGE FOR DIVISION ", ind)
    
    ### FIRST STAGE
        # Generate polynomial terms on inputs variables for Phi_hat estimation (prefix: "fs")
        degree <- 3 # degree of polynomial expansion
        for (iter1 in 1:degree) {
            dt_est[, paste0("fs_", "l", iter1) := l^iter1]
            dt_est[, paste0("fs_", "m", iter1) := m^iter1]
            dt_est[, paste0("fs_", "k", iter1) := k^iter1]
            
            for (iter2 in 1:degree) {
                dt_est[, paste0("fs_", "l", iter1, "k", iter2) := l^iter1 * k^iter2]
                dt_est[, paste0("fs_", "m", iter1, "k", iter2) := m^iter1 * k^iter2]
                dt_est[, paste0("fs_", "l", iter1, "m", iter2) := l^iter1 * m^iter2]
            }
            
            dt_est[, paste0("fs_", "l", iter1, "m", iter1, "k", iter1) := l^iter1 * m^iter1 * k^iter1]
        }
        rm(iter1, iter2, degree) # remove useless values
        fs_vars <- colnames(dt_est)[grep("^fs_", colnames(dt_est))] # list all "fs_" vars
        
        # Run first-stage estimation
        frml <- paste("y ~",paste(fs_vars, collapse = " + "),"| year") # formula
        fs_model <- feols(as.formula(frml), data = dt_est, subset = use_1st & sec_sel, panel.id=c("id","year"))
        
        # Store estimates of expected output (phi_hat) and measurement error (epsilon_hat) 
        dt_est[use_1st & sec_sel, `:=` (Phi_hat     = predict(fs_model), 
                                        epsilon_hat = residuals(fs_model))]
        dt_est[, Phi_hat_lag := shift(Phi_hat, type="lag"), by = id]
        
        # Drop first stage variables (no longer needed)
        dt_est[, (fs_vars) := NULL]
        rm(fs_vars, frml)
    
    
    message("ESTIMATING SECOND STAGE FOR DIVISION ", ind)
        
    ### SECOND STAGE
        # Create new variables needed for productivity estimation
        dt_est[, `:=` (l_lag = shift(l, 1),
                       m_lag = shift(m, 1),
                       k_lag = shift(k, 1)), 
               by = id]
        dt_est[, `:=` (l_lag2          = l_lag^2,
                       m_lag2          = m_lag^2,
                       k_lag2          = k_lag^2,
                       l_lagk_lag      = l_lag * k_lag,
                       m_lagk_lag      = m_lag * k_lag,
                       l_lagm_lag      = l_lag * m_lag,
                       l_lagm_lagk_lag = l_lag * m_lag * k_lag,
                       l_lagk          = l_lag * k,
                       m_lagk          = m_lag * k,
                       l_lagm_lagk     = l_lag * m_lag * k)]
        
        # Generate constant term and indicator for which observations are to be used
        # i.e., we need current period inputs & phi, and lagged inputs & phi
        # we also select for the sector here.
        dt_est[, cons := 1]
        use_2nd  <- !is.na(dt_est$l) & !is.na(dt_est$l_lag) & !is.na(dt_est$Phi_hat) & !is.na(dt_est$Phi_hat_lag)
        
        # OLS estimates on Cobb Douglas production function
        olscd_model <- feols(y ~ l + m + k | year, 
                             data = dt_est, subset = use_2nd & sec_sel, panel.id=c("id","year"))
        dt_est[sec_sel, olscd := list(olscd_model$coefficients)]
        dt_init[sec==ind & pf=="CD", names(as.list(olscd_model$coefficients)) := as.list(olscd_model$coefficients)]
        
        # OLS estimates on Translog production function
        olstl_model <- feols(y ~ l + m + k + l2 + m2 + k2 + lk + mk + lm + lmk | year, 
                             data = dt_est, subset = use_2nd & sec_sel, panel.id=c("id","year"))
        dt_est[sec_sel, olstl := list(olstl_model$coefficients)]
        dt_init[sec==ind & pf=="TL", names(as.list(olstl_model$coefficients)) := as.list(olstl_model$coefficients)]
        
        # ACF estimates using DLW method on Cobb Douglas production function
        dt_est[sec_sel, dlwcd := list(DLW_CD(init_par = c(1, olscd_model$coefficients["l"], olscd_model$coefficients["m"], olscd_model$coefficients["k"])))]
        # dt_est[sec_sel, dlwcd := list(DLW_CD(init_par = c(1, shares[isic_division==ind]$L_share, shares[isic_division==ind]$M_share, shares[isic_division==ind]$K_share)))]
        
        # ACF estimates using DLW method on Translog production function
        dt_est[sec_sel, dlwtl := list(DLW_TL(init_par = c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)))]
    
    message("\n")
}

rm(fs_model, olscd_model, olstl_model, sec_sel, ind)

# Set missings
for (ind in c("C10", "C12", "D16", "D30")) {
    dt_est[sec==ind, c("olscd", "dlwcd", "olstl", "dlwtl") := NA]
}

#///////////////////////////////////////////////////////////////////////////////
#----               4 - COMPUTE MARK-UPS AND PRODUCTIVITIES                 ----
#///////////////////////////////////////////////////////////////////////////////

# Extract individual betas from coefficient lists
dt_est[sec %nin% c("C10", "C12", "D16", "D30"), beta_l_olscd := sapply(olscd, function(x) x[["l"]])]
dt_est[sec %nin% c("C10", "C12", "D16", "D30"), beta_m_olscd := sapply(olscd, function(x) x[["m"]])]
dt_est[sec %nin% c("C10", "C12", "D16", "D30"), beta_k_olscd := sapply(olscd, function(x) x[["k"]])]
dt_est[, olscd := NULL]
dt_est[sec %nin% c("C10", "C12", "D16", "D30"), beta_l_dlwcd := sapply(dlwcd, function(x) x[["l"]])]
dt_est[sec %nin% c("C10", "C12", "D16", "D30"), beta_m_dlwcd := sapply(dlwcd, function(x) x[["m"]])]
dt_est[sec %nin% c("C10", "C12", "D16", "D30"), beta_k_dlwcd := sapply(dlwcd, function(x) x[["k"]])]
dt_est[, dlwcd := NULL]
dt_est[sec %nin% c("C10", "C12", "D16", "D30"), beta_l1_olstl := sapply(olstl, function(x) x[["l"]])]
dt_est[sec %nin% c("C10", "C12", "D16", "D30"), beta_m1_olstl := sapply(olstl, function(x) x[["m"]])]
dt_est[sec %nin% c("C10", "C12", "D16", "D30"), beta_k1_olstl := sapply(olstl, function(x) x[["k"]])]
dt_est[sec %nin% c("C10", "C12", "D16", "D30"), beta_l2_olstl := sapply(olstl, function(x) x[["l2"]])]
dt_est[sec %nin% c("C10", "C12", "D16", "D30"), beta_m2_olstl := sapply(olstl, function(x) x[["m2"]])]
dt_est[sec %nin% c("C10", "C12", "D16", "D30"), beta_k2_olstl := sapply(olstl, function(x) x[["k2"]])]
dt_est[sec %nin% c("C10", "C12", "D16", "D30"), beta_lk_olstl := sapply(olstl, function(x) x[["lk"]])]
dt_est[sec %nin% c("C10", "C12", "D16", "D30"), beta_mk_olstl := sapply(olstl, function(x) x[["mk"]])]
dt_est[sec %nin% c("C10", "C12", "D16", "D30"), beta_lm_olstl := sapply(olstl, function(x) x[["lm"]])]
dt_est[sec %nin% c("C10", "C12", "D16", "D30"), beta_lmk_olstl := sapply(olstl, function(x) x[["lmk"]])]
dt_est[, olstl := NULL]
dt_est[sec %nin% c("C10", "C12", "D16", "D30"), beta_l1_dlwtl := sapply(dlwtl, function(x) x[["l"]])]
dt_est[sec %nin% c("C10", "C12", "D16", "D30"), beta_m1_dlwtl := sapply(dlwtl, function(x) x[["m"]])]
dt_est[sec %nin% c("C10", "C12", "D16", "D30"), beta_k1_dlwtl := sapply(dlwtl, function(x) x[["k"]])]
dt_est[sec %nin% c("C10", "C12", "D16", "D30"), beta_l2_dlwtl := sapply(dlwtl, function(x) x[["l2"]])]
dt_est[sec %nin% c("C10", "C12", "D16", "D30"), beta_m2_dlwtl := sapply(dlwtl, function(x) x[["m2"]])]
dt_est[sec %nin% c("C10", "C12", "D16", "D30"), beta_k2_dlwtl := sapply(dlwtl, function(x) x[["k2"]])]
dt_est[sec %nin% c("C10", "C12", "D16", "D30"), beta_lk_dlwtl := sapply(dlwtl, function(x) x[["lk"]])]
dt_est[sec %nin% c("C10", "C12", "D16", "D30"), beta_mk_dlwtl := sapply(dlwtl, function(x) x[["mk"]])]
dt_est[sec %nin% c("C10", "C12", "D16", "D30"), beta_lm_dlwtl := sapply(dlwtl, function(x) x[["lm"]])]
dt_est[sec %nin% c("C10", "C12", "D16", "D30"), beta_lmk_dlwtl := sapply(dlwtl, function(x) x[["lmk"]])]
dt_est[, dlwtl := NULL]

# Compute log TFP
dt_est[, tfp_olscd := y       - beta_l_olscd*l   - beta_m_olscd*m   - beta_k_olscd*k]
dt_est[, tfp_olstl := y       - beta_l1_olstl*l  - beta_m1_olstl*m  - beta_k1_olstl*k 
                              - beta_l2_olstl*l2 - beta_m2_olstl*m2 - beta_k2_olstl*k2
                              - beta_lk_olstl*lk - beta_mk_olstl*mk - beta_lm_olstl*lm - beta_lmk_olstl*lmk]
dt_est[, tfp_dlwcd := Phi_hat - beta_l_dlwcd*l   - beta_m_dlwcd*m   - beta_k_dlwcd*k]
dt_est[, tfp_dlwtl := Phi_hat - beta_l1_dlwtl*l  - beta_m1_dlwtl*m  - beta_k1_dlwtl*k 
                              - beta_l2_dlwtl*l2 - beta_m2_dlwtl*m2 - beta_k2_dlwtl*k2
                              - beta_lk_dlwtl*lk - beta_mk_dlwtl*mk - beta_lm_dlwtl*lm - beta_lmk_dlwtl*lmk]

# Compute mark-ups
dt_est[, alpha_l  := exp(l) / exp(Phi_hat)] # corrected share of labour inputs (using expected output Ф instead of realised y)
dt_est[, alpha_m  := exp(m) / exp(Phi_hat)] # corrected share of material inputs (using expected output Ф instead of realised y)
dt_est[, beta_l_olstl := (1*beta_l1_olstl + 2*l*beta_l2_olstl + k*beta_lk_olstl + m*beta_lm_olstl + m*k*beta_lmk_olstl)] # translog total output elasticity of labour inputs
dt_est[, beta_m_olstl := (1*beta_m1_olstl + 2*m*beta_m2_olstl + k*beta_mk_olstl + l*beta_lm_olstl + l*k*beta_lmk_olstl)] # translog total output elasticity of material inputs
dt_est[, beta_l_dlwtl := (1*beta_l1_dlwtl + 2*l*beta_l2_dlwtl + k*beta_lk_dlwtl + m*beta_lm_dlwtl + m*k*beta_lmk_dlwtl)] # translog total output elasticity of labour inputs
dt_est[, beta_m_dlwtl := (1*beta_m1_dlwtl + 2*m*beta_m2_dlwtl + k*beta_mk_dlwtl + l*beta_lm_dlwtl + l*k*beta_lmk_dlwtl)] # translog total output elasticity of material inputs
dt_est[, mu_l_olscd := beta_l_olscd / alpha_l]
dt_est[, mu_l_dlwcd := beta_l_dlwcd / alpha_l]
dt_est[, mu_l_olstl := beta_l_olstl / alpha_l]
dt_est[, mu_l_dlwtl := beta_l_dlwtl / alpha_l]
dt_est[, mu_m_olscd := beta_m_olscd / alpha_m]
dt_est[, mu_m_dlwcd := beta_m_dlwcd / alpha_m]
dt_est[, mu_m_olstl := beta_m_olstl / alpha_m]
dt_est[, mu_m_dlwtl := beta_m_dlwtl / alpha_m]

#///////////////////////////////////////////////////////////////////////////////
#----                           5 - OUTPUT                                  ----
#///////////////////////////////////////////////////////////////////////////////

# Table with elasticities
res <- unique(dt_est[,.(sec, beta_l_dlwcd, beta_m_dlwcd, beta_k_dlwcd)])
setorder(res, sec)
if (!all.equal(res$sec,shares$isic_division)) {
    message("NOT IN SAME ORDER")
}
elast_cd <- data.table(industry  = shares$isic_division,
                       # initial_v = shares$V_share,
                       # initial_k = shares$K_share,
                       initial_l = dt_init[pf=="CD"]$l,
                       initial_m = dt_init[pf=="CD"]$m,
                       initial_k = dt_init[pf=="CD"]$k,
                       final_l   = res$beta_l_dlwcd,
                       final_m   = res$beta_m_dlwcd,
                       final_k   = res$beta_k_dlwcd)


# Table with number of observations/firms per sector available in whole panel, used
# for first stage and second stage. First stage = simply active observations.
df_sections <- fread(paste0(pathCle, "output/isic_codes_division.csv"))
n_table <- df_sections[substr(isic_division,1,1) %in% c("A","B","D","F","G"), .(desc=isic_division_desc, sec=isic_division)]
n_table[, full_panel_obs  := table(factor(dt_est$sec, levels = n_table$sec))]
n_table[, full_panel_ids  := table(factor(unique(dt_est[,.(id,sec)])$sec, levels = n_table$sec))]
n_table[, `1st_stage_obs` := table(factor(dt_est[use_1st]$sec, levels = n_table$sec))]
n_table[, `1st_stage_ids` := table(factor(unique(dt_est[use_1st,.(id,sec)])$sec, levels = n_table$sec))]
n_table[, `2nd_stage_obs` := table(factor(dt_est[use_2nd]$sec, levels = n_table$sec))]
n_table[, `2nd_stage_ids` := table(factor(unique(dt_est[use_2nd,.(id,sec)])$sec, levels = n_table$sec))]
write_xlsx(n_table, paste0(pathWD, "meeting prep/meeting 2/sample_div_alt.xlsx"))



# Custom percentile functions
p10   <- function (x,na.rm=T) quantile(x, prob=c(0.10),na.rm=na.rm)
p25   <- function (x,na.rm=T) quantile(x, prob=c(0.25),na.rm=na.rm)
p50   <- function (x,na.rm=T) quantile(x, prob=c(0.50),na.rm=na.rm)
p75   <- function (x,na.rm=T) quantile(x, prob=c(0.75),na.rm=na.rm)
p90   <- function (x,na.rm=T) quantile(x, prob=c(0.90),na.rm=na.rm)

for (var in c("L","M")) {
for (pf in c("CD","TL")) {
    ### OLS
    dist_table_ols <- dcast.data.table(data = dt_est[use_1st], 
                                       formula = sec ~.,
                                       fun = list(mean, sd, min, p10, p25, p50, p75, p90, max),
                                       na.rm = T,
                                       value.var = paste0("mu_",tolower(var),"_ols",tolower(pf)))
    setnames(dist_table_ols, 2:10, c("mean","sd","min","p10", "p25", "p50", "p75", "p90", "max"))
    dist_table_ols[, est := "OLS"]
    
    ### DLW
    dist_table_dlw <- dcast.data.table(data = dt_est[use_1st], 
                                       formula = sec ~.,
                                       fun = list(mean, sd, min, p10, p25, p50, p75, p90, max),
                                       na.rm = T,
                                       value.var = paste0("mu_",tolower(var),"_dlw",tolower(pf)))
    setnames(dist_table_dlw, 2:10, c("mean","sd","min","p10", "p25", "p50", "p75", "p90", "max"))
    dist_table_dlw[, est := "DLW"]
    
    # Combine OLS and DLW, sort, write to Excel
    dist_table <- rbind(dist_table_ols, dist_table_dlw)
    setorder(dist_table, "sec", "est")
    dist_table <- dist_table[, .(sec, est, mean, sd, min, p10, p25, p50, p75, p90, max)]
    dist_table[sec %in% c("C10", "C12", "D16", "D30"), c("mean", "sd", "min", "p10", "p25", "p50", "p75", "p90", "max") := NA]
    write_xlsx(dist_table, paste0(pathWD, "meeting prep/meeting 2/",pf,"_markups_div_",var,".xlsx"))
}
}


# Check correlations between log TFP and outputs, inputs
tfp_cor <- data.table(pf  = c(rep("CD",2), rep("TL", 2)),
                      est = rep(c(rep("OLS",1),rep("DLW",1)),2),
                      y   = 0,
                      l   = 0,
                      m   = 0,
                      k   = 0)
tfp_cor[pf=="CD" & est=="OLS", c("y", "l", "m", "k") := list(cor(dt_est$tfp_olscd, dt_est$y, use="pairwise"),
                                                             cor(dt_est$tfp_olscd, dt_est$l, use="pairwise"),
                                                             cor(dt_est$tfp_olscd, dt_est$m, use="pairwise"),
                                                             cor(dt_est$tfp_olscd, dt_est$k, use="pairwise"))]
tfp_cor[pf=="CD" & est=="DLW", c("y", "l", "m", "k") := list(cor(dt_est$tfp_dlwcd, dt_est$Phi_hat, use="pairwise"),
                                                             cor(dt_est$tfp_dlwcd, dt_est$l, use="pairwise"),
                                                             cor(dt_est$tfp_dlwcd, dt_est$m, use="pairwise"),
                                                             cor(dt_est$tfp_dlwcd, dt_est$k, use="pairwise"))]
tfp_cor[pf=="TL" & est=="OLS", c("y", "l", "m", "k") := list(cor(dt_est$tfp_olstl, dt_est$y, use="pairwise"),
                                                             cor(dt_est$tfp_olstl, dt_est$l, use="pairwise"),
                                                             cor(dt_est$tfp_olstl, dt_est$m, use="pairwise"),
                                                             cor(dt_est$tfp_olstl, dt_est$k, use="pairwise"))]
tfp_cor[pf=="TL" & est=="DLW", c("y", "l", "m", "k") := list(cor(dt_est$tfp_dlwtl, dt_est$Phi_hat, use="pairwise"),
                                                             cor(dt_est$tfp_dlwtl, dt_est$l, use="pairwise"),
                                                             cor(dt_est$tfp_dlwtl, dt_est$m, use="pairwise"),
                                                             cor(dt_est$tfp_dlwtl, dt_est$k, use="pairwise"))]



# Construct aggregate mark-up and plot evolution
agg_mu <- data.table(pf   = c(rep("CD",2), rep("TL", 2)),
                     est  = rep(c(rep("OLS",1),rep("DLW",1)),2),
                     inp  = rep(c(rep("L",1),rep("M",1)),2),
                     year = c(rep(2008,4),rep(2009,4),rep(2010,4),rep(2011,4)))
for (yyy in 2008:2011) {
    agg_mu[pf=="CD" & est=="OLS" & year==yyy, value := weighted.mean(dt_est[year==yyy]$mu_olscd, w = exp(dt_est[year==yyy]$v), na.rm = T)]
    agg_mu[pf=="CD" & est=="DLW" & year==yyy, value := weighted.mean(dt_est[year==yyy]$mu_dlwcd, w = exp(dt_est[year==yyy]$v), na.rm = T)]
    agg_mu[pf=="TL" & est=="OLS" & year==yyy, value := weighted.mean(dt_est[year==yyy]$mu_olstl, w = exp(dt_est[year==yyy]$v), na.rm = T)]
    agg_mu[pf=="TL" & est=="DLW" & year==yyy, value := weighted.mean(dt_est[year==yyy]$mu_dlwtl, w = exp(dt_est[year==yyy]$v), na.rm = T)]
}
ggplot(agg_mu, aes(x = year, y = value, color = est, linetype = pf)) +
    geom_line() +
    geom_point() +
    labs(x = "Year", y = "Aggregate μ", color = "Estimation Method", linetype = "Production Function") +
    scale_linetype_manual(values = c("solid", "dashed")) +
    theme_bw()

#///////////////////////////////////////////////////////////////////////////////

markups <- subset(dt_est, subset=(year==2008 & sec %nin% c("D16","D30")), select=c("id", colnames(dt_est)[grep("mu_", colnames(dt_est))]) )
fwrite(markups, paste0(pathEst, "output/markups_2008_alt.csv"))

