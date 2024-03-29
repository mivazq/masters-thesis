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

# Generate value added
panel[, value_added := operative_revenues - material_costs]

# Exclude too small sectors
panel <- panel[isic_division %nin% c("A02", "D16", "D23", "D30", "D32", "D35", "D37")]

# Correlations
cor(panel[isic_section=="G"]$material_costs, panel[isic_section=="G"]$operative_revenues, use="pairwise")
cor(panel[isic_section=="G"]$labour_costs,   panel[isic_section=="G"]$operative_revenues, use="pairwise")
cor(panel[isic_section=="G"]$variable_costs, panel[isic_section=="G"]$operative_revenues, use="pairwise")

cor(log(panel[isic_section=="G"]$material_costs), log(panel[isic_section=="G"]$operative_revenues), use="pairwise")
cor(log(panel[isic_section=="G"]$labour_costs),   log(panel[isic_section=="G"]$operative_revenues), use="pairwise")
cor(log(panel[isic_section=="G"]$variable_costs), log(panel[isic_section=="G"]$operative_revenues), use="pairwise") 

# Exclude outliers
# panel[, v_ratio := variable_costs/operative_revenues]
# panel[, v_ratio_99 := p99(v_ratio), by = year]
# panel[, v_ratio_01 := p01(v_ratio), by = year]
# panel <- panel[!(v_ratio>v_ratio_99)]
# panel <- panel[!(v_ratio<v_ratio_01)]

panel[, m_ratio := material_costs/operative_revenues]
panel[, m_ratio_99 := p99(m_ratio), by = year]
panel[, m_ratio_01 := p01(m_ratio), by = year]
panel[, l_ratio := labour_costs/operative_revenues]
panel[, l_ratio_99 := p99(l_ratio), by = year]
panel[, l_ratio_01 := p01(l_ratio), by = year]
panel <- panel[!(m_ratio>m_ratio_99 | l_ratio>l_ratio_99)]
panel <- panel[!(m_ratio<m_ratio_01 | l_ratio<l_ratio_01)]


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
                    sec = isic_division,
                    # div = isic_division,
                    # e = is_exporter,
                    y = log(operative_revenues), 
                    k = log(fixed_assets), 
                    l = log(labour_costs),
                    m = log(material_costs),
                    v = log(variable_costs))]

# Create new variables needed for translog production function
dt_est[, `:=` (v2 = v^2,
               k2 = k^2,
               vk = v * k)]

# Create table to store OLS estimates by sector which will be used as initial values
# for GMM.
sectors <- sort(unique(dt_est$sec))
dt_init <- data.table(sec = rep(sectors,2),
                      pf  = c(rep("CD",length(sectors)), rep("TL",length(sectors))))

# Iterate over industries 
for (ind in sort(unique(dt_est$sec))) {
    
    # Logical vector for sector selection and first stage selection
    sec_sel <- dt_est$sec==ind
    use_1st  <- !is.na(dt_est$y)
    
    message("ESTIMATING FIRST STAGE FOR DIVISION ", ind)
    
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
        use_2nd  <- !is.na(dt_est$v) & !is.na(dt_est$v_lag) & !is.na(dt_est$Phi_hat) & !is.na(dt_est$Phi_hat_lag)
        
        # OLS estimates on Cobb Douglas production function
        olscd_model <- feols(y ~ v + k | year, 
                             data = dt_est, subset = use_2nd & sec_sel, panel.id=c("id","year"))
        dt_est[sec_sel, olscd := list(olscd_model$coefficients)]
        
        # OLS estimates on Translog production function
        olstl_model <- feols(y ~ v + k + v2 + k2 + vk | year, 
                             data = dt_est, subset = use_2nd & sec_sel, panel.id=c("id","year"))
        dt_est[sec_sel, olstl := list(olstl_model$coefficients)]
        
        # Construct initial parameters for GMM as combination of GNR (50%) and OLS (50%)
        dt_init[sec==ind & pf=="CD", names(as.list(olscd_model$coefficients)) := as.list(olscd_model$coefficients)]
        # init_CD_v = 0.91*0.5 + dt_init[sec==ind & pf=="CD"]$v*0.5 # 0.91 is average in GNR between Colombia and Chile across all manufacturing
        # init_CD_k = 0.15*0.5 + dt_init[sec==ind & pf=="CD"]$k*0.5 # 0.15 is average in GNR between Colombia and Chile across all manufacturing
        init_CD_v = 0.91*0 + dt_init[sec==ind & pf=="CD"]$v*1 # 0.91 is average in GNR between Colombia and Chile across all manufacturing
        init_CD_k = 0.15*0 + dt_init[sec==ind & pf=="CD"]$k*1 # 0.15 is average in GNR between Colombia and Chile across all manufacturing
        dt_init[sec==ind & pf=="TL", names(as.list(olstl_model$coefficients)) := as.list(olstl_model$coefficients)]
        
        # ACF estimates using DLW method on Cobb Douglas production function
        dt_est[sec_sel, dlwcd := list(DLW_CD(init_par = c(1, init_CD_v, init_CD_k)))]
        # dt_est[sec_sel, dlwcd := list(DLW_CD(init_par = c(1, shares[isic_division==ind]$V_share, shares[isic_division==ind]$K_share)))]
        
        # ACF estimates using DLW method on Translog production function
        dt_est[sec_sel, dlwtl := list(DLW_TL(init_par = c(0, 0, 0, 0, 0, 0)))]
        
    message("\n")
}

rm(fs_model, olscd_model, olstl_model, sec_sel, ind)

#///////////////////////////////////////////////////////////////////////////////
#----               4 - COMPUTE MARK-UPS AND PRODUCTIVITIES                 ----
#///////////////////////////////////////////////////////////////////////////////

# Extract individual betas from coefficient lists
dt_est[, beta_v_olscd := sapply(olscd, function(x) x[["v"]])]
dt_est[, beta_k_olscd := sapply(olscd, function(x) x[["k"]])]
dt_est[, olscd := NULL]
dt_est[, beta_v_dlwcd := sapply(dlwcd, function(x) x[["v"]])]
dt_est[, beta_k_dlwcd := sapply(dlwcd, function(x) x[["k"]])]
dt_est[, dlwcd := NULL]
dt_est[, beta_v1_olstl := sapply(olstl, function(x) x[["v"]])]
dt_est[, beta_k1_olstl := sapply(olstl, function(x) x[["k"]])]
dt_est[, beta_v2_olstl := sapply(olstl, function(x) x[["v2"]])]
dt_est[, beta_k2_olstl := sapply(olstl, function(x) x[["k2"]])]
dt_est[, beta_vk_olstl := sapply(olstl, function(x) x[["vk"]])]
dt_est[, olstl := NULL]
dt_est[, beta_v1_dlwtl := sapply(dlwtl, function(x) x[["v"]])]
dt_est[, beta_k1_dlwtl := sapply(dlwtl, function(x) x[["k"]])]
dt_est[, beta_v2_dlwtl := sapply(dlwtl, function(x) x[["v2"]])]
dt_est[, beta_k2_dlwtl := sapply(dlwtl, function(x) x[["k2"]])]
dt_est[, beta_vk_dlwtl := sapply(dlwtl, function(x) x[["vk"]])]
dt_est[, dlwtl := NULL]

# Compute log TFP
dt_est[, tfp_olscd := y       - beta_v_olscd*v  - beta_k_olscd*k]
dt_est[, tfp_olstl := y       - beta_v1_olstl*v - beta_k1_olstl*k - beta_v2_olstl*v2 - beta_k2_olstl*k2 - beta_vk_olstl*vk]
dt_est[, tfp_dlwcd := Phi_hat - beta_v_dlwcd*v  - beta_k_dlwcd*k]
dt_est[, tfp_dlwtl := Phi_hat - beta_v1_dlwtl*v - beta_k1_dlwtl*k - beta_v2_dlwtl*v2 - beta_k2_dlwtl*k2 - beta_vk_dlwtl*vk]

# Compute mark-ups
dt_est[, alpha_v  := exp(v) / exp(Phi_hat)] # corrected share of variable inputs (using expected output Ф instead of realised y)
dt_est[, beta_v_olstl := (1*beta_v1_olstl + 2*v*beta_v2_olstl + k*beta_vk_olstl)] # translog total output elasticity of variable inputs
dt_est[, beta_v_dlwtl := (1*beta_v1_dlwtl + 2*v*beta_v2_dlwtl + k*beta_vk_dlwtl)] # translog total output elasticity of variable inputs
dt_est[, mu_olscd := beta_v_olscd / alpha_v]
dt_est[, mu_dlwcd := beta_v_dlwcd / alpha_v]
dt_est[, mu_olstl := beta_v_olstl / alpha_v]
dt_est[, mu_dlwtl := beta_v_dlwtl / alpha_v]

#///////////////////////////////////////////////////////////////////////////////
#----                           5 - OUTPUT                                  ----
#///////////////////////////////////////////////////////////////////////////////

# Table with number of observations/firms per sector available in whole panel, used
# for first stage and second stage. First stage = simply active observations.
df_sections <- fread(paste0(pathCle, "output/isic_codes_division.csv"))
n_table <- df_sections[substr(isic_division,1,1) %in% c("A","B","D","F","G"), .(desc=isic_division_desc, sec=isic_division)]
n_table <- n_table[sec %nin% c("A02", "D16", "D23", "D30", "D32", "D35", "D37")]
n_table[, full_panel_obs  := table(factor(dt_est$sec, levels = n_table$sec))]
n_table[, full_panel_ids  := table(factor(unique(dt_est[,.(id,sec)])$sec, levels = n_table$sec))]
n_table[, `1st_stage_obs` := table(factor(dt_est[use_1st]$sec, levels = n_table$sec))]
n_table[, `1st_stage_ids` := table(factor(unique(dt_est[use_1st,.(id,sec)])$sec, levels = n_table$sec))]
n_table[, `2nd_stage_obs` := table(factor(dt_est[use_2nd]$sec, levels = n_table$sec))]
n_table[, `2nd_stage_ids` := table(factor(unique(dt_est[use_2nd,.(id,sec)])$sec, levels = n_table$sec))]
write_xlsx(n_table, paste0(pathEst, "output/sample_div.xlsx"))


# Table with elasticities
res <- unique(dt_est[,.(sec, beta_v_dlwcd, beta_k_dlwcd)])
setorder(res, sec)
if (!all.equal(res$sec,shares$isic_division)) {
    message("NOT IN SAME ORDER")
}
elast_cd <- data.table(industry  = shares$isic_division,
                       # initial_v = shares$V_share,
                       # initial_k = shares$K_share,
                       initial_v = dt_init[pf=="CD"]$v,
                       initial_k = dt_init[pf=="CD"]$k,
                       # initial_v = 0.91*0.5 + dt_init[pf=="CD"]$v*0.5,
                       # initial_k = 0.15*0.5 + dt_init[pf=="CD"]$k*0.5,
                       final_v   = res$beta_v_dlwcd,
                       final_k   = res$beta_k_dlwcd)
elast_cd <- elast_cd[industry %nin% c("A02", "D16", "D23", "D30", "D32", "D35", "D37")]
elast_cd[, n := n_table$`2nd_stage_ids`]
write_xlsx(elast_cd, paste0(pathEst, "output/elasticities.xlsx"))



for (pf in c("CD","TL")) {
    ### OLS
    dist_table_ols <- dcast.data.table(data = dt_est[use_1st], 
                                       formula = sec ~.,
                                       fun = list(mean, sd, min, p10, p25, p50, p75, p90, max),
                                       na.rm = T,
                                       value.var = paste0("mu_ols",tolower(pf)))
    setnames(dist_table_ols, 2:10, c("mean","sd","min","p10", "p25", "p50", "p75", "p90", "max"))
    dist_table_ols[, est := "OLS"]
    
    ### DLW
    dist_table_dlw <- dcast.data.table(data = dt_est[use_1st], 
                                       formula = sec ~.,
                                       fun = list(mean, sd, min, p10, p25, p50, p75, p90, max),
                                       na.rm = T,
                                       value.var = paste0("mu_dlw",tolower(pf)))
    setnames(dist_table_dlw, 2:10, c("mean","sd","min","p10", "p25", "p50", "p75", "p90", "max"))
    dist_table_dlw[, est := "DLW"]
    
    # Combine OLS and DLW, sort, write to Excel
    dist_table <- rbind(dist_table_ols, dist_table_dlw)
    setorder(dist_table, "sec", "est")
    dist_table <- dist_table[, .(sec, est, mean, sd, min, p10, p25, p50, p75, p90, max)]
    dist_table <- dist_table[sec %nin% c("A02", "D16", "D23", "D30", "D32", "D35", "D37")]
    write_xlsx(dist_table, paste0(pathEst, "output/",pf,"_markups_div.xlsx"))
}


# Check correlations between log TFP and outputs, inputs
tfp_cor <- data.table(pf  = c(rep("CD",2), rep("TL", 2)),
                      est = rep(c(rep("OLS",1),rep("DLW",1)),2),
                      y   = 0,
                      v   = 0,
                      k   = 0)
tfp_cor[pf=="CD" & est=="OLS", c("y", "v", "k") := list(cor(dt_est$tfp_olscd, dt_est$y, use="pairwise"),
                                                        cor(dt_est$tfp_olscd, dt_est$v, use="pairwise"),
                                                        cor(dt_est$tfp_olscd, dt_est$k, use="pairwise"))]
tfp_cor[pf=="CD" & est=="DLW", c("y", "v", "k") := list(cor(dt_est$tfp_dlwcd, dt_est$Phi_hat, use="pairwise"),
                                                        cor(dt_est$tfp_dlwcd, dt_est$v, use="pairwise"),
                                                        cor(dt_est$tfp_dlwcd, dt_est$k, use="pairwise"))]
tfp_cor[pf=="TL" & est=="OLS", c("y", "v", "k") := list(cor(dt_est$tfp_olstl, dt_est$y, use="pairwise"),
                                                        cor(dt_est$tfp_olstl, dt_est$v, use="pairwise"),
                                                        cor(dt_est$tfp_olstl, dt_est$k, use="pairwise"))]
tfp_cor[pf=="TL" & est=="DLW", c("y", "v", "k") := list(cor(dt_est$tfp_dlwtl, dt_est$Phi_hat, use="pairwise"),
                                                        cor(dt_est$tfp_dlwtl, dt_est$v, use="pairwise"),
                                                        cor(dt_est$tfp_dlwtl, dt_est$k, use="pairwise"))]




# Construct aggregate mark-up and plot evolution
agg_mu <- data.table(pf   = c(rep("CD",2), rep("TL", 2)),
                     est  = rep(c(rep("OLS",1),rep("DLW",1)),2),
                     year = c(rep(2008,4),rep(2009,4),rep(2010,4),rep(2011,4)))
for (yyy in 2008:2011) {
    agg_mu[pf=="CD" & est=="OLS" & year==yyy, value := weighted.mean(dt_est[year==yyy]$mu_olscd, w = exp(dt_est[year==yyy]$v), na.rm = T)]
    agg_mu[pf=="CD" & est=="DLW" & year==yyy, value := weighted.mean(dt_est[year==yyy]$mu_dlwcd, w = exp(dt_est[year==yyy]$v), na.rm = T)]
    agg_mu[pf=="TL" & est=="OLS" & year==yyy, value := weighted.mean(dt_est[year==yyy]$mu_olstl, w = exp(dt_est[year==yyy]$v), na.rm = T)]
    agg_mu[pf=="TL" & est=="DLW" & year==yyy, value := weighted.mean(dt_est[year==yyy]$mu_dlwtl, w = exp(dt_est[year==yyy]$v), na.rm = T)]
}
rm(yyy)
ggplot(agg_mu, aes(x = year, y = value, color = est, linetype = pf)) +
    geom_line() +
    geom_point() +
    labs(x = "Year", y = "Aggregate μ", color = "Estimation Method", linetype = "Production Function") +
    scale_linetype_manual(values = c("solid", "dashed")) +
    theme_bw()
ggsave(paste0(pathFig, sysdate, "_aggregate_markup.pdf"))

#///////////////////////////////////////////////////////////////////////////////

markups <- subset(dt_est, subset=(year==2008 & sec %nin% c("D16","D30")), select=c("id", colnames(dt_est)[grep("mu_", colnames(dt_est))]) )
fwrite(markups, paste0(pathEst, "output/markups_2008.csv"))




