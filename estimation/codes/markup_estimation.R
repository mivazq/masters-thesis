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
#----                         1 - LOAD FINAL PANEL                          ----
#///////////////////////////////////////////////////////////////////////////////

# Load data
df_tax_filings <- fread(file=paste0(pathCle, "output/tax_filings.csv"), na.strings="")
df_firm_info   <- fread(file=paste0(pathCle, "output/firm_info.csv"), na.strings="")
df_deflators   <- fread(file=paste0(pathCle, "output/deflators.csv"), na.strings="")

# Merge tax filings panel with firm industry information
panel <- merge(df_tax_filings, df_firm_info[,.(id_sri, soe, isic_section, isic_class)], by="id_sri", all.x=T)
panel <- panel[isic_section %nin% c("P","Q","R","S","T")]

# Merge price deflators into panel
panel <- merge(panel, df_deflators, by=c("year","isic_class"), all.x=T) # when specific ISIC



# Olley Pakes -------------------------------------------------------------

df <- read.csv("PS1_Data_Olley_Pakes.csv")

# OLS
ols_model <- lm(Y ~ K + L, data = df)
modelsummary(ols_model)
within = feols(Y ~ K + L | firm, data = df)
modelsummary(within)
plm_fd = plm(Y ~ K + L + firm, data = df, model = 'fd')
modelsummary(plm_fd, coef_map = cm)

## First stage OP

OP_1 = feols(Y ~ L + I  + K + Isq + Ksq + K*I + as.factor(year) , data=df )

B_l = OP_1$coefficients[2]
cm = c('K' = 'Capital', 'L'= 'Labor', 'I' = 'Investment',
       'Ksq' = 'Capital squared', 'Isq' = 'Investment squared',
       'I:K' = 'InvestmentXCapital', '(Intercept)'= 'Intercept')
modelsummary(OP_1, title = "OP 1st stage", 
             stars = c('*' = 0.1 , '**'=0.05, '***'=0.01), coef_map = cm, 
             gof_omit = 'DF|Deviance|(R2.[A-Z]*)|AIC|BIC|Lik')
modelsummary(OP_1, title = "OP 1st stage", 
             stars = c('*' = 0.1 , '**'=0.05, '***'=0.01), coef_map = cm, 
             gof_omit = 'DF|Deviance|(R2.[A-Z]*)|AIC|BIC|Lik',
             output = 'OP_1stage.tex' )

#### b Phi estimation ####

df = df %>% mutate( Phi = OP_1$coefficients[1] + OP_1$coefficients[3]*I +
                        OP_1$coefficients[4]*K +
                        OP_1$coefficients[5]*Isq + OP_1$coefficients[6]*Ksq +
                        OP_1$coefficients[16]*K*I) %>%
    ungroup() 
df <- df %>% group_by(firm) %>%
    mutate(Phi_lag = dplyr::lag(Phi), K_lag = dplyr::lag(K)) %>% ungroup()


#### c  OP 2nd stage #### 
df = df %>% dummy_cols(select_columns = 'year')
#df = df %>% mutate(constant = 1)
OP_2 = nls(Y - B_l*L  ~ b_0 + b_k*K + b_1* (Phi_lag - b_k*(K_lag)) +
               b_2* (((Phi_lag) - b_k*(K_lag))^2) + 
               fe3*year_3 + fe4*year_4 + fe5*year_5 + fe6*year_6 + fe7*year_7 +
               fe8*year_8 +fe9*year_9 + fe10*year_10, data=df ,
           start=list(b_0=0, b_k = 0.2, b_1 = 0.5,  b_2 = 0.25,
                      fe3 = 1, fe4 = 1, fe5=1, fe6=1, fe7=1,
                      fe8=1, fe9=9, fe10=9))
OP_2
modelsummary(OP_2, title = "OP 2nd stage", 
             stars = c('*' = 0.1 , '**'=0.05, '***'=0.01), 
             coef_map = c('b_0' = 'Intercept','b_k' = 'Capital', 
                          'b_1'= "$(Phi_{t-1} - B_{k}*K_{t-1})$",
                          'b_2' = '$(Phi_{t-1} - B_k*K_{t-1})^2$'),
             gof_omit = 'DF|Deviance|(R2.[A-Z]*)|AIC|BIC|Lik|finTol|isConv')
# Export results
modelsummary(OP_2, title = "OP 2nd stage", 
             stars = c('*' = 0.1 , '**'=0.05, '***'=0.01), 
             gof_omit = 'DF|Deviance|(R2.[A-Z]*)|AIC|BIC|Lik|finTol|isConv',
             output = 'OP_2ndstage.tex' )

# Adding third degree term in polynomial does not change results
OP_2b = nls(Y - B_l*L  ~ b_0 + b_k*K + b_1* (Phi_lag - b_k*(K_lag)) +
                b_2* (((Phi_lag) - b_k*(K_lag))^2) +
                b_3* (((Phi_lag) - b_k*(K_lag))^3) +
                fe3*year_3 + fe4*year_4 + fe5*year_5 + fe6*year_6 + fe7*year_7 +
                fe8*year_8 +fe9*year_9 + fe10*year_10, data=df ,
            start=list(b_0=0, b_k = 0.2, b_1 = 0.5,  b_2 = 0.25, b_3 = 0.01,
                       fe3 = 1, fe4 = 1, fe5=1, fe6=1, fe7=1,
                       fe8=1, fe9=9, fe10=9))
OP_2b

# Other way to export results to Latex
stargazer(ols_model,
          out = 'ols_q2.tex', title = 'OLS estimation')
