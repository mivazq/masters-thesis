#///////////////////////////////////////////////////////////////////////////////
# File name:		DLW.R
# Author:			Miguel Vázquez Vázquez
# Creation date:    23 January 2024
# Description:      This file contains the necessary functions to perform the 
#                   GMM to estimate log productivity based on either Cobb-Douglas
#                   or Translog production functions. For the law of motion of 
#                   productivity it assumes a 3rd degree lag to explain current
#                   period productivity.
#///////////////////////////////////////////////////////////////////////////////

# Generalized method of moments De Loecker-Warzynski (2012)
GMM_DLW <- function(BETAS, PHI, PHI_LAG, Z, X, X_LAG, Y, C) {
    OMEGA      <- PHI     - X     %*% BETAS
    OMEGA_lag  <- PHI_LAG - X_LAG %*% BETAS
    OMEGA_lag_pol <- cbind(C, OMEGA_lag)
    g_b <- solve(t(OMEGA_lag_pol) %*% OMEGA_lag_pol) %*% t(OMEGA_lag_pol) %*% OMEGA # (X'X)⁻¹(X'Y), i.e. regress OMEGA on OMEGA_lag_pol
    XI <- OMEGA - OMEGA_lag_pol %*% g_b # ξ
    crit <- t(t(Z) %*% XI) %*% (t(Z) %*% XI) # moments to be minimized
    return(crit)
}

#///////////////////////////////////////////////////////////////////////////////
#----                   DLW FOR ONE VARIABLE COST (V)                       ----
#///////////////////////////////////////////////////////////////////////////////

# Assumes Cobb-Douglas production function
DLW_CD_V <- function(init_par = as.matrix(c(1, 0.65, 0.35))) {
    S <- optim(par = init_par, 
               fn  = GMM_DLW,
               control = list(maxit=1000),
               PHI     = as.matrix(dt_est[use_2nd & ind_sel]$Phi_hat), 
               PHI_LAG = as.matrix(dt_est[use_2nd & ind_sel]$Phi_hat_lag),
               Z       = as.matrix(dt_est[use_2nd & ind_sel,.(cons, v_lag, k)]),
               X       = as.matrix(dt_est[use_2nd & ind_sel,.(cons, v,     k)]),
               X_LAG   = as.matrix(dt_est[use_2nd & ind_sel,.(cons, v_lag, k_lag)]),
               Y       = as.matrix(dt_est[use_2nd & ind_sel]$y), 
               C       = as.matrix(dt_est[use_2nd & ind_sel]$cons)
    )
    result <- S$par
    msg <- ifelse(S$convergence==0, "Convergence", 
                  ifelse(S$convergence==1, "No convergence (max iter = 1000 reached)", 
                         ifelse(S$convergence==10, "Nelder-Mead simplex degeneracy", "Unknown status")))
    message(paste0("Cobb-Douglas DLW: ",msg," after ",S$counts[1]," iterations of the GMM_DLW function."))
    names(result) <- c("c", "v", "k")
    return(result)
}

# Assumes Translog production function
DLW_TL_V <- function(init_par = as.matrix(c(0, 0, 0, 0, 0, 0))) {
    S <- optim(par = init_par,
               fn  = GMM_DLW,
               control = list(maxit=2000),
               PHI     = as.matrix(dt_est[use_2nd & ind_sel]$Phi_hat),
               PHI_LAG = as.matrix(dt_est[use_2nd & ind_sel]$Phi_hat_lag),
               Z       = as.matrix(dt_est[use_2nd & ind_sel,.(cons, v_lag, k,     v_lag2, k2,     v_lagk)]),
               X       = as.matrix(dt_est[use_2nd & ind_sel,.(cons, v,     k,     v2,     k2,     vk)]),
               X_LAG   = as.matrix(dt_est[use_2nd & ind_sel,.(cons, v_lag, k_lag, v_lag2, k_lag2, v_lagk_lag)]),
               Y       = as.matrix(dt_est[use_2nd & ind_sel]$y),
               C       = as.matrix(dt_est[use_2nd & ind_sel]$cons)
    )
    result <- S$par
    msg <- ifelse(S$convergence==0, "Convergence", 
                  ifelse(S$convergence==1, "No convergence (max iter = 2000 reached)", 
                         ifelse(S$convergence==10, "Nelder-Mead simplex degeneracy", "Unknown status")))
    message(paste0("Translog DLW: ",msg," after ",S$counts[1]," iterations of the GMM_DLW function."))
    names(result) <- c("c", "v", "k", "v2", "k2", "vk")
    return(result)
}

#///////////////////////////////////////////////////////////////////////////////
#----                   DLW FOR TWO VARIABLE COSTS (M,L)                    ----
#///////////////////////////////////////////////////////////////////////////////

# Assumes Cobb-Douglas production function
DLW_CD_ML <- function(init_par = as.matrix(c(1, 0.6, 0.3, 0.1))) {
    S <- optim(par = init_par,
               fn  = GMM_DLW,
               control = list(maxit=1000),
               PHI     = as.matrix(dt_est[use_2nd & ind_sel]$Phi_hat), 
               PHI_LAG = as.matrix(dt_est[use_2nd & ind_sel]$Phi_hat_lag),
               Z       = as.matrix(dt_est[use_2nd & ind_sel,.(cons, m_lag, l_lag, k)]),
               # Z       = as.matrix(dt_est[use_2nd & ind_sel,.(cons, m_lag, l,     k)]), # L as dynamic
               X       = as.matrix(dt_est[use_2nd & ind_sel,.(cons, m,     l,     k)]),
               X_LAG   = as.matrix(dt_est[use_2nd & ind_sel,.(cons, m_lag, l_lag, k_lag)]),
               Y       = as.matrix(dt_est[use_2nd & ind_sel]$y), 
               C       = as.matrix(dt_est[use_2nd & ind_sel]$cons)
    )
    result <- S$par
    msg <- ifelse(S$convergence==0, "Convergence", 
                  ifelse(S$convergence==1, "No convergence (max iter = 1000 reached)", 
                         ifelse(S$convergence==10, "Nelder-Mead simplex degeneracy", "Unknown status")))
    message(paste0("Cobb-Douglas DLW: ",msg," after ",S$counts[1]," iterations of the GMM_DLW function."))
    names(result) <- c("c", "m", "l", "k")
    return(result)
}

# Assumes Translog production function
DLW_TL_ML <- function(init_par = as.matrix(c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0))) {
    S <- optim(par = init_par,
               fn  = GMM_DLW,
               control = list(maxit=2000),
               PHI     = as.matrix(dt_est[use_2nd & ind_sel]$Phi_hat),
               PHI_LAG = as.matrix(dt_est[use_2nd & ind_sel]$Phi_hat_lag),
               Z       = as.matrix(dt_est[use_2nd & ind_sel,.(cons, m_lag, l_lag, k,     m_lag2, l_lag2, k2,     m_lagk,     l_lagk,     m_lagl_lag, m_lagl_lagk)]),
               # Z       = as.matrix(dt_est[use_2nd & ind_sel,.(cons, m_lag, l,     k,     m_lag2, l2,     k2,     m_lagk,     lk,         m_lagl,     m_laglk)]), # L as dynamic
               X       = as.matrix(dt_est[use_2nd & ind_sel,.(cons, m,     l,     k,     m2,     l2,     k2,     mk,         lk,         ml,         mlk)]),
               X_LAG   = as.matrix(dt_est[use_2nd & ind_sel,.(cons, m_lag, l_lag, k_lag, m_lag2, l_lag2, k_lag2, m_lagk_lag, l_lagk_lag, m_lagl_lag, m_lagl_lagk_lag)]),
               Y       = as.matrix(dt_est[use_2nd & ind_sel]$y),
               C       = as.matrix(dt_est[use_2nd & ind_sel]$cons)
    )
    result <- S$par
    msg <- ifelse(S$convergence==0, "Convergence", 
                  ifelse(S$convergence==1, "No convergence (max iter = 2000 reached)", 
                         ifelse(S$convergence==10, "Nelder-Mead simplex degeneracy", "Unknown status")))
    message(paste0("Translog DLW: ",msg," after ",S$counts[1]," iterations of the GMM_DLW function."))
    names(result) <- c("c", "m", "l", "k", "m2", "l2", "k2", "mk", "lk", "ml", "mlk")
    return(result)
}
