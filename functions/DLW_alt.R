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
    OMEGA_lag  <- PHI_LAG - X_LAG %*% BETAS # lag^1
    OMEGA_lag2 <- OMEGA_lag  * OMEGA_lag    # lag^2
    # OMEGA_lag3 <- OMEGA_lag2 * OMEGA_lag    # lag^3
    # OMEGA_lag_pol <- cbind(C, OMEGA_lag, OMEGA_lag2, OMEGA_lag3) # third order is too high, highly collinear with 2nd order "system is computationally singular: reciprocal condition number = 2.20816e-16"
    OMEGA_lag_pol <- cbind(C, OMEGA_lag, OMEGA_lag2)
    g_b <- solve(t(OMEGA_lag_pol) %*% OMEGA_lag_pol) %*% t(OMEGA_lag_pol) %*% OMEGA # invert to get g() function
    XI <- OMEGA - OMEGA_lag_pol %*% g_b # ξ
    crit <- t(t(Z) %*% XI) %*% (t(Z) %*% XI) # moments to be minimized
    return(crit)
}

# Assumes Cobb-Douglas production function
DLW_CD <- function(init_par = as.matrix(c(1, 0.55, 0.35, 0.1))) {
    S <- optim(par = init_par, 
               fn  = GMM_DLW,
               PHI     = as.matrix(dt_est[use_2nd & sec_sel]$Phi_hat), 
               PHI_LAG = as.matrix(dt_est[use_2nd & sec_sel]$Phi_hat_lag),
               Z       = as.matrix(dt_est[use_2nd & sec_sel,.(cons, l_lag, m_lag, k)]),
               X       = as.matrix(dt_est[use_2nd & sec_sel,.(cons, l,     m,     k)]),
               X_LAG   = as.matrix(dt_est[use_2nd & sec_sel,.(cons, l_lag, m_lag, k_lag)]),
               Y       = as.matrix(dt_est[use_2nd & sec_sel]$y), 
               C       = as.matrix(dt_est[use_2nd & sec_sel]$cons)
    )
    result <- S$par
    names(result) <- c("c", "l", "m", "k")
    return(result)
}

# Assumes Translog production function
DLW_TL <- function(init_par = as.matrix(c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0))) {
    S <- optim(par = init_par,
               fn  = GMM_DLW,
               PHI     = as.matrix(dt_est[use_2nd & sec_sel]$Phi_hat),
               PHI_LAG = as.matrix(dt_est[use_2nd & sec_sel]$Phi_hat_lag),
               Z       = as.matrix(dt_est[use_2nd & sec_sel,.(cons, l_lag, m_lag, k,     l_lag2, m_lag2, k2,     l_lagk,     m_lagk,     l_lagm_lag, l_lagm_lagk)]),
               X       = as.matrix(dt_est[use_2nd & sec_sel,.(cons, l,     m,     k,     l2,     m2,     k2,     lk,         mk,         lm,         lmk)]),
               X_LAG   = as.matrix(dt_est[use_2nd & sec_sel,.(cons, l_lag, m_lag, k_lag, l_lag2, m_lag2, k_lag2, l_lagk_lag, m_lagk_lag, l_lagm_lag, l_lagm_lagk_lag)]),
               Y       = as.matrix(dt_est[use_2nd & sec_sel]$y),
               C       = as.matrix(dt_est[use_2nd & sec_sel]$cons)
    )
    result <- S$par
    names(result) <- c("c", "l", "m", "k", "l2", "m2", "k2", "lk", "mk", "lm", "lmk")
    return(result)
}
