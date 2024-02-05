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
    # OMEGA_lag_pol <- cbind(C, OMEGA_lag, OMEGA_lag2, OMEGA_lag3) # polynomial of degree 3 of lag terms
    OMEGA_lag_pol <- cbind(C, OMEGA_lag, OMEGA_lag2)
    g_b <- solve(t(OMEGA_lag_pol) %*% OMEGA_lag_pol) %*% t(OMEGA_lag_pol) %*% OMEGA # invert to get g() function
    XI <- OMEGA - OMEGA_lag_pol %*% g_b # ξ
    crit <- t(t(Z) %*% XI) %*% (t(Z) %*% XI) # moments to be minimized
    return(crit)
}

# Assumes Cobb-Douglas production function
DLW_CD <- function(init_par = as.matrix(c(1, 0.65, 0.35))) {
    S <- optim(par = init_par, 
               fn  = GMM_DLW,
               PHI     = as.matrix(dt_est[use_2nd & sec_sel]$Phi_hat), 
               PHI_LAG = as.matrix(dt_est[use_2nd & sec_sel]$Phi_hat_lag),
               Z       = as.matrix(dt_est[use_2nd & sec_sel,.(cons, v_lag, k)]),
               X       = as.matrix(dt_est[use_2nd & sec_sel,.(cons, v,     k)]),
               X_LAG   = as.matrix(dt_est[use_2nd & sec_sel,.(cons, v_lag, k_lag)]),
               Y       = as.matrix(dt_est[use_2nd & sec_sel]$y), 
               C       = as.matrix(dt_est[use_2nd & sec_sel]$cons)
    )
    result <- S$par
    msg <- ifelse(S$convergence==0, "Convergence", 
                  ifelse(S$convergence==1, "No convergence (max iter = 500 reached)", 
                         ifelse(S$convergence==10, "Nelder-Mead simplex degeneracy", "Unknown status")))
    message(paste0("Cobb-Douglas DLW: ",msg," after ",S$counts[1]," iterations of the GMM_DLW function."))
    names(result) <- c("c", "v", "k")
    return(result)
}

# Assumes Translog production function
DLW_TL <- function(init_par = as.matrix(c(0, 0, 0, 0, 0, 0))) {
    S <- optim(par = init_par,
               fn  = GMM_DLW,
               control = list(maxit=1000),
               PHI     = as.matrix(dt_est[use_2nd & sec_sel]$Phi_hat),
               PHI_LAG = as.matrix(dt_est[use_2nd & sec_sel]$Phi_hat_lag),
               Z       = as.matrix(dt_est[use_2nd & sec_sel,.(cons, v_lag, k,     v_lag2, k2,     v_lagk)]),
               X       = as.matrix(dt_est[use_2nd & sec_sel,.(cons, v,     k,     v2,     k2,     vk)]),
               X_LAG   = as.matrix(dt_est[use_2nd & sec_sel,.(cons, v_lag, k_lag, v_lag2, k_lag2, v_lagk_lag)]),
               Y       = as.matrix(dt_est[use_2nd & sec_sel]$y),
               C       = as.matrix(dt_est[use_2nd & sec_sel]$cons)
    )
    result <- S$par
    msg <- ifelse(S$convergence==0, "Convergence", 
                  ifelse(S$convergence==1, "No convergence (max iter = 1000 reached)", 
                         ifelse(S$convergence==10, "Nelder-Mead simplex degeneracy", "Unknown status")))
    message(paste0("Translog DLW: ",msg," after ",S$counts[1]," iterations of the GMM_DLW function."))
    names(result) <- c("c", "v", "k", "v2", "k2", "vk")
    return(result)
}
