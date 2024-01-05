#///////////////////////////////////////////////////////////////////////////////
#----                                Preamble                               ----
# File name:        leontief_degree.R
# Previous author:  Wouter Leenders
# Current author:   Miguel Vázquez Vázquez
# Creation date:    11 February 2020
# Description:      This file multiplies a specified multiplicand, be it a vector 
#                   or a matrix, with the approximation of the Leontief inverse 
#                   matrix (input-output matrix).
#                   
# Input:            
#                   leontief matrix, multiplicand(vector or matrix), order, degree
# Output:           
#                   In case of default order "lm":
#                   (I - leontief)^(-1) * multiplicand; but approximated as:
#                   (I + leontief + leontief^2 + ...) * multiplicand
#                   In the other case, order "ml":
#                   multiplicand * (I - leontief)^(-1); but approximated as:
#                   multiplicand * (I + leontief + leontief^2 + ...)
#                   
# Proofreader:      Juan David Hernández Leal
# Proofread date:   13 December 2020
#///////////////////////////////////////////////////////////////////////////////

leontief_degree <- function(leontief, multiplicand, order = "lm", degree = 10){
    
    # Set degree counter
    d <- 1
    
    # Assign multiplicand to result, temporarily
    result <- multiplicand
    
    # Compute first product
    # Order is the order in which the user writes the 'leontief' and 'multiplicand' arguments
    if(order == "lm"){
        product <- leontief %*% multiplicand
    } else if(order == "ml"){
        product <- multiplicand %*% leontief
    }
    
    # Loop to compute all degrees
    while(d <= degree){
        print(paste0('Computing degree ',d))
        
        # Sum product of multiplicand and leontief to previous result
        result <- result + product
        
        # Compute next product
        if(order == "lm"){
            product <- leontief %*% product
        } else if(order == "ml"){
            product <- product  %*% leontief
        }
        
        # Increase counter 
        d <- d + 1
    }
    
    # Return result
    return(result)
}