#///////////////////////////////////////////////////////////////////////////////
# File name:		db.R
# Current author:	Miguel Vázquez Vázquez
# Description:      This function calculates quantiles of distributions.
#
# Proofreader:	  	
# Proofread date:	  
#///////////////////////////////////////////////////////////////////////////////

db <- function(object, kind = 'upper', nonzero = F, stata = 0) {
	
	if (stata == 0) {
		if (nonzero == T) {
			if (kind == 'upper') {
				return(quantile(object[object!=0], c(0,             .10,.25,.50,.75,.90,.95,.99,.999,1)))
			}
			if (kind == 'lower') {
				return(quantile(object[object!=0], c(0,.001,.01,.05,.10,.25,.50,.75,.90,             1)))
				}
			if (kind == 'standard') {
				return(quantile(object[object!=0], c(0,                 .25,.50,.75,                 1)))
			}
		}
			
		if (nonzero == F) {
			if (kind == 'upper') {
				return(quantile(object, c(0,             .10,.25,.50,.75,.90,.95,.99,.999,1)))
			}
			if (kind == 'lower') {
				return(quantile(object, c(0,.001,.01,.05,.10,.25,.50,.75,.90,             1)))
				}
			if (kind == 'standard') {
				return(quantile(object, c(0,                 .25,.50,.75,                 1)))
			}
		}
	}
	if (stata == 1) {
		return(quantile(object, c(0,.01,.05,.10,.25,.50,.75,.90,.95,.99,1)))
	}
	
}
