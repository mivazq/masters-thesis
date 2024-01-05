#///////////////////////////////////////////////////////////////////////////////
# File name:		fp.R
# Current author:	Miguel Vázquez Vázquez
# Description:      This function prints rounded, formatted coefficients to have
#					the same length (keeps trailing zeros).
#
# Proofreader:	  	
# Proofread date:	  
#///////////////////////////////////////////////////////////////////////////////

# Format print
fp <- function(value, dig=0) {
	return(formatC(round(value, dig), format='f', digits=dig, big.mark=","))
}

# Format print for percentuals
fpp <- function(value, dig) {
	acc <- 0.1^dig
	perc <- percent(value, accuracy=acc)
	perc_nchar <- nchar(perc)
	perc_nosign <- substr(perc, 1, perc_nchar-1)
	return(paste0(perc_nosign,"\\%"))
}

# Format print for percentuals in parentheses
fppp <- function(value, dig) {
	acc <- 0.1^dig
	perc <- percent(value, accuracy=acc)
	perc_nchar <- nchar(perc)
	perc_nosign <- substr(perc, 1, perc_nchar-1)
	return(paste0("(", perc_nosign,"\\%)"))
}

# Format print for values in parentheses
fpt <- function(value, dig) {
	return(paste0("(", formatC(round(value, dig), format='f', digits=dig, big.mark=","),")"))
}

# Format print for values in brackets
fpb <- function(value, dig) {
	return(paste0("[", formatC(round(value, dig), format='f', digits=dig, big.mark=","),"]"))
}

# Format print for values in brackets
fpci <- function(l_value, u_value, dig) {
	return(paste0("[", formatC(round(l_value, dig), format='f', digits=dig, big.mark=","),
				  ", ",formatC(round(u_value, dig), format='f', digits=dig, big.mark=","),"]"))
}