#///////////////////////////////////////////////////////////////////////////////
# File name:		percentiles.R
# Current author:	Miguel Vázquez Vázquez
# Description:      This function computes custom percentiles.
#///////////////////////////////////////////////////////////////////////////////

# Custom percentile functions
p001  <- function (x,na.rm=T) quantile(x, prob=c(0.001), na.rm=na.rm)
p01   <- function (x,na.rm=T) quantile(x, prob=c(0.01),  na.rm=na.rm)
p05   <- function (x,na.rm=T) quantile(x, prob=c(0.05),  na.rm=na.rm)
p10   <- function (x,na.rm=T) quantile(x, prob=c(0.10),  na.rm=na.rm)
p25   <- function (x,na.rm=T) quantile(x, prob=c(0.25),  na.rm=na.rm)
p50   <- function (x,na.rm=T) quantile(x, prob=c(0.50),  na.rm=na.rm)
p75   <- function (x,na.rm=T) quantile(x, prob=c(0.75),  na.rm=na.rm)
p90   <- function (x,na.rm=T) quantile(x, prob=c(0.90),  na.rm=na.rm)
p95   <- function (x,na.rm=T) quantile(x, prob=c(0.95),  na.rm=na.rm)
p99   <- function (x,na.rm=T) quantile(x, prob=c(0.99),  na.rm=na.rm)
p999  <- function (x,na.rm=T) quantile(x, prob=c(0.999), na.rm=na.rm)
