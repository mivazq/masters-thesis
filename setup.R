#///////////////////////////////////////////////////////////////////////////////
# File name:		setup.R
# Author:			Miguel Vázquez Vázquez
# Creation date:    20 November 2023
# Description:      This file installs packages, defines paths, loads functions
#///////////////////////////////////////////////////////////////////////////////
#----                       1 - PACKAGES AND OPTIONS                        ----
#///////////////////////////////////////////////////////////////////////////////

# List needed packages
packages_needed <- c('writexl',                                                 # To export to Excel
                     'readxl',                                                  # To read from Excel
                     'haven',                                                   # To import files from other software (Stata, SPSS, ...)
                     'data.table',                                              # Improved syntax for data manipulation
                     'gdata',   											    # Various R programming tools for data manipulation
                     'ggplot2',                                                 # Beautiful plotting
                     'tidyr',                                                   # Other tools for data manipulation
                     'scales',                                                  # To handle very big numbers
                     'bit64',                                                   # To handle very big numbers
                     'Matrix',                                                  # (Sparse) matrix manipulation
                     'lsa',                                                     # For cosine-similarity
                     'fixest',                                                  # For fixed-effects estimation
                     'plm',                                                     # Linear models for panel data
                     'modelsummary'                                             # Summary of models
                     )
					 
# Get list of installed packages
installedPackages <- installed.packages()[,1]

# Install needed packages that are not already installed
for (pack in packages_needed) {
  if(!is.element(pack, installedPackages)) { 
    install.packages(pack)}
}

# Load all needed packages
lapply(packages_needed, library, character.only=T, quietly=T)

# Disable scientific notation
options(scipen = 999)

# Remove all R objects
rm(list = ls())

# Store date to save files
sysdate = format(Sys.Date())

# Clear console
cat("\014") # Credit: https://stackoverflow.com/a/16084793

#///////////////////////////////////////////////////////////////////////////////
#----                               2 - PATHS                               ----
#///////////////////////////////////////////////////////////////////////////////

# Main paths for Ecuador project
ecuRaw  <- '~/data/transactions_ecuador/1_rawdata/'
ecuAll  <- '~/data/transactions_ecuador/2_shared/'
ecuFCT  <- paste0(ecuAll, 'factorContentTrade/')
ecuMine <- '~/data/transactions_ecuador/3_mivazq/'

# Set working directory
pathWD <- paste0(ecuMine, 'Masters_Thesis/')
setwd(pathWD)

# Project sub-folders
pathCle <- paste0(pathWD, 'cleaning/')
pathEst <- paste0(pathWD, 'estimation/')
pathFun <- paste0(pathWD, 'functions/')

# Output files
pathFig <- paste0(pathWD, 'results/figures/')
pathTab <- paste0(pathWD, 'results/tables/')
pathItx <- paste0(pathWD, 'results/intext/')

#///////////////////////////////////////////////////////////////////////////////
#----                           3 - FUNCTIONS                               ----
#///////////////////////////////////////////////////////////////////////////////

# If functions folder is not empty, source all functions in the folder
if (all.equal(character(0), list.files(path = pathFun, pattern = '.R')) != TRUE) {
    lapply(paste0(pathFun, list.files(path = pathFun, pattern = '.R')), source)
}

# Create operator for "not in"
`%nin%` <- Negate(`%in%`)
