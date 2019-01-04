##--------------------------------------------------------------
##
##  compnet-factor-market
##   - Factor Market Interaction with Product Market Competition
##
##  GERGM REGRESSION ANALYSIS
##  TEST 1
##
##--------------------------------------------------------------
# .libPaths('C:/Users/T430/Documents/R/win-library/3.2')
library(igraph)
library(intergraph)

## DIRECTORIES
cb_dir <- "C:/Users/T430/Google Drive/PhD/Dissertation/crunchbase/crunchbase_export_20161024"
work_dir <- "C:/Users/T430/Google Drive/PhD/Dissertation/competition networks/factor_market_rivalry/compnet-factor-market"
data_dir <- file.path(work_dir, 'data')
firm_nets_dir <- file.path(data_dir, 'firm_nets')
img_dir  <- file.path(work_dir, 'img')

## set woring dir
setwd(work_dir)

# ## Crunchbase Data
# cb <- source(file.path(work_dir,'R','cb_data_prep.R'))$value