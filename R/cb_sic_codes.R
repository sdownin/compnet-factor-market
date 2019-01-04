##########################################################################################
#
# AMJ 2018 SPECIAL ISSUE SIC CODES
#
# Notes: 
#  - must load 
#
#
##########################################################################################
library(stringr)
library(stringi)
library(stringdist)

## DIRECTORIES
data_dir <- "C:/Users/T430/Google Drive/PhD/Dissertation/crunchbase/crunchbase_export_20161024"
work_dir <- "C:/Users/T430/Google Drive/PhD/Dissertation/competition networks/compnet2"

## SET WORKING DIR
setwd(work_dir)

## LOAD AND PREP CRUNCHBASE DATA IF NOT IN MEMORY
if(!('cb' %in% ls())) 
  source(file.path(getwd(),'R','amj_cb_data_prep.R'))

## CACHE ENVIRONMENT to keep when clearing tmp objects added here
## excluding directories ending in `_dir`
.ls <- ls()[grep('(?<!_dir)$',ls(),perl = T)]

## CrunchBase-Compustat name mapping
namemap.filename <- 'amj_cb_cs_name_map.csv'
cs.filename <- 'amj_cs_names.csv'

if(!('namemap' %in% names(cb)))  {
  if (namemap.filename %in% dir()) {
    cb$namemap <- cb$readCsv(namemap.filename)
    cs <- cb$readCsv(cs.filename)
    .ls <- c(.ls,'cs')
  } else {
    ## may take several hours to run; load from file instead, if possible
    source(file.path(getwd(),'R','amj_cb_cs_name_map.R'))
  }
}

cat('\nloading SIC codes...')

##===============================
## Functions
##-------------------------------


##===============================
## EXPORT INTO CB NAMESPACE
##-------------------------------
## subset checked firms:   the companies are matched below tolorance or checked manually
cb$namemap.ck <- cb$namemap[cb$namemap$is_matched | cb$namemap$is_checked_manually, ]

##===============================
## MERGE SIC CODES 
##-------------------------------
tmp <- cb$namemap.ck[,c('cb_uuid','cb_name','cs_name','cs_gvkey','cs_cusip')]
names(tmp) <- c('company_uuid','company_matched_name','company_compustat_name','company_gvkey','company_cusip')
cb$co <- merge(x=cb$co, y=tmp, by.x='company_uuid', by.y='company_uuid', all.x=T, all.y=F)

tmp <- cs[,c('gvkey','sic')]
names(tmp) <- c('gvkey','company_sic')
cb$co <- merge(x=cb$co, y=tmp, by.x='company_gvkey', by.y='gvkey', all.x=T, all.y=F)

## add compustat dataframe cs to environment exportable
.ls <- c(.ls,'cs')


##===============================
## CLEAR ENVIRONMENT
##-------------------------------
## clear tmp objects in environment 
## from this script not to be exported
## when called from external script
.rm <- c('x')
for (x in setdiff(ls(), .ls)) .rm <- c(.rm, x)
rm(list=.rm)
gc()

cat('done.\n')











