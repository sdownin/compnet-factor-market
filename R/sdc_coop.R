##########################################################################################
#
# AMJ 2018 SPECIAL ISSUE SDC COOPERATIVE RELATIONS DATA
#
# Notes: 
#  - must load 
#
#
##########################################################################################
library(stringr)
library(stringi)
library(stringdist)
library(lubridate)

## DIRECTORIES
data_dir <- "C:/Users/T430/Google Drive/PhD/Dissertation/competition networks/compnet2/SDC_data"
work_dir <- "C:/Users/T430/Google Drive/PhD/Dissertation/competition networks/compnet2"

## SET WORING DIR
setwd(work_dir)

## LOAD AND PREP CRUNCHBASE DATA IF NOT IN MEMORY
if(!('cb' %in% ls())) 
  source(file.path(getwd(),'R','amj_cb_data_prep.R'))

## CACHE ENVIRONMENT to keep when clearing tmp objects added here
## excluding directory variables ending in `_dir`
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
    source(file.path(getwd(),'R','amj_cb_cs_name_map.R')) ## which loads amj_cb_cs_name_map.csv if exists, else creates it in amj_cb_cs_name_map.R
  }
  source(file.path(getwd(),'R','amj_cb_sic_codes.R'))
}


cat('\nloading SDC cooperative relations data...')

##===============================
## Functions
##-------------------------------
## none




##===============================
## SDC COOPERATIVE RELATIONS DATA
##  - after preprocessing by 
##     split_sdc_relations.py script
##-------------------------------

## SET REQUIRED COLUMNS IN SDC DATAFRAMES 
## TO BE USED IN MATCHING AGAINST CRUNCHBASE COMPANIES
req.cols <- c(
  'participant_parent_cusip',
  'parti_cusip',
  'ultimate_parent_cusip',
  'participant_ultimate_parent_cusip'
)

## LOAD SDC DATAFRAMES
sdc.files <- dir(data_dir)[grep('.*SPLIT[\\d]{9,11}.csv$', dir(data_dir), perl = T)]
.ldf <- list()
.cols <- c()
for (sdc.file in sdc.files) {
  .ldf[[sdc.file]] <- cb$readCsv(file.path(data_dir, sdc.file))
  ## keep only intersection of columns from all 
  if(length(.cols)==0) {
    .cols <- names(.ldf[[sdc.file]])
  } else {
    .cols <- intersect(.cols, names(.ldf[[sdc.file]]))
  }
}

## combine all sdc files
sdc <- data.frame()
for (key in names(.ldf)) {
  sdc <- rbind(sdc, .ldf[[key]][,.cols])
}


# cusip.cols <- names(sdc)[grep('cusip',names(sdc))]

## COMPUSTAT CUSIP CODE SUBSTRING 6 to MATHCH
cb$co$company_cusip_6 <- substr(cb$co$company_cusip, 1, 6)

## use only the crunchbase companies that have a matched cusip
tmp.co <- cb$co[!is.na(cb$co$company_cusip_6),c('company_uuid','company_cusip_6')]

## MERGE IN CRUNCHBASE COMPANY UUIDs FROM ALL CUSIP COLUMNS LISTED ABOVE
uuid.col.names <- c()
for (i in 1:length(req.cols)) {
  # cat(sprintf('SDC coop relations in col `%s` ',req.cols[i]))
  req.col <- req.cols[i]
  if (req.col %in% names(sdc)) {
    req.col.uuid <- sprintf('company_uuid_from_%s',req.col)
    uuid.col.names <- c(uuid.col.names, req.col.uuid)
    names(tmp.co)[1] <- req.col.uuid
    sdc <- merge(x=sdc, y=tmp.co, by.x=req.col, by.y='company_cusip_6', all.x=T, all.y=F)
  } else {
    # cat('skipping ')
  }
}

## REDUCE ALL CB UUIDS TO ONE company_uuid COLUMN
sdc$company_uuid <- apply(sdc[,uuid.col.names],1,function(x){
    xna <- na.omit(x)
    if (length(xna)==0) return(NA)
    return(paste(unique(xna),collapse = "|"))
  })

sdc <- sdc[,names(sdc)[which(!(names(sdc)%in%uuid.col.names))]]
sdc <- sdc[order(sdc$coop_id, decreasing = F),]

## reformat date strings to YYYY-MM-DD format
sdc$date_effective <- as.character(mdy(sdc$date_effective))
sdc$date_alliance_terminated <- as.character(mdy(sdc$date_alliance_terminated))
sdc$date_expired <- as.character(mdy(sdc$date_expired))


## add SDC relations to environment exportable as: coop
coop <- sdc
.ls <- c(.ls,'coop')


##===============================
## CLEAR NAMESPACE 
##-------------------------------
## clear tmp objects in environment 
## from this script not to be exported
## when called from external script
.rm <- c('x')
for (x in setdiff(ls(), .ls)) .rm <- c(.rm, x)
rm(list=.rm)
gc()

cat('done.\n')








