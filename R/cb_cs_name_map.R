##########################################################################################
#
# AMJ 2018 SPECIAL ISSUE CREATE CRUNCHBASE-COMPUSTAT FIRM NAME MAPPING
#
# Notes: 
#  - must load CrunchBase companies dataframe ("co") first -- use script cb_data_prep.R
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


cat('\nloading SIC data...')

##===============================
## Functions
##-------------------------------

##
# Checks if two strings match 
# @param [character] x       The string to check match against y
# @param [character] y       THe string to check match against x
# @param [mixed[]] check.na  The vector of <NA> values to check each x,y against before matching (if any is <NA>, return FALSE)
# @param [boolean] lower     A flag to convert x,y to lower case
# @param [boolean] trim      A flag to trim whitespace from x,y
# @return [boolean]
##
cb$checkMatch <- function(x, y, check.na=c('NA',NA), lower=TRUE, trim=TRUE) {
  if (x %in% check.na) return(FALSE)
  if (y %in% check.na) return(FALSE)
  if (lower) {
    x <- str_to_lower(x)
    y <- str_to_lower(y)
  }
  if (trim) {
    x <- trimws(x)
    y <- trimws(y)
  }
  return(x == y)
}




##===============================
## Company Name Mapping 
##-------------------------------

## SIC CODE DESCRIPTIONS
sic <- cb$readCsv('SIC/sic_4digit_classcodes_com.csv') ## SIC_codes_description.csv
sic$description <- str_to_lower(sic$description)

## COMPUSTAT PUBLIC COMPANY SIC CODES
cs.all <- cb$readCsv('SIC/compustat_public_sic.csv')
## company name and SIC code sting separated by "|" example: "1040|1010"
cat('reshaping compustat firm SIC codes dataframe...')
cs <- ddply(cs.all, 'conml', .progress = 'tk', summarize,
            gvkey=paste(unique(gvkey), collapse="|"),
            cusip=paste(unique(cusip), collapse="|"),
            sic=paste(unique(sic), collapse="|"), 
            country=paste(unique(fic), collapse="|"),
            city=paste(unique(city), collapse="|"),
            state=paste(unique(state), collapse="|"),
            url=paste(unique(weburl), collapse="|"),
            domain=paste(unique(str_replace_all(weburl, '^(http://)?www\\.','')), collapse="|"),
            zip=paste(unique(addzip), collapse="|")
)

cat('done.\n')
cat('creating name mapping to match CrunchBase to Compustat entities...')

## find all public companies in CrunchBase that are public at any time (in co_ipo records)
pub.uuid <- unique(c(cb$co$company_uuid[cb$co$status=='ipo'], cb$co_ipo$company_uuid))
co.pub <- cb$co[which(cb$co$company_uuid %in% pub.uuid), ]

##replace corporation ending terms 
names.pattern <- "\\s(ltd|co|co ltd|inc|corp|limited|incorporated|incorporation|corporation)$"
## Add lowercase short name without corp suffixes
co.pub.names <- co.pub
co.pub.names$short <- str_replace_all(str_to_lower(co.pub$company_name), names.pattern,"")
## Add lowercase short name without corp suffixes
cs.names <- cs
cs.names$short <- str_replace_all(str_to_lower(cs$conml), names.pattern,"")

## cache full cs.names dataframe to subset in the loop
cs.names.full <- cs.names


## match Compustat companies to CrunbhBase Companies
tol <- 0.01
namemap <- data.frame()
for(i in seq_len(nrow(co.pub.names))) {
  
  if (i %% 50 == 0) {
    saveRDS(namemap, file = 'compustat_crunchbase_co_name_mapping_by_domain_df.rds')
    cat(sprintf('i = %s\n',i))
  }
  
  isMatched <- FALSE
  cb.xi <- NA
  cs.x <- NA
  best.d <- Inf
  best.idx <- NA
  best.method <- NA
  
  ## subset compustat firms when the Crunchbase name is a substring of the full compustat name
  ## ex:  `zillow` in `zillowgroup` exact substring match
  cs.names <- cs.names.full[grep(co.pub.names$short[i],cs.names.full$short), ]
  if (nrow(cs.names) == 0) {
    cs.names <- cs.names.full
  }
  
  ## string matching logic: check compustat subset with the following stringdist metrics for attribute types
  ## until one matches (or keep closest match if none are exact)
  for (dist.method in c('osa','lcs','dl','lv')) 
  {
    for (type in c('domain','name'))
    {
      ## get items to compare
      if (type == 'domain') {
        if (is.na(co.pub.names$domain[i]))
          next
        cb.xi <- co.pub.names$domain[i]
        cs.x <- cs.names$domain
      } else if (type == 'name') {
        cb.xi <- co.pub.names$short[i]
        cs.x <- cs.names$short
      } else {
        next
      }
      
      ## compute metrics
      # cat(sprintf('i %s, %s, %s [%s]\n',i,dist.method,type,cb.xi))
      edit <- stringdist(cb.xi, cs.x, method = dist.method) ## edit distance
      if (nrow(cs.names)==1) {
        d <- -1
      } else {
        d <- sapply(edit, function(edit_i) ifelse(max(edit)==0, 0, edit_i / max(edit)) )  ## 0 <= distance ratio <= 1   ### maxlen <- sapply(str_length(cs.x), function(j)max(j,str_length(cb.xi))) ## max string len
      }
      idx <- which.min(d)
      method <- sprintf('%s::%s-%s',dist.method,type,type)
      
      ## update when optimal
      if (d[idx] < best.d) {
        best.d <- d[idx]
        best.idx <- idx
        best.method <- method
      }
      
      ## exit when optimal beats tolerance
      if (best.d < tol) {
        break(2)
      }
    }
  }
  
  ## CS index to keep
  idx <- best.idx
  
  ## Check attributes match to increase confidence in correctly matched entities
  match.country <- cb$checkMatch(co.pub.names$country_code[i], cs.names$country[idx])
  match.city    <- cb$checkMatch(co.pub.names$city[i], cs.names$city[idx])
  match.state   <- cb$checkMatch(co.pub.names$state_code[i], cs.names$state[idx])
  match.zip     <- cb$checkMatch(co.pub.names$zipcode[i], cs.names$zip[idx])
  
  ## create current firm i dataframe and add to namemap
  tmpdf <- data.frame(
    cb_name=co.pub.names$company_name[i], 
    cb_name_short=co.pub.names$short[i], 
    cb_domain=co.pub.names$domain[i],
    cb_uuid=co.pub.names$company_uuid[i], 
    cs_name=cs.names$conml[idx], 
    cs_short=cs.names$short[idx], 
    cs_domain=cs.names$domain[idx],
    cs_gvkey=cs.names$gvkey[idx], 
    cs_cusip=cs.names$cusip[idx], 
    d=best.d, 
    method=best.method,
    match_name_substr=as.integer(nrow(cs.names)==1),
    match_city=as.integer(match.city),
    match_state=as.integer(match.state),
    match_country=as.integer(match.country),
    match_zip=as.integer(match.zip),
    stringsAsFactors = F
  )
  namemap <- rbind(namemap, tmpdf)
  
}


##===============================
## SET IS_MATCH RULE
##-------------------------------
## reorder columns
map.cols <- c('cb_uuid','cs_gvkey','cs_cusip')
otr.cols <- names(namemap)[ !(names(namemap) %in% map.cols) ]
namemap <- namemap[,c(map.cols,otr.cols)]
## add is_matched rule
namemap$is_matched <- as.integer(namemap$d < tol)
## add dummy for is_manually_checked
namemap$is_checked_manually <- 0

##===============================
## EXPORT COMPUSTAT DATA and NAMEMAP
##-------------------------------
cb$namemap <- namemap
## add cs to environment exportable list
cs <- cs.names.full
.ls <- c(.ls,'cs')

##===============================
## SAVE
##-------------------------------
## save in rds
saveRDS(namemap, file = 'compustat_crunchbase_co_name_mapping_by_domain_df.rds')
## save namemap to csv
write.csv(namemap, file = 'amj_cb_cs_name_map.csv', row.names = F)

## save compustat dataframe
write.csv(cs.names.full, file = 'amj_cs_names.csv', row.names = F)


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


