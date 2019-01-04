##########################################################################################
#
# AMJ 2018 SPECIAL ISSUE COMPETITION NETWORKS AND AWARENESS
#
# @author   sdowning.bm02g@nctu.edu.tw
#
# @export [list] cb
#
#
# ## update founded_on,closed_on dates  - Jin-Su's Email 2018-04-23
# ## C:\\Users\\T430\\Google Drive\\PhD\\Dissertation\\competition networks\\compnet2\\founded_on_date_edit
# co.date <- cb$readCsv('founded_on_date_edit/missing_companies_20180330.csv')
#
##########################################################################################
library(plyr, quietly = T)
library(magrittr, quietly = T)
library(reshape2, quietly = T)
library(lubridate, quietly = T)
library(stringr, quietly = T)


## MAIN CrunchBase DATA (and Owler data combined) PREP FUNCTION
##  - function name beginning with "." not cached in "Global Environment"
.main.cbdp <- function()
{

  ## DATAFILE FOR CB DATA LIST OBJECT
  tmp_cb_file  <- 'cb_v3_jobs.rds'  ## v2:  founded_on date updated

  ## DIRECTORIES
  tmp_data_dir <- "C:\\Users\\T430\\Google Drive\\PhD\\Dissertation\\crunchbase\\crunchbase_export_20161024"
  tmp_work_dir <- "C:\\Users\\T430\\Google Drive\\PhD\\Dissertation\\competition networks\\factor_market_rivalry\\compnet-factor-market"
  
  ## if file already saved, load and return
  if (file.exists(file.path(tmp_data_dir, tmp_cb_file))) {
    cat('loading rds data object...')
    cb <- readRDS(file.path(tmp_data_dir, tmp_cb_file))
    cat('done.\n')
    return(cb)
  }
  
  
  ## cache working dir to reset at end
  orig_work_dir <- getwd()
  
  ## SET WORING DIR
  setwd(tmp_work_dir)
  
  ## set <NA> strings
  na.strings <- c(NA, 'NA', '')
  
  ##
  # CRUNCHBASE DATA LIST OBJECT
  # @export [list] cb
  ##
  cb <- list()
  
  ##=================================
  ##  FUNCTIONS
  ##----------------------------------
  
  ##
  # Create a Universal Unique ID (UUID) or Global Unique ID (GUID) 
  #    according to RFC 4122 standard, example: 
  #    '0f62f1de-418d-11e3-8a19-cb0ceccb58ec'
  # @param [any] x   Unused argument included to allow function passed as closure within an iterator
  # @return [string] 128-bit UUID
  ##
  cb$uuid <- function(x=NA)
  {
    return(system('uuidgen', intern=TRUE))
  }
  
  ##
  # Return vector of indices of  x which are in y
  # @param [vector] x
  # @param [vector] y
  # @return [vector] length is length(intersect(x,y))
  ##
  cb$match <- function (x,y)
  {
    return(which(x %in% y))
  }
  
  ##
  # Returns vector of values from dataframe df
  #  by checking for non-NA values each row
  #  in columns (old.col, new.col)
  #  - if neither is non-NA, return default value provided (defaults to NA)
  #  - if 1 is non-NA, return the non-NA value
  #  - if both are non-NA, return the value of column `new.col`
  # @param [dataframe] df
  # @param [character] old.col  The old column name (to be replaced by new.col)
  # @param [character] new.col  The new column name (to replace old.col)
  # @return  [vector]
  ##
  cb$parseNonNa <- function(df, old.col, new.col, default=NA)
  {
    return(unname(
      apply(df[ , c(old.col, new.col)], MARGIN = 1, FUN = function(x){
        idx <- which(!is.na(x))
        switch(as.character(length(idx)),
               '0'=default,## `default` if neither is non-NA
               '1'=x[idx], ## the non-NA value if only 1 is non-NA
               '2'=x[2]    ## the replacement column if both are non-NA
        )
      })
    ))
  }
  
  ##
  # Checks if the argument `x` is a falsy (null, <nan>, <na>, 'NA', or "" empty string)
  # @param [any] x  The element to check
  # @return [bool]
  ##
  cb$falsy <- function(x)
  {
    if (is.null(x))
      return(TRUE)
    return (is.nan(x) | is.na(x) | x=='NA' | x=='')
  }
  
  ##
  # Returns vector of competitive relation beginning date strings (or NA) by the following logic:
  #  - take the latest founding date when both have valid founding dates
  #  - else NA
  # @param [dataframe] df  The dataframe to apply the relationBeganOn logic to
  # @return [character[]]  The vector of beginning date strings
  ##
  cb$relationBeganOn <- function(df)
  {
    cols <- c('company_founded_on','competitor_founded_on')
    for(col in cols){
      if (!(col %in% names(df))) stop(sprintf('dataframe missing column `%s`.',col))
    }
    return(unname(apply(X = df[,cols],MARGIN = 1,FUN = function(x){
      max(x)
    })))
  }
  
  ##
  # Returns vector of competitive relation ending date strings (or NA) by the following logic:
  #  - take the earliest ending date (closed, acquired) if any (company,competitor) are not NA
  #  - else NA
  # @param [dataframe] df  The dataframe to apply the relationEndedOn logic to
  # @return [character[]]  The vector of ending date strings
  ##
  cb$relationEndedOn <- function(df)
  {
    cols <- c('company_closed_on','company_acquired_on','competitor_closed_on','competitor_acquired_on')
    for(col in cols){
      if (!(col %in% names(df))) stop(sprintf('dataframe missing column `%s`.',col))
    }
    return(unname(apply(X = df[,cols],MARGIN = 1,FUN = function(x){
      ifelse(any(!cb$falsy(x)), min(x[!cb$falsy(x)]), NA)
    })))
  }
  
  ##
  # Loads and returns a dataframe from a CSV file
  # @param [character] filename         The file name (absolute path or relative to current working directory)
  # @param [boolean] header             A flag to include header row
  # @param [character] quote            The quotation character
  # @param [character] na.strings       The character strings to represent <NA> values
  # @param [character] stringsAsFactors A flag to convert character strings to factor type
  # @param [character] fill             A flag to fill missing columns per row
  # @return [dataframe] filename
  ##
  cb$readCsv <- function(filename, header=T, quote='"', na.strings=c('NA',''), stringsAsFactors=F, fill=T)
  {
    return(read.csv(file = filename, header=header, quote=quote, na.strings = na.strings,
                    stringsAsFactors = stringsAsFactors, fill=fill))
  }
  
  ##
  # Fix an arbitrary date string to be YYYY-MM-DD format
  #     by replacing missing MM and DD with `01`
  # @param x [string]  the date string ('YYYY', 'YYYY-MM', or 'YYYY-MM-DD')
  # @return [string]  'YYYY-MM-DD' format
  ##
  cb$fixDateYMD <- function(x)
  {
    if(is.na(x)) return (x)
    x <- str_replace_all(x, '[/]','-') ## replace slashes with dash "-"
    parts <- as.integer(str_split(x, '[-]')[[1]])
    len_parts <- length(parts)
    i.y <- which(parts > 31) ## index which part of date string is the year
    if (length(i.y) != 1)
    {
      return(x) ## either no year or multiple years, just return x
    }
    if (len_parts == 1) ## only year
    {
      return(sprintf('%04d-01-01',parts[1]))
    }
    if (len_parts == 2) ## only year and month
    {
      i.m <- which(parts <= 31)
      return(sprintf('%04d-%02d-01',parts[i.y],parts[i.m]))
    }
    if (len_parts == 3) ## all year month day
    {
      if (any(parts[-i.y] > 12)) { ## if the day is over 12 then day and month are determined
        i.m <- which(parts <= 12)
        i.d <- which(parts > 12 & parts <= 31)
      } else { ## else assume month before day (YYYY-MM-DD)
        i.m <- which(parts <= 31)[1]
        i.d <- which(parts <= 31)[2]
      }
      return(sprintf('%04d-%02d-%02d',parts[i.y],parts[i.m],parts[i.d]))
    }
    return(x)
  }
  

  ##=================================
  ##  Sources
  ##----------------------------------
  cb$csv <- list()
  cb$csv$co          <- 'organizations.csv'
  cb$csv$co_comp     <- 'competitors.csv'
  cb$csv$co_cust     <- 'customers.csv'
  cb$csv$co_parent   <- 'org_parents.csv'
  cb$csv$co_prod     <- 'products.csv'
  cb$csv$co_acq      <- 'acquisitions.csv'
  cb$csv$co_br       <- 'branches.csv'
  cb$csv$co_rou      <- 'funding_rounds.csv'  ## company--funding_round
  cb$csv$co_ipo      <- 'ipos.csv'
  cb$csv$fund        <- 'funds.csv'
  cb$csv$inv         <- 'investors.csv'
  cb$csv$inv_rou     <- 'investments.csv'  ## investor--funding_round
  cb$csv$inv_part    <- 'investment_partners.csv'
  # cb$csv$ev        <- 'events.csv'
  # cb$csv$ev_rel    <- 'event_relationships.csv'
  # cb$csv$categ     <- 'category_groups.csv'
  cb$csv$jobs      <- 'jobs.csv'
  cb$csv$ppl       <- 'people.csv'
  cb$csv$ppl_desc  <- 'people_descriptions.csv'
  
  ##=================================
  ##  Data import
  ##----------------------------------
  cat('\nloading dataframes...')
  
  co        <- cb$readCsv(file.path(tmp_data_dir, cb$csv$co))
  co_comp   <- cb$readCsv(file.path(tmp_data_dir, cb$csv$co_comp))
  co_cust   <- cb$readCsv(file.path(tmp_data_dir, cb$csv$co_cust))
  co_parent <- cb$readCsv(file.path(tmp_data_dir, cb$csv$co_parent))
  co_prod   <- cb$readCsv(file.path(tmp_data_dir, cb$csv$co_prod))
  co_acq    <- cb$readCsv(file.path(tmp_data_dir, cb$csv$co_acq))
  co_br     <- cb$readCsv(file.path(tmp_data_dir, cb$csv$co_br))
  co_rou    <- cb$readCsv(file.path(tmp_data_dir, cb$csv$co_rou))
  co_ipo    <- cb$readCsv(file.path(tmp_data_dir, cb$csv$co_ipo))
  fund      <- cb$readCsv(file.path(tmp_data_dir, cb$csv$fund))
  inv       <- cb$readCsv(file.path(tmp_data_dir, cb$csv$inv))
  inv_rou   <- cb$readCsv(file.path(tmp_data_dir, cb$csv$inv_rou))
  inv_part  <- cb$readCsv(file.path(tmp_data_dir, cb$csv$inv_part))
  # ev        <- cb$readCsv(file.path(tmp_data_dir, cb$csv$ev))
  # ev_rel    <- cb$readCsv(file.path(tmp_data_dir, cb$csv$ev_rel))
  # categ     <- cb$readCsv(file.path(tmp_data_dir, cb$csv$categ))
  job       <- cb$readCsv(file.path(tmp_data_dir, cb$csv$jobs))
  ppl       <- cb$readCsv(file.path(tmp_data_dir, cb$csv$ppl))
  ppl_desc  <- cb$readCsv(file.path(tmp_data_dir, cb$csv$ppl_desc))
  
  
  # cat('done.\nadding Owler data\n')
  # 
  # ##==========================
  # ##
  # ## Add Owler data
  # ##
  # ##--------------------------
  # 
  # ## load edge list 
  # ow.el <- read.csv(file.path(tmp_owler_data_dir, 'owler_edge_list.csv'), na.strings = na.strings, stringsAsFactors = F)
  # ## load vertex list 
  # ow.vt <- read.csv(file.path(tmp_owler_data_dir, 'owler_vertex_list.csv'), na.strings = na.strings, stringsAsFactors = F)
  # 
  # ## cache original CrunchBase organizations dataframe
  # co.orig <- co 
  # 
  # ## indicate source
  # ow.vt$source <- 'owler'
  # co$source <- 'crunchbase'
  # 
  # # ## replace  "/" with "-" in dates (YYYY-MM-DD) 
  # # date_cols <- c('founded_date','acquired_date','closed_date')
  # # for (date_col in date_cols) {
  # #   ow.vt[,date_col] <- unname(sapply(ow.vt[,date_col], function(x)str_replace_all(x,'[/]','-')))
  # # }
  # 
  # ## assign founded year to date if missing date
  # idx.no.founded.date <- which(is.na(ow.vt$founded_date))
  # ow.vt[idx.no.founded.date,'founded_date'] <- ow.vt[idx.no.founded.date,'founded_year']  
  # 
  # ## fix owler dates -- add month and day if missing (use MM=01, DD=01)
  # ow.vt$founded_date <- sapply(ow.vt$founded_date, cb$fixDateYMD)
  # ow.vt$acquired_date <- sapply(ow.vt$acquired_date, cb$fixDateYMD)
  # ow.vt$closed_date <- sapply(ow.vt$closed_date, cb$fixDateYMD)
  # 
  # 
  # ##=================================
  # ##
  # ## Add Owler companies to CrunchBase 
  # ##  - 5 Parts: 1. Owler companies missing from CrunchBase
  # ##             2. Attributes (columns) from Owler for companies already in CrunchBase
  # ##             3. IPOs list
  # ##             4. Acquisitions list
  # ##             5. Competitor relations
  # ##
  # ##---------------------------------
  # 
  # # ##=================================
  # # ## 1. Add Owler companies that were not in CrunchBase
  # # ##---------------------------------
  # # ## which owler companies are not in CrunchBase 
  # # idx.ow.not.cb <- which( ! ow.vt$company_name_unique %in% co$company_name_unique)
  # # c('1. adding new Owler firms to CrunchBase...\n')
  # # for (index in idx.ow.not.cb) 
  # # {
  # #   new_firm <- ow.vt[index, ] ## new owler firm to add
  # #   cat(sprintf('owler index %s %s\n',index, new_firm$company_name_unique))
  # #   co[nrow(co)+1,] <- NA      ## add empty row at end
  # #   n <- nrow(co)              ## new number of firms
  # #   ## add attributes of new owler firm to its new row in the crunchbase organizations dataframe
  # #   for (col in names(cb2ow)) {
  # #     co[n,col] <- new_firm[1, cb2ow[col] ]
  # #   }
  # #   ## add metadata
  # #   co$data_source[n] <- 'owler'
  # #   co$company_uuid[n] <- cb$uuid() ## generate new UUID for this company in CrunchBase organization dataframe
  # #   ## Other owler columns
  # #   added_cols <- c(names(cb2ow), unname(cb2ow))
  # #   extra_cols <- owler_cols[ !(owler_cols %in% added_cols) & !grepl('competitor_\\d', owler_cols) ]
  # #   for (col in extra_cols) {
  # #     co[n,col] <- new_firm[1,col]
  # #   }  
  # # }
  # 
  # ##=================================
  # ## 1. Add Owler companies that were not in CrunchBase
  # ##---------------------------------
  # cat('1. adding new Owler firms to CrunchBase\n')
  # 
  # ## subset owler companies that are not in CrunchBase 
  # idx.cols.ow <- which( ! grepl('competitor_\\d+', names(ow.vt)) & ! names(ow.vt) %in% c('founded_year'))
  # cols.ow <- names(ow.vt)[idx.cols.ow]
  # ow.sub <- ow.vt[ ! ow.vt$company_name_unique %in% co$company_name_unique, cols.ow]
  # 
  # ## set UUID for new owler firms (not already in CrunchBase)
  # cat('   adding UUIDs to owler firms...\n')
  # ow.sub[ , 'company_uuid'] <- sapply(seq_len(nrow(ow.sub)), cb$uuid)
  # 
  # ## append "owler_" prefix to owler data column names
  # ## to avoid name conflicts during merge and to be identified & removed after merge
  # names(ow.sub) <- unname(sapply(names(ow.sub),function(x)sprintf('owler_%s',x)))
  # 
  # ## append (via merge) new firms from Owler into CrunchBase dataframe
  # co <- merge(co, ow.sub, by.x='company_uuid', by.y='owler_company_uuid', all.x=T, all.y=T)
  # 
  # ## column mapping FROM Crunbhcase TO Owler
  # cb2ow <- c(
  #   company_name = 'owler_name',
  #   company_name_unique = 'owler_company_name_unique',
  #   founded_on = 'owler_founded_date',
  #   closed_on = 'owler_closed_date',
  #   acquired_on = 'owler_acquired_date',
  #   region = 'owler_hq_region',
  #   funding_total_usd = 'owler_total_funding_usd',
  #   employee_count = 'owler_employees',
  #   status = 'owler_status',
  #   source = 'owler_source'
  # )
  # 
  # ## assign owler dataframe column to crunchbase column for newly added firms (rows)
  # idx.cb.from.ow <- which( co$company_uuid %in% ow.sub$owler_company_uuid)
  # for (col in names(cb2ow)) {
  #   co[idx.cb.from.ow, col ] <-   co[idx.cb.from.ow, cb2ow[col] ]
  # }
  # 
  # ## remove owler columns from CrunchBase dataframe (after having added owler firms)
  # cols.remove <- names(co)[grepl('^owler_.+', names(co))]
  # for (col in cols.remove) {
  #   co[, col] <- NULL
  # }
  # 
  # 
  # # ## add empty owler columns to crunchbase  
  # # cols.skip <- c(owler_cols[grepl('competitor_\\d',owler_cols)],
  # #                'founded_date','founded_year','closed_date','acquired_date') ## don't add these
  # # cols.ow2cb <- owler_cols[ !(owler_cols %in% names(co)) & !(owler_cols %in% cols.skip) ]
  # # for (col in cols.ow2cb) {
  # #   co[,col] <- NA
  # # }
  # # ## add empty cruncbhase columns to owler
  # # cols.cb2ow <- names(co)[ ! names(co) %in% cols.ow2cb]
  # # for (col in cols.cb2ow) {
  # #   ow.sub[,col] <- NA
  # # }
  # # 
  # # ## row-bind new owler firms to CrunchBase dataframe including only specified columns from owler
  # # cols.owInCb <- names(co)[ names(co) %in% names(ow.sub) ]
  # # co <- rbind(co, ow.sub[, cols.owInCb])
  # # 
  # # ## column mapping FROM Crunbhcase TO Owler
  # # cb2ow <- c(
  # #   company_name = 'name',
  # #   founded_on = 'founded_on',
  # #   closed_on = 'closed_on',
  # #   acquired_on = 'acquired_on',
  # #   region = 'hq_region',
  # #   funding_total_usd = 'total_funding_usd',
  # #   employee_count = 'employees',
  # #   status = 'status',
  # #   company_uuid = 'owler_company_uuid'
  # # )
  # # 
  # # ## assign owler dataframe column to crunchbase column for newly added firms (rows)
  # # idx.cb.from.ow <- which( co$company_name_unique %in% ow.sub$company_name_unique)
  # # for (col in names(cb2ow)) {
  # #   co[idx.cb.from.ow, col ] <-   co[idx.cb.from.ow, cb2ow[col] ]
  # # }
  # 
  # 
  # ##=================================
  # ## 2. Add new Owler attribute for companies that were already in CrunchBase
  # ##---------------------------------
  # cat('2. adding attributes from Owler to CrunchBase firms\n')
  # # idx.ow.in.cb <- which(ow.vt$company_name_unique %in% co$company_name_unique)
  # ## subset owler companies that ARE in CrunchBase 
  # idx.cols.ow <- which( ! grepl('competitor_\\d+', names(ow.vt)) & ! names(ow.vt) %in% c('founded_year'))
  # cols.ow <- names(ow.vt)[idx.cols.ow]
  # ow.sub2 <- ow.vt[ ow.vt$company_name_unique %in% co$company_name_unique, cols.ow]
  # 
  # 
  # ## append "owler_" prefix to owler data column names
  # ## to avoid name conflicts during merge and to be identified & removed after merge
  # names(ow.sub2) <- unname(sapply(names(ow.sub2), function(x)sprintf('owler_%s',x)))
  # 
  # ## append (via merge) new columns of firms from Owler into CrunchBase dataframe
  # ## only for firms with company_name_unique not missing <NA>
  # co <- merge(co[!is.na(co$company_name_unique), ], 
  #             ow.sub2[!is.na(ow.sub2$owler_company_name_unique), ], 
  #             by.x='company_name_unique', by.y='owler_company_name_unique', all.x=T, all.y=F)
  # 
  # ## column mapping FROM Crunbhcase TO Owler
  # cb2ow <- c(
  #   company_name = 'owler_name',  ## no owler_company_name_unique because already merged on that column
  #   founded_on = 'owler_founded_date',
  #   closed_on = 'owler_closed_date',
  #   acquired_on = 'owler_acquired_date',
  #   region = 'owler_hq_region',
  #   funding_total_usd = 'owler_total_funding_usd',
  #   employee_count = 'owler_employees',
  #   status = 'owler_status',
  #   source = 'owler_source'
  # )
  # 
  # ## assign owler dataframe column to crunchbase column for newly added firms (rows)
  # idx.cb.from.ow2 <- which( co$company_name_unique %in% ow.sub2$owler_company_name_unique)
  # for (col in names(cb2ow)) {
  #   co[idx.cb.from.ow2, col ] <- co[idx.cb.from.ow2, cb2ow[col] ]
  # }
  # 
  # ## remove owler columns from CrunchBase dataframe (after having added owler firms)
  # cols.remove2 <- names(co)[grepl('^owler_.+', names(co))]
  # for (col in cols.remove2) {
  #   co[, col] <- NULL
  # }
  # 
  # 
  # ##==========================
  # ## 3. add Acquisition dates and relations from Owler to CrunchBase IPO list
  # ##--------------------------
  # cat('3. adding acquisitions from Owler to CrunchBase acquisitions table\n')
  # acq_cols <- c('company_name_unique','acquired_by_company_name_unique','acquired_date')
  # ow.acq <- ow.vt[!is.na(ow.vt$acquired_date) & !is.na(ow.vt$acquired_by_company_name_unique), acq_cols]
  # ## indices of new acquisitions (not currently in CrunchBase acquisitions table)
  # idx.acq <- which( !(ow.acq$acquired_by_company_name_unique %in% co_acq$acquirer_name_unique
  #                    & ow.acq$company_name_unique %in% co_acq$acquiree_name_unique))
  # for (i in idx.acq) {
  #   acq_i <- ow.acq[i,]
  #   # cat(sprintf('acq %s: %s\n',i, acq_i$company_name_unique))
  #   n1 <- nrow(co_acq) + 1
  #   co_acq[n1, ] <- NA
  #   co_acq$acquiree_name_unique[n1] <- acq_i$company_name_unique  ##acquired firm
  #   co_acq$acquirer_name_unique[n1] <- acq_i$acquired_by_company_name_unique
  #   co_acq$acquired_on[n1] <- acq_i$acquired_date
  #   co_acq$acquiree_uuid[n1] <- co$company_uuid[ co$company_name_unique==co_acq$acquiree_name_unique[n1] ]
  #   ## in case acquirer is not in CrunchBase organizations table, create new acquirer UUID
  #   acquirer_uuid <- co$company_uuid[ co$company_name_unique==co_acq$acquirer_name_unique[n1] ]
  #   co_acq$acquirer_uuid[n1] <- ifelse(length(acquirer_uuid)!=0, acquirer_uuid, cb$uuid())
  #   co_acq$acquisition_uuid[n1] <- cb$uuid()
  # }
  # 
  # ##==========================
  # ## 4. add IPO dates from Owler to CrunchBase IPO list
  # ##--------------------------
  # cat('4. adding IPOs from Owler to CrunchBase IPOs table\n')
  # ipo_cols <- c('name','company_name_unique','ipo_date','stock_symbol_1','stock_symbol_2')
  # ow.ipo <- ow.vt[!is.na(ow.vt$ipo_date), ipo_cols]
  # ## indices of new acquisitions (not currently in CrunchBase acquisitions table)
  # idx.ipo <- which( ! ow.ipo$company_name_unique %in% co_ipo$company_name_unique )
  # 
  # if ( ! 'stock_exchange_symbol_2' %in% names(co_ipo))
  #   co_ipo$stock_exchange_symbol_2 <- NA
  # 
  # for (i in idx.ipo) {
  #   ipo_i <- ow.ipo[i,]
  #   n1 <- nrow(ipo_i) + 1
  #   co_ipo[n1, ] <- NA
  #   co_ipo$company_name_unique[n1] <- ipo_i$company_name_unique  ##acquired firm
  #   co_ipo$name[n1] <- ipo_i$name  ##acquired firm
  #   co_ipo$went_public_on[n1] <- ipo_i$ipo_date
  #   co_ipo$stock_exchange_symbol[n1] <- ipo_i$stock_symbol_1  ## name of exchange
  #   co_ipo$stock_exchange_symbol_2[n1] <- ipo_i$stock_symbol_2 ## name of exchange
  #   co_ipo$company_uui[n1] <- co$company_uuid[ co$company_name_unique==co_ipo$company_name_unique[n1] ]
  #   co_ipo$ipo_uuid[n1] <- cb$uuid()
  # }
  # 
  # ##==========================
  # ## 5. add competior relations from Owler to CrunchBase competitor list
  # ##--------------------------
  # cat('5. adding competitors from Owler to CrunchBase competitors table\n')
  # 
  # ## indices of owler competitor relations not in CrunchBase competitor list
  # idx.comp <- which( 
  #   (
  #     ! (
  #       ow.el$source %in% co_comp$company_name_unique 
  #       & ow.el$target %in% co_comp$competitor_name_unique
  #     )
  #   ) & (
  #     ! (
  #       ow.el$target %in% co_comp$company_name_unique 
  #       & ow.el$source %in% co_comp$competitor_name_unique
  #     )
  #   )
  # )
  # 
  # if ( ! 'rank' %in% names(co_comp) )
  #   co_comp$rank <- NA
  # 
  # names(ow.el)[ names(ow.el)=='source' ] <- 'company_name_unique'
  # names(ow.el)[ names(ow.el)=='target' ] <- 'competitor_name_unique'
  # 
  # ## merge in company dates to compute relation period
  # company_dates <- co[, c('company_name_unique', 'company_uuid', 'founded_on', 'acquired_on', 'closed_on')]
  # names(company_dates) <- c('company_name_unique', 'company_uuid','company_founded_on', 'company_acquired_on', 'company_closed_on')
  # ow.el <- merge(ow.el, company_dates, by.x='company_name_unique', by.y='company_name_unique', all.x=T, all.y = F)
  # 
  # ## merge in competitor dates to compute relation period
  # competitor_dates <- co[, c('company_name_unique', 'company_uuid', 'founded_on', 'acquired_on', 'closed_on')]
  # names(competitor_dates) <- c('competitor_name_unique', 'competitor_uuid','competitor_founded_on', 'competitor_acquired_on', 'competitor_closed_on')
  # ow.el <- merge(ow.el, competitor_dates, by.x='competitor_name_unique', by.y='competitor_name_unique', all.x=T, all.y = F)
  # 
  # ## save updated owler competitor edge list to file to check if debuggin is necessary
  # write.csv(ow.el, file = file.path(tmp_owler_data_dir, 'owler_edge_list_CHECK.csv'), row.names = F, na =  na.strings)
  # 
  # cols.comp2ow <- names(co_comp)[ ! names(co_comp) %in% names(ow.el) ]
  # for (col in cols.comp2ow) {
  #   ow.el[,col] <- NA
  # }
  # 
  # ## row-bind owler competive relations to CrunchBase competitors list
  # co_comp <- rbind(co_comp, ow.el[,names(co_comp)])
  # 
  #   # for (i in 1:length(idx.comp)) {
  # #   index <- idx.comp[i]
  # #   comp_i <- ow.el[index,]
  # #   n1 <- nrow(co_comp) + 1
  # #   co_comp[n1,] <- NA
  # #   ## company name, UUID, and dates used to compute competitor relation period
  # #   co_comp$company_name_unique[n1] <- comp_i$source
  # #   uuid1 <- co$company_uuid[ co$company_name_unique==co_comp$company_name_unique[n1] ]
  # #   co_comp$company_uuid[n1] <- ifelse(length(uuid1)!=0, uuid1, cb$uuid())
  # #   co_comp$company_founded_on[n1] <- co$founded_on[ co$company_uuid==co_comp$company_uuid[n1] ]
  # #   co_comp$company_closed_on[n1] <- co$closed_on[ co$company_uuid==co_comp$company_uuid[n1] ]
  # #   co_comp$company_acquired_on[n1] <- co$acquired_on[ co$company_uuid==co_comp$company_uuid[n1] ]
  # #   ## company name, UUID, and dates used to compute competitor relation period
  # #   co_comp$competitor_name_unique[n1] <- comp_i$target
  # #   uuid2 <- co$company_uuid[ co$company_name_unique==co_comp$competitor_name_unique[n1] ]
  # #   co_comp$competitor_uuid[n1] <- ifelse(length(uuid2)!=0, uuid2, cb$uuid())
  # #   co_comp$competitor_founded_on[n1] <- co$founded_on[ co$company_uuid==co_comp$competitor_uuid[n1] ]
  # #   co_comp$competitor_closed_on[n1] <- co$closed_on[ co$company_uuid==co_comp$competitor_uuid[n1] ]
  # #   co_comp$competitor_acquired_on[n1] <- co$acquired_on[ co$company_uuid==co_comp$competitor_uuid[n1] ]
  # #   ## rank of competitor (target) for focal firm (source)
  # #   co_comp$rank[n1] <- comp_i$rank
  # #   if (i %% 50 == 0) cat(sprintf('competitors %s: %s-%s\n',index, comp_i$source, comp_i$target))
  # # }
  # 
  
  
  ##===============================
  ##
  ## FIX CRUNCHBASE DATA COLUMNS
  ##
  ##-------------------------------
  cat('preparing CrunchBase data\n')
  
  ##==========================
  ## COMPANIES
  ##--------------------------
  ##
  ## v2: update founded_on,closed_on dates
  ##     Jin-Su's Email 2018-04-23
  ## 
  co.date <- cb$readCsv(file.path(tmp_data_dir,'founded_on_date_edit','missing_companies_20180330.csv'))
  names(co.date) <- c('com_uuid','com_name','com_founded_on_UPDATE','com_closed_on_UPDATE','com_status','note')
  ## fix date formatting  '12/13/2011' ==> '2011-12-31'
  co.date$com_founded_on_UPDATE <- sapply(co.date$com_founded_on_UPDATE, function(x){
    return(ifelse(cb$falsy(x), NA,  as.character(mdy(x))))
  })
  co.date$com_closed_on_UPDATE <- sapply(co.date$com_closed_on_UPDATE, function(x){
    return(ifelse(cb$falsy(x), NA,  as.character(mdy(x))))
  })
  
  ## merge udpates into company df
  co <- merge(x=co,y=co.date,by.x='company_uuid',by.y='com_uuid',all.x=T,all.y=F)
  ## APPLY UPDATE OLD_COLUMN TO NEW_COLUMN
  ## FASTER THAN FOR-LOOP WHEN DATAFRAME IS VERY LARGE
  co$company_name <- cb$parseNonNa(co, 'company_name','com_name')
  co$status_update <- cb$parseNonNa(co, 'status','com_status')
  co$founded_on <- cb$parseNonNa(co, 'founded_on','com_founded_on_UPDATE')
  co$closed_on <- cb$parseNonNa(co, 'closed_on','com_closed_on_UPDATE')
  ## DROP REPLACEMENT COLUMNS
  co$com_name <- NULL
  co$com_status <- NULL
  co$com_founded_on_UPDATE <- NULL
  co$com_closed_on_UPDATE <- NULL
  co$note <- NULL
  
  # ##  DROP COMPANY row with mising name
  co <- co[which(co$company_name_unique!="" & !is.na(co$company_name_unique)), ]
  
  ## ONLY COMPANIES (no universities, etc)
  co <- co[which(co$primary_role == 'company'), ]
  
  ## DROP DUPLICATES
  co.cnt <- plyr::count(co$company_name_unique)
  co.cnt <- co.cnt[order(co.cnt$freq, decreasing = T), ]
  # co.cnt[co.cnt$freq > 1, ]
  co.dups <- as.character(co.cnt$x[which(co.cnt$freq > 1)])
  if(length(co.dups) > 0) {
    co <- co[which( !(co$company_name_unique %in% co.dups) ), ]
  }
  
  ## ADD YEAR variables
  co$founded_year <- as.numeric(stringr::str_sub( co$founded_on,1,4))
  co$closed_year <-  as.numeric(stringr::str_sub( co$closed_on,1,4))
  co$acquired_year <-  as.numeric(stringr::str_sub( co$acquired_on,1,4))
  
  
  ##==========================
  ## ACQUISITIONS
  ##--------------------------
  ## make unique acquired_on dates for each company
  ##  - as min of acquired_on dates when multiples (acquired multiple times)
  cat('reshaping acquisitions dataframe...')
  co_acq_acquired <- ddply(co_acq, 'acquiree_name_unique', .progress='none', summarize,
                           acquiree_uuid=paste(unique(acquiree_uuid),collapse="|"),
                           acquired_on=min(acquired_on),
                           acquired_on_concat=paste(acquired_on,collapse="|"))
  co_acq_acquired <- co_acq_acquired[which(!cb$falsy(co_acq_acquired$acquiree_name_unique) & !cb$falsy(co_acq_acquired$acquired_on)),]
  ##
  co_acq$company_name_unique <- co_acq$acquirer_name_unique
  co_acq$acquired_year <- as.numeric(stringr::str_sub(co_acq$acquired_on,1,4))
  
  
  ##==========================
  ## COMPETITIVE RELATIONS
  ##--------------------------
  ## add dates to compute relation_began_on, relation_ended_on
  ## remove acquired_on|closed_on|founed_on dates from python pre-processing script
  ## if these exist:
  drop.cols <- c('company_closed_on','competitor_closed_on','company_founded_on','competitor_founded_on',
                 'relation_ended_on','max_founded_on','relation_began_on',
                 'acquiree_name_unique_concat','acquired_on_concat','acquired_on')
  for (col in drop.cols){
    if (col %in% names(co_comp)) {
      co_comp[,col] <- NULL
    }
  }
  ## rename entity_uuid to company_uuid
  names(co_comp)[which(names(co_comp)=='entity_uuid')] <- 'company_uuid'
  ##  - COMPANY FOUNDED | CLOSED
  tmp <- co[,c('company_uuid','founded_on','closed_on')]
  names(tmp) <- c('company_uuid','company_founded_on','company_closed_on')
  co_comp <- merge(x=co_comp,y=tmp, by.x='company_uuid',by.y='company_uuid', all.x=T,all.y=F)
  ##  - COMPANY ACQUIRED  ## using co_acq_acquired for unique acquiree dates
  tmp <- co_acq_acquired[,c('acquiree_uuid','acquired_on')]
  names(tmp) <- c('acquiree_uuid','company_acquired_on')
  co_comp <- merge(x=co_comp,y=tmp, by.x='company_uuid',by.y='acquiree_uuid', all.x=T,all.y=F)
  ##  - COMPETITOR FOUNDED | CLOSED
  tmp <- co[,c('company_uuid','founded_on','closed_on')]
  names(tmp) <- c('competitor_uuid','competitor_founded_on','competitor_closed_on')
  co_comp <- merge(x=co_comp,y=tmp, by.x='company_uuid',by.y='competitor_uuid', all.x=T,all.y=F)
  ##  - COMPETITOR ACQUIRED  ## using co_acq_acquired for unique acquiree dates
  tmp <- co_acq_acquired[,c('acquiree_uuid','acquired_on')]
  names(tmp) <- c('acquiree_uuid','competitor_acquired_on')
  co_comp <- merge(x=co_comp,y=tmp, by.x='company_uuid',by.y='acquiree_uuid', all.x=T,all.y=F)
  
  ##
  ## keep only intersection of competitive relations by the following conditions:
  ##
  ## 1. company or competitor names is missing
  idx.name.exists <- which(co_comp$company_name_unique != '' & !is.na(co_comp$company_name_unique)
                           & co_comp$competitor_name_unique != '' & !is.na(co_comp$competitor_name_unique) )
  ## 2. only relations where both parties are in the company table
  idx.name.in.co <- which(co_comp$company_uuid %in% co$company_uuid
                          | co_comp$competitor_uuid %in% co$company_uuid)
  ## 3. only relations where founded_on date is not missing or "no exist"
  idx.date.exists <- which( !cb$falsy(co_comp$company_founded_on)
                            & co_comp$company_founded_on != "no exist"
                            & !cb$falsy(co_comp$competitor_founded_on)
                            & co_comp$competitor_founded_on != "no exist" )
  idx.all <- intersect( intersect(idx.name.exists,idx.name.in.co), idx.date.exists )
  co_comp <- co_comp[idx.all, ]
  ## remove previous relation_began_on|relation_ended_on
  co_comp$relation_began_year <- NULL ##  as.numeric(stringr::str_sub( co_comp$relation_began_on,1,4))
  co_comp$relation_ended_year <- NULL ##  as.numeric(stringr::str_sub( co_comp$relation_ended_on,1,4))
  
  ## set relation_began_on|relation_ended_on by new rules use new founed_on dates
  
  ##==========================
  ## Compute Relation Periods
  ##--------------------------
  ## RELATION BEGAN ON
  co_comp$relation_began_on <- cb$relationBeganOn(co_comp)
  ## RELATION ENDED ON
  co_comp$relation_ended_on <- cb$relationEndedOn(co_comp)
  
  ##==========================
  ## MULTI_MARKET_CONTACT CODE
  ##--------------------------
  co_br$mmc_code <- apply(co_br[,c('country_code3','region_code2')],1,function(x){
    paste(x, collapse="_")
  })
  
  
  ##==========================
  ## FUNDING ROUND
  ##--------------------------
  co_rou$funded_year <- as.numeric(stringr::str_sub(co_rou$announced_on,1,4))
  
  
  ##==========================
  ## BRANCHES
  ##--------------------------
  co_br$created_year <- as.numeric(stringr::str_sub(co_br$created_at,1,4))
  
  
  ##==========================
  ## IPO
  ##--------------------------
  co_ipo$went_public_year <- as.numeric(stringr::str_sub(co_ipo$went_public_on,1,4))
  
  
  ##==========================
  ## CUSTOMER
  ##--------------------------
  co_cust$created_year <- as.numeric(stringr::str_sub(co_cust$created_at,1,4))
  
  
  ##==========================
  ## Unique Branches
  ##--------------------------
  co_br <- unique(co_br)
  
  
  cat('done.\n')
  
  
  
  ##==========================
  ##
  ##  CLEAN ENVIRONMENT
  ##
  ##--------------------------
  cat('clearing environment...')
  ## dataframes
  cb$co        <- co
  cb$co_comp   <- co_comp
  cb$co_cust   <- co_cust
  cb$co_parent <- co_parent
  cb$co_prod   <- co_prod
  cb$co_acq    <- co_acq
  cb$co_br     <- co_br
  cb$co_rou    <- co_rou
  cb$co_ipo    <- co_ipo
  cb$fund      <- fund
  cb$inv       <- inv
  cb$inv_rou   <- inv_rou
  cb$inv_part  <- inv_part
  # cb$ev        <- ev
  # cb$ev_rel    <- ev_rel
  # cb$categ     <- categ
  cb$job       <- job
  cb$ppl       <- ppl
  cb$ppl_desc  <- ppl_desc

  ## RESET original working dir
  setwd(orig_work_dir)

  ## SAVING SERIALIZED
  saveRDS(cb, file.path(tmp_data_dir, tmp_cb_file)) 
  
  cat('done.\n')  
  
  ## RETURN cb list
  return(cb)

} ## END MAIN


##============================
## RUN
##----------------------------
.main.cbdp()







