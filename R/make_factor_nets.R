##--------------------------------------------------------------
##
##  compnet-factor-market
##   - Factor Market Interaction with Product Market Competition
##
##  Create Factor Market Networks (DV)
##  Add attributes of competition network structure (IVs)
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

g.full <- read.graph(file.path(data_dir,'graphs','g_full.graphml'), format = 'graphml')



## set firms to create networks (focal firm or replication study focal firms)
firms.todo <- c('qualtrics')


## -- settings --
d <- 3
yrpd <- 1
startYr <- 2005
endYr <- 2017            ## dropping first for memory term; actual dates 2007-2016
lg.cutoff <- 1100        ## large network size cutoff to save periods seprately 
force.overwrite <- FALSE ## if network files in directory should be overwritten
## --------------  


##----------------------------------------
## run main network period creation loop
##----------------------------------------
for (i in 1:length(firms.todo)) {
  
  name_i <- firms.todo[i]
  cat(sprintf('\n\n------------ %s -------------\n\n',name_i))
  periods <- seq(startYr,endYr,yrpd)
  company.name <- 'company_name_unique'
  g.base <- g.full  
  
  ## focal firm ego network sample
  g.d.sub <- igraph::make_ego_graph(graph = g.base, nodes = V(g.base)[V(g.base)$name==name_i], order = d, mode = 'all')[[1]]
  
  ## convert to network object
  net.d.sub <- asNetwork(g.d.sub)
  net <- net.d.sub
  net %n% 'ego' <- name_i
  
  ##-------process pre-start-year acquisitions----------
  acqs.pd <- cb$co_acq[cb$co_acq$acquired_on <= sprintf('%d-12-31',startYr-1), ]
  g.d.sub <- aaf$nodeCollapseGraph(g.d.sub, acqs.pd, remove.isolates=T, verbose = T)
  net.d.sub <- asNetwork(g.d.sub)
  cat(sprintf('v = %d, e = %d\n',vcount(g.d.sub),ecount(g.d.sub)))
  
  # ## subset to firms with employees count > 10
  # idx.employee <- which( !(V(g.d.sub)$employee_count %in% c('NA','-','1-10')) )
  # g.d.sub <- igraph::induced.subgraph(g.d.sub, vids = V(g.d.sub)[idx.employee])
  # cat(sprintf('filtered >10 employee count: v = %d, e = %d\n',vcount(g.d.sub),ecount(g.d.sub)))
  
  ##------------Network Time Period List--------------------
  nl <- list()
  
  for (t in 2:length(periods)) 
  {
    ## period dates
    cat(sprintf('\nmaking period %s-%s:\n', periods[t-1],periods[t]))
    t1 <- sprintf('%d-01-01',periods[t-1]) ## inclusive start date 'YYYY-MM-DD'
    t2 <- sprintf('%d-12-31',periods[t-1]) ## inclusive end date 'YYYY-MM-DD'
    
    ## check if period network file exists (skip if not force overwrite)
    file.rds <- sprintf('%s/%s_pd%s_d%s_y%s.rds',firm_nets_dir,name_i,yrpd,d,periods[t-1])
    if (!force.overwrite & file.exists(file.rds)) {
      cat(sprintf('file exists: %s\nskipping.\n', file.rds))
      next
    }
    
    ## 1. Node Collapse acquisitions within period
    acqs.pd <- cb$co_acq[cb$co_acq$acquired_on >= t1 & cb$co_acq$acquired_on <= t2, ]
    g.d.sub <- aaf$nodeCollapseGraph(g.d.sub, acqs.pd, verbose = T)
    
    ## 2. Subset Period Network
    nl[[t]] <- aaf$makePdNetwork(asNetwork(g.d.sub), periods[t-1], periods[t], isolates.remove = F) 
    
    ## 3. Set Covariates for updated Period Network
    nl[[t]] <- aaf$setCovariates(nl[[t]], periods[t-1], periods[t],
                                 acq=cb$co_acq,br=cb$co_br,rou=cb$co_rou,ipo=cb$co_ipo,
                                 coop=coop)
    
    ## save each period if large network (would exceed memory as full list of time periods)
    if (vcount(g.d.sub) >= lg.cutoff) {
      saveRDS(nl[[t]], file = file.rds)
      nv <- length(nl[[t]]$val)
      names(nv)[1] <- as.character(periods[t-1])
      write.csv(nv, file = sprintf('%s/%s_pd%s_d%s.csv',firm_nets_dir,name_i,yrpd,d),append = TRUE)
      nl[[t]] <- NULL ## remove from memory
    }
    
  }
  
  ##---------Small Networks: clean period list and save whole -----------
  if (vcount(g.d.sub) < lg.cutoff) 
  {
    ## ----drop null and skipped periods----
    nl.bak <- nl
    nl <- nl[which(sapply(nl, length)>0)]
    
    if (length(nl) > 1) {
      names(nl) <- periods[2:length(periods)]
    }
    
    # ## ---------- add LAGS ----------------
    # if (length(nl) > 1) {
    #   for (t in 2:length(nl)) { 
    #     nl[[t]] %n% 'DV_lag' <- nl[[t-1]][,]
    #   }
    # }  
    
    ##--------------- GET TERGM NETS LIST -----------
    ## only nets with edges > 0
    if (length(nl) > 1) {
      nets.all <- nl[2:length(nl)]
    } else {
      nets.all <- nl
    }
    nets <- nets.all[ which(sapply(nets.all, aaf$getNetEcount) > 0) ]
    ## record network sizes
    write.csv(sapply(nets,function(x)length(x$val)), file = sprintf('%s/%s_pd%s_d%s.csv',firm_nets_dir,name_i,yrpd,d))
    
    #-------------------------------------------------
    
    ## CAREFUL TO OVERWRITE 
    file.rds <- sprintf('%s/%s_pd%s_d%s.rds',firm_nets_dir,name_i,yrpd,d)
    saveRDS(nets, file = file.rds)
  }
  
}



