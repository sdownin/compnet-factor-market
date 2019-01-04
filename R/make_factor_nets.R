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

## Crunchbase Data
cb <- source(file.path(work_dir,'R','cb_data_prep.R'))$value

## full graph
g.full <- read.graph(file.path(data_dir,'graphs','g_full.graphml'), format = 'graphml')

## set firms to create networks (focal firm or replication study focal firms)
firms.todo <- c('qualtrics')


## -- Compnet Settings --
d <- 3
yrpd <- 1
startYr <- 2005
endYr <- 2017            ## dropping first for memory term; actual dates 2007-2016
lg.cutoff <- 1100        ## large network size cutoff to save periods seprately 
force.overwrite <- FALSE ## if network files in directory should be overwritten
## --------------  

## -- Factornet Settings --
f.d <- 3
f.yrpd <- 3
f.startYr <- 2015
f.endYr <- 2017            ## dropping first for memory term; actual dates 2007-2016
f.lg.cutoff <- 1100        ## large network size cutoff to save periods seprately 
f.force.overwrite <- FALSE ## if network files in directory should be overwritten
## --------------  


##----------------
## loop here
##----------------
# for (i in 1:length(firms.todo)) {
i <- 1 ## index
tyear <- 2016
t1 <- sprintf('%d-01-01',tyear-2) ## inclusive start date 'YYYY-MM-DD'
t2 <- sprintf('%d-12-31',tyear) ## inclusive end date 'YYYY-MM-DD'

## cache focal firm name
name_i <- firm_i <- firms.todo[i]

## compnet ego firm graph
nets.c <- readRDS(sprintf("%s/%s_d%s.rds",firm_nets_dir,name_i,d))

## copmnet for given tyear
net.c <- nets.c[[ which(names(nets.c)==f.endYr) ]]
g.c <- asIgraph(net.c)
V(g.c)$name <- V(g.c)$vertex.names

## ego firm competition network firm names and UUIDs
vgcnames <- V(g.c)$name
vgcuuids <- V(g.c)$company_uuid

## JOB:  [person]--[org]
head(cb$job)
## PPL:  person
head(cb$ppl)

## job data for firm in comp ego net
row.job <- which(
  (cb$job$org_uuid %in% vgcuuids)
  & (cb$job$started_on >= t1)
  & (cb$job$started_on <= t2)
)
## jobs at companies in compnet of ego firm i
jbi <- cb$job[row.job, ]
jbi <- merge(jbi, cb$co[,c('company_uuid','company_name_unique')],
             by.x='org_uuid',by.y='company_uuid',all.x=T,all.y=F)
dim(jbi)


##========================================
## Create Human Capital Flow Data 
##----------------------------------------
## [job1::ppl--org1]-->[job2::ppl--org2]
hcfcols <- c('is_current','title','job_role','executive_role','advisory_role')
hcf <- data.frame()  ## HCF dataframe

for (j in 1:nrow(jbi)) {
  jbi_j <- jbi[j,]
  cat(sprintf('job %s (%.2f%s) org::%s  ppl::%s \n',j,100*j/nrow(jbi),'%',jbi_j$company_name_unique,jbi_j$person_uuid))
  
  ## previous jobs for person of job `j`
  idx.jprev <- which(cb$job$person_uuid==jbi_j$person_uuid & cb$job$started_on < jbi_j$started_on)
  jprev <- cb$job[idx.jprev, ]
  
  ## skip job if person has no previous job (no org-org human capital flow relation)
  if (nrow(jprev) < 1) {
    cat('  no previous job. skipping.\n')
    next
  } else { ## the most recent job before the current newjob
    jprev <- jprev[order(jprev$started_on, decreasing = T), ][1, ]
  }
  
  ## compute time differences
  old_tenure_days  <- ifelse(any(is.na(c(jprev$started_on, jprev$ended_on))), NA, as.numeric(ymd(jprev$ended_on)-ymd(jprev$started_on)))
  old_new_gap_days <- ifelse(any(is.na(c(jprev$ended_on, jbi_j$started_on))), NA, as.numeric(ymd(jbi_j$started_on)-ymd(jprev$ended_on)))
  new_tenure_days  <- ifelse(any(is.na(c(jbi_j$ended_on, jbi_j$started_on))), NA, as.numeric(ymd(jbi_j$ended_on)-ymd(jbi_j$started_on)))
  
  ##  company name unqiue
  from_name_unique <- cb$co$company_name_unique[which(cb$co$company_uuid==jprev$org_uuid)]
  from_name_unique <- ifelse(length(from_name_unique)>0, from_name_unique, cb$co$company_name[which(cb$co$company_uuid==jprev$org_uuid)])
  
  ## tmp df for this human capital flow relation
  .tmp.hcf <- data.frame(
      from_company_uuid = jprev$org_uuid,
      to_company_uuid = jbi_j$org_uuid,
      from_company_name_unique = ifelse(length(from_name_unique)>0 & !is.na(from_name_unique), from_name_unique, jprev$org_uuid),
      to_company_name_unique = jbi_j$company_name_unique,
      person_uuid = jbi_j$person_uuid,
      old_started_on = jprev$started_on,
      old_ended_on = jprev$ended_on,
      old_tenure_days = old_tenure_days,
      old_new_gap_days = old_new_gap_days,
      new_started_on = jbi_j$started_on,
      new_ended_on = jbi_j$ended_on,
      new_tenure_days = new_tenure_days,
      stringsAsFactors = F
    )
  
  ## add extra hcf edge attributes
  for (col in hcfcols) 
    .tmp.hcf[1,col] <- unname(jbi_j[1,col])
  
  ## append job
  hcf <- rbind(hcf, .tmp.hcf)
}

## sort by new job started_on
hcf <- hcf[order(hcf$new_started_on, decreasing = F), ]

##--------------------------
## KSAOs Heuristic FUnction
##--------------------------
hcf$ksao <- apply(hcf[,c('job_role', 'executive_role', 'advisory_role')], 1, function(x){
  if (x[1]=='t' & x[2]=='t') return(3) ## Executive Job
  if (x[3]=='t') return(2)             ## advisory role
  if (x[1]=='t') return(1)             ## nonexecutive job
  return(0)
})


## write human capital flows edge list to CSV
hcf.file <- sprintf('human_capital_flow_EDGES_%s_tyear%s_t1%s_t2%s_pd%s_d%s.csv',name_i,tyear,t1,t2,yrpd,d)
write.csv(hcf, file = file.path(data_dir,hcf.file), row.names = F)

##==================================
## CREATE Human Capital Flows Graph
##----------------------------------
makeHcfGraph <- function(el, 
                         vertdf,
                         vertNameCol='company_name_unique',
                         srcCol='from_company_name_unique', 
                         trgCol='to_company_name_unique', 
                         weightCol='ksao',
                         vertAttrs=NA,
                         simplify.edges=TRUE)
{
  if(is.na(vertAttrs)) {
    vertAttrs <- c('company_name','founded_on','founded_year','closed_on','closed_year','category_list',
                   'category_group_list','state_code','country_code','region','city','acquired_on',
                   'company_gvkey','company_uuid','domain','status_update',
                   'company_cusip','company_cusip_6','company_sic','employee_count')
  }
  ## remove missing names
  el <- el[which(el[,srcCol]!="" & el[,trgCol]!=""), ]
  ## weights
  el$weight <- el[,weightCol]
  ## rearrange cols
  idx.src <- which(names(el) == srcCol)
  idx.trg <- which(names(el) == trgCol)
  idx.oth <- which( ! (1:ncol(el)) %in% c(idx.src,idx.trg) )
  el <- el[,c(idx.src, idx.trg, idx.oth)]
  ## make vertex df
  verts <- data.frame(company_name_unique=unique(c(el[,srcCol],el[,trgCol])), stringsAsFactors = F)
  verts <- merge(x=verts,y=vertdf[,c(vertNameCol,vertAttrs[vertAttrs%in%names(vertdf)])],
                 by.x=vertNameCol, by.y=vertNameCol, all.x=T,all.y=F)  
  ## make graph
  g <- igraph::graph.data.frame(d = el, directed = T, vertices = verts)
  V(g)$orig.vid <- as.integer(V(g))
  ## Simplify edges
  if (simplify.edges) {
      edge.attr.comb = list(weight='sum')
      g <- igraph::simplify(g, remove.loops=T, remove.multiple=T, edge.attr.comb=edge.attr.comb)
  }
  return(g)
}

## MAKE FACTOR GRAPH
g.f <- makeHcfGraph(hcf, cb$co)

## SAVE FACTOR GRAPH
g.f.file <- sprintf('g_hcf_%s_tyear%s_t1%s_t2%s_pd%s_d%s.graphml',name_i,tyear,t1,t2,yrpd,d)
write.graph(g.f, file = file.path(data_dir, 'graphs', g.f.file), format = 'graphml')


##===============
## Style Formatted Plot
##---------------
plot2 <- function(gx, layout=layout.fruchterman.reingold, focal.firm=NA, fam='sans', edge.curved=F, seed=11111, ...)
{
  par(mar=c(.1,.1,.1,.1))
  vAttrs <- igraph::list.vertex.attributes(gx) 
  set.seed(seed)
  vertex.label <- sapply(1:vcount(gx), function(x) {
      if("name" %in% vAttrs) return(V(gx)$name[x])
      if("company_name_unique" %in% vAttrs) return(V(gx)$company_name_unique[x])
      if("vertex.names" %in% vAttrs) return(V(gx)$vertex.names[x])
    })
  indeg <- igraph::degree(gx, mode = 'in')
  vertes.label2 <- sapply(1:vcount(gx), function(i){
    return(ifelse(indeg[i] > 0, vertex.label[i], ""))
  })
  plot(gx, 
       layout = layout, 
       layout.par = list(), 
       labels = NULL, 
       label.color = 'black',
       label.font = NULL, 
       label.degree = -pi/4, 
       label.dist = 0, 
       vertex.label=vertes.label2,
       vertex.label.cex=.3 + .2*log(igraph::degree(gx,mode='total')),
       vertex.color = rgb(.1,.1,.1,.05),
       # vertex.shape = vshapes,
       vertex.size = 3 + 1.5*log(igraph::degree(gx,mode='total')), 
       # vertex.frame.color=framecols, 
       # vertex.frame.width=framewidths, 
       vertex.label.family=fam,  # Font family of the label (e.g."Times", "Helvetica")
       vertex.label.font=1,  # Font: 1 plain, 2 bold, 3, italic, 4 bold italic, 5 symbol
       vertex.label.color='darkblue',
       edge.color = "grey", 
       edge.width = 1 *E(gx)$weight,
       edge.arrow.size = .2 *E(gx)$weight,
       edge.label = "", 
       edge.lty=1, 
       margin=0,
       loop.angle=0, 
       axes = FALSE, 
       xlab = "", 
       ylab = "",
       xlim=c(-1,1), 
       ylim=c(-1,1), 
       edge.curved=edge.curved,
       ...)
  par(.par$mar)
}

plot2(g)






## SUMMARY OF PEOPLE IN COMPNET
pcnt <- plyr::count(jbi$person_uuid)
pcnt <- pcnt[order(pcnt$freq, decreasing = T), ]
## SUMMARY OF COMPANIES IN jobs data filtered to compnent comapnies
ocnt <- plyr::count(jbi$company_name_unique)
ocnt <- ocnt[order(ocnt$freq, decreasing = T), ]
print(ocnt)


# }














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



