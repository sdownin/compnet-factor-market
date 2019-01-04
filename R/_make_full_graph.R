##--------------------------------------------------------------
##
##        Make Full Global Competition Netowrk
##
##--------------------------------------------------------------
# .libPaths('C:/Users/T430/Documents/R/win-library/3.2')
library(igraph)

## DIRECTORIES
data_dir <- "C:/Users/T430/Google Drive/PhD/Dissertation/crunchbase/crunchbase_export_20161024"
work_dir <- "C:/Users/T430/Google Drive/PhD/Dissertation/competition networks/compnet2"
img_dir  <- "C:/Users/T430/Google Drive/PhD/Dissertation/competition networks/envelopment/img"

## set woring dir
setwd(work_dir)

source(file.path(getwd(),'R','amj_awareness_functions.R'))
source(file.path(getwd(),'R','amj_cb_data_prep.R'))
source(file.path(getwd(),'R','amj_cb_sic_codes.R'))
source(file.path(getwd(),'R','amj_sdc_coop.R'))


## CACHE ENVIRONMENT to keep when clearing tmp objects added here
## excluding directories ending in `_dir`
.ls <- ls()[grep('(?<!_dir)$',ls(),perl = T)]


# graph filename
full.graph.file <- 'g_full.graphml'

## load full graph, else make full graph if not exists in working directory
if (full.graph.file %in% dir()) 
{
  g.full <- read.graph(full.graph.file, format='graphml')
} else {

  cat('\nmaking full graph...')
  
  max.year <- 2016
  
  ## delete edges at or later than this date (the year after max.year)
  exclude.date <- sprintf('%d-01-01', max.year+1)
  
  ## make graph
  g.full <- aaf$makeGraph(comp = cb$co_comp, vertdf = cb$co)
  
  ## cut out confirmed dates >= 2016
  g.full <- igraph::induced.subgraph(g.full, vids=V(g.full)[which(V(g.full)$founded_year <= max.year
                                                                  | is.na(V(g.full)$founded_year)
                                                                  | V(g.full)$founded_year=='' ) ] )
  g.full <- igraph::delete.edges(g.full, E(g.full)[which(E(g.full)$relation_created_at >= exclude.date)])
  
  ## SIMPLIFY
  g.full <- igraph::simplify(g.full, remove.loops=T,remove.multiple=T,
                             edge.attr.comb = list(weight='sum',
                                                   relation_began_on='max',
                                                   relation_ended_on='min'))
  
  ## save graph file
  igraph::write.graph(graph = g.full, file="g_full.graphml", format = 'graphml')
  
  cat('done.\n')
  
}


##============================================
## ADD MANUAL UPDATES (EDGES | NODES)
##--------------------------------------------

cat('\nmanually updating competitive relations in full graph...')

## add Qualtrics - Medallia competitive relation
vid1 <- which(V(g.full)$company_uuid=='2f6ed0df-e019-f0ad-10bc-d7eee4710103')  ## qualtrics
vid2 <- which(V(g.full)$company_uuid=='405c6579-fce0-ff76-6870-aa0236bafde7')  ## medallia
edgeAttrs <- list(weight=1, relation_began_on=max('2002-01-01'), relation_ended_on=NA)
g.full <- igraph::add.edges(g.full, c(vid1,vid2), attr = edgeAttrs)
g.full <- igraph::simplify(g.full, remove.loops=T,remove.multiple=T,
                           edge.attr.comb = list(weight='sum',
                                                 relation_began_on='max',
                                                 relation_ended_on='min'))
V(g.full)$weight <- 1
## save graph file
igraph::write.graph(graph = g.full, file="g_full.graphml", format = 'graphml')

cat('done.\n')





##===============================
## CLEAR NAMESPACE 
##-------------------------------
## add graph to namespace exportable
.ls <- c(.ls, 'g.full')
## clear tmp objects in environment 
## from this script not to be exported
## when called from external script
.rm <- c('x')
for (x in setdiff(ls(), .ls)) .rm <- c(.rm, x)
rm(list=.rm)
gc()

cat('done.\n')




