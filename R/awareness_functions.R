##
# AMJ 2018 SPECIAL ISSUE AWARENESS FUNCTIONS
# amj_awareness_functions.R
#
# Notes:
#  @export list `aaf`
##

library(igraph)
library(network)
library(intergraph)
library(xergm)
library(stringr)

##
# amj_awareness_functions list object
##
aaf <- list()


##
# Checks vector for general NA types (NULL, NA, 'NA', NaN) 
#  NOTE does not check data.frame or list -- returns FALSE
##
.isNA <- function(x, check.string.na=TRUE) {
  if (is.null(x))
    return(TRUE)
  uncheckable <- c('data.frame','list')
  if (class(x) %in% uncheckable)
    return(FALSE)
  return(unname(unlist(sapply(x,function(i){
    (is.null(i) | is.na(i) | is.nan(i) | (check.string.na & i== 'NA'))
  }))))
}


##
# Checks vector for NA or missing types (NULL, NA, 'NA', NaN) 
#  NOTE does not check data.frame or list -- returns FALSE
##
.hasNA <- function(x, check.string.na=TRUE) {
  if (is.null(x))
    return(TRUE)
  return(any(.isNA(x,check.string.na)))
}

##
# SUM the non-NA values of X, else return a default value if none exist
##
.sumNA <- function(x, default=NA) {
  return(ifelse(length(x[!.isNA(x)]) > 0, sum(x[!.isNA(x)]), default))
}

##
# MIN the non-NA values of X, else return a default value if none exist
##
.minNA <- function(x, default=NA) {
  return(ifelse(length(x[!.isNA(x)]) > 0, min(x[!.isNA(x)]), default))
}

##
# MAX the non-NA values of X, else return a default value if none exist
##
.maxNA <- function(x, default=NA) {
  return(ifelse(length(x[!.isNA(x)]) > 0, max(x[!.isNA(x)]), default))
}



###
# Create igraph from competitive relation in comp dataframe
#  with optional vertex attributes in vertdf dataframe
# @param [dataframe] comp               The edgelist of competitive relations
# @param [dataframe] vertdf             The vertex attributes dataframe
# @param [character] name               The company's name attribute
# @param [character] compName           The compeitor's name attribute
# @param [character] relationStartCol   The competitive relation's starting date attribute
# @param [character] relationEndCol     The competitive relation's ending date attribute
# @param [character[]] vertAttrs        The names of vertex attributes from vertdf to include in the graph
# @return [igraph]
##
aaf$makeGraph <- function(comp,vertdf,name='company_name_unique', 
                          compName='competitor_name_unique', 
                          relationStartCol='relation_began_on',
                          relationEndCol='relation_ended_on',
                          vertAttrs=NA )
{
  if(is.na(vertAttrs)) {
    vertAttrs <- c('company_name','founded_on','founded_year','closed_on','closed_year','category_list',
                   'category_group_list','state_code','country_code','region','city','acquired_on',
                   'company_gvkey','company_uuid','domain','status_update',
                   'company_cusip','company_cusip_6','company_sic','employee_count')
  }
  el <- data.frame(source=comp[,name], 
                   target=comp[,compName],
                   relation_began_on=comp[,relationStartCol], 
                   relation_ended_on=comp[,relationEndCol], 
                   stringsAsFactors = F)
  ## remove missing names
  el <- el[which(el$source!="" & el$target!=""), ]
  ## make vertex df
  verts <- data.frame(company_name_unique=unique(c(el$source,el$target)), stringsAsFactors = F)
  verts <- merge(x=verts,y=vertdf[,c(name,vertAttrs[vertAttrs%in%names(vertdf)])],
                 by=name,all.x=T,all.y=F)  
  ## make graph
  g <- igraph::graph.data.frame(d = el, directed = F, vertices = verts)
  E(g)$weight <- 1
  V(g)$weight <- 1
  V(g)$orig.vid <- as.integer(V(g))
  return(g)
}


##
# Computes the Generalist (vs Specialist) Index
# for an igraph with given cluster memberships
# values bounded between 0 and K (for K clusters in the memberships)
# @param [igraph] g   The igraph object
# @param [int[]] memberships  The vector of integers representing cluster memberships
# @return [float[]] The generalist index vector [float[]]
##
aaf$generalistIndex <- function(g, memberships)
{
  if (class(g) != 'igraph') stop("g must be an igraph object")
  clusters <- unique(memberships)
  K <- length(clusters)
  adj <- igraph::get.adjacency(g, sparse = F)
  ## apply GI algorithm to adjacency matrix of graph g
  gen.idx <- sapply(1:nrow(adj), function(i){
    x <- adj[i,]
    val <- 0 ## reset value for next node
    for (k in clusters) {
      ## get index of vertices in k'th cluster
      v.in.k <- which(memberships == k)
      ## exclude vertex i from computation of it's own generalist index
      v.in.k <- v.in.k[v.in.k != i]
      ## filter to members in cluster k (cols) for this row x
      sub.k.x <- x[v.in.k]
      ## sum edges from focal node i to members of cluster k
      cnt.e.k <- sum(sub.k.x)
      ## total possible edges node i to cluster k
      max.e.k <- length(sub.k.x)
      ## increment value by cluster ratio
      ## define ratio as 0 if empty denominator (max.e.k=0)
      val <- val + ifelse(max.e.k > 0, (cnt.e.k / max.e.k), 0)
    }
    return(val)
  })
  return(gen.idx)
}

##
# Computes the Number of competitors' clusters as proxy for Jobs To BE Done
# for an igraph with given cluster memberships
# values bounded between 0 and K (for K clusters in the memberships)
# @param [igraph] g   The igraph object
# @param [int[]] memberships  The vector of integers representing cluster memberships
# @return [float[]] The generalist index vector [float[]]
##
aaf$jobsToBeDone <- function(g, memberships)
{
  if (class(g) != 'igraph') stop("g must be an igraph object")
  adj <- igraph::get.adjacency(g, sparse = T)
  return(apply(adj, 1, function(x){
    length(unique(memberships[which(x==1)])) ## cluster memberships of competitors (which cols==1) for this node (row x)
  }))
}

##
# Returns the vector of firm-branch regions, each concatenated as a character string (separated by pipes "|")
# @see setCovariates(), .covMmc()
# @param [dataframe] df   The firm-branch dataframe
# @param [boolean] drop   A flag to drop fields in the plyr::ddply() call
# @param [dataframe] 
# 
##
aaf$mmcMarketsDf <- function(df, drop=FALSE, ...)
{
  if( !('company_name_unique' %in% names(df)))
    stop('df must contain `company_name_unique` column')
  if( !('mmc_code' %in% names(df)))
    stop('df must contain `mmc_code` column')
  return(plyr::ddply(df, .variables = .(company_name_unique),.drop = drop,
                     summarise,
                     concat = paste(mmc_code,collapse = "|"),
                     .progress = 'text', ...))
}

##
# Returns the MMC value between two firms based on their firm branch regions
# @see setCovariates(), .covMmc()
# @param [character] x   The concatenated markets, eg, 'USA_CA|USA_NY|ARG_9'
# @param [character] y   The concatenated markets, eg, 'USA_CA|USA_NY|ARG_9'
# @return [float]
##
aaf$mmcfromMarketConcat <- function(x,y)
{
  mx <- c(stringr::str_split(x, '[|]', simplify = T))
  my <- c(stringr::str_split(y, '[|]', simplify = T))
  if (length(mx)==0 | length(my)==0)
    return(0)
  nx <- sum(mx %in% my)
  ny <- sum(my %in% mx)
  mmc <- (nx / length(mx)) * (ny / length(my))
  mmc[diag(mmc)] <- 0
  return(mmc)
}







##
# Returns the vector of firm-alliance relations
# @see setCovariates(), .covMmc()
# @param [dataframe] df   The firm-alliance dataframe
# @param [boolean] drop   A flag to drop fields in the plyr::ddply() call
# @param [dataframe] 
# 
##
aaf$coopConcatDf <- function(df, drop=FALSE, ...)
{
  if( !('company_uuid' %in% names(df)))
    stop('df must contain `company_uuid` column')
  if( !('coop_id' %in% names(df)))
    stop('df must contain `coop_id` column')
  return(plyr::ddply(df, 'company_uuid', .progress = 'text', summarise,
                     concat = paste(unique(coop_id),collapse = "|"),
                     ...))
}

##
# Returns the cooperative relations between two firms 
# @see setCovariates(), .covMmc()
# @param [character] x   The concatenated relations 
# @param [character] y   The concatenated relations
# @return [float]
##
aaf$coopFromConcat <- function(x,y)
{
  if (is.na(x) | is.na(y))
    return(0)
  mx <- c(stringr::str_split(x, '[|]', simplify = T))
  my <- c(stringr::str_split(y, '[|]', simplify = T))
  if (length(mx)==0 | length(my)==0)
    return(0)
  nx <- sum(mx %in% my)   ## TODO CHECK THIS
  ny <- sum(my %in% mx)   ## TODO CHECK THIS
  return(min(nx,ny))
}


##
# Returns current firm-firm alliance/jv count of active cooperative relations in current period
# @see setCovariates()
# @param [network] net     The network object   
# @param [character[]] firms  The vector of firm names (company_name_unique)
# @param [integer] end        The ending year (excluded)
# @return [matrix]
##
aaf$.cov.coop <- function(net, coop, company_uuids, end, ...)
{
  cols <- c('company_uuid','date_alliance_terminated','date_effective','date_expired')
  for (col in cols){
    if (!(col %in% names(coop))) stop(sprintf('coop dataframe missing attribute `%s`', col))
  }
  
  g <- intergraph::asIgraph(net)
  idx <- which(
    coop$company_uuid %in% company_uuids
    & coop$date_effective < end 
    & (
      is.na(coop$date_alliance_terminated) 
      | coop$date_alliance_terminated >= end 
      | is.na(coop$date_expired)
      | coop$date_expired >= end
    )
  )
  
  cat('concatenating current cooperative relations...')
  df <- aaf$coopConcatDf(coop[idx, ])
  tmp <- data.frame(company_uuid=company_uuids, stringsAsFactors = F)
  df.m <- merge(x=tmp, y=df, by = 'company_uuid', all.x=T, all.y=F)
  cat('done.\n')
  
  cat('computing current cooperative relations outerproduct matrix...')
  coop.outer <- outer(df.m$concat, df.m$concat, Vectorize(aaf$coopFromConcat))
  coop.outer.m <- as.matrix(coop.outer)
  ## remove diagonal total (firm can't  have self-alliance)
  diag(coop.outer.m) <- 0
  cat('done.\n')
  
  return(coop.outer.m)
}

##
# Returns past firm-firm alliance/jv count of PAST cooperative relations (not still active)
# @see setCovariates()
# @param [network] net     The network object    
# @param [character[]] firms  The vector of firm names (company_name_unique)
# @param [integer] end        The ending year (excluded)
# @return [matrix]
##
aaf$.cov.coopPast <- function(net, coop, company_uuids, start, ...)
{
  cols <- c('company_uuid','date_alliance_terminated','date_effective','date_expired')
  for (col in cols){
    if (!(col %in% names(coop))) stop(sprintf('coop dataframe missing attribute `%s`', col))
  }
  
  g <- intergraph::asIgraph(net)
  idx <- which(
    coop$company_uuid %in% company_uuids
    & coop$date_effective < start 
    & (
      is.na(coop$date_alliance_terminated) 
      | coop$date_alliance_terminated < start 
      | is.na(coop$date_expired)
      | coop$date_expired < start
    )
  )
  
  cat('concatenating past cooperative relations...')
  df <- aaf$coopConcatDf(coop[idx, ])
  tmp <- data.frame(company_uuid=company_uuids, stringsAsFactors = F)
  df.m <- merge(x=tmp, y=df, by = 'company_uuid', all.x=T, all.y=F)
  cat('done.\n')
  
  cat('computing past cooperative relations outerproduct matrix...')
  coop.outer <- outer(df.m$concat, df.m$concat, Vectorize(aaf$coopFromConcat))
  coop.outer.m <- as.matrix(coop.outer)
  ## remove diagonal total (firm can't  have self-alliance)
  diag(coop.outer.m) <- 0
  cat('done.\n')
  
  return(coop.outer.m)
}


##
# Returns age covariate
# @see setCovariates()
# @param [network] net     The network object
# @param [integer] end     The ending year (excluded)
# @return [integer[]] 
##
aaf$.cov.age <- function(net, end)
{
  year <- net %v% 'founded_year'
  year <- unname(sapply(year,function(x)min(as.integer(str_split(x,'[|]')[[1]]))))
  year[is.na(year) | is.nan(year)] <- median(year, na.rm = T)
  age <- end - year
  age[age < 0] <- 0
  return(age)
}

##
# Returns firm-branch geogrpahic overlap (proxying in this case firm-branch Multimarket contact)
# @see setCovariates()
# @param [dataframe] br       
# @param [character[]] firms  The vector of firm names (company_name_unique)
# @param [integer] end        The ending year (excluded)
# @return [matrix]
##
aaf$.cov.mmc <- function(br, firms, end, ...)
{
  cols <- c('company_name_unique','created_year')
  for (col in cols){
    if (!(col %in% names(br))) stop(sprintf('br dataframe missing attribute `%s`', col))
  }
  brsub <- br[which(br$company_name_unique %in% firms & br$created_year < end), ]
  if (nrow(brsub)==0) {
    cat('no firm branches in period. creating empty mmc matrix...')
    mmc <- matrix(0, nrow=length(firms), ncol=length(firms))
  } else {
    cat('concatenating firm branch markets...\n')
    df <- aaf$mmcMarketsDf(brsub, ...)
    tmp <- data.frame(company_name_unique=firms,stringsAsFactors = F)
    df.m <- merge(x=tmp, y=df, by = 'company_name_unique', all.x=T, all.y=F)
    cat('computing MMC outer product matrix...')
    mmc <- as.matrix(outer(df.m$concat, df.m$concat, Vectorize(aaf$mmcfromMarketConcat)))
  }
  cat('done.\n')
  return(mmc)
}

##
# Returns the dyadic distances of nodes in g.net
# @see setCovariates()
# @param [igraph] g.net  The igraph object
# @return [matrix]
##
aaf$.cov.dist <- function(g.net)
{
  D <- as.matrix(igraph::distances(g.net))
  rownames(D) <- NULL
  colnames(D) <- NULL
  D[D==0] <- 1e-16
  return(D)
}

##
# Returns a binary vector of 1
# @see setCovariates()
# @param [network] net  The network object
# @return [matrix]
##
aaf$.cov.ipo <- function(net, ipo, end, nameAttr='vertex.names')
{
  cols <- c('company_name_unique','went_public_year')
  for (col in cols){
    if (!(col %in% names(ipo))) stop(sprintf('ipo dataframe missing attribute `%s`', col))
  }
  idx <- which(ipo$company_name_unique %in% (net %v% 'vertex.names') & ipo$went_public_year < end)
  iposub <- ipo[idx, ]
  names <- net %v% nameAttr
  return(ifelse(names %in% iposub$company_name_unique, 1, 0))
}

##
# Returns the vector of constraint values for each node
# @see setCovariates()
# @param [igraph] g.net  The igraph object 
# @return [float[]]
##
aaf$.cov.constraint <- function(g)
{
  cons <-  igraph::constraint(g)    ### isolates' constraint = NaN
  cons[is.nan(cons) | is.na(cons)] <- 0 ### ?Is this theoretically OK?
  return(cons)
}

##
# Returns the vector of similarity scores for each node
# @see setCovariates()
# @param [igraph] g.net      The igraph object 
# @param [character] method  The similarity compuation method (see igraph docs)
# @return [float[]]
##
aaf$.cov.similarity <- function(g, method='invlogweighted')
{
  sim <- igraph::similarity(g, vids = V(g), mode = "all", method = method)
  sim[is.nan(sim) | is.na(sim)] <- 0
  return(sim)
}

##
# Assigns multiple versions of centrality scores to the given network object
#   and returns the updated network object
# @see setCovariates()
# @param [network] net   The network object 
# @return [network] 
##
aaf$.cov.centrality <- function(net)
{
  g <- asIgraph(net)
  # cat('computing betweenness...\n')
  # betw <- igraph::betweenness(g.net)
  # net %v% 'betweenness' <- betw
  # net %v% 'betweenness_log' <- log(betw + .001) 
  net %v% 'cent_deg' <- igraph::degree(g)
  net %v% 'cent_eig' <- igraph::eigen_centrality(g)$vector
  ## larger exp (Bonacich "beta") increase sensitivity to effects from distant node
  pcn0.0 <- tryCatch(tmpn0.0<- igraph::power_centrality(g,exp= 0), error = function(e)e)
  pcn0.1 <- tryCatch(tmpn0.1<- igraph::power_centrality(g,exp=-0.1), error = function(e)e)
  pcn0.2 <- tryCatch(tmpn0.2<- igraph::power_centrality(g,exp=-0.2), error = function(e)e)
  pcn0.3 <- tryCatch(tmpn0.3<- igraph::power_centrality(g,exp=-0.3), error = function(e)e)
  pcn0.4 <- tryCatch(tmpn0.4<- igraph::power_centrality(g,exp=-0.4), error = function(e)e)
  pcn0.5 <- tryCatch(tmpn0.5<- igraph::power_centrality(g,exp=-0.5), error = function(e)e)
  
  if (!inherits(pcn0.0, "error")) net %v% 'cent_pow_n0_0' <- pcn0.0
  if (!inherits(pcn0.1, "error")) net %v% 'cent_pow_n0_1' <- pcn0.1
  if (!inherits(pcn0.2, "error")) net %v% 'cent_pow_n0_2' <- pcn0.2
  if (!inherits(pcn0.3, "error")) net %v% 'cent_pow_n0_3' <- pcn0.3
  if (!inherits(pcn0.4, "error")) net %v% 'cent_pow_n0_4' <- pcn0.4
  if (!inherits(pcn0.5, "error")) net %v% 'cent_pow_n0_5' <- pcn0.5
  
  return(net)
}


## 
# Assigns multiple versions of generalistIndex to the given network object
#   and returns the updated network object
# @see setCovariates(), generalistIndex()
# @param [network] net       The network object 
# @return [network] 
## 
aaf$.cov.generalistIndex <- function(net)
{
  ## Community membership
  g <- intergraph::asIgraph(net)
  ##igraph::optimal.community()  ## too slow
  ##igraph::spinglass.community() ## too slow
  net %v% 'com_multilevel'  <- igraph::multilevel.community(g)$membership
  net %v% 'com_infomap'     <- igraph::infomap.community(g)$membership
  net %v% 'com_walktrap'    <- igraph::walktrap.community(g)$membership
  net %v% 'com_fastgreedy'  <- igraph::fastgreedy.community(g)$membership
  net %v% 'com_edgebetween' <- igraph::edge.betweenness.community(g)$membership
  net %v% 'com_labelprop'   <- igraph::label.propagation.community(g)$membership
  # net %v% 'com_eigenvector' <- igraph::leading.eigenvector.community(g.net)$membership  ## Arpack solver error
  ## Generalist Index
  net %v% 'genidx_multilevel'  <- aaf$generalistIndex(g, net %v% 'com_multilevel' )
  net %v% 'genidx_infomap'     <- aaf$generalistIndex(g, net %v% 'com_infomap' )
  net %v% 'genidx_walktrap'    <- aaf$generalistIndex(g, net %v% 'com_walktrap' )
  net %v% 'genidx_fastgreedy'  <- aaf$generalistIndex(g, net %v% 'com_fastgreedy' )
  net %v% 'genidx_edgebetween' <- aaf$generalistIndex(g, net %v% 'com_edgebetween' )
  net %v% 'genidx_labelprop'   <- aaf$generalistIndex(g, net %v% 'com_labelprop' )
  ## Count of competitor's niche clusters == 'Jobs to be done' proxy
  net %v% 'njobs_multilevel'  <- aaf$jobsToBeDone(g, net %v% 'com_multilevel' )
  net %v% 'njobs_infomap'     <- aaf$jobsToBeDone(g, net %v% 'com_infomap' )
  net %v% 'njobs_walktrap'    <- aaf$jobsToBeDone(g, net %v% 'com_walktrap' )
  net %v% 'njobs_fastgreedy'  <- aaf$jobsToBeDone(g, net %v% 'com_fastgreedy' )
  net %v% 'njobs_edgebetween' <- aaf$jobsToBeDone(g, net %v% 'com_edgebetween' )
  net %v% 'njobs_labelprop'   <- aaf$jobsToBeDone(g, net %v% 'com_labelprop' )
  return(net)
}




##
# Set network covariates
# @param [network] net          The network object
# @param [integer] start        The starting year (included)
# @param [integer] end          The ending year (excluded)
# @param [character[]] covlist  The names of covariates to add to the network (vertices or edges), including 'age','mmc','dist','similarity','ipo_status','centrality','generalist'
# @param [dataframe] acq        The acquisitions dataframe
# @param [dataframe] rou        The investment|funding roundings dataframe
# @param [dataframe] br         The firm branches dataframe 
# @param [dataframe] ipo        The firm IPO listing dataframe
# @return [network]
##
aaf$setCovariates <- function(net, start, end,
                              covlist=c('age','mmc','dist','ipo_status','constraint','similarity','centrality','generalist','coop'),
                              acq=NA,rou=NA,br=NA,ipo=NA,coop=NA,
                              verbose=TRUE)
{ 
  if( network::network.edgecount(net) > 0 ) {
    
    g <- intergraph::asIgraph(net)
    
    if ('age' %in% covlist) 
    {
      if (verbose) cat('computing age ...')
      net %v% 'age' <- aaf$.cov.age(net, end)
      if (verbose) cat('done\n')
    }
    if ('mmc' %in% covlist) 
    {
      if (verbose) cat('computing multi-market contact (branch geographic overlap)...')
      net %n% 'mmc' <- aaf$.cov.mmc(br, (net %v% 'vertex.names'), end)
      if (verbose) cat('done\n')
    }
    if ('dist' %in% covlist) 
    {
      if (verbose) cat('computing distances lag contact...')
      net %n% 'dist' <- aaf$.cov.dist(g)
      if (verbose) cat('done\n')
    }
    if ('ipo_status' %in% covlist) 
    {
      if (verbose) cat('computing IPO status contact...')
      net %v% 'ipo_status' <- aaf$.cov.ipo(net, ipo, end) 
      if (verbose) cat('done\n')
    }
    if ('constraint' %in% covlist) 
    {
      if (verbose) cat('computing constraint...')
      net %v% 'constraint' <- aaf$.cov.constraint(g)
      if (verbose) cat('done\n')
    }
    if ('similarity' %in% covlist) 
    {
      if (verbose) cat('computing inv.log.w.similarity...')
      net %n% 'similarity' <- aaf$.cov.similarity(g)
      if (verbose) cat('done\n')
    }
    if ('centrality' %in% covlist) 
    {
      if (verbose) cat('computing centralities...')
      net <- aaf$.cov.centrality(net)  ## returns the updated network
      if (verbose) cat('done\n')
    }
    if ('generalist' %in% covlist) 
    {
      if (verbose) cat('computing Generalist (vs Specialist) Index...')
      net <- aaf$.cov.generalistIndex(net)  ## returns the updated network
      if (verbose) cat('done\n')
    }
    if ('coop' %in% covlist) 
    {
      if (verbose) cat('computing Cooperative relations (alliance/JV)...')
      t1 <- sprintf('%s-01-01',start)
      t2 <- sprintf('%s-12-31',start)
      #
      mat.coop <- aaf$.cov.coop(net, coop, V(g)$company_uuid, t2)  ## returns the updated network
      mat.coop.bin <- mat.coop
      mat.coop.bin[mat.coop.bin >= 1] <- 1
      net %n% 'coop' <- mat.coop
      net %n% 'coop_bin' <- mat.coop.bin
      #
      mat.coop.past <- aaf$.cov.coopPast(net, coop, V(g)$company_uuid, t1)  ## returns the updated network
      mat.coop.past.bin <- mat.coop.past
      mat.coop.past.bin[mat.coop.past.bin >= 1] <- 1
      net %n% 'coop_past' <- mat.coop.past
      net %n% 'coop_past_bin' <- mat.coop.past.bin
      if (verbose) cat('done\n')
    }
    
  } else {
    
    cat('zero edges, skipping attributes.\n')
    
  }
  
  return(net)
}




##
# Update Graph collapsing nodes by acquisitions mapping
# NOTE: add 'weight' and 'acquisitions' attributes to graph before start
# @param [igraph] g                 The igraph object 
# @param [dataframe] acquisitions   The dataframe of acquisitions
# @param [bool] verbose             A flag to echo stutus updates
# @return [igraph] 
##
aaf$nodeCollapseGraph <- function(g, acquisitions, remove.isolates=FALSE, verbose=FALSE)
{
  if (class(g) != 'igraph') stop("g must be an igraph object")
  if (class(acquisitions) != 'data.frame') stop("acquisitions must be a data frame")

  ##--------------------- Acquisitions Mapping --------------------------
  ## acqs = c(1,4,3,3,1,4,...)
  ## {acquired} index --> {acquirer} acqs[index]  WHEN BOTH IN NETWORK
  acqs.sub <- acquisitions[which(acquisitions$acquirer_uuid %in% V(g)$company_uuid
                                 & acquisitions$acquiree_uuid %in% V(g)$company_uuid ), ]
  cat(sprintf('processing acquisitions: %s ...', nrow(acqs.sub)))
  
  if (nrow(acqs.sub) > 0) 
  {
    
    vAttrs = igraph::list.vertex.attributes(g)
    for (attr in c('acquired_vids','acquired_name','acquired_uuid')) {
      if ( !(attr %in% vAttrs) ) {
        g <- igraph::set.vertex.attribute(g, attr, V(g), NA)
      }
    }

    ##---------------- ACQUISITIONS LOOP---------------------------------------
    for (i in 1:nrow(acqs.sub))
    {
      x = acqs.sub[i, ]
      acquirer.vid <- which(V(g)$company_uuid == x$acquirer_uuid)
      target.vid <- which(V(g)$company_uuid == x$acquiree_uuid)
      
      ## target's neighbor's vids
      target.nbr.vids <- as.integer(igraph::neighbors(g, target.vid))
      target.deg <- length(target.nbr.vids)
      
      ## create edge vector to add to graph for neighbors of target
      if (target.deg == 0) {
        edges.c <- NA  ## target had NO neighbors, so no edges to add
      } else {
        ## string of edges with acquirer.vid after each target neighbor vid, ex: "56, 1098 ,335, 1098 ,383, 1098 ,384, 1098"
        edge.str.parts <- c(paste(target.nbr.vids,collapse=sprintf(',%s,',acquirer.vid)), as.character(acquirer.vid))
        edges.str <- paste(edge.str.parts, collapse=',')
        # convert string to vector of edges; each pair of vids are one edge: c(1,2,1,3,3,4) is edges 1-2, 1-3, 3-4
        edges.c <- as.integer(str_split(rev(edges.str), ',')[[1]])
        ## edges (source,target) matrix
      }

      el <- igraph::ends(g, E(g), names=F)
      
      ##-------------- 1. TRACK ACQUISITION LIST  ---------------------------------------------
      ## track acquisitions
      noAcq <- .isNA(V(g)$acquired_uuid[acquirer.vid])
      V(g)$acquired_vids[acquirer.vid] <- if(noAcq){
          target.vid
        }else{
          paste(V(g)$acquired_vids[acquirer.vid], target.vid, sep = "|")
        }
      V(g)$acquired_name[acquirer.vid] <- if(noAcq){
          V(g)$name[target.vid]
        }else{
          paste(V(g)$acquired_name[acquirer.vid], V(g)$name[target.vid], sep = "|")
        }
      V(g)$acquired_uuid[acquirer.vid] <- if(noAcq){
          V(g)$company_uuid[target.vid]
        }else{
          paste(V(g)$acquired_uuid[acquirer.vid], V(g)$company_uuid[target.vid], sep = "|")
        }
    
      ##-------------- 3. REMOVE TARGETS EDGES ----------------------------------------
      ## NOTE: firm nodes with degree(v)==0 are "removed" from the network by definition
      ##       but still in the data structure for ERGM estimation.
      ##       (Theoretically this function would not work for studying 
      ##        a monopoly with no indirect competitors)
      ## edge ids involving the neighbors of the target -- TO DELETE
      target.nbr.eids <- which((el[,1] %in% target.vid) | (el[,2] %in% target.vid))

      ## cache edge weights to transfer below
      tmp.weights <- as.numeric(igraph::get.edge.attribute(g, 'weight', target.nbr.eids))
      ## remove NAs from weights 
      target.nbr.edge.weights <- unlist(sapply(tmp.weights, function(x) ifelse(is.numeric(x) & !.isNA(x), x, 1)))
      
      ## delete edges 
      g <- igraph::delete.edges(g, target.nbr.eids)
      
      ##-------------- 4. TRANSFER TARGETS EDGES (IF ANY)  ----------------------------------------
      if (target.deg > 0) 
      {
        ##-------------- add targets edges to acquirer -------------------
        edgeAttrs <- list(weight=target.nbr.edge.weights, 
                          relation_began_on=rep(x$acquired_on, target.deg), 
                          relation_ended_on=rep(NA, target.deg))
        g <- igraph::add.edges(g, edges.c, attr = edgeAttrs)      

        ##--------------- simplify duplicate edges ------------------------
        ## contract edges
        if (verbose) cat('\nsimplifying edges...')
        edge.attr.comb = list(weight=function(x) .sumNA(x, 1), ## sum values that are not NA, else return 1
                              relation_began_on=function(x) .maxNA(x, NA), ## max value of those that are not NA, else return NA
                              relation_ended_on=function(x) .minNA(x, NA)) ## min value of those that are not NA, else return NA
        # edge.attr.comb = list(weight="concat",relation_began_on="concat",relation_ended_on="concat")
        g <- igraph::simplify(g, remove.multiple=T, remove.loops=T, edge.attr.comb=edge.attr.comb)
        if (verbose) cat('done.')
      } 
      
    } ## acquisition loop 
    
  } ## main 
  
  cat('\ndone.\n')
  return(g)
}


##
# Makes period network 
# - remove missing edges and nodes
# - transfers edges and apply node collapse on acquisitions
# @see Hernandez & Menon 2017  Network Revolution
# @param [network] net                The network object 
# @param [integer] start              The starting year (included)
# @param [integer] end                The ending year (excluded)
# @param [boolean] isolates.remove    A flag to remove isolate nodes
# @param [character] edgeCreatedAtt   The name of the edge attribute for created date
# @param [character] edgeDeletedAttr  The name of the edge attribute for deleted|removed date
# @param [character] vertFoundedAttr  The name of the vertex attribute for founded date
# @param [character] vertClosedAttr   The name of the vertex attribute for closed date
# @param [character] vertAcquiredAttr The name of the vertex attribute for acquired date (acquired by other company)
# @return [igraph] 
##
aaf$makePdNetwork <- function(net, start, end, 
                          isolates.remove = FALSE,
                          edgeCreatedAttr='relation_began_on',
                          edgeDeletedAttr='relation_ended_on',
                          vertFoundedAttr='founded_year',
                          vertClosedAttr='closed_year',
                          vertAcquiredAttr='acquired_year')
{
  cat('collecting edges and vertices to remove...')
  vertAttrs <- network::list.vertex.attributes(net)
  edgeAttrs <- network::list.edge.attributes(net)
  inactiveEdges <- c(); inactiveVertsEdges <- c(); inactiveVerts <- c()
  ##------------------ COLLECT EDGES TO REMOVE -----------
  ## Get EDGES CREATED AT ___ to be removed
  if (edgeCreatedAttr %in% edgeAttrs) {
    tmp <- network::get.edge.attribute(net, edgeCreatedAttr)
    eids <- which(tmp >= end)
    inactiveEdges <- unique( c(inactiveEdges, eids) )
  }
  if (edgeDeletedAttr %in% edgeAttrs) {
    tmp <- network::get.edge.attribute(net, edgeDeletedAttr)
    eids <- which(tmp < start)
    inactiveEdges <- unique( c(inactiveEdges, eids) )
  }
  ##------------------ COLLECT VERTICES TO REMOVE ------- 
  ##  REMOVE VERTICES founded_on >= `end`
  if(vertFoundedAttr %in% vertAttrs) {
    tmp <- network::get.vertex.attribute(net, vertFoundedAttr)
    vids <- which(tmp >= end) #(g)[which(tmp > end)]
    inactiveVerts <- unique( c(inactiveVerts, vids) )
  }
  ##  REMOVE VERTICES closed_on < `start`
  if(vertClosedAttr %in% vertAttrs) {
    tmp <- network::get.vertex.attribute(net, vertClosedAttr)
    vids <- which( tmp < start )  # V(g)[which(tmp < start)]
    inactiveVerts <- unique( c(inactiveVerts, vids) )
  }
  # ##  REMOVE VERTICES acquired_at < `start` <<<=== acquisitions logic moved into nodeCollapseGraph()
  # if(vertAcquiredAttr %in% vertAttrs) {
  #   tmp <- network::get.vertex.attribute(net, vertAcquiredAttr)
  #   vids <- which( tmp < start )  # V(g)[which(tmp < start)]
  #   inactiveVerts <- unique( c(inactiveVerts, vids) )
  # }
  # ##---------- GET EDGES FOR WHICH VERTICES ARE INACTIVES -------
  el <- network::as.edgelist(net)
  inactiveVertsEdges <-  which( el[,1] %in% inactiveVerts | el[,2] %in% inactiveVerts )
  inactiveEdges <- unique(c(inactiveEdges, inactiveVertsEdges))
  ##------------- DELTE EDGES & VERTICES --------------------------------------
  net <- network::delete.edges(net, inactiveEdges)
  # net <- network::delete.vertices(net, inactiveVerts) ## causes ERROR in btergm()
  ## remove isolates
  if (isolates.remove) {
    g <- asIgraph(net)
    net <- asNetwork(igraph::induced.subgraph(g,vids = which(igraph::degree(g)>0)))
  }
  cat('done.\n')
  return(net)
}



##
# Makes period graph 
# - remove missing edges and nodes
# - transfers edges and apply node collapse on acquisitions
# @see Hernandez & Menon 2017  Network Revolution
# @param [igraph] g                   The igraph object 
# @param [integer] start              The starting year (included)
# @param [integer] end                The ending year (excluded)
# @param [boolean] isolates.remove    A flag to remove isolate nodes
# @param [character] edgeCreatedAtt   The name of the edge attribute for created date
# @param [character] edgeDeletedAttr  The name of the edge attribute for deleted|removed date
# @param [character] vertFoundedAttr  The name of the vertex attribute for founded date
# @param [character] vertClosedAttr   The name of the vertex attribute for closed date
# @param [character] vertAcquiredAttr The name of the vertex attribute for acquired date (acquired by other company)
# @return [igraph] 
##
aaf$makePdGraph <- function(g, start, end,
                            isolates.remove = FALSE,
                            edgeCreatedAttr='relation_began_on',
                            edgeDeletedAttr='relation_ended_on',
                            vertFoundedAttr='founded_on',
                            vertClosedAttr='closed_on',
                            vertAcquiredAttr='acquired_on')
{
  if (class(g) != 'igraph') stop("g must be an igraph object")
  ## start INCLUSIVE : end EXCLUSIVE
  ## start <= {date} < end
  vertAttrs <- igraph::list.vertex.attributes(g)
  edgeAttrs <- igraph::list.edge.attributes(g)
  inactiveEdges <- c(); inactiveVertsEdges <- c(); inactiveVerts <- c()
  ##------------------ REMOVE EDGES ----------- 
  cat('collecting edges to remove...')
  ## REMOVE EDGES CREATED AT  >= end  (exclude end day)  OR UNKNOWN CREATED DATE
  if (edgeCreatedAttr %in% edgeAttrs) {
    tmp <- igraph::get.edge.attribute(g, edgeCreatedAttr) 
    # eids <- which(tmp >= end | tmp == "NA") ## created after OR unknown start
    eids <- which(tmp >= end & tmp != "NA") ## created after OR unknown start
    inactiveEdges <- c(inactiveEdges, eids) 
  }  
  ## REMOVE EDGES ENDED AT < start  AND ENDED AT DATE IS NOT UNKNOWN
  if (edgeDeletedAttr %in% edgeAttrs) { 
    tmp <- igraph::get.edge.attribute(g, edgeDeletedAttr)
    # eids <- which(tmp < start & tmp != "NA") ## created before AND not NA (NA means not yet ended)
    eids <- which(tmp < start & tmp != "NA") ## created before AND not NA (NA means not yet ended)
    inactiveEdges <- c(inactiveEdges, eids)
  }
  g <- igraph::delete.edges(g, inactiveEdges)
  ##------------------ REMOVE VERTICES  ------- 
  cat('done.\ncollecting vertices to remove...')
  ##  REMOVE VERTICES founded_on >= END OR UNKONW FOUNDED ON DATE
  if(vertFoundedAttr %in% vertAttrs) {
    tmp <- igraph::get.vertex.attribute(g, vertFoundedAttr)
    # vids <- which(tmp >= end | tmp == "NA") 
    vids <- which(tmp >= end & tmp != "NA") 
    inactiveVerts <- unique( c(inactiveVerts, vids) )
  }
  ##  REMOVE VERTICES closed_on < START AND CLOSED DATE IS NOT UNKOWN
  if(vertClosedAttr %in% vertAttrs) {
    tmp <- igraph::get.vertex.attribute(g, vertClosedAttr)
    vids <- which(tmp < start & tmp != "NA")  
    inactiveVerts <- unique( c(inactiveVerts, vids) )
  }
  # ##  REMOVE VERTICES acquired_on < `start` <<<=== acquisitions logic moved into nodeCollapseGraph()
  # if(vertAcquiredAttr %in% vertAttrs) {
  #   tmp <- igraph::get.vertex.attribute(g, vertAcquiredAttr)
  #   vids <- which( tmp < start & tmp != "NA" )  # V(g)[which(tmp < start)]
  #   inactiveVerts <- unique( c(inactiveVerts, vids) )
  # }
  ## GET active VERTICES
  activeVerts <- which( !(V(g) %in% inactiveVerts) )
  ## SUBGRAPH OF ONLY ACTIVE VERTICES
  g <- igraph::induced.subgraph(g, activeVerts)
  ## remove isolates
  if (isolates.remove)
    g <- igraph::induced.subgraph(g,vids = which(igraph::degree(g)>0))
  cat('done.\n')
  return(g)
}


##
#
##
aaf$getNetEcount <- function(net, symmetric=TRUE, upper.tri.diag=FALSE)
{
  if(symmetric)  {
    return(sum(net[upper.tri(net, diag = upper.tri.diag)]))
  } else {
    return(sum(net[,]))
  }
}

##
#
##


##
# Plot hostility profile firms
#  - must add vertex property ("prob") to graph argument
##
aaf$plotCompNetColPredict <- function(gs, focal.firm=NA, cutoff=.9, 
                                      probAttrName='prob',
                                      competitors=NA,
                                      label.scale=NA, vertex.scale=NA, 
                                      rcolors=c('gray'),
                                      layout.algo=layout.fruchterman.reingold, 
                                      margins=NA, seed=1111,  ...) 
{
  gs <- igraph::induced.subgraph(gs, vids = V(gs)[which(igraph::degree(gs)>0)])
  if(all(is.na(margins)))
    margins <- c(.01,.01,.01,.01)
  ## 
  par(mar=margins)
  d <- igraph::degree(gs)
  vertshape <- rep('circle',vcount(gs))
  vertcol <-  'white'#rgb(.9,.9,.9,.4)
  ## HANDLE SAME COLORS FOR COMPETITORS
  probs <- igraph::get.vertex.attribute(gs, probAttrName)
  logprobs <- log(probs)
  col2 <- c(NA, 'red')  ## (low,  high)
  # vertcol <- ifelse(logprobs > quantile(logprobs,cutoff), col2[2], col2[1] )
  vertcol <- ifelse(logprobs > log(cutoff), col2[2], col2[1] )
  vertcol[V(gs)$vertex.names==focal.firm] <- 'blue' ##rgb(.05,.05,.17,0)
  vertcol[V(gs)$name==focal.firm] <- 'blue' ##rgb(.05,.05,.17,0)
  ## shape
  high.aware.idx <- which(logprobs > quantile(logprobs,cutoff))
  V(gs)$shape <- 'circle'
  V(gs)$shape[V(gs)$name == focal.firm] <- 'square'
  V(gs)$shape[V(gs)$vertex.names == focal.firm] <- 'square'
  ##label
  vertex.label <- ''
  
  if(is.na(vertex.scale)) vertex.scale <- 100 * (1/vcount(gs)^.5)
  if(is.na(label.scale))  label.scale <- 13 * (1/vcount(gs)^.5)
  ## size
  V(gs)$vertex.size <- vertex.scale
  V(gs)$vertex.size[which(V(gs)$name == focal.firm)] <- 1.5 * vertex.scale
  ##
  set.seed(seed)
  plot.igraph(gs
              , layout=layout.algo
              , vertex.size=V(gs)$vertex.size
              , vertex.color=vertcol
              , vertex.label=vertex.label
              , vertex.label.cex=label.scale
              , vertex.label.color='black'
              , vertex.label.font = 2
              , vertex.label.family = 'sans'
              , vertex.shape = V(gs)$shape)
}























# 
# ##
# #
# # ORIGINAL VERSION OF FUNCTION -- POSSIBLE ERROR -- DEBUGGING IN function nodeCollapseGraph()
# #
# # Update Graph collapsing nodes by acquisitions mapping
# # NOTE: add 'weight' and 'acquisitions' attributes to graph before start
# # @param [igraph] g                 The igraph object 
# # @param [dataframe] acquisitions   The dataframe of acquisitions
# # @param [bool] verbose             A flag to echo stutus updates
# # @return [igraph] 
# ##
# aaf$nodeCollapseContractGraph <- function(g, acquisitions, remove.isolates=FALSE, verbose=FALSE)
# {
#   if (class(g) != 'igraph') stop("g must be an igraph object")
#   if (class(acquisitions) != 'data.frame') stop("acquisitions must be a data frame")
#   if (!('acquirer_vid' %in% names(acquisitions))) stop("acquirer_vid must be set in acquisitions dataframe")
#   if (!('acquiree_vid' %in% names(acquisitions))) stop("acquiree_vid must be set in acquisitions dataframe")
#   
#   ##--------------------- Acquisitions Mapping --------------------------
#   ## acqs = c(1,4,3,3,1,4,...)
#   ## {acquired} index --> {acquirer} acqs[index]  WHEN BOTH IN NETWORK
#   acqs.sub <- acquisitions[which(acquisitions$acquirer_vid %in% V(g)$orig.vid
#                                  & acquisitions$acquiree_vid %in% V(g)$orig.vid ), ]
#   cat(sprintf('processing acquisitions: %s ...', nrow(acqs.sub)))
#   
#   if (nrow(acqs.sub) == 0) 
#   {
#     
#     g.acq.s <- g
#     
#   } else {
#     
#     # acqMapping <- V(g)$orig.vid
#     acqMapping <- as.integer( V(g) )
#     if (verbose) cat('updating acq mapping i: ')
#     for(i in 1:length(unique(acqs.sub$acquirer_vid))) {
#       if (verbose) cat(sprintf(" %s ",i))
#       acqr.i <- unique(acqs.sub$acquirer_vid)[i] ##  acquirer vid
#       acqe.sub.i <- acqs.sub[acqs.sub$acquirer_vid == acqr.i, 'acquiree_vid'] ## acquiree's vids
#       acqe.vids <- acqe.sub.i[which(acqe.sub.i %in% V(g)$orig.vid)] ## filter acquiree's vids to those in subgraph
#       if (length(acqe.vids) > 0) { ##replace current g vid of acquiree with current graph vid of acquirer by orig.vid property
#         acqr.g.i <- which(V(g)$orig.vid == acqr.i)
#         acqe.g.vids <- sapply(acqe.vids, function(x)which(V(g)$orig.vid == x))
#         acqMapping[  which( as.integer(V(g)) %in% acqe.g.vids ) ] <- as.integer(acqr.g.i)  ## assign acquirer's vid to value in acquiree's spots
#       }
#     }
#     
#     if (verbose) cat('reindexing mapping ')
#     ## change orig.vid to current subgraph vids (reindexing)
#     # acqMappingSub <- sapply(1:length(acqMapping), function(i){ 
#     #   if (verbose & (i %% 500 == 0)) cat(sprintf(" %s ",i))
#     #   x <- acqMapping[i]
#     #   return( as.integer(V(g)[which(x==V(g)$orig.vid)]) )
#     # })
#     acqMappingSub <- acqMapping ## no need to reindex here bc already using current subgraph vids
#     if (verbose) cat('finished reindexing  ')
#     
#     ##-------------- CONFIG GRAPH ATTRIBUTES COMBINATIONS ---------------------
#     ## build vertex attr comb list
#     vertex.attr.comb <- list(weight=function(x)sum(x),
#                              tmp.name=function(x)x[1],
#                              tmp.orig.vid=function(x)x[1],
#                              absorbed=function(x)paste(x,collapse="|") ) ## paste(x,collapse="|")
#     attrs <- igraph::list.vertex.attributes(g)
#     skipAttrs <- c('name','weight',names(vertex.attr.comb))
#     if (verbose) cat('finished reindexing  ')
#     for (attr in attrs[which(!(attrs %in% skipAttrs))]) {
#       vertex.attr.comb[[attr]] <- function(x)paste(unique(x),collapse="|")
#     }
#     if (verbose) cat('finished adding attrs ')
#     
#     ## temporary attrs used to concat in mapping
#     V(g)$absorbed <- V(g)$name
#     V(g)$tmp.name <- V(g)$name[acqMappingSub]
#     V(g)$tmp.orig.vid <- V(g)$orig.vid[acqMappingSub]
#     
#     ##---------------------- COLLAPSE NODES ---------------------------------
#     if (verbose) cat('contracting vertices ')
#     g.acq <- igraph::contract.vertices(g, acqMappingSub, vertex.attr.comb=vertex.attr.comb)
#     ## reassign name & attributes of acquired company to empty node (which was acquired)
#     g.attrs <- igraph::list.vertex.attributes(g)
#     g.acq.attrs <- igraph::list.vertex.attributes(g.acq)
#     vAttrs <- g.acq.attrs[g.acq.attrs %in% g.attrs]
#     # for (vid in acqe.g.vids) {
#     #   for (attr in vAttrs) {
#     #     g.acq <- igraph::set.vertex.attribute(g.acq, attr, vid, igraph::get.vertex.attribute(g,attr,vid))
#     #   }
#     # }
#     
#     ## remove nodes that were acquired (had no remaining edges : degree=0) if flag is TRUE
#     if (remove.isolates) {
#       if (verbose) cat('removing isolates... ')
#       g.acq <- igraph::induced.subgraph(g.acq,vids = which(igraph::degree(g.acq)>0)) ## ERROR: keep empty vertices for  same size TERGM panels
#     }
#     V(g.acq)$name <- V(g.acq)$tmp.name
#     V(g.acq)$orig.vid <- V(g.acq)$tmp.orig.vid
#     
#     ## contract edges
#     if (verbose) cat('simplifying edges ')
#     edge.attr.comb = list(weight="sum",relation_began_on="max",relation_ended_on="min")
#     g.acq.s <- igraph::simplify(g.acq, remove.multiple=T, remove.loops=T, edge.attr.comb=edge.attr.comb)
#     
#   } 
#   
#   cat('done.\n')
#   return(g.acq.s)
# }