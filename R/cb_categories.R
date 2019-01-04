

library(igraph)
library(intergraph)
library(stringr)

setwd("C:/Users/T430/Google Drive/PhD/Dissertation/competition networks/compnet2")


source(file.path(getwd(),'R','amj_awareness_functions.R'))


## graph filename
full.graph.file <- 'g_full.graphml'

## focal firm nets
nets <- readRDS('firm_nets_rnr/hearstcorporation_d3.rds')
gs <- list()
for (yr in names(nets)) {
  gs[[yr]] <- asIgraph(nets[[yr]])
  nets[[yr]] = NULL
  gc()
}

sapply(gs,vcount)

## use latest year
net <- nets[[length(nets)]]
g <- asIgraph(net)

g.full <- igraph::read.graph(full.graph.file, format = 'graphml')

## full graph categories
gfcgl <- unique(c(str_split(V(g.full)$category_group_list,"[|]",simplify = T)))
gfcl <- unique(c(str_split(V(g.full)$category_list,"[|]",simplify = T)))

## focal firm graph graph categories
gcgl <- unique(c(str_split(V(g)$category_group_list,"[|]",simplify = T)))
gcl <- unique(c(str_split(V(g)$category_list,"[|]",simplify = T)))

## lengths
len.gfcgl <- length(gfcgl)
len.gfcl <- length(gfcl)
len.gcgl <- length(gcgl)
len.gcl <- length(gcl)

## what proportion of all crunchbase database categories and group categories
## are covered by firms  in the focal firm network
print(sprintf('%.1f%s of category_group_list included in network', 100*len.gcgl/len.gfcgl,'%'))
print(sprintf('%.1f%s of category_list included in network', 100*len.gcl/len.gfcl,'%'))






