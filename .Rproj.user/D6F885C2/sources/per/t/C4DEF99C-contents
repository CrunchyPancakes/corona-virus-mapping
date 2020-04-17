
library("igraph")
library("readr")

dd <- read_csv("weighted.csv")
colnames(dd) <- c("id1","id2","label","weight")
gg <- graph.data.frame(dd, directed=TRUE)
E(gg)$weight <- dd$weight 

# build tree from clusters
wt <- cluster_walktrap(gg)
dend <- as.dendrogram(wt)
plot(dend,horiz=T)
plot(dend, type = "triangle")

library(ape)
library(RColorBrewer)
n <- max(wt$membership)
qual_col_pals = brewer.pal.info[brewer.pal.info$category == 'qual',]
col_vector = unlist(mapply(brewer.pal, qual_col_pals$maxcolors, rownames(qual_col_pals)))

library(viridis)
col_vector = viridis_pal(option = "D")(n)

plot(as.phylo(as.hclust(wt)), cex = 0.3, label.offset = 1, tip.color = col_vector[wt$membership])
plot(as.phylo(as.hclust(wt)), type = "unrooted")
plot(as.phylo(as.hclust(wt)), type = "fan")

visNLinks <- dd[,-3]
visNNodes <- as.data.frame(unique(c(dd$id1,dd$id2)))
colnames(visNNodes) <- c("id")
visNNodes$cluster <- wt$membership
visNNodes$deg <- strength(gg)
colnames(visNLinks) <- c("from","to","weight")
visnet <- visNetwork(nodes=visNNodes ,edges = visNLinks, width="100%", height="400px")
visOptions(visnet, highlightNearest = TRUE, selectedBy = "cluster")
# only keep strongest edge per node

uniqueSources <- unique(dd$id1)

treeLinks <- data.frame(id1=character(0),id2=character(0),label=character(0),weight=numeric(0))

for(src in uniqueSources){
  allLinksForSource <- dd[which(dd$id1==src),]
  allLinksForSource <- allLinksForSource[order(-allLinksForSource$weight),]
  treeLinks <- rbind(treeLinks,as.data.frame(allLinksForSource[1,]))
}


gg <- graph.data.frame(treeLinks, directed=TRUE)
V(gg)$size <- 3
V(gg)$frame.color <- "white"
V(gg)$color <- "orange"
E(gg)$label <- "" 
E(gg)$arrow.mode <- 0
E(gg)$weight <- 1


plot(gg, layout=layout_on_grid, vertex.label.cex=0.3)
plot(gg, layout=layout_as_star, vertex.label.cex=0.3)
plot(gg, edge.arrow.mode=0, layout=layout_with_dh, vertex.label.cex=0.3)


saveRDS(dd,"dd.rda")
saveRDS(wt,"wt.rda")
#saveRDS(wt_b,"wt_b.rda")
