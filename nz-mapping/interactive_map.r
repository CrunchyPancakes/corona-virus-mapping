library(igraph)
library(leaflet)
library(png)
library(plyr)
library(sp)
library(tidyverse)
library(leaflet.extras)
library(htmltools)

nodes <- read_csv("data/nz_node_list.csv")
edges <- read_csv("data/nz_edge_list.csv")

edge_source_target <- cbind(edges[,1], edges[,2])

n <- graph.data.frame(edge_source_target, 
                      directed = TRUE,
                      vertices = nodes)

network <- get.data.frame(n, "both")

vert <- network$vertices

coordinates(vert) <- ~ lon + lat

edges_igraph <- network$edges

edges_igraph <- cbind(edges_igraph, edges[,4])

edges_igraph <- lapply(1:nrow(edges_igraph),
                       function(i){
                         as(rbind(vert[vert$name == edges_igraph[i, "from"], ],
                                  vert[vert$name == edges_igraph[i, "to"], ]),
                            "SpatialLines")
                       }
)

for(i in seq_along(edges_igraph)) {
  edges_igraph[[i]] <- spChFIDs(edges_igraph[[i]], as.character(i))
}

edges_igraph <- do.call(rbind, edges_igraph)

leaflet(vert) %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  addMarkers(data = vert, 
             popup = ~htmlEscape(region),
             clusterOptions = markerClusterOptions())%>%
  addPolylines(data = edges_igraph, color = "#000000", weight = 0.0001*edges$weight)

