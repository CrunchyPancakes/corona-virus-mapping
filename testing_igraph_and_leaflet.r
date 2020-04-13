library(igraph)
library(leaflet)
library(png)
library(plyr)
library(sp)
library(tidyverse)
library(leaflet.extras)
library(htmltools)

setwd("C:\Users\30mat\Documents\VUW\Corona Virus Data Analytics\Mapping")
nodes <- read_csv("data/node_list_with_lat_lon.csv")
edges <- read_csv("data/edge_list.csv")

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
             popup = ~htmlEscape(location_name),
             clusterOptions = markerClusterOptions())%>%
  addPolylines(data = edges_igraph, color = "#000000", weight = 0.0001*edges$weight)

