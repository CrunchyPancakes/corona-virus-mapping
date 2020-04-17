library(tidyverse)

setwd("C:/Users/30mat/Documents/VUW/Corona Virus Data Analytics/Mapping/nz-mapping")

links <- read_csv("data/links.csv")
meta <- read_csv("data/meta.csv")
nodes <- read_csv("data/nodes.csv")
weighted <- read_csv("data/weighted.csv")

# Creating node list
node_id_region <- data.frame(c(meta[,"id"],
                                         meta[,"region"]))

node_id_region$location <- 
  ifelse(node_id_region$region == "Auckland", "Auckland", 
                                  ifelse(node_id_region$region == "Combined Wellington", "Wellington",
                                         ifelse(node_id_region$region == "Southern", "Queenstown",
                                                ifelse(node_id_region$region == "Waikato", "Waikato", 
                                                       ifelse(node_id_region$region == "Wairarapa", "Wairarapa", 
                                                              ifelse(node_id_region$region == "Bay of Plenty", "Bay of Plenty", "invalid"))))))

# Getting lat/lon coordinates for each node
library(ggmap)

locations <- lapply(node_id_region[3],
                    function(x){
                      as.character(x)
                    })
locations <- unlist(locations)

geocodes <- geocode(locations, source = "google")

node_list <- cbind(node_id_region,
                   geocodes)
view(node_list)

# Write node list into csv
write_csv(node_list, "data/nz_node_list.csv")


# Creating edge list
nodes <- data.frame(c(weighted[,"Source"], 
                              weighted[,"Target"]))

# Extract id for source nodes
source_nodes <- nodes[,1]
source_nodes_split <- str_split(source_nodes, "-")
source_node_ids <- lapply(source_nodes_split, 
                          function(x){
                            x[2] 
                          })
source_node_ids_df <- data.frame(unlist(source_node_ids))
view(head(source_node_ids_df, 5))

# Extract id for target nodes
target_nodes <- nodes[,2]
target_nodes_split <- str_split(target_nodes, "-")
target_node_ids <- lapply(target_nodes_split, 
                          function(x){
                            x[2] 
                          })
target_node_ids_df <- data.frame(unlist(target_node_ids))
view(head(target_node_ids_df, 5))

# Put columns back together
edge_list <- cbind(source_node_ids_df,
                   target_node_ids_df,
                   weighted[,"label"],
                   weighted[,"weight"])
names(edge_list) <- c("source", 
                      "target",
                      "label",
                      "weight")
view(head(edge_list, 10))

# Write edge list into csv
write_csv(edge_list, "nz_edge_list.csv")
