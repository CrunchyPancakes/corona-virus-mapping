# Load libraries
library(tidyverse)

setwd("C:\Users\30mat\Documents\VUW\Corona Virus Data Analytics\Mapping")

original_csv <- read_csv("weighted123_23_02_2020.csv")
view(head(original_csv, 5))

nodes <- data.frame(nodes = c(original_csv[,"Source"], 
                              original_csv[,"Target"]))

# Extract node ids for source nodes
source_nodes <- nodes[,1]
source_nodes_split <- str_split(source_nodes, "/")
source_node_ids <- lapply(source_nodes_split, 
                  function(x){
                    x[1] 
                  })
source_node_ids_df <- data.frame(unlist(source_node_ids))
view(head(source_node_ids_df, 5))

# Extract node ids for target nodes
target_nodes <- nodes[,2]
target_nodes_split <- str_split(target_nodes, "/")
target_node_ids <- lapply(target_nodes_split, 
                          function(x){
                            x[1] 
                          })
target_node_ids_df <- data.frame(unlist(target_node_ids))
view(head(target_node_ids_df, 5))

# Put node id lists back together
edge_list <- cbind(source_node_ids_df,
                   target_node_ids_df,
                   original_csv[,"label"],
                   original_csv[,"weight"])
names(edge_list) <- c("source", 
                      "target",
                      "label",
                      "weight")
view(head(edge_list, 10))

# Write edge list into csv
write_csv(edge_list, "edge_list.csv")

