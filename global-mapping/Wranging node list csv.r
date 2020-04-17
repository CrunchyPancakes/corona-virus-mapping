# Load libraries
library(tidyverse)

setwd("C:\Users\30mat\Documents\VUW\Corona Virus Data Analytics\Mapping")

original_csv <- read_csv("weighted123_23_02_2020.csv")
view(head(original_csv, 5))

nodes <- data.frame(nodes = c(original_csv[,"Source"], 
                              original_csv[,"Target"]))

# Combine columns into one (e.g append one column to the end of another)
nodes_1_column <- data.frame(nodes=unlist(nodes, use.names = FALSE))

# Convert 1 column data frame into a vector
node_vector <- nodes_1_column[['nodes']]
unique_node_vector <- unique(node_vector)

# Split each string by "/" character
unique_node_vector_split <- str_split(unique_node_vector, "/")
view(unique_node_vector_split)

# Goes through each value in the node_vector_split list.
# Gets the second index of each character list, which will
# contain the place name. 
# Ultimately we get a list of place names. 
node_id <- lapply(unique_node_vector_split, 
                        function(x){
                          x[1] 
                        })
location_name <- lapply(unique_node_vector_split, 
                     function(x){
                        x[2] 
                     })
details <- lapply(unique_node_vector_split, 
                        function(x){
                          x[3] 
                        })
year <- lapply(unique_node_vector_split, 
                        function(x){
                          x[4] 
                        })
# Turns the place names list into a data frame.
# Rename the column. 
node_id_df <- data.frame(unlist(node_id))
location_name_df <- data.frame(unlist(location_name))
details_df <- data.frame(unlist(details))
year_df <- data.frame(unlist(year))

node_list <- cbind(node_id_df, 
                   location_name_df, 
                   details_df, 
                   year_df)

names(node_list) <- c("node_id", 
                      "location_name",
                      "details",
                      "year")

view(node_list)


# Getting lat/lon coordinates for each node
library(ggmap)

countries <- lapply(node_list[2],
                   function(x){
                     as.character(x)
                   })
countries <- unlist(countries)

geocodes <- geocode(countries, source = "google")

node_list <- cbind(node_list,
                   geocodes)
view(node_list)


# Write node list into csv
write_csv(node_list, "node_list_with_lat_lon.csv")
