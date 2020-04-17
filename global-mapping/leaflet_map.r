# Load data, libraries, packages etc
library(tidyverse)

setwd("C:\Users\30mat\Documents\VUW\Corona Virus Data Analytics\Mapping")
nodes <- read_csv("data/node_list_with_lat_lon.csv")
edges <- read_csv("data/edge_list.csv")

# Prepare edge start and end coords
source_nodes <- filter(nodes,
                       node_id %in% edges$source)
target_nodes <- filter(nodes,
                       node_id %in% edges$target)

source_node_coords <-
  source_nodes %>% select(lon, lat)
names(source_node_coords) <- c("sourceLon",
                               "sourceLat")
target_node_coords <-
  target_nodes %>% select(lon, lat)
names(target_node_coords) <- c("targetLon",
                               "targetLat")

edge_coords <- cbind(source_node_coords,
                     target_node_coords)
view(head(edge_coords, 10))

# Prepare data to be displayed as a network


# Create leaflet map
library(leaflet)
library(leaflet.extras)
library(htmltools)
library(geosphere)

icons <- awesomeIcons(
  icon = 'ios-medical',
  iconColor = 'black',
  library = 'ion',
  markerColor = "blue"
)


leaflet(nodes) %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  addAwesomeMarkers(lat = ~lat, 
                    lng = ~lon, 
                    icon = icons,
                    popup = ~htmlEscape(location_name),
                    clusterOptions = markerClusterOptions())



