library(dplyr)

setwd("C:\Users\30mat\Documents\VUW\Corona Virus Data Analytics\Mapping")
nodes <- read_csv("data/node_list_with_lat_lon.csv")
edges <- read_csv("data/edge_list.csv")

maptheme <- theme(panel.grid = element_blank()) +
  theme(axis.text = element_blank()) +
  theme(axis.ticks = element_blank()) +
  theme(axis.title = element_blank()) +
  theme(legend.position = "bottom") +
  theme(panel.grid = element_blank()) +
  theme(panel.background = element_rect(fill = "#596673")) +
  theme(plot.margin = unit(c(0, 0, 0.5, 0), 'cm'))

country_shapes <- geom_polygon(aes(x = long, y = lat, group = group),
                               data = map_data('world'),
                               fill = "#CECECE", color = "#515151",
                               size = 0.15)
mapcoords <- coord_fixed(xlim = c(-150, 180), ylim = c(-55, 80))

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

# Drawing the map
ggplot(nodes) + country_shapes +
  # Draw nodes as points
  geom_point(aes(x = lon, y = lat, size = 100),
           shape = 21, fill = 'red',
           color = 'black', stroke = 0.5) + 
  # Draw edges as curves
  geom_curve(aes(x = sourceLon, y = sourceLat, xend = targetLon + 1, yend = targetLat + 1,    
                 colour = "blue", size = 10),
             data = edge_coords, curvature = 0.33,
             alpha = 0.5) +
  scale_color_manual(values=c("blue")) +
  scale_size_continuous(guide = FALSE, range = c(0.25, 2)) +
  # Draw labels
  geom_text(aes(x = lon, y = lat, label = location_name),             
          hjust = 0, nudge_x = 1, nudge_y = 4,
          size = 3, color = "white", fontface = "bold") +
  mapcoords + maptheme
  
  


