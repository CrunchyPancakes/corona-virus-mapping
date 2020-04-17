library(assertthat)
library(dplyr)
library(purrr)
library(igraph)
library(ggplot2)
library(ggraph)
library(ggmap)
library(readr)

nodesLL <- read_csv("node_list_with_lat_lon.csv")
nodesLL$id <- ""
for(i in 1:nrow(nodesLL)){
  nodesLL$id[i] <- paste(nodesLL[i,1],nodesLL[i,2],nodesLL[i,3],nodesLL[i,4],sep = "/")
}
nodesLL$name <- nodesLL$id
#nodesLL$id <- 1:nrow(nodesLL)

nodesLL <- nodesLL[,-c(1:4)]

edgesLL <- read_csv("weighted123_23_02_2020.csv")
colnames(edgesLL) <- c("id1","id2","label","weight")
uniqueSources <- unique(edgesLL$id1)

singleLinks <- data.frame(id1=character(0),id2=character(0),label=character(0),weight=numeric(0))


for(src in uniqueSources){
  allLinksForSource <- edgesLL[which(edgesLL$id1==src),]
  allLinksForSource <- allLinksForSource[order(-allLinksForSource$weight),]
  singleLinks <- rbind(singleLinks,as.data.frame(allLinksForSource[1,]))
}

singleLinks$categories <- 1

edgesLL <- singleLinks[,-3]

set.seed(123)  # set random generator state for the same output

N_EDGES_PER_NODE_MIN <- 1
N_EDGES_PER_NODE_MAX <- 4
N_CATEGORIES <- 4

# edges: create random connections between countries (nodes)
edges <- map_dfr(nodesLL$id, function(id) {
  n <- floor(runif(1, N_EDGES_PER_NODE_MIN, N_EDGES_PER_NODE_MAX+1))
  to <- sample(1:max(nodesLL$id), n, replace = FALSE)
  to <- to[to != id]
  categories <- sample(1:N_CATEGORIES, length(to), replace = TRUE)
  weights <- runif(length(to))
  data_frame(from = id, to = to, weight = weights, category = categories)
})

edges <- edges %>% mutate(category = as.factor(category))

nodesLL <- nodesLL[,c(3,4,2,1)]

nodesLL$lat <- nodesLL$lat+runif(nrow(nodesLL), min=.5, max=10)
nodesLL$lon <- nodesLL$lon+runif(nrow(nodesLL), min=.5, max=10)

nodesLL$id[77] <- "77 - BetaCoV/Japan/AI/I-004/2020"
nodesLL$id[67] <- "67 - BetaCoV/Singapore/ 1/2020"

colnames(edgesLL) <- c("from","to","weight","category")

edgesLL$weight <- 0.1

g <- graph_from_data_frame(edgesLL, directed = FALSE, vertices = nodesLL)

edges_for_plot <- edgesLL %>%
  inner_join(nodesLL %>% select(id, lon, lat), by = c('from' = 'id')) %>%
  rename(x = lon, y = lat) %>%
  inner_join(nodesLL %>% select(id, lon, lat), by = c('to' = 'id')) %>%
  rename(xend = lon, yend = lat)

assert_that(nrow(edges_for_plot) == nrow(edgesLL))

nodesLL$weight = degree(g)

maptheme <- theme(panel.grid = element_blank()) +
  theme(axis.text = element_blank()) +
  theme(axis.ticks = element_blank()) +
  theme(axis.title = element_blank()) +
  theme(legend.position = "none") +
  theme(panel.grid = element_blank()) +
  theme(panel.background = element_rect(fill = "#000000")) +
  theme(plot.margin = unit(c(0, 0, 0, 0), 'cm'))

country_shapes <- geom_polygon(aes(x = long, y = lat, group = group),
                               data = map_data('world'),
                               fill = "#000000", color = "#515151",
                               size = 0.15)
mapcoords <- coord_fixed(xlim = c(-150, 180), ylim = c(-55, 80))

ggplot(nodesLL) + country_shapes +
  geom_curve(aes(x = x, y = y, xend = xend, yend = yend,     # draw edges as arcs
                 color = "#ff4000", size = weight),
             data = edges_for_plot, curvature = 0.33,
             alpha = 0.5) +
  #scale_size_continuous(guide = FALSE, range = c(0.25, 0.25)) + # scale for edge widths
  geom_point(aes(x = lon, y = lat, size = 0.1),           # draw nodes
             shape = 21, fill = 'white',
             color = 'black', stroke = 0.1) +
  scale_size_continuous(guide = FALSE, range = c(0.3, 0.3)) +    # scale for node size
  geom_text(aes(x = lon, y = lat, label = name),             # draw text labels
            hjust = 0, nudge_x = 1, nudge_y = 4,
            size = 1, color = "white", fontface = "bold") +
  mapcoords + maptheme
