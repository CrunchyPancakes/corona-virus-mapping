#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
#


library(shiny)
library(dplyr)
library(DT)
library(ggplot2)
library(shinydashboard)
library(plotly)
library(data.table)
library(ape)
library(viridis)
library(phylocanvas)
library(visNetwork)
library(RColorBrewer)
library(igraph)


# Define UI for application that draws a histogram
ui <- dashboardPage(
    dashboardHeader(title = "EpiBoard"),
    dashboardSidebar(
        selectInput("phylo_canvas_layout", "Tree layout:",
                    c("Hierarchical" = "hierarchical",
                      "Rectangular" = "rectangular",
                      "Diagonal" = "diagonal",
                      "Circular" = "circular",
                      "Radial" = "Radial"))
    ),
    dashboardBody(
        tabsetPanel(
            tabPanel("Aotearoa covid-19 genomes",
                fluidRow(
                    valueBoxOutput("genomes"),
                    valueBoxOutput("clusters")
                ),
                fluidRow(
                    box(title = "Phylogenetic tree", solidHeader = T,
                        width = 12, collapsible = T,
                        phylocanvasOutput("phylo_canvas", width = "100%", height = "800px")
                    )
                ), fluidRow(
                    box(title = "Transcendental Information Cascade network", solidHeader = T,
                        width = 12, collapsible = T,
                        visNetworkOutput("phylo_net", width = "100%", height = "800px")
                    )
                )#, fluidRow(
                 #   box(title = "Map output", solidHeader = T,
                 #       width = 12, collapsible = T,
                 #       plotOutput("phylo_map", width = "100%", height = "800px")
                 #   )
                #)
            ),
            tabPanel("Global covid-19 genomes",
                     fluidRow(
                         valueBoxOutput("genomes_g"),
                         valueBoxOutput("clusters_g")
                     ),
                     fluidRow(
                         box(title = "Phylogenetic tree", solidHeader = T,
                             width = 12, collapsible = T,
                             phylocanvasOutput("phylo_canvas_g", width = "100%", height = "800px")
                         )
                     )#, fluidRow(
                      #   box(title = "Transcendental Information Cascade network", solidHeader = T,
                      #       width = 12, collapsible = T,
                      #       visNetworkOutput("phylo_net_g", width = "100%", height = "800px")
                      #   )
                     #)#, fluidRow(
                     #   box(title = "Map output", solidHeader = T,
                     #       width = 12, collapsible = T,
                     #       plotOutput("phylo_map_g", width = "100%", height = "800px")
                     #   )
                     #)
            )
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    # get the data
    all_files <- list.files(path="~/OneDrive - Victoria University of Wellington - STAFF/RScripts/2020-03-09-epidemic-dashboard", pattern = "*.rda",full.names = T)
    for(fil in all_files){
        nVar <- gsub("/Users/MLR/OneDrive - Victoria University of Wellington - STAFF/RScripts/2020-03-09-epidemic-dashboard/","",gsub("\\.rda","",fil))
        
        assign(nVar,readRDS(fil),envir = .GlobalEnv)
    }
    
    output$genomes <- renderValueBox({
        valueBox("Genomes", length(wt$membership), icon = icon("exclamation-triangle"), color = "green")
    })
    
    output$clusters <- renderValueBox({
        valueBox("Clusters", max(wt$membership), icon = icon("exclamation-triangle"), color = "green")
    })
    
    output$genomes_g <- renderValueBox({
        valueBox("Genomes", length(wt_g$membership), icon = icon("exclamation-triangle"), color = "green")
    })
    
    output$clusters_g <- renderValueBox({
        valueBox("Clusters", max(wt_g$membership), icon = icon("exclamation-triangle"), color = "green")
    })
    
    output$phylo_canvas <- renderPhylocanvas({
        n <- max(wt$membership)
        col_vector = viridis_pal(option = "D")(n)
        #plot(as.phylo(as.hclust(wt)), cex = 0.3, label.offset = 1, tip.color = col_vector[wt$membership])
        phycanv <- phylocanvas(as.phylo(as.hclust(wt)),treetype = input$phylo_canvas_layout, alignlabels = T, width = 1000, textsize = 10, nodesize = 10)
        for (nodename in wt$names) {
            nodename1 <- gsub("\\s","_",nodename)
            #print(nodename)
            phycanv$x$nodestyles[[nodename1]]$colour <- col_vector[wt$membership[which(wt$names==nodename)]]
            #phycanv <- style_node(phycanv, nodeid = nodename, color=col_vector[wt$membership[which(wt$names==nodename)]], fillcolor=col_vector[wt$membership[which(wt$names==nodename)]])
        }
        phycanv
    })
    
    output$phylo_canvas_g <- renderPhylocanvas({
        n <- max(wt_g$membership)
        col_vector = viridis_pal(option = "D")(n)
        #plot(as.phylo(as.hclust(wt)), cex = 0.3, label.offset = 1, tip.color = col_vector[wt$membership])
        phycanv <- phylocanvas(as.phylo(as.hclust(wt_g)),treetype = input$phylo_canvas_layout, alignlabels = T, width = 1000, textsize = 10, nodesize = 10)
        for (nodename in wt_g$names) {
            nodename1 <- gsub("\\s","_",nodename)
            #print(nodename)
            phycanv$x$nodestyles[[nodename1]]$colour <- col_vector[wt_g$membership[which(wt_g$names==nodename)]]
            #phycanv <- style_node(phycanv, nodeid = nodename, color=col_vector[wt$membership[which(wt$names==nodename)]], fillcolor=col_vector[wt$membership[which(wt$names==nodename)]])
        }
        phycanv
    })
    
    output$phylo_net <- renderVisNetwork({
        gg <- graph.data.frame(dd, directed=TRUE)
        
        # build tree from clusters
        wt_b <- cluster_walktrap(gg)
        n <- max(wt_b$membership)
        col_vector = viridis_pal(option = "D")(n)
        visNLinks <- dd[,-3]
        visNNodes <- as.data.frame(unique(c(dd$id1,dd$id2)))
        colnames(visNNodes) <- c("id")
        visNNodes$group <- wt_b$membership
        visNNodes$label <- visNNodes$id
        visNNodes$title <- paste0("<p><b>", visNNodes$id,"</b></p>")
        visNNodes$value <- ceiling(strength(gg)/1000)
        visNNodes$color <- col_vector[wt_b$membership]
        colnames(visNLinks) <- c("from","to","weight")
        computedWeights <- visNLinks
        computedWeights$weight[which(visNLinks$weight<30000)] <- 6
        computedWeights$weight[which(visNLinks$weight<20000)] <- 5
        computedWeights$weight[which(visNLinks$weight<10000)] <- 4
        computedWeights$weight[which(visNLinks$weight<1000)] <- 3
        computedWeights$weight[which(visNLinks$weight<100)] <- 2
        computedWeights$weight[which(visNLinks$weight<10)] <- 1
        visNLinks$value <- computedWeights$weight
        visNetwork(nodes=visNNodes ,edges = visNLinks, width="100%", height="400px") %>% 
            visEdges(arrows = "to") %>% 
            visOptions(manipulation = F, selectedBy = "group") %>% 
            visPhysics(solver = "forceAtlas2Based", forceAtlas2Based = list(gravitationalConstant = -500))
        
        #visOptions(visnet, highlightNearest = TRUE, selectedBy = "cluster")
    })
    
    # output$phylo_net_g <- renderVisNetwork({
    #     gg <- graph.data.frame(dd_g, directed=TRUE)
    #     
    #     # build tree from clusters
    #     wt_b <- cluster_walktrap(gg)
    #     n <- max(wt_b$membership)
    #     col_vector = viridis_pal(option = "D")(n)
    #     visNLinks <- dd
    #     visNNodes <- as.data.frame(unique(c(dd_g$id1,dd_g$id2)))
    #     colnames(visNNodes) <- c("id")
    #     visNNodes$group <- wt_b$membership
    #     visNNodes$label <- visNNodes$id
    #     visNNodes$title <- paste0("<p><b>", visNNodes$id,"</b></p>")
    #     visNNodes$value <- ceiling(strength(gg)/1000)
    #     visNNodes$color <- col_vector[wt_b$membership]
    #     colnames(visNLinks) <- c("from","to","weight")
    #     computedWeights <- visNLinks
    #     computedWeights$weight[which(visNLinks$weight<30000)] <- 6
    #     computedWeights$weight[which(visNLinks$weight<20000)] <- 5
    #     computedWeights$weight[which(visNLinks$weight<10000)] <- 4
    #     computedWeights$weight[which(visNLinks$weight<1000)] <- 3
    #     computedWeights$weight[which(visNLinks$weight<100)] <- 2
    #     computedWeights$weight[which(visNLinks$weight<10)] <- 1
    #     visNLinks$value <- computedWeights$weight
    #     visNetwork(nodes=visNNodes ,edges = visNLinks, width="100%", height="400px") %>% 
    #         visEdges(arrows = "to") %>% 
    #         visOptions(manipulation = F, selectedBy = "group") %>% 
    #         visPhysics(solver = "forceAtlas2Based", forceAtlas2Based = list(gravitationalConstant = -500))
    #     
    #     #visOptions(visnet, highlightNearest = TRUE, selectedBy = "cluster")
    # })
    
    # output$phylo_map <- renderPlot({
    #     
    #     g <- graph_from_data_frame(edgesLL, directed = FALSE, vertices = nodesLL)
    #     
    #     edges_for_plot <- edgesLL %>%
    #         inner_join(nodesLL %>% select(id, lon, lat), by = c('from' = 'id')) %>%
    #         rename(x = lon, y = lat) %>%
    #         inner_join(nodesLL %>% select(id, lon, lat), by = c('to' = 'id')) %>%
    #         rename(xend = lon, yend = lat)
    #     
    #     assert_that(nrow(edges_for_plot) == nrow(edgesLL))
    #     
    #     nodesLL$weight = degree(g)
    #     
    #     maptheme <- theme(panel.grid = element_blank()) +
    #         theme(axis.text = element_blank()) +
    #         theme(axis.ticks = element_blank()) +
    #         theme(axis.title = element_blank()) +
    #         theme(legend.position = "none") +
    #         theme(panel.grid = element_blank()) +
    #         theme(panel.background = element_rect(fill = "#ffffff")) +
    #         theme(plot.margin = unit(c(0, 0, 0, 0), 'cm'))
    #     
    #     country_shapes <- geom_polygon(aes(x = long, y = lat, group = group),
    #                                    data = map_data('world'),
    #                                    fill = "#ffffff", color = "#000000",
    #                                    size = 0.15)
    #     mapcoords <- coord_fixed(xlim = c(-150, 180), ylim = c(-55, 80))
    #     
    #     ggplot(nodesLL) + country_shapes +
    #         geom_curve(aes(x = x, y = y, xend = xend, yend = yend,     # draw edges as arcs
    #                        color = "#ff4000", size = weight),
    #                    data = edges_for_plot, curvature = 0.33,
    #                    alpha = 0.5) +
    #         #scale_size_continuous(guide = FALSE, range = c(0.25, 0.25)) + # scale for edge widths
    #         geom_point(aes(x = lon, y = lat, size = 0.1),           # draw nodes
    #                    shape = 21, fill = 'black',
    #                    color = 'black', stroke = 0.1) +
    #         scale_size_continuous(guide = FALSE, range = c(0.5, 0.5)) +    # scale for node size
    #         geom_text(aes(x = lon, y = lat, label = name),             # draw text labels
    #                   hjust = 0, nudge_x = 1, nudge_y = 4,
    #                   size = 1, color = "black", fontface = "bold") +
    #         mapcoords + maptheme
    #     
    #     
    # })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
