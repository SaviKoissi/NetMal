# Network analysis

# Loading packages
library(igraph) 
library(readr)
library(haven)
library (ggplot2)
library(ggraph)
library(reshape2)
library(tcltk)

setwd(tk_choose.dir())
LULC_labels <- read.csv('id.csv', header=T)
LULC_links <- read.csv('network_analysis.csv', header=T)

unique_nodes <- unique(c(as.numeric(LULC_links$Sender), 
                         as.numeric(LULC_links$Target)))
length(unique_nodes)
length(LULC_labels$name)

missing_nodes <- setdiff(as.numeric(LULC_labels$id), unique_nodes)
missing_nodes

# Manually add the missing edge
LULC_links <- LULC_links %>% 
  tibble::add_row(Sender = c(2,2),Target = c(15, 20), Weight = c(1,1))

# Assuming LULC_labels is a data frame
# LULC_labels <- LULC_labels[!LULC_labels$id %in% missing_nodes, ]

# Now, update the graph
graph <- graph_from_data_frame(LULC_links, directed = TRUE)
V(graph)$label <- LULC_labels$name

# Plot the updated graph
plot(graph, edge.arrow.size = 0.5, layout = layout_with_kk(graph))

# Better visualization

# Use Fruchterman-Reingold layout
layout <- layout_with_fr(graph)

# Plot the graph using ggraph
ggraph(graph, layout = layout) +
  geom_edge_link() +
  geom_node_point() +
  geom_node_text(aes(label = name), repel = TRUE) +
  theme_void()

V(graph)$label <- LULC_labels$name

ggraph(graph, layout = layout) +
  geom_edge_link() +
  geom_node_point() +
  geom_node_text(aes(label = label), repel = TRUE) +
  theme_void()

# Simple visualization
set.seed(1123)

plot(graph,
     vertex.label = V(graph)$name,  # Assuming you have node names as labels
     vertex.label.cex = 0.8,  # Adjust label size
     vertex.label.color = "black",  # Label color
     edge.arrow.size = 0.5,  # Adjust arrow size
     edge.label = LULC_links$Weight,  # Use edge weights as labels
     edge.label.cex = 0.8,  # Adjust label size
     edge.label.color = ifelse(LULC_links$Weight == 1, "green", "red"),  # Use green for positive and red for negative edges
     main = "Directed Network Plot with Positive and Negative Edges"
)

# Simple visualiz
set.seed(1123)

# Assuming 'graph' is your existing graph created from the edge list

# Define layout for better visualization
layout <- create_layout(graph, layout = "fr")

# Plot the directed graph with positive and negative edges using ggraph
ggraph(layout, 'fr') +
  geom_edge_link(
    aes(
      alpha = LULC_links$Weight,
      color = factor(LULC_links$Weight)
    ),
    show.legend = FALSE
  ) +
  geom_node_point(aes(color = "black"), size = 3) +
  geom_node_text(aes(label = name), repel = TRUE) +
  theme_void() +
  theme(legend.position = "none")

# Study of the topology of the network
summary(graph)

# In-degree distribution (number of incoming edges)
in_degree <- degree(graph, mode = "in")

table(in_degree)

# Out-degree distribution (number of outgoing edges)
out_degree <- degree(graph, mode = "out")

table(out_degree)

# Plot degree distributions
hist(in_degree, main = "In-degree Distribution", xlab = "In-degree")
hist(out_degree, main = "Out-degree Distribution", xlab = "Out-degree")

#Step 5: Clustering Coefficient

# Calculate clustering coefficient
clustering_coefficient <- transitivity(graph, type = "local")

# Plot clustering coefficient distribution
hist(clustering_coefficient, main = "Clustering Coefficient Distribution", 
     xlab = "Clustering Coefficient")

#Step 6: Path Length
# Calculate average shortest path length
average_path_length <- average.path.length(graph, directed = TRUE)

# Print the result
cat("Average Shortest Path Length:", average_path_length, "\n")

# Calculate degree centrality
degree_centrality <- degree(graph, mode = "all")

# Calculate betweenness centrality
betweenness_centrality <- betweenness(graph, directed = TRUE)

# Calculate closeness centrality
closeness_centrality <- closeness(graph, mode = "all")

# Combine centrality measures into a data frame
centrality_data <- data.frame(
  Node = V(graph)$name,
  Degree = degree_centrality,
  Betweenness = betweenness_centrality,
  Closeness = closeness_centrality
)

# Print the centrality data
print(centrality_data)



# Centrality measures 
# Calculate eigenvector centrality
eigenvector_centrality <- eigen_centrality(graph)$vector

# Calculate PageRank
pagerank_values <- page.rank(graph)$vector

# Centrality measures into the existing data frame
centrality_data <- data.frame(
  Node = V(graph)$name,
  Degree = degree_centrality,
  Betweenness = betweenness_centrality,
  Closeness = closeness_centrality,
  Eigenvector = eigenvector_centrality,
  PageRank = pagerank_values
)

# Print the updated centrality data
print(centrality_data)

# Visualization

# Combine centrality measures into a single data frame
centrality_data <- data.frame(
  Node = V(graph)$name,
  Degree = degree_centrality,
  Betweenness = betweenness_centrality,
  Closeness = closeness_centrality,
  Eigenvector = eigenvector_centrality,
  PageRank = pagerank_values
)

# Melt the data frame for easy plotting

melted_centrality_data <- melt(centrality_data, id.vars = "Node")

# Plot the centrality measures

ggplot(melted_centrality_data, aes(x = variable, y = value, fill = Node)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Centrality Measures",
       x = "Centrality Measure",
       y = "Centrality Value") +
  theme_minimal()

# Community Detection
# Assuming 'graph' is your directed graph
walktrap_communities <- walktrap.community(graph)

# Access the community membership
community_membership <- membership(walktrap_communities)

# Assortativity

# Assuming 'graph' is your directed graph
assortativity(graph, directed = TRUE, types1 = degree(graph, 
                                                      mode = "in"), 
              types2 = degree(graph, mode = "out"))




