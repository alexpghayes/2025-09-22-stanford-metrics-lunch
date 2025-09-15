library(fastRG)
library(tidygraph)
library(ggraph)
library(igraph)
library(ggplot2)
library(dplyr)
library(here)

# Define parameters
n_blocks <- 4
n <- 80  # Total nodes
block_sizes <- rep(1 / n_blocks, n_blocks)  # Proportions for each block

# Define a connectivity matrix favoring within-block edges
B <- matrix(0.01, n_blocks, n_blocks)  # Low baseline connection probability
diag(B) <- 0.3  # Higher within-block probability

set.seed(27)

# Generate the SBM adjacency matrix
sbm_graph <- fastRG::sbm(n = n, B = B, pi = block_sizes, poisson_edges = FALSE, allow_self_loops = FALSE)

# Convert to tidygraph format
graph <- sample_tidygraph(sbm_graph) |> 
  mutate(block = sbm_graph$z) |> 
  activate(edges) |> 
  mutate(edge_id = row_number())
    

# Extract edges and assign unique IDs
 
edges <- graph |> 
  as_tibble()

dashed_edges <- sample(edges$edge_id, 25)

# Add styling information
graph |> 
  activate(edges) |> 
  mutate(
    edge_color = ifelse(edge_id %in% dashed_edges, "firebrick", "gray"),
    edge_linetype = ifelse(edge_id %in% dashed_edges, "dashed", "solid")
  ) |> 
ggraph(layout = "fr") + 
geom_edge_fan(aes(color = edge_color, linetype = edge_linetype), alpha = 0.6) +  # Edge aesthetics
geom_node_point(aes(fill = block), size = 3, stroke = 1.2, shape = 21, color = "white") +  # White border around nodes
scale_edge_color_identity() +  # Use defined colors directly
scale_edge_linetype_identity() +  # Use defined linetypes directly
theme_void() + 
scale_fill_viridis_d(guide = "none")

ggsave(
  "./figures/missing-edges.pdf",
  width = 5,
  height = 3,
  dpi = 300
)

graph
