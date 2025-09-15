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
B <- matrix(0.005, n_blocks, n_blocks)  # Low baseline connection probability
diag(B) <- 0.22  # Higher within-block probability

node_to_shift <- 77

set.seed(23)

# Generate the SBM adjacency matrix
sbm_graph <- fastRG::sbm(n = n, B = B, pi = block_sizes, poisson_edges = FALSE, allow_self_loops = FALSE)

# Convert to tidygraph format
graph <- sample_tidygraph(sbm_graph) |> 
  mutate(
    block = sbm_graph$z,
    block_int = as.integer(block),
    fill = viridis::viridis(4)[block_int]
  ) |> 
  activate(edges) |> 
  mutate(
    edge_color = "gray",
    edge_linetype = "solid"
  )

layout <- create_layout(graph, layout = "fr")  # fr = Fruchterman-Reingold

# Store the node coordinates
node_coords <- tibble(
  x = layout$x,
  y = layout$y
)


# Add styling information

p1 <- ggraph(graph, layout = "manual", x = node_coords$x, y = node_coords$y) +
  geom_edge_fan(aes(color = edge_color, linetype = edge_linetype), alpha = 0.6) +  # Edge aesthetics
    geom_node_point(aes(fill = fill), size = 1.5, stroke = 0.3, shape = 21, color = "white") +  # White border around nodes
  scale_edge_color_identity() +  # Use defined colors directly
  scale_edge_linetype_identity() +  # Use defined linetypes directly
  scale_fill_identity(guide = "none") +  # Use defined colors directly
  theme_void() +
  labs(
    title = "T = 1"
  ) +
  theme(
    plot.title = element_text(hjust = 0.5),
    text = element_text(family = "fira")  # Apply Fira Sans
  )

p1


# Get nodes from block 3
block3_nodes <- graph |>
  activate(nodes) |> 
  mutate(row_number = row_number()) |>
  filter(block_int == 3) |>
  sample_n(10) |> 
  pull(row_number) 

# Create new edges between our node and block 3 nodes
new_edges <- tibble(
  from = rep(node_to_shift, length(block3_nodes)),
  to = block3_nodes,
  new = TRUE
)

# Add the new edges to the graph
graph3 <- graph |>
  activate(edges) |> 
  filter(from != node_to_shift, to != node_to_shift) |>
  mutate(new = FALSE) |>
  bind_edges(new_edges) |> 
  mutate(
    edge_color = ifelse(new, "steelblue", "gray"),
    edge_linetype = ifelse(new, "dashed", "solid")
  ) |> 
  activate(nodes) |> 
  mutate(
    block_int = if_else(name == node_to_shift, 3, block_int),
    fill = viridis::viridis(4)[block_int]
  )

# Add styling information
p2 <- ggraph(graph3, layout = "manual", x = node_coords$x, y = node_coords$y) +
  geom_edge_fan(aes(color = edge_color, linetype = edge_linetype), alpha = 0.6) +  # Edge aesthetics
    geom_node_point(aes(fill = fill), size = 1.5, stroke = 0.3, shape = 21, color = "white") +  # White border around nodes
  scale_edge_color_identity() +  # Use defined colors directly
  scale_edge_linetype_identity() +  # Use defined linetypes directly
  scale_fill_identity(guide = "none") +  # Use defined colors directly
  theme_void() +
  labs(
    title = "T = 2"
  ) +
  theme(
    plot.title = element_text(hjust = 0.5),
    text = element_text(family = "fira")  # Apply Fira Sans
  )

# Display side by side
library(patchwork)
p1 + p2

ggsave(
  "./figures/time-varying.png",
  width = 5,
  height = 3,
  dpi = 300
)

graph

ggsave("intervention.png", width = 8, height = 4, dpi = 300)


