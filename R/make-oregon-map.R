library(sf)
library(ggplot2)
library(dplyr)
library(spdep)
library(tigris)
library(showtext)

# Load Google Font "Fira Sans"
showtext_auto()
font_add_google("Fira Sans", "fira")
showtext_opts(dpi = 300)  # Ensure proper font embedding

# Load Oregon counties shapefile
options(tigris_use_cache = TRUE)
oregon_counties <- counties(state = "OR", cb = TRUE, class = "sf")

# Compute centroids
oregon_counties <- oregon_counties %>% mutate(centroid = st_centroid(geometry))
centroids <- st_coordinates(oregon_counties$centroid)
centroid_df <- data.frame(NAME = oregon_counties$NAME, X = centroids[,1], Y = centroids[,2])

# Create neighbor relationships
nb <- poly2nb(oregon_counties)  # Find neighboring polygons

# Create connecting lines between neighboring county centroids with lengths
neighbor_lines <- list()
for (i in seq_along(nb)) {
  if (length(nb[[i]]) > 0) {
    for (j in nb[[i]]) {
      line_geom <- st_linestring(rbind(centroids[i,], centroids[j,]))
      line_sf <- st_sf(geometry = st_sfc(line_geom, crs = st_crs(oregon_counties)))
      line_sf$length <- as.numeric(st_length(line_sf))
      neighbor_lines <- append(neighbor_lines, list(line_sf))
    }
  }
}

# Combine valid lines into an sf object
if (length(neighbor_lines) > 0) {
  neighbor_sf <- do.call(rbind, neighbor_lines)
} else {
  neighbor_sf <- st_sf(geometry = st_sfc(), crs = st_crs(oregon_counties))
}

# Create the plot
oregon_plot <- ggplot() +
  geom_sf(data = oregon_counties, fill = "white", color = "black") +
  geom_sf(data = neighbor_sf, aes(color = length), size = 0.8) +
  scale_color_gradient(high = "#D3D3D3", low = "#4F4F4F", name = "Distance") +  # Muted color scale
  geom_point(data = centroid_df, aes(X, Y), color = "firebrick", size = 1) +
  theme_minimal() +
  theme(
    axis.title = element_blank(),
    legend.position = "none",  # Remove the legend
    text = element_text(family = "fira")  # Apply Fira Sans
  )

# Save as a PDF with embedded Fira Sans
ggsave("oregon_map.png",  width = 3 * 16/9, height = 3, dpi = 500)
