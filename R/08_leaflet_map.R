# =============================================================================
# 8. build_leaflet_map
# =============================================================================
#' Build an interactive Leaflet map of cluster assignments.

#' @param polygons_clustered  sf object with cluster column and popup fields.
#' @param cluster_col         Name of the cluster column.
#' @param label_fn            A function(row) → HTML string for popup labels.
#'                            If NULL, a simple default is used.
#' @param border_sf           Optional sf of country/region borders (outlines).
#' @param palette_option      viridis palette name for clusters (default "turbo").
#' @param title               Legend title.
#'
#' @return A leaflet map object.
#' @export
build_leaflet_map <- function(polygons_clustered,
                              cluster_col      = "cluster",
                              label_fn         = NULL,
                              border_sf        = NULL,
                              palette_option   = "turbo",
                              title            = "Cluster") {

  clust_vals <- polygons_clustered[[cluster_col]]
  pal <- colorFactor(
    palette = viridis(length(unique(clust_vals[!is.na(clust_vals)])),
                      option = palette_option),
    domain = clust_vals
  )

  # Default label
  if (is.null(label_fn)) {
    label_fn <- function(df) {
      htmltools::HTML(paste0("<b>Cluster: </b>", df[[cluster_col]]))
    }
  }

  labels <- lapply(seq_len(nrow(polygons_clustered)), function(i) {
    label_fn(polygons_clustered[i, ])
  })

  # leaflet uses R's ~ formula interface, not dplyr's .data pronoun.
  # Build formulas dynamically from the column name string.
  fill_formula   <- as.formula(paste0("~ pal(", cluster_col, ")"))
  values_formula <- as.formula(paste0("~ ", cluster_col))

  # Compute bounding box in WGS84 so fitBounds() always works
  bbox <- sf::st_bbox(sf::st_transform(polygons_clustered, 4326))

  m <- leaflet(polygons_clustered) |>
    addTiles() |>
    fitBounds(                        # zoom to data on load
      lng1 = bbox[["xmin"]], lat1 = bbox[["ymin"]],
      lng2 = bbox[["xmax"]], lat2 = bbox[["ymax"]]
    ) |>
    addPolygons(
      fillColor   = fill_formula,
      weight      = 0.6,
      color       = "black",
      fillOpacity = 0.75,
      label       = labels,
      highlightOptions = highlightOptions(
        weight = 2, color = "white", fillOpacity = 0.9, bringToFront = TRUE
      )
    ) |>
    addLegend(pal = pal, values = values_formula,
              title = title, opacity = 0.7)

  if (!is.null(border_sf)) {
    border_sf <- sf::st_transform(border_sf, crs = sf::st_crs(polygons_clustered))
    m <- m |> addPolygons(data  = border_sf, fill = FALSE,
                          color = "black", weight = 2)
  }

  return(m)
}

