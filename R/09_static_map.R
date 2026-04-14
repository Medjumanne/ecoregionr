# =============================================================================
# 9. make_static_map
# =============================================================================
#' Publication-quality ggplot map of cluster assignments.

#' @param polygons_clustered  sf object.
#' @param cluster_col         Column used for fill.
#' @param border_sf           Optional sf for overlaid borders.
#' @param projection          PROJ string (default Mollweide for Africa).
#' @param viridis_option      Viridis palette (default "turbo").
#' @param legend_title        Legend title.
#' @param ...                 Extra arguments passed to theme().
#'
#' @return A ggplot object.
#' @export
make_static_map <- function(polygons_clustered,
                            cluster_col    = "cluster",
                            border_sf      = NULL,
                            projection     = "+proj=moll",
                            viridis_option = "turbo",
                            legend_title   = "Cluster",
                            simplify       = TRUE,
                            keep_prop      = 0.02,
                            ...) {

  # ── Speed fix 1: simplify filled polygons only ──────────────────────────────
  # Only the clustered fill layer needs simplification — it can have millions of
  # vertices across hundreds of admin-2 polygons.  The border_sf is a single
  # country outline and renders fast as-is; simplifying it at keep = 0.02
  # collapses it to a coarse triangle, which is why it looks wrong on the map.
  if (simplify) {
    if (requireNamespace("rmapshaper", quietly = TRUE)) {
      polygons_clustered <- rmapshaper::ms_simplify(polygons_clustered,
                                                    keep = keep_prop,
                                                    keep_shapes = TRUE)
    } else {
      tol <- 0.01   # degrees; increase if still slow
      polygons_clustered <- sf::st_simplify(polygons_clustered,
                                            dTolerance = tol,
                                            preserveTopology = TRUE)
    }
    # border_sf is intentionally NOT simplified — keep its full detail
  }

  # ── Speed fix 2: pre-project before ggplot ──────────────────────────────────
  # coord_sf() reprojects every vertex at render time.  Pre-projecting once
  # means ggplot only needs to draw, not reproject.
  polygons_clustered <- sf::st_transform(polygons_clustered, crs = projection)
  if (!is.null(border_sf))
    border_sf <- sf::st_transform(border_sf, crs = projection)

  p <- ggplot() +
    geom_sf(data = polygons_clustered,
            aes(fill = factor(.data[[cluster_col]])),
            color = "grey20", linewidth = 0.25) +
    scale_fill_viridis_d(
      option = viridis_option,
      name   = legend_title,
      guide  = guide_legend(ncol = 1, title.position = "top")
    ) +
    annotation_north_arrow(
      location = "tl", which_north = "true",
      style    = north_arrow_minimal(),
      height   = unit(1.2, "cm"), width = unit(1.2, "cm")
    ) +
    annotation_scale(location = "bl", width_hint = 0.25, text_cex = 0.9) +
    labs(x = "Longitude", y = "Latitude") +
    coord_sf() +   # no crs= needed; data is already projected
    theme(
      panel.grid.major = element_line(color = "grey70", linewidth = 0.5),
      panel.grid.minor = element_line(color = "grey85", linewidth = 0.3),
      legend.title     = element_text(face = "bold"),
      legend.box.background = element_rect(fill = "white", color = "black"),
      ...
    )

  if (!is.null(border_sf)) {
    p <- p + geom_sf(data = border_sf, fill = NA,
                     color = "black", linewidth = 0.8)
  }

  return(p)
}

