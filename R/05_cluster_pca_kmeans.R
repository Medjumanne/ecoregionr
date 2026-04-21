# =============================================================================
# 5. cluster_pca_kmeans  (single-group)
# =============================================================================
#' PCA + k-means clustering for one group (e.g. one country).
#'
#' Logic is a direct generic port of the original cluster_country_pca_kmeans().
#' Only the hardcoded column names ("ID", country column) are replaced by
#' parameters; everything else — NbClust call, vote counting, min/max k
#' enforcement, silhouette fallback, visualisations — is identical.

#' @param df          Tibble / data.frame for one group.
#' @param group_col   Name of the grouping column (e.g. "ADM0_NAME").
#'                    Used for status messages and excluded from clustering.
#' @param id_col      Name of the unique ID column (excluded from clustering).
#' @param min_k       Minimum number of clusters to allow (default 6, matching
#'                    the original NbClust min.nc = 6).
#' @param max_k       Maximum number of clusters to test (default 15).
#' @param pca_var     Cumulative variance threshold for PC retention (default 0.90).
#' @param nstart      k-means nstart for the final run (default 50).
#' @param visualize   Logical – print consensus dot-plot, scree, PCA, and
#'                    silhouette plots (default TRUE).
#' @param seed        Random seed (default 1).
#'
#' @return The input data frame with extra columns: `cluster` (factor),
#'         `k_used` (integer), and PC score columns (PC1, PC2, …).
#'@export
cluster_pca_kmeans <- function(df,
                               group_col  = NULL,
                               id_col     = "ID",
                               min_k      = MIN_K,
                               max_k      = MAX_K,
                               pca_var    = 0.90,
                               nstart     = 100,
                               visualize  = TRUE,
                               seed       = 1) {

  set.seed(seed)

  group_label <- if (!is.null(group_col)) unique(df[[group_col]])[1] else "group"

  # ── 1. Prepare feature matrix ──────────────────────────────────────────────
  # Drop ID and grouping column exactly as the original does
  exclude_cols <- unique(c(id_col, if (!is.null(group_col)) group_col))
  X <- df[, !names(df) %in% exclude_cols, drop = FALSE]

  # Remove zero-variance variables (original logic preserved exactly)
  sds <- apply(X, 2, sd, na.rm = TRUE)
  sds[is.na(sds)] <- 0
  removed_vars <- names(X)[sds == 0]
  X <- X[, sds > 0, drop = FALSE]
  if (length(removed_vars) > 0)
    message(group_label, " | Removed ", length(removed_vars),
            " zero-variance variables")

  # Remove rows with NA / Inf (original logic preserved exactly)
  keep_rows <- apply(X, 1, function(r) all(is.finite(r)))
  X  <- X[keep_rows, , drop = FALSE]
  df <- df[keep_rows, , drop = FALSE]

  # Small-dataset guard (original used nrow < 8)
  if (ncol(X) < 3 || nrow(X) < 8) {
    message(group_label, " | Too small → single cluster")
    df$cluster <- factor(1)
    df$k_used  <- 1L
    return(df)
  }

  # ── 2. Scale ───────────────────────────────────────────────────────────────
  X_scaled <- scale(X)

  # ── 3. PCA ─────────────────────────────────────────────────────────────────
  pca_res <- prcomp(X_scaled, center = FALSE, scale. = FALSE)
  cum_var <- cumsum(pca_res$sdev^2) / sum(pca_res$sdev^2)
  n_pc    <- which.max(cum_var >= pca_var)
  n_pc    <- min(max(n_pc, 3L), ncol(X_scaled))

  message(group_label, " | Using ", n_pc, " PCs (",
          round(cum_var[n_pc] * 100, 1), "% variance explained)")

  pca_data           <- as.data.frame(pca_res$x[, 1:n_pc])
  colnames(pca_data) <- paste0("PC", seq_len(n_pc))
  df                 <- cbind(df, pca_data)

  # ── 4. Determine k via NbClust (original logic, line-for-line) ─────────────
  message(group_label, " | Running NbClust...")

  nb_res <- tryCatch(
    NbClust(pca_data, min.nc = min_k, max.nc = max_k,
            method = "kmeans", index = "all"),
    error = function(e) NULL
  )

  if (!is.null(nb_res)) {

    # Pass 1 – majority-rule k  (mirrors original exactly)
    vote_data <- as.integer(nb_res$Best.nc[1, ])
    vote_data <- vote_data[is.finite(vote_data)]
    vote_data <- vote_data[vote_data >= min_k & vote_data <= max_k]
    k_raw     <- as.integer(names(which.max(table(vote_data))))

    # Pass 2 – rebuild vote table for the consensus plot  (mirrors original)
    vote_data    <- nb_res$Best.nc[1, ]
    vote_data    <- vote_data[vote_data >= min_k & vote_data <= max_k]
    consensus_df <- as.data.frame(table(vote_data)) |>
      rename(k = vote_data, votes = Freq) |>
      mutate(k = as.integer(as.character(k))) |>
      arrange(desc(votes))

    if (visualize) {
      print(
        ggplot(consensus_df, aes(x = votes, y = reorder(factor(k), votes))) +
          geom_segment(aes(x = 0, xend = votes, yend = factor(k)),
                       color = "grey70") +
          geom_point(aes(color = votes), size = 4) +
          scale_color_viridis_c(option = "viridis", guide = "none") +
          geom_text(aes(label = votes), vjust = -0.8, size = 3) +
          labs(title    = paste("Cluster Selection Consensus:", group_label),
               subtitle = paste("Dominant Suggestion: k =", k_raw),
               x = "Number of Indices (Votes)",
               y = "Suggested Number of Clusters (k)") +
          theme_test()
      )
    }

  } else {
    # Silhouette fallback (original used `which.max(sil) + 1` which is a known
    # off-by-one bug when min_k > 2; corrected here to `+ min_k - 1`)
    sil   <- sapply(seq(min_k, max_k), function(k) {
      km <- kmeans(pca_data, centers = k, nstart = 25)
      mean(silhouette(km$cluster, dist(pca_data))[, 3], na.rm = TRUE)
    })
    k_raw <- which.max(sil) + min_k - 1L
    message(group_label, " | NbClust failed, silhouette k = ", k_raw)
  }

  # ── 5. Apply min/max k constraint (original logic) ─────────────────────────
  message("k_raw = ", k_raw, " | min_k = ", min_k, " | max_k = ", max_k)
  k_opt <- max(min_k, min(k_raw, max_k))
  message(group_label, " | Final k (after min constraint) = ", k_opt)

  # ── 6. Final k-means ───────────────────────────────────────────────────────
  final_km   <- kmeans(pca_data, centers = k_opt, nstart = nstart)
  df$cluster <- factor(final_km$cluster)
  df$k_used  <- k_opt

  # ── 7. Diagnostic plots (original order and style) ─────────────────────────
  if (visualize) {

    cat("\n=== Visual diagnostics for", group_label, "===\n")

    # Scree plot with red dashed line at n_pc
    print(
      fviz_eig(pca_res, addlabels = TRUE, ncp = 15) +
        geom_vline(xintercept = n_pc, linetype = "dashed",
                   color = "red", linewidth = 1) +
        labs(title    = paste("PCA Scree Plot -", group_label),
             subtitle = paste("Dotted line indicates", n_pc,
                              "PCs used (", round(pca_var * 100),
                              "% variance threshold)"))
    )

    # PCA individual plot with convex ellipses
    pal_n <- colorRampPalette(RColorBrewer::brewer.pal(12, "Paired"))(max_k)
    print(
      fviz_pca_ind(pca_res,
                   geom        = "point",
                   habillage   = final_km$cluster,
                   palette     = pal_n,
                   addEllipses = TRUE,
                   ellipse.type = "convex",
                   title = paste("PCA Clusters -", group_label)) +
        theme_test()
    )

    # Silhouette plot
    sil_obj <- silhouette(final_km$cluster, dist(pca_data))
    print(
      fviz_silhouette(sil_obj) +
        labs(title = paste("Silhouette (k =", k_opt, ") -", group_label))
    )
  }

  return(df)
}

