# ecoregionr NEWS

## ecoregionr 0.3.0 (2025-06-02)

### New features

* Added **spatially constrained clustering** via the SKATER algorithm
  (`run_skater_clustering()`, `plot_skater_elbow()`, `make_skater_heatmap()`,
  `plot_skater_pca_loadings()`). Every cluster produced by SKATER forms a
  geographically contiguous region — no fragmented zones.
* `run_skater_clustering()` supports an optional elevation-boost argument
  (`elev_col` / `elev_weight`) to increase elevation's influence in the PCA
  space used to weight the minimum spanning tree.
* Isolated polygon (island) detection: if any polygon has no neighbours within
  the distance threshold, the function automatically bridges it to its nearest
  neighbour and emits an informative message.
* Added `ecoregionr_dual_workflow.Rmd` — a full annotated workflow demonstrating
  both PCA + k-means and SKATER on the same dataset, with side-by-side maps and
  a method-comparison table.

### Bug fixes

* `make_cluster_heatmap()`: fixed crash when `facet_by = NULL` (the default).
  `group_by(.data[[NULL]])` was called unconditionally; now the grouping
  variable list is built conditionally.

### Package improvements

* Added `spdep (>= 1.2)` to `Imports`.
* Added `knitr`, `rmarkdown`, and `patchwork` to `Suggests`; set
  `VignetteBuilder: knitr`.
* Updated `DESCRIPTION` with `URL`, `BugReports`, and an expanded `Description`
  field covering both clustering strategies.
* Version bumped from 0.2.0 → 0.3.0.

---

## ecoregionr 0.2.0

### New features

* `run_clustering()` now supports `constraint_mode = "split"` for
  elevation-band-stratified clustering.
* `run_ecoregion_workflow()` — one-call wrapper for the full k-means pipeline.
* `generate_cluster_paragraphs()` — auto-generate natural-language bioregion
  descriptions from cluster summary tables.
* `summarize_clusters_compact()` — compact summary of each cluster's dominant
  environmental traits.

---

## ecoregionr 0.1.0

* Initial release.
* Core pipeline: `extract_continuous_stats()`, `extract_categorical_composition()`,
  `build_cluster_table()`, `impute_medians()`, `cluster_pca_kmeans()`,
  `run_clustering()`, `make_cluster_heatmap()`, `build_leaflet_map()`,
  `make_static_map()`.
* Built-in `kg_lookup` (Köppen-Geiger) and `lc_lookup` (ESRI land cover)
  internal lookup tables.
