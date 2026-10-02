###
# Maps comparing the thresholded SDM of the round-2 baseline (run09162026_1k)
# with each method-test arm (thinning, full correlation pruning, both).
# One PNG per taxon in temp/methodTests_20261002/maps/, plus a table of
# modelled-area change. Thinning only changes the points given to the model
# (generateModelData); natural area and every gap metric still use all points.
#   Rscript work2026/methodTestMaps_20261002.R
###
suppressMessages({
  library(terra); library(sf); library(dplyr); library(readr)
  library(ggplot2); library(tidyterra); library(patchwork)
})

if (!exists("baseRun")) baseRun <- "run09162026_1k"
arms <- c("5 km thinning" = "run10022026_thin",
          "Full correlation pruning" = "run10022026_fullcorr",
          "Thinning + pruning" = "run10022026_both")
if (!exists("taxa")) taxa <- c("Vitis lincecumii", "Vitis munsoniana", "Vitis x novae-angliae", "Vitis peninsularis")
outDir <- "temp/methodTests_20261002/maps"
dir.create(outDir, recursive = TRUE, showWarnings = FALSE)

cols <- c("Both runs" = "#c3c2b7", "Baseline only (lost)" = "#eb6834", "Test run only (gained)" = "#2a78d6")
land <- rnaturalearth::ne_states(country = c("united states of america", "mexico", "canada"), returnclass = "sf")

# presence points that entered the model, from the geometry text column
modelPoints <- function(path){
  d <- read_csv(path, show_col_types = FALSE) |> filter(presence == 1)
  xy <- regmatches(d$geometry, gregexpr("-?[0-9.]+", d$geometry))
  tibble(x = as.numeric(sapply(xy, `[`, 1)), y = as.numeric(sapply(xy, `[`, 2)))
}

areaRows <- list()
for (t in taxa){
  dirOf <- function(run) file.path("data/Vitis", t, run)
  base <- rast(file.path(dirOf(baseRun), "results/prj_threshold.tif"))
  natArea <- st_read(file.path(dirOf(baseRun), "results/naturalArea.gpkg"), quiet = TRUE) |> st_union()
  allPts <- st_read(file.path(dirOf(baseRun), "occurances/spatialData.gpkg"), quiet = TRUE)
  allXY <- as_tibble(st_coordinates(allPts)) |> rename(x = X, y = Y)
  cellKm2 <- cellSize(base, unit = "km")
  bb <- st_bbox(natArea)

  panels <- lapply(names(arms), function(a){
    test <- rast(file.path(dirOf(arms[[a]]), "results/prj_threshold.tif"))
    b <- subst(base, NA, 0); s <- subst(resample(test, base, method = "near"), NA, 0)
    cls <- ifel(b == 1 & s == 1, 1, ifel(b == 1, 2, ifel(s == 1, 3, NA)))
    km <- zonal(cellKm2, cls, "sum", na.rm = TRUE)
    getKm <- function(v) { x <- km[km[[1]] == v, 2]; if (length(x)) x else 0 }
    both <- getKm(1); lost <- getKm(2); gained <- getKm(3)
    used <- modelPoints(file.path(dirOf(arms[[a]]), "occurances/allmodelData.csv"))
    key <- function(d) paste(round(d$x, 5), round(d$y, 5))
    pts <- allXY |> mutate(status = ifelse(key(allXY) %in% key(used), "In model", "Thinned out"))
    areaRows[[length(areaRows) + 1]] <<- tibble(
      taxon = t, arm = a, points_all = nrow(allXY), points_in_model = sum(pts$status == "In model"),
      baseline_km2 = round(both + lost), test_km2 = round(both + gained),
      lost_km2 = round(lost), gained_km2 = round(gained),
      pct_change = round(100 * (gained - lost) / (both + lost), 1),
      overlap_pct = round(100 * both / (both + lost + gained), 1))
    auc <- read_csv(file.path(dirOf(arms[[a]]), "results/aucMetrics.csv"), show_col_types = FALSE)
    validTxt <- if (isTRUE(as.logical(auc$Valid[1]))) "model passes validity rule" else paste0("MODEL FAILS validity rule: ", auc$Reason[1])
    levels(cls) <- data.frame(id = 1:3, class = names(cols))
    ggplot() +
      geom_sf(data = land, fill = "#f9f9f7", colour = "#e1e0d9", linewidth = 0.2) +
      geom_spatraster(data = cls, maxcell = 8e5) +
      scale_fill_manual(values = cols, na.value = NA, na.translate = FALSE, drop = FALSE, name = "Modelled distribution") +
      geom_sf(data = natArea, fill = NA, colour = "#52514e", linewidth = 0.3, linetype = "dashed") +
      # thinned-out points sit within 5 km of a kept one, so draw them last
      geom_point(data = arrange(pts, status), aes(x, y, shape = status, size = status), stroke = 0.4, colour = "#0b0b0b", fill = "#fcfcfb", show.legend = TRUE) +
      scale_shape_manual(values = c("In model" = 21, "Thinned out" = 16), limits = c("In model", "Thinned out"), name = "Occurrence points") +
      scale_size_manual(values = c("In model" = 1.5, "Thinned out" = 0.6), limits = c("In model", "Thinned out"), name = "Occurrence points") +
      coord_sf(xlim = bb[c(1, 3)], ylim = bb[c(2, 4)], expand = TRUE) +
      labs(title = a,
           subtitle = sprintf("%d of %d points in model\narea %s -> %s km2 (%+.1f%%), overlap %.0f%%\n%s (test AUC %.3f)",
                              sum(pts$status == "In model"), nrow(pts),
                              format(round(both + lost), big.mark = ","), format(round(both + gained), big.mark = ","),
                              100 * (gained - lost) / (both + lost), 100 * both / (both + lost + gained), validTxt, auc$ATAUC[1]),
           x = NULL, y = NULL) +
      theme_minimal(base_size = 9) +
      theme(panel.grid = element_line(colour = "#e1e0d9", linewidth = 0.2),
            plot.title = element_text(face = "bold", size = 10),
            plot.subtitle = element_text(colour = "#52514e", size = 7.5),
            axis.text = element_text(colour = "#898781", size = 6))
  })
  fig <- wrap_plots(panels, nrow = 1, guides = "collect") +
    plot_annotation(title = paste0(t, ": baseline (", baseRun, ") vs method tests"),
                    caption = "Dashed line: natural area (from all points, identical in every run). Gap metrics use all points; thinning only changes the points given to the model.",
                    theme = theme(plot.title = element_text(face = "bold", size = 12),
                                  plot.caption = element_text(colour = "#52514e", size = 7, hjust = 0))) &
    theme(legend.position = "bottom", plot.background = element_rect(fill = "#fcfcfb", colour = NA))
  ggsave(file.path(outDir, paste0(gsub(" ", "_", t), "_methodTests.png")), fig, width = 15, height = 6.2, dpi = 170)
  message("written: ", t)
}
write_csv(bind_rows(areaRows), file.path(outDir, "modelledArea_baseline_vs_tests.csv"))
print(as.data.frame(bind_rows(areaRows)), row.names = FALSE)
