# fig_theme.R
# Shared plotting theme and palette for all manuscript figures.
# Source this at the top of each figure script: source("fig_theme.R")
#
# Design rules enforced here:
#   - No text is drawn inside plot panels by any figure script.
#     Panel tags (A, B, C) are added at composition time via
#     patchwork::plot_annotation(tag_levels = "A"). All interpretive
#     text lives in the figure captions, not on the plot.
#   - Colorblind-safe palette (Wong 2011, Nature Methods).
#   - Vector PDF plus 600 dpi TIFF/PNG output for publication.

suppressPackageStartupMessages({
  library(ggplot2)
})

# ---- Wong (2011) colorblind-safe palette ----
PAL <- c(
  black   = "#000000",
  orange  = "#E69F00",
  skyblue = "#56B4E9",
  green   = "#009E73",
  yellow  = "#F0E442",
  blue    = "#0072B2",
  vermil  = "#D55E00",
  purple  = "#CC79A7",
  grey    = "#999999"
)

# Semantic assignments used across figures
COL_SPREAD  <- PAL[["blue"]]
COL_CLUSTER <- PAL[["vermil"]]
COL_BUDGET  <- PAL[["purple"]]
COL_THRESH  <- PAL[["grey"]]
COL_90      <- PAL[["green"]]
COL_180     <- PAL[["blue"]]
COL_365     <- PAL[["skyblue"]]

# Sequential ramp for the four multi-year caseload levels (low to high)
COL_SEQ4 <- c("#9ecae1", "#4292c6", "#08519c", "#08306b")

# Sobol tier colors
COL_TIER <- c(
  "Major"      = PAL[["vermil"]],
  "Moderate"   = PAL[["blue"]],
  "Minor"      = PAL[["skyblue"]],
  "Negligible" = PAL[["grey"]]
)

# Scenario ramp for the Fig 5B interim-supply panel (mild to severe)
COL_INTERIM <- c(
  "15 spread (SW06)"          = "#0072B2",
  "15 cluster (S03)"          = "#E69F00",
  "20 cluster (S04)"          = "#D55E00",
  "15 cluster, 1 slot (CQ01)" = "#882255",
  "30 cluster (S05)"          = "#440154"
)

# ---- Theme ----
theme_paper <- function(base_size = 11, base_family = "") {
  # Sized for printing at 7.2-inch (double-column) journal width.
  # All sizes are absolute so they do not shrink when scaled to print.
  # Legends sit BELOW panels so panel tags at top-left never collide.
  theme_classic(base_size = base_size, base_family = base_family) +
    theme(
      plot.title       = element_blank(),   # never draw titles in-panel
      plot.subtitle    = element_blank(),
      axis.title       = element_text(size = 11, color = "black"),
      axis.text        = element_text(size = 10, color = "black"),
      axis.line        = element_line(linewidth = 0.5, color = "black"),
      axis.ticks       = element_line(linewidth = 0.5, color = "black"),
      axis.ticks.length = unit(0.18, "lines"),
      legend.position  = "bottom",
      legend.title     = element_blank(),
      legend.text      = element_text(size = 9.5, color = "black"),
      legend.key.size  = unit(0.7, "lines"),
      legend.key.width = unit(1.1, "lines"),
      legend.spacing.x = unit(0.2, "lines"),
      legend.margin    = margin(2, 0, 0, 0),
      legend.box.margin = margin(0, 0, 0, 0),
      plot.tag         = element_text(face = "bold", size = 14, color = "black"),
      plot.tag.position = "topleft",
      plot.margin      = margin(12, 14, 6, 6)
    )
}

# ---- Output helper: write PDF (vector) + high-res TIFF ----
save_pub <- function(plot, stem, width, height, dpi = 600,
                     dir = "outputs/figures") {
  if (!dir.exists(dir)) dir.create(dir, recursive = TRUE)
  ggsave(file.path(dir, paste0(stem, ".pdf")), plot,
         width = width, height = height, bg = "white")
  ggsave(file.path(dir, paste0(stem, ".tiff")), plot,
         width = width, height = height, dpi = dpi, bg = "white",
         compression = "lzw")
  invisible(plot)
}

# ---- Scenario -> annual caseload maps (from build scripts) ----
SW_CASES <- data.frame(
  scenario_id = sprintf("SW%02d", 1:10),
  cases = c(3, 5, 8, 10, 12, 15, 18, 20, 25, 30),
  stringsAsFactors = FALSE
)

CLUST_CASES <- data.frame(
  scenario_id = c("S03", "S04", "S05"),
  cases = c(15, 20, 30),
  stringsAsFactors = FALSE
)

# CQ slot positions. Unlimited is stored as NA in the data
# (max_concurrent_construction); for plotting it is placed one even
# step beyond 8 on a manual position scale. Build order guarantees
# CQ01-06 = summer cluster, CQ07-12 = spread even, each over
# slots = 1, 2, 3, 5, 8, unlimited.
CQ_SLOTS <- data.frame(
  scenario_id = sprintf("CQ%02d", 1:12),
  regime = c(rep("cluster", 6), rep("spread", 6)),
  slot_pos = rep(1:6, 2),                       # even spacing on x
  slot_label = rep(c("1", "2", "3", "5", "8", "unlim"), 2),
  stringsAsFactors = FALSE
)
