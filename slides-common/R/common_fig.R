# Shared constants and helpers for future LAS statistical figures.

color_bg <- "#F4F1EA"
color_primary <- "#173F48"
color_primary_dark <- "#102F36"
color_secondary <- "#6F9878"
color_secondary_light <- "#A9C4AD"
color_accent <- "#D58A3D"
color_accent_light <- "#F1D4AF"
# Decision regions use red; observed-result tail probabilities use teal.
color_rejection <- "#B83B42"
color_rejection_light <- "#F4DADC"
color_pvalue <- "#287D86"
color_pvalue_light <- "#C8E4E5"
color_text <- "#19343B"
color_muted <- "#5C6E72"
color_white <- "#FFFFFF"
color_grid <- "#D3DDD6"
color_panel_cool <- "#E6EEE8"
color_panel_warm <- "#F5E7D5"

# These aspect ratios match the standard LAS visual and full-width slide slots.
FIG_LAYOUTS <- list(
  right = list(aspect = 1.00, point_size = 20),
  full = list(aspect = 2.04, point_size = 18)
)

open_png <- function(path, layout = c("right", "full"), width_px = 3000, bg = color_bg) {
  layout <- match.arg(layout)
  layout_cfg <- FIG_LAYOUTS[[layout]]
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)

  height_px <- round(width_px / layout_cfg$aspect)
  png(
    filename = path,
    width = width_px,
    height = height_px,
    res = 300,
    bg = bg,
    pointsize = layout_cfg$point_size
  )
  par(
    family = "sans",
    cex = 1.2,
    cex.axis = 1.1,
    cex.lab = 1.15,
    cex.main = 1.2
  )
}

close_png <- function() {
  dev.off()
}

