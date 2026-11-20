library(lme4)
library(ggplot2)
library(jsonlite)
source("slides-common/R/common_fig.R")
assets <- "lectures_2627/week3/assets"
airbnb <- read.csv("labs_2627/data/airbnb.csv")
airbnb <- subset(airbnb, room_type == "Entire home/apt")
airbnb$log_price <- log(airbnb$price)
airbnb$capacity_c <- airbnb$accommodates - 4
airbnb$walk10 <- (airbnb$WalkScore - 80) / 10
areas <- unique(airbnb[c("district", "neighborhood")])
stopifnot(!anyDuplicated(areas$neighborhood))
district_for <- setNames(areas$district, areas$neighborhood)
stopifnot(district_for["Humboldt Park"] == "West", district_for["West Town"] == "West",
          district_for["Rogers Park"] == "Far North")

# The y-axis order follows the mean of the plotted neighborhood means.
means <- aggregate(log_price ~ district + neighborhood, data = airbnb, FUN = mean)
means$listings <- as.integer(table(airbnb$neighborhood)[means$neighborhood])
means$district <- reorder(means$district, means$log_price, mean)
eda <- ggplot(means, aes(log_price, district)) +
  geom_point(aes(size = listings), shape = 21, fill = color_secondary,
             colour = color_primary, alpha = 0.8, stroke = 0.7,
             position = position_jitter(width = 0, height = 0.11, seed = 3)) +
  scale_size_area(name = "Homes per\nneighborhood", max_size = 10, breaks = c(10, 50, 150)) +
  labs(x = "Mean log nightly price in each neighborhood", y = NULL) +
  theme_minimal(base_size = 18, base_family = "Liberation Sans") +
  theme(panel.grid.minor = element_blank(), axis.text = element_text(colour = color_text),
        legend.position = "right", legend.title = element_text(size = 15),
        axis.title.x = element_text(margin = margin(t = 8)))
ggsave(file.path(assets, "nested_airbnb_eda.png"), eda, width = 11, height = 4.55, dpi = 170, bg = "white")

# Give each neighborhood one observation when comparing district WalkScores.
walk <- unique(airbnb[c("district", "neighborhood", "WalkScore")])
stopifnot(!anyDuplicated(walk$neighborhood))
district_walk <- aggregate(WalkScore ~ district, walk, mean)
district_walk <- district_walk[order(district_walk$WalkScore, district_walk$district), ]
district_walk$row <- seq_len(nrow(district_walk))
walk$row <- district_walk$row[match(walk$district, district_walk$district)]
# Separate ties vertically so their observed scores remain visible.
walk$offset <- ave(walk$row, walk$row, walk$WalkScore,
                   FUN = function(x) (seq_along(x) - mean(seq_along(x))) * 0.26)
walk_plot <- ggplot(walk, aes(WalkScore, row)) +
  geom_line(aes(y = row + 0.13, group = district), colour = color_grid, linewidth = 0.8) +
  geom_point(aes(y = row + 0.13 + offset, shape = "Neighborhood"),
             colour = color_pvalue, size = 2.8) +
  geom_point(data = district_walk, aes(y = row - 0.23, shape = "District mean"),
             colour = "#AB4F24", size = 3.4) +
  scale_shape_manual(name = NULL, values = c("Neighborhood" = 16, "District mean" = 18),
                     breaks = c("Neighborhood", "District mean")) +
  scale_x_continuous(breaks = seq(50, 95, 5), limits = c(46, 97), expand = c(0, 0)) +
  scale_y_continuous(breaks = district_walk$row, labels = district_walk$district,
                     expand = expansion(add = 0.35)) +
  labs(x = "Neighborhood WalkScore", y = NULL) +
  theme_minimal(base_size = 17, base_family = "Liberation Sans") +
  theme(legend.position = "top", legend.justification = "left",
        legend.margin = margin(0, 0, 0, 0), legend.box.spacing = unit(0, "pt"),
        panel.grid.major.y = element_blank(), panel.grid.minor = element_blank(),
        axis.text = element_text(colour = color_text))
ggsave(file.path(assets, "nested_airbnb_walkscores.png"), walk_plot,
       width = 11.5, height = 4.65, dpi = 170, bg = "white")

# A schematic of four illustrative listings, reused for the pairwise discussion.
nodes <- data.frame(
  id = c("West", "Far North", "Humboldt Park", "West Town", "Rogers Park", "A", "B", "C", "D"),
  x = c(3.3, 8.1, 2.0, 4.9, 8.1, 1.2, 2.8, 4.9, 8.1),
  y = rep(c(3, 2, 1), c(2, 3, 4)))
edges <- data.frame(parent = c("West", "West", "Far North", "Humboldt Park", "Humboldt Park", "West Town", "Rogers Park"),
                    child = c("Humboldt Park", "West Town", "Rogers Park", "A", "B", "C", "D"))
parent <- nodes[match(edges$parent, nodes$id), ]
child <- nodes[match(edges$child, nodes$id), ]
diagram <- ggplot(nodes, aes(x, y)) +
  annotate("segment", x = parent$x, y = parent$y, xend = child$x, yend = child$y,
           colour = color_muted, linewidth = 0.9) +
  geom_label(aes(label = id, fill = factor(y)), size = 6.9, fontface = "bold", colour = color_text,
             label.padding = unit(0.45, "lines"), linewidth = 0.45, show.legend = FALSE) +
  scale_fill_manual(values = c("1" = "white", "2" = color_panel_cool, "3" = color_accent_light)) +
  annotate("text", x = -0.25, y = 3:1, label = c("District", "Neighborhood", "Listing"),
           hjust = 1, size = 6, colour = color_muted) +
  coord_cartesian(xlim = c(-2.25, 9.25), ylim = c(0.55, 3.45), expand = FALSE) + theme_void()
ggsave(file.path(assets, "nested_airbnb_hierarchy.png"), diagram, width = 11, height = 3.8, dpi = 170, bg = "white")

two <- lmer(log_price ~ 1 + (1 | neighborhood), airbnb)
three <- lmer(log_price ~ 1 + (1 | district/neighborhood), airbnb)
two_adjusted <- update(two, . ~ . + capacity_c + walk10)
three_listing <- update(three, . ~ . + capacity_c)
three_adjusted <- update(three_listing, . ~ . + walk10)
stopifnot(length(unique(vapply(list(two, three, three_listing, two_adjusted, three_adjusted), nobs, integer(1)))) == 1)
variance <- function(model) {
  z <- as.data.frame(VarCorr(model))
  as.list(setNames(z$vcov, z$grp))
}
coef_table <- function(model) {
  z <- coef(summary(model))
  z <- z[rownames(z) != "(Intercept)", , drop = FALSE]
  data.frame(term = rownames(z), estimate = z[, "Estimate"], se = z[, "Std. Error"], row.names = NULL)
}
write_json(list(listings = nrow(airbnb), neighborhoods = nrow(areas), districts = length(unique(areas$district)),
                two = variance(two), three = variance(three),
                three_listing = variance(three_listing), three_adjusted = variance(three_adjusted),
                two_fixed = coef_table(two_adjusted), three_fixed = coef_table(three_adjusted)),
           file.path(assets, "nested_airbnb_results.json"), auto_unbox = TRUE, pretty = TRUE, digits = NA)

# Typeset the model and the two shared-variance calculations for projection.
v <- variance(three)
district <- v$district
area <- v[["neighborhood:district"]]
total <- sum(unlist(v))
equations <- c(
  model = r"(L_{ijk}=\beta_0+u_k+v_{jk}+\varepsilon_{ijk})",
  distributions = r"(\begin{aligned}u_k &\sim \mathcal{N}(0,\sigma^2_{\mathrm{district}})\\[3pt] v_{jk} &\sim \mathcal{N}(0,\sigma^2_{\mathrm{neighborhood}})\\[3pt] \varepsilon_{ijk} &\sim \mathcal{N}(0,\sigma^2_{\mathrm{listing}})\end{aligned})",
  same_neighborhood = sprintf(r"(\frac{%.4f+%.4f}{%.4f}=%.3f)", district, area, total, (district + area) / total),
  same_district = sprintf(r"(\frac{%.4f}{%.4f}=%.3f)", district, total, district / total)
)
for (name in names(equations)) {
  stem <- file.path(tempdir(), name)
  writeLines(c(r"(\documentclass[border=2pt]{standalone})", r"(\usepackage{amsmath})",
               r"(\begin{document})", paste0("$\\displaystyle ", equations[[name]], "$"),
               r"(\end{document})"), paste0(stem, ".tex"))
  stopifnot(system2("pdflatex", c("-interaction=batchmode", "-halt-on-error",
                                  paste0("-output-directory=", tempdir()), paste0(stem, ".tex")),
                    stdout = FALSE) == 0)
  stopifnot(system2("pdftocairo", c("-png", "-transp", "-singlefile", "-r", "600",
                                    paste0(stem, ".pdf"), file.path(assets, paste0("nested_airbnb_eq_", name)))) == 0)
}
