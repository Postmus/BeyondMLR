# Instructor asset generation only. Lab 4 displays the Lab 3 plot as background.
suppressPackageStartupMessages({ library(ggplot2); library(dplyr) })
script <- sub("^--file=", "", commandArgs(FALSE)[grepl("^--file=", commandArgs(FALSE))])
here <- dirname(normalizePath(script))
wound <- read.csv(file.path(here, "../../data/wound_healing_week2.csv"))
wound$treatment <- factor(wound$treatment, levels = c("Standard", "Experimental"))
wound$laboratory <- factor(wound$laboratory, levels = paste("Laboratory", LETTERS[1:5]))
laboratory_summary <- wound |>
  group_by(laboratory, treatment) |>
  summarise(n = n(), mean_closure = mean(closure), sd_closure = sd(closure), .groups = "drop")
p <- ggplot(laboratory_summary,
            aes(laboratory, mean_closure, color = treatment, group = treatment)) +
  geom_line(linewidth = .9) + geom_point(size = 3) +
  scale_color_manual(values = c(Standard = "#173F48", Experimental = "#D58A3D")) +
  labs(x = "Laboratory", y = "Mean wound closure (%)", color = "Treatment") +
  theme_minimal(base_size = 13) + theme(legend.position = "top")
ggsave(file.path(here, "wound-healing-interaction.png"), p,
       width = 8, height = 5, dpi = 240, bg = "white")
