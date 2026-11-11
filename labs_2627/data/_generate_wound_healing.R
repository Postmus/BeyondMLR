# Reproducible teaching data for the fixed treatment-by-laboratory interaction lab.
# Run with Rscript from any directory. The student file contains observations only.
# Design: five fixed laboratories, two treatments, ten independent plates per cell.
# Outcome: percentage of the original wound area closed after 48 hours.
# The lab follows the Week 1 guided-script format and the old factorial lab's
# sequence: exploration, interaction model, nested F-test, simple effects,
# and residual diagnostics. The exploratory plot shows cell means only.
# Teaching parameters below define heterogeneous effects; they are not estimates
# from a real experiment. The seed was fixed before examining the model results.
script <- sub("^--file=", "", commandArgs(FALSE)[grepl("^--file=", commandArgs(FALSE))])
output_dir <- dirname(normalizePath(script))
set.seed(20261111)
laboratories <- paste("Laboratory", LETTERS[1:5])
standard_mean <- c(44, 52, 47, 58, 49)
treatment_effect <- c(2, 8, 16, -3, 22)
individual_sd <- 8

wound <- do.call(rbind, lapply(seq_along(laboratories), function(j) {
  treatment <- sample(rep(c("Standard", "Experimental"), each = 10))
  expected <- standard_mean[j] + treatment_effect[j] * (treatment == "Experimental")
  data.frame(
    plate_id = sprintf("%s%02d", LETTERS[j], 1:20),
    laboratory = laboratories[j], treatment = treatment,
    closure = round(rnorm(20, expected, individual_sd), 1)
  )
}))
stopifnot(nrow(wound) == 100, !anyDuplicated(wound$plate_id),
          all(table(wound$laboratory, wound$treatment) == 10),
          all(wound$closure > 0 & wound$closure < 100))
# No clipping, rejection sampling, or selection based on statistical significance.
write.csv(wound, file.path(output_dir, "wound_healing_week2.csv"), row.names = FALSE)
cat("Wrote 100 independent plate observations, 10 per treatment in each of five laboratories.\n")
