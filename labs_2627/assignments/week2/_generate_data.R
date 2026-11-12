# Reproduce the simulated Week 2 assignment data using base R.
# Run with: Rscript labs_2627/assignments/week2/_generate_data.R

script_argument <- commandArgs(trailingOnly = FALSE)
script_argument <- script_argument[grepl("^--file=", script_argument)]
if (length(script_argument) != 1L) stop("Run this script using Rscript.")
script_directory <- dirname(normalizePath(sub("^--file=", "", script_argument)))
output_directory <- file.path(script_directory, "downloads")
dir.create(output_directory, recursive = TRUE, showWarnings = FALSE)

set.seed(262702)

hospital_levels <- sprintf("Hospital %02d", 1:12)
protocol_levels <- c("Standard", "Enhanced")
patients_per_protocol <- 15L

# Continuous pain scores on a 0-10 scale. Draw each hospital's intercept and
# treatment-effect deviation jointly; draw independent patient errors.
baseline_pain <- 5.3
protocol_difference <- -1.1
intercept_sd <- 0.65
slope_sd <- 0.8
intercept_slope_correlation <- -0.45
residual_sd <- 1

z_intercept <- rnorm(length(hospital_levels))
z_slope <- rnorm(length(hospital_levels))
hospital_intercepts <- intercept_sd * z_intercept
hospital_slopes <- slope_sd * (
  intercept_slope_correlation * z_intercept +
    sqrt(1 - intercept_slope_correlation^2) * z_slope
)

pain_data <- do.call(rbind, lapply(seq_along(hospital_levels), function(j) {
  protocol <- sample(rep(protocol_levels, each = patients_per_protocol))
  data.frame(
    patient_id = sprintf("H%02d-P%02d", j, seq_along(protocol)),
    hospital = hospital_levels[j],
    protocol = protocol,
    pain_score = round(
      baseline_pain + hospital_intercepts[j] +
        (protocol_difference + hospital_slopes[j]) * (protocol == "Enhanced") +
        rnorm(length(protocol), 0, residual_sd),
      digits = 1
    )
  )
}))

# The fixed seed yields scores within the scale through the chosen parameters.
# Preserve the simulated errors directly, without clipping or resampling scores.
stopifnot(
  nrow(pain_data) == 360L,
  length(unique(pain_data$patient_id)) == 360L,
  all(table(pain_data$hospital, pain_data$protocol) == 15L),
  all(is.finite(pain_data$pain_score)),
  all(pain_data$pain_score >= 0 & pain_data$pain_score <= 10)
)

write.csv(pain_data, file.path(output_directory, "postoperative_pain.csv"),
          row.names = FALSE)
cat("Generated 360 patient measurements from 12 hospitals.\n")
