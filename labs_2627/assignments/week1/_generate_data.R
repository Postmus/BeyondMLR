# Reproduce the simulated Week 1 assignment data using base R.
# Run with: Rscript labs_2627/assignments/week1/_generate_data.R
# The output location is resolved relative to this script.

script_argument <- commandArgs(trailingOnly = FALSE)
script_argument <- script_argument[grepl("^--file=", script_argument)]
if (length(script_argument) != 1L) {
  stop("Run this script using Rscript.")
}
script_directory <- dirname(normalizePath(sub("^--file=", "", script_argument)))
output_directory <- file.path(script_directory, "downloads")
dir.create(output_directory, recursive = TRUE, showWarnings = FALSE)

set.seed(262701)

donor_levels <- sprintf("Donor %02d", 1:10)
protocol_levels <- c("Standard", "Modified")
aliquots_per_protocol <- 4L

# Protein yield is expressed in micrograms of protein per milligram of tissue.
# Donor contributions are shared by all aliquots from a donor. The protocol
# difference is common to donors, and individual aliquot errors are independent.
baseline_yield <- 60
protocol_difference <- 6
donor_standard_deviation <- 12
aliquot_standard_deviation <- 8

donor_contributions <- rnorm(length(donor_levels), 0, donor_standard_deviation)

protein_data <- do.call(rbind, lapply(seq_along(donor_levels), function(j) {
  # Assign four of the eight aliquots to each protocol at random.
  protocol <- sample(rep(protocol_levels, each = aliquots_per_protocol))
  data.frame(
    aliquot = sprintf("D%02d-A%02d", j, seq_along(protocol)),
    donor = donor_levels[j],
    protocol = protocol,
    protein_yield = round(
      baseline_yield + donor_contributions[j] +
        protocol_difference * (protocol == "Modified") +
        rnorm(length(protocol), 0, aliquot_standard_deviation),
      digits = 2
    )
  )
}))

stopifnot(
  nrow(protein_data) == 80L,
  length(unique(protein_data$aliquot)) == 80L,
  length(unique(protein_data$donor)) == 10L,
  all(table(protein_data$donor, protein_data$protocol) == 4L),
  all(is.finite(protein_data$protein_yield)),
  all(protein_data$protein_yield > 0)
)

write.csv(protein_data, file.path(output_directory, "protein_yield.csv"),
          row.names = FALSE)
cat("Generated 80 aliquot measurements from 10 donors.\n")
