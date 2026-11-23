# Reproduce the 1987 ILEA cohort with base R.
# Run: Rscript labs_2627/assignments/week3/_prepare_data.R [path/to/ilea567.zip]
# Source: Inner London Education Authority, distributed by the University of
# Bristol Centre for Multilevel Modelling. ILEA567.ASC documents the fields.
source_url <- "https://www.bristol.ac.uk/cmm/media/migrated/ilea567.zip"
args <- commandArgs(trailingOnly = TRUE)
if (length(args) > 1L) stop("Supply at most one path to the Bristol archive.")
if (length(args) == 1L) {
  archive <- args[1]
} else {
  archive <- tempfile(fileext = ".zip")
  download.file(source_url, archive, mode = "wb")
}

records <- readLines(unz(archive, "ILEA567.DAT"), warn = FALSE)
# Column 1 codes the examination year: 3 denotes 1987.
records <- records[startsWith(records, "3")]
fields <- c("YearCode", "School", "ExamScore", "PercentFSM", "PercentVR1",
            "Gender", "VRBand", "EthnicGroup", "SchoolGender", "SchoolDenomination")
cohort <- read.fwf(textConnection(records), widths = c(1, 3, 2, 2, 2, 1, 1, 2, 1, 1),
                   col.names = fields)

# Keep the six teaching variables and code verbal reasoning from lower to higher.
cohort$Gender <- factor(cohort$Gender, levels = 0:1, labels = c("Male", "Female"))
# Source codes 1, 3 and 2 denote VR3 (lower), VR2 (middle) and VR1 (higher).
# Exclude source code 0 (unrecorded band) before coding the middle band as 0.
stopifnot(all(cohort$VRBand %in% 0:3), sum(cohort$VRBand == 0L) == 12L)
cohort <- subset(cohort, VRBand != 0L)
cohort$VRBand <- match(cohort$VRBand, c(1L, 3L, 2L)) - 2L
cohort$SchoolDenomination <- factor(cohort$SchoolDenomination, levels = 1:3,
                                    labels = c("Maintained", "Church of England", "Roman Catholic"))
students <- cohort[c("School", "ExamScore", "PercentFSM", "Gender", "VRBand", "SchoolDenomination")]
stopifnot(nrow(students) == 5921L, length(unique(students$School)) == 114L,
          nrow(unique(students[c("School", "PercentFSM")])) == 114L)

script_file <- sub("^--file=", "", grep("^--file=", commandArgs(), value = TRUE))
downloads <- file.path(dirname(normalizePath(script_file)), "downloads")
dir.create(downloads, recursive = TRUE, showWarnings = FALSE)
write.csv(students, file.path(downloads, "ilea_1987.csv"), row.names = FALSE)
