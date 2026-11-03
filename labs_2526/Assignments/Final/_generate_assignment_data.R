# =============================================================================
# Generate Master Dataset for Final Assignment
# Longitudinal study: Recovery after cardiac surgery
# =============================================================================

library(MASS)
library(dplyr)
library(tidyr)

set.seed(42)

# =============================================================================
# STUDY DESIGN
# =============================================================================
#
# Context: Study of recovery after cardiac bypass surgery
#
# Structure (TWO-LEVEL):
#   - 500 patients (Level 2)
#   - 5 follow-up visits per patient (Level 1): baseline (pre-surgery),
#     2 weeks, 6 weeks, 3 months, 6 months post-surgery
#
# Outcomes (students assigned one based on last 2 digits of student number):
#   1. Quality of Life score (0-100)
#   2. 6-minute walk distance (meters)
#
# Predictors for moderation analysis:
#   - Sex (Male/Female)
#   - Diabetes (Yes/No)
#
# All students examine: Rehabilitation program (Standard/Enhanced)
#
# =============================================================================

# -----------------------------------------------------------------------------
# Parameters
# -----------------------------------------------------------------------------

n_patients <- 500
n_timepoints <- 5
time_values <- c(0, 2, 6, 12, 24)  # weeks

# -----------------------------------------------------------------------------
# Generate patient-level data
# -----------------------------------------------------------------------------

patient_data <- data.frame(
  patient_id = 1:n_patients
) %>%
  mutate(
    # Demographics
    sex = sample(c("Male", "Female"), n_patients, replace = TRUE, prob = c(0.5, 0.5)),

    # Clinical characteristics
    diabetes = sample(c("No", "Yes"), n_patients, replace = TRUE, prob = c(0.5, 0.5)),
    rehab_program = sample(c("Standard", "Enhanced"), n_patients, replace = TRUE, prob = c(0.5, 0.5)),

    # Baseline severity (latent, affects outcomes)
    baseline_severity = rnorm(n_patients, mean = 0, sd = 1)
  )

# -----------------------------------------------------------------------------
# Generate random effects for each patient
# -----------------------------------------------------------------------------

generate_random_effects <- function(n, sd_intercept, sd_slope, cor_int_slope) {
  # Covariance matrix
  cov_matrix <- matrix(c(
    sd_intercept^2, cor_int_slope * sd_intercept * sd_slope,
    cor_int_slope * sd_intercept * sd_slope, sd_slope^2
  ), nrow = 2)

  effects <- mvrnorm(n, mu = c(0, 0), Sigma = cov_matrix)
  colnames(effects) <- c("u_intercept", "u_slope")
  return(as.data.frame(effects))
}

# Patient random effects
patient_effects <- generate_random_effects(
  n = n_patients,
  sd_intercept = 10,  # patient-level intercept SD
  sd_slope = 1.5,     # patient-level slope SD
  cor_int_slope = -0.3  # patients starting lower tend to improve more
)
patient_effects$patient_id <- 1:n_patients

# Merge effects
patient_data <- patient_data %>%
  left_join(patient_effects, by = "patient_id")

# -----------------------------------------------------------------------------
# Generate longitudinal outcome data
# -----------------------------------------------------------------------------

# Expand to long format
long_data <- patient_data %>%
  crossing(time_weeks = time_values)

# -----------------------------------------------------------------------------
# OUTCOME 1: Quality of Life (0-100)
# -----------------------------------------------------------------------------

beta_qol <- list(
  intercept = 45,
  time = 1,                 # base improvement per week
  sex_male = 3,             # males report slightly higher QoL
  diabetes = -5,            # diabetes associated with lower QoL
  enhanced_rehab = 4,       # enhanced rehab improves outcomes
  enhanced_rehab_x_time = 0.2,  # enhanced rehab has greater improvement over time
  severity = -6             # baseline severity effect
)

long_data <- long_data %>%
  mutate(
    qol_fixed = beta_qol$intercept +
      beta_qol$time * time_weeks +
      beta_qol$sex_male * (sex == "Male") +
      beta_qol$diabetes * (diabetes == "Yes") +
      beta_qol$enhanced_rehab * (rehab_program == "Enhanced") +
      beta_qol$enhanced_rehab_x_time * (rehab_program == "Enhanced") * time_weeks +
      beta_qol$severity * baseline_severity,

    qol_random = u_intercept + u_slope * time_weeks,
    qol_error = rnorm(n(), 0, 5),

    quality_of_life = round(pmax(0, pmin(100, qol_fixed + qol_random + qol_error)), 0)
  )

# -----------------------------------------------------------------------------
# OUTCOME 2: 6-minute walk distance (meters)
# -----------------------------------------------------------------------------

beta_walk <- list(
  intercept = 280,
  time = 5,                 # improvement per week
  sex_male = 35,            # males walk further
  diabetes = -30,           # diabetes associated with lower walk distance
  enhanced_rehab = 25,
  enhanced_rehab_x_time = 1,
  severity = -35
)

long_data <- long_data %>%
  mutate(
    walk_fixed = beta_walk$intercept +
      beta_walk$time * time_weeks +
      beta_walk$sex_male * (sex == "Male") +
      beta_walk$diabetes * (diabetes == "Yes") +
      beta_walk$enhanced_rehab * (rehab_program == "Enhanced") +
      beta_walk$enhanced_rehab_x_time * (rehab_program == "Enhanced") * time_weeks +
      beta_walk$severity * baseline_severity,

    walk_random = u_intercept * 3 + u_slope * 1.2 * time_weeks,
    walk_error = rnorm(n(), 0, 30),

    walk_distance = round(pmax(50, walk_fixed + walk_random + walk_error), 0)
  )

# -----------------------------------------------------------------------------
# Create final dataset for students
# -----------------------------------------------------------------------------

cardiac_recovery_data <- long_data %>%
  select(
    # IDs
    patient_id,
    # Time variable
    time_weeks,
    # Patient characteristics
    sex,
    # Clinical variables
    diabetes, rehab_program,
    # Outcomes
    quality_of_life, walk_distance
  ) %>%
  arrange(patient_id, time_weeks)

# Check the data
cat("Dataset dimensions:", nrow(cardiac_recovery_data), "rows,",
    ncol(cardiac_recovery_data), "columns\n")
cat("Number of patients:", n_distinct(cardiac_recovery_data$patient_id), "\n")
cat("Observations per patient:", nrow(cardiac_recovery_data) / n_distinct(cardiac_recovery_data$patient_id), "\n")

# Summary statistics
cat("\n=== Outcome summaries ===\n")
summary(cardiac_recovery_data[, c("quality_of_life", "walk_distance")])

# Save the master dataset
write.csv(cardiac_recovery_data,
          "/home/simalgo/projects/BeyondMLR/Assignments/Final/downloads/cardiac_recovery_master.csv",
          row.names = FALSE)

cat("\nMaster dataset saved!\n")

# =============================================================================
# Generate student subsample function
# =============================================================================

generate_student_data <- function(student_id, master_data) {
  set.seed(student_id)

  # Sample ~60% of patients
  sampled_patients <- sample(unique(master_data$patient_id),
                              size = round(0.6 * length(unique(master_data$patient_id))))

  # Filter to sampled patients
  student_data <- master_data %>%
    filter(patient_id %in% sampled_patients)

  return(student_data)
}

# Test the function
test_student <- generate_student_data(12345, cardiac_recovery_data)
cat("\n=== Test student (ID: 12345) ===\n")
cat("Patients:", n_distinct(test_student$patient_id), "\n")
cat("Observations:", nrow(test_student), "\n")
