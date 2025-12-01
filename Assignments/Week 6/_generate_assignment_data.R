# =============================================================================
# Generate Master Dataset for Week 6 Assignment
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
# Outcomes (students will be assigned one):
#   1. Quality of Life score (0-100)
#   2. 6-minute walk distance (meters)
#   3. Depression score (PHQ-9, 0-27)
#
# Patient-level predictors (Level 2):
#   - Age (centered at 65)
#   - Sex (Male/Female)
#   - Diabetes (Yes/No)
#   - Surgery type (CABG only / CABG + valve)
#   - Rehab program (Standard / Enhanced)
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
    age = round(rnorm(n_patients, mean = 65, sd = 10)),
    age_c = age - 65,  # centered at 65
    sex = sample(c("Male", "Female"), n_patients, replace = TRUE, prob = c(0.65, 0.35)),

    # Clinical characteristics
    diabetes = sample(c("No", "Yes"), n_patients, replace = TRUE, prob = c(0.7, 0.3)),
    surgery_type = sample(c("CABG", "CABG+Valve"), n_patients, replace = TRUE, prob = c(0.75, 0.25)),
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
  crossing(time_weeks = time_values) %>%
  mutate(
    time_months = time_weeks / 4,
    visit = case_when(
      time_weeks == 0 ~ "Baseline",
      time_weeks == 2 ~ "2 weeks",
      time_weeks == 6 ~ "6 weeks",
      time_weeks == 12 ~ "3 months",
      time_weeks == 24 ~ "6 months"
    ),
    visit = factor(visit, levels = c("Baseline", "2 weeks", "6 weeks", "3 months", "6 months"))
  )

# -----------------------------------------------------------------------------
# OUTCOME 1: Quality of Life (0-100)
# -----------------------------------------------------------------------------
# Recovery pattern: starts around 45, improves to ~70-75 over 6 months

beta_qol <- list(
  intercept = 45,
  time = 4,                 # base improvement per month
  age = -0.2,               # older patients have slightly lower QoL
  sex_male = 3,             # males report slightly higher QoL
  diabetes = -5,            # diabetes associated with lower QoL
  surgery_complex = -4,     # complex surgery = worse initial QoL
  enhanced_rehab = 4,       # enhanced rehab improves outcomes
  enhanced_rehab_x_time = 0.8,  # enhanced rehab has greater improvement over time
  severity = -6             # baseline severity effect
)

long_data <- long_data %>%
  mutate(
    qol_fixed = beta_qol$intercept +
      beta_qol$time * time_months +
      beta_qol$age * age_c +
      beta_qol$sex_male * (sex == "Male") +
      beta_qol$diabetes * (diabetes == "Yes") +
      beta_qol$surgery_complex * (surgery_type == "CABG+Valve") +
      beta_qol$enhanced_rehab * (rehab_program == "Enhanced") +
      beta_qol$enhanced_rehab_x_time * (rehab_program == "Enhanced") * time_months +
      beta_qol$severity * baseline_severity,

    qol_random = u_intercept + u_slope * time_months,
    qol_error = rnorm(n(), 0, 5),

    quality_of_life = round(pmax(0, pmin(100, qol_fixed + qol_random + qol_error)), 0)
  )

# -----------------------------------------------------------------------------
# OUTCOME 2: 6-minute walk distance (meters)
# -----------------------------------------------------------------------------

beta_walk <- list(
  intercept = 280,
  time = 20,                # improvement per month
  age = -3,                 # older patients walk less
  sex_male = 35,            # males walk further
  diabetes = -30,           # diabetes associated with lower walk distance
  surgery_complex = -25,
  enhanced_rehab = 25,
  enhanced_rehab_x_time = 4,
  severity = -35
)

long_data <- long_data %>%
  mutate(
    walk_fixed = beta_walk$intercept +
      beta_walk$time * time_months +
      beta_walk$age * age_c +
      beta_walk$sex_male * (sex == "Male") +
      beta_walk$diabetes * (diabetes == "Yes") +
      beta_walk$surgery_complex * (surgery_type == "CABG+Valve") +
      beta_walk$enhanced_rehab * (rehab_program == "Enhanced") +
      beta_walk$enhanced_rehab_x_time * (rehab_program == "Enhanced") * time_months +
      beta_walk$severity * baseline_severity,

    walk_random = u_intercept * 3 + u_slope * 5 * time_months,
    walk_error = rnorm(n(), 0, 30),

    walk_distance = round(pmax(50, walk_fixed + walk_random + walk_error), 0)
  )

# -----------------------------------------------------------------------------
# OUTCOME 3: Depression score (PHQ-9, 0-27)
# -----------------------------------------------------------------------------

beta_dep <- list(
  intercept = 12,
  time = -0.8,              # improvement (decrease) per month
  age = 0.03,               # slight increase with age
  sex_male = -2,            # males report less depression
  diabetes = 2,
  surgery_complex = 1.5,
  enhanced_rehab = -2,
  enhanced_rehab_x_time = -0.3,
  severity = 2.5
)

long_data <- long_data %>%
  mutate(
    dep_fixed = beta_dep$intercept +
      beta_dep$time * time_months +
      beta_dep$age * age_c +
      beta_dep$sex_male * (sex == "Male") +
      beta_dep$diabetes * (diabetes == "Yes") +
      beta_dep$surgery_complex * (surgery_type == "CABG+Valve") +
      beta_dep$enhanced_rehab * (rehab_program == "Enhanced") +
      beta_dep$enhanced_rehab_x_time * (rehab_program == "Enhanced") * time_months +
      beta_dep$severity * baseline_severity,

    dep_random = u_intercept * 0.4 + u_slope * 0.3 * time_months,
    dep_error = rnorm(n(), 0, 2),

    depression_score = round(pmax(0, pmin(27, dep_fixed + dep_random + dep_error)), 0)
  )

# -----------------------------------------------------------------------------
# Create final dataset for students
# -----------------------------------------------------------------------------

cardiac_recovery_data <- long_data %>%
  select(
    # IDs
    patient_id,
    # Time variables
    visit, time_weeks, time_months,
    # Patient characteristics
    age, sex,
    # Clinical variables
    diabetes, surgery_type, rehab_program,
    # Outcomes
    quality_of_life, walk_distance, depression_score
  ) %>%
  arrange(patient_id, time_weeks)

# Check the data
cat("Dataset dimensions:", nrow(cardiac_recovery_data), "rows,",
    ncol(cardiac_recovery_data), "columns\n")
cat("Number of patients:", n_distinct(cardiac_recovery_data$patient_id), "\n")
cat("Observations per patient:", nrow(cardiac_recovery_data) / n_distinct(cardiac_recovery_data$patient_id), "\n")

# Summary statistics
cat("\n=== Outcome summaries ===\n")
summary(cardiac_recovery_data[, c("quality_of_life", "walk_distance", "depression_score")])

# Save the master dataset
write.csv(cardiac_recovery_data,
          "/home/simalgo/projects/BeyondMLR/Assignments/Week 6/downloads/cardiac_recovery_master.csv",
          row.names = FALSE)

cat("\nMaster dataset saved!\n")

# =============================================================================
# LOOKUP TABLE: Research questions by student ID
# =============================================================================

outcomes <- c("quality_of_life", "walk_distance", "depression_score")
outcome_labels <- c("Quality of Life", "6-minute walk distance", "Depression (PHQ-9)")

predictors <- c("sex", "diabetes", "surgery_type")
predictor_labels <- c("Sex", "Diabetes status", "Surgery complexity")

# Generate lookup table
lookup_table <- expand.grid(
  outcome_idx = 1:3,
  predictor_idx = 1:3
) %>%
  mutate(
    question_id = row_number(),
    outcome = outcomes[outcome_idx],
    outcome_label = outcome_labels[outcome_idx],
    main_predictor = predictors[predictor_idx],
    predictor_label = predictor_labels[predictor_idx]
  ) %>%
  select(question_id, outcome, outcome_label, main_predictor, predictor_label)

# Add research questions as text
lookup_table <- lookup_table %>%
  mutate(
    research_question = paste0(
      "Investigate how ", tolower(predictor_label),
      " affects ", tolower(outcome_label), " recovery trajectories in cardiac surgery patients. ",
      "Additionally, examine whether the rehabilitation program (Standard vs Enhanced) ",
      "moderates the recovery pattern."
    ),
    hypothesis = case_when(
      outcome == "quality_of_life" & main_predictor == "sex" ~
        "Males may report higher QoL. Enhanced rehab may accelerate recovery.",
      outcome == "quality_of_life" & main_predictor == "diabetes" ~
        "Diabetes associated with lower QoL. Enhanced rehab may help diabetic patients more.",
      outcome == "quality_of_life" & main_predictor == "surgery_type" ~
        "Complex surgery associated with slower recovery. Enhanced rehab may improve outcomes.",
      outcome == "walk_distance" & main_predictor == "sex" ~
        "Males likely walk further. Enhanced rehab may accelerate improvement.",
      outcome == "walk_distance" & main_predictor == "diabetes" ~
        "Diabetes associated with shorter walk distance. Enhanced rehab may help.",
      outcome == "walk_distance" & main_predictor == "surgery_type" ~
        "Complex surgery may impair physical recovery. Enhanced rehab may help.",
      outcome == "depression_score" & main_predictor == "sex" ~
        "Females may report higher depression. Enhanced rehab may improve mental health.",
      outcome == "depression_score" & main_predictor == "diabetes" ~
        "Comorbid diabetes may increase depression. Enhanced rehab may provide support.",
      outcome == "depression_score" & main_predictor == "surgery_type" ~
        "Complex surgery may increase depression. Enhanced rehab may help.",
      TRUE ~ "Examine main effects and potential interactions."
    )
  )

print(lookup_table)

# Save lookup table
write.csv(lookup_table,
          "/home/simalgo/projects/BeyondMLR/Assignments/Week 6/downloads/research_questions_lookup.csv",
          row.names = FALSE)

cat("\nLookup table saved! (9 unique research questions)\n")

# =============================================================================
# FUNCTION: Generate student subsample
# =============================================================================

generate_student_data <- function(student_id, master_data, lookup_table) {
  set.seed(student_id)

  # Determine research question (cycle through 9 questions)
  question_id <- ((student_id - 1) %% nrow(lookup_table)) + 1
  question <- lookup_table[question_id, ]

  # Sample ~60% of patients
  sampled_patients <- sample(unique(master_data$patient_id),
                              size = round(0.6 * length(unique(master_data$patient_id))))

  # Filter to sampled patients
  student_data <- master_data %>%
    filter(patient_id %in% sampled_patients)

  # Introduce some missing data (5% MCAR)
  n_missing <- round(nrow(student_data) * 0.05)
  missing_idx <- sample(1:nrow(student_data), n_missing)
  outcome_col <- question$outcome
  student_data[missing_idx, outcome_col] <- NA

  return(list(
    data = student_data,
    question = question,
    n_patients = n_distinct(student_data$patient_id),
    n_observations = nrow(student_data)
  ))
}

# Test the function
test_student <- generate_student_data(12345, cardiac_recovery_data, lookup_table)
cat("\n=== Test student (ID: 12345) ===\n")
cat("Patients:", test_student$n_patients, "\n")
cat("Observations:", test_student$n_observations, "\n")
cat("Outcome:", test_student$question$outcome_label, "\n")
cat("Main predictor:", test_student$question$predictor_label, "\n")
cat("\nResearch question:\n", test_student$question$research_question, "\n")
