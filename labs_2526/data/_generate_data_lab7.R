# Simulating individual-level asthma exacerbation data
set.seed(123)  # For reproducibility

# Number of individuals
n <- 500

# Generate individual-level predictors
age <- rnorm(n, mean = 50, sd = 15)  # Age in years
smoking_status <- rbinom(n, size = 1, prob = 0.3)  # 30% smokers (1 = smoker, 0 = non-smoker)

# Define model coefficients
beta_0 <- -1  # Intercept
beta_age <- 0.02  # Effect of age
beta_smoking <- 0.5  # Effect of smoking status

# Calculate Poisson mean (lambda) for each individual
eta <- beta_0 + beta_age * age + beta_smoking * smoking_status
lambda <- exp(eta)

# Simulate number of asthma exacerbations using the Poisson distribution
asthma_data <- data.frame(
  ID = 1:n,
  Age = round(age, 0),
  Smoking_Status = smoking_status,
  Exacerbations = rpois(n, lambda = lambda)
)

# Preview the simulated data
head(asthma_data)

write.csv(asthma_data, "asthma_data.csv", row.names=FALSE)