# Set seed for reproducibility
set.seed(123)

# Define parameters
n <- 50  # Number of patients per group
means <- c(Control = 200, `Robotic-Assisted Therapy` = 280, `Aquatic Therapy` = 240)
sd <- 30  # Increased standard deviation for variability in improvement

# Generate data for each group
Control <- rnorm(n, mean = means["Control"], sd = sd)
Robotic <- rnorm(n, mean = means["Robotic-Assisted Therapy"], sd = sd)
Aquatic <- rnorm(n, mean = means["Aquatic Therapy"], sd = sd)

# Combine into a data frame
data_mobility <- data.frame(
  Treatment = factor(rep(c("Control", "Robotic-Assisted Therapy", "Aquatic Therapy"), each = n),
                     levels = c("Control", "Robotic-Assisted Therapy", "Aquatic Therapy")),
  Mobility_Improvement = c(Control, Robotic, Aquatic)
)

data_mobility$Mobility_Improvement <- round(data_mobility$Mobility_Improvement)

# Display the first few rows of the dataset
head(data_mobility)

# write datast to CSV file 
write.csv(data_mobility, "data_mobility.csv", row.names=FALSE)
