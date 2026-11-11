# Lecture debrief of Lab 3. Read the student dataset without regenerating it.
# Run from any directory: Rscript /path/to/week2/generate_lab3_debrief.R
library(jsonlite)
script_arg <- grep("^--file=", commandArgs(FALSE), value = TRUE)
script_dir <- dirname(normalizePath(sub("^--file=", "", script_arg)))
data_path <- file.path(script_dir, "../../labs_2627/data/wound_healing_week2.csv")
output_dir <- file.path(script_dir, "assets/lab3-debrief")
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
wound <- read.csv(data_path)
wound$treatment <- factor(wound$treatment, levels = c("Standard", "Experimental"))
wound$laboratory <- factor(wound$laboratory, levels = paste("Laboratory", LETTERS[1:5]))
contrasts(wound$treatment) <- contr.treatment(levels(wound$treatment))
contrasts(wound$laboratory) <- contr.sum(levels(wound$laboratory))
additive <- lm(closure ~ treatment + laboratory, data = wound)
interaction <- lm(closure ~ treatment * laboratory, data = wound)
model_result <- function(model) {
  term <- "treatmentExperimental"
  ci <- confint(model, term)
  list(estimate = unname(coef(model)[term]),
       standard_error = unname(coef(summary(model))[term, "Std. Error"]),
       lower_95 = unname(ci[1]), upper_95 = unname(ci[2]),
       p = unname(coef(summary(model))[term, "Pr(>|t|)"]),
       residual_sd = sigma(model), residual_df = df.residual(model),
       residual_sum_squares = deviance(model))
}
cell_means <- tapply(wound$closure, list(wound$laboratory, wound$treatment), mean)
differences <- cell_means[, "Experimental"] - cell_means[, "Standard"]
# Independently verify the average coefficient and its pooled-variance SE.
stopifnot(nrow(wound) == 100L, all(table(wound$laboratory, wound$treatment) == 10L),
          abs(coef(additive)["treatmentExperimental"] - mean(differences)) < 1e-10,
          abs(coef(interaction)["treatmentExperimental"] - mean(differences)) < 1e-10)
for (model in list(additive, interaction)) {
  stopifnot(abs(model_result(model)$standard_error - sigma(model) * sqrt(2 / 50)) < 1e-10)
}
results <- list(
  dataset = "labs_2627/data/wound_healing_week2.csv",
  dataset_md5 = unname(tools::md5sum(data_path)),
  laboratories = 5L, plates_per_treatment_per_laboratory = 10L,
  outcome = "Percentage of original wound area closed after 48 hours",
  contrast = "Experimental minus standard, in percentage points",
  coding = "Dummy coding for treatment; sum-to-zero effects coding for laboratory",
  additive_model = model_result(additive), interaction_model = model_result(interaction),
  inference = "Usual model-based 95% t intervals and two-sided p-values for zero average effect; no multiplicity adjustment",
  unequal_size_example = list(
    description = "Hypothetical two-laboratory illustration, not new estimates from the Lab 3 dataset; half the plates receive each treatment within each laboratory",
    plates = c(20L, 80L), effects = c(5, 15),
    interaction_average = mean(c(5, 15)),
    additive_average = weighted.mean(c(5, 15), c(20, 80))
  )
)
write_json(results, file.path(output_dir, "model_results.json"),
           pretty = TRUE, auto_unbox = TRUE, digits = 12)
print(rbind(additive = unlist(results$additive_model),
            interaction = unlist(results$interaction_model)))
