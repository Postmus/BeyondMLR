# Reproduce the allocation-imbalance comparison used in the lecture opening.
# Run from the repository root: Rscript lectures_2627/week2/explore_allocation_imbalance.R
library(nlme)
script_path <- sub('^--file=', '', commandArgs()[grepl('^--file=', commandArgs())][1])
script_dir <- dirname(normalizePath(script_path))

centers <- paste('Center', LETTERS[1:6])
generation_centers <- centers[c(4, 1, 6, 3, 2, 5)]
center_effects <- setNames(c(-29, -17, -5, 8, 16, 27), generation_centers)
n_standard <- setNames(c(5, 8, 10, 10, 12, 15), centers)

generate <- function(standard_counts) {
  # Replay Week 1's seed, center order and randomization. Both allocations
  # use the same random permutations and individual errors: only the number
  # assigned to each treatment changes. Regenerate outcomes accordingly.
  set.seed(158)
  dat <- do.call(rbind, lapply(generation_centers, function(center) {
    n0 <- standard_counts[[center]]
    treatment <- sample(c(rep('Standard', n0), rep('Enhanced', 20 - n0)))
    error <- rnorm(20, 0, 24)
    data.frame(center, treatment, individual_error = error,
      distance = round(360 + center_effects[[center]] +
        10 * (treatment == 'Enhanced') + error, 1))
  }))
  dat$center <- factor(dat$center, levels = centers)
  dat$treatment <- factor(dat$treatment, levels = c('Standard', 'Enhanced'))
  contrasts(dat$center) <- contr.sum(length(centers))
  dat
}

balanced <- generate(setNames(rep(10, 6), centers))
unbalanced <- generate(n_standard)
original <- read.csv(file.path(script_dir, '../week1/assets/rehabilitation_data.csv'))
stopifnot(identical(as.character(balanced$center), original$center),
          identical(as.character(balanced$treatment), original$treatment),
          isTRUE(all.equal(balanced$distance, original$distance)),
          identical(balanced$individual_error, unbalanced$individual_error),
          all(table(unbalanced$treatment) == 60))

fit_models <- function(dat, design) {
  models <- list(
    'Treatment only' = lm(distance ~ treatment, data = dat),
    'Fixed center effects' = lm(distance ~ treatment + center, data = dat),
    'Random center effects' = lme(distance ~ treatment, random = ~1 | center,
                                  data = dat, method = 'REML'))
  rows <- lapply(names(models), function(name) {
    model <- models[[name]]
    if (inherits(model, 'lme')) {
      estimate <- fixef(model)[['treatmentEnhanced']]
      se <- summary(model)$tTable['treatmentEnhanced', 'Std.Error']
      vc <- as.numeric(VarCorr(model)[1, 'Variance'])
    } else {
      estimate <- coef(model)[['treatmentEnhanced']]
      se <- summary(model)$coefficients['treatmentEnhanced', 'Std. Error']
      vc <- NA_real_
    }
    data.frame(design, model = name, estimate, se,
               residual_sd = sigma(model), center_variance = vc)
  })
  do.call(rbind, rows)
}

results <- rbind(fit_models(balanced, 'Week 1 balanced'),
                 fit_models(unbalanced, 'Unequal treatment proportions'))
allocation <- data.frame(center = centers, standard = unname(n_standard),
  enhanced = 20 - unname(n_standard),
  true_standard_mean = 360 + unname(center_effects[centers]))
# The pooled contrast also contains this baseline-composition contribution.
composition <- with(allocation,
  sum(enhanced * true_standard_mean) / sum(enhanced) -
  sum(standard * true_standard_mean) / sum(standard))

output <- file.path(script_dir, 'assets/allocation-imbalance')
dir.create(output, recursive = TRUE, showWarnings = FALSE)
write.csv(results, file.path(output, 'model_results.csv'), row.names = FALSE)
write.csv(allocation, file.path(output, 'allocation.csv'), row.names = FALSE)
write.csv(unbalanced, file.path(output, 'rehabilitation_unbalanced.csv'), row.names = FALSE)
jsonlite::write_json(list(
  seed = 158, generating_treatment_effect = 10, individual_error_sd = 24,
  allocation = allocation, results = results,
  baseline_composition = composition
), file.path(output, 'model_results.json'), auto_unbox = TRUE, digits = 10, pretty = TRUE)
cat('Treatment effect is 10 m in every center; error SD is 24 m.\n')
cat('Balanced data exactly reproduce Week 1; individual errors match across designs.\n\n')
print(allocation, row.names = FALSE)
cat('\n')
print(results, row.names = FALSE, digits = 7)
cat(sprintf('\nBaseline-composition contribution to expected pooled contrast: %.4f m\n', composition))
cat(sprintf('Expected pooled contrast with unequal allocation: %.4f m\n', 10 + composition))
