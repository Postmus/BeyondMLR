# Assignment 1 recap and a hypothetical change in allocation within donors.
library(lmerTest)
library(jsonlite)
script_path <- sub('^--file=', '', commandArgs()[grepl('^--file=', commandArgs())][1])
script_dir <- dirname(normalizePath(script_path))
data_path <- file.path(script_dir, '../../labs_2627/assignments/week1/downloads/protein_yield.csv')
original <- read.csv(data_path)
donors <- unique(original$donor)
protocols <- c('Standard', 'Modified')
n_standard <- setNames(c(1, 2, 3, 3, 4, 4, 5, 5, 6, 7), donors)

generate <- function(standard_counts) {
  # Replay Assignment 1's seed and draws. The two allocations use identical
  # random permutations, donor contributions and individual aliquot errors.
  set.seed(262701)
  donor_effects <- rnorm(length(donors), 0, 12)
  dat <- do.call(rbind, lapply(seq_along(donors), function(j) {
    n0 <- standard_counts[[donors[j]]]
    protocol <- sample(c(rep('Standard', n0), rep('Modified', 8 - n0)))
    error <- rnorm(8, 0, 8)
    data.frame(aliquot = original$aliquot[original$donor == donors[j]],
      donor = donors[j], protocol, individual_error = error,
      protein_yield = round(60 + donor_effects[j] + 6 * (protocol == 'Modified') + error, 2))
  }))
  attr(dat, 'donor_effects') <- donor_effects
  dat
}
balanced_replay <- generate(setNames(rep(4, 10), donors))
unbalanced <- generate(n_standard)
stopifnot(identical(balanced_replay$aliquot, original$aliquot),
          identical(balanced_replay$donor, original$donor),
          identical(balanced_replay$protocol, original$protocol),
          identical(balanced_replay$protein_yield, original$protein_yield),
          identical(balanced_replay$individual_error, unbalanced$individual_error),
          all(table(original$donor, original$protocol) == 4),
          all(table(unbalanced$protocol) == 40))

fit_models <- function(dat, design) {
  dat$donor <- factor(dat$donor)
  dat$protocol <- factor(dat$protocol, levels = protocols)
  contrasts(dat$donor) <- contr.sum(nlevels(dat$donor))
  models <- list(
    'Protocol only' = lm(protein_yield ~ protocol, data = dat),
    'Fixed donor effects' = lm(protein_yield ~ protocol + donor, data = dat),
    'Random donor effects' = lmer(protein_yield ~ protocol + (1 | donor), data = dat, REML = TRUE))
  rows <- lapply(names(models), function(name) {
    model <- models[[name]]
    co <- coef(summary(model))['protocolModified', ]
    mixed <- inherits(model, 'merMod')
    if (mixed) stopifnot(!isSingular(model), is.null(model@optinfo$conv$lme4$messages))
    df <- if (mixed) co['df'] else df.residual(model)
    estimate <- unname(co['Estimate']); se <- unname(co['Std. Error'])
    data.frame(design, model = name, estimate, se, df = unname(df),
      lower_95 = estimate - qt(.975, df) * se,
      upper_95 = estimate + qt(.975, df) * se,
      p = unname(co['Pr(>|t|)']), residual_sd = sigma(model),
      donor_variance = if (mixed) as.numeric(VarCorr(model)$donor) else NA_real_)
  })
  do.call(rbind, rows)
}
results <- rbind(fit_models(original, 'Assignment 1 balanced'),
                 fit_models(unbalanced, 'Unequal protocol proportions'))
b <- subset(results, design == 'Assignment 1 balanced')
stopifnot(diff(range(b$estimate)) < 1e-7,
          abs(b$se[2] - b$se[3]) < 1e-5)
allocation <- data.frame(donor = donors, standard = unname(n_standard),
  modified = 8 - unname(n_standard),
  true_standard_mean = 60 + attr(balanced_replay, 'donor_effects'))
composition <- with(allocation,
  weighted.mean(true_standard_mean, modified) - weighted.mean(true_standard_mean, standard))
out <- file.path(script_dir, 'assets/allocation-imbalance')
dir.create(out, recursive = TRUE, showWarnings = FALSE)
write.csv(results, file.path(out, 'model_results.csv'), row.names = FALSE)
write.csv(allocation, file.path(out, 'allocation.csv'), row.names = FALSE)
write.csv(unbalanced, file.path(out, 'protein_yield_unbalanced.csv'), row.names = FALSE)
write_json(list(seed = 262701, assignment_data_md5 = unname(tools::md5sum(data_path)),
  generating_treatment_effect = 6, individual_error_sd = 8,
  inference = 'OLS t inference; random-intercept REML with Satterthwaite degrees of freedom, matching Lab 2.',
  allocation = allocation, results = results, baseline_composition = composition),
  file.path(out, 'model_results.json'), auto_unbox = TRUE, digits = 10, pretty = TRUE)
cat('Assignment 1 replay and allocation checks passed. Common protocol effect: 6 µg/mg.\n')
print(results, row.names = FALSE, digits = 6)
