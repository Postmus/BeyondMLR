# Beyond MLR: Week 1

**Randomized block designs** — *Modeling center effects and comparing treatments*

Slides 2–5 introduce the course: why grouping matters for iid error assumptions, the before/during/after-class workflow, Quarto reports and submission of both source and HTML with code visible, and certificate/assessment requirements. The 80% attendance requirement, weekly assignment submissions and final-assignment grading are confirmed unchanged for this edition. Four probability recap slides follow the introduction, before Part 1.

## Lecture sequence

The 37-slide lecture moves from a motivating trial to an explanation of what omitting center does to the errors, the treatment estimate and its estimated uncertainty. Simulation figures illustrate the argument directly; the labs apply the center-adjusted models.

| Slides | Purpose |
| --- | --- |
| 6–7 | Probability recap: expected value, variance and standard deviation, then covariance and independence. Bullets on the left accompany paired plots and notation on the right, without regression or center effects. |
| 8–9 | Expectation and variance rules for linear combinations, the independent Normal-sum rule, and special cases for constants, rescaling, sums and differences. |
| 10–16 | Part 1: trial overview (11), pooled outcomes (12), treatment-only model and recap-rule calculations (13), outcome distribution (14), results (15) and diagnostics (16). |
| 17–18 | Part 2 divider and the multicenter design versus the pooled analysis. |
| 19 | Immediately rewrite the generating model to show what is left in the true error when center is omitted. Define the fixed center shift cⱼ; distinguish individual errors from combined errors. |
| 20–21 | Four histograms and four scatterplots follow A1, A2, B1 and B2 through 10,000 studies. Center effects remain fixed. The histogram caption gives the six center effects, individual-error SD and balanced allocation. Different means and equal variances coexist with independence. |
| 22 | Compare the true error distribution with the treatment-only model’s assumptions: independence and equal variances hold, but the model misses the center-specific means. |
| 23 | Pooled-error histogram with six equally weighted component curves and their mixture. Balanced center contributions and zero-average center effects give mean zero. |
| 24 | Balance makes center contributions cancel from the treatment contrast, leaving an unbiased treatment estimate. |
| 25 | Connect the pooled spread to the estimated SE. Center differences cancel from the treatment contrast but remain in the residual SD used to estimate its SE. |
| 26 | Consequences for inference: inflated estimated SEs widen confidence intervals and reduce power. The reported uncertainty increases, not the estimate’s actual sampling variability. The callout bridges to accounting for center in Lab 1. |
| 27–28 | Lab 1 and debrief: including center accounts for systematic differences, so residual SD estimates individual-error SD. Balance leaves the treatment estimate unchanged. |
| 29–31 | Part 3 divider (29), motivation for modeling a population of centers (30), then the random-intercept model and recap-rule derivation of the outcome expectation, variance and Normal distribution (31). |
| 32–33 | Four combined-error histograms and four scatterplots use the same positions and pairings as slides 20–21. Redraw center effects and individual errors in each study: all error means are zero, variance is 976 m², same-center covariance is 400 m², and between-center covariance is zero. |
| 34 | Variance and within-center covariance on the left lead to the ICC as a correlation on the right. Two distinct patients share cⱼ, so their covariance is σc²; divide by their common variance σc² + σ². Both interpretations follow, with treatment accounted for throughout. |
| 35–36 | Lab 2 placeholder, then a full-width overall summary titled “Comparing the models”. The table compares all three treatment estimates and SEs, followed by explanations of balance and variance separation. The callout distinguishes fixed and random descriptions of center variation. |
| 37 | Questions, immediately after the overall model-comparison summary. |

The recap compares a Normal distribution and a right-skewed Gamma distribution, both with mean 4, variance 4 and standard deviation 2. The joint-distribution examples use independently generated Normal variables versus positively correlated Normal variables, with the same population means and variances for both variables. Dashed lines mark population expectations; finite-sample covariance is not forced to equal the population value.

### Explanation through simulated errors

Part 2 moves directly from the multicenter design on slide 18 to the model rewriting on slide 19. The separate simulation overview, model-definition table, parameter setup, calibration assessment, live demonstration and findings slides are omitted. The remaining figures illustrate the argument rather than introducing a separate demonstration.

The fixed-center illustration uses six centers, 20 patients per center and exactly 10 per treatment in each center. Individual errors are independent Normal draws with SD 24 m. Center effects A–F are −17, +16, +8, −29, +27 and −5 m, held fixed across all 10,000 studies. The generating outcome parameters remain baseline 360 m and treatment effect 10 m; these constants do not affect the error plots. The settings needed to read the figures are included in the histogram caption.

After the error-distribution comparison, show the pooled-error mixture (23), then explain unbiasedness through balance (24). The estimated-SE explanation (25) refers back to both: center differences cancel from the contrast but contribute to the pooled spread. Slide 26 explains why the inflated estimated SE matters: unnecessarily wide confidence intervals, generally larger p-values, and lower power. It distinguishes reported uncertainty from actual sampling uncertainty and motivates including center. Lab 1 and its debrief conclude Part 2 by showing how including center improves SE calibration.

Part 3 begins from the fixed-effects model having resolved the SE issue. Its scientific motivation is to describe variation across a population of centers, beyond differences between the six included centers. It introduces a random-intercept model and applies the recap rules from slides 8–9 directly to the outcome: add expectations, add variances of independent terms, and use the Normal-sum rule. Treatment is treated as fixed while center effects and individual errors vary. The conditional/marginal labels are omitted. The shared random center contribution produces within-center dependence; two simulation-figure slides illustrate this with the same patient positions and pairings as in Part 2. Each study draws six independent Normal center effects (SD 20 m) and 120 independent individual errors (SD 24 m); errors within each center share that study’s center draw. Both figures use the same 10,000 studies, without recentering or decorrelating the samples. These are known generating errors, not fitted residuals. Slide 34 connects the total outcome variance and within-center covariance to the ICC, interpreted as both a correlation and a variance proportion after accounting for treatment, followed by Lab 2 and an overall model-comparison summary. There is no additional live demonstration, smaller toy example, or partial-pooling explanation. The full-width summary compares all three models without a separate ICC panel. It connects identical treatment estimates to balanced allocation and the center models’ better-calibrated standard errors to separating center and individual variation. The bottom statement distinguishes separate parameters for the six centers from a population distribution of center effects. This comparison is discussed in the lecture summary rather than repeated as a Lab 2 exercise.

The explanatory fixed-center figures on slides 20–21 use known generating errors from the same 10,000 full 120-patient studies, not fitted residuals. Every plotted pair uses aligned simulation rows; empirical covariance is not forced to zero. The standalone `#/concepts/block-se-calibration` simulator and former demonstration source remain available as supporting material, but the current deck does not depend on a live demonstration. Historical staged-histogram, toy-example and dartboard assets are also retained outside the deck.

The lecture compares treatment-only, fixed-center and random-intercept models, all assuming a common treatment effect. Each new model has a plain-language interpretation alongside its equation. The proposed Week 2 sequence starts with treatment-by-center interactions and random treatment slopes, followed by cluster randomization and crossed random effects; see the [Week 2 planning notes](../week2/PLANNING.md). These extensions are outside the scope of Week 1. Initial drafts of both labs now follow this lecture; see the [lab development notes](LABS.md). The new [Week 1 assignment](../../labs_2627/assignments/week1/assignment.qmd) combines the Lab 1 EDA with a random-intercept analysis of a tissue-donor experiment. Students adapt the lab code and report their findings; the three-model comparison is covered in the lecture debrief.

The presentation uses simulated knee-rehabilitation data from six centers with balanced treatment allocation. These are teaching data and do not represent a real clinical study. Center labels are deliberately permuted: the original A–F cohorts are now D, A, F, C, B and E. Center contributions in alphabetical A–F order are −17, +16, +8, −29, +27 and −5 m. The revised simulation uses 10 patients per treatment in each center, a baseline of 360 m, a treatment effect of 10 m, and individual-error SD of 24 m, retaining seed 158. The auxiliary single-center assets use Center D. The smaller trial and treatment effect make the change in uncertainty consequential; interpretation emphasizes confidence intervals rather than a particular significance threshold.

## Source structure

- `Beyond_MLR_Week_1.pptx`: presentation.
- `build_week1.js`: PptxGenJS content and slide order.
- `linear_combination_recap.js`: general expectation/variance rules and a four-row special-cases table; equation assets are generated by `generate_interlude_equations.js`.
- `probability_recap.js`: two introductory probability slides, including mean, variance, standard deviation, covariance and independence.
- `generate_probability_recap.R`: reproducible density and scatterplot figures; run `npm run probability-recap`. Assets are `assets/recap-distributions.png` and `assets/recap-covariance.png`.
- `course_introduction.js`: four introductory slides covering motivation, organization, reproducible reporting and assessment. Slide 2 introduces shared settings, between-group and within-group variation, implications for inference, and what you learn in the course, with a generated illustration on the right.
- `assets/grouped-patients-hospitals.png`: hospital and patient groups illustration generated with the built-in image tool; exact prompt in `assets/grouped-patients-hospitals-prompt.txt`.
- `generate_outcome_distributions.R`: wide conceptual Normal outcome-distribution figure for slide 14; available through `npm run outcome-distributions`.
- `error_recap.js`: two treatment-only model slides: systematic part/error on the left with distributional assumptions and the adding-a-constant rules from slide 9 on the right, followed by a single-panel outcome-distribution plot. Treatment is treated as fixed and conditioning notation is omitted; unused source for the former error-concept and covariance recap is retained.
- `error_interlude.js`: the full-model definition slide, omitted-center explanation, true-versus-assumed error comparison and fixed-versus-random sampling perspective, plus retained source for the former expected-error and covariance comparison.
- `fixed_center.js`: fixed-center explanation, title-and-image Lab 1 placeholder and results debrief concluding Part 2.
- `simulation_demonstration.js`: retained supporting source, omitted from the deck; four-step simulation overview, bias/calibration assessment, fixed-center demonstration placeholder and findings, and retained source for the former random-center demonstration bridge.
- `fixed_center_error_figures.js`: two 2 × 2 simulation-figure slides immediately after the omitted-center explanation, plus the pooled-error mixture figure before the true-SE derivation.
- `fixed_center_sampling.js`: pooled-error figure, balance/unbiasedness explanation, a side-by-side true/estimated SE comparison, and the consequences for confidence intervals and hypothesis tests in the balanced clinical design. The error-distribution and paired-error figures precede these derivations in the lecture.
- `design_conclusion.js`: retained source for the former closing design-principle slide, omitted to avoid repeating the Part 2 explanation and Lab 1 debrief.
- `omitted_center_consequences.js`: retained consequence columns and equation for the former random-center explanation.
- `generate_bias_precision.js`: reproducible SVG/PNG dartboard figure, using shared course colors and fonts. Run `npm run bias-precision` to regenerate it.
- `grouping_summary.js`: retained source for the three former closing summaries, omitted from the deck; slide 36 is the overall summary.
- `lab2_debrief.js`: title-and-image Lab 2 placeholder and a full-width model-comparison summary.
- `assets/lab2-break.png`: generated Lab 2 illustration; prompt saved in `assets/lab2-break-prompt.txt`.
- `assets/population-of-centers.png`: illustration generated with the built-in image tool for slide 30; exact prompt in `assets/population-of-centers-prompt.txt`.
- `random_center_model.js`: scientific motivation, random-intercept assumptions and outcome-distribution calculations, and the variance/covariance derivation of the ICC.
- `random_center_error_figures.js`: the two 2 × 2 combined-error figure slides after slide 31.
- `generate_random_center_error_figures.R`: generates and validates the new random-center illustrations; run `npm run random-center-error-figures`. Parameters, empirical moments and pair covariances are recorded in `assets/random-center-combined-errors.json`.
- `random_center_sampling.js`: retained source for the former random-center simulation and assumption/consequence review; not included in the deck.
- `generate_random_center_sampling.R`: retained legacy illustration, not used in the deck; reproducible random-center simulation, cumulative error histograms, paired-error plots and statistical validation. Run `npm run random-center-simulation` to regenerate it.
- `assets/random-center-sampling.json`: parameters, first study, empirical moments and treatment-estimate checks.
- `generate_fixed_center_sampling.R`: reproducible full 120-patient simulation using the clinical generating parameters. All three error figures and the treatment-estimate/SE checks use the same 10,000 trials. The pooled figure weights each center by 1/6; checks verify component areas, mixture normalization and its zero theoretical mean. The first 20 trials are also checked against `lm`.
- `assets/fixed-center-sampling.json`: clinical simulation settings, empirical moments for all 120 patient positions, paired-error correlations and covariances, pooled-mixture weights and means, and calibration summaries for both analyses.
- `generate_interlude_figure.R`: schematic covariance figures; no case-study data are used.
- `generate_interlude_equations.js`: optional regeneration of missing interlude equation assets.
- `generate_data.R`: simulation, figures and model calculations.
- `generate_center_summary.R`: ggplot2 generation of the patient observations, center-by-treatment means and overall treatment means from the existing trial data, with checks on allocation and mean calculations. Run `npm run center-summary` to regenerate it independently.
- `assets/center-summary.png`: retained reference exploratory plot; omitted from the lecture so students perform the EDA in Lab 1.
- `assets/rehabilitation-center-context.png`: generated illustration for the conceptual opening of Part 2; prompt saved in `assets/rehabilitation-center-context-prompt.txt`.
- `generate_initial_analysis.R`: pooled outcomes, treatment-only diagnostics and initial-model results from the existing complete dataset.
- `initial_model.js`: opening model equations, using `u` for the treatment-only error consistently with the interlude.
- `assets/initial-analysis-results.json`: coefficient estimates, uncertainty and residual variation for the opening.
- `assets/rehabilitation_data.csv`: simulated patient-level dataset.
- `assets/model_results.json`: statistics read by the deck builder.
- `assets/live-simulation-demo.png`: slide-wide live-demonstration illustration generated with the built-in image tool; exact prompt in `assets/live-simulation-demo-prompt.txt`.
- `assets/lab1-break.png`: illustration generated with the built-in image-generation tool; its prompt is saved in `assets/lab1-break-prompt.txt`.
- `assets/rehabilitation-walk-assessment.png`: generated illustration for the clinical case.
- `assets/equations/`: tracked high-resolution PNG fallbacks. Generated `.tex` and SVG intermediates remain local and are ignored by Git; equation definitions live in the JavaScript sources.
- `pptxgenjs_helpers/`: local text-sizing, image, LaTeX and QA helpers.
- `../../slides-common/js/`: shared course theme, layout primitives and course builders.
- `../../slides-common/R/common_fig.R`: shared figure palette and sizing conventions.

The lecture focuses on statistical concepts and interpretation; R code and software instructions are reserved for the labs.

Use ordinary punctuation rather than long dashes in slide text. Keep presenter cues and speaker-note-like introductory sentences off the slides. Slide text should communicate the content directly; all speaker notes remain empty.

For outcome-distribution comparisons, prefer plain boxplots without overlaid individual observations. Slide 12 uses this style.

All speaker notes are empty strings. Slide text, tables and layout elements are editable. Statistical figures and equations are embedded as images. Equations are authored as LaTeX; PNG fallbacks provide consistent PowerPoint/LibreOffice rendering. The estimated-parameter symbols in the equation on slide 28 use LaTeX assets rather than Unicode combining hats. The shared `inline_math.js` paragraph helper remains available for baseline-aligned inline symbols.

## Rebuild

Install dependencies once from the repository root with `npm ci`. The root workspace and
lockfile provide the local `node_modules/` installation used by this deck and shared helpers.
Do not set `NODE_PATH` or rely on packages in a temporary folder.

For slide-text or layout changes, run from the repository root:

```sh
npm run build:week1
```

This rebuilds the PPTX using existing assets and exports `output/pdf/Beyond_MLR_Week_1.pdf`.
Use `npm run slides:week1` or `npm run render:week1` for either step separately.
LibreOffice must be installed; the render script handles its isolated profile and runtime.

When figure sources or data change, use `npm run rebuild:week1` from the repository root.
R requires `ggplot2`, `nlme` and `jsonlite`. Figure generation finishes before the presentation
is built. The fixed seed makes the data and model results reproducible. Supplied equation
PNGs allow ordinary slide rebuilds without a local TeX installation.

The individual figure commands below run from this directory after the root `npm ci`.

To regenerate only the fixed-center teaching simulation, run `npm run fixed-center-simulation`. Its checks verify allocation, error moments, independence, treatment-estimate bias and SE calibration. Closed-form estimates and SEs are checked against `lm`. The histogram and paired-error figures use the same full trials; the first two patient positions in centers A and B appear in four histograms, and two within-center plus two between-center pairs appear in the scatterplots. All eight panels use 10,000 observations; the pooled figure uses all 1,200,000 true errors across the six centers; empirical correlations and covariances are recorded without forcing them to zero.

To regenerate only the initial-analysis figures and statistics, run `npm run initial-analysis`. This reads the existing trial data without regenerating it or changing later-slide assets.

To regenerate missing opening and interlude equation assets, run `npm run interlude-equations` with `pdflatex` and `pdftoppm` available. Existing equation PNGs are preserved. The interlude uses the shared `slides-common/js/concept_panels.js` layout helper; other slide builders are unaffected.

The Lab 1 and Lab 2 debrief values come from the lecture dataset. The standalone calibration simulator is optional supporting material outside the slide sequence.
