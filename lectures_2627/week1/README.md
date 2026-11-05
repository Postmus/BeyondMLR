# Beyond MLR: Week 1

**Randomized block designs** — *Modeling center effects and comparing treatments*

Slides 2–5 introduce the course: why grouping matters for iid error assumptions, the before/during/after-class workflow, Quarto reports and submission of both source and HTML with code visible, and certificate/assessment requirements. The 80% attendance requirement, weekly assignment submissions and final-assignment grading are confirmed unchanged for this edition. The course introduction leads directly into Part 1.

## Lecture sequence

The 31-slide lecture moves from a motivating trial to a lecturer-led demonstration, explanation of its findings, and student application in the labs.

| Slides | Purpose |
| --- | --- |
| 6–11 | Part 1: the 120-patient rehabilitation trial, complete treatment-only model (9), model results (10) and model diagnostics (11). OLS review is assigned as homework; there are no separate error-recap or model-completion slides. Part 1 ends after slide 11. |
| 12–17 | Part 2 opening and multicenter design versus pooled analysis (13), a four-step simulation overview (14), the full fixed-effects generating model (15), parameter setup (16), then “Assessing bias and calibration” (17) immediately before the live demonstration. The overview distinguishes a model that reflects the study design from the design itself. |
| 18–19 | Image-only live-demonstration placeholder (18), then the finding: unbiased treatment estimates with overstated uncertainty. Slide 19 connects this to overcoverage of the usual 95% confidence intervals and a type I error rate below 5% when the true treatment effect is zero. |
| 20–22 | Decompose the omitted-center error and state its consequence: independent Normal errors with center-specific means and equal variances. Slide 21 substitutes the full patient-level model into the treatment contrast before cancelling the shared baseline and center terms, then gives the true SE symbolically. Slide 22 gives a parallel derivation under the treatment-only model’s iid zero-mean error assumption, then explains why its residual SD includes center differences and yields an overstated SE. |
| 23–24 | Lab 1 and debrief: students create the EDA plot, fit the fixed-effects model, and find an unchanged treatment estimate with a smaller estimated SE. The debrief directly addresses the SE problem on slide 22: including center accounts for systematic center differences in the fitted outcomes, so the residual SD now estimates the individual-error SD. The treatment estimate and its sampling distribution remain unchanged because balanced allocation already makes center contributions cancel. The slide substitutes the fitted individual-error SD into the SE expression and links better calibration to the variation that contributes to the treatment comparison in this randomized block design. It then leads directly into Part 3. |
| 25–28 | Part 3 divider, scientific motivation for modeling a population of centers in four bullets beside a generated illustration, without a callout (26), random-intercept model and its implied marginal model side by side, with explanations of the conditional and marginal means (27), then variance components and ICC (28). The marginal mean matches the treatment-only structure, but combined errors share a random center effect and are dependent within centers. |
| 29–30 | Title-and-image Lab 2 placeholder (29), then a two-column debrief interpreting the ICC and comparing the three models’ treatment estimates and standard errors (30). |
| 31 | Questions, directly after the Lab 2 debrief. |

### Lecturer-led demonstration

The demonstration is lecturer-led, not an independent student activity. Slide 18 is a visual placeholder with its existing title and a slide-wide generated illustration, without bullet instructions. During the demonstration, first compare the mean treatment estimate with the true effect, then compare the empirical SE with the mean estimated SE. The demonstration focuses on one selected analysis model and its empirical sampling distribution. Within each batch, changing the analysis must reuse the same generated studies. The comparison is the empirical SD of treatment estimates versus the average estimated SE from that selected model. Their agreement is approximate: a mean estimated SE has a small finite-sample bias, and both empirical summaries fluctuate with the number of repetitions.

For Part 2, use six centers, 20 patients per center and exactly 10 per treatment in each center. Keep baseline 360 m, treatment effect 10 m, independent Normal individual errors with SD 24 m, and center effects A–F of −17, +16, +8, −29, +27 and −5 m fixed across repetitions. Begin with treatment only. Explain the findings, then let students fit the fixed-effects model and construct the EDA in Lab 1. The debrief concludes Part 2; there is no return to the fixed-center simulation or separate summary slide. Slide 17 explicitly frames the SE comparison as a check of calibration. Slide 19 explains confidence-interval coverage and type I error as consequences under this generating model. The type I error statements concern a true zero treatment effect, not an observed rejection rate from the 10 m demonstration. There is no separate type I error/power simulation.

Part 3 begins from the fixed-effects model having resolved the SE issue. Its scientific motivation is to describe variation across a population of centers, beyond differences between the six included centers. It introduces a random-intercept model and explicitly rewrites it as a marginal model with combined error cⱼ + εᵢⱼ. The marginal mean retains the treatment-only structure, while the shared random center contribution produces within-center dependence. Variance components and ICC follow, then Lab 2 and one interpretation-focused debrief. There is no second simulation, smaller toy example, or partial-pooling explanation. The debrief shows the estimated ICC and its two interpretations beside a table comparing all three models. It connects identical treatment estimates to balanced allocation and the center models’ better-calibrated standard errors to separating center and individual variation. The bottom statement distinguishes separate parameters for the six centers from a population distribution of center effects. This comparison is discussed in the lecture rather than repeated as a Lab 2 exercise.

The lecturer-led demonstration now uses the separate `#/concepts/block-se-calibration` module in the sampling-simulator project. It retains fixed center effects and offers treatment-only and fixed-center analyses of the same simulated datasets. The labels distinguish estimated SEs within datasets from the empirical SE across simulations. A boxplot shows the estimated SEs; the empirical SE and calibration comparison appear after 1,000 completed datasets. The original randomized-block module remains unchanged.

Part 2 omits the separate four-patient example, its individual-study tables and assumption-review table, the staged error histograms, the lecture EDA slide, and the separate hypothesis-testing slides. The clinical EDA is now a student activity in Lab 1. The two explanatory error plots are generated together from 10,000 full 120-patient trials; they use known generating errors, not fitted residuals. The former Part 3 simulation figures and source are retained as supporting material but are not used in the lecture. Historical staged-histogram and dartboard assets are retained but are not used in the deck.

The lecture compares treatment-only, fixed-center and random-intercept models, all assuming a common treatment effect. Each new model has a plain-language interpretation alongside its equation. The proposed Week 2 sequence starts with treatment-by-center interactions and random treatment slopes, followed by cluster randomization and crossed random effects; see the [Week 2 planning notes](../week2/PLANNING.md). These extensions are outside the scope of Week 1. Initial drafts of both labs now follow this lecture; see the [lab development notes](LABS.md). The new [Week 1 assignment](../../labs_2627/assignments/week1/assignment.qmd) combines the Lab 1 EDA with a random-intercept analysis of a tissue-donor experiment. Students adapt the lab code and report their findings; the three-model comparison is covered in the lecture debrief.

The presentation uses simulated knee-rehabilitation data from six centers with balanced treatment allocation. These are teaching data and do not represent a real clinical study. Center labels are deliberately permuted: the original A–F cohorts are now D, A, F, C, B and E. Center contributions in alphabetical A–F order are −17, +16, +8, −29, +27 and −5 m. The revised simulation uses 10 patients per treatment in each center, a baseline of 360 m, a treatment effect of 10 m, and individual-error SD of 24 m, retaining seed 158. The auxiliary single-center assets use Center D. The smaller trial and treatment effect make the change in uncertainty consequential; interpretation emphasizes confidence intervals rather than a particular significance threshold.

## Source structure

- `Beyond_MLR_Week_1.pptx`: presentation.
- `build_week1.js`: PptxGenJS content and slide order.
- `course_introduction.js`: four introductory slides covering motivation, organization, reproducible reporting and assessment. Slide 2 uses four bullets ordered from setting to problem, approach and course purpose, with a generated illustration on the right.
- `assets/grouped-patients-hospitals.png`: hospital and patient groups illustration generated with the built-in image tool; exact prompt in `assets/grouped-patients-hospitals-prompt.txt`.
- `error_recap.js`: complete treatment-only model with iid Normal errors and the outcome-distribution plot; unused source for the former error-concept and covariance recap is retained.
- `error_interlude.js`: the full-model definition slide, omitted-center comparison and fixed-versus-random sampling perspective, plus retained source for the former expected-error and covariance comparison.
- `fixed_center.js`: fixed-center explanation, title-and-image Lab 1 placeholder and results debrief concluding Part 2.
- `simulation_demonstration.js`: four-step simulation overview, bias/calibration assessment, fixed-center demonstration placeholder and findings, and retained source for the former random-center demonstration bridge.
- `fixed_center_sampling.js`: two-slide derivation of the true sampling SE and the treatment-only model’s residual-based SE estimate in the balanced clinical design. The former error-distribution and paired-error figures are retained as supporting assets but omitted from the lecture.
- `design_conclusion.js`: retained source for the former closing design-principle slide, omitted to avoid repeating the Part 2 explanation and Lab 1 debrief.
- `omitted_center_consequences.js`: retained consequence columns and equation for the former random-center explanation.
- `generate_bias_precision.js`: reproducible SVG/PNG dartboard figure, using shared course colors and fonts. Run `npm run bias-precision` to regenerate it.
- `lab2_debrief.js`: title-and-image Lab 2 placeholder and a debrief combining ICC interpretation with the three-model treatment comparison.
- `assets/lab2-break.png`: generated Lab 2 illustration; prompt saved in `assets/lab2-break-prompt.txt`.
- `assets/population-of-centers.png`: illustration generated with the built-in image tool for slide 26; exact prompt in `assets/population-of-centers-prompt.txt`.
- `random_center_model.js`: scientific motivation, random-intercept and marginal formulations, and variance components/ICC.
- `random_center_sampling.js`: retained source for the former random-center simulation and assumption/consequence review; not included in the deck.
- `generate_random_center_sampling.R`: reproducible random-center simulation, cumulative error histograms, paired-error plots and statistical validation. Run `npm run random-center-simulation` to regenerate it.
- `assets/random-center-sampling.json`: parameters, first study, empirical moments and treatment-estimate checks.
- `generate_fixed_center_sampling.R`: reproducible full 120-patient simulation using the clinical generating parameters. Both error figures and the treatment-estimate/SE checks use the same 10,000 trials. The first 20 trials are also checked against `lm`.
- `assets/fixed-center-sampling.json`: clinical simulation settings, empirical moments for all 120 patient positions, paired-error correlations, and calibration summaries for both analyses.
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

For outcome-distribution comparisons, prefer plain boxplots without overlaid individual observations. Slide 8 uses this style.

All speaker notes are empty strings. Slide text, tables and layout elements are editable. Statistical figures and equations are embedded as images. Equations are authored as LaTeX; PNG fallbacks provide consistent PowerPoint/LibreOffice rendering. The estimated-parameter symbols in the equation on slide 24 use LaTeX assets rather than Unicode combining hats. The shared `inline_math.js` paragraph helper remains available for baseline-aligned inline symbols.

## Rebuild

Run from this directory after installing the Node dependencies in `package.json`. R requires `ggplot2`, `nlme` and `jsonlite`.

```sh
npm install
npm run build
```

Figure generation finishes before the presentation is built. The fixed seed makes the data and model results reproducible. The supplied equation PNGs allow the presentation to render without a local TeX installation.

To regenerate only the fixed-center teaching simulation, run `npm run fixed-center-simulation`. Its checks verify allocation, error moments, independence, treatment-estimate bias and SE calibration. Closed-form estimates and SEs are checked against `lm`. The histogram and paired-error figures use the same full trials; six designated patient positions appear in the histograms, and two fixed pairs appear in the scatterplots.

To regenerate only the initial-analysis figures and statistics, run `npm run initial-analysis`. This reads the existing trial data without regenerating it or changing later-slide assets.

To regenerate missing opening and interlude equation assets, run `npm run interlude-equations` with `pdflatex` and `pdftoppm` available. Existing equation PNGs are preserved. The interlude uses the shared `slides-common/js/concept_panels.js` layout helper; other slide builders are unaffected.

The live demonstration uses `#/concepts/block-se-calibration` in the separate sampling-simulator project. The Lab 1 and Lab 2 debrief values continue to come from the lecture dataset.
