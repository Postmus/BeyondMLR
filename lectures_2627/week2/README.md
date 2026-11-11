# Beyond MLR: Week 2

The revised opening has 25 main slides and two backup-section slides. It begins with a course title slide and the Week 1 model-comparison table,
then examines unequal treatment allocation in four formula-free slides before
introducing treatment-by-center interaction with dummy coding for treatment
and effects coding for center. It compares additive and interaction models for the average treatment effect,
then introduces uncertainty about a population average.
See [PLANNING.md](PLANNING.md) for the rest of the proposed lecture.

## Current lecture opening

| Slides | Content |
| --- | --- |
| 1 | Title: Treatment effects across centers — Interactions and uncertainty. |
| 2 | Week 1 recap: rehabilitation trial setting beside its closing comparison table. |
| 3–4 | Unequal treatment allocation within centers and the fitted three-model comparison. |
| 5 | Within- versus between-center comparisons and the random-intercept assumptions. |
| 6 | Unequal center sizes versus unequal treatment proportions: comparison table with a plain-language explanation. |
| 7 | Part 1 divider: Treatment-by-center interaction. |
| 8 | Known-means 2 × 2 additive example: marginal averages, parallel treatment lines, equal treatment-gap annotations and the individual-outcome model. |
| 9 | Matching known-means interaction example: treatment gaps of 10/30 m, average effect of 20 m, center deviations of -10/+10 m and the interaction model. |
| 10 | Conceptual comparison: fixed center parameters versus a population distribution of random center effects. |
| 11 | Two-column empirical transition: trial setting and estimation task on the left, generated rehabilitation image on the right. |
| 12 | Separate exploratory analysis: patient outcomes and sample means by treatment and center. |
| 13 | Grouped fixed-effects model with a center-specific treatment slope, parameter interpretations and enlarged effects-coding constraints. |
| 14 | All 12 fitted regression coefficients: estimates, estimated SEs, 95% confidence intervals and p-values, in the Week 1 table format. |
| 15 | Nested-model interaction test: full/reduced model RSS and residual df, annotated F formula and numerical substitution. |
| 16 | Simple versus marginal treatment effects, links to beta_1 + d_j and beta_1, and the definition of estimated marginal means (EMMs). |
| 17 | Coding explanation alongside predicted treatment means and simple effects, pointwise 95% CIs, and Bonferroni-adjusted p-values across six centers. |
| 18 | Full interaction model diagnostics: residuals versus fitted values and a Normal Q-Q plot, closing the case study. |
| 19 | Average treatment estimates, estimated SEs, 95% CIs and p-values under additive and interaction models; plain-language explanation of the difference. |
| 20 | Unequal center sizes: equally weighted interaction-model average versus the size-weighted additive estimate, illustrated by 20 m versus 26 m. |
| 21 | Part 2 divider: From included centers to a population. |
| 22–25 | Repeating the study in different centers, the extra random-slope variance term, and implications for patient and center sample sizes. |
| 26–27 | Backup divider and the explanation of why the estimated standard errors are almost identical in the unequal-allocation example. |

The title and section dividers use the same shared layouts as Week 1. The
last four main slides are a theoretical bridge; fitting random slopes and the later
cluster-randomization section remain to be developed. Labs remain separate work.

Current sources and outputs:

- `additive_example.js` and `generate_additive_example.R`: the paired known-means introductory slides and reproducible PNG/SVG table-and-plot assets.
- `allocation_imbalance.js`: four formula-free main slides and the precision backup; tables read the R-generated allocation and results.
- `fixed_interaction.js`: slide content and equation definitions, using the
  shared concept-panel, teaching-table and figure layouts.
- `build_week2.js`: deck construction and layout checks.
- `generate_equations.js`: optional TeX/MathJax equation regeneration.
- `Beyond_MLR_Week_2.pptx`: editable slide text and tables, with raster equations
  and statistical figures.
- `../../output/pdf/Beyond_MLR_Week_2.pdf`: rendered review copy.

From the repository root, use `npm run build:week2` for a normal build and PDF
export using the existing assets. Use `npm run rebuild:week2` to regenerate data,
figures and any missing equation images before building. Dependencies use the
existing root installation (`npm ci`); PDF export requires LibreOffice and equation
regeneration requires TeX/Poppler, as in Week 1.

The fixed-target derivation conditions on the included centers, with independent
homoscedastic individual errors. With J centers and n patients per treatment per
center, the variance of the average is `2 sigma^2 / (J n)`. Under independently
sampled random treatment effects, the population-target variance also includes
`sigma_d^2 / J`. These are balanced-model sampling variances, distinct from fitted
standard errors and uncertainty in estimated variance components. The random-slope
interpretation follows [Bates et al.](https://lme4.github.io/lme4/articles/lmer.pdf).

All content slides and the added title and dividers were visually checked at full render resolution, including the
figure labels and equation panels. The final deck passes the OOXML validator,
font checks and layout checks, and opens read-only in Windows PowerPoint 16
with repair disabled. Reproducible PptxGenJS export issues (a colliding native
slide-number shape ID, notes-master list ordering and a shared notes/slide theme) were corrected in the
shared exporter and covered by focused regression checks.

## Unequal-allocation example

Run `Rscript lectures_2627/week2/explore_allocation_imbalance.R` from the repository
root, or `npm run allocation-data` from this directory. The generator writes
allocation, patient data, and fitted results to `assets/allocation-imbalance/`.
It is included in the full rebuild pipeline and uses `nlme` and `jsonlite`.

The standard/enhanced allocations in centers A–F are 5/15, 8/12, 10/10, 10/10,
12/8 and 15/5. Both treatments still have 60 patients overall. The generator
replays Week 1's seed, center effects and individual errors, regenerating outcomes
for the changed assignments. The true treatment effect remains 10 m everywhere;
the balanced replay is checked against the original Week 1 data.

The treatment-only, fixed-center and random-intercept estimates are 3.87, 8.40
and 8.13 m, with standard errors 5.75, 4.56 and 4.55 m. Center uses effects coding
in the fixed model; the random-intercept model is fitted by REML. The discussion
distinguishes within-center comparisons from extra between-center information,
which relies on baseline center effects being unrelated to treatment allocation.
Interaction is first introduced through the known-means toy example on slide 9; the six-center empirical analysis begins on slide 11, followed by exploratory analysis on slide 12 and model specification on slide 13.

The design-comparison table in slide 6 assumes a common treatment effect and
independent homoscedastic individual errors. Equal treatment proportions in every
center preserve identical fixed- and random-center treatment estimates even with
unequal center sizes. Exact equality of their estimated standard errors in the
fully balanced case assumes REML and a positive estimated center variance, as in
Week 1. With unequal center sizes, estimated individual-error variances can differ.
The slide explains this without formulas or a technical footnote. Its title
explicitly compares fixed and random center effects, and “Can differ” is used
consistently. One sentence beneath the table explains that identical treatment
estimates also hold for the treatment-only model when allocation is 50:50 within
every center. The callout introduces treatment effects that vary across centers.
There is no additional explanation of variance estimation on the slide.
The first row says “Identical” for the standard balanced analysis used in the
lecture. The speaker notes retain the fitting-method and boundary qualifications;
the equality is a general result under those conditions, not specific to Week 1.
Use “estimated standard error”, not “reported
standard error”, in teaching copy. The former slide 6 is retained as backup slide 27.

## Known-means additive and interaction examples

Slide 8 assumes the four true means are known: center A has standard/enhanced
means of 350/370 m and center B has 390/410 m. These are hypothetical expected
outcomes, not estimated values or newly generated patient observations. Row
averages are 360 and 400; column averages are 370 and 390; the grand mean is 380.
Treatment is coded 0/1, so beta_0 is the standard-care column average (370),
c_A = -20, c_B = 20, and beta_1 = 20. Center is on the plot's horizontal axis,
color denotes treatment, and two vertical brackets show beta_1 = 20 at A and B.
The equation distinguishes these means from individual patient outcomes and errors.
Slide 9 changes the enhanced-care means to 360/420 m while retaining standard-care
means of 350/390 m. Row averages become 355/405; treatment column averages and the
grand mean remain unchanged. The two gap labels are beta_1 + d_A = 10 and
beta_1 + d_B = 30. An annotation below the legend gives the average treatment
effect beta_1 = 20 m; deviations d_A = -10 m and d_B = +10 m appear below their
centers. The model replaces beta_1 T_ij with (beta_1 + d_j) T_ij. Both slides use
the same axis coordinates and treatment colors.

Regenerate with `Rscript lectures_2627/week2/generate_additive_example.R` or the
local `npm run additive-example` command. It is included in the figure pipeline
and uses base R/grid with the shared course figure palette.

## From center representation to the fixed model

Slide 10 first distinguishes separate baseline/treatment deviations for the
included centers from a population distribution of those deviations. Both routes
allow treatment effects to vary. The transition callout starts the fixed-center
analysis; uncertainty details remain in the later section.

Slide 13 uses the grouped formulation
`Y_ij = beta_0 + c_j + (beta_1 + d_j) T_ij + epsilon_ij` so the center-specific
treatment slope is explicit. Two columns define baseline and treatment averages
and deviations, with separate sum-to-zero constraints. Treatment retains 0/1
coding, and each center receives equal weight in the average parameters. Speaker
notes distinguish this treatment slope from the plotted lines across centers.
Slide 11 makes the transition from known toy means to estimating unknown group means from patient outcomes, using the shared two-column text/image layout. Slide 12 separately explores the six-center data and explicitly identifies the diamonds as sample means. The first later use of delta_j defines it as the true center-specific effect.

The transition image is a fictional rehabilitation scene generated with the
built-in image tool. Its project asset is
`assets/fixed-interaction/rehabilitation-case-study.png`; the exact prompt and
provenance are recorded in `rehabilitation-case-study-prompt.txt` beside it.

Slide 14 presents all 12 model coefficients with estimates and confidence limits
to one decimal, estimated SEs to two decimals, and p-values to three decimals or
`< .001`, following Week 1. The generator exports `regression_coefficients.csv`
and includes the same full-precision results in `model_results.json`. Intervals
and two-sided coefficient tests use 108 residual degrees of freedom. The five
center coefficients describe baseline deviations; the five interaction
coefficients describe treatment-effect deviations. Center F is implied by the
sum-to-zero constraints, so it has no additional coefficient row. Interaction
coefficient p-values test deviations from the average effect, not each center's
overall treatment effect or the global interaction hypothesis.

Slide 15 introduces nested-model comparison through the joint interaction test.
Both models retain center-specific baselines. The reduced model imposes a common
treatment effect; the full model estimates center-specific treatment effects.
The table shows RSS and residual df, followed by the general partial F formula
and substitution of those table values. The numerator measures improvement per
additional parameter and the denominator estimates individual-error variance
from the full model. The result is F(5, 108) = 5.95, p < .001. RSS is rounded on
the slide; the test uses full precision. Speaker notes cover nesting, the joint
null hypothesis, and the common-variance normal-error assumptions. The RSS values
are exported by R, and the equation asset is generated from the same results.

Slide 16 distinguishes simple treatment effects within centers from the marginal
effect averaged equally across centers. It defines estimated marginal means (EMMs)
on first use and links the effects to beta_1 + d_j and beta_1 under the current
coding. Slide 17 explains why coding affects which effects appear directly as
coefficients, then presents predicted means and enhanced-minus-standard contrasts.
No package names or software syntax appear on these slides.

The simple-effect table includes pointwise 95% t confidence intervals and
Bonferroni-adjusted two-sided p-values for the family of six center-specific
treatment comparisons. The adjustment is min(6 * p, 1), using 108 residual degrees
of freedom. Center A through F have adjusted p-values 1.000, .067, .707, 1.000,
< .001, and .001 when rounded for display. The interval and p-value conventions
are explicitly labeled: intervals remain pointwise, so they
need not agree with adjusted tests at .05. Coefficient-level tests on slide 14
remain unchanged. The R generator exports both raw and adjusted simple-effect
p-values in `within_center_comparisons.csv` and `model_results.json`.

Slide 18 closes the case study with residuals versus fitted values and a Normal
Q-Q plot of internally standardized residuals from the full interaction model.
It reuses the Week 1 diagnostics layout with taller plot slots. There is no clear
change in residual spread across fitted values, and the Q-Q points broadly follow
the reference line. Speaker notes explain the vertical stacks, that within-cell
residual centering is automatic in this full factorial model, and that these plots
do not establish independence. The former treatment-contrast plot remains an
available generated asset but is no longer shown on slide 18.

Slides 19 and 20 replace the former technical section on fixed-center uncertainty
and finish Part 1 without variance derivations. Slide 19 reads the two fitted
models' treatment summaries from R, including their two-sided t p-values. Equal
cell sizes keep the average estimate identical; omitted interaction increases
the estimated residual variation and estimated SE here, without changing the
average estimator's actual sampling variance. Slide 20 presents the interaction
model first, then the additive model under equal and unequal center sizes. Its
hypothetical two-center example uses effects 10/30 m and sizes 20/80, giving 20 m
with equal center weighting and 26 m under the additive model. The slide focuses
on coefficient interpretation, not a universal bias claim. Speaker notes retain
the fixed-allocation setup: the displayed size weights assume a common treatment
proportion across centers. No allocation sentence or weighting formula appears
on the slide. The population-center section is now Part 2; its previously
introduced symbols are defined locally where needed.

## Fixed-interaction lecture dataset

Run from the repository root:

```sh
Rscript lectures_2627/week2/generate_interaction_data.R
```

Alternatively, run `npm run interaction-data` from this directory. The generator
requires `ggplot2` and `jsonlite` and resolves paths relative to its own location.

The new simulated rehabilitation trial has 120 patients: six centers with 10
patients assigned to standard care and 10 to enhanced rehabilitation in each.
The outcome remains six-minute walk distance at 12 weeks, in meters. These are
new teaching observations, not additional observations from the Week 1 trial.
The lab will use a separate dataset, potentially adapted from the archived labs.

Outputs are in `assets/fixed-interaction/`:

- `rehabilitation_interaction_data.csv`: patient data, with `center`, `treatment`
  and `distance` columns, matching the Week 1 naming conventions.
- `generating_center_effects.csv`: known center-specific means and treatment
  effects, kept separately from the patient data.
- `fitted_cell_means.csv` and `within_center_comparisons.csv`: fitted means and
  enhanced-minus-standard contrasts, with pointwise 95% confidence intervals and raw/Bonferroni-adjusted p-values.
- `model_results.json`: generating parameters, model results, interaction test
  and R session information.
- `interaction-outcomes.png`: patient observations and connected treatment means
  in six center panels on a common outcome scale.
- `interaction_diagnostics.csv`: fitted values, raw residuals and internally standardized residuals from the full interaction model.
- `interaction-residuals-vs-fitted.png` and `interaction-normal-qq.png`: the two diagnostics displayed on slide 18.
- `interaction-contrasts.png`: estimated treatment differences and confidence
  intervals, with the equally weighted average as a reference line.
- `interaction-outcomes-slide.png` and `interaction-contrasts-slide.png`: versions
  with larger labels and captions supplied by the slide layout.

### Generating model and interpretation

Seed 20261110 generates one bivariate Normal intercept/slope pair per center,
then independent individual errors. The population baseline is 360 m and the
population-average treatment effect is 10 m. Intercept and treatment-effect
standard deviations are both 20 m, with correlation 0.25; individual-error
standard deviation is 24 m. Outcomes are rounded to 0.1 m. The realized center
effects are not recentered. This generation also supports the later random-slope
section, while the initial analysis treats these six centers' parameters as fixed.

The true treatment effects for Centers A–F are 6.52, 21.42, 1.74, −7.52, 29.77
and 41.03 m. Their equally weighted average is 15.49 m. Distinguish this realized
six-center average from the generating population average of 10 m and the
estimated six-center average of 21.27 m. Individual sampling noise also means
the observed ordering of treatment effects can differ from their true ordering.

Use the fixed model

```r
rehab$treatment <- factor(rehab$treatment, levels = c("Standard", "Enhanced"))
rehab$center <- factor(rehab$center, levels = paste("Center", LETTERS[1:6]))
contrasts(rehab$treatment) <- contr.treatment(levels(rehab$treatment))
contrasts(rehab$center) <- contr.sum(levels(rehab$center))
model_interaction <- lm(distance ~ treatment * center, data = rehab)
```

In the notation `Y_ij = beta_0 + c_j + (beta_1 + d_j) T_ij + epsilon_ij`, both
`sum(c_j) = 0` and `sum(d_j) = 0`. The intercept is the expected standard-care
outcome averaged equally across the six centers. The treatment coefficient is
the equally weighted average treatment effect; each interaction coefficient is
a center's deviation from that average. Center F's contributions are minus the
sums of the corresponding five displayed center and interaction coefficients.
Effects coding constrains the fixed parameters; it does not require altering
the simulated random draws.

### Checked analysis

The center-specific estimated differences are 14.16, 29.62, −18.09, 0.58, 57.42
and 43.90 m. The interaction model gives an average of 21.27 m (standard error
4.68 m; 95% confidence interval 11.98 to 30.55 m). The overall interaction test
comparing the additive and interaction models gives F(5, 108) = 5.95,
p = 0.0000675. Intervals for individual center contrasts are pointwise,
without multiplicity adjustment.

Omitting the fixed interaction leaves the average estimate unchanged in this
balanced design, but increases its estimated standard error to 5.17 m. This
comparison concerns an average across these six fixed centers. Inference about
a wider population of center-specific treatment effects is a different target
for the later random-slope section.

The script checks treatment allocation, fitted versus observed cell contrasts,
the effects-coded intercept and treatment coefficient, the variance of the
average contrast, sum-to-zero constraints, and agreement of fitted values under
reference coding. Both figures were visually inspected. The script computes
contrasts directly from the fitted model's design and covariance matrices;
the lecture introduces the prediction grid and averaging concept, while
`emmeans` code can be introduced in the later lab.

## Earlier cluster-randomization starter

The preserved starter source continues the rehabilitation example from an
earlier Week 1 draft and introduces cluster randomization. Its data, generator
constants and slide sequence need updating before integration into the revised
lecture. The following details describe that earlier starter.

The sequence is a Week 1 recap, a comparison of randomization levels, a simulated cluster-randomized trial, the random-intercept model with center-level treatment, numerical uncertainty, and repeated-trial discussion. All models retain a common treatment effect across centers. New equations have plain-language interpretations. Labs and assignments are unchanged.

### Archived starter sources

- `build_cluster_starter.js`: preserved PptxGenJS content and slide order; it
  writes `Beyond_MLR_Week_2_cluster_starter.pptx` when explicitly run.
- `generate_data.R`: cluster-assigned teaching data, model results and statistical figure.
- `assets/rehabilitation_cluster_data.csv`: simulated patient outcomes under center-level assignment.
- `assets/model_results.json`: calculated results consumed by the deck builder.
- `assets/cluster-centers.png`: patient outcomes and means by center.
- `assets/equations/`: LaTeX source, MathJax SVG and PNG equation fallback.
- `../../slides-common/`: shared course theme, primitives and figure palette.
- `../week1/pptxgenjs_helpers/`: the existing course helper implementation.

The saved hypothetical trial contains six centers with 80 patients each and
three centers per program. The legacy generator assumes the old 24 m treatment
effect and original center labels. Those assumptions no longer match Week 1.
Update its constants and data-generation approach before regenerating or reusing
its numerical results in the current lecture.

### Legacy preview

To inspect the saved starter with its existing assets, run from this directory:

```sh
npm run cluster-starter
```

The current build never runs the legacy generator or appends its old slides.

The repeated-trial discussion uses `#/concepts/randomized-block-design` and `#/concepts/cluster-randomization` in the separate sampling-simulator project. That application uses 20 patients per center and different outcome units, so it illustrates the designs rather than reproducing the lecture numbers.
