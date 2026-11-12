# Beyond MLR: Week 2

The revised lecture has 38 slides and no backup slides. It begins with a course title slide and the Week 1 model-comparison table,
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
| 12 | Traditional interaction plot: centers on the horizontal axis, one line per treatment connecting its sample means; title and plot only. |
| 13 | Grouped fixed-effects model with a center-specific treatment slope, parameter interpretations and enlarged effects-coding constraints. |
| 14 | All 12 fitted regression coefficients: estimates, estimated SEs, 95% confidence intervals and p-values, in the Week 1 table format. |
| 15 | Simple versus marginal treatment effects, links to beta_1 + d_j and beta_1, and the definition of estimated marginal means (EMMs). |
| 16 | Coding explanation alongside predicted treatment means and simple effects, pointwise 95% CIs, and unadjusted p-values for each center. |
| 17 | Full interaction model diagnostics: residuals versus fitted values and a Normal Q-Q plot, closing the case study. |
| 18 | Time for Lab 3, with a generated illustration in Week 1's break-slide style. |
| 19 | Wound-healing debrief: average treatment estimates, estimated SEs, 95% CIs and p-values under additive and interaction models. |
| 20 | Hypothetical unequal laboratory sizes: equally weighted interaction average of 10 percentage points versus the additive average of 13. |
| 21 | Part 2 divider: From included centers to a population. |
| 22 | Population target: new patients in the same centers versus new centers from the same population. |
| 23 | Two-by-two grid of true means: random intercepts versus random intercepts and slopes, for two independent samples of centers. |
| 24 | Random-intercept and random-slope specification, population-average parameters, and jointly distributed center deviations. |
| 25 | Positive and negative covariance illustrated with true center means and treatment effects. |
| 26 | Rehabilitation variance-component estimates, followed by the correlation formula, numerical substitution and interpretation. |
| 27 | Population-average baseline and treatment estimates with estimated SEs, 95% CIs and p-values. |
| 28 | Fixed versus random treatment estimates, estimated SEs and p-values, with different targets of inference. |
| 29 | Time for Lab 4, using the Week 1 collaborative-learning illustration. Lab 4 continues the wound-healing study with random intercepts and slopes. |
| 30 | Post-lab random-intercept-only versus random-intercept-and-slope comparison on the rehabilitation data. |
| 31 | Conceptual variance decomposition and ICC discussion, contrasting the two random-effects structures. |
| 32 | Part 3 divider: Crossed random effects. |
| 33 | From one grouping variable to multiple grouping variables: nested versus crossed, with crossed effects next and nested effects in Week 3. |

| 34 | Radiologist case study: study setting beside a generated radiology illustration. |
| 35 | Exploratory line plot: patients on the horizontal axis and one line per radiologist. |
| 36 | Crossed patient and radiologist random intercepts, with distributions and interpretations. |
| 37 | Estimated variance components and the absolute-agreement ICC for a single measurement. |
| 38 | Closing Questions slide, matching the Week 1 closing style. |

The title and section dividers use the same shared layouts as Week 1. The
population-model section is followed by the opening of Part 3 on crossed effects.
The radiologist case study introduces a crossed random-intercept model and single-measurement absolute agreement. Cluster randomization stays
at the start of Week 3, before observational multilevel analysis. Labs remain separate work.

Current sources and outputs:

- `additive_example.js` and `generate_additive_example.R`: the paired known-means introductory slides and reproducible PNG/SVG table-and-plot assets.
- `allocation_imbalance.js`: four formula-free main slides; the precision-backup helper remains in source but is no longer included in the deck.
- `fixed_interaction.js`: slide content and equation definitions, using the
  shared concept-panel, teaching-table and figure layouts.
- `crossed_case_study.js` and `generate_radiologist_data.R`: the four-slide radiologist example, reproducible data, REML fit and exploratory plot.
- `crossed_intro.js`: Part 3 divider and conceptual introduction to multiple grouping variables.
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

The retained mathematical background conditions on the included centers, with independent
homoscedastic individual errors. With J centers and n patients per treatment per
center, the variance of the average is `2 sigma^2 / (J n)`. Under independently
sampled random treatment effects, the population-target variance also includes
`sigma_d^2 / J`. These are balanced-model sampling variances, distinct from fitted
standard errors and uncertainty in estimated variance components. These derivations
are no longer on slides 23–24, which now establish the random-effects model conceptually. The random-slope
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
standard error”, in teaching copy. The former precision backup and its divider have been removed from the deck.

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
Slide 11 makes the transition from known toy means to estimating unknown group means from patient outcomes, using the shared two-column text/image layout. Slide 12 explores the six-center data using a traditional interaction plot with centers on the horizontal axis and one colored line per treatment connecting its sample means, without individual observations. The slide contains only the title and plot. The first later use of delta_j defines it as the true center-specific effect.

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
coefficient p-values test whether a center's deviation from the average effect is zero.

The coefficient table leads directly into interpretation of treatment effects and their estimation with estimated marginal means. Formal tests comparing fixed- and random-effects model structures are developed in Weeks 3 and 4.

Slide 15 distinguishes simple treatment effects within centers from the marginal
effect averaged equally across centers. It defines estimated marginal means (EMMs)
on first use and links the effects to beta_1 + d_j and beta_1 under the current
coding. Slide 16 explains why coding affects which effects appear directly as
coefficients, then presents predicted means and enhanced-minus-standard contrasts.
No package names or software syntax appear on these slides.

The simple-effect table includes pointwise 95% t confidence intervals and
unadjusted two-sided p-values for each center-specific treatment comparison,
using 108 residual degrees of freedom. Centers A through F have p-values .220,
.011, .118, .960, < .001, and < .001 when rounded for display. The interval and
p-value conventions are explicitly labelled and address each individual
comparison consistently. Lab 3 uses the same convention in its main analysis,
with an optional callout explaining multiplicity and showing Bonferroni-adjusted
tests across all five laboratories using `by = NULL`. The R generator retains
both raw and adjusted simple-effect p-values in `within_center_comparisons.csv`
and `model_results.json`.

Slide 17 closes the case study with residuals versus fitted values and a Normal
Q-Q plot of internally standardized residuals from the full interaction model.
It reuses the Week 1 diagnostics layout with taller plot slots. There is no clear
change in residual spread across fitted values, and the Q-Q points broadly follow
the reference line. Speaker notes explain the vertical stacks, that within-cell
residual centering is automatic in this full factorial model, and that these plots
do not establish independence. The former treatment-contrast plot remains an
available generated asset but is no longer shown on slide 17.

Slide 18 pauses for Lab 3 using the same title-and-illustration layout as Week 1.
The fictional illustration and generation prompt are in `assets/lab3-debrief/`.
Students analyze five laboratories with ten plates per treatment in each, using
`../../labs_2627/data/wound_healing_week2.csv`. The lab itself does not add an
omission-of-interaction exercise.

Slides 19 and 20 finish Part 1 with a lecture debrief of the wound-healing case.
Run `Rscript lectures_2627/week2/generate_lab3_debrief.R` from the repository root
to refit the actual student CSV and update `assets/lab3-debrief/model_results.json`.
The script is included in the full rebuild pipeline and checks both treatment
coefficients against the arithmetic average of observed laboratory differences.
It records the dataset hash and verifies the pooled-variance standard errors.

Slide 19 shows an average experimental-minus-standard effect of 9.28 percentage
points in both models. The additive model gives estimated SE 1.81, 95% CI
5.69 to 12.88, and p = 0.000001587; the interaction model gives estimated SE 1.62,
95% CI 6.07 to 12.49, and p = 0.0000001231. Both p-values display as < .001.
These unadjusted tests concern a zero average effect. The lab presents
unadjusted tests of the treatment difference within each laboratory. Equal cell sizes preserve the average estimate;
omitted interaction increases the estimated residual variation and estimated SE
here, without changing the average estimator's actual sampling variance.

Slide 20 presents the interaction model first, then the additive model under equal
and unequal laboratory sizes. Its explicitly hypothetical two-laboratory example
uses effects of 5/15 percentage points and 20/80 plates, giving 10 points with
equal laboratory weighting and 13 under the additive model. The slide focuses on
coefficient interpretation, not universal bias. Speaker notes retain the common
treatment-proportion condition for these size weights. No allocation sentence or
weighting formula appears on the slide. Part 2 starts on slide 21 and returns to
the rehabilitation centers for population-average uncertainty.

## Introducing the population model

Slides 22–25 introduce the target, center sampling, and model before any new
variance derivation. Slide 22 contrasts repeating the rehabilitation study in its
six centers with sampling new centers from the same population. Speaker notes
retain the dependence of generalization on center selection and assumptions.

Slide 23 uses `generate_random_center_samples.R` and the assets in
`assets/random-center-samples/`. Rows show two independent samples of four
hypothetical centers (A–D and E–H). Columns compare random intercepts with random
intercepts and slopes, reusing baseline draws across columns to isolate the slope
variation. Centers stay on the horizontal axis; the two treatments use the course
teal/orange palette. Every panel has the same outcome scale. These are true means,
without patient sampling noise, not estimates from the rehabilitation dataset.

Seed 20261111 draws eight center intercept deviations (SD 20 m), then slope
deviations (SD 15 m) correlated .25 with the intercepts. The hypothetical population
standard-care mean is 360 m and its average treatment effect is 20 m. The left
column has a constant 20 m gap. The right column has sampled average effects of
18.95 and 16.31 m; the realized deviations are not recentered. The CSV records the
exact true means and deviations. Regeneration is included in `figures:week2` and
the week-specific `random-center-samples` command.

Slide 24 reuses the grouped formula from the fixed-effects section and explicitly
shows the joint Normal distribution of c_j/d_j, its variance-covariance matrix,
and the Normal individual-error distribution. It defines all four variance and
covariance parameters and states independence across centers and between errors
and center effects. The callout distinguishes zero population means from
sum-to-zero constraints on the realized sample deviations.

Slide 25 illustrates positive and negative covariance with two scatterplots.
`generate_random_center_samples.R` resets seed 20261111 and draws 40 hypothetical
center baseline deviations (SD 20 m) and treatment deviations (SD 12 m, population
correlation .75). Standard-care means equal 360 plus the baseline deviation;
treatment effects equal 20 plus the treatment deviation. The second panel
reflects these effects about 20, giving identical empirical spreads and opposite
covariance signs. Axis limits match. `covariance_true_values.csv` records both
sets of true values. These are conceptual illustrations without patient noise,
not estimated center effects. Adding fixed population means to the deviations
does not change their covariance. The notes distinguish covariance from correlation.

The previous variance-expansion and sample-size slides are replaced; their
equation source assets remain available for later development.

## Fitted random-intercept and slope model

The sequence is model specification (24), covariance illustration (25), variance
components (26), population coefficients (27), fixed-versus-random comparison (28),
Time for Lab 4 (29), post-lab random-structure comparison (30), and conceptual
variance decomposition/ICC discussion (31). The separate random-model diagnostics
slide has been removed. Lab 4 now uses the same wound-healing dataset as Lab 3; its guided exercises are in `../../labs_2627/BLR_lab_random_interaction.qmd`.

`fit_random_interaction.R` reads the existing rehabilitation CSV without changing
observations. Outputs are in `assets/random-interaction/`; regeneration follows
`interaction-data` in the figure pipeline. The correlated model is
`distance ~ T + (1 + T | center)`, with 0/1 treatment coding and REML estimation via
`lmerTest`. Both models converge with nonsingular covariance estimates. Package
versions and an input-data MD5 are recorded in `model_results.json`.

The random-slope treatment estimate is 21.265 m, estimated SE 11.42585 m, 95% CI
-8.106 to 50.636 m, and p=.12179. The intercept is 349.865 m with estimated SE
5.03717 m. Coefficient tests and pointwise confidence intervals use Satterthwaite
t inference, with approximately 5 degrees of freedom. These are not Normal-Wald
intervals or Bonferroni-adjusted tests. The method is documented in the
[lmerTest paper](https://www.jstatsoft.org/article/view/v082i13).

Slide 26 presents estimated variances of 86.39395 m² for intercepts, 651.6111 m²
for treatment slopes, and 658.4437 m² for individual errors. The intercept-slope
covariance is 127.0682 m², corresponding to correlation .53555. The table displays only component, parameter and estimate (m²), without an SD
column. Below it, the estimated correlation is calculated as covariance divided
by the square root of the product of the two center variances: 127.1 /
sqrt(86.4 × 651.6), approximately .54. The symbolic formula and numerical
substitution are generated from the fitted results by `correlationEquation` and
the standard equation pipeline. A short interpretation links higher standard-care
means to larger treatment effects. This is a correlation between center deviations,
not an ICC. The title and introductory sentence identify these as fitted
rehabilitation case-study results.
With 0/1 coding all three variances have units m², so their magnitudes can be
compared directly. Treatment-effect variation is much larger than baseline
variation and almost as large as individual residual variation. These comparisons
are not percentages of a common total.

Slide 28 compares the fixed interaction and random-slope models: both treatment
estimates are 21.3 m, with estimated SEs 4.68 and 11.43 m and p-values < .001 and
.122. The targets differ: these six centers versus a population of centers. The
substantial treatment heterogeneity and the sample of only six centers explain
the additional population-average uncertainty. Technical formulas and reference
degrees of freedom remain in the speaker notes.

Slide 29 uses `../week1/assets/lab2-break.png` in the same title-plus-image layout
as the other lab breaks. Lab 4 continues the wound-healing case study with a random-intercept and slope model. The subsequent discussion explicitly returns to the existing
rehabilitation observations.

For slide 30, the additional REML fit `distance ~ T + (1 | center)` gives the same
21.265 m estimate, estimated SE 5.17238 m, 95% CI 11.0176 to 31.5124 m, and
p=.0000748936 (displayed < .001). Satterthwaite treatment df are approximately 113,
compared with 5 under random slopes. `random_intercept_coefficients.csv`,
`random_intercept_summary.txt`, and the nested `random_intercept_model` JSON entry
record the fit. An independent balanced-design check verifies its estimated SE
from its residual variance. The residual variance is about 802.6 m², higher than
658.4 under random slopes, but that increase does not replace the missing shared
center-specific treatment variation. Both random models target a population
average; the key difference is whether treatment effects are assumed common or
allowed to vary. The displayed p-values test treatment coefficients, not the
addition of a random slope.

Slide 31 connects the Week 1 decomposition and ICC to the changed structure. A
random intercept supplies the same shared contribution under both treatments;
a random slope makes that contribution treatment-dependent. Outcome variances
and correlations can depend on treatment, so one common variance breakdown and
one ICC no longer summarize every patient/pair. This remains conceptual, without
formulas, numerical ICCs or percentages.

The partial-pooling and residual-diagnostic source outputs remain available but
are not displayed in the lecture. No lab files are changed by this revision.
Part 3 now follows as slides 32–33; the backup slides have been removed.

## Part 3: multiple grouping variables

`crossed_intro.js` adds the Part 3 divider (33) and introductory slide (34). The
transition starts with center as the single grouping variable in the first two
weeks, even when both an intercept and slope vary across its levels. The two
columns compare wards nested within hospitals with crossed patient/radiologist
groups. Notes identify the observational unit as a patient outcome in the first
example and a radiologist measurement in the second. The callout states that
crossed grouping variables come next, while nested grouping variables are covered
in Week 3. The case study and further crossed-model slides are not yet added.

The course plan keeps cluster randomization at the beginning of Week 3, introducing
a context-level predictor before observational multilevel analysis. No backup
slides remain in this deck.

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
- `interaction-residuals-vs-fitted.png` and `interaction-normal-qq.png`: the two diagnostics displayed on slide 17.
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
4.68 m; 95% confidence interval 11.98 to 30.55 m). Intervals for individual
center contrasts are pointwise. The instructor data-generation output retains
the overall interaction test, F(5, 108) = 5.95, p = 0.0000675, for reference.
Formal model-comparison tests are reserved for Weeks 3 and 4.

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

## Radiologist case study

`generate_radiologist_data.R` reproduces the old Week 2 simulation (seed 123): 10 patients, five radiologists, and one measurement per pair. It fits `measurement ~ 1 + (1 | patient) + (1 | radiologist)` by REML. The estimated variances are 7.4411, 1.4684 and 1.0025 mm², respectively. The single-measurement absolute-agreement ICC is 0.7507. Figures, the CSV and full-precision results are under `assets/radiologists/`. The setting image was generated using the built-in image tool; its prompt is saved in `assets/radiologists/image-prompt.txt`.

## Lab 4: random intercepts and treatment slopes

The lab reuses Lab 3's wound-healing data and displays its interaction plot as background, without asking students to recreate it. The guided sequence covers model specification and fitting, variance components and intercept-slope correlation, the population-average treatment effect with estimated SE, 95% CI and p-value, and conditional residual diagnostics. It does not include model comparisons or conclusion writing. Regenerate the background asset with `Rscript ../../labs_2627/assets/lab4/_generate_background_plot.R` from this directory. The lab is available from the Week 2 menu.
