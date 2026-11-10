# Beyond MLR: Week 2

The revised opening has 24 main slides and two backup-section slides. It begins with a course title slide and the Week 1 model-comparison table,
then examines unequal treatment allocation in four formula-free slides before
introducing treatment-by-center interaction with dummy coding for treatment
and effects coding for center. It derives uncertainty for the average in the
included centers and contrasts it with uncertainty about a population average.
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
| 8–10 | Restore balanced allocation, introduce new rehabilitation outcomes, the fixed interaction model and effects coding. |
| 11–12 | Fitted cell means and center-specific treatment contrasts. |
| 13 | Part 2 divider: Uncertainty in the included centers. |
| 14–17 | Average estimand, cancellation within centers, and the variance of individual-center and average contrasts. |
| 18–19 | Estimated standard error and confidence interval; consequences of omitting the fixed interaction. |
| 20 | Part 3 divider: From included centers to a population. |
| 21–24 | Repeating the study in different centers, the extra random-slope variance term, and implications for patient and center sample sizes. |
| 25–26 | Backup divider and the explanation of why the estimated standard errors are almost identical in the unequal-allocation example. |

The title and section dividers use the same shared layouts as Week 1. The
last four main slides are a theoretical bridge; fitting random slopes and the later
cluster-randomization section remain to be developed. Labs remain separate work.

Current sources and outputs:

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
No interaction is introduced until balanced allocation is restored in slide 8.

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
standard error”, in teaching copy. The former slide 6 is retained as backup slide 26.

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
  enhanced-minus-standard contrasts, with pointwise 95% confidence intervals.
- `model_results.json`: generating parameters, model results, interaction test
  and R session information.
- `interaction-outcomes.png`: patient observations and connected treatment means
  in six center panels on a common outcome scale.
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
