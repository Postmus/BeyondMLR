# Beyond MLR: Week 2

The lecture has 40 slides, empty speaker notes and no backup slides. The introduction
revisits Assignment 1's protein-extraction experiment: setting, additive model,
three-model results and a feedback placeholder. A hypothetical allocation change
then motivates the within- and between-donor comparisons, followed by a known-means
interaction example. Parts 1 and 2 use the simulated HIPARCO-inspired CPAP case.
See [PLANNING.md](PLANNING.md) for the teaching sequence.

## Current lecture sequence

| Slides | Content |
| --- | --- |
| 1 | Title: When treatment effects vary. Subtitle: Interactions, random slopes and crossed random effects. |
| 2 | Assignment 1 experiment: donor tissue aliquots randomized to Standard or Modified protocols, with a generated laboratory image. |
| 3 | Known-means additive example: two donors, a common protocol effect of 6 µg/mg, row and column averages, parallel lines and the individual-outcome model. |
| 4 | Full-width model comparison using the original Assignment 1 data: estimates, estimated SEs, 95% CIs and p-values. |
| 5 | Explicit placeholder for two or three feedback observations after reviewing student submissions. |
| 6 | Assignment 1 allocation beside a hypothetical unequal allocation within the same ten donors. |
| 7 | Three-model results for the hypothetical allocation. |
| 8 | Three columns of concise bullets explaining protocol-only, fixed-donor and random-donor comparisons, leading into interaction. |
| 9 | Known-means interaction example: donor effects of 2/10 µg/mg, average effect 6, and deviations of −4/+4 µg/mg. |
| 10 | Conceptual choice between fixed group parameters and a population distribution of random group effects; transition from donors to hospitals. |
| 11 | Part 1 divider: Treatment-by-center interaction: fixed effects. |
| 12 | Two-column empirical transition: trial setting and estimation task on the left, generated CPAP image on the right. |
| 13 | Traditional interaction plot: hospitals on the horizontal axis, one line per treatment connecting its sample means; title and plot only. |
| 14 | Grouped fixed-effects model with a center-specific treatment slope, parameter interpretations and enlarged effects-coding constraints. |
| 15 | All 12 fitted regression coefficients: estimates, estimated SEs, 95% confidence intervals and p-values, in the Week 1 table format. |
| 16 | Estimated marginal means: treatment-by-hospital grid, fitted-model equation for each cell, and equal-weight averaging across the six hospitals. |
| 17 | Marginal and simple treatment effects as comparisons of estimated means, with effects-coded coefficients explained in the text. |
| 18 | Full-width table of estimated treatment means and simple effects, pointwise 95% CIs and unadjusted p-values. |
| 19 | Full interaction model diagnostics: residuals versus fitted values and a Normal Q-Q plot, closing the case study. |
| 20 | Time for Lab 3, with a generated illustration in Week 1's break-slide style. |
| 21 | Wound-healing debrief: average treatment estimates, estimated SEs, 95% CIs and p-values under additive and interaction models. |
| 22 | Hypothetical unequal laboratory sizes: equally weighted interaction average of 10 percentage points versus the additive average of 13. |
| 23 | Part 2 divider: Treatment-by-center interaction: random effects. |
| 24 | Population target: new patients in the same centers versus new centers from the same population. |
| 25 | Two-by-two grid of true means: random intercepts versus random intercepts and slopes, for two independent samples of centers. |
| 26 | Random-intercept and random-slope specification, population-average parameters, and jointly distributed center deviations. |
| 27 | Positive and negative covariance illustrated with true center means and treatment effects. |
| 28 | CPAP variance-component estimates, followed by the correlation formula, numerical substitution and interpretation. |
| 29 | Population-average control reduction and CPAP effect estimates with estimated SEs, 95% CIs and p-values. |
| 30 | Fixed versus random treatment estimates, estimated SEs and p-values, with different targets of inference. |
| 31 | Random-intercept-only versus random-intercept-and-slope comparison on the CPAP data, motivating the model used in Lab 4. |
| 32 | Time for Lab 4, using the Week 1 collaborative-learning illustration. Lab 4 continues the wound-healing study with random intercepts and slopes. |
| 33 | Conceptual variance decomposition and ICC discussion, contrasting the two random-effects structures. |
| 34 | Part 3 divider: Crossed random effects. |
| 35 | From one grouping variable to multiple grouping variables: nested versus crossed, with crossed effects next and nested effects in Week 3. |
| 36 | Radiologist case study: study setting beside a generated radiology illustration. |
| 37 | Exploratory line plot: patients on the horizontal axis and one line per radiologist. |
| 38 | Crossed patient and radiologist random intercepts, with distributions and interpretations. |
| 39 | Estimated variance components and the absolute-agreement ICC for a single measurement. |
| 40 | Closing Questions slide, matching the Week 1 closing style. |

The title and section dividers use the same shared layouts as Week 1. The
population-model section is followed by the opening of Part 3 on crossed effects.
The radiologist case study introduces a crossed random-intercept model and single-measurement absolute agreement. Cluster randomization stays
at the start of Week 3, before observational multilevel analysis. Labs remain separate work.

Current sources and outputs:

- `assignment_recap.js`: experiment setting, additive illustration, original-data results and feedback placeholder.
- `additive_example.js` and `generate_additive_example.R`: the paired known-means introductory slides and reproducible PNG/SVG table-and-plot assets.
- `allocation_imbalance.js`: three formula-free main slides; the precision-backup helper remains in source but is no longer included in the deck.
- `fixed_interaction.js`: slide content and equation definitions, using the
  shared concept-panel, teaching-table and figure layouts. The EMM concept uses a full-width reference grid above the prediction and averaging panels.
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

## HIPARCO-inspired simulated case study

The empirical analyses in Parts 1 and 2 use **simulated patient data** inspired by
[Martínez-García et al., JAMA 2013;310:2407–2415](https://doi.org/10.1001/jama.2013.281250).
HIPARCO studied CPAP in patients with obstructive sleep apnea and resistant hypertension.
Its 194 patients were recruited at 24 hospitals and followed for 12 weeks.
Our teaching dataset has its own six-hospital design, patient records and assumed
between-hospital heterogeneity.

Both groups continue usual antihypertensive medication. The CPAP group additionally
uses continuous positive airway pressure through a mask during sleep.
The outcome is **reduction in 24-hour mean blood pressure from baseline to 12 weeks,
in mmHg**. Positive values indicate lower blood pressure at follow-up; a positive
CPAP-minus-control contrast indicates greater reduction with CPAP.

The active dataset is [`assets/fixed-interaction/hiparco_simulated.csv`](assets/fixed-interaction/hiparco_simulated.csv).
Its columns are `patient` (unique synthetic ID), `center` (hospital A–F), `treatment`
(Control or CPAP), and `bp_reduction` (mmHg). Each hospital allocates equally to both
treatments, while its total sample size differs:

| Hospital | Control | CPAP | Total |
| --- | ---: | ---: | ---: |
| A | 12 | 12 | 24 |
| B | 16 | 16 | 32 |
| C | 18 | 18 | 36 |
| D | 22 | 22 | 44 |
| E | 24 | 24 | 48 |
| F | 28 | 28 | 56 |
| Total | 120 | 120 | 240 |

`generate_interaction_data.R` uses seed 20261113, a population control reduction of
2 mmHg, an average additional CPAP reduction of 3.1 mmHg, hospital intercept and
slope SDs of 4 mmHg, intercept/slope correlation 0.5, and individual-error SD 8 mmHg.
It draws one bivariate Normal intercept/slope pair per hospital and independent
Normal individual errors. Realized hospital deviations retain their sampled means.
Outcomes are rounded to one decimal place. The source paper motivates the treatment
comparison, follow-up, outcome and approximate average effect; the heterogeneity
and allocation are explicit teaching assumptions.

### Fixed-effects analysis

The full model is `bp_reduction ~ treatment * center`, with Control as reference
and effects coding for hospital. Its treatment coefficient is the equally weighted
average of the six hospital-specific CPAP effects, including when hospital sizes differ.
The generator exports all 12 regression coefficients, fitted cell means, simple
CPAP contrasts, pointwise 95% confidence intervals, unadjusted p-values, and diagnostics.
The residual degrees of freedom are 228. The simple-effect results are checked
against direct cell means and the model covariance matrix.

The additive-model coefficient is the patient-count-weighted average of the
within-hospital contrasts because treatment allocation is 50:50 in every hospital.
Thus the average coefficient can change when interaction is omitted in this case.
The wound-healing Lab 3 debrief retains its own equal-size laboratory example.

### Random-effects analysis

`fit_random_interaction.R` reads the same CSV and fits correlated hospital intercepts
and CPAP slopes by REML. Its fixed-effect tests and confidence intervals use
Satterthwaite degrees of freedom. Both the random-intercept-only and random-slope
fits converge without singularity. Independent GLS and conditional-mean checks
use each hospital's own sample size and sampling covariance.

| Analysis | CPAP effect (mmHg) | Estimated SE | 95% CI | p-value |
| --- | ---: | ---: | --- | ---: |
| Fixed interaction | 3.821 | 1.055 | 1.742 to 5.900 | 0.000361 |
| Random intercept and slope | 3.994 | 2.122 | −1.413 to 9.401 | 0.116831 |
| Random intercept only | 4.080 | 1.053 | 2.006 to 6.154 | 0.000138 |

Unequal hospital sizes give different precision to hospital comparisons. The
fixed-interaction coefficient equally averages included-hospital effects, while
the random-slope model uses the modeled covariance and differing precision to
estimate a population average. Both the estimates and their estimated uncertainty
can therefore differ. The random-intercept-only model assumes a common effect.

Estimated control-mean variance is 26.192, treatment-slope variance 20.544,
control-mean/slope covariance 11.219, and residual variance 61.837, all in mmHg².
The estimated intercept/slope correlation is 0.484. Treatment-slope variance is
comparable to control-mean variance. Generalizing across hospitals adds uncertainty
to the population-average CPAP effect.

### Figures and teaching continuity

- Slides 2–9 use Assignment 1 protein extraction for the recap, allocation discussion
  and known-means examples. Slide 12 introduces the CPAP case.
- The empirical interaction plot shows treatment means across hospitals, with a
  title and plot only. The two residual plots close the fixed-effects case study.
- The Part 2 hypothetical sampling and covariance illustrations use CPAP/control
  blood-pressure reductions on a consistent mmHg scale.
- Labs 3 and 4 and their break/debrief slides retain the wound-healing setting.
- The crossed-effects section retains the radiologist case study.
- Speaker notes remain empty throughout.

The CPAP scene was generated with the built-in image-generation tool and saved as
`assets/fixed-interaction/cpap-case-study.png`. Its exact prompt and provenance are
in `assets/fixed-interaction/cpap-case-study.prompt.txt`.

### Rebuilding and checks

Run the existing `npm run rebuild:week2` pipeline to regenerate all numeric assets,
equations, PowerPoint and PDF. The fixed-model generator checks allocation, effects
coding, contrasts, reference-coding invariance and the weighted additive estimate.
The random-model generator checks convergence, singularity, independent GLS means,
conditional predictions and the random-intercept weighted contrast.
The OOXML validator and a read-only native PowerPoint open verify the delivery file;
rendered changed slides are inspected for layout and classroom readability.

## Assignment 1 introduction and separate teaching cases

`explore_allocation_imbalance.R` reads the original Assignment 1 CSV at
`labs_2627/assignments/week1/downloads/protein_yield.csv`. There are ten donors,
eight aliquots per donor and four aliquots per protocol within every donor.
The lecture fits protocol-only OLS, fixed-donor OLS with effects coding, and a
random-donor-intercept model by REML. Random-model inference uses Satterthwaite
degrees of freedom, matching the Week 1 lab.

| Model | Estimate (µg/mg) | Estimated SE | 95% CI | p-value |
| --- | ---: | ---: | --- | ---: |
| Protocol only | 4.6135 | 2.89437 | −1.1488 to 10.3758 | 0.114991 |
| Fixed donor effects | 4.6135 | 1.76004 | 1.1023 to 8.1247 | 0.010769 |
| Random donor effects | 4.6135 | 1.76004 | 1.1023 to 8.1247 | 0.010769 |

For the explicitly hypothetical allocation comparison, Standard counts are
1, 2, 3, 3, 4, 4, 5, 5, 6 and 7 in donor order. Modified counts complete each
donor's total of eight. Each protocol still has 40 aliquots overall. The generator
replays seed 262701 and verifies exact agreement with the original balanced data,
then changes allocation while preserving the donor contributions and individual
errors. The true protocol effect remains 6 µg/mg. Results and the hypothetical CSV
are in `assets/allocation-imbalance/`; the assignment source data remain unchanged.
The hypothetical protocol-only, fixed-donor and random-donor estimates are 5.6535,
4.4395 and 4.5320 µg/mg, with estimated SEs 2.8984, 1.9523 and 1.9382.

`generate_additive_example.R` draws hypothetical true means for donors A and B:
50/56 and 70/76 µg/mg for the additive case; 50/52 and 70/80 µg/mg for interaction.
Both examples have an equally weighted average protocol effect of 6 µg/mg.
`µ_tj` identifies the true cell means. These figures illustrate the model assumptions.

The laboratory scene was generated with the built-in image-generation tool and
saved to `assets/assignment1-recap/protein-extraction.png`; its exact prompt is in
`assets/assignment1-recap/protein-extraction.prompt.txt`. It matches the square
image frame in the usual two-column setting slide.

Slide 5 is a feedback placeholder. Replace its text with observations from the
submitted reports after reviewing them.

`generate_lab3_debrief.R` maintains the separate wound-healing comparisons, and
`generate_radiologist_data.R` maintains the crossed patient/radiologist example.
These assets are independent of the introductory and CPAP examples.
