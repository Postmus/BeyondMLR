# Beyond MLR: Week 2

The revised lecture has 37 slides, empty speaker notes and no backup slides. It begins with a course title slide and the Week 1 model-comparison table,
then examines unequal treatment allocation in three formula-free slides before
introducing treatment-by-center interaction with dummy coding for treatment
and effects coding for center. It compares additive and interaction models for the average treatment effect,
then introduces uncertainty about a population average.
See [PLANNING.md](PLANNING.md) for the rest of the proposed lecture.

## Current lecture sequence

| Slides | Content |
| --- | --- |
| 1 | Title: When treatment effects vary. Subtitle: Interactions, random slopes and crossed random effects. |
| 2 | Week 1 recap: rehabilitation trial setting beside its closing comparison table. |
| 3–4 | Unequal treatment allocation within centers and the fitted three-model comparison. |
| 5 | Within- versus between-center comparisons, the random-intercept assumptions, and a transition to treatment effects that vary across centers. |
| 6 | Known-means 2 × 2 additive example: marginal averages, parallel treatment lines, equal treatment-gap annotations and the individual-outcome model. |
| 7 | Matching known-means interaction example: treatment gaps of 10/30 m, average effect of 20 m, center deviations of -10/+10 m and the interaction model. |
| 8 | Conceptual comparison: fixed center parameters versus a population distribution of random center effects. |
| 9 | Part 1 divider: Treatment-by-center interaction: fixed effects. |
| 10 | Two-column empirical transition: trial setting and estimation task on the left, generated CPAP image on the right. |
| 11 | Traditional interaction plot: hospitals on the horizontal axis, one line per treatment connecting its sample means; title and plot only. |
| 12 | Grouped fixed-effects model with a center-specific treatment slope, parameter interpretations and enlarged effects-coding constraints. |
| 13 | All 12 fitted regression coefficients: estimates, estimated SEs, 95% confidence intervals and p-values, in the Week 1 table format. |
| 14 | Simple versus marginal treatment effects, links to beta_1 + d_j and beta_1, and the definition of estimated marginal means (EMMs). |
| 15 | Coding explanation alongside predicted treatment means and simple effects, pointwise 95% CIs, and unadjusted p-values for each center. |
| 16 | Full interaction model diagnostics: residuals versus fitted values and a Normal Q-Q plot, closing the case study. |
| 17 | Time for Lab 3, with a generated illustration in Week 1's break-slide style. |
| 18 | Wound-healing debrief: average treatment estimates, estimated SEs, 95% CIs and p-values under additive and interaction models. |
| 19 | Hypothetical unequal laboratory sizes: equally weighted interaction average of 10 percentage points versus the additive average of 13. |
| 20 | Part 2 divider: Treatment-by-center interaction: random effects. |
| 21 | Population target: new patients in the same centers versus new centers from the same population. |
| 22 | Two-by-two grid of true means: random intercepts versus random intercepts and slopes, for two independent samples of centers. |
| 23 | Random-intercept and random-slope specification, population-average parameters, and jointly distributed center deviations. |
| 24 | Positive and negative covariance illustrated with true center means and treatment effects. |
| 25 | CPAP variance-component estimates, followed by the correlation formula, numerical substitution and interpretation. |
| 26 | Population-average control reduction and CPAP effect estimates with estimated SEs, 95% CIs and p-values. |
| 27 | Fixed versus random treatment estimates, estimated SEs and p-values, with different targets of inference. |
| 28 | Random-intercept-only versus random-intercept-and-slope comparison on the CPAP data, motivating the model used in Lab 4. |
| 29 | Time for Lab 4, using the Week 1 collaborative-learning illustration. Lab 4 continues the wound-healing study with random intercepts and slopes. |
| 30 | Conceptual variance decomposition and ICC discussion, contrasting the two random-effects structures. |
| 31 | Part 3 divider: Crossed random effects. |
| 32 | From one grouping variable to multiple grouping variables: nested versus crossed, with crossed effects next and nested effects in Week 3. |
| 33 | Radiologist case study: study setting beside a generated radiology illustration. |
| 34 | Exploratory line plot: patients on the horizontal axis and one line per radiologist. |
| 35 | Crossed patient and radiologist random intercepts, with distributions and interpretations. |
| 36 | Estimated variance components and the absolute-agreement ICC for a single measurement. |
| 37 | Closing Questions slide, matching the Week 1 closing style. |

The title and section dividers use the same shared layouts as Week 1. The
population-model section is followed by the opening of Part 3 on crossed effects.
The radiologist case study introduces a crossed random-intercept model and single-measurement absolute agreement. Cluster randomization stays
at the start of Week 3, before observational multilevel analysis. Labs remain separate work.

Current sources and outputs:

- `additive_example.js` and `generate_additive_example.R`: the paired known-means introductory slides and reproducible PNG/SVG table-and-plot assets.
- `allocation_imbalance.js`: three formula-free main slides; the precision-backup helper remains in source but is no longer included in the deck.
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

- Slides 2–7 retain rehabilitation for the Week 1 recap, allocation discussion and
  known-means introductory examples. Slide 10 explicitly introduces the new CPAP case.
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

## Introductory rehabilitation and separate teaching cases

`explore_allocation_imbalance.R` reproduces the Week 1 generating model while changing
standard/enhanced allocations to 5/15, 8/12, 10/10, 10/10, 12/8 and 15/5. Its treatment-only,
fixed-center and random-intercept estimates remain 3.87, 8.40 and 8.13 m.
`generate_additive_example.R` retains the known-means two-center illustrations:
350/370 and 390/410 m for the additive case, and 350/360 and 390/420 m with interaction.
`generate_lab3_debrief.R` maintains the separate wound-healing comparisons.
`generate_radiologist_data.R` maintains the crossed patient/radiologist example.
These assets are independent of the new CPAP simulation.
