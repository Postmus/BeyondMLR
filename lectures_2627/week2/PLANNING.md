# Week 2 planning notes

Week 2 develops treatment effects that vary between centers, then briefly introduces crossed random effects. Build directly on the Week 1 multicenter setting. Keep cluster randomization at the start of Week 3 to introduce context-level predictors before observational multilevel analysis, predictors at both levels and cross-level interactions. Nested grouping variables are also covered in Week 3. The current 38-slide lecture contains the completed fixed and random treatment-effect sections, followed by the Part 3 divider and introductory slide on multiple grouping variables. Slides 34–37 now present the radiologist setting, exploratory plot, crossed random-intercept model and estimated variance components with a single-measurement absolute-agreement ICC. Slide 38 closes with Questions in the Week 1 style. Labs 3 and 4 use the wound-healing experiment for fixed interaction and random-intercept/slope analyses, respectively. Lab 4 reuses the rendered exploratory plot, ends with residual diagnostics, and leaves model comparisons to the lecture and conclusion writing to the assignment. The earlier cluster-first starter is preserved in `build_cluster_starter.js`.

## Opening and theoretical emphasis

After the title, begin with the model-comparison table from the final substantive Week 1
slide. Slides 3–6 introduce unequal treatment proportions within centers while
retaining equal overall group sizes and a common treatment effect. Show the
allocation, fitted results, the comparisons used by the three models, and why the
estimated standard errors can agree or differ across designs. Slide 6 now compares
equal center sizes, unequal center sizes with balanced treatment allocation, and
unequal treatment proportions. Explain the same mix of centers and estimation of
individual variation in plain language. Exact estimated-SE equality in the fully
balanced case assumes REML and a positive estimated center variance. The detailed precision discussion for unequal allocation and its backup divider
have been removed from the deck. Use “estimated standard error” throughout the lecture.
Keep the opening formula-free and qualify the extra between-center information
by the random-intercept assumptions.
Begin Part 1 with the known-means two-center additive example on slide 8. Its table includes row and column averages, while its plot places center on the horizontal axis and annotates both 20 m treatment gaps. The matching interaction example on slide 9 changes the gaps to 10/30 m, retains the 20 m average effect, and labels center deviations of -10/+10 m before introducing the six-center dataset.
Then restore balanced allocation before introducing treatment heterogeneity and
the distinction between included-center and population-average uncertainty.

After the toy examples, slide 10 introduces the conceptual choice between fixed center parameters and a population distribution of random center effects. Slide 11 begins the empirical six-center analysis in the usual text-left/image-right layout, explicitly moving from known means to estimation from patient data. Slide 12 shows a traditional interaction plot with centers on the horizontal axis and one line per treatment connecting its observed means, with only a title and plot. Slide 13 then introduces the grouped treatment-slope formulation, parameter definitions and enlarged sum-to-zero constraints. Slide 14 presents all fitted coefficients in the Week 1 estimate/SE/95% CI/p-value format, followed directly by simple versus marginal effects and the definition of estimated marginal means on slide 15. Formal interaction testing is deferred to model-building discussions in Weeks 3 and 4. Lab 3 follows the same progression, moving from manual calculations of simple effects to estimated marginal means and their contrasts. Slide 16 pairs a coding explanation with predicted means, simple effects, pointwise 95% CIs and unadjusted p-values for each center. Lab 3 uses the same main analysis and introduces Bonferroni adjustment in an optional callout. Slide 17 closes the empirical analysis with residuals versus fitted values and a Normal Q-Q plot from the full interaction model. Keep software names and syntax for the lab. Slide 18 pauses for Lab 3 in the Week 1 break-slide style. Finish Part 1 with a wound-healing debrief: the additive/interaction average-effect comparison on slide 19 and a hypothetical unequal-laboratory-size illustration on slide 20. The population-center section begins Part 2 on slide 21.

Keep effects coding for center in the fixed interaction. Explain the consequences
of omitting interaction in plain language: equal cell sizes preserve the average
treatment estimate, while unexplained treatment heterogeneity increases the
estimated residual variation and estimated SE here. Unequal center sizes can
change the additive coefficient's interpretation because larger centers receive
more weight in the fixed-allocation example. Retain its allocation condition in
speaker notes only; do not add weighting formulas to the slide. Then contrast
repeating the study in the same centers with sampling different centers in Part 2.

Part 2 now begins with the population target on slide 22, followed by a 2 × 2
grid on slide 23. Rows show new samples of centers; columns show random intercepts
versus random intercepts and slopes. Plot hypothetical true means, use distinct
center names across rows, retain a common outcome scale, and do not recenter the
sampled treatment deviations. Slide 24 then reuses the grouped model formula and
explains the population-average parameters and joint distribution of center
deviations, explicitly showing the variance-covariance matrix and individual-error
distribution with independence assumptions. Slide 25 illustrates positive and
negative covariance using hypothetical true center means and treatment effects
on identical axes. Defer variance derivations. The empirical sequence then presents
variance components and the calculated baseline/treatment-effect correlation (26),
population-average coefficients (27), and the
fixed-versus-random comparison (28). Add Time for Lab 4 as slide 29, retaining the
usual break-slide style; Lab 4 now continues the wound-healing case study. After the break,
return to the rehabilitation data for a random-intercept-only versus random-slope
comparison (30), then use the conceptual variance-decomposition and ICC slide (31)
to explain the structural difference. Both post-lab models target the population
average; they differ in whether treatment heterogeneity is represented. The
separate lecture diagnostics slide and earlier standalone target/uncertainty
summary are removed. Keep the numerical results and their interpretation together
before the lab, and the random-structure discussion together afterward.

## 1. Treatment effects that vary between centers

Keep the Week 1 randomized block design, with patients assigned to both treatments within each center. Start from the model already established in Week 1: center-specific intercepts and a common treatment effect. There is no need to restart with the treatment-only analysis.

The Week 1 summary explicitly assumes the same explanatory-variable effect across groups (no interaction). Week 2 relaxes that assumption: what if the treatment effect also differs between centers?

### Fixed treatment-by-center interaction

First extend the fixed-effects model to include the treatment-by-center interaction. Use estimated marginal means to interpret the expected outcomes under each treatment within each center, then obtain within-center treatment comparisons. An overall treatment comparison can follow, with explicit attention to how centers are weighted when averaging their treatment effects.

Investigate what happens if the interaction is omitted while retaining center intercepts. Distinguish what the fitted common treatment effect represents, whether its estimated uncertainty is appropriate, and the scientific information lost by summarizing heterogeneous treatment effects with one average. Do not assume that omitting an interaction necessarily biases every possible average-effect estimator.

This gives interactions and estimated marginal means a clear purpose. A separate section on factorial designs is no longer needed. The estimated-marginal-means text removed from Week 1 is retained in [the separate note](../../labs_2627/notes/estimated_marginal_means.qmd).

### Random intercept and random treatment slope

Then view centers as examples from a wider population and model variation in both their intercepts and their treatment effects. Introduce this as a **random treatment effect, also called a random slope**. With binary treatment, the slope is the difference between the two treatments.

Use the random-intercept/random-slope formulation as the working direction:

```r
lmer(outcome ~ treatment + (1 + treatment | center), data = dat)
```

This closely parallels the fixed-effects interaction model and establishes terminology and syntax that return in Week 4. Retain dummy coding for treatment and effects coding for center from Week 1. In the fixed-effects interaction model, the treatment coefficient describes the equally weighted average treatment effect across the included centers, and interaction coefficients describe center-specific deviations from that average. In the random-effects model, the fixed treatment coefficient describes the population-average treatment effect.

The old replicated-block lab instead used:

```r
lmer(outcome ~ treatment + (1 | center) + (1 | treatment:center), data = dat)
```

Both allow treatment effects to vary between centers, but their covariance assumptions differ. The old formulation has an independent shared center contribution and independent treatment-by-center contributions with a common interaction variance. The random-slope formulation allows a more flexible variance-covariance structure. Changing formulation is therefore a deliberate modeling change, not merely a syntax change.

For the two-treatment example, explain the intercept variance, treatment-effect variance, intercept-slope covariance, and individual residual variance. With 0/1 coding, the intercept refers to control outcomes. Do not carry forward the old lab's "interaction ICC" terminology: a variance fraction is not automatically a pairwise correlation. With random slopes, within-center correlation can differ for two control patients, two experimental-treatment patients, and a pair receiving different treatments. There is no single common ICC to report.

For a single teaching dataset, draw one intercept/slope pair per center from a bivariate Normal distribution, then generate patient outcomes with independent individual errors. This aligns the generating assumptions with the fitted model. It does not require a repeated-sampling demonstration like the Week 1 simulation.

The lecture dataset is now generated by `generate_interaction_data.R`, with patient data, known generating effects, fixed-model results and figures in `assets/fixed-interaction/`. See [README.md](README.md) for parameters and interpretation. Lab 3 uses the separate wound-healing dataset in `labs_2627/data/wound_healing_week2.csv`; `generate_lab3_debrief.R` fits its models for the post-lab discussion.

## 2. Multiple grouping variables and crossed random effects

Part 3 starts on slide 32 with the crossed-random-effects section divider. Slide 33
begins from one grouping variable, center, and asks what changes with two or more.
Keep the definitions specific to grouping variables. A random intercept and slope
still involve one grouping variable, not two.

- Nested: each group at one level belongs to exactly one group at the next level.
  Patient outcomes grouped by ward and hospital provide the example, with each
  ward belonging to one hospital.
- Crossed: neither grouping variable is contained within the other. Measurements
  are grouped by patient and radiologist; patients are assessed by several
  radiologists and radiologists assess several patients.

The slide explicitly points ahead to crossed effects next and nested grouping
variables in Week 3. Slides 34–37 reuse the old simulated radiologist-agreement
example: a two-column setting slide with a generated image, a plot of measurements
by patient and radiologist, the crossed random-intercept specification, and the
estimated variance components with an absolute-agreement ICC for one measurement.
Keep this a brief demonstration without a fixed-effects or interaction detour.

## Week 3 opening: treatment assigned to whole centers

Retain cluster randomization at the start of Week 3. Treatment assigned to whole
centers supplies a clean first example of a context-level predictor, before
observational data with predictors at both individual and context levels and
possible cross-level interactions. Whole-center assignment removes the
within-center treatment contrast, while random assignment still supports
estimation of the average treatment effect through between-center comparisons.
Then introduce nested grouping structures within the Week 3 sequence.

## Four model structures across the course

| Structure | Main idea | Example syntax | Main introduction |
| --- | --- | --- | --- |
| Random intercept | Groups differ in their expected outcome, with a common explanatory-variable effect. | `(1 | center)` | Week 1 |
| Random intercept and slope | Groups also differ in the effect of an explanatory variable. | `(1 + treatment | center)` | Week 2; revisited for time in Week 4 |
| Nested random effects | Lower-level groups belong to higher-level groups. | `(1 | district/neighborhood)` | Week 3 |
| Crossed random effects | Observations belong to two grouping structures that are not nested. | `(1 | participant) + (1 | stimulus)` | Week 2, Part 3 |

These are four useful teaching examples, not four mutually exclusive model classes. Intercept versus slope describes **what varies between groups**; nested versus crossed describes **how grouping structures relate**. They can be combined, for example by including random slopes in a nested model.

## Crossed sources of variation: end of Week 2

Use the radiologist case study to introduce random intercepts for patients and
radiologists, two grouping variables that are not nested. Each radiologist
assesses multiple patients, and each patient is assessed by multiple radiologists.
This extends the first two weeks from one grouping variable to multiple shared
sources of variation. Connect the later variance decomposition to agreement.

## Connection to the other weeks

- Week 1 establishes randomized block designs, fixed and random center effects, and a common treatment effect.
- Week 2 keeps the randomized block setting and introduces treatment-by-center interaction, first as fixed effects with estimated marginal means and then as random intercepts and slopes. It ends with a brief crossed-random-effects section.
- Week 3 opens with cluster randomization to introduce a context-level predictor. It then develops observational multilevel data, predictors at both levels, cross-level interactions and nested grouping variables.
- Week 4 covers longitudinal data and returns to random intercepts and slopes, now with time as the explanatory variable: `(1 + time | person)`. The structure is familiar from Week 2, while the interpretation becomes individual differences in starting outcomes and changes over time.

When developing examples, distinguish bias, actual sampling variation and estimated uncertainty. The Week 1 balanced within-center treatment contrast cancels a shared center intercept; dependence alone does not imply that ignoring grouping understates the standard error. Revisit the consequences as the model or design changes.

## Existing material to revisit

- [Old Week 2 lecture](../../lectures_2526/BLR_Week_2_reveal.qmd): interactions and replicated randomized block designs. Omit the standalone factorial-design section and retain a brief crossed-random-effects section in Week 2.
- [Old replicated-block lab](../../labs_2526/BLR_lab_replicated_randomized_block.qmd): interaction model, estimated marginal means, variance components and omission of the random interaction. Revise its random-effects specification and ICC interpretation as described above.
- [Old Week 3 lecture](../../lectures_2526/BLR_Week_3_reveal.qmd): retain its cluster-randomization opening in Week 3, followed by observational and nested-model material.
- [Revised Week 1 lecture notes](../week1/README.md): starting model, case study and simulation approach.
