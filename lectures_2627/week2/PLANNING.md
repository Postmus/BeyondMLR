# Week 2 planning notes

Week 2 develops treatment effects that vary between centers, then briefly introduces crossed random effects. Use Assignment 1 protein extraction for the recap and introductory examples, then introduce a HIPARCO-inspired CPAP case for Parts 1 and 2. Keep cluster randomization at the start of Week 3 to introduce context-level predictors before observational multilevel analysis, predictors at both levels and cross-level interactions. Nested grouping variables are also covered in Week 3. The current 40-slide lecture contains the completed fixed and random treatment-effect sections, followed by the Part 3 divider and introductory slide on multiple grouping variables. Slides 36–39 now present the radiologist setting, exploratory plot, crossed random-intercept model and estimated variance components with a single-measurement absolute-agreement ICC. Slide 40 closes with Questions in the Week 1 style. Labs 3 and 4 use the wound-healing experiment for fixed interaction and random-intercept/slope analyses, respectively. Lab 4 reuses the rendered exploratory plot, ends with residual diagnostics, and leaves model comparisons to the lecture and conclusion writing to the assignment. The earlier cluster-first starter is preserved in `build_cluster_starter.js`.

## Opening and theoretical emphasis

After the title, slide 2 recalls Assignment 1's protein-extraction experiment,
with study details on the left and a laboratory image on the right. Slide 3 shows
the requested EDA plot: aliquot observations, donor–protocol means and overall
protocol means. Slide 4 retains the observed group means and overlays two fitted
random-intercept profiles, separated by the common estimated effect of 4.61 µg/mg.
It replaces the hypothetical additive table and figure with the fitted assignment
model, with the annotated model equation and protocol coding beneath the plot.
The two plots use the same donor order, protocol colours and outcome scale.

Slides 5–7 present the requested random-intercept results: protocol effect and
confidence interval, variance components/ICC, and conditional-residual diagnostics.
A full-width band labelled “From the submissions” places one specific feedback
observation beneath each worked example on slides 4–7. The model slide recognises
correct explanations of donor baselines and the common protocol effect; the results slide recognises correct
estimation and use of confidence intervals in most reports; the variance-components
slide distinguishes the donor-specific shift from its variance; the diagnostic slide
qualifies the conclusions.
Slide 8 gives Quarto feedback in stacked sections with a short positive assessment
and practical points to consider. Keep all speaker notes empty.

The recap leads directly to paired hypothetical additive and interaction plots:
donor effects of 6 and 6 µg/mg on the left, and 2 and 10 µg/mg on the right.
Use the same baselines, colours and axes, with labelled vertical gaps. Model
equations beneath each plot highlight the additional donor-specific treatment
term in the interaction model. Both examples average to 6 µg/mg.
Its title asks what happens when the protocol difference varies between donors,
linking back to the common protocol effect on slide 4. Use “estimated standard
error” throughout. The allocation patterns, three-model results and explanation
of their comparisons are retained in a separate three-slide allocation supplement.

The CPAP case has 240 patients in six hospitals of sizes 24, 32, 36, 44, 48 and 56. Treatment allocation is 50:50 within each hospital. The outcome is reduction in 24-hour mean blood pressure after 12 weeks. This retains a simple within-hospital comparison while allowing unequal precision across hospitals.

After the interaction example, slide 10 focuses on the donor-specific protocol-effect deviations dⱼ: estimate a separate fixed deviation for each included donor, or model the deviations as draws from a population distribution. It contrasts the corresponding interpretations of β₁ and transitions to a multicenter clinical trial. The Part 1 divider on slide 11 then introduces the fixed-effects analysis. Slide 12 introduces the simulated CPAP study and begins the empirical six-hospital analysis in the usual text-left/image-right layout, explicitly moving from known means to estimation from patient data. Slide 13 shows a traditional interaction plot with centers on the horizontal axis and one line per treatment connecting its observed means, with only a title and plot. Slide 14 then introduces the grouped treatment-slope formulation, parameter definitions and enlarged sum-to-zero constraints. Slide 15 presents all fitted coefficients in the Week 1 estimate/SE/95% CI/p-value format, followed by the definition of estimated marginal means on slide 16. Show the treatment-by-hospital grid, predict each cell mean using the fitted model, and average equally across the six hospitals. Slide 17 then compares marginal and simple treatment effects, with their effects-coded coefficient interpretations integrated into the text. A bottom callout explains that simple treatment effects are especially relevant with significant interaction, while the average effect may still be of primary interest. Slide 18 restores the joint F-test of all hospital-specific treatment deviations: compare additive and interaction models using their residual sums of squares and degrees of freedom. The CPAP result is F(5, 228) = 4.55, p < .001, motivating examination of simple effects alongside the average effect. Lab 3 follows the same progression, moving from manual calculations of simple effects to estimated marginal means and their contrasts. Slide 19, “Estimated marginal means and treatment effects”, presents the hospital-specific means and simple effects above a separate single-row table of marginal means and the average treatment effect. Give all hospitals equal weight and align the columns of both tables. Keep the units, pointwise 95% CI convention and unadjusted p-values in a small footer. Lab 3 uses the same main analysis and introduces Bonferroni adjustment in an optional callout. Slide 20 closes the empirical analysis with residuals versus fitted values and a Normal Q-Q plot from the full interaction model. Keep software names and syntax for the lab. Slide 21 pauses for Lab 3 in the Week 1 break-slide style. Finish Part 1 with a wound-healing debrief: the additive/interaction average-effect comparison on slide 22. Its callout explains that omitting the interaction increases the estimated standard error in this balanced study, and that unequal laboratory sizes can also change the estimate. The separate unequal-size example is removed. The population-center section begins Part 2 on slide 23.

Keep effects coding for center in the fixed interaction. Explain the consequences
of omitting interaction in plain language: equal cell sizes preserve the average
treatment estimate, while unexplained treatment heterogeneity increases the
estimated residual variation and estimated SE here. Unequal center sizes can
change the additive coefficient's interpretation because larger centers receive
more weight in the fixed-allocation example, which assumes a common treatment
proportion across centers. Keep speaker notes empty and explain the example verbally. Then contrast
repeating the study in the same centers with sampling different centers in Part 2.

Part 2 begins with the population target on slide 24. Slide 25 shows the joint
population distribution of intercept and treatment-effect deviations on the left,
and true control/CPAP means for two independent samples of four hospitals on the
right. Use the same population for both samples, and hospital labels A–D within
each sample. Keep the two outcome axes identical. The callout connects each
hospital's control mean and treatment effect to its drawn deviations, without
introducing sample averages. Slide 26 then reuses the grouped model formula and
explains the population-average parameters and joint distribution of center
deviations. Its callout describes zero population means and the role of the
variances and covariance, without discussing sample averages. The slide explicitly shows the variance-covariance matrix and individual-error
distribution with independence assumptions. Slide 27 illustrates positive and
negative covariance using hypothetical true center means and treatment effects
on identical axes. Defer variance derivations. The empirical sequence then presents
variance components and the calculated control-mean/treatment-effect correlation (28),
population-average coefficients (29), and the
fixed-versus-random comparison (30). Explain that unequal hospital sizes allow their point estimates to differ, and population generalization adds uncertainty from hospital sampling. Follow with the CPAP random-intercept-only versus random-slope
comparison (31) to motivate the model used in Lab 4. Add Time for Lab 4 as slide 32,
retaining the usual break-slide style; Lab 4 continues the wound-healing case study.
After the lab, use the conceptual variance-decomposition and ICC slide (33)
to explain the structural difference. Both models in the comparison target the population
average; they differ in whether treatment heterogeneity is represented. The
separate lecture diagnostics slide and earlier standalone target/uncertainty
summary are removed. Keep the numerical results and their interpretation together
before the lab, and the random-structure discussion together afterward.

## 1. Treatment effects that vary between centers

Revisit the randomized block design through Assignment 1, with aliquots assigned to both protocols within each donor. Start from group-specific intercepts and a common protocol effect, compare the three fitted models, and then extend the argument to treatment effects across clinical centers.

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

For the simulated CPAP dataset, draw one intercept/slope pair per hospital from a bivariate Normal distribution, then generate patient outcomes with independent individual errors. This aligns the generating assumptions with the fitted model. It does not require a repeated-sampling demonstration like the Week 1 simulation.

The HIPARCO-inspired simulated lecture dataset is generated by `generate_interaction_data.R`, with patient data, known generating effects, fixed-model results and figures in `assets/fixed-interaction/`. See [README.md](README.md) for parameters and interpretation. Lab 3 uses the separate wound-healing dataset in `labs_2627/data/wound_healing_week2.csv`; `generate_lab3_debrief.R` fits its models for the post-lab discussion.

## 2. Multiple grouping variables and crossed random effects

Part 3 starts on slide 34 with the crossed-random-effects section divider. Slide 35
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
variables in Week 3. Slides 36–39 reuse the old simulated radiologist-agreement
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
