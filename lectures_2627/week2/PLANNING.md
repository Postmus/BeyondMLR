# Week 2 planning notes

The proposed Week 2 lecture explores treatment effects that vary between centers, followed by a brief section on cluster randomization. Build directly on the Week 1 model in the same multicenter setting. Move crossed random effects to the end of Week 3, alongside nested grouping structures. The current 25-slide opening includes a title and two section dividers around the Week 1 recap, unequal allocation, fixed interaction and uncertainty, ending with a theoretical bridge to random treatment effects. The random-slope analysis, cluster-randomization section and labs remain to be developed. The earlier cluster-first starter is preserved in `build_cluster_starter.js`.

## Opening and theoretical emphasis

After the title, begin with the model-comparison table from the final substantive Week 1
slide. Slides 3–6 introduce unequal treatment proportions within centers while
retaining equal overall group sizes and a common treatment effect. Show the
allocation, fitted results, the comparisons used by the three models, and why the
estimated standard errors can agree or differ across designs. Slide 6 now compares
equal center sizes, unequal center sizes with balanced treatment allocation, and
unequal treatment proportions. Explain the same mix of centers and estimation of
individual variation in plain language. Exact estimated-SE equality in the fully
balanced case assumes REML and a positive estimated center variance. Move the
detailed precision discussion for the unequal-allocation example to backup slide 27
(after the backup divider). Use “estimated standard error” throughout the lecture.
Keep the opening formula-free and qualify the extra between-center information
by the random-intercept assumptions.
Begin Part 1 with the known-means two-center additive example on slide 8. Its table includes row and column averages, while its plot places center on the horizontal axis and annotates both 20 m treatment gaps. The matching interaction example on slide 9 changes the gaps to 10/30 m, retains the 20 m average effect, and labels center deviations of -10/+10 m before introducing the six-center dataset.
Then restore balanced allocation before introducing treatment heterogeneity and
the distinction between included-center and population-average uncertainty.

After the toy examples, slide 10 introduces the conceptual choice between fixed center parameters and a population distribution of random center effects. Slide 11 begins the empirical six-center analysis in the usual text-left/image-right layout, explicitly moving from known means to estimation from patient data. Slide 12 separately shows the exploratory patient-outcome plot. Slide 13 then introduces the grouped treatment-slope formulation, parameter definitions and enlarged sum-to-zero constraints. Slide 14 then presents all fitted coefficients in the Week 1 estimate/SE/95% CI/p-value format before the nested-model interaction test on slide 15. Compare full and reduced model RSS using the annotated partial F formula, then introduce simple versus marginal effects and define estimated marginal means on slide 16. Slide 17 pairs a coding explanation with predicted means, simple effects, pointwise 95% CIs and Bonferroni-adjusted p-values across the six centers. Slide 18 closes the empirical analysis with residuals versus fitted values and a Normal Q-Q plot from the full interaction model. Keep software names and syntax for the lab. Finish Part 1 with the additive/interaction average-effect comparison on slide 19 and the unequal-center-size illustration on slide 20. Remove the former technical Part 2; the population-center section becomes Part 2 on slide 21.

Keep effects coding for center in the fixed interaction. Explain the consequences
of omitting interaction in plain language: equal cell sizes preserve the average
treatment estimate, while unexplained treatment heterogeneity increases the
estimated residual variation and estimated SE here. Unequal center sizes can
change the additive coefficient's interpretation because larger centers receive
more weight in the fixed-allocation example. Retain its allocation condition in
speaker notes only; do not add weighting formulas to the slide. Then contrast
repeating the study in the same centers with sampling different centers in Part 2.

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

The lecture dataset is now generated by `generate_interaction_data.R`, with patient data, known generating effects, fixed-model results and figures in `assets/fixed-interaction/`. See [README.md](README.md) for parameters and interpretation. Use a separate dataset for the later lab, potentially adapted from the old labs; lab development is deferred.

## 2. Treatment assigned to whole centers

End Week 2 with a brief introduction to cluster randomization as a change in experimental design: whole centers, rather than patients within centers, are assigned to treatment.

Suggested transition: “What happens when we can no longer compare treatments within the same center?”

Link this to the preceding discussion of center effects and treatment-by-center interactions. Each center is observed under only one treatment, so its baseline contribution and its treatment-specific contribution cannot be separated using a within-center treatment comparison. This limitation should be distinguished from confounding of the average treatment effect: random assignment of centers still supports estimation of the average treatment effect through comparisons between centers.

Move the main cluster-randomization material from the old Week 3 lecture into this section.

## Four model structures across the course

| Structure | Main idea | Example syntax | Main introduction |
| --- | --- | --- | --- |
| Random intercept | Groups differ in their expected outcome, with a common explanatory-variable effect. | `(1 | center)` | Week 1 |
| Random intercept and slope | Groups also differ in the effect of an explanatory variable. | `(1 + treatment | center)` | Week 2; revisited for time in Week 4 |
| Nested random effects | Lower-level groups belong to higher-level groups. | `(1 | district/neighborhood)` | Week 3 |
| Crossed random effects | Observations belong to two grouping structures that are not nested. | `(1 | participant) + (1 | stimulus)` | End of Week 3 |

These are four useful teaching examples, not four mutually exclusive model classes. Intercept versus slope describes **what varies between groups**; nested versus crossed describes **how grouping structures relate**. They can be combined, for example by including random slopes in a nested model.

## Crossed sources of variation: move to the end of Week 3

Introduce crossed random effects after nested models. The transition is: so far groups have been nested; what changes when an observation belongs to two groups that are not nested?

For example, participants respond to several stimuli, with each stimulus presented to several participants. Outcomes can vary between participants and between stimuli; stimuli are shared across participants rather than nested within them. Use this structure to motivate the two sets of random effects.

## Connection to the other weeks

- Week 1 establishes randomized block designs, fixed and random center effects, and a common treatment effect.
- Week 2 keeps the randomized block setting and introduces treatment-by-center interaction, first as fixed effects with estimated marginal means and then as random intercepts and slopes. It ends with a brief contrast with cluster randomization.
- Week 3 focuses on observational multilevel data, explanatory variables at different levels, contextual predictors, cross-level interactions, and nested random effects. It ends with crossed random effects. Move the cluster-randomization section from its old opening to the end of Week 2; cluster-assigned treatment can still be recalled briefly as a context-level variable.
- Week 4 covers longitudinal data and returns to random intercepts and slopes, now with time as the explanatory variable: `(1 + time | person)`. The structure is familiar from Week 2, while the interpretation becomes individual differences in starting outcomes and changes over time.

When developing examples, distinguish bias, actual sampling variation and estimated uncertainty. The Week 1 balanced within-center treatment contrast cancels a shared center intercept; dependence alone does not imply that ignoring grouping understates the standard error. Revisit the consequences as the model or design changes.

## Existing material to revisit

- [Old Week 2 lecture](../../lectures_2526/BLR_Week_2_reveal.qmd): interactions and replicated randomized block designs. Omit the standalone factorial-design section and relocate crossed random effects to Week 3.
- [Old replicated-block lab](../../labs_2526/BLR_lab_replicated_randomized_block.qmd): interaction model, estimated marginal means, variance components and omission of the random interaction. Revise its random-effects specification and ICC interpretation as described above.
- [Old Week 3 lecture](../../lectures_2526/BLR_Week_3_reveal.qmd): move its cluster-randomization examples to Week 2 and retain its observational and nested-model material for Week 3.
- [Revised Week 1 lecture notes](../week1/README.md): starting model, case study and simulation approach.
