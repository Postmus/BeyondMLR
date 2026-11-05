# Week 2 planning notes

The proposed Week 2 lecture explores treatment effects that vary between centers, followed by a brief section on cluster randomization. Build directly on the Week 1 model in the same multicenter setting. Move crossed random effects to the end of Week 3, alongside nested grouping structures. This is a planning note; the lecture and labs still need to be developed. The existing Week 2 starter deck begins with cluster randomization and therefore does not yet follow this revised order.

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

This closely parallels the fixed-effects interaction model and establishes terminology and syntax that return in Week 4. With reference coding, the fixed-effects model's treatment coefficient describes the reference center; in the random-effects model it describes the population-average treatment effect.

The old replicated-block lab instead used:

```r
lmer(outcome ~ treatment + (1 | center) + (1 | treatment:center), data = dat)
```

Both allow treatment effects to vary between centers, but their covariance assumptions differ. The old formulation has an independent shared center contribution and independent treatment-by-center contributions with a common interaction variance. The random-slope formulation allows a more flexible variance-covariance structure. Changing formulation is therefore a deliberate modeling change, not merely a syntax change.

For the two-treatment example, explain the intercept variance, treatment-effect variance, intercept-slope covariance, and individual residual variance. With 0/1 coding, the intercept refers to control outcomes. Do not carry forward the old lab's "interaction ICC" terminology: a variance fraction is not automatically a pairwise correlation. With random slopes, within-center correlation can differ for two control patients, two experimental-treatment patients, and a pair receiving different treatments. There is no single common ICC to report.

For a single teaching dataset, draw one intercept/slope pair per center from a bivariate Normal distribution, then generate patient outcomes with independent individual errors. This aligns the generating assumptions with the fitted model. It does not require a repeated-sampling demonstration like the Week 1 simulation.

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
