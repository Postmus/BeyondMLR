# Week 2 planning notes

The proposed Week 2 lecture explores how experimental design informs the random-effects structure. Build on Week 1 by extending the model in the same multicenter setting, then introduce two other experimental settings. This is a planning note; the lecture and labs still need to be developed.

## 1. Treatment effects that vary between centers

Keep the Week 1 design, with patients assigned to both treatments within each center. Extend the common treatment effect to allow treatment effects to differ between centers.

Introduce the treatment-by-center interaction and then the random treatment slope. This gives interactions a clear purpose within the progression beyond the random-intercept model. A separate section on factorial designs is no longer needed.

## 2. Treatment assigned to whole centers

Introduce cluster randomization as a change in experimental design: whole centers, rather than patients within centers, are assigned to treatment.

Suggested transition: “What happens when we can no longer compare treatments within the same center?”

Link this to the preceding discussion of center effects and treatment-by-center interactions. Each center is observed under only one treatment, so its baseline contribution and its treatment-specific contribution cannot be separated using a within-center treatment comparison. This limitation should be distinguished from confounding of the average treatment effect: random assignment of centers still supports estimation of the average treatment effect through comparisons between centers.

Move the main cluster-randomization material from the old Week 3 lecture into this section.

## 3. Crossed sources of variation

Retain crossed random effects as another common experimental setting. For example, participants respond to several stimuli, with each stimulus presented to several participants. Outcomes can vary between participants and between stimuli; stimuli are shared across participants rather than nested within them.

Use the design to motivate the two sets of random effects. The section complements the preceding center-based examples by introducing a different grouping structure.

## Connection to the other weeks

- Week 1 establishes randomized block designs, fixed and random center effects, and a common treatment effect.
- Week 2 extends the model to random slopes, changes the allocation scheme to cluster randomization, and introduces crossed random effects.
- Week 3 can focus on observational multilevel health-research data, contextual predictors and cross-level interactions. Cluster-assigned treatment can be recalled briefly as an example of a context-level variable.

When developing examples, distinguish bias, actual sampling variation and reported uncertainty. The Week 1 balanced within-center treatment contrast cancels a shared center intercept; dependence alone does not imply that ignoring grouping understates the standard error. Revisit the consequences as the model or design changes.

## Existing material to revisit

- [Old Week 2 lecture](../../lectures_2526/BLR_Week_2_reveal.qmd): interactions, replicated randomized block designs and crossed random effects. Omit the standalone factorial-design section.
- [Old Week 3 lecture](../../lectures_2526/BLR_Week_3_reveal.qmd): cluster-randomization examples and their implications for estimation and uncertainty.
- [Revised Week 1 lecture notes](../week1/README.md): starting model, case study and simulation approach.
