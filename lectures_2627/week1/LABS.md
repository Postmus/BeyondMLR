# Week 1 lab development notes

Initial 2026–27 drafts use the same 120-patient rehabilitation dataset as the lecture. They retain the previous guided format: supplied R code, explanatory text, and interpretation questions. Rendering displays the code without executing it; students run the chunks themselves.

## Lab 1

[Modeling center as a fixed effect](../../labs_2627/BLR_lab_center_fixed.qmd) focuses on the study design, a center-by-treatment exploratory plot, the fixed-effects model, coefficient interpretation, treatment uncertainty, an overall ANOVA test for center, and diagnostics. The main EDA deliverable reproduces the plot removed from the lecture: individual observations, observed treatment means within each center, and overall treatment means as dashed reference lines. The question asks whether the plot suggests substantial center differences. Students do not repeat the pooled boxplot or fit a treatment-only model.

The coding explanation retains dummy coding for treatment and effects coding for center, including the design matrix and recovery of the sixth center contribution. Students use `summary()`, `confint()` and `sigma()` to interpret the treatment effect and uncertainty, followed by `anova()` to assess center after treatment. The lab distinguishes evidence for center differences from their size in meters. It ends with model diagnostics; the reporting exercise belongs to the assignment.

The previous estimated marginal means explanations, reference grids, treatment comparisons, and comparison-table code are preserved in a separate [teaching note](../../labs_2627/notes/estimated_marginal_means.qmd). The note includes setup for both fitted models so it can be reused independently. It is not part of the required Week 1 lab or the website navigation/render list.

## Lab 2

[Modeling center as a random effect](../../labs_2627/BLR_lab_center_random.qmd) introduces the complete random-intercept model and its assumptions, `lmer()` syntax, fixed effects and variance components. Students interpret the treatment estimate and uncertainty, calculate the ICC, interpret it as a variance proportion and correlation after accounting for treatment, and inspect conditional residuals. The lab runs independently in a fresh R session.

Lab 2 uses the treatment coefficient directly from the model summary. A worked confidence-interval section extracts its estimate, standard error and Satterthwaite degrees of freedom, then calculates an approximate 95% t interval. Students report and interpret the interval in a dedicated question and can adapt the calculation for the assignment. The lab uses the lecture notation, distinguishes uncertainty about the treatment difference from individual outcome variation, and explicitly uses REML. The three-model comparison, reporting exercise, and further-reading section have been removed. The comparison is now included in the overall lecture summary “Comparing the models” (slide 36). The ICC and its interpretations are explained on slide 34 and applied in the lab.

## Lab conventions

Explain what a concept means directly. Avoid unnecessary sentences defining it by what it is not; delete such sentences when they add no useful explanation.

Student-facing instructions should treat the labs as web pages with code to run in an R script. Do not mention Quarto, notebooks or rendering in the labs.

Do not use `with()`. Use explicit data-frame references, such as `table(rehab$center, rehab$treatment)`.

Spell out statistical terms in explanations and questions, including standard error, standard deviation and intraclass correlation coefficient. Preserve literal R column names and mathematical symbols when explaining the output or equations.

## Data and backups

- The student download is [labs_2627/data/rehabilitation_week1.csv](../../labs_2627/data/rehabilitation_week1.csv), an exact copy of the [lecture dataset](assets/rehabilitation_data.csv). Keep these copies synchronized if the teaching data change.
- The original labs are preserved in the independent [2025–2026 website project](../../labs_2526/README.md), including `BLR_lab_FE_oneway.qmd` and `BLR_lab_randomized_block.qmd`.
- The original unpublished random-effects lab and Week 1 assignment are unchanged. A new [Week 1 assignment](../../labs_2627/assignments/week1/assignment.qmd) uses a tissue-donor experiment, with a dataset and report template; see its [development notes](../../labs_2627/assignments/week1/README.md).
- The revised labs are in `labs_2627/`, with distinct filenames: `BLR_lab_center_fixed.qmd` and `BLR_lab_center_random.qmd`. They form the 2026–2027 Quarto website project, with a homepage and Week 1 navigation. The archived edition is a separate project in `labs_2526/`. Each project builds into its own `_site/` folder; replacing the published `docs/` files is a later deployment step.

## Validation and review

The revised dataset contains 120 patients, with 10 per treatment in each of six centers and a simulated treatment effect of 10 m. Data balance, exact agreement between the lab and lecture CSVs, and the treatment-only and fixed-center estimates were checked. The random-intercept results were regenerated with REML using `nlme`, as in the lecture generator. The revised Lab 1 code was run from a fresh R session: its plot, coding matrices, model fit, treatment confidence interval, residual standard deviation, ANOVA table, and diagnostic plots were checked. The center test gives F(5, 113) = 19.196 and p = 8.672e-14.

On 9 November 2026, all Lab 2 analysis chunks were run in a fresh R session using locally installed `lme4` 2.1-0 and `lmerTest` 3.2-1; package installation was performed separately. The new interval calculation was checked against `lmerTest::contest1D()` with Satterthwaite degrees of freedom, both for the rehabilitation data and after adaptation to the assignment data. The treatment estimates, standard errors, variance components and ICC agree with the lecture results below. `pak` 0.11.2 is also installed in the persistent personal R library for the course setup workflow. The retained `emmeans` teaching note was not rerun.

| Model | Enhanced minus standard (m) | SE (m) | 95% CI (m) |
| --- | ---: | ---: | --- |
| Treatment only | 11.03000 | 5.77406 | -0.40421 to 22.46421 |
| Center fixed | 11.03000 | 4.33880 | 2.43405 to 19.62595 |
| Center random | 11.03000 | 4.33880 | 2.43405 to 19.62595 |

The random-intercept model gives center variance 513.81 m², individual variance 564.76 m² and ICC 0.47638.

Website previews are available in [labs_2627/_site](../../labs_2627/_site/). Their accompanying `data` directory contains the student download. These previews contain code for students to run, following the old lab format. Executed versions were rendered separately for validation. The revised Lab 1 HTML and downloadable PDF were rendered and visually checked, including the plotting code, mathematical notation, coding table, and ANOVA section.

Rendering requires Quarto and the R packages `knitr` and `rmarkdown`. Install missing rendering packages with `install.packages(c("knitr", "rmarkdown"))`. Each lab includes setup code for its analysis packages.

To render the current website, including the labs and new assignment, run from the repository root:

```sh
quarto render labs_2627 --to html
```

The next review should focus on pacing, the amount of coding explanation, and whether any questions should become tasks with less supplied code.
