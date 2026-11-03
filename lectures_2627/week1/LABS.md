# Week 1 lab development notes

Initial 2026–27 drafts use the same 480-patient rehabilitation dataset as the lecture. They retain the previous guided format: supplied R code, explanatory text, interpretation questions, and a short reporting task. Rendering displays the code without executing it; students run the chunks themselves.

## Lab 1

[Modeling center as a fixed effect](../../labs_2627/BLR_lab_center_fixed.qmd) covers the study design, treatment and center summaries, treatment-only and fixed-center models, coefficient interpretation, and treatment contrasts using `emmeans`. Students compare the estimated difference, SE, confidence interval and residual SD. A dedicated section before model fitting introduces dummy coding for treatment and effects coding for center. This makes the treatment coefficient the enhanced-minus-standard difference and the intercept the expected standard-care outcome averaged equally across centers. It explains the coding matrices and how to recover the sixth center contribution from the sum-to-zero constraint. Diagnostics and reporting complete the lab.

## Lab 2

[Modeling center as a random effect](../../labs_2627/BLR_lab_center_random.qmd) introduces the complete random-intercept model and its assumptions, `lmer()` syntax, fixed effects and variance components. Students compare the three treatment analyses, calculate the ICC, interpret it as a variance proportion and correlation after accounting for treatment, and inspect conditional residuals. The lab runs independently in a fresh R session.

Both labs focus on one planned enhanced-minus-standard comparison, calculated with `pairs(..., reverse = TRUE)` rather than explicit contrast weights. Lab 1 introduces estimated marginal means as predicted mean outcomes calculated from the estimated regression coefficients, explains the `emmeans()` syntax, and develops the reference grid from two treatment rows to twelve treatment-by-center combinations. It explains averaging over centers and connects that averaging to effects coding. Lab 2 uses the same comparison syntax while distinguishing averaging over the random-effect distribution from averaging over named fixed centers. They use the lecture notation and distinguish the SE of a contrast from outcome variation and the SEs of individual treatment means. Lab 2 explicitly uses REML and Satterthwaite degrees of freedom.

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

All R chunks were executed. Treatment estimates, SEs and confidence intervals reproduce the lecture values; variance components and ICC also match. The coding check confirms identical fitted values and treatment contrasts under alternative center coding, the interpretation of the intercept, and recovery of the sixth center contribution. Lab 2 uses the same coding when refitting the fixed-center model. Data balance and exact agreement with the lecture CSV were checked, and backups were compared byte for byte with their committed originals.

| Model | Enhanced minus standard (m) | SE (m) | 95% CI (m) |
| --- | ---: | ---: | --- |
| Treatment only | 23.62625 | 2.76350 | 18.19614 to 29.05636 |
| Center fixed | 23.62625 | 2.25568 | 19.19385 to 28.05865 |
| Center random | 23.62625 | 2.25568 | 19.19385 to 28.05865 |

The random-intercept model gives center variance 365.50 m², individual variance 610.57 m² and ICC 0.37446.

Website previews are available in [labs_2627/_site](../../labs_2627/_site/). Their accompanying `data` directory contains the student download. These previews contain code for students to run, following the old lab format. Executed versions were rendered separately for validation.

Rendering requires Quarto and the R packages `knitr` and `rmarkdown`. Install missing rendering packages with `install.packages(c("knitr", "rmarkdown"))`. Each lab includes setup code for its analysis packages.

To render the current website, including the labs and new assignment, run from the repository root:

```sh
quarto render labs_2627 --to html
```

The next review should focus on pacing, the amount of coding explanation, and whether any questions should become tasks with less supplied code.
