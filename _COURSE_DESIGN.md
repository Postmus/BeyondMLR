# Beyond Multiple Linear Regression (BLR) — Course Design (2024–2025)

## Course Description
Linear regression is powerful for modeling continuous outcomes, but its assumptions (independent, identically distributed errors) often fail in real medical and public health settings. This course introduces linear mixed-effects models and related methods for clustered, hierarchical, and longitudinal data, and then extends beyond Gaussian outcomes to generalized linear models. Over five weeks, we progress from experimental design and ANOVA to multilevel modeling, longitudinal (growth curve) analysis, and GLMs for counts and binaries. Emphasis is on practical modeling decisions, clear interpretation, reproducibility with R and Quarto, and transparent reporting.

## Global Learning Objectives
By the end of the course, students will be able to:
- Apply linear mixed-effects models across medical research contexts, including randomized block designs, within-subject designs, multilevel data, and repeated measurements.
- Apply generalized linear models (and where appropriate, mixed GLMs) to analyze clustered binary and count data; diagnose overdispersion and select appropriate alternatives.
- Create reproducible reports in Quarto that document analysis, diagnostics, and interpretation clearly and concisely.

## Purpose
This document outlines the course structure for BLR and specifies learning objectives for the four core modules: Experimental Design, Multilevel Data, Longitudinal Measurements, and Beyond Continuous Outcomes.

## Structure
- Duration: 5 teaching weeks (4 modules + integrative GLM week)
- Modality per week:
  - Pre‑class: collaborative reading (Perusall) + short concept checks
  - In‑class: interactive lecture (30–40m) + guided R lab (60–70m) + wrap‑up (10–15m)
  - Post‑class: lab assignment (rendered .html + .qmd) + brief reflection
- Deliverables: weekly pass/fail; graded students submit a final portfolio
- Tools: R, Quarto (.qmd), `tidyverse`, `lme4/lmerTest`, `emmeans`, plus topic‑specific packages

## Weekly Spine (Option A)
- Week 1 — Experimental Design I: One‑Way Fixed/Random Effects, ANOVA
  - Labs: `BLR_lab_FE_oneway.qmd`, `BLR_lab_RE_oneway.qmd`
  - Slides: `Lectures_2425/BLR Week 1.pdf`
- Week 2 — Experimental Design II: (Replicated) Randomized Block, Two‑Way ANOVA, Mixed vs Fixed Blocks
  - Labs: `BLR_lab_randomized_block.qmd`, `BLR_lab_replicated_randomized_block.qmd`
  - Slides: `Lectures_2425/BLR Week 2.pdf`
- Week 3 — Multilevel Data in Epidemiology/Public Health
  - Lab: `BLR_lab_MLA.qmd`
  - Slides: `Lectures_2425/BLR Week 3.pdf`
- Week 4 — Longitudinal Measurements (Growth Curve Models)
  - Lab: `BLR_lab_longitudinal.qmd`
  - Slides: `Lectures_2425/BLR Week 4.pdf`
- Week 5 — Beyond Continuous Outcomes (GLMs: Poisson/Logistic; model fit)
  - Lab: `BLR_lab_Poisson.qmd` (+ optional logistic extension)
  - Slides: `Lectures_2425/Lecture week 5.pdf`

## Learning Objectives by Module

### 1) Experimental Design
Students will be able to:
- Formulate and analyze one‑factor experiments using fixed‑effects ANOVA; connect coding schemes (treatment vs effects) to group means and model coefficients.
- Decide and justify when a factor is modeled as fixed vs random; interpret variance components in a one‑way random‑effects model.
- Use estimated marginal means and multiplicity‑adjusted pairwise comparisons; report effect sizes (η²/ω²) with clear interpretation.
- Diagnose assumptions (normality, homoscedasticity); apply remedies/alternatives (Levene/Welch, transformation when appropriate).
- Explain and quantify the precision/power implications of design choices at a high level.

Associated materials:
- Labs: `BLR_lab_FE_oneway.qmd`, `BLR_lab_RE_oneway.qmd`, `BLR_lab_randomized_block.qmd`, `BLR_lab_replicated_randomized_block.qmd`
- Slides: Weeks 1–2

### 2) Multilevel Data (Epidemiology/Public Health)
Students will be able to:
- Explain hierarchical data structures (subjects within contexts; cluster randomization) and quantify clustering via ICC.
- Fit and interpret random‑intercept models; add level‑1 and level‑2 covariates; center continuous predictors and interpret the intercept.
- Assess cross‑level interactions; describe how adding covariates affects within‑ vs between‑cluster variance components.
- Compare ANOVA/mixed approaches conceptually; justify modeling choices under imbalance/missingness.

Associated materials:
- Lab: `BLR_lab_MLA.qmd`
- Slides: Week 3

### 3) Longitudinal Measurements
Students will be able to:
- Frame longitudinal data as multilevel (measurement occasions nested in individuals) and specify growth‑curve models.
- Compare random‑intercept vs random‑slope models; select random‑effects structure using LRTs under REML (no refit), then finalize fixed effects under ML.
- Interpret subject‑specific (mixed) effects vs population‑average trends; produce and read diagnostic plots.
- Discuss practical issues (missingness patterns; zero‑inflation caveat where relevant) and implications for inference.

Associated materials:
- Lab: `BLR_lab_longitudinal.qmd`
- Slides: Week 4

### 4) Beyond Continuous Outcomes (GLMs)
Students will be able to:
- Select appropriate GLM families/links (Poisson/logistic) and interpret coefficients on the link scale.
- Diagnose overdispersion; apply offsets; discuss when to consider negative binomial or zero‑inflated models.
- Evaluate goodness‑of‑fit (Pearson residuals/chi‑square) and communicate model adequacy.
- Extend the mixed‑model mindset to non‑Gaussian outcomes conceptually (awareness level if time constrained).

Associated materials:
- Lab: `BLR_lab_Poisson.qmd` (counts); optional logistic extension
- Slides: Week 5

## Assessments and Grading
- Weekly assignments (pass/fail): renderable .qmd/.html, core analyses completed, brief reflection (2–4 sentences on assumptions/limitations).
- Portfolio (graded students):
  - Contents: all weekly assignments (qmd + html) + one individual dataset analysis.
  - Criteria: correctness of analysis/code, interpretation accuracy, quality of the rendered output (layout/format/presentation), and clarity of justifications.
  - Submission: by posted deadline (see slide deck Week 5).

## Readings and Preparation
- Pre‑class Perusall reading with 3–5 targeted concept checks per week.
- Recommended references: Roback & Legler, Beyond Multiple Linear Regression (for Labs 5+); course slide notes for design modules.

## Textbooks (freely available online)
- Oehlert GW. A First Course in Design and Analysis of Experiments. http://users.stat.umn.edu/~gary/Book.html
- Leyland AH, Groenewegen PP. Multilevel Modelling for Public Health and Health Services Research. https://link.springer.com/book/10.1007/978-3-030-34801-4
- Roback P, Legler J. Beyond Multiple Linear Regression. https://bookdown.org/roback/bookdown-BeyondMLR/
- Wickham H, Cetinkaya-Rundel M, and Grolemund G. R for Data Science (2nd ed.). https://r4ds.hadley.nz/

## Prerequisites
- Introductory statistics and comfort with R.
- Recommended: Chapters 1–8 ("The Whole Game") of R for Data Science before the course.

## Evaluation (program requirements)
- Certificate: complete pre-class readings, participate in at least 80% of in-class activities, and submit all weekly assignments.
- Graded students (e.g., RM or Epidemiologist B): portfolio graded at end of course, consisting of all weekly assignments (qmd + HTML) plus one individual dataset analysis; rubric on Brightspace.

## Credits
- 3 EC

## Course Coordinator
- Douwe Postmus

## R/Quarto Workflow Standards
- Default chunk options: `warning: false`, `message: false`, `eval: true` (override per chunk as needed).
- Reproducible paths: keep datasets under `data/`; use project root consistently.
- Contrasts: set explicitly when needed, e.g., `options(contrasts = c("contr.sum", "contr.poly"))` for effects coding.
- Reporting: always include model specification, key estimates with uncertainty, diagnostics summary, and practical interpretation.

## Suggested Improvements (Weeks 1–2 Priorities)
- Add side‑by‑side coding illustration (`model.matrix`) and link to `emmeans`.
- Include effect sizes (η²/ω²) and Tukey pairwise comparisons in Lab 1.
- Add Levene’s/Welch’s checks and a lightweight power illustration for ANOVA.
- For blocks: compare two‑way ANOVA (block fixed) vs mixed `(1|block)`; quantify ICC; small simulation showing precision gains under blocking.

## Data and Labs Mapping
- Experimental Design: Labs 1–4 (simulated data)
- Multilevel: Lab 5 — `data/airbnb.csv`
- Longitudinal: Lab 6 — `data/alcoholpp.sav`
- Beyond Continuous: Lab 7 — `data/asthma_data.csv`

## Milestones and Timeline
- Week −1: publish syllabus, datasets in `data/`, and lab templates
- Week 1–2: emphasize design decisions, coding schemes, and mixed vs fixed
- Week 3: multilevel fundamentals and variance components
- Week 4: random slopes and model‑building workflow (REML/ML)
- Week 5: GLMs; fit/diagnose Poisson; optional logistic extension
- Final week: portfolio due; optional office hour clinic

## Accessibility and Support
- Provide code‑light summaries and interpretation guides.
- Maintain consistent figure themes and readable defaults.
- Offer brief “Quarto/R tips” each week and a troubleshooting FAQ.
