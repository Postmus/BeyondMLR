# Beyond MLR: Week 1

Week 1 starts with a multicenter rehabilitation trial and asks whether an initial treatment-only model adequately represents the study:

1. The opening introduces all 480 patients in six centers, with 40 patients randomized to each program within every center. Outcomes are first pooled by treatment. Slide 6 introduces the systematic part as the proposed expected outcome, interprets the coefficients, and explicitly defines the error with zero expectation. Slide 7 recaps expected value, variance and covariance in a table linked to the errors. Slide 8 illustrates zero and positive covariance using pairs of errors across hypothetical repetitions, with matching means, variances and scales. Slide 9 completes the model with four explicit assumptions: zero mean for each patient, equal variance, independence and Normality. The initial results follow on slide 10: 23.6 m (SE 2.76 m; 95% CI 18.2–29.1 m). Slide 11 places the two diagnostic plots above explanations of their purpose, with one shared limitation: pooled residuals can look reassuring even when the model does not adequately represent differences between centers.
2. Part 1 ends on slide 12 by combining the data-generating model and treatment-only model: omitting center puts its contribution into the error. This is a conceptual example, separate from the case-study EDA.
3. Part 2 (slides 13–26) begins with new patients sampled in the same six centers, then simplifies to two centers and four patient positions. The simulation sets the baseline to 100, treatment effect to 5, center contributions to −10 and +10, and independent Normal individual errors with mean 0 and SD 5. It uses the known treatment-only component to compute errors. One sampled study is followed by four histograms at 20 and 10,000 repetitions, using cumulative draws, identical axes and identical bins. Advance the slides to reveal the developing distributions. The next slide pairs errors from the same repetitions: u₁₁ against u₂₁ within center 1, and u₁₁ against u₁₂ across centers. Both pairs have model covariance zero; dashed lines mark their respective expected errors. Each position has expected error −10 or +10, despite a pooled expected error of 0. After the four-assumption review, slide 21 starts from the expected treatment estimate (the true effect plus the difference in average center contributions), then explains where the estimate is centered and how much it varies: balanced allocation cancels center contributions in the treatment contrast, while the larger pooled residual spread overstates its repeated-sampling variation. The center-adjustment slide then motivates the return to center indicators, interpretation and results from the six-center trial. The section ends with the conceptual bridge to a distribution of center contributions.
4. The existing randomized-block section is retained as Part 3 (slides 27 onward) for later refinement. The expected-error and covariance comparison appears on slides 34–35, after fixed and random center effects have both been introduced. Fixed contributions shift error means; random contributions create shared variation around the marginal means. The covariance slide explicitly assumes independent individual errors, independent of any random center contribution. Labs and assignments remain to be revised separately.

The multicenter model assumes a common treatment effect and uses a random center intercept. Each new model has a plain-language interpretation alongside its equation. Cluster randomization starts the revised Week 2 lecture. Treatment-by-center interactions and random treatment slopes are outside the scope of Week 1. Labs and assignments will be revised separately once the lecture is settled.

The presentation uses simulated knee-rehabilitation data from six centers with balanced treatment allocation. These are teaching data and do not represent a real clinical study.

## Source structure

- `Beyond_MLR_Week_1.pptx`: presentation.
- `build_week1.js`: PptxGenJS content and slide order.
- `error_recap.js`: expected-outcome model, error-concept table and paired covariance illustrations.
- `error_interlude.js`: the combined omitted-center slide and the later two-slide comparison of expected errors and covariance.
- `fixed_center.js`: the fixed-center section, its regression model, result comparison and conceptual bridge to random center effects.
- `fixed_center_sampling.js`: repeated-sampling perspective, simulation setup, one study, cumulative histograms, assumption review, consequences of omitting center and center-adjustment explanation.
- `generate_fixed_center_sampling.R`: reproducible four-position teaching simulation, histograms and paired-error scatterplots, independent of the rehabilitation dataset.
- `assets/fixed-center-sampling.json`: simulation settings, first study, empirical moments and correlations.
- `generate_interlude_figure.R`: schematic covariance figures; no case-study data are used.
- `generate_interlude_equations.js`: optional regeneration of missing interlude equation assets.
- `generate_data.R`: simulation, figures and model calculations.
- `generate_initial_analysis.R`: pooled outcomes, treatment-only diagnostics and initial-model results from the existing complete dataset.
- `initial_model.js`: opening model equations, using `u` for the treatment-only error consistently with the interlude.
- `assets/initial-analysis-results.json`: coefficient estimates, uncertainty and residual variation for the opening.
- `assets/rehabilitation_data.csv`: simulated patient-level dataset.
- `assets/model_results.json`: statistics read by the deck builder.
- `assets/rehabilitation-walk-assessment.png`: generated illustration for the clinical case.
- `assets/equations/`: LaTeX source, MathJax SVG and high-resolution PNG fallbacks.
- `pptxgenjs_helpers/`: local text-sizing, image, LaTeX and QA helpers.
- `../../slides-common/js/`: shared course theme, layout primitives and course builders.
- `../../slides-common/R/common_fig.R`: shared figure palette and sizing conventions.

The lecture focuses on statistical concepts and interpretation; R code and software instructions are reserved for the labs.

Slide text, tables and layout elements are editable. Statistical figures and equations are embedded as images. Equations are authored as LaTeX; PNG fallbacks provide consistent PowerPoint/LibreOffice rendering.

## Rebuild

Run from this directory after installing the Node dependencies in `package.json`. R requires `ggplot2`, `nlme` and `jsonlite`.

```sh
npm install
npm run build
```

Figure generation finishes before the presentation is built. The fixed seed makes the data and model results reproducible. The supplied equation PNGs allow the presentation to render without a local TeX installation.

To regenerate only the fixed-center teaching simulation, run `npm run fixed-center-simulation`. Its checks verify the error means, variances and pairwise correlations against the generating process. Both histogram slides and the paired-error scatterplots use the same simulation draws.

To regenerate only the initial-analysis figures and statistics, run `npm run initial-analysis`. This reads the existing trial data without regenerating it or changing later-slide assets.

To regenerate missing opening and interlude equation assets, run `npm run interlude-equations` with `pdflatex` and `pdftoppm` available. Existing equation PNGs are preserved. The interlude uses the shared `slides-common/js/concept_panels.js` layout helper; other slide builders are unaffected.

The simulator demonstration uses `#/concepts/randomized-block-design` in the separate sampling-simulator project. It compares ordinary regression with a random-intercept model. The presentation's fixed-center comparison is calculated from the lecture dataset.
