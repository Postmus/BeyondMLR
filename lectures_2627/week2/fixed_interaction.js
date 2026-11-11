const path = require('path');
const { addConceptPanelsSlide } = require('../../slides-common/js/concept_panels');
const { addDiagnosticsInterpretationSlide } = require('../../slides-common/js/course_builders');
const { addTeachingTableSlide } = require('../../slides-common/js/teaching_table');
const { addFigureSlide, addSectionSlide, addBulletVisualSlide } = require('../../slides-common/js/pptx_layout_primitives');
const { addAllocationImbalance } = require('./allocation_imbalance');
const { addAdditiveExample, addInteractionExample } = require('./additive_example');

const E = {
  additive: String.raw`Y_{ij}=\beta_0+c_j+\beta_1T_{ij}+\varepsilon_{ij}`,
  model: String.raw`Y_{ij}=\beta_0+c_j+(\beta_1+d_j)T_{ij}+\varepsilon_{ij}`,
  centerConstraint: String.raw`\sum_j c_j=0`,
  interactionConstraint: String.raw`\sum_j d_j=0`,
  errors: String.raw`\varepsilon_{ij}\overset{\mathrm{iid}}{\sim}N(0,\sigma^2)`,
  constraints: String.raw`\sum_{j=1}^{J}c_j=0,\qquad \sum_{j=1}^{J}d_j=0`,
  standard: String.raw`\mu_{0j}=\beta_0+c_j`,
  enhanced: String.raw`\mu_{1j}=\beta_0+c_j+\beta_1+d_j`,
  effect: String.raw`\delta_j=\mu_{1j}-\mu_{0j}=\beta_1+d_j`,
  simpleEffect: String.raw`\text{Effect in center }j:\quad\beta_1+d_j`,
  marginalEffect: String.raw`\text{Average effect:}\quad\beta_1`,
  beta0: String.raw`\beta_0=\frac{1}{J}\sum_{j=1}^{J}\mu_{0j}`,
  beta1: String.raw`\beta_1=\frac{1}{J}\sum_{j=1}^{J}\delta_j`,
  deviations: String.raw`c_j=\mu_{0j}-\beta_0,\qquad d_j=\delta_j-\beta_1`,
  difference: String.raw`D_j=\bar Y_{1j}-\bar Y_{0j}`,
  expansion: String.raw`\begin{aligned}D_j&=(\beta_0+c_j+\beta_1+d_j+\bar\varepsilon_{1j})\\&\quad -(\beta_0+c_j+\bar\varepsilon_{0j})\\&=\beta_1+d_j+\bar\varepsilon_{1j}-\bar\varepsilon_{0j}\end{aligned}`,
  noise: String.raw`\eta_j=\bar\varepsilon_{1j}-\bar\varepsilon_{0j}`,
  differenceNoise: String.raw`D_j=\beta_1+d_j+\eta_j`,
  errorVariance: String.raw`\operatorname{Var}(\bar\varepsilon_{1j})=\operatorname{Var}(\bar\varepsilon_{0j})=\frac{\sigma^2}{n}`,
  noiseVariance: String.raw`\operatorname{Var}(\eta_j)=\frac{\sigma^2}{n}+\frac{\sigma^2}{n}=\frac{2\sigma^2}{n}`,
  average: String.raw`\widehat\beta_1=\frac{1}{J}\sum_{j=1}^{J}D_j`,
  fixedExpansion: String.raw`\widehat\beta_1=\beta_1+\frac{1}{J}\sum_{j=1}^{J}\eta_j`,
  fixedVariance: String.raw`\operatorname{Var}(\widehat\beta_1)=\frac{1}{J^2}\sum_{j=1}^{J}\frac{2\sigma^2}{n}=\frac{2\sigma^2}{Jn}`,
  estimatedSE: String.raw`\widehat{\operatorname{SE}}(\widehat\beta_1)=\widehat\sigma\sqrt{\frac{2}{Jn}}`,
  randomSlope: String.raw`d_j\overset{\mathrm{iid}}{\sim}N(0,\sigma_d^2)`,
  randomExpansion: String.raw`\widehat\beta_1=\beta_1+\frac{1}{J}\sum_{j=1}^{J}d_j+\frac{1}{J}\sum_{j=1}^{J}\eta_j`,
  populationVariance: String.raw`\operatorname{Var}(\widehat\beta_1)=\frac{\sigma_d^2}{J}+\frac{2\sigma^2}{Jn}`,
  fixedLimit: String.raw`\frac{2\sigma^2}{Jn}\longrightarrow 0\quad\text{as }n\longrightarrow\infty`,
  randomLimit: String.raw`\frac{\sigma_d^2}{J}+\frac{2\sigma^2}{Jn}\longrightarrow\frac{\sigma_d^2}{J}`,
};

function interactionFEquation(results) {
  const a = results.additive_model, m = results.interaction_model;
  const rss = value => Math.round(value).toLocaleString('en-US').replaceAll(',', '{,}');
  return String.raw`F=\frac{(\mathrm{RSS}_{\mathrm{reduced}}-\mathrm{RSS}_{\mathrm{full}})/(\mathrm{df}_{\mathrm{reduced}}-\mathrm{df}_{\mathrm{full}})}{\mathrm{RSS}_{\mathrm{full}}/\mathrm{df}_{\mathrm{full}}}` +
    String.raw`=\frac{(${rss(a.residual_sum_squares)}-${rss(m.residual_sum_squares)})/(${a.residual_df}-${m.residual_df})}{${rss(m.residual_sum_squares)}/${m.residual_df}}\approx ${results.interaction_test.F.toFixed(2)}`;
}

function addWeek2Opening(pptx, drawEquation, week1, results, allocation) {
  const f = (v, digits = 1) => Number(v).toFixed(digits);
  const panels = (title, content, takeaway, options = {}) => addConceptPanelsSlide(pptx, {
    title, panels: content, takeaway, drawEquation, ...options
  });
  const text = (value, h, options = {}) => ({ text: value, h, fontSize: 22, ...options });
  const eq = (key, h = .65, gap = .18) => ({ equation: E[key], h, gap });
  const assets = path.join(__dirname, 'assets/fixed-interaction');
  const a = results.additive_model, m = results.interaction_model;

  // Recap the original trial alongside the closing Week 1 comparison table.
  panels('Week 1 recap: comparing the models', [
    { title: 'THE REHABILITATION TRIAL', blocks: [
      text('Simulated trial: 120 patients after knee replacement at six centers.', .70, { fontSize: 21 }),
      text('Within each center: 10 patients randomized to each treatment.', .70, { fontSize: 21 }),
      text('Standard care: weekly physiotherapy and home exercises.', .70, { fontSize: 21 }),
      text('Enhanced care: standard care plus a weekly supervised session.', .70, { fontSize: 21 }),
      text('Outcome: six-minute walk distance at 12 weeks.', .70, { fontSize: 21 })
    ] },
    { title: 'THE TREATMENT COMPARISON', blocks: [
    { table: {
      columns: [
        { label: 'Model', weight: 2.4 },
        { label: 'Estimate\n(m)', weight: 1.6, align: 'center' },
        { label: 'Estimated\nSE (m)', weight: 1.9, align: 'center' }
      ], rows: [
        ['Treatment only', f(week1.pooled.estimate), f(week1.pooled.se, 2)],
        ['Fixed center effects', f(week1.fixed.estimate), f(week1.fixed.se, 2)],
        ['Random center effects', f(week1.random.estimate), f(week1.random.se, 2)]
      ], fontSize: 21, headerFontSize: 18, headerHeight: .80, firstColumnBold: false
    }, h: 2.90, gap: .25 },
    text('All three models assumed a common treatment effect across centers.', .75, { fontSize: 21 })
  ] }], 'Fixed center parameters and a random center distribution gave the same treatment estimate and estimated standard error in this setting.');

  addAllocationImbalance(pptx, allocation);

  // 3: A new realization in the familiar design; no old-data relabeling.
  addSectionSlide(pptx, {
    sectionLabel: 'PART 1',
    sectionTitle: 'Treatment-by-center interaction'
  });
  addAdditiveExample(pptx, drawEquation);
  addInteractionExample(pptx, drawEquation);

  // Choose how centers enter the model before formalizing the fixed-effects route.
  const representationSlide = panels('How should we represent differences between centers?', [
    { title: 'FIXED CENTER EFFECTS', blocks: [
      text('Give each included center its own baseline deviation and treatment-effect deviation.', 1.35, { gap: .30 }),
      text('Express the average treatment effect across the included centers.', 1.20)
    ] },
    { title: 'RANDOM CENTER EFFECTS', blocks: [
      text('Describe baseline deviations and treatment-effect deviations through a population distribution.', 1.35, { gap: .30 }),
      text('Express the average treatment effect in the population represented by those centers.', 1.20)
    ] }
  ], 'We start with fixed center effects and ask what we can learn about treatment in the included centers.', {
    intro: 'Beyond two centers, we still allow differences in baseline outcome and treatment effect.'
  });
  representationSlide.addNotes('Both approaches allow treatment effects to vary between centers. The distinction is separate fixed center parameters versus a distribution of random center effects, not common versus varying treatment effects. This slide previews the included-center and population-average targets used in this lecture; it is not a claim that a random-effects model cannot estimate included-center contrasts or that model choice alone establishes population representativeness. Random treatment effects require random slopes as well as center intercepts. Implications for uncertainty are developed later.');

  const empiricalSlide = addBulletVisualSlide(pptx, {
    title: 'From known means to patient data',
    bulletTitle: 'THE REHABILITATION TRIAL',
    bullets: [
      'New simulated trial: 120 patients after knee replacement at six centers.',
      'Within each center: 10 patients randomized to standard care and 10 to enhanced care.',
      'Outcome: six-minute walk distance at 12 weeks.',
      'Fit the fixed-effects interaction model to estimate treatment effects in these centers.'
    ],
    bulletFontSize: 21, bulletMinFontSize: 21,
    assetPath: path.join(assets, 'rehabilitation-case-study.png'),
    note: 'The group means are now unknown and must be estimated from patient outcomes.',
    noteFontSize: 20, noteHeight: 1.0
  });
  empiricalSlide.addNotes('The preceding toy examples assumed the four true group means were known. We now begin an empirical analysis of a new simulated six-center trial: the analyst estimates unknown group means and treatment effects from individual patient outcomes. Each center has 10 patients per treatment. After exploring the data, formalize the grouped fixed-effects interaction model on the next model slide. The right-hand image is an AI-generated fictional illustration of a rehabilitation walking assessment, not a study participant photograph. Exploratory plots and fitted cell means follow on separate slides.');

  addFigureSlide(pptx, {
    title: 'Explore outcomes by treatment and center',
    assetPath: path.join(assets, 'interaction-outcomes-slide.png'),
    caption: 'Each dot is a patient outcome; diamonds show sample means.\nWhat differences do you notice in the treatment gaps across centers?',
    captionFontSize: 19, captionMinFontSize: 19, captionHeight: .85
  });

  const fixedSpecificationSlide = panels('A fixed-effects model for treatment and center', [
    { title: 'BASELINE OUTCOMES', blocks: [
      text('β₀: average standard-care outcome across the included centers.', .90, { fontSize: 21 }),
      text('cⱼ: center j’s deviation from that baseline average.', .85, { fontSize: 21 }),
      eq('centerConstraint', .72, .10)
    ] },
    { title: 'TREATMENT EFFECTS', blocks: [
      text('β₁: average treatment effect across the included centers.', .90, { fontSize: 21 }),
      text('dⱼ: center j’s deviation from the average treatment effect.', .85, { fontSize: 21 }),
      eq('interactionConstraint', .72, .10)
    ] }
  ], 'Each center receives equal weight in the averages. Center deviations are fixed, unknown parameters.', {
    lead: { equation: E.model, text: 'Tᵢⱼ = 0 for standard care and 1 for enhanced care. Center uses effects coding.' }
  });
  fixedSpecificationSlide.addNotes('This is the same grouped model as the preceding toy interaction example. The center-specific standard-care mean is beta_0 + c_j and the center-specific treatment effect (the slope for treatment coded 0/1) is beta_1 + d_j. These treatment slopes are the vertical gaps in the earlier two-center toy plots. In the immediately preceding exploratory plot, each center has its own panel and treatment is on the horizontal axis. It applies to any number of centers with two treatments. Patient i is in center j. Effects coding imposes sum_j c_j = 0 and sum_j d_j = 0, so beta_0 and beta_1 are equally weighted included-center averages. Treatment retains 0/1 coding; beta_0 is the average standard-care mean, not the grand mean across both treatments. The patient error epsilon_ij has mean zero. Independent normal errors with a common variance are the assumptions used later for the fixed-model sampling variance and t interval; those assumptions and their role belong to the uncertainty section.');

  const coefficientSlide = addTeachingTableSlide(pptx, {
    title: 'Fixed-effects interaction model: results',
    columns: [
      { label: 'Term', weight: 3.9 },
      { label: 'Estimate (m)', weight: 1.8, align: 'center' },
      { label: 'Estimated\nSE (m)', weight: 1.8, align: 'center' },
      { label: '95% CI (m)', weight: 2.9, align: 'center' },
      { label: 'p-value', weight: 1.6, align: 'center' }
    ],
    rows: results.regression_coefficients.map(row => [
      row.label, f(row.estimate), f(row.standard_error, 2),
      `${f(row.lower_95)} to ${f(row.upper_95)}`,
      row.p < .001 ? '< .001' : f(row.p, 3)
    ]),
    fontSize: 18,
    tableOptions: { headerFontSize: 16, headerHeight: .62, cellPaddingY: .035, firstColumnBold: false },
    takeaway: 'Effects coding: Center F follows from the sum-to-zero constraints.',
    takeawayHeight: .60
  });
  coefficientSlide.addNotes('OLS coefficient estimates from lm(distance ~ treatment * center), with Standard as treatment zero and contr.sum for center. Columns follow the Week 1 convention: estimates and confidence limits to one decimal, estimated standard errors to two decimals, and p-values to three decimals or < .001. All intervals are pointwise 95% t intervals and p-values are two-sided t tests of a zero coefficient, using 108 residual degrees of freedom. The intercept is the equally weighted standard-care mean; the Enhanced care coefficient is the equally weighted treatment effect. Center A through E are baseline deviations c_j; Enhanced by Center A through E are treatment-effect deviations d_j. Their p-values test deviations from the included-center average, not each center\'s overall treatment effect. Center F has no separate regression coefficient: its baseline and treatment deviations are minus the corresponding sums for A through E. The global interaction test is a different, five-degree-of-freedom test.');

  const interactionTest = results.interaction_test;
  const comparisonSlide = panels('Testing interaction by comparing two models', [{ blocks: [
    { table: {
      columns: [
        { label: 'Model', weight: 6.1 },
        { label: 'Residual sum\nof squares (RSS)', weight: 3.3, align: 'center' },
        { label: 'Residual df', weight: 1.9, align: 'center' }
      ],
      rows: [
        ['Reduced: common treatment effect', Math.round(a.residual_sum_squares).toLocaleString('en-US'), a.residual_df],
        ['Full: center-specific treatment effects', Math.round(m.residual_sum_squares).toLocaleString('en-US'), m.residual_df]
      ], fontSize: 20, headerFontSize: 17, headerHeight: .60, cellPaddingY: .045, firstColumnBold: false
    }, h: 1.50, gap: .10 },
    text('RSS: residual sum of squares. df: residual degrees of freedom.', .32, { fontSize: 18, gap: .10 }),
    { equation: interactionFEquation(results), h: .95, gap: .10 },
    text('Numerator: improvement in fit per additional parameter.', .34, { fontSize: 20, gap: .08 }),
    text('Denominator: estimated individual-error variance from the full model.', .34, { fontSize: 20 })
  ] }], `F(${interactionTest.numerator_df}, ${interactionTest.denominator_df}) = ${f(interactionTest.F, 2)}, p < .001: strong evidence that treatment effects differ across centers.`, {
    intro: 'Fit the full model → fit the reduced model → compare their fit.'
  });
  comparisonSlide.addNotes('Both models retain center-specific baselines, treatment coded 0/1, and effects coding for center. The full model is distance ~ treatment * center; the reduced model is distance ~ treatment + center, obtained by setting all d_j to zero. They are nested models fitted by OLS to the same 120 patients. RSS measures variation left unexplained, so smaller RSS means better fit. The full model always fits at least as well. The partial F test asks whether its improvement is larger than expected from sampling variation under the reduced model. The difference in residual degrees of freedom is 113 - 108 = 5, the number of additional interaction parameters. These five coefficients are tested jointly; the sixth deviation is implied by the sum-to-zero constraint. The numerator is the reduction in RSS per added parameter; the denominator estimates the common individual-error variance using the full model. Under independent normal errors with common variance and the null hypothesis, the statistic has an F distribution with 5 and 108 degrees of freedom. Table RSS values are rounded; the test and p-value use full-precision R output from anova(model_additive, model_interaction). This is a test of variation in treatment effects, not a test that the average treatment effect is zero. Next, inspect fitted means and center-specific contrasts to see where and by how much effects differ.');

  const effectConceptSlide = panels('Which treatment effect do we want to estimate?', [
    { title: 'SIMPLE TREATMENT EFFECTS', blocks: [
      text('How do treatments compare within a particular center?', .80, { bold: true, gap: .15 }),
      text('Compare the enhanced and standard means within each center.', 1.0, { fontSize: 21, gap: .15 }),
      eq('simpleEffect', .50, .15),
      text('One treatment effect per center.', .85, { fontSize: 20 })
    ] },
    { title: 'MARGINAL TREATMENT EFFECT', blocks: [
      text('How do treatments compare on average across centers?', .80, { bold: true, gap: .15 }),
      text('Average each treatment’s mean equally across centers, then compare the averages.', 1.0, { fontSize: 21, gap: .15 }),
      eq('marginalEffect', .50, .15),
      text('With effects coding, the treatment deviations average to zero.', .85, { fontSize: 20 })
    ] }
  ], 'Estimated marginal means (EMMs) are model-based estimates of group means, averaged over other factors where appropriate. Comparing these means gives the treatment effects.', { takeawayHeight: 1.10 });
  effectConceptSlide.addNotes('Recall the known-means two-center examples: differences within rows were simple treatment effects, while the difference between equally weighted treatment-column averages was the marginal effect. The same questions now concern unknown means estimated from patient data. Under effects coding for center and 0/1 coding for treatment, the true simple effect in center j is beta_1 + d_j; the equally weighted marginal effect is beta_1 because sum_j d_j = 0. Estimated effects use the corresponding fitted parameters. The estimate of the marginal treatment effect is beta-hat_1 = 21.3 m, the Enhanced care coefficient on slide 14. Estimated marginal means are predicted means for specified groups, possibly averaged over other factors. Keeping center separate gives the treatment-by-center cell means here; averaging equally across centers gives one marginal mean per treatment. The effects are contrasts of these means. This is a definition of the estimands and their model-based estimation, not a new regression model or a software tutorial.');

  // Keep fitted means and their within-center contrasts together, with inference.
  const cell = (center, treatment) => results.fitted_cell_means.find(r => r.center === center && r.treatment === treatment).estimate;
  const simpleEffectsSlide = panels('Estimating simple treatment effects with estimated marginal means', [
    { title: 'CODING AND EFFECTS', weight: 1, blocks: [
      text('Coding determines which effects appear directly as coefficients.', 1.10, { fontSize: 19, bullet: true, gap: .15 }),
      text('Here, a center’s effect combines the average effect with its treatment deviation.', 1.25, { fontSize: 19, bullet: true, gap: .15 }),
      text('Predict both treatment means within each center, then compare them.', 1.10, { fontSize: 19, bullet: true })
    ] },
    { weight: 2.5, blocks: [
      { table: {
        columns: [
          { label: 'Center', weight: .90, align: 'center' },
          { label: 'Standard\nmean', weight: 1.15, align: 'center' },
          { label: 'Enhanced\nmean', weight: 1.30, align: 'center' },
          { label: 'Difference', weight: 1.25, align: 'center', highlight: true },
          { label: '95% CI', weight: 1.90, align: 'center', highlight: true },
          { label: 'p-value\n(Bonf.)', weight: 1.20, align: 'center', highlight: true }
        ],
        columnGroups: [{ label: '', count: 1 }, { label: 'Predicted means', count: 2 }, { label: 'Simple treatment effect', count: 3 }],
        rows: results.within_center_comparisons.map(r => [
          r.center.replace('Center ', ''), f(cell(r.center, 'Standard')), f(cell(r.center, 'Enhanced')),
          f(r.estimate), `${f(r.lower_95)} to ${f(r.upper_95)}`,
          r.p_bonferroni < .001 ? '< .001' : f(r.p_bonferroni, 3)
        ]), fontSize: 18, headerFontSize: 16, headerHeight: .65,
        cellPaddingX: .07, cellPaddingY: .04, firstColumnBold: false
      }, h: 3.65, gap: .15 },
      text('Means, differences and CIs in meters. Difference: enhanced minus standard.\nPointwise 95% CIs; p-values adjusted across six comparisons (Bonferroni).', .70, { fontSize: 16 })
    ] }
  ], 'Equivalent coding schemes give different coefficients, but the same fitted means and treatment comparisons.');
  simpleEffectsSlide.addNotes('All quantities are derived from the same fitted full interaction model. Standard and Enhanced columns are model-based predicted cell means, which coincide with sample cell means in this full factorial model. Enhanced minus Standard is each center’s simple treatment effect, not the interaction coefficient d_j alone. Estimated standard errors use the full coefficient covariance matrix, including covariance between the two predicted means, and the pooled full-model residual variance. Confidence intervals are pointwise 95% t intervals with 108 residual degrees of freedom. Two-sided p-values test zero treatment difference in each center and are Bonferroni-adjusted as min(6 * p, 1), treating all six centers as one family. The p-value column is adjusted, whereas the confidence intervals are pointwise, so an interval can exclude zero while its adjusted p-value exceeds .05. The effect estimate and its uncertainty do not change under an equivalent full-rank recoding of the same fitted model. No package names or software syntax are needed for this lecture explanation.');

  // Close the case study by checking residuals from the full interaction model.
  const diagnosticsSlide = addDiagnosticsInterpretationSlide(pptx, {
    title: 'Model diagnostics: fixed-effects interaction',
    plotHeight: 3.0,
    diagnostics: [
      { assetPath: path.join(assets, 'interaction-residuals-vs-fitted.png'),
        purpose: 'Assess whether residual spread is similar across fitted walk distances.' },
      { assetPath: path.join(assets, 'interaction-normal-qq.png'),
        purpose: 'Assess whether the residual distribution is approximately Normal.' }
    ],
    takeaway: 'The plots show no clear change in residual spread and only small departures from the Normal reference line.'
  });
  diagnosticsSlide.addNotes('These diagnostics use the fitted full fixed-effects interaction model from this case study: distance ~ treatment * center. The left plot shows raw residuals against fitted cell means in meters. Vertical stacks are expected because patients in the same treatment-center cell share a fitted mean. The fitted model includes a separate mean for each of the 12 cells, so residuals average to zero within every cell by construction; this fact alone does not validate the mean model. Look particularly for systematic changes in residual spread. The right plot compares internally standardized residuals, calculated using residual leverage and the estimated residual standard deviation, with theoretical Normal quantiles. The reference line uses the first and third quartiles. The displayed data show no clear funnel pattern and only small tail deviations from the line. These plots do not establish independence or prove the assumptions. No additional formal diagnostic test or p-value is used. This closes the fitted-model analysis; two concluding Part 1 slides compare omission of interaction and unequal center sizes.');

  // Close Part 1 with the consequences of omitting interaction, without derivations.
  const omissionSlide = panels('What if we leave out the interaction?', [{ blocks: [
    text('Estimated average treatment effect across the six centers', .42, { fontSize: 21, bold: true, gap: .16 }),
    { table: {
      columns: [
        { label: 'Model', weight: 3.0 },
        { label: 'Estimate (m)', weight: 1.8, align: 'center' },
        { label: 'Estimated\nSE (m)', weight: 1.8, align: 'center' },
        { label: '95% CI (m)', weight: 2.8, align: 'center' },
        { label: 'p-value', weight: 1.5, align: 'center' }
      ], rows: [['Additive', a], ['With interaction', m]].map(([label, model]) => [
        label, f(model.estimate), f(model.standard_error, 2),
        `${f(model.lower_95)} to ${f(model.upper_95)}`,
        model.p < .001 ? '< .001' : f(model.p, 3)
      ]), fontSize: 22, headerFontSize: 18, headerHeight: .70, cellPaddingY: .06,
      firstColumnBold: false
    }, h: 1.80, gap: .28 },
    text('Same estimate: equal numbers in every treatment–center group make both models estimate the average of the six within-center treatment differences.', .85, { fontSize: 21, gap: .18 }),
    text('Larger estimated SE: without interaction, differences in treatment effects remain in the residuals, increasing the estimated residual variation.', .85, { fontSize: 21 })
  ] }], 'In this balanced study, omitting interaction leaves the average treatment estimate unchanged but increases its estimated uncertainty.');
  omissionSlide.addNotes('Both models include fixed center effects with effects coding and treatment coded 0/1. The current design has ten patients in every treatment-center cell, so the treatment coefficients are the same equally weighted average of the six observed differences. Omitting interaction does not change this estimator or its actual sampling variance. It leaves systematic treatment heterogeneity in the residuals and inflates the usual estimated residual variance in expectation. In this realization, the estimated SE rises from 4.68 to 5.17 m. Intervals and p-values are the usual model-based t summaries, with 113 residual degrees of freedom for the additive model and 108 for the interaction model. P-values are two-sided tests that the average treatment effect is zero; these are not interaction tests or the six Bonferroni-adjusted simple-effect tests. The additive model imposes a common effect despite the heterogeneity, so its conventional SE and interval are presented to explain the consequences of omission, not as a claim that it correctly specifies the cell means.');

  const sizeSlide = panels('What if center sizes differ?', [
    { title: 'WHAT THE MODELS ESTIMATE', weight: 1.15, blocks: [
      text('With our effects coding, the interaction model estimates the equally weighted average treatment effect, regardless of center sizes.', 1.40, { fontSize: 21, bullet: true, gap: .18 }),
      text('If centers have equal sizes, the additive model estimates this same average.', .95, { fontSize: 21, bullet: true, gap: .18 }),
      text('If center sizes differ, the additive model gives larger centers more weight.', .95, { fontSize: 21, bullet: true })
    ] },
    { title: 'ILLUSTRATIVE EXAMPLE', weight: 1, blocks: [
      { table: {
        columns: [
          { label: 'Center', weight: 1.5 },
          { label: 'Patients', weight: 1.0, align: 'center' },
          { label: 'Treatment\neffect (m)', weight: 1.6, align: 'center' }
        ], rows: [['Small', '20', '10'], ['Large', '80', '30']],
        fontSize: 22, headerFontSize: 17, headerHeight: .75,
        cellPaddingX: .10, cellPaddingY: .06, firstColumnBold: false
      }, h: 2.0, gap: .38 },
      text('Interaction model: 20 m', .55, { fontSize: 23, bold: true, gap: .20 }),
      text('Additive model: 26 m', .55, { fontSize: 23, bold: true })
    ] }
  ], 'When treatment effects differ across centers, omitting interaction can change both the interpretation of the treatment coefficient and its estimated uncertainty.', { takeawayHeight: 1.05 });
  sizeSlide.addNotes('Only center sizes are varied in this illustration; allocation remains half to each treatment within each center, as in the preceding empirical design. Keep that setup in the spoken explanation if needed rather than adding an allocation sentence to the slide. The displayed effects of 10 and 30 m are hypothetical known center-specific effects, not new estimates from the rehabilitation dataset. The model values are the averages targeted by the fitted treatment coefficients; equivalently they are fitted coefficients if the observed within-center differences equal these values. With effects coding and an interaction, beta_1 is the arithmetic average, (10 + 30) / 2 = 20 m, regardless of cell sizes when all cell means are estimable. With a common treatment proportion in all centers, OLS in the additive fixed-center model weights the within-center effects by total center size: (20 * 10 + 80 * 30) / 100 = 26 m. These weights are imposed by the additive fit, not chosen after fitting. For arbitrary unequal treatment proportions, the weights instead depend on n_j p_j (1-p_j); the simplified equal-size and size-weighting claims on this slide belong to the fixed-allocation illustration, not to all unbalanced designs. Describe a change in coefficient interpretation rather than universal bias. No general ordering of estimated SEs is asserted for unequal center sizes.');

  // Part 2: a bridge to the later random-slope fitting section.
  addSectionSlide(pptx, {
    sectionLabel: 'PART 2',
    sectionTitle: 'From included centers to a population'
  });
  panels('What changes if the study uses different centers?', [
    { title: 'THESE SIX CENTERS', blocks: [
      text('Repeat the study with new patients in the same six centers.', 1.0, { gap: .25 }),
      text('Each center’s true treatment effect stays the same.', 1.0, { gap: .25 }),
      text('Uncertainty comes from estimating those six effects using patient outcomes.', 1.15)
    ] },
    { title: 'A POPULATION OF CENTERS', blocks: [
      text('Repeat the study with new centers sampled from a wider population.', 1.0, { gap: .25 }),
      text('The selected centers can have different true treatment effects.', 1.0, { gap: .25 }),
      text('Uncertainty also comes from which centers enter the study.', 1.15)
    ] }
  ], 'A population-average treatment effect requires accounting for variation in treatment effects between sampled centers.');

  panels('Random treatment effects add a source of uncertainty', [{ blocks: [
    text('Let β₁ be the population-average treatment effect, with independent random center deviations:', .65),
    eq('randomSlope', .65, .20),
    text('The deviations have expectation zero; their realized average need not be zero.', .65),
    eq('randomExpansion', .75, .20),
    text('ηⱼ is patient sampling noise, independent of the random center effects.', .65, { fontSize: 20 }),
    eq('populationVariance', .65),
    text('J = centers; n = patients per treatment per center; σ² = individual-error variance.', .60, { fontSize: 19 })
  ] }]);

  panels('More patients and more centers answer different needs', [
    { title: 'AVERAGE IN THE INCLUDED CENTERS', blocks: [
      text('Increasing patients per treatment makes each center’s effect more precise.', 1.10),
      eq('fixedLimit', .85, .25),
      text('Even heterogeneous treatment effects can have a precisely estimated average within these centers.', 1.25)
    ] },
    { title: 'AVERAGE IN THE POPULATION', blocks: [
      text('More patients reduce individual noise, but the study still samples only J centers.', 1.10),
      eq('randomLimit', .85, .25),
      text('More centers reduce both variance terms. Six precisely measured centers still provide only six center effects.', 1.25)
    ] }
  ], 'These are sampling-variance formulas under the balanced model; fitted uncertainty also involves estimating the variance components.');

  panels('The target determines which uncertainty belongs', [
    { title: 'FIXED INTERACTION', blocks: [
      text('Target: the average effect in the included centers.', .85, { bold: true, gap: .25 }),
      text('Patient sampling determines uncertainty around that fixed average.', 1.05, { gap: .25 }),
      text('Omitting the interaction can inflate the estimated uncertainty by leaving heterogeneity in the residuals.', 1.35)
    ] },
    { title: 'RANDOM TREATMENT EFFECT', blocks: [
      text('Target: the average effect in a population of centers.', .85, { bold: true, gap: .25 }),
      text('Center sampling adds uncertainty about that population average.', 1.05, { gap: .25 }),
      text('A random-intercept-only model can understate uncertainty because it omits treatment-effect variation.', 1.35)
    ] }
  ], 'Next: model a population of center-specific intercepts and treatment effects using random intercepts and slopes.');
}

module.exports = { addWeek2Opening, interactionFEquation, equations: Object.values(E) };
