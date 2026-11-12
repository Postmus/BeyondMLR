const path = require('path');
const crypto = require('crypto');
const { addConceptPanelsSlide } = require('../../slides-common/js/concept_panels');
const { addDiagnosticsInterpretationSlide } = require('../../slides-common/js/course_builders');
const { addTeachingTableSlide } = require('../../slides-common/js/teaching_table');
const { addFigureSlide, addSectionSlide, addBulletVisualSlide } = require('../../slides-common/js/pptx_layout_primitives');
const { addRandomInteractionResults } = require('./random_interaction');
const { addAllocationImbalance } = require('./allocation_imbalance');
const { addAdditiveExample, addInteractionExample } = require('./additive_example');

const E = {
  additive: String.raw`Y_{ij}=\beta_0+c_j+\beta_1T_{ij}+\varepsilon_{ij}`,
  model: String.raw`Y_{ij}=\beta_0+c_j+(\beta_1+d_j)T_{ij}+\varepsilon_{ij}`,
  centerVarianceSymbol: String.raw`\sigma_c^2`,
  slopeVarianceSymbol: String.raw`\sigma_d^2`,
  covarianceSymbol: String.raw`\sigma_{cd}`,
  errorVarianceSymbol: String.raw`\sigma^2`,
  centerConstraint: String.raw`\sum_j c_j=0`,
  interactionConstraint: String.raw`\sum_j d_j=0`,
  errors: String.raw`\varepsilon_{ij}\overset{\mathrm{iid}}{\sim}N(0,\sigma^2)`,
  randomCenterDistribution: String.raw`\begin{pmatrix}c_j\\d_j\end{pmatrix}\sim N\!\left[\begin{pmatrix}0\\0\end{pmatrix},\begin{pmatrix}\sigma_c^2&\sigma_{cd}\\\sigma_{cd}&\sigma_d^2\end{pmatrix}\right]`,
  individualErrorDistribution: String.raw`\varepsilon_{ij}\sim N(0,\sigma^2)`,
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

function addWeek2Opening(pptx, drawEquation, week1, results, allocation, lab3, randomInteraction) {
  const f = (v, digits = 1) => Number(v).toFixed(digits);
  const panels = (title, content, takeaway, options = {}) => addConceptPanelsSlide(pptx, {
    title, panels: content, takeaway, drawEquation, ...options
  });
  const text = (value, h, options = {}) => ({ text: value, h, fontSize: 22, ...options });
  const eq = (key, h = .65, gap = .18) => ({ equation: E[key], h, gap });
  const assets = path.join(__dirname, 'assets/fixed-interaction');

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
    assetPath: path.join(assets, 'interaction-outcomes-slide.png')
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
  fixedSpecificationSlide.addNotes('This is the same grouped model as the preceding toy interaction example. The center-specific standard-care mean is beta_0 + c_j and the center-specific treatment effect (the slope for treatment coded 0/1) is beta_1 + d_j. These treatment slopes are the vertical gaps in the earlier two-center toy plots. In the immediately preceding exploratory plot, centers are on the horizontal axis and each treatment has its own line. The vertical gap between the enhanced and standard lines is the observed treatment difference at each center. It applies to any number of centers with two treatments. Patient i is in center j. Effects coding imposes sum_j c_j = 0 and sum_j d_j = 0, so beta_0 and beta_1 are equally weighted included-center averages. Treatment retains 0/1 coding; beta_0 is the average standard-care mean, not the grand mean across both treatments. The patient error epsilon_ij has mean zero. Independent normal errors with a common variance are the assumptions used later for the fixed-model sampling variance and t interval; those assumptions and their role belong to the uncertainty section.');

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
  coefficientSlide.addNotes('OLS coefficient estimates from lm(distance ~ treatment * center), with Standard as treatment zero and contr.sum for center. Columns follow the Week 1 convention: estimates and confidence limits to one decimal, estimated standard errors to two decimals, and p-values to three decimals or < .001. All intervals are pointwise 95% t intervals and p-values are two-sided t tests of a zero coefficient, using 108 residual degrees of freedom. The intercept is the equally weighted standard-care mean; the Enhanced care coefficient is the equally weighted treatment effect. Center A through E are baseline deviations c_j; Enhanced by Center A through E are treatment-effect deviations d_j. Their p-values test deviations from the included-center average, not each center\'s overall treatment effect. Center F has no separate regression coefficient: its baseline and treatment deviations are minus the corresponding sums for A through E.');

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
          { label: 'p-value', weight: 1.20, align: 'center', highlight: true }
        ],
        columnGroups: [{ label: '', count: 1 }, { label: 'Predicted means', count: 2 }, { label: 'Simple treatment effect', count: 3 }],
        rows: results.within_center_comparisons.map(r => [
          r.center.replace('Center ', ''), f(cell(r.center, 'Standard')), f(cell(r.center, 'Enhanced')),
          f(r.estimate), `${f(r.lower_95)} to ${f(r.upper_95)}`,
          r.p < .001 ? '< .001' : f(r.p, 3)
        ]), fontSize: 18, headerFontSize: 16, headerHeight: .65,
        cellPaddingX: .07, cellPaddingY: .04, firstColumnBold: false
      }, h: 3.65, gap: .15 },
      text('Means, differences and CIs in meters. Difference: enhanced minus standard.\nPointwise 95% CIs and unadjusted p-values.', .70, { fontSize: 16 })
    ] }
  ], 'Equivalent coding schemes give different coefficients, but the same fitted means and treatment comparisons.');
  simpleEffectsSlide.addNotes('All quantities are derived from the same fitted full interaction model. Standard and Enhanced columns are model-based predicted cell means, which coincide with sample cell means in this full factorial model. Enhanced minus Standard is each center’s simple treatment effect, not the interaction coefficient d_j alone. Estimated standard errors use the full coefficient covariance matrix, including covariance between the two predicted means, and the pooled full-model residual variance. Confidence intervals are pointwise 95% t intervals with 108 residual degrees of freedom. Unadjusted two-sided p-values test zero treatment difference in each center. Each pointwise 95% confidence interval corresponds to its individual test at the .05 level. Lab 3 includes an optional callout on multiplicity and Bonferroni adjustment. The effect estimate and its uncertainty do not change under an equivalent full-rank recoding of the same fitted model. No package names or software syntax are needed for this lecture explanation.');

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
  diagnosticsSlide.addNotes('These diagnostics use the fitted full fixed-effects interaction model from this case study: distance ~ treatment * center. The left plot shows raw residuals against fitted cell means in meters. Vertical stacks are expected because patients in the same treatment-center cell share a fitted mean. The fitted model includes a separate mean for each of the 12 cells, so residuals average to zero within every cell by construction; this fact alone does not validate the mean model. Look particularly for systematic changes in residual spread. The right plot compares internally standardized residuals, calculated using residual leverage and the estimated residual standard deviation, with theoretical Normal quantiles. The reference line uses the first and third quartiles. The displayed data show no clear funnel pattern and only small tail deviations from the line. These plots do not establish independence or prove the assumptions. No additional formal diagnostic test or p-value is used. This closes the rehabilitation analysis. Pause for Lab 3 next, then use its wound-healing setting for two concluding Part 1 discussion slides on omission of interaction and unequal laboratory sizes.');

  const lab3Slide = addFigureSlide(pptx, {
    title: 'Time for Lab 3',
    assetPath: path.join(__dirname, 'assets/lab3-debrief/lab3-break.png')
  });
  lab3Slide.addNotes('Lab 3: Treatment effects across laboratories. Students analyze the simulated wound-healing dataset: five fixed laboratories, ten independent cell-culture plates per treatment in each laboratory, and percentage wound closure after 48 hours. The lab covers exploration, the fixed interaction model, estimated marginal means and simple effects, and residual diagnostics. The omission-of-interaction comparison belongs to the following lecture debrief, not an extra lab exercise. Illustration generated for this break slide in the same editorial style as Week 1.');

  // Return from Lab 3 to discuss its wound-healing analysis, without derivations.
  const labAdditive = lab3.additive_model, labInteraction = lab3.interaction_model;
  const omissionSlide = panels('Lab 3: what if we leave out the interaction?', [{ blocks: [
    text('Average effect across the five laboratories\nExperimental minus standard, in percentage points of wound closure', .75, { fontSize: 21, gap: .10 }),
    { table: {
      columns: [
        { label: 'Model', weight: 3.0 },
        { label: 'Estimate', weight: 1.8, align: 'center' },
        { label: 'Estimated\nSE', weight: 1.8, align: 'center' },
        { label: '95% CI', weight: 2.8, align: 'center' },
        { label: 'p-value', weight: 1.5, align: 'center' }
      ], rows: [['Additive', labAdditive], ['With interaction', labInteraction]].map(([label, model]) => [
        label, f(model.estimate, 2), f(model.standard_error, 2),
        `${f(model.lower_95, 2)} to ${f(model.upper_95, 2)}`,
        model.p < .001 ? '< .001' : f(model.p, 3)
      ]), fontSize: 22, headerFontSize: 18, headerHeight: .70, cellPaddingY: .06,
      firstColumnBold: false
    }, h: 1.80, gap: .18 },
    text('Same estimate: 10 plates per treatment in every laboratory make both models estimate the average of the five within-laboratory treatment differences.', .75, { fontSize: 21, gap: .18 }),
    text('Larger estimated SE: without interaction, differences in treatment effects remain in the residuals, increasing the estimated residual variation.', .75, { fontSize: 21 })
  ] }], 'In this balanced study, omitting interaction leaves the average treatment estimate unchanged but increases its estimated uncertainty.');
  omissionSlide.addNotes('This table is fitted to the same wound_healing_week2.csv used in Lab 3. Both models include fixed laboratory effects with effects coding and treatment coded 0/1. The design has ten plates in every treatment-laboratory cell, so the treatment coefficients are the same equally weighted average of the five observed differences: 9.284 percentage points. Omitting interaction does not change this estimator or its actual sampling variance. It leaves systematic treatment heterogeneity in the residuals and inflates the usual estimated residual variance in expectation. Here the estimated SE rises from 1.616 to 1.812 percentage points. Intervals and p-values are the usual model-based t summaries, with 94 residual degrees of freedom for the additive model and 90 for the interaction model. The two-sided p-values test a zero average effect and are unadjusted; the lab separately presents five unadjusted simple-effect tests, with optional Bonferroni adjustment in a callout. The additive model imposes a common effect despite heterogeneity, so its conventional uncertainty summaries illustrate the consequences of omission rather than asserting correct specification of the cell means.');

  const sizeExample = lab3.unequal_size_example;
  const sizeSlide = panels('What if laboratory sizes differ?', [
    { title: 'WHAT THE MODELS ESTIMATE', weight: 1.15, blocks: [
      text('With our effects coding, the interaction model estimates the equally weighted average treatment effect, regardless of laboratory sizes.', 1.40, { fontSize: 21, bullet: true, gap: .18 }),
      text('If laboratories have equal sizes, the additive model estimates this same average.', .95, { fontSize: 21, bullet: true, gap: .18 }),
      text('If laboratory sizes differ, the additive model gives larger laboratories more weight.', .95, { fontSize: 21, bullet: true })
    ] },
    { title: 'HYPOTHETICAL EXAMPLE', weight: 1, blocks: [
      text('Effects in percentage points', .35, { fontSize: 19, gap: .05 }),
      { table: {
        columns: [
          { label: 'Laboratory', weight: 1.5 },
          { label: 'Plates', weight: 1.0, align: 'center' },
          { label: 'Treatment\neffect', weight: 1.6, align: 'center' }
        ], rows: ['Small', 'Large'].map((label, j) =>
          [label, String(sizeExample.plates[j]), String(sizeExample.effects[j])]),
        fontSize: 22, headerFontSize: 17, headerHeight: .75,
        cellPaddingX: .10, cellPaddingY: .06, firstColumnBold: false
      }, h: 1.85, gap: .20 },
      text(`Interaction model: ${f(sizeExample.interaction_average, 0)} points`, .55, { fontSize: 22, bold: true, gap: .20 }),
      text(`Additive model: ${f(sizeExample.additive_average, 0)} points`, .55, { fontSize: 22, bold: true })
    ] }
  ], 'When treatment effects differ across laboratories, omitting interaction can change both the interpretation of the treatment coefficient and its estimated uncertainty.', { takeawayHeight: 1.05 });
  sizeSlide.addNotes('This is a hypothetical extension of the wound-healing setting, using two laboratories for a simple illustration rather than new estimates from the five-laboratory dataset. Laboratory size is the number of independent plates. Only laboratory sizes change; half the plates still receive each treatment within each laboratory, as in Lab 3. Keep this setup in the spoken explanation rather than adding an allocation sentence to the slide. The effects of 5 and 15 percentage points are hypothetical known effects. The model values are the averages targeted by the treatment coefficients; equivalently they are fitted coefficients if the observed within-laboratory differences equal these values. With effects coding and an interaction, the arithmetic average is (5 + 15) / 2 = 10 percentage points, regardless of cell sizes when all means are estimable. With a common treatment proportion in all laboratories, the additive fixed-laboratory model weights the effects by laboratory size: (20 * 5 + 80 * 15) / 100 = 13 percentage points. These weights are imposed by the additive fit, not chosen afterward. With arbitrary treatment proportions, weights instead depend on n_j p_j (1-p_j); the simplified slide claims apply to this fixed-allocation illustration. Describe a change in coefficient interpretation rather than universal bias. No general ordering of estimated SEs is asserted for unequal sizes.');

  // Part 2: a bridge to the later random-slope fitting section.
  addSectionSlide(pptx, {
    sectionLabel: 'PART 2',
    sectionTitle: 'From included centers to a population'
  });
  const populationTargetSlide = panels('From the included centers to a population', [
    { title: 'THESE SIX CENTERS', blocks: [
      text('Imagine repeating the study with new patients in the same six centers.', 1.0, { gap: .25 }),
      text('The centers’ true treatment effects stay the same.', 1.0, { gap: .25 }),
      text('We estimate the average effect in these six centers.', 1.15)
    ] },
    { title: 'A POPULATION OF CENTERS', blocks: [
      text('Imagine repeating the study with new centers from the same population.', 1.0, { gap: .25 }),
      text('The selected centers can have different true treatment effects.', 1.0, { gap: .25 }),
      text('We estimate the average effect across the population of centers.', 1.15)
    ] }
  ], 'When we generalize to a population of centers, uncertainty also depends on which centers enter the study.');
  populationTargetSlide.addNotes('Return to the rehabilitation setting after the wound-healing debrief. The fixed-effects analysis described the six included centers. For the population target, think of those centers as a sample from a wider population, with new independent samples possible in repetitions of the study. The population remains the same across repetitions, but the included centers and their effects can change. Generalization also depends on how centers were selected and on substantive assumptions; fitting a random-effects model alone does not establish representativeness. Patient sampling uncertainty remains in both settings.');

  const sampleGridSlide = addFigureSlide(pptx, {
    title: 'What changes when we sample new centers?',
    assetPath: path.join(__dirname, 'assets/random-center-samples/two-center-samples.png'),
    caption: 'Hypothetical true mean walking distances (m), without patient sampling noise.',
    captionFontSize: 18, captionMinFontSize: 18, captionHeight: .42,
    takeaway: 'The population model stays the same, but each sample contains centers with different realized effects.',
    takeawayHeight: .82
  });
  sampleGridSlide.addNotes('Read across columns to compare model structures and down rows to compare independent center samples from the same population under each structure. The first sample contains Centers A to D; the second contains E to H. There are four illustrative centers per sample, not the six centers of the fitted rehabilitation dataset. The plotted points are true group means, with no individual patient noise. In the left column random intercepts change standard-care outcomes but the true treatment gap is always 20 m. In the right column both intercepts and treatment effects vary between centers. The same intercept draws are reused across columns within a row to isolate the added treatment-effect variation. Random slope means are not recentered: the two sampled average treatment effects are approximately 18.95 and 16.31 m, despite a common population mean of 20 m. The random-slope illustration draws independent center pairs from a bivariate Normal distribution with intercept SD 20 m, slope SD 15 m, and correlation .25. These are conceptual settings, not fitted estimates. Centers are on the horizontal axis. Treatment slope means the change from standard to enhanced care, seen here as the vertical gap between the colored lines, not the slope of a segment joining centers.');

  const varianceLabel = (key, description) => ({
    inlineMath: [
      { image: path.join(__dirname, 'assets/equations', `eq_${crypto.createHash('sha1').update(E[key]).digest('hex').slice(0, 12)}.png`), heightEm: 1.05, baselineFraction: .9 },
      { text: `: ${description}` }
    ], h: .67, gap: .01, fontSize: 20
  });
  const randomModelSlide = panels('The center effects follow a population distribution', [
    { title: 'DISTRIBUTIONAL ASSUMPTIONS', blocks: [
      eq('randomCenterDistribution', .95, .12),
      eq('individualErrorDistribution', .45, .12),
      text('Effect pairs are independent across centers.\nErrors are mutually independent and independent of the center effects.', 1.05, { fontSize: 19 })
    ] },
    { title: 'VARIANCES AND COVARIANCE', blocks: [
      varianceLabel('centerVarianceSymbol', 'variance of standard-care means across centers.'),
      varianceLabel('slopeVarianceSymbol', 'variance of treatment effects across centers.'),
      varianceLabel('covarianceSymbol', 'covariance of baseline and treatment-effect deviations.'),
      varianceLabel('errorVarianceSymbol', 'individual variance around the center-specific treatment mean.')
    ] }
  ], 'Center deviations have population means of zero. They are not constrained to sum to zero in the sampled centers.', {
    lead: { equation: E.model, text: 'β₀ and β₁ are population averages. Tᵢⱼ = 0 (standard) or 1 (enhanced).' }
  });
  randomModelSlide.addNotes('The outcome equation has the same algebraic structure as the fixed-effects specification. The crucial addition is an explicit population distribution for the center effects. Beta_0 is the population-average standard-care mean; beta_1 is the population-average enhanced-minus-standard effect. Each center has a joint Normal pair (c_j, d_j) with expectation (0,0), intercept variance sigma_c squared, slope variance sigma_d squared, and covariance sigma_cd. Covariance is not correlation: it has squared-outcome units, and dividing by sigma_c times sigma_d gives the correlation. Center pairs are independent across centers. Their sample sums need not vanish, unlike the effects-coding constraint on the earlier fixed parameters. Individual errors are independent Normal draws with mean zero and variance sigma squared and are independent of every center-effect pair. The residual variance describes individual patient variation around the center-specific treatment mean. Marginalizing over the shared center effects induces within-center outcome dependence. Interpret the covariance visually on the next slide.');

  const covarianceSlide = addFigureSlide(pptx, {
    title: 'How do baseline outcomes and treatment effects vary together?',
    assetPath: path.join(__dirname, 'assets/random-center-samples/covariance-examples.png'),
    caption: 'True means and enhanced-minus-standard effects, not estimates. Each point is one hypothetical center.',
    captionFontSize: 18, captionMinFontSize: 18, captionHeight: .42,
    takeaway: 'Covariance describes how a center’s baseline outcome and treatment effect vary together.',
    takeawayHeight: .82
  });
  covarianceSlide.addNotes('These two hypothetical populations illustrate positive and negative covariance between the random intercept c_j and random treatment slope d_j. The horizontal axis uses the true standard-care mean beta_0 + c_j, and the vertical axis uses the true enhanced-minus-standard effect beta_1 + d_j. Adding these fixed population averages does not change the covariance. Both panels use identical axis limits and mirrored treatment-effect deviations, preserving exactly the same empirical spreads while reversing the covariance sign. Each dot is a different hypothetical center within a panel. The points are known true values, not estimated means, treatment-effect estimates, or fitted random effects. There is no patient sampling noise, and these are not fitted results for the six rehabilitation centers. Positive covariance means that higher standard-care means tend to accompany larger treatment effects; negative covariance means they tend to accompany smaller treatment effects. A negative covariance does not require a negative average treatment effect. This relationship is descriptive rather than a causal claim about changing baseline outcomes.');

  addRandomInteractionResults(pptx, drawEquation, randomInteraction, E, results.interaction_model);

}

module.exports = { addWeek2Opening, equations: Object.values(E) };
