const path = require('path');
const crypto = require('crypto');
const { addConceptPanelsSlide } = require('../../slides-common/js/concept_panels');
const { addDiagnosticsInterpretationSlide } = require('../../slides-common/js/course_builders');
const { addTeachingTableSlide } = require('../../slides-common/js/teaching_table');
const { addFigureSlide, addSectionSlide, addBulletVisualSlide } = require('../../slides-common/js/pptx_layout_primitives');
const { addRandomInteractionResults } = require('./random_interaction');
const { addAllocationImbalance } = require('./allocation_imbalance');
const { addInteractionExample } = require('./additive_example');
const { addAssignmentRecap } = require('./assignment_recap');

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
  emmPrediction: String.raw`\widehat{\mu}_{tj}=\widehat{\beta}_0+\widehat{c}_j+(\widehat{\beta}_1+\widehat{d}_j)t`,
  emmAverage: String.raw`\widehat{\mu}_{t\cdot}=\frac{\widehat{\mu}_{tA}+\widehat{\mu}_{tB}+\cdots+\widehat{\mu}_{tF}}{6}`,
  estimatedMarginalEffect: String.raw`\widehat{\beta}_1`,
  estimatedSimpleEffect: String.raw`\widehat{\beta}_1+\widehat{d}_j`,
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

// Symbols used in the treatment-by-hospital reference grid.
for (const t of [0, 1]) {
  for (const hospital of ['A', 'B', 'C', 'D', 'E', 'F']) {
    E[`grid${t}${hospital}`] = String.raw`\widehat{\mu}_{${t}${hospital}}`;
  }
  E[`grid${t}Average`] = String.raw`\widehat{\mu}_{${t}\cdot}`;
}

function addWeek2Opening(pptx, drawEquation, results, allocation, lab3, randomInteraction) {
  const f = (v, digits = 1) => Number(v).toFixed(digits);
  const panels = (title, content, takeaway, options = {}) => addConceptPanelsSlide(pptx, {
    title, panels: content, takeaway, drawEquation, ...options
  });
  const text = (value, h, options = {}) => ({ text: value, h, fontSize: 22, ...options });
  const eq = (key, h = .65, gap = .18) => ({ equation: E[key], h, gap });
  const assets = path.join(__dirname, 'assets/fixed-interaction');

  addAssignmentRecap(pptx, drawEquation, allocation);
  addAllocationImbalance(pptx, allocation);
  addInteractionExample(pptx, drawEquation);

  // Choose how centers enter the model before formalizing the fixed-effects route.
  const representationSlide = panels('How should we represent differences between groups?', [
    { title: 'FIXED GROUP EFFECTS', blocks: [
      text('Give each included group its own baseline deviation and treatment-effect deviation.', 1.35, { gap: .30 }),
      text('Express the average treatment effect across the included groups.', 1.20)
    ] },
    { title: 'RANDOM GROUP EFFECTS', blocks: [
      text('Describe baseline deviations and treatment-effect deviations through a population distribution.', 1.35, { gap: .30 }),
      text('Express the average treatment effect in the population represented by those groups.', 1.20)
    ] }
  ], 'We start with fixed effects for groups, using a multicenter clinical trial.', {
    intro: 'Donors, hospitals and other groups can differ in baseline outcome and treatment effect.'
  });
  representationSlide.addNotes('');

  addSectionSlide(pptx, {
    sectionLabel: 'PART 1',
    sectionTitle: 'Treatment-by-center interaction: fixed effects'
  });

  const empiricalSlide = addBulletVisualSlide(pptx, {
    title: 'Case study: CPAP and blood pressure',
    bulletTitle: 'A SIMULATED MULTICENTER TRIAL',
    bullets: [
      '240 patients with sleep apnea and hypertension despite medication, across six hospitals.',
      'Hospital sizes: 24, 32, 36, 44, 48 and 56; equal treatment allocation within each.',
      'Control: continue usual blood-pressure medication.',
      'CPAP: add continuous positive airway pressure through a mask during sleep.',
      'Outcome: reduction in 24-hour mean blood pressure after 12 weeks (mmHg).'
    ],
    bulletFontSize: 20, bulletMinFontSize: 20,
    assetPath: path.join(assets, 'cpap-case-study.png'),
    fit: 'cover',
    note: 'Positive values indicate a blood-pressure reduction. Inspired by HIPARCO (Martínez-García et al., JAMA, 2013); simulated patient data.',
    noteFontSize: 17, noteHeight: 1.0
  });
  empiricalSlide.addNotes('');

  addFigureSlide(pptx, {
    title: 'Blood-pressure reduction by treatment and hospital',
    assetPath: path.join(assets, 'interaction-outcomes-slide.png')
  });

  const fixedSpecificationSlide = panels('A fixed-effects model for treatment and center', [
    { title: 'CONTROL OUTCOMES', blocks: [
      text('β₀: average control-group reduction across the included centers.', .90, { fontSize: 21 }),
      text('cⱼ: center j’s deviation from that control average.', .85, { fontSize: 21 }),
      eq('centerConstraint', .72, .10)
    ] },
    { title: 'TREATMENT EFFECTS', blocks: [
      text('β₁: average treatment effect across the included centers.', .90, { fontSize: 21 }),
      text('dⱼ: center j’s deviation from the average treatment effect.', .85, { fontSize: 21 }),
      eq('interactionConstraint', .72, .10)
    ] }
  ], 'Each center receives equal weight in the averages. Center deviations are fixed, unknown parameters.', {
    lead: { equation: E.model, text: 'Yᵢⱼ: blood-pressure reduction (mmHg). Tᵢⱼ = 0 for control and 1 for CPAP.' }
  });
  fixedSpecificationSlide.addNotes('');

  const coefficientSlide = addTeachingTableSlide(pptx, {
    title: 'Fixed-effects interaction model: results',
    columns: [
      { label: 'Term', weight: 3.9 },
      { label: 'Estimate\n(mmHg)', weight: 1.8, align: 'center' },
      { label: 'Estimated\nSE (mmHg)', weight: 1.8, align: 'center' },
      { label: '95% CI (mmHg)', weight: 2.9, align: 'center' },
      { label: 'p-value', weight: 1.6, align: 'center' }
    ],
    rows: results.regression_coefficients.map(row => [
      row.label, f(row.estimate), f(row.standard_error, 2),
      `${f(row.lower_95)} to ${f(row.upper_95)}`,
      row.p < .001 ? '< .001' : f(row.p, 3)
    ]),
    fontSize: 18,
    tableOptions: { headerFontSize: 16, headerHeight: .62, cellPaddingY: .035, firstColumnBold: false },
    takeaway: 'Effects coding: Hospital F follows from the sum-to-zero constraints.',
    takeawayHeight: .60
  });
  coefficientSlide.addNotes('');

  const hospitals = ['A', 'B', 'C', 'D', 'E', 'F'];
  const emmConceptSlide = panels('Estimated marginal means', [
    { title: '2. PREDICT EACH CELL MEAN', blocks: [
      text('Use the fitted model for each treatment–hospital combination.', .58, { fontSize: 20, gap: .15 }),
      { ...eq('emmPrediction', .48, .20), equationAlign: 'left' },
      text('t = 0 for Control; t = 1 for CPAP.\nj identifies the hospital.', .72, { fontSize: 20 })
    ] },
    { title: '3. AVERAGE ACROSS HOSPITALS', blocks: [
      text('Average each row, giving all six hospitals equal weight.', .58, { fontSize: 20, gap: .10 }),
      { ...eq('emmAverage', .85, .08), equationAlign: 'left' },
      text('This gives one estimated marginal mean per treatment.', .55, { fontSize: 20 })
    ] }
  ], undefined, {
    intro: 'Estimated marginal means (EMMs) are model-based means, averaged over selected factors.',
    grid: { title: '1. DEFINE THE TREATMENT–HOSPITAL GRID', height: 1.40,
      table: {
        columns: [
          { label: 'Treatment', weight: 1.5 },
          ...hospitals.map(label => ({ label, weight: 1, align: 'center' })),
          { label: '', weight: .4, align: 'center' },
          { label: 'Row average', weight: 1.6, align: 'center' }
        ],
        rows: [0, 1].map(t => [t === 0 ? 'Control' : 'CPAP',
          ...hospitals.map(j => ({ equation: E[`grid${t}${j}`], h: .35 })),
          '→', { equation: E[`grid${t}Average`], h: .35 }]),
        fontSize: 20, headerFontSize: 18, headerHeight: .45,
        cellPaddingY: .06, cellPaddingX: .10, firstColumnBold: false
      }
    }
  });
  emmConceptSlide.addNotes('');

  const effectInterpretation = (description, key) => ({
    inlineMath: [
      { text: description },
      { image: path.join(__dirname, 'assets/equations', `eq_${crypto.createHash('sha1').update(E[key]).digest('hex').slice(0, 12)}.png`),
        heightEm: 1.15, baselineFraction: .85 },
      { text: '.' }
    ], h: 1.45, fontSize: 21
  });
  const effectConceptSlide = panels('Which treatment effect do we want to estimate?', [
    { title: 'MARGINAL TREATMENT EFFECT', blocks: [
      text('How do treatments compare on average across hospitals?', .95, { bold: true, gap: .25 }),
      text('Compare the two treatment means averaged equally across the six hospitals.', 1.15, { fontSize: 21, gap: .30 }),
      effectInterpretation('With our effects-coded model, the estimated marginal treatment effect is ', 'estimatedMarginalEffect')
    ] },
    { title: 'SIMPLE TREATMENT EFFECTS', blocks: [
      text('How do treatments compare within a particular hospital?', .95, { bold: true, gap: .25 }),
      text('Compare the estimated CPAP and control means within that hospital.', 1.15, { fontSize: 21, gap: .30 }),
      effectInterpretation('With our effects-coded model, the estimated simple treatment effect in hospital j is ', 'estimatedSimpleEffect')
    ] }
  ]);
  effectConceptSlide.addNotes('');

  // Keep fitted means and their within-hospital contrasts together, with inference.
  const cell = (center, treatment) => results.fitted_cell_means.find(r => r.center === center && r.treatment === treatment).estimate;
  const simpleEffectsSlide = addTeachingTableSlide(pptx, {
    title: 'Estimated means and simple treatment effects',
    columns: [
      { label: 'Hospital', weight: 1, align: 'center' },
      { label: 'Control', weight: 1.3, align: 'center' },
      { label: 'CPAP', weight: 1.3, align: 'center' },
      { label: 'Difference', weight: 1.4, align: 'center', highlight: true },
      { label: '95% CI', weight: 2, align: 'center', highlight: true },
      { label: 'p-value', weight: 1.3, align: 'center', highlight: true }
    ],
    rows: results.within_center_comparisons.map(r => [
      r.center.replace('Center ', ''), f(cell(r.center, 'Control')), f(cell(r.center, 'CPAP')),
      f(r.estimate), `${f(r.lower_95)} to ${f(r.upper_95)}`,
      r.p < .001 ? '< .001' : f(r.p, 3)
    ]),
    fontSize: 23,
    tableOptions: {
      columnGroups: [{ label: '', count: 1 }, { label: 'Estimated means', count: 2 }, { label: 'Simple treatment effect', count: 3 }],
      headerFontSize: 20, headerHeight: .60, groupHeaderHeight: .48,
      cellPaddingX: .12, cellPaddingY: .06, firstColumnBold: false
    },
    takeaway: 'Means, differences and CIs in mmHg. Difference: CPAP minus control.\nPointwise 95% CIs and unadjusted p-values.',
    takeawayHeight: .90
  });
  simpleEffectsSlide.addNotes('');

  // Close the case study by checking residuals from the full interaction model.
  const diagnosticsSlide = addDiagnosticsInterpretationSlide(pptx, {
    title: 'Model diagnostics: fixed-effects interaction',
    plotHeight: 3.0,
    diagnostics: [
      { assetPath: path.join(assets, 'interaction-residuals-vs-fitted.png'),
        purpose: 'Assess whether residual spread is similar across fitted blood-pressure reductions.' },
      { assetPath: path.join(assets, 'interaction-normal-qq.png'),
        purpose: 'Assess whether the residual distribution is approximately Normal.' }
    ],
    takeaway: 'Residual spread is broadly similar across fitted values, and points lie close to the Normal reference line.'
  });
  diagnosticsSlide.addNotes('');

  const lab3Slide = addFigureSlide(pptx, {
    title: 'Time for Lab 3',
    assetPath: path.join(__dirname, 'assets/lab3-debrief/lab3-break.png')
  });
  lab3Slide.addNotes('');

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
  omissionSlide.addNotes('');

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
  sizeSlide.addNotes('');

  // Part 2: a bridge to the later random-slope fitting section.
  addSectionSlide(pptx, {
    sectionLabel: 'PART 2',
    sectionTitle: 'Treatment-by-center interaction: random effects'
  });
  const populationTargetSlide = panels('From the included centers to a population', [
    { title: 'THESE SIX HOSPITALS', blocks: [
      text('Imagine repeating the study with new patients in the same six hospitals.', 1.0, { gap: .25 }),
      text('The centers’ true treatment effects stay the same.', 1.0, { gap: .25 }),
      text('We estimate the average effect in these six hospitals.', 1.15)
    ] },
    { title: 'A POPULATION OF CENTERS', blocks: [
      text('Imagine repeating the study with new centers from the same population.', 1.0, { gap: .25 }),
      text('The selected centers can have different true treatment effects.', 1.0, { gap: .25 }),
      text('We estimate the average effect across the population of centers.', 1.15)
    ] }
  ], 'When we generalize to a population of centers, uncertainty also depends on which centers enter the study.');
  populationTargetSlide.addNotes('');

  const sampleGridSlide = addFigureSlide(pptx, {
    title: 'What changes when we sample new centers?',
    assetPath: path.join(__dirname, 'assets/random-center-samples/two-center-samples.png'),
    caption: 'Hypothetical true mean blood-pressure reductions (mmHg) across hospitals.',
    captionFontSize: 18, captionMinFontSize: 18, captionHeight: .42,
    takeaway: 'The population model stays the same, but each sample contains centers with different realized effects.',
    takeawayHeight: .82
  });
  sampleGridSlide.addNotes('');

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
      varianceLabel('centerVarianceSymbol', 'variance of control means across hospitals.'),
      varianceLabel('slopeVarianceSymbol', 'variance of treatment effects across centers.'),
      varianceLabel('covarianceSymbol', 'covariance of control-mean and treatment-effect deviations.'),
      varianceLabel('errorVarianceSymbol', 'individual variance around the center-specific treatment mean.')
    ] }
  ], 'Center deviations have population means of zero. Their average varies across samples of hospitals.', {
    lead: { equation: E.model, text: 'β₀ and β₁ are population averages. Tᵢⱼ = 0 (control) or 1 (CPAP).' }
  });
  randomModelSlide.addNotes('');

  const covarianceSlide = addFigureSlide(pptx, {
    title: 'How do control outcomes and treatment effects vary together?',
    assetPath: path.join(__dirname, 'assets/random-center-samples/covariance-examples.png'),
    caption: 'True control means and CPAP-minus-control effects (mmHg). Each point is one hypothetical hospital.',
    captionFontSize: 18, captionMinFontSize: 18, captionHeight: .42,
    takeaway: 'Covariance describes how a hospital’s control outcome and treatment effect vary together.',
    takeawayHeight: .82
  });
  covarianceSlide.addNotes('');

  addRandomInteractionResults(pptx, drawEquation, randomInteraction, E, results.interaction_model);

}

module.exports = { addWeek2Opening, equations: Object.values(E) };
