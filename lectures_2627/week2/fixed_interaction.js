const path = require('path');
const { addConceptPanelsSlide } = require('../../slides-common/js/concept_panels');
const { addTeachingTableSlide } = require('../../slides-common/js/teaching_table');
const { addFigureSlide, addSectionSlide } = require('../../slides-common/js/pptx_layout_primitives');
const { addAllocationImbalance } = require('./allocation_imbalance');

const E = {
  additive: String.raw`Y_{ij}=\beta_0+c_j+\beta_1T_{ij}+\varepsilon_{ij}`,
  model: String.raw`Y_{ij}=\beta_0+c_j+(\beta_1+d_j)T_{ij}+\varepsilon_{ij}`,
  errors: String.raw`\varepsilon_{ij}\overset{\mathrm{iid}}{\sim}N(0,\sigma^2)`,
  constraints: String.raw`\sum_{j=1}^{J}c_j=0,\qquad \sum_{j=1}^{J}d_j=0`,
  standard: String.raw`\mu_{0j}=\beta_0+c_j`,
  enhanced: String.raw`\mu_{1j}=\beta_0+c_j+\beta_1+d_j`,
  effect: String.raw`\delta_j=\mu_{1j}-\mu_{0j}=\beta_1+d_j`,
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
  addFigureSlide(pptx, {
    title: 'Restore balanced allocation and let treatment effects vary',
    assetPath: path.join(assets, 'interaction-outcomes-slide.png'),
    caption: 'New simulated trial: 120 patients, six centers, 10 per treatment per center. Diamonds are observed treatment means.',
    captionFontSize: 19, captionMinFontSize: 19, captionHeight: .70
  });

  // 4–5: Define the complete fixed model and the chosen parameterization.
  panels('Extend the fixed-effects model', [
    { title: 'A TREATMENT DIFFERENCE FOR EACH CENTER', blocks: [
      eq('model', .65),
      text('Tᵢⱼ = 0 for standard care and 1 for enhanced care.', .80),
      eq('effect', .60),
      text('The interaction contribution dⱼ allows the expected treatment difference to vary by center.', 1.15)
    ] },
    { title: 'FIXED PARAMETERS AND INDIVIDUAL ERRORS', blocks: [
      text('Each cⱼ and dⱼ is a fixed, unknown parameter for one of the six included centers.', 1.05),
      eq('errors', .65),
      text('After accounting for the center-specific treatment means, individual errors are independent with a common variance.', 1.30)
    ] }
  ], 'We retain the Week 1 center effects and add center-specific deviations from the average treatment effect.');

  panels('Effects coding defines the average and the deviations', [
    { title: 'THE AVERAGE PARAMETERS', blocks: [
      eq('beta0', .70),
      text('β₀: expected standard-care outcome, averaged equally across the included centers.', 1.05),
      eq('beta1', .70),
      text('β₁: treatment difference, averaged equally across those same centers.', 1.05)
    ] },
    { title: 'CENTER-SPECIFIC DEVIATIONS', blocks: [
      eq('deviations', .65),
      text('cⱼ and dⱼ describe a center’s deviations in standard-care mean and treatment effect.', 1.00),
      eq('constraints', .65),
      text('Center F’s deviations are minus the sums of the corresponding five estimated deviations.', 1.05)
    ] }
  ], 'Treatment uses dummy coding; center uses effects coding, as in Week 1.');

  // 6: Model predictions provide the reference grid for estimated marginal means.
  const cell = (center, treatment) => results.fitted_cell_means.find(r => r.center === center && r.treatment === treatment).estimate;
  addTeachingTableSlide(pptx, {
    title: 'Interpret the interaction through fitted means',
    intro: 'Predict the mean for each treatment–center combination: the reference grid used by estimated marginal means.',
    introHeight: .65,
    columns: [
      { label: 'Center', weight: 1.4 },
      { label: 'Standard (m)', weight: 2, align: 'center' },
      { label: 'Enhanced (m)', weight: 2, align: 'center' },
      { label: 'Difference (m)', weight: 2, align: 'center' }
    ],
    rows: results.within_center_comparisons.map(r => [r.center, f(cell(r.center, 'Standard')), f(cell(r.center, 'Enhanced')), f(r.estimate)]),
    takeaway: 'The full interaction model reproduces the observed cell means. Subtract within each center to obtain its treatment comparison.',
    fontSize: 21
  });

  // 7: Keep the visible heterogeneity and the uncertainty about each contrast together.
  addFigureSlide(pptx, {
    title: 'Treatment differences and their uncertainty',
    assetPath: path.join(assets, 'interaction-contrasts-slide.png'),
    caption: 'Enhanced minus standard care. Bars: pointwise 95% confidence intervals, unadjusted. Dashed line: average estimate.',
    captionFontSize: 19, captionMinFontSize: 19, captionHeight: .70
  });

  // 8: State the estimand before deriving its uncertainty.
  addSectionSlide(pptx, {
    sectionLabel: 'PART 2',
    sectionTitle: 'Uncertainty in the included centers'
  });
  panels('Which average treatment effect are we estimating?', [{ blocks: [
    eq('beta1', .65, .25),
    text('Our current target is the equally weighted average true treatment effect in these six centers.', .85, { gap: .20 }),
    eq('average', .65, .25),
    text('Dⱼ is the observed enhanced-minus-standard difference in center j. Here, their average is 21.27 m.', .85, { gap: .20 }),
    text('Averaging fitted means over centers and then contrasting the treatments gives the same estimate.', .65)
  ] }], 'An average remains well defined when treatment effects differ. The weighting and the target centers must be explicit.');

  // 9–11: Derive the fixed-target uncertainty using the Week 1 linear-combination rules.
  panels('Within a center, the baseline contribution cancels', [{ blocks: [
    eq('difference', .60, .20),
    eq('expansion', 1.60, .15),
    text('Both treatment groups share β₀ + cⱼ, so these terms cancel. The center-specific treatment deviation dⱼ remains.', 1.0, { gap: .18 }),
    text('Each barred epsilon averages the individual errors in one treatment group within that center.', .75)
  ] }], 'The within-center comparison removes baseline differences, but it retains differences in treatment effects.');

  panels('Uncertainty in one center’s treatment difference', [
    { title: 'INDIVIDUAL SAMPLING NOISE', blocks: [
      eq('differenceNoise', .65),
      eq('noise', .60),
      text('For this fixed center, β₁ + dⱼ remains constant across repeated samples of patients.', 1.15),
      text('Only the individual errors contribute to the sampling variability of Dⱼ.', 1.0)
    ] },
    { title: 'APPLY THE VARIANCE RULES', blocks: [
      text('Let n be the number of patients per treatment per center; here n = 10.', .95),
      eq('errorVariance', .70),
      text('The two groups have independent errors, so the variance of their difference is:', .95),
      eq('noiseVariance', .85)
    ] }
  ], 'Differences between the centers’ true treatment effects do not add sampling variation within a fixed center.');

  panels('Uncertainty in the average across fixed centers', [{ blocks: [
    text('Average the J independent center comparisons. The fixed treatment deviations sum to zero.', .70, { gap: .12 }),
    eq('fixedExpansion', .65, .20),
    eq('fixedVariance', .85, .25),
    text('The individual errors are the only source of sampling variation around the true average for these centers.', .85, { gap: .20 }),
    text('More patients in these same centers can make this average increasingly precise.', .75)
  ] }], 'Conditioning on the included centers, treatment heterogeneity adds no extra variance term for their average.');

  // 12: Connect the theoretical SE to the fitted residual variance.
  panels('Estimate the standard error from individual variation', [
    { title: 'FROM THE MODEL RESIDUALS', blocks: [
      text('The model fits a separate mean for every treatment–center combination.', .90),
      text(`Residual standard deviation: ${f(m.residual_sd, 2)} m, estimating the individual-error standard deviation σ.`, .95),
      eq('estimatedSE', .90),
      text('Use J = 6 centers and n = 10 patients per treatment per center.', .75)
    ] },
    { title: 'THE AVERAGE TREATMENT COMPARISON', blocks: [
      text(`Estimated difference: ${f(m.estimate, 2)} m`, .65, { bold: true, gap: .25 }),
      text(`Estimated standard error: ${f(m.standard_error, 2)} m`, .65, { gap: .25 }),
      text(`95% confidence interval:\n${f(m.lower_95, 2)} to ${f(m.upper_95, 2)} m`, .95, { gap: .25 }),
      text('The t interval uses 108 residual degrees of freedom: 120 patients minus 12 fitted cell means.', .95)
    ] }
  ], 'This interval describes uncertainty about the true average treatment effect in the six included centers.');

  // 13: Explain unchanged point estimates before showing the changed estimated SE.
  panels('What if we omit the fixed interaction?', [{ blocks: [
    { table: {
      columns: [
        { label: 'Fixed-effects model', weight: 3.2 },
        { label: 'Estimate (m)', weight: 1.7, align: 'center' },
        { label: 'Estimated SE (m)', weight: 2.0, align: 'center' }
      ], rows: [
        ['Treatment + center', f(a.estimate, 2), f(a.standard_error, 2)],
        ['Treatment × center', f(m.estimate, 2), f(m.standard_error, 2)]
      ], fontSize: 22, headerFontSize: 18, headerHeight: .64
    }, h: 1.65, gap: .25 },
    text('Exact balance keeps the estimate equal to the average of the six observed treatment differences.', .70, { gap: .15 }),
    text('Omitting the interaction leaves systematic treatment-by-center differences in the residuals.', .70, { gap: .15 }),
    text('This inflates the estimated residual variance in expectation, and therefore the usual estimated variance of the average.', .85)
  ] }], 'Here the estimated standard error changes; the average estimator and its actual sampling variance do not.');

  // 14–17: A theoretical bridge to the later random-slope fitting section.
  addSectionSlide(pptx, {
    sectionLabel: 'PART 3',
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
    text('Assume the random center effects are independent of the individual errors. Then:', .55),
    eq('populationVariance', .65),
    text('The two variance terms reflect center sampling and individual sampling noise.', .50)
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

module.exports = { addWeek2Opening, equations: Object.values(E) };
