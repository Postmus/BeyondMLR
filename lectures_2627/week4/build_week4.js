const path = require('node:path');
const pptxgen = require('pptxgenjs');
const { FONT_FAMILIES } = require('../../slides-common/js/blr_theme');
const {
  defineTemplateMasters, addBrandedTitleSlide, addBase, addSlideTitle, addFooter, addFigureSlide, addSectionSlide,
} = require('../../slides-common/js/pptx_layout_primitives');
const { addStudyOverviewSlide } = require('../../slides-common/js/course_builders');
const { addConceptPanelsSlide } = require('../../slides-common/js/concept_panels');
const { addTeachingTableSlide } = require('../../slides-common/js/teaching_table');
const { writePresentation } = require('../../slides-common/js/write_presentation');
const { warnIfSlideHasOverlaps, warnIfSlideElementsOutOfBounds } = require('../week1/pptxgenjs_helpers/layout');
const r = require('./assets/growth_results.json');

const pptx = new pptxgen();
pptx.layout = 'LAYOUT_WIDE';
pptx.author = 'Dr Douwe Postmus';
pptx.title = 'Beyond MLR | Week 4 | Modeling change over time';
pptx.subject = 'Longitudinal models for infant growth and adolescent alcohol use';
pptx.lang = 'en-GB';
pptx.theme = { headFontFace: FONT_FAMILIES.sans, bodyFontFace: FONT_FAMILIES.sans, lang: 'en-GB' };
defineTemplateMasters(pptx);

const asset = name => path.join(__dirname, 'assets', name + '.png');
const fmt = (value, digits = 2) => value.toFixed(digits).replace('-', '−');
const text = (value, fontSize = 21) => ({ text: value, fontSize });
const metric = value => ({ text: value, bold: true, fontSize: 25, gap: .09 });
const plot = (name, h) => ({ image: asset(name), h });
const col = (label, weight = 1) => ({ label, weight, align: 'center' });
const panel = (title, ...blocks) => ({ title, blocks });
const panels = options => addConceptPanelsSlide(pptx, options);
function figure(title, name, caption, takeaway) {
  addFigureSlide(pptx, { title, assetPath: asset(name), caption,
    captionFontSize: 17, captionMinFontSize: 17, captionHeight: .49,
    takeaway, takeawayHeight: .92 });
}
function table(columns, rows, h, options = {}) {
  return { h, table: { columns, rows, fontSize: 19, headerFontSize: 17,
    cellPaddingX: .07, cellPaddingY: .05, firstColumnBold: false, ...options } };
}

addBrandedTitleSlide(pptx, {
  courseLabel: 'BEYOND MULTIPLE LINEAR REGRESSION · WEEK 4',
  mainTitle: 'Modeling change over time', subtitle: 'Infant growth and adolescent alcohol use',
  figurePath: asset('opening'), figureCaption: 'OBSERVED HEIGHTS OF THREE BOYS',
});

{
  const slide = pptx.addSlide();
  addBase(slide);
  addSlideTitle(slide, 'Assignment 2 debrief');
  addFooter(slide);
}

addStudyOverviewSlide(pptx, {
  title: 'Infant growth during the first year',
  studyTitle: 'THE INFANT GROWTH STUDY',
  bullets: [
    `Researchers measured height repeatedly in ${r.children} boys between ${r.age_range[0]} and ${r.age_range[1]} weeks of age.`,
    'They also recorded each boy’s birth weight.',
  ],
  question: [
    'How does height change with age, and how much do growth trajectories vary between boys?',
    'Is birth weight associated with the mean growth trajectory, and if so, does that association change with age?',
  ],
  questionHeight: 2.85,
  assetPath: asset('infant_length_visit'),
});

addSectionSlide(pptx, { sectionLabel: 'PART 1', sectionTitle: 'Modeling growth over time' });

figure('Exploratory data analysis', 'observed',
  'Green lines connect observed visits for six boys. The dark curve smooths measurements from all 100 boys.',
  'What shape does the mean growth trajectory have? How closely do the individual boys’ trajectories follow that shape?');

panels({
  title: 'A time-only model with a random intercept',
  intro: 'For child i at visit j, let tᵢⱼ be age in weeks minus 26. At 26 weeks, tᵢⱼ = 0.',
  panels: [
    panel('POPULATION MEAN GROWTH TRAJECTORY',
      metric('β₀ + β₁tᵢⱼ'),
      text('β₀ is the mean height at 26 weeks, in centimeters.'),
      text('β₁ is the average gain in height per week. The model assumes a common linear growth rate.')),
    panel('HEIGHT AT ONE VISIT',
      metric('Heightᵢⱼ = β₀ + β₁tᵢⱼ + b₀ᵢ + εᵢⱼ'),
      text('The random intercept b₀ᵢ is the child’s deviation from the mean height at 26 weeks. This shift applies at every visit.'),
      text('The visit error εᵢⱼ is the deviation of the measured height from the child’s line.')),
  ],
});

const fixedTerms = {
  '(Intercept)': 'Intercept at 26 weeks (cm)',
  t: 'Weekly height gain (cm/week)',
};
const meanHeight = r.ri_fixed.find(row => row.term === '(Intercept)');
const weeklyGain = r.ri_fixed.find(row => row.term === 't');
addTeachingTableSlide(pptx, {
  title: 'Fixed effects of the random-intercept model',
  columns: [{ label: 'Fixed effect', weight: 2.95 }, col('Estimate'), col('SE'),
    col('95% CI', 1.75), col('p-value')],
  rows: r.ri_fixed.map(row => {
    const digits = row.term === 't' ? 3 : 2;
    return [fixedTerms[row.term], fmt(row.estimate, digits), fmt(row.se, digits),
      `${fmt(row.lower, digits)} to ${fmt(row.upper, digits)}`, row.p < .001 ? '< .001' : fmt(row.p, 3)];
  }),
  tableOptions: { firstColumnBold: false, headerFontSize: 19 },
  takeaway: `The estimated mean height at 26 weeks is ${fmt(meanHeight.estimate, 1)} cm. The model estimates an average gain of ${fmt(weeklyGain.estimate)} cm per week.`,
  takeawayHeight: 1.10,
});

const childVariance = r.ri_variance.find(row => row.grp === 'id');
const visitVariance = r.ri_variance.find(row => row.grp === 'Residual');
panels({
  title: 'Interpreting variation in the random-intercept model',
  panels: [
    panel('SOURCES OF VARIATION',
      table([{ label: 'Source', weight: 1.65 }, col('Variance (cm²)', 1.35), col('SD (cm)')], [
        ['Child intercept', fmt(childVariance.vcov), fmt(childVariance.sdcor)],
        ['Visit residual', fmt(visitVariance.vcov), fmt(visitVariance.sdcor)],
      ], 2.20),
      text(`The intercept SD (${fmt(childVariance.sdcor)} cm) describes the spread of boys’ trajectories around the population mean line.`, 19),
      text(`The residual SD (${fmt(visitVariance.sdcor)} cm) describes the spread of observed heights around each boy’s fitted line.`, 19)),
    panel('OBSERVED HEIGHTS AND FITTED LINES', plot('intercept', 3.45),
      text('The dashed line is the population mean. Each boy has a parallel fitted line. Vertical segments show the residuals at his visits.', 18)),
  ],
});

panels({
  title: 'Checking the fit of the linear age trend',
  panels: [
    panel('CONDITIONAL RESIDUALS BY AGE', plot('residual_linear', 3.55),
      text('Each dot is one visit. The curve smooths the residuals across ages.', 18)),
    panel('READ THE PATTERN',
      text('A conditional residual is observed height minus the height predicted for that child at that visit.'),
      text('If a linear age trend describes mean growth well, the smoothed residuals should stay close to zero across ages.'),
      text('Here the curve dips below zero at early and late ages and rises above zero in the middle. The model overpredicts height at the ends of the age range.')),
  ],
});

panels({
  title: 'A quadratic age trend for infant height',
  intro: 'Let t be age in weeks minus 26. Adding t² lets the rate of growth change with age.',
  panels: [
    panel('POPULATION MEAN GROWTH TRAJECTORY',
      metric('β₀ + β₁t + β₂t²'),
      text('At 26 weeks, β₀ is the mean height and β₁ is the growth rate in cm/week.'),
      text('At other ages, the growth rate is β₁ + 2β₂t. Each additional week changes the rate by 2β₂.')),
    panel('GROWTH TRAJECTORY FOR CHILD i',
      text('(β₀ + b₀ᵢ) + β₁t + β₂t²', 23),
      text('The random intercept shifts the whole curve up or down by the same amount at every age.'),
      text('All children share the same linear and quadratic age coefficients. The visit error is the difference between the measured height and the child’s curve.')),
  ],
});

const quadTerms = {
  '(Intercept)': ['26-week height\n(cm)', 2],
  t: ['Rate at 26 weeks\n(cm/week)', 3],
  'I(t^2)': ['Squared age (cm/week²)', 4],
};
function quadraticFixedTable(results) {
  const rows = results.map(row => {
    const [label, digits] = quadTerms[row.term];
    return [label, fmt(row.estimate, digits), fmt(row.se, digits),
      `${fmt(row.lower, digits)} to ${fmt(row.upper, digits)}`, row.p < .001 ? '< .001' : fmt(row.p, 3)];
  });
  return table([{ label: 'Fixed effect', weight: 1.95 }, col('Estimate', 1.03), col('SE', .88),
    col('95% CI', 1.73), col('p-value', .91)], rows, 3.35,
    { fontSize: 17, headerFontSize: 16, cellPaddingX: .04 });
}
const curvature = r.quad_fixed.find(row => row.term === 'I(t^2)').estimate;
panels({
  title: 'Fixed effects of the quadratic growth model',
  panels: [
    panel('FITTED MEAN HEIGHT', plot('curvature', 3.40)),
    { title: 'FIXED EFFECTS OF THE QUADRATIC MODEL', weight: 1.72, blocks: [
      quadraticFixedTable(r.quad_fixed),
    ] },
  ],
  takeaway: `The estimated growth rate falls by ${fmt(-2 * curvature, 3)} cm/week with each additional week of age (twice the magnitude of the squared-age coefficient).`,
  takeawayHeight: 1.09,
});

panels({
  title: 'Comparing predicted gains over eight weeks',
  panels: [
    panel('PREDICTED MEAN GAIN',
      table([col('Age interval'), col('Height gain')], [
        ['12–20 weeks', fmt(r.gain[0], 1) + ' cm'], ['36–44 weeks', fmt(r.gain[1], 1) + ' cm'],
      ], 2.02),
      text('Fitted gain = end height − start height.')),
    panel('RESIDUALS AFTER ADDING CURVATURE', plot('residual_quadratic', 3.12),
      text('The smooth curve now stays much closer to zero across ages.', 18)),
  ],
  takeaway: `Over the same eight-week span, estimated growth is ${fmt(r.gain[0] - r.gain[1], 1)} cm greater in the earlier interval.`,
});

addFigureSlide(pptx, {
  title: 'Specifying three random-effects structures', assetPath: asset('random_structures'),
  caption: 'Follow the height gap between two illustrative boys (dashed segments). It can stay constant, change by a constant amount each week, or change by a different amount each week.',
  captionFontSize: 17, captionMinFontSize: 17, captionHeight: .78,
  takeaway: 'The residuals support a quadratic mean trajectory. Keeping the same fixed age terms lets us isolate what each random term adds to the description of variation between boys.',
  takeawayHeight: .96,
});

panels({
  title: 'Variance components of the full growth model',
  panels: [
    { title: 'BETWEEN-CHILD VARIATION', weight: 1.35, blocks: [
      table([{ label: 'Child effect', weight: 1.35 }, col('SD', 1.40), col('Corr. with\nintercept', 1.05),
        col('Corr. with\nlinear age', 1.05)], [
        ['Intercept', fmt(r.full_sd[0]) + ' cm', '', ''],
        ['Linear age', fmt(r.full_sd[1], 3) + ' cm/wk', fmt(r.full_cor[1][0]), ''],
        ['Quadratic age', fmt(r.full_sd[2], 5) + ' cm/wk²', fmt(r.full_cor[2][0]), fmt(r.full_cor[2][1])],
      ], 3.00, { headerHeight: .78, fontSize: 17, headerFontSize: 16 }),
      text(`Visit residual SD = ${fmt(r.full_residual_sd)} cm.`, 19),
    ] },
    panel('CHILD-SPECIFIC CURVATURE',
      text('The full model gives each boy his own intercept, linear age coefficient and quadratic age coefficient.'),
      text('Allowing curvature to vary adds three parameters. We estimate its variance and its correlations with the intercept and linear age term.', 20)),
  ],
  takeaway: 'Does allowing curvature to vary between boys improve the fit enough to justify those three parameters?',
});

panels({
  title: 'Comparing models with AIC',
  intro: 'Akaike’s information criterion (AIC) balances fit to the observed data against the number of estimated parameters.',
  introHeight: .65,
  panels: [
    panel('HOW THE SCORE WORKS',
      metric('AIC = −2 × log likelihood + 2k'),
      text('A better fit lowers the first term. The model has k estimated parameters, so each one adds two points to the penalty.', 20),
      text('Extra terms can fit chance patterns. The penalty helps estimate how well the model would fit new data.', 20)),
    panel('HOW TO COMPARE MODELS',
      text('Fit plausible models to the same outcome and the same observations.', 20),
      text('Lower AIC indicates a better balance between fit and complexity.', 20),
      text('When two models have nearly identical AIC values, the simpler model is a reasonable choice.', 20)),
  ],
});

panels({
  title: 'Choosing REML or ML for model comparison',
  panels: [
    panel('COMPARING RANDOM EFFECTS',
      metric('lmer(..., REML = TRUE)'),
      text('Restricted maximum likelihood accounts for estimating the population mean when estimating variances.', 20),
      text('Use identical fixed effects in every candidate. Here, each model includes age and age squared.', 20)),
    panel('COMPARING FIXED EFFECTS',
      metric('lmer(..., REML = FALSE)'),
      text('Changing the fixed effects changes the restricted likelihood. Maximum likelihood gives the models a common basis for comparison.', 20),
      text('Keep the random effects identical and fit every candidate using ML.', 20)),
  ],
  takeaway: 'Fit all candidates in a comparison with the same method, then use AIC() in R. Fit the selected model with REML to report its estimates.',
  takeawayHeight: 1.05,
});

const candidateNames = {
  full: 'Intercept + linear age + quadratic age',
  slope: 'Intercept + linear age',
  intercept: 'Intercept only',
};
addTeachingTableSlide(pptx, {
  title: 'Selecting the random effects for infant growth',
  intro: 'All three models use REML, the same visits and the same quadratic mean trajectory.',
  columns: [{ label: 'Random effects for each child', weight: 3.15 }, col('Parameters', 1.10),
    col('AIC')],
  rows: r.comparison.map(row => [candidateNames[row.model], String(row.df), fmt(row.AIC)]),
  tableOptions: { firstColumnBold: false, headerFontSize: 19 },
  takeaway: 'The intercept-only model has the highest AIC. The two slope models have nearly identical AICs, so we keep the simpler model with a random intercept and a random linear slope.',
  takeawayHeight: 1.10,
});

const interceptSlopeCorrelation = (r.correlation > 0 ? '+' : '') + fmt(r.correlation);
panels({
  title: 'Interpreting the selected random-slope model',
  panels: [
    panel('ESTIMATED VARIANCE COMPONENTS',
      table([{ label: 'Component', weight: 1.18 }, col('Variance', .92), col('SD', 1.12),
        col('Corr. with\nintercept', 1.00)], [
        ['Child intercept', fmt(r.rs_intercept_sd ** 2), fmt(r.rs_intercept_sd) + ' cm', ''],
        ['Child age slope', fmt(r.slope_sd ** 2, 5), fmt(r.slope_sd, 3) + ' cm/wk', interceptSlopeCorrelation],
        ['Visit residual', fmt(r.rs_residual_sd ** 2), fmt(r.rs_residual_sd) + ' cm', ''],
      ], 3.20, { headerHeight: .78, fontSize: 17, headerFontSize: 16, cellPaddingX: .04 }),
      text('Height variances are in cm². The slope variance is in (cm/week)².', 17)),
    panel('INTERPRETING THE CHILD AGE SLOPE',
      { text: 'How much do growth rates vary?', fontSize: 22, bold: true },
      text(`At any age, the between-child SD in growth rate is ${fmt(r.slope_sd, 3)} cm/week. A boy one SD above the mean rate gains ${fmt(14 * r.slope_sd)} cm more than average over 14 weeks.`, 20),
      { text: 'How is height at 26 weeks related to growth rate?', fontSize: 22, bold: true },
      text(`The intercept–slope correlation is ${interceptSlopeCorrelation}. In this model, boys who are taller at 26 weeks tend to grow slightly faster.`, 20)),
  ],
});

panels({
  title: 'Fitted growth trajectories for two boys and the population mean',
  panels: [
    { title: 'FIXED EFFECTS FOR THE POPULATION MEAN', weight: 1.5,
      blocks: [quadraticFixedTable(r.selected_fixed),
        text('Mean growth slows with age. The boys share the same curvature and can differ in how much height they gain over eight weeks.', 19)] },
    panel('TWO BOYS’ FITTED TRAJECTORIES', plot('selected_trajectories', 3.1),
      text('Dots mark observed heights. Each solid curve is one boy’s fitted trajectory. The dashed curve is the population mean.', 18)),
  ],
});

addSectionSlide(pptx, { sectionLabel: 'PART 2', sectionTitle: 'Birth weight and infant growth' });

const [cut1, cut2] = r.birth_cutoffs.map(x => x.toLocaleString('en-US'));
figure('Exploring growth by birth weight', 'birth_weight_tertiles',
  `Each dot is a visit. Curves smooth heights in the three birth-weight groups, with cutoffs at ${cut1} g and ${cut2} g.`,
  'At a given age, how does height differ across the birth-weight groups? How does the gap between the curves change as the boys grow?');

panels({
  title: 'Extending the growth model with birth weight',
  intro: 'Use each boy’s recorded birth weight. Let w be birth weight in kg minus 3.5, and t be age in weeks minus 26.',
  introHeight: .72,
  panels: [
    panel('POPULATION MEAN HEIGHT',
      metric('β₀ + β₁t + β₂t² + β₃w + β₄tw'),
      text('For a boy born at 3.5 kg, the mean trajectory is β₀ + β₁t + β₂t².', 20),
      text('Each boy still has a random intercept and a random linear age slope.', 20)),
    panel('MEAN HEIGHT GAP PER KG AT BIRTH',
      metric('β₃ + β₄t'),
      text('At 26 weeks, β₃ is the mean height difference associated with a 1 kg higher birth weight.', 20),
      text('Each additional week changes that difference by β₄ cm.', 20)),
  ],
});

addFigureSlide(pptx, {
  title: 'Defining the two model comparisons', assetPath: asset('birth_weight_models'),
  caption: 'The lower (dashed) and higher (solid) birth-weight curves coincide in the age-only model.',
  captionHeight: .62, captionFontSize: 17, captionMinFontSize: 17,
  takeaway: 'For overall association, compare the full model (right) with age only (left). For the interaction, compare the full model with the constant-gap model (middle).',
  takeawayHeight: 1.08,
});

function birthWeightTestSlide({ title, reduced, test, terms, nullHypothesis, decision, takeaway }) {
  const p = test.p < .001 ? 'p < .001' : 'p = ' + fmt(test.p, 3).replace(/^0/, '');
  panels({
    title,
    panels: [
      panel('MODELS FITTED BY MAXIMUM LIKELIHOOD',
        table([{ label: 'Model', weight: 1.7 }, col('Log likelihood', 1.18)], [
          [reduced, fmt(test.reduced_loglik)],
          ['Full interaction model', fmt(test.full_loglik)],
        ], 2.28),
        text(terms, 19)),
      panel('LIKELIHOOD-RATIO TEST',
        text(nullHypothesis, 20),
        metric(`χ²(${test.df}) = ${fmt(test.chisq)},  ${p}`),
        text(decision, 20)),
    ],
    takeaway, takeawayHeight: 1.05,
  });
}

birthWeightTestSlide({
  title: 'Testing the overall association with birth weight',
  reduced: 'Age only', test: r.overall_test,
  terms: 'Use ML because the fixed effects differ.\nχ² = 2 × gain in log likelihood.',
  nullHypothesis: 'Under the null, both birth-weight coefficients are zero. All birth weights share one mean trajectory.',
  decision: 'The data provide evidence that mean growth trajectories differ by birth weight.',
  takeaway: 'With evidence of an association, proceed to test whether the gap changes with age. Otherwise, the planned testing sequence ends here.',
});
birthWeightTestSlide({
  title: 'Testing whether the height gap changes with age',
  reduced: 'Age + birth weight', test: r.interaction_test,
  terms: 'Both models include birth weight. The full model adds the age-by-birth-weight interaction.',
  nullHypothesis: 'Under the null, the interaction coefficient is zero. A constant height gap is allowed.',
  decision: 'The data provide evidence that the mean height gap changes with age.',
  takeaway: 'Use the interaction model to estimate the direction and size of that change.',
});

const birthTerms = {
  '(Intercept)': ['Height at 26 weeks (cm)', 2],
  t: ['Rate at 26 weeks (cm/week)', 3],
  'I(t^2)': ['Squared age (cm/week²)', 4],
  w: ['Birth weight (cm/kg)', 2],
  't:w': ['Age × birth weight (cm/week/kg)', 3],
};
const weightEffect = r.birth_fixed.find(row => row.term === 'w').estimate;
const weightInteraction = r.birth_fixed.find(row => row.term === 't:w').estimate;
addTeachingTableSlide(pptx, {
  title: 'Interpreting the fixed effects',
  intro: 'REML estimates. The intercept and linear age coefficient describe a boy born at 3.5 kg.',
  columns: [{ label: 'Fixed effect', weight: 2.75 }, col('Estimate'), col('SE'), col('95% CI', 1.8), col('p-value')],
  rows: r.birth_fixed.map(row => {
    const [label, digits] = birthTerms[row.term];
    return [label, fmt(row.estimate, digits), fmt(row.se, digits),
      `${fmt(row.lower, digits)} to ${fmt(row.upper, digits)}`, row.p < .001 ? '< .001' : fmt(row.p, 3)];
  }),
  tableOptions: { firstColumnBold: false, headerFontSize: 18, cellPaddingY: .03 },
  fontSize: 18,
  takeaway: `At 26 weeks, a 1 kg higher birth weight corresponds to ${fmt(weightEffect)} cm greater mean height. This gap narrows by ${fmt(-weightInteraction, 3)} cm each week.`,
  takeawayHeight: 1.03,
});

const [lowWeight, highWeight] = r.birth_references.map(x => fmt(x));
figure('Estimating mean growth at two reference birth weights', 'birth_weight_emms',
  `The 1/6 and 5/6 quantiles (${lowWeight} and ${highWeight} kg) lie halfway through the lower and upper thirds in rank.`,
  'The colored dots mark estimated marginal means at 8, 26 and 48 weeks. How does the height gap between the two curves change with age?');

const [earlyGap, middleGap, lateGap] = r.birth_contrasts;
figure('Interpreting the height difference across age', 'birth_weight_contrast',
  `Birth weights of ${highWeight} kg versus ${lowWeight} kg (heavier minus lighter). Shading shows the pointwise 95% CI.`,
  `The fitted gap narrows from ${fmt(earlyGap.estimate)} cm at ${earlyGap.age} weeks to ${fmt(middleGap.estimate)} cm at ${middleGap.age} weeks and ${fmt(lateGap.estimate)} cm at ${lateGap.age} weeks.`);

addSectionSlide(pptx, { sectionLabel: 'PART 3', sectionTitle: 'Modeling time as a factor' });

addStudyOverviewSlide(pptx, {
  title: 'Alcohol use in adolescence',
  studyTitle: 'THREE ANNUAL VISITS',
  bullets: [
    `The same ${r.alcohol_n} adolescents took part at ages 14, 15, and 16.`,
    'At each visit, answers to several survey questions were combined into an alcohol-use score.',
    'Higher scores indicate greater reported alcohol use.',
    'These data accompany Singer and Willett (2003).',
  ],
  question: 'How much does the mean score change from age 14 to 15, and from age 15 to 16?',
  questionHeight: 1.7,
  assetPath: asset('adolescent_survey'),
});

panels({
  title: 'Specifying age as a number or a factor',
  intro: 'Both models include a random intercept for each adolescent. The fixed effects describe the population mean score.',
  introHeight: .72,
  panels: [
    panel('LINEAR AGE TREND',
      metric('Mean = β₀ + β₁t'),
      text('Here t is age in years minus 14. The intercept is the mean score at age 14.'),
      text('The slope β₁ is the increase or decrease in mean score for each one-year interval.')),
    panel('AGE AS A FACTOR',
      metric('Age 14 is the reference'),
      text('The three means are α₀ at 14, α₀ + α₁ at 15, and α₀ + α₂ at 16.'),
      text('The first annual change is α₁. The second is α₂ − α₁. The two changes can differ.')),
  ],
});

const av = r.alcohol_visits;
const annualChange = (k, term) => fmt(av[k][term] - av[k - 1][term]);
addTeachingTableSlide(pptx, {
  title: 'Estimated changes in mean alcohol-use score',
  columns: [{ label: 'Model for age', weight: 2.3 }, col('Age 14'), col('Age 15'), col('Age 16'),
    col('14 to 15', 1.15), col('15 to 16', 1.15)],
  rows: [['Linear trend', 'linear'], ['Age as a factor', 'factor_mean']].map(([label, term]) =>
    [label, ...av.map(visit => fmt(visit[term])), annualChange(1, term), annualChange(2, term)]),
  tableOptions: { firstColumnBold: false, headerFontSize: 19 },
  takeaway: `With age as a factor, the estimated mean score rises by ${annualChange(1, 'factor_mean')} points in the first year and ${annualChange(2, 'factor_mean')} in the second. The linear model estimates a common increase of ${annualChange(1, 'linear')} points per year.`,
  takeawayHeight: 1.30,
});

panels({
  title: 'A quadratic curve through three visit means',
  panels: [
    panel('AT THE SCHEDULED AGES', plot('alcohol_visit_means', 3.00),
      text('Dots show the fitted means from the factor model. The line shows the fitted quadratic mean.', 18)),
    panel('BETWEEN THE VISITS',
      text('A quadratic has three coefficients. At three distinct ages, it can pass through any three visit means.', 21),
      text('With the same random-effects structure, both models therefore fit the same means at ages 14, 15, and 16.', 21),
      text('At age 15½, the quadratic supplies an interpolated mean. The factor model specifies means only at its three levels.', 21)),
  ],
});

// Three closing slides connect assumptions, incomplete outcomes, and individual predictions.
panels({
  title: 'Residual variation across visits',
  intro: 'After accounting for each adolescent’s trajectory, scores can still vary from one visit to the next.',
  introHeight: .6,
  panels: [
    panel('UNEQUAL RESIDUAL VARIANCES', plot('visit_residual_spread', 2.05),
      text('In this illustration, the bars span one residual SD on either side of zero. The model can allow a different residual variance at each visit.', 19)),
    panel('RESIDUAL CORRELATION',
      text('An adolescent who scores above their own trajectory at one visit might also score above it at the next.', 21),
      text('A mixed model can allow residuals from nearby visits to be more strongly correlated than residuals farther apart.', 21)),
  ],
});

panels({
  title: 'Incomplete follow-up',
  intro: 'Suppose one adolescent misses the final visit. What can we learn from the two earlier scores?',
  introHeight: .65,
  panels: [
    panel('ONE HYPOTHETICAL VISIT RECORD',
      table([col('Age 14'), col('Age 15'), col('Age 16')], [['Observed', 'Observed', 'Missing']], 1.5),
      text('Both observed scores contribute to the likelihood. Other adolescents contribute information about mean alcohol use at 16.', 20)),
    panel('WHY WAS THE VISIT MISSED?',
      text('Under a missing-at-random assumption, attendance can depend on the scores already observed and on covariates in the model.', 20),
      text('If attendance also depends on the unobserved score at 16 after accounting for that information, the estimated mean may be biased.', 20)),
  ],
});

figure('Partial pooling of individual growth trajectories', 'growth_partial_pooling',
  'Zero is the age-specific population mean. Both fits share curvature. Dashed lines use each boy’s visits alone.',
  'The mixed model draws each boy’s estimated slope toward the population slope. With fewer visits or a narrower age range, the population pattern generally carries more weight.');

pptx._slides.forEach((slide, index) => {
  slide.addNotes('');
  // The branded cover intentionally layers its artwork over the colored background.
  if (index > 0) warnIfSlideHasOverlaps(slide, pptx, { ignoreLines: true });
  warnIfSlideElementsOutOfBounds(slide, pptx);
});
writePresentation(pptx, path.join(__dirname, 'Beyond_MLR_Week_4.pptx'))
  .then(() => console.log(`Built Week 4 with ${pptx._slides.length} slides.`))
  .catch(error => { console.error(error); process.exitCode = 1; });
