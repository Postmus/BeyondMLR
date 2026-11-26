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
  intro: 'For boy i at visit j, tᵢⱼ is age in weeks minus 26. At 26 weeks, tᵢⱼ = 0.',
  panels: [
    panel('HEIGHT AT ONE VISIT',
      plot('linear_growth_model', 1.34),
      text('β₀ is the mean height at 26 weeks (cm). β₁ is the weekly height gain (cm/week), shared by all boys.'),
      text('The random intercept b₀ᵢ shifts boy i’s line up or down by the same amount at every visit.'),
      text('The visit error εᵢⱼ is the difference between the measured height and the boy’s line. The visit errors are independent of the random intercepts.')),
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

panels({
  title: 'Fitting a random linear age slope for each boy',
  panels: [
    { title: 'FIXED EFFECTS FOR THE POPULATION MEAN', weight: 1.5,
      blocks: [quadraticFixedTable(r.selected_fixed),
        text('At 26 weeks, boy i’s growth rate is β₁ + b₁ᵢ. The shared quadratic coefficient means every boy’s rate slows by the same amount each week.', 19)] },
    panel('TWO BOYS’ FITTED TRAJECTORIES', plot('selected_trajectories', 3.1),
      text('Dots mark observed heights. Each solid curve is one boy’s fitted trajectory. The dashed curve is the population mean.', 18)),
  ],
});

const interceptSlopeCorrelation = (r.correlation > 0 ? '+' : '') + fmt(r.correlation);
panels({
  title: 'Interpreting the random linear age slope',
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
      text(`At 26 weeks, the between-child SD in growth rate is ${fmt(r.slope_sd, 3)} cm/week. A boy one SD above the mean rate gains ${fmt(14 * r.slope_sd)} cm more than average over 14 weeks.`, 20),
      { text: 'How is height at 26 weeks related to growth rate?', fontSize: 22, bold: true },
      text(`The intercept–slope correlation is ${interceptSlopeCorrelation}. In this model, boys who are taller at 26 weeks tend to grow slightly faster.`, 20)),
  ],
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

function compareGrowthModels(title, simpler, extended, takeaway) {
  const labels = {
    intercept: 'Intercept only',
    slope: 'Intercept + linear age',
    full: 'Intercept + linear age + quadratic age',
  };
  addTeachingTableSlide(pptx, {
    title,
    intro: 'Both models use REML, the same visits and the same quadratic population mean.',
    columns: [{ label: 'Random effects for each child', weight: 3.15 }, col('Parameters', 1.10), col('AIC')],
    rows: [simpler, extended].map(model => {
      const row = r.comparison.find(candidate => candidate.model === model);
      return [labels[model], String(row.df), fmt(row.AIC)];
    }),
    tableOptions: { firstColumnBold: false, headerFontSize: 19 },
    takeaway, takeawayHeight: 1.10,
  });
}

compareGrowthModels('Does AIC favor a child-specific growth rate?', 'intercept', 'slope',
  'AIC falls when the linear age slope varies between boys. The gain in fit outweighs the penalty for estimating the slope variance and its correlation with height at 26 weeks.');

panels({
  title: 'Adding a random quadratic age slope',
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
      text('Two boys could grow at the same rate at 26 weeks, yet one boy’s growth could slow more sharply over the next month.'),
      text('Give each boy a quadratic coefficient β₂ + b₂ᵢ. This adds its variance and its correlations with the intercept and linear slope.', 20)),
  ],
  takeaway: 'Does allowing curvature to vary between boys improve the fit enough to justify those three parameters?',
});

compareGrowthModels('Does AIC favor child-specific curvature?', 'slope', 'full',
  'The AIC values are almost identical. We retain the simpler model in which boys have their own growth rates at 26 weeks and share the same curvature.');

panels({
  title: 'Start with a sufficiently rich fixed-effects structure',
  intro: 'The fixed effects should capture the main pattern in average growth. Here, the bend in the residuals led us to include both age and age squared.',
  introHeight: .83,
  panels: [
    panel('STEP-UP APPROACH',
      text('Start with a random intercept so boys can differ in height at 26 weeks.'),
      text('Add a random linear slope so boys can differ in growth rate. Then add a random quadratic slope so they can differ in how quickly growth slows.')),
    panel('TOP-DOWN APPROACH',
      text('Start with a model in which height, growth rate and curvature can all vary between boys.'),
      text('First consider a shared curvature. Then consider whether boys can also share the same growth rate at 26 weeks.')),
  ],
  takeaway: 'Keep the fixed effects the same when comparing random effects. Fitting all three models to the same visits gives the same AIC ranking in either order.',
  takeawayHeight: 1.02,
});

addFigureSlide(pptx, { title: 'Time for Lab 7', assetPath: asset('lab7_chick_growth') });

addSectionSlide(pptx, { sectionLabel: 'PART 2', sectionTitle: 'Birth weight and infant growth' });

const [cut1, cut2] = r.birth_cutoffs.map(x => x.toLocaleString('en-US'));
figure('Exploring growth by birth weight', 'birth_weight_tertiles',
  `Each dot is a visit. Curves smooth heights in the three birth-weight groups, with cutoffs at ${cut1} g and ${cut2} g.`,
  'At a given age, how does height differ across the birth-weight groups? How does the gap between the curves change as the boys grow?');

panels({
  title: 'Birth weight can modify growth rate and curvature',
  intro: 'For boy i at visit j, tᵢⱼ is age in weeks minus 26, and wᵢ is birth weight in kg minus 3.5.',
  introHeight: .54,
  panels: [
    panel('HEIGHT AT ONE VISIT',
      plot('birth_weight_growth_model', 1.35),
      text('For boys born at 3.5 kg, the mean height is β₀ + β₁t + β₂t².', 21),
      text('For each additional kg at birth, the mean height gap is β₃ at 26 weeks. The two interactions allow this gap to change with age as β₃ + β₄t + β₅t².', 21),
      text('Each boy has a random intercept and slope. D contains their variances and covariance. Visit errors are independent of these effects.', 20)),
  ],
});

addFigureSlide(pptx, {
  title: 'Defining the two model comparisons', assetPath: asset('birth_weight_models'),
  caption: 'Illustrative mean curves for a lower (dashed) and higher (solid) birth weight. In the age-only model they coincide.',
  captionHeight: .62, captionFontSize: 17, captionMinFontSize: 17,
  takeaway: 'For overall association, compare the full model (right) with age only (left). To test both interactions together, compare the full model with the constant-gap model (middle).',
  takeawayHeight: 1.08,
});

function birthWeightTestSlide({ title, reduced, test, terms, nullEquation, nullHypothesis, decision, takeaway }) {
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
        metric(nullEquation),
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
  nullEquation: 'H₀: β₃ = β₄ = β₅ = 0',
  nullHypothesis: 'All three restrictions hold together. Boys of every birth weight share the same mean height trajectory.',
  decision: 'The data provide evidence that mean growth trajectories differ by birth weight.',
  takeaway: 'With evidence of an association, proceed to test whether the gap changes with age. Otherwise, the planned testing sequence ends here.',
});
birthWeightTestSlide({
  title: 'Testing whether the height gap changes with age',
  reduced: 'Age + birth weight', test: r.interaction_test,
  terms: 'Both models include birth weight. The full model adds its interactions with age and age squared.',
  nullEquation: 'H₀: β₄ = β₅ = 0',
  nullHypothesis: 'With both interactions zero, β₃ gives the same mean height gap per kg at every age.',
  decision: 'The data provide evidence that the mean height gap changes with age.',
  takeaway: 'Use the interaction model to estimate the direction and size of that change.',
});

const birthTerms = {
  '(Intercept)': ['Height at 26 weeks (cm)', 2],
  t: ['Rate at 26 weeks (cm/week)', 3],
  'I(t^2)': ['Squared age (cm/week²)', 4],
  w: ['Birth weight (cm/kg)', 2],
  't:w': ['Age × birth weight (cm/week/kg)', 3],
  'I(t^2):w': ['Age² × birth weight (cm/week²/kg)', 5],
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
  takeaway: `At 26 weeks, a 1 kg higher birth weight is associated with ${fmt(weightEffect)} cm greater mean height and a growth rate ${fmt(-weightInteraction, 3)} cm/week slower.`,
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

addFigureSlide(pptx, { title: 'Time for Lab 8', assetPath: asset('lab8_chick_diet') });

addSectionSlide(pptx, { sectionLabel: 'PART 3', sectionTitle: 'Modeling change across scheduled visits' });

addStudyOverviewSlide(pptx, {
  title: 'Alcohol use at three annual visits',
  studyTitle: 'THREE ANNUAL VISITS',
  bullets: [
    `The same ${r.alcohol_n} adolescents took part at ages 14, 15, and 16.`,
    'At each visit, answers to several survey questions were combined into an alcohol-use score.',
    'Higher scores indicate greater reported alcohol use.',
    'These data accompany Singer and Willett (2003).',
  ],
  question: 'How much did the mean alcohol-use score change in each of the two years?',
  questionHeight: 1.7,
  assetPath: asset('adolescent_survey'),
});

addFigureSlide(pptx, {
  title: 'Individual alcohol-use trajectories and visit means',
  assetPath: asset('alcohol_observed_trajectories'),
});

const av = r.alcohol_visits;
const annualChange = (k, term) => fmt(av[k][term] - av[k - 1][term]);

panels({
  title: 'Treating age as a factor gives a mean at each visit',
  panels: [
    panel('ALCOHOL-USE SCORE FOR ADOLESCENT i AT VISIT j',
      metric('Scoreᵢⱼ = α₀ + α₁ I(age = 15) + α₂ I(age = 16) + b₀ᵢ + εᵢⱼ'),
      text('Each indicator equals 1 at the named age and 0 otherwise. At age 14 both are 0.'),
      text('The random intercept b₀ᵢ shifts all three means by the same amount for adolescent i.'),
      table([{ label: 'Population mean', weight: 1.45 }, ...av.map(v => col('Age ' + v.age))], [
        ['Model', 'α₀', 'α₀ + α₁', 'α₀ + α₂'],
        ['Estimated score', ...av.map(v => fmt(v.factor_mean))],
      ], 1.72)),
  ],
  takeaway: `From 14 to 15, the mean rises by α₁ = ${annualChange(1, 'factor_mean')} points. From 15 to 16, it rises by α₂ − α₁ = ${annualChange(2, 'factor_mean')} points. Both coefficients use age 14 as the reference.`,
  takeawayHeight: 1.10,
});

panels({
  title: 'Random visit effects allow changes to differ between adolescents',
  panels: [
    panel('A RANDOM INTERCEPT',
      text('Each adolescent has their own starting level at age 14.'),
      text('The model gives everyone the same expected change in the first year and the same expected change in the second.')),
    panel('ADDING TWO RANDOM VISIT EFFECTS',
      text('Each adolescent now has their own expected change from age 14 to 15 and their own expected change from age 14 to 16.'),
      text('Both changes are measured from the starting level at age 14.')),
  ],
  takeaway: 'With their own starting score and two changes, each adolescent’s fitted scores can match all three observations exactly. We then cannot estimate how much scores fluctuate around that individual pattern.',
  takeawayHeight: 1.34,
});

addTeachingTableSlide(pptx, {
  title: 'A continuous age trend links the changes across visits',
  intro: 'All three models use a random intercept. For continuous age, t is years since age 14.',
  columns: [{ label: 'Model for age', weight: 1.15 }, { label: 'Population mean', weight: 1.95 },
    col('Change 14–15', 1.18), col('Change 15–16', 1.18)],
  rows: [
    ['Factor', 'One mean per visit', 'factor_mean'],
    ['Linear', 'β₀ + β₁t', 'linear'],
    ['Quadratic', 'β₀ + β₁t + β₂t²', 'quadratic'],
  ].map(([label, mean, term]) => [label, mean, annualChange(1, term), annualChange(2, term)]),
  tableOptions: { firstColumnBold: false, headerFontSize: 19 },
  takeaway: 'A straight line gives the same increase each year. With three scheduled ages, a quadratic curve can pass through all three visit means and gives the same two changes as the factor model.',
  takeawayHeight: 1.19,
});

addTeachingTableSlide(pptx, {
  title: 'The study question and visit schedule guide how we model time',
  columns: [{ label: '', weight: .91 }, { label: 'Time as a factor', weight: 2.15 },
    { label: 'Continuous time', weight: 2.15 }],
  rows: [
    ['Question', 'How much does the mean change from one visit to the next?',
      'How does the mean change with age or elapsed time?'],
    ['Number of times', 'With a few visits, one mean per visit is easy to interpret.',
      'A line or curve can summarize many distinct measurement times with a few coefficients.'],
    ['Visit timing', 'A shared schedule gives each visit the same meaning across participants.',
      'Use each participant’s actual age or elapsed time when visits occur at different times.'],
    ['Lecture example', 'Alcohol use at ages 14, 15, and 16. Each annual change is of interest.',
      'Infant height from 6 to 52 weeks. Boys were measured at different ages across the first year.'],
  ],
  fontSize: 20,
  tableOptions: { headerFontSize: 20 },
});

addSectionSlide(pptx, { sectionLabel: 'The End', sectionTitle: 'Questions' });

pptx._slides.forEach((slide, index) => {
  slide.addNotes('');
  // The branded cover intentionally layers its artwork over the colored background.
  if (index > 0) warnIfSlideHasOverlaps(slide, pptx, { ignoreLines: true });
  warnIfSlideElementsOutOfBounds(slide, pptx);
});
writePresentation(pptx, path.join(__dirname, 'Beyond_MLR_Week_4.pptx'))
  .then(() => console.log(`Built Week 4 with ${pptx._slides.length} slides.`))
  .catch(error => { console.error(error); process.exitCode = 1; });
