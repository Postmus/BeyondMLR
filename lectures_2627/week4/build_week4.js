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
pptx.subject = 'Longitudinal models for infant growth';
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
  mainTitle: 'Modeling change over time', subtitle: 'Longitudinal models for infant growth',
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
    'They also recorded whether each boy was exclusively breastfed at four weeks.',
  ],
  question: [
    'How does height change with age, and how much do growth trajectories vary between boys?',
    'How do mean growth trajectories differ by breastfeeding status at four weeks?',
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

addSectionSlide(pptx, { sectionLabel: 'PART 2', sectionTitle: 'Comparing growth trajectories by breastfeeding status' });

figure('Exploring growth trajectories by breastfeeding status', 'feeding_observed',
  'Each point is a mean height at one week of age in one feeding group. Lines smooth the weekly means.',
  'Feeding status was recorded at four weeks. How far apart are the groups at 26 weeks? Does their height gap change with age?');

panels({
  title: 'Breastfeeding status and the age slope',
  intro: 'Add feeding group and its interaction with centered age to the quadratic model. Both groups share the quadratic term.',
  introHeight: .70,
  panels: [
    panel('FITTED GROUP MEANS', plot('feeding_fitted', 3.30)),
    panel('GROUP DIFFERENCE IN MEAN HEIGHT',
      table([col('Age'), col('Difference'), col('95% CI', 1.50)],
        r.contrasts.map((v, i) => [i === 0 ? '26 weeks' : '40 weeks', fmt(v.estimate) + ' cm',
          fmt(v['asymp.LCL']) + ' to ' + fmt(v['asymp.UCL'])]), 1.78),
      text('Differences compare exclusive breastfeeding minus other feeding. At 26 weeks this is the group coefficient.', 19),
      text(`The age-by-group coefficient is ${fmt(r.interaction, 3)} cm/week. The estimated gap widens by ${fmt(-14 * r.interaction)} cm over 14 weeks.`, 19)),
  ],
});

panels({
  title: 'Interpreting the growth model',
  panels: [
    panel('OBSERVED GROWTH', plot('observed_panel', 3.55),
      text('Return to the observed trajectories. What feature of growth does each model term describe?', 19)),
    panel('INTERPRET THE MODEL',
      text('Random intercepts describe differences in height at 26 weeks.', 20),
      text('The slope SD describes variation in growth rates. The correlation relates growth rate to height at 26 weeks.', 20),
      text('The quadratic age term describes slowing growth.', 20),
      text('The feeding-group term gives the gap at 26 weeks. Its interaction with age gives the weekly change in that gap.', 20)),
  ],
});

pptx._slides.forEach((slide, index) => {
  slide.addNotes('');
  // The branded cover intentionally layers its artwork over the colored background.
  if (index > 0) warnIfSlideHasOverlaps(slide, pptx, { ignoreLines: true });
  warnIfSlideElementsOutOfBounds(slide, pptx);
});
writePresentation(pptx, path.join(__dirname, 'Beyond_MLR_Week_4.pptx'))
  .then(() => console.log(`Built Week 4 with ${pptx._slides.length} slides.`))
  .catch(error => { console.error(error); process.exitCode = 1; });
