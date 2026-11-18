const pptxgen = require('pptxgenjs');
const path = require('node:path');
const { COLORS, FONT_FAMILIES, LAYOUT, SPACING } = require('../../slides-common/js/blr_theme');
const {
  defineTemplateMasters, addBrandedTitleSlide, addSectionSlide,
  addBase, addSlideTitle, addTeachingCards, addNoteBox, addAssetFrame, addFooter,
} = require('../../slides-common/js/pptx_layout_primitives');
const { addConceptPanelsSlide } = require('../../slides-common/js/concept_panels');
const { addTeachingTable, addTeachingTableSlide } = require('../../slides-common/js/teaching_table');
const { writePresentation } = require('../../slides-common/js/write_presentation');
const {
  warnIfSlideHasOverlaps, warnIfSlideElementsOutOfBounds,
} = require('../week1/pptxgenjs_helpers/layout');
const results = require('./assets/model_results.json');

const pptx = new pptxgen();
pptx.layout = 'LAYOUT_WIDE';
pptx.author = 'Dr Douwe Postmus';
pptx.subject = 'Week 3: cluster randomization and cross-level interactions';
pptx.title = 'Beyond MLR | Week 3';
pptx.lang = 'en-GB';
pptx.theme = { headFontFace: FONT_FAMILIES.sans, bodyFontFace: FONT_FAMILIES.sans, lang: 'en-GB' };
defineTemplateMasters(pptx);

const asset = name => path.join(__dirname, 'assets', name);
const fmt = (value, digits = 2) => value.toFixed(digits).replace('-', '−');
const ci = (row, digits = 2) => `${fmt(row.lower, digits)} to ${fmt(row.upper, digits)}`;
const pValue = value => value < .001 ? '< .001' : value.toFixed(3).replace(/^0/, '');
const coefficientRow = (term, row) => [term, fmt(row.estimate), fmt(row.se), ci(row), pValue(row.p_value)];
const equation = text => ({ text, fontSize: 25, h: .53, gap: .25 });
const fixedPart = 'Yᵢⱼ = β₀ + β₁Tⱼ + β₂Xᵢⱼ';
// Reuse the Normal distribution equation from the Week 1 random-intercept slide.
const randomInterceptDistributions = path.join(__dirname, '../week1/assets/equations/eq_fbb126480bb9.png');

addBrandedTitleSlide(pptx, {
  courseLabel: 'BEYOND MULTIPLE LINEAR REGRESSION · WEEK 3',
  mainTitle: 'Variables at different levels',
  subtitle: 'Cluster randomization and cross-level interactions',
  author: 'Dr Douwe Postmus',
  affiliation: 'UMCG · Department of Epidemiology',
  badge: 'BLR',
});

addSectionSlide(pptx, { sectionLabel: 'PART 1', sectionTitle: 'Cluster randomization' });

const [minCci, maxCci] = results.cci_range;
addConceptPanelsSlide(pptx, {
  title: 'A cluster-randomized rehabilitation trial',
  intro: 'Does enhanced care improve walking distance at 12 weeks after knee replacement, and does its effect vary with baseline comorbidity?',
  introHeight: .75,
  panels: [
    {
      weight: 1.04,
      blocks: [
        {
          text: `Design: cluster-randomized trial in which ${results.centers} centers are randomized equally to two treatments. All ${results.patients_per_center} patients in each center receive its assigned treatment.`,
          bullet: true, fontSize: 19, gap: .04,
        },
        { text: 'Context-level variable: Treatment', bullet: true, fontSize: 19, gap: .04 },
        { text: 'Individual-level variable: Baseline CCI', bullet: true, fontSize: 19, gap: .04 },
        { text: 'Outcome: Distance on the six-minute walk test at 12 weeks', bullet: true, fontSize: 19 },
      ],
      callout: {
        text: `The Charlson Comorbidity Index (CCI) sums weights for a patient’s pre-existing conditions. We use the score without age points. Scores in this dataset range from ${minCci} to ${maxCci}.`,
        height: 1.48, fontSize: 18,
      },
    },
    { blocks: [{ image: asset('center_allocation.png'), h: 4.55 }] },
  ],
});

addConceptPanelsSlide(pptx, {
  title: 'Exploratory data analysis',
  panels: [
    { title: 'WALKING DISTANCE BY CENTER', blocks: [
      { image: asset('walking_distance_by_center.png'), h: 3.72 },
      { text: 'Boxes show the middle 50% of distances.\nThe line inside each box marks the median.', fontSize: 19 },
    ] },
    { title: 'WALKING DISTANCE BY BASELINE CCI', blocks: [
      { image: asset('walking_distance_by_cci.png'), h: 3.72 },
      { text: 'Each dot shows one patient.\nThe line shows the overall linear trend.', fontSize: 19 },
    ] },
  ],
});

addConceptPanelsSlide(pptx, {
  title: 'Additive random-intercept model',
  intro: 'The model estimates a common treatment effect across CCI scores and a common CCI slope across treatments.',
  introHeight: .75,
  panels: [
    { title: 'FIXED EFFECTS', blocks: [
      { text: 'β₀ is the expected walking distance under standard care at CCI 0.', fontSize: 20, gap: .15 },
      { text: 'β₁ is the expected difference between enhanced and standard care at a given CCI score.', fontSize: 20, gap: .15 },
      { text: 'β₂ is the change in expected walking distance per additional CCI point.', fontSize: 20 },
    ] },
    { title: 'MODEL AND ASSUMPTIONS', blocks: [
      equation(`${fixedPart} + cⱼ + εᵢⱼ`),
      { image: randomInterceptDistributions, h: 1.18, gap: .12 },
      { text: 'Patients in a center share cⱼ. Patient errors are independent of center effects.', fontSize: 20 },
    ] },
  ],
  takeaway: 'Tⱼ is 0 for standard care and 1 for enhanced care. Xᵢⱼ is the patient’s baseline CCI score.',
  takeawayHeight: .95,
});

const fixedEffectTerms = ['Intercept', 'Treatment', 'Baseline CCI'];
const additive = results.random_intercept;
addConceptPanelsSlide(pptx, {
  title: 'Results from the additive model',
  panels: [
    { title: 'FIXED EFFECTS', weight: 1.60, blocks: [
      { h: 2.95, table: {
        columns: [
          { label: 'Term', weight: 1.61 },
          { label: 'Estimate', weight: .94, align: 'center' },
          { label: 'SE', weight: .85, align: 'center' },
          { label: '95% CI', weight: 1.94, align: 'center' },
          { label: 'p-value', weight: .91, align: 'center' },
        ],
        rows: additive.terms.map((row, i) => coefficientRow(fixedEffectTerms[i], row)),
        fontSize: 18, headerFontSize: 15, cellPaddingX: .075,
      } },
    ] },
    { title: 'VARIANCE COMPONENTS AND ICC', blocks: [
      { h: 2.00, gap: .25, table: {
        columns: [
          { label: 'Source', weight: 1.55 },
          { label: 'Variance in m²', weight: 1.2, align: 'center' },
        ],
        rows: [
          ['Center intercept', fmt(additive.center_variance, 1)],
          ['Patient residual', fmt(additive.residual_variance, 1)],
        ],
        fontSize: 18, headerFontSize: 15, cellPaddingX: .075,
      } },
      { text: `Estimated ICC = ${fmt(additive.icc)}`, fontSize: 21, bold: true },
      { text: 'This is the estimated correlation between walking distances for two patients in the same center after accounting for treatment and baseline CCI.', fontSize: 18 },
    ] },
  ],
});

const variables = fixedEffectTerms.slice(1);
const [minCenterCci, maxCenterCci] = results.center_mean_cci_range;
addTeachingTableSlide(pptx, {
  title: 'What happens to the estimates and SEs when we omit center?',
  intro: 'Fit ordinary regression with the same treatment and baseline CCI terms.',
  takeaway: 'Accounting for center increases the treatment SE and decreases the CCI SE in this example.',
  takeawayPosition: 'top',
  takeawayHeight: .82,
  columns: [
    { label: 'Variable', weight: 2.5 },
    { label: 'Estimate', weight: 1.25, align: 'center' },
    { label: 'SE', weight: 1, align: 'center', highlight: true },
    { label: 'p-value', weight: 1.05, align: 'center' },
    { label: 'Estimate', weight: 1.25, align: 'center' },
    { label: 'SE', weight: 1, align: 'center', highlight: true },
    { label: 'p-value', weight: 1.05, align: 'center' },
  ],
  rows: variables.map((label, i) => {
    const ordinary = results.ordinary[i];
    const mixed = additive.terms[i + 1];
    return [label, fmt(ordinary.estimate), fmt(ordinary.se), pValue(ordinary.p_value),
      fmt(mixed.estimate), fmt(mixed.se), pValue(mixed.p_value)];
  }),
  tableOptions: {
    columnGroups: [
      { label: '', count: 1 },
      { label: 'ORDINARY REGRESSION', count: 3 },
      { label: 'RANDOM-INTERCEPT MODEL', count: 3 },
    ],
    headerHeight: .66, groupHeaderHeight: .55,
  },
  footnote: `Average baseline CCI ranged from ${fmt(minCenterCci)} to ${fmt(maxCenterCci)} points across centers. Accounting for the shared center effect gives comparisons between center averages less influence on the fitted CCI slope, so the estimate can shift slightly.`,
  footnoteHeight: 1.14,
  footnoteFontSize: 20,
});

addConceptPanelsSlide(pptx, {
  title: 'Why do the SEs move in opposite directions?',
  panels: [
    { title: 'TREATMENT · BETWEEN CENTERS', blocks: [
      { text: 'Enhanced centers − standard centers', fontSize: 21, bold: true },
      equation('c̄ (enhanced) − c̄ (standard)'),
      { text: 'Chance differences between those sets of centers contribute to uncertainty about the treatment effect.' },
      { text: 'Ordinary regression treats patients as independent and gives too small an SE.', bold: true, fontSize: 20 },
    ] },
    { title: 'CCI · MOSTLY WITHIN CENTERS', blocks: [
      { text: 'Two patients in the same center', fontSize: 21, bold: true },
      equation('cⱼ − cⱼ = 0'),
      { text: 'Most CCI variation is within centers. The shared center effect cancels in those comparisons.' },
      { text: 'Ordinary regression includes center differences in the residual spread. Here it gives a larger CCI SE.', bold: true, fontSize: 20 },
    ] },
  ],
});

addConceptPanelsSlide(pptx, {
  title: 'Cross-level interaction',
  intro: 'A cross-level interaction occurs when the relationship between an individual-level variable and the outcome depends on a context-level variable.',
  introHeight: .78,
  panels: [
    { title: 'REHABILITATION EXAMPLE', blocks: [
      { text: 'Does the relationship between baseline CCI and walking distance differ between standard and enhanced care?', fontSize: 20, gap: .25 },
      { text: 'HOW WE TEST IT', fontSize: 17, bold: true, h: .32, gap: .10 },
      { text: 'Add the interaction term TⱼXᵢⱼ to the model. Its coefficient, β₃, estimates the difference between the two CCI slopes. We test whether that coefficient is zero.', fontSize: 20 },
    ] },
    { weight: 1.18, blocks: [
      { image: asset('program_comorbidity_interaction.png'), h: 4.50 },
    ] },
  ],
});

const interaction = results.interaction;
const interactionTerms = [...fixedEffectTerms, 'Treatment × Baseline CCI'];
addTeachingTableSlide(pptx, {
  title: 'Fitted interaction model',
  columns: [
    { label: 'Term', weight: 3.2 },
    { label: 'Estimate', weight: 1.15, align: 'center' },
    { label: 'SE', weight: .85, align: 'center' },
    { label: '95% CI', weight: 2.25, align: 'center' },
    { label: 'p-value', weight: 1.0, align: 'center', highlight: true },
  ],
  rows: interaction.terms.map((row, i) => coefficientRow(interactionTerms[i], row)),
  footnote: `The treatment coefficient compares enhanced with standard care at CCI 0.\nEstimated SDs are ${fmt(interaction.center_sd, 1)} m for the center intercept and ${fmt(interaction.residual_sd, 1)} m for the patient residual.`,
  footnoteHeight: .72,
  footnoteFontSize: 17,
  takeaway: `The interaction test provides evidence that the treatment effect increases with CCI. The estimated increase is ${fmt(interaction.terms.at(-1).estimate, 1)} m per CCI point.`,
  takeawayHeight: 1.02,
});

addTeachingTableSlide(pptx, {
  title: 'Treatment effects at selected CCI scores',
  columns: [
    { label: 'Baseline CCI', weight: 1.45, align: 'center' },
    { label: 'Standard care', weight: 1.55, align: 'center' },
    { label: 'Enhanced care', weight: 1.55, align: 'center' },
    { label: 'Difference', weight: 1.45, align: 'center', highlight: true },
    { label: 'Pointwise 95% CI', weight: 2.1, align: 'center' },
  ],
  rows: interaction.treatment_effects.map(row => [
    row.cci, fmt(row.standard, 1), fmt(row.enhanced, 1), fmt(row.estimate, 1), ci(row, 1),
  ]),
  tableOptions: {
    columnGroups: [
      { label: '', count: 1 },
      { label: 'ESTIMATED MARGINAL MEANS', count: 2 },
      { label: 'SIMPLE TREATMENT EFFECT', count: 2 },
    ],
    headerHeight: .62, groupHeaderHeight: .55,
  },
  footnote: 'Means, differences and confidence intervals are in meters.',
  footnoteFontSize: 17,
  takeaway: 'The interaction provides evidence that the advantage of enhanced care grows\nwith baseline CCI. At CCI 1 and 4, both confidence intervals include zero, so\nwhether enhanced care improves walking distance at either score remains uncertain.',
  takeawayHeight: 1.30,
});

addSectionSlide(pptx, { sectionLabel: 'PART 2', sectionTitle: 'Grouped observational data' });

const variationModels = [
  { within: 70, between: 30, fixed: 0 },
  { within: 50, between: 25, fixed: 20 },
  { within: 50, between: 15, fixed: 30 },
];
const remainingVariance = model => model.within + model.between;
const groupShare = model => model.between / remainingVariance(model);
const percent = share => `${Math.round(100 * share)}%`;
const [nullModel] = variationModels;
const nullIcc = groupShare(nullModel);

{
  const slide = pptx.addSlide();
  addBase(slide);
  addSlideTitle(slide, 'From randomized trials to grouped observational data');

  const { x, y, w, h } = LAYOUT.left;
  const trialCardHeight = 2.81;
  addTeachingCards(slide, {
    x, y, w, h: trialCardHeight,
    cards: [{ title: 'Randomized trials so far', body: [
      { text: 'How does treatment affect the outcome?', options: { bold: true, breakLine: true } },
      { text: 'We accounted for grouping when quantifying uncertainty in the treatment effect.' },
    ] }],
  });
  addTeachingCards(slide, {
    x, y: y + trialCardHeight + SPACING.gutter, w, h: h - trialCardHeight - SPACING.gutter,
    cards: [{ title: 'Grouped observational data', body: [
      { text: 'How much does the outcome vary within and between groups?', options: { bold: true, breakLine: true } },
      { text: 'Which individual-level and context-level variables help explain that variation?', options: { bold: true } },
    ] }],
  });
  addAssetFrame(slide, {
    box: LAYOUT.right, assetPath: asset('grouped-observational-data.png'), framed: false,
  });
  addFooter(slide);
}

{
  const slide = pptx.addSlide();
  addBase(slide);
  addSlideTitle(slide, 'The null model and ICC');

  const { x, y, w, h } = LAYOUT.body;
  const bodyText = { fontFace: FONT_FAMILIES.sans, fontSize: 20, color: COLORS.text, margin: 0 };
  slide.addText('The null model is a random-intercept model whose only fixed effect is the overall intercept.', {
    ...bodyText, x, y: y + .06, w, h: .43,
  });
  slide.addText('Yᵢⱼ = β₀ + bⱼ + εᵢⱼ', {
    ...bodyText, x, y: y + .67, w, h: .60, fontSize: 30, align: 'center',
  });
  addTeachingTable(slide, {
    box: { x, y: y + 1.48, w, h: 2.76 },
    columns: [{ label: 'Term', weight: 1 }, { label: 'Meaning', weight: 8 }],
    rows: [
      ['β₀', 'Overall mean'],
      ['bⱼ', 'Group j’s deviation from the overall mean. Its variance is the between-group variance.'],
      ['εᵢⱼ', 'Observation i’s deviation from its group mean. Its variance is the within-group variance.'],
    ],
    fontSize: 19, headerFontSize: 18,
  });

  const noteHeight = 1.48;
  const noteY = y + h - noteHeight;
  const noteWidth = 6.10;
  addNoteBox(slide, {
    box: { x, y: noteY, w: noteWidth, h: noteHeight },
    title: 'Intraclass correlation coefficient (ICC)', titleFontSize: 17,
    text: 'In the null model, the ICC is the proportion of total outcome variance that lies between groups.',
    fontSize: 20, minFontSize: 20,
  });
  const formulaX = x + noteWidth + SPACING.gutter;
  const fractionX = formulaX + .90;
  const fractionW = x + w - fractionX;
  const ruleY = noteY + noteHeight / 2;
  const fractionText = { ...bodyText, x: fractionX, w: fractionW, h: .33,
    fontSize: 16, align: 'center', color: COLORS.primary };
  slide.addText('ICC =', {
    ...bodyText, x: formulaX, y: ruleY - .20, w: .90, h: .40, color: COLORS.primary,
  });
  slide.addText('between-group variance', { ...fractionText, y: ruleY - .39 });
  slide.addShape('line', { x: fractionX, y: ruleY, w: fractionW, h: 0,
    line: { color: COLORS.primary, width: 1.2 } });
  slide.addText('between-group variance + within-group variance', { ...fractionText, y: ruleY + .06 });
  addFooter(slide);
}

addConceptPanelsSlide(pptx, {
  title: 'How much do math scores vary within and between schools?',
  panels: [
    { title: 'STUDENTS GROUPED BY SCHOOL', blocks: [
      { text: 'Outcome', fontSize: 18, bold: true, h: .34, gap: .04 },
      { text: 'End-of-year math score', fontSize: 21, gap: .24 },
      { text: 'Individual-level variable', fontSize: 18, bold: true, h: .34, gap: .04 },
      { text: 'Prior-year math score', fontSize: 21, gap: .24 },
      { text: 'Context-level variable', fontSize: 18, bold: true, h: .34, gap: .04 },
      { text: 'Student–teacher ratio at each school', fontSize: 21 },
    ] },
    { title: 'ILLUSTRATIVE NULL-MODEL RESULTS', blocks: [
      { h: 1.94, gap: .22, table: {
        columns: [
          { label: 'Component', weight: 1.70 },
          { label: 'Variance', weight: 1, align: 'center' },
        ],
        rows: [['Within schools', nullModel.within], ['Between schools', nullModel.between]],
        fontSize: 20, headerFontSize: 17,
      } },
      { text: `ICC = ${fmt(nullIcc)}`, fontSize: 22, bold: true },
      { text: `${percent(nullIcc)} of the total variation in scores lies between schools.`, fontSize: 20 },
    ] },
  ],
  takeaway: 'What might account for differences in scores within and between schools?',
});

addTeachingTableSlide(pptx, {
  title: 'Which variation can each variable help explain?',
  intro: 'Marginal R² is the share of total outcome variation explained by the fixed effects.',
  columns: [
    { label: 'Variable', weight: 1.84 },
    { label: 'Within groups', weight: 3.00 },
    { label: 'Between groups', weight: 3.31 },
  ],
  rows: [
    ['Individual-level', 'Can explain differences among individuals in the same group.',
      'Can also explain differences between group means when groups differ in their mix of individuals.'],
    ['Context-level', 'Everyone in the group shares the same value.',
      'Can explain differences between group means by shifting the fitted mean for the whole group.'],
  ],
  fontSize: 21,
  tableOptions: { headerHeight: .62, headerFontSize: 18, cellPaddingX: .25 },
});

addTeachingTableSlide(pptx, {
  title: 'How much variation in math scores remains?',
  intro: 'Illustrative results for three models of end-of-year math scores in the same schools.',
  columns: [
    { label: 'Measure', weight: 1.53 },
    { label: 'Null model', weight: .66, align: 'center' },
    { label: 'Add prior-year\nmath score', weight: 1.21, align: 'center' },
    { label: 'Then add\nstudent–teacher ratio', weight: 1.27, align: 'center' },
  ],
  rows: [
    ['Within-school variance', ...variationModels.map(model => model.within)],
    ['Between-school variance', ...variationModels.map(model => model.between)],
    ['ICC', ...variationModels.map(model => percent(groupShare(model)))],
    ['Marginal R²', ...variationModels.map(model => percent(model.fixed / (model.fixed + remainingVariance(model))))],
  ],
  tableOptions: { headerHeight: .85, headerFontSize: 18 },
  takeaway: 'Where does the remaining variation decrease at each step?\nWhy does the ICC rise when we add prior-year math score?',
  takeawayHeight: 1.05,
});

addConceptPanelsSlide(pptx, {
  title: 'Lab 5 · Chicago Airbnb listings',
  panels: [
    { weight: 1.16, blocks: [
      { text: 'How much do log nightly prices vary within and between neighborhoods?', fontSize: 25, bold: true, h: 1.22, gap: .28 },
      { text: 'Fit the null model, then see how the two variance components change as you add listing-level and neighborhood-level variables.', fontSize: 21 },
    ] },
    { blocks: [{ image: path.join(__dirname, '../../lectures_2526/R-lab.png'), h: 4.90 }] },
  ],
});

pptx._slides.forEach((slide, index) => {
  slide.addNotes('');
  // The shared title layout uses intentional decorative overlaps.
  if (index > 0) warnIfSlideHasOverlaps(slide, pptx, { ignoreLines: true });
  warnIfSlideElementsOutOfBounds(slide, pptx);
});
writePresentation(pptx, path.join(__dirname, 'Beyond_MLR_Week_3.pptx'))
  .then(() => console.log(`Built Beyond_MLR_Week_3: ${pptx._slides.length} slides.`))
  .catch(error => { console.error(error); process.exitCode = 1; });
