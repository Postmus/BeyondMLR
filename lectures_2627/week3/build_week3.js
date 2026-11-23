const pptxgen = require('pptxgenjs');
const path = require('node:path');
const { COLORS, FONT_FAMILIES, LAYOUT, SPACING } = require('../../slides-common/js/blr_theme');
const {
  defineTemplateMasters, addBrandedTitleSlide, addSectionSlide, addFigureSlide,
  addBase, addSlideTitle, addTeachingCards, addNoteBox, addAssetFrame, addFooter,
} = require('../../slides-common/js/pptx_layout_primitives');
const { addConceptPanelsSlide } = require('../../slides-common/js/concept_panels');
const { addTeachingTable, addTeachingTableSlide } = require('../../slides-common/js/teaching_table');
const { writePresentation } = require('../../slides-common/js/write_presentation');
const {
  warnIfSlideHasOverlaps, warnIfSlideElementsOutOfBounds,
} = require('../week1/pptxgenjs_helpers/layout');
const results = require('./assets/model_results.json');

const lectureTitle = 'Analyzing multilevel observational data';
const lectureSubtitle = 'Explaining variation within and between groups';
const pptx = new pptxgen();
pptx.layout = 'LAYOUT_WIDE';
pptx.author = 'Dr Douwe Postmus';
pptx.subject = lectureSubtitle;
pptx.title = `Beyond MLR | Week 3 | ${lectureTitle}`;
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
  mainTitle: lectureTitle,
  subtitle: lectureSubtitle,
  author: 'Dr Douwe Postmus',
  affiliation: 'UMCG · Department of Epidemiology',
  badge: 'BLR',
});

const recap = require('./assets/assignment2_results.json');
addFigureSlide(pptx, {
  title: 'Assignment 2: postoperative pain across 12 hospitals',
  assetPath: asset('assignment2_interaction.png'),
  takeawayTitle: 'FROM THE SUBMISSIONS',
  takeaway: 'Many reports picked out both patterns. Mean pain was lower with enhanced care in every hospital, and the size of the reduction varied across hospitals.',
  takeawayHeight: 1.25,
});

const meanReduction = Number(fmt(-recap.varying.estimate));
const slopeSD = Number(fmt(recap.slope_sd));
const lowerReduction = fmt(meanReduction - slopeSD);
const upperReduction = fmt(meanReduction + slopeSD);
addConceptPanelsSlide(pptx, {
  title: 'How much does the pain reduction vary across hospitals?',
  panels: [
    { title: 'RANDOM-SLOPE MODEL', blocks: [
      { text: 'Average pain reduction', fontSize: 20, gap: .06 },
      { text: `${fmt(meanReduction)} points`, fontSize: 32, bold: true, gap: .35 },
      { text: 'Between-hospital SD of the protocol effect', fontSize: 20, gap: .06 },
      { text: `${fmt(slopeSD)} points`, fontSize: 32, bold: true },
    ] },
    { title: 'MODELLED DISTRIBUTION ACROSS HOSPITALS', blocks: [
      { image: asset('assignment2_hospital_effects.png'), h: 3.3 },
    ] },
  ],
  takeaway: `Under the fitted Normal model, about 68% of comparable hospitals would have a mean pain reduction between ${lowerReduction} and ${upperReduction} points (mean ± 1 SD).`,
  takeawayHeight: 1.15,
});

addTeachingTableSlide(pptx, {
  title: 'How does a random slope affect the estimate and its SE?',
  columns: [
    { label: 'Hospital random effects', weight: 3.5 },
    { label: 'Mean pain difference (points)\nEnhanced − Standard', weight: 3.5, align: 'center' },
    { label: 'SE (points)', weight: 1.6, align: 'center', highlight: true },
  ],
  rows: [
    ['Random intercept and protocol slope', fmt(recap.varying.estimate), fmt(recap.varying.se, 3)],
    ['Random intercept only', fmt(recap.common.estimate), fmt(recap.common.se, 3)],
  ],
  fontSize: 25, tableOptions: { headerHeight: 1.1, headerFontSize: 21 },
  takeaway: 'In Assignment 2, patients were randomized within each hospital.\nWhat changes when the whole hospital is randomized?',
  takeawayHeight: 1.05,
});

addSectionSlide(pptx, { sectionLabel: 'PART 1', sectionTitle: 'Cluster randomization' });

addConceptPanelsSlide(pptx, {
  title: 'A cluster-randomized rehabilitation trial',
  intro: 'Accounting for baseline comorbidity, does enhanced care improve walking distance 12 weeks after knee replacement?',
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
        text: 'The Charlson Comorbidity Index (CCI) measures a patient’s burden of illness before surgery, with higher scores indicating more or more serious pre-existing conditions.',
        height: 1.48, fontSize: 18,
      },
    },
    { title: 'PATIENTS WITHIN TWO EXAMPLE CENTERS', blocks: [
      { h: 3.68, table: {
        columns: [
          { label: 'Center', weight: .78, align: 'center' },
          { label: 'Assigned\ntreatment', weight: 1.38 },
          { label: 'Patient', weight: .83, align: 'center' },
          { label: 'Baseline\nCCI', weight: .99, align: 'center' },
          { label: '12-week\nwalk (m)', weight: 1.02, align: 'center' },
        ],
        rows: [
          [1, 'Standard care', 1, 3, 325],
          [1, 'Standard care', 2, 1, 388],
          [3, 'Enhanced care', 41, 1, 370],
          [3, 'Enhanced care', 42, 1, 409],
        ],
        rowFills: [COLORS.card, COLORS.card, COLORS.surfaceMuted, COLORS.surfaceMuted],
        fontSize: 17, headerFontSize: 15, headerHeight: .72, cellPaddingX: .045,
      } },
    ] },
  ],
});

addConceptPanelsSlide(pptx, {
  title: 'Exploratory data analysis',
  panels: [
    { title: 'WALKING DISTANCE BY CENTER', blocks: [
      { image: asset('walking_distance_by_center.png'), h: 4.95 },
    ] },
    { title: 'WALKING DISTANCE BY BASELINE CCI', blocks: [
      { image: asset('walking_distance_by_cci.png'), h: 4.95 },
    ] },
  ],
});

addConceptPanelsSlide(pptx, {
  title: 'Random-intercept model',
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
  title: 'Results from the random-intercept model',
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
addTeachingTableSlide(pptx, {
  title: 'What happens to the estimates and SEs when we omit center?',
  intro: 'Fit ordinary regression with the same treatment and baseline CCI terms.',
  takeaway: 'Accounting for center increases the treatment SE and decreases the CCI SE in this example.',
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
});

addConceptPanelsSlide(pptx, {
  title: 'Why do the standard errors move in opposite directions?',
  panels: [
    { title: 'TREATMENT', blocks: [
      'Treatment varies between centers, with three centers assigned to each arm.',
      'Chance differences between those centers contribute to uncertainty about the treatment effect.',
      'Ordinary regression understates that uncertainty by treating all 120 patients as independent.',
      `Accounting for center raises the treatment SE from ${fmt(results.ordinary[0].se)} to ${fmt(additive.terms[1].se)} metres.`,
    ].map(text => ({ text, bullet: true, fontSize: 19, h: 1.08, gap: .16 })) },
    { title: 'BASELINE CCI', blocks: [
      'Most of the variation in CCI is among patients within centers.',
      'A center-wide shift in walking distance leaves the CCI pattern within that center intact.',
      'Ordinary regression includes center differences in its residual variation.',
      `Separating out the center variation lowers the CCI slope’s SE here from ${fmt(results.ordinary[1].se)} to ${fmt(additive.terms[2].se)} metres per CCI point.`,
    ].map(text => ({ text, bullet: true, fontSize: 19, h: 1.08, gap: .16 })) },
  ],
});

addConceptPanelsSlide(pptx, {
  title: 'Should we add a random treatment slope for center?',
  panels: [
    { title: 'ASSIGNMENT 2', blocks: [
      { text: 'Each hospital delivered both protocols.', fontSize: 22, gap: .24 },
      { text: 'We could compare mean pain scores within each hospital and estimate how much the protocol effect varied across hospitals.', fontSize: 22 },
    ] },
    { title: 'CLUSTER-RANDOMIZED TRIAL', blocks: [
      { text: 'Each center delivered one treatment. Suppose patients at an enhanced-care center walk 10 m farther on average than expected.', fontSize: 21, gap: .24 },
      { text: 'Would they also have walked farther under standard care, or did they benefit more from enhanced care?', fontSize: 23, bold: true },
    ] },
  ],
  takeaway: 'Comparing trial arms estimates the average treatment effect. The random intercept captures each center’s overall deviation under its assigned treatment. Usual walking distance and treatment benefit can both contribute to that deviation.',
  takeawayHeight: 1.48,
});

addSectionSlide(pptx, { sectionLabel: 'PART 2', sectionTitle: 'Grouped observational data' });

const variationModels = [
  { within: 70, between: 30 },
  { within: 50, between: 25 },
  { within: 50, between: 15 },
];
const remainingVariance = model => model.within + model.between;
const groupShare = model => model.between / remainingVariance(model);
const percent = share => `${Math.round(100 * share)}%`;
const [nullModel] = variationModels;
const nullIcc = groupShare(nullModel);

{
  const slide = pptx.addSlide();
  addBase(slide);
  addSlideTitle(slide, 'From treatment effects to variation within and between groups');

  const { x, y, h } = LAYOUT.body;
  const w = 7.15;
  const trialCardHeight = 2.64;
  addTeachingCards(slide, {
    x, y, w, h: trialCardHeight,
    cards: [{ title: 'Randomized trials', fontSize: 19, body: [
      { text: 'What is the treatment effect?', options: { bold: true, breakLine: true } },
      { text: 'We estimate the treatment effect and use a model that reflects the trial design so that we get the standard error right.' },
    ] }],
  });
  addTeachingCards(slide, {
    x, y: y + trialCardHeight + SPACING.gutter, w, h: h - trialCardHeight - SPACING.gutter,
    cards: [{ title: 'Descriptive and exploratory analysis of grouped observational data', fontSize: 19, body: [
      { text: 'How much do outcomes vary within and between groups?', options: { bold: true, breakLine: true } },
      { text: 'We describe how much variation lies at each level, then explore which individual and group characteristics help account for it.' },
    ] }],
  });
  addAssetFrame(slide, {
    box: { x: x + w + SPACING.gutter, y, w: LAYOUT.body.w - w - SPACING.gutter, h },
    assetPath: asset('grouped-observational-data.png'), framed: false,
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
  title: 'Which differences in math scores can each predictor explain?',
  columns: [
    { label: 'Variable', weight: 1.84 },
    { label: 'Within a school', weight: 3.00 },
    { label: 'Between schools', weight: 3.31 },
  ],
  rows: [
    ['Prior-year score (individual level)', 'Differences in students’ prior scores can help explain why their end-of-year scores differ within the same school.',
      'If students at one school started the year with higher scores on average, that may help explain a higher end-of-year mean.'],
    ['Student–teacher ratio (context level)', 'All students at a school share its ratio, so it explains none of the differences among classmates in this model.',
      'Schools have different ratios, which may help explain differences in their mean end-of-year scores.'],
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
  ],
  tableOptions: { headerHeight: .85, headerFontSize: 18 },
  takeaway: 'Both variances fall when we add prior-year score. Why does the ICC rise?\nWhich variance decreases when we then add the student–teacher ratio?',
  takeawayHeight: 1.05,
});

addFigureSlide(pptx, {
  title: 'Time for Lab 5',
  assetPath: asset('lab5-chicago-break.png'),
});

addSectionSlide(pptx, { sectionLabel: 'PART 3', sectionTitle: 'Random slopes and cross-level interactions' });
require('./hba1c_slides')(pptx);
require('./nested_airbnb_slides')(pptx);
addSectionSlide(pptx, { sectionLabel: 'The End', sectionTitle: 'Questions' });

pptx._slides.forEach((slide, index) => {
  slide.addNotes('');
  // The shared title layout uses intentional decorative overlaps.
  if (index > 0) warnIfSlideHasOverlaps(slide, pptx, { ignoreLines: true });
  warnIfSlideElementsOutOfBounds(slide, pptx);
});
writePresentation(pptx, path.join(__dirname, 'Beyond_MLR_Week_3.pptx'))
  .then(() => console.log(`Built Beyond_MLR_Week_3: ${pptx._slides.length} slides.`))
  .catch(error => { console.error(error); process.exitCode = 1; });
