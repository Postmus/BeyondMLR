const path = require('node:path');
const { addStudyOverviewSlide } = require('../../slides-common/js/course_builders');
const { imageSizingContain } = require('../week1/pptxgenjs_helpers/image');
const { COLORS, FONT_FAMILIES, LAYOUT } = require('../../slides-common/js/blr_theme');
const { addBase, addSlideTitle, addBulletBox, addFooter, addFigureSlide } = require('../../slides-common/js/pptx_layout_primitives');
const { addConceptPanelsSlide } = require('../../slides-common/js/concept_panels');
const { addTeachingTableSlide } = require('../../slides-common/js/teaching_table');
const r = require('./assets/hba1c_results.json');

const fmt = (n, digits = 2) => n.toFixed(digits).replace('-', '−');
const p = value => value < .001 ? 'p < .001' : `p = ${value.toFixed(3)}`;
const column = label => ({ label, weight: 1, align: 'center' });

module.exports = function addHbA1cSlides(pptx) {
  const asset = name => path.join(__dirname, 'assets', name);
  const ci = row => `${fmt(row.lower)} to ${fmt(row.upper)}`;
  const termLabels = {
    '(Intercept)': 'Usual care at 8% (β₀)', baseline_c: 'Baseline HbA1c (β₁)',
    'programStructured follow-up': 'Structured follow-up (β₂)',
    'baseline_c:programStructured follow-up': 'Baseline HbA1c × program (β₃)',
  };
  const coefficientColumns = [{ label: 'Fixed effect', weight: 2.5 }, { ...column('Estimate'), weight: 1.25 },
    { ...column('SE'), weight: .75 }, { ...column('95% CI'), weight: 1.95 }, column('p-value')];
  const coefficientRows = (terms, labels = termLabels) => terms.map(term => [labels[term.term], fmt(term.estimate),
    fmt(term.se), ci(term), term.p < .001 ? '< .001' : fmt(term.p, 3)]);

  function drawEquation(slide, file, x, y, w, h) {
    slide.addImage({ path: file, ...imageSizingContain(file, x, y, w, h) });
  }

  function figure(title, name, caption, takeaway) {
    addFigureSlide(pptx, {
      title, assetPath: asset(`hba1c_${name}.png`),
      caption, captionFontSize: 17, captionMinFontSize: 17, captionHeight: .46,
      takeaway, takeawayHeight: .96,
    });
  }

  addStudyOverviewSlide(pptx, {
    title: 'Diabetes follow-up in primary care',
    studyTitle: 'CLINICAL CONTEXT AND STUDY DESIGN',
    bullets: [
      `The study followed ${r.patients.toLocaleString('en-US')} patients with newly diagnosed diabetes across ${r.practices} primary care practices.`,
      'Each practice provided either structured follow-up with a diabetes nurse or usual care.',
      'The outcome was the change in HbA1c from diagnosis to six months, measured in percentage points.',
    ],
    question: 'Do patients with newly diagnosed diabetes see a greater drop in HbA1c over six months with structured nurse follow-up than with usual care?',
    questionHeight: 1.42,
    assetPath: asset('hba1c_consultation.png'),
  });

  figure('Exploring six-month change in HbA1c', 'eda',
    'Each dot is one patient. Each line is fitted separately within one of the three largest practices per program.',
    'Looking at the slopes of the six fitted lines, would you include a random slope for baseline HbA1c across practices?');

  {
    const slide = pptx.addSlide();
    addBase(slide);
    addSlideTitle(slide, 'Random slope model');
    const { body, left, right } = LAYOUT;
    const textStyle = { fontFace: FONT_FAMILIES.sans, fontSize: 19, color: COLORS.text,
      margin: 0, align: 'center', valign: 'mid' };
    drawEquation(slide, asset('hba1c_eq_slope.png'), body.x, body.y, body.w, .56);
    slide.addText('Yᵢⱼ is six-month HbA1c change for patient i in practice j. Pⱼ = 0 (usual care) or 1 (structured follow-up).',
      { ...textStyle, x: body.x, y: 1.70, w: body.w, h: .46 });

    const headingStyle = { ...textStyle, bold: true, color: COLORS.primary, fontSize: 17, y: 2.43, h: .36 };
    slide.addText('Practice-level random effects', { ...headingStyle, x: left.x, w: left.w });
    slide.addText('Patient-level errors', { ...headingStyle, x: right.x, w: right.w });
    drawEquation(slide, asset('hba1c_eq_practice.png'), left.x + .12, 2.99, left.w - .24, 1.18);
    drawEquation(slide, asset('hba1c_eq_errors.png'), right.x, 3.01, right.w, .52);
    slide.addText('Independent of the practice random effects.',
      { ...textStyle, x: right.x, y: 3.70, w: right.w, h: .42 });

    addBulletBox(slide, {
      box: { x: body.x, y: 4.50, w: body.w, h: 2.41 }, fontSize: 20, minFontSize: 20, paraSpaceAfter: 8,
      bullets: [
        'We center baseline HbA1c at 8%, so Bᵢⱼ = 0 for a patient who starts at 8%.',
        'β₁ is the average slope across practices. It gives the difference in expected six-month change for a one-percentage-point higher starting HbA1c.',
        'dⱼ is how far practice j’s slope lies above or below the average, so its slope is β₁ + dⱼ.',
      ],
    });
    addFooter(slide);
  }

  function variancePanel(interceptSD, slopeSD, residualSD, correlation, fontSize = 17) {
    return { title: 'VARIANCE COMPONENTS', blocks: [
      { h: 2.4, table: { columns: [{ label: 'Source', weight: 1.8 }, { ...column('Variance'), weight: 1.24 }, { ...column('SD'), weight: .76 }],
        rows: [['Practice intercept', interceptSD], ['Practice slope', slopeSD], ['Patient residual', residualSD]]
          .map(([label, sd]) => [label, fmt(sd * sd, 3), fmt(sd)]),
        fontSize, headerFontSize: fontSize - 2, cellPaddingX: .06, firstColumnBold: false } },
      { text: `Intercept–slope correlation: ${fmt(correlation)}.`, fontSize },
    ] };
  }

  function resultsSlide(title, fixed, interceptSD, slopeSD, residualSD, correlation, takeaway) {
    addConceptPanelsSlide(pptx, {
      title, panels: [
        { title: 'REGRESSION COEFFICIENTS', weight: 1.82, blocks: [
          { h: 3.40, table: { columns: coefficientColumns, rows: coefficientRows(fixed),
            fontSize: 17, headerFontSize: 15, cellPaddingX: .06, firstColumnBold: false } },
        ] },
        variancePanel(interceptSD, slopeSD, residualSD, correlation),
      ],
      takeaway, takeawayHeight: 1.13,
    });
  }

  const program = r.additive_fixed.find(term => term.term === 'programStructured follow-up');
  addConceptPanelsSlide(pptx, {
    title: 'How much does the baseline HbA1c slope vary across practices?',
    panels: [
      variancePanel(r.intercept_sd, r.slope_sd, r.residual_sd, r.slope_correlation, 21),
      { title: 'FITTED NORMAL DISTRIBUTION', blocks: [
        { text: `Mean slope ${fmt(r.mean_slope)}   ·   Slope SD ${fmt(r.slope_sd)}`, fontSize: 21, bold: true, h: .4 },
        { image: asset('hba1c_practice_slopes.png'), h: 2.85 },
      ] },
    ],
    takeaway: `In about 68% of practices, the model predicts a six-month HbA1c drop ${fmt(-r.mean_slope - r.slope_sd)} to ${fmt(-r.mean_slope + r.slope_sd)} percentage points greater for a patient starting at 9% than for one starting at 8% in the same practice.`,
    takeawayHeight: 1.25,
  });

  addTeachingTableSlide(pptx, {
    title: 'How does six-month HbA1c change differ between follow-up programs?',
    columns: [{ label: 'Fixed effect', weight: 3.6 }, ...coefficientColumns.slice(1)],
    rows: coefficientRows(r.additive_fixed, {
      '(Intercept)': 'Usual care at 8% baseline HbA1c (β₀)',
      baseline_c: 'Baseline HbA1c, per percentage point (β₁)',
      'programStructured follow-up': 'Structured follow-up versus usual care (β₂)',
    }),
    fontSize: 21,
    tableOptions: { headerFontSize: 19, headerHeight: .65, firstColumnBold: false,
      rowFills: r.additive_fixed.map(term => term === program ? COLORS.surfaceMuted : COLORS.card) },
    takeaway: `At the same starting HbA1c, the estimated six-month drop was ${fmt(-program.estimate)} percentage points greater with structured follow-up than with usual care. The program difference was not statistically significant (${p(program.p)}).`,
    takeawayHeight: 1.25,
  });

  addConceptPanelsSlide(pptx, {
    title: 'Does the program difference depend on HbA1c at diagnosis?',
    lead: { equation: asset('hba1c_eq_interaction.png'),
      text: 'Bᵢⱼ is baseline HbA1c minus 8. Pⱼ is 0 for usual care and 1 for structured follow-up.' },
    drawEquation,
    panels: [
      { title: 'A CROSS-LEVEL INTERACTION', blocks: [
        { text: 'Baseline HbA1c varies between patients. The follow-up program varies between practices.', fontSize: 20 },
        { text: 'Their product, Bᵢⱼ × Pⱼ, allows the average slope for baseline HbA1c to differ between programs.', fontSize: 20 },
      ] },
      { title: 'THE PROGRAM COMPARISON', blocks: [
        { text: 'At 8%, Bᵢⱼ = 0. The expected difference in change between structured follow-up and usual care is β₂.', fontSize: 20 },
        { text: 'At a baseline HbA1c of 9%, Bᵢⱼ = 1 and the expected program difference is β₂ + β₃.', fontSize: 20 },
      ] },
    ],
    takeaway: 'If β₃ = 0, the average program difference is the same at every starting HbA1c.',
  });

  resultsSlide('Interaction between baseline HbA1c and program', r.interaction_fixed,
    r.full_intercept_sd, r.full_slope_sd, r.full_residual_sd, r.full_correlation,
    `At a starting HbA1c of 8%, the fitted drop is ${fmt(-r.means[0].difference)} points greater with structured follow-up than with usual care. At 9%, that gap widens by ${fmt(-r.interaction)} percentage points (interaction ${p(r.interaction_p)}).`);

  addConceptPanelsSlide(pptx, {
    title: 'Estimated marginal means of six-month HbA1c change',
    panels: [
      { title: 'FITTED PROGRAM AVERAGES', blocks: [
        { image: asset('hba1c_interaction.png'), h: 4.05 },
        { text: 'Dots mark the estimated marginal means at baseline HbA1c values of 8% and 9%.', fontSize: 17 },
      ] },
      { title: 'ESTIMATED MARGINAL MEANS', weight: 1.22, blocks: [
        { h: 1.68, gap: .18, table: {
          columns: [column('Baseline HbA1c'), column('Usual care'), { ...column('Structured follow-up'), weight: 1.5 }],
          rows: r.means.map(m => [`${m.baseline}%`, fmt(m.usual), fmt(m.structured)]),
          fontSize: 18, headerFontSize: 16, cellPaddingX: .06,
        } },
        { text: 'STRUCTURED FOLLOW-UP VERSUS USUAL CARE', bold: true, fontSize: 17, gap: .12 },
        { h: 1.73, gap: .13, table: {
          columns: [column('Baseline HbA1c'), column('Difference'), { ...column('95% CI'), weight: 1.6 }, column('p-value')],
          rows: r.means.map(m => [`${m.baseline}%`, fmt(m.difference), ci(m), m.p < .001 ? '< .001' : fmt(m.p, 3)]),
          fontSize: 17, headerFontSize: 15, cellPaddingX: .05,
        } },
        { text: 'Differences are structured minus usual care, in HbA1c percentage points.', fontSize: 17 },
      ] },
    ],
  });

  addFigureSlide(pptx, {
    title: 'Time for Lab 6',
    assetPath: asset('lab6-chicago-break.png'),
  });
};
