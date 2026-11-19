const path = require('node:path');
const { addStudyOverviewSlide } = require('../../slides-common/js/course_builders');
const { imageSizingContain } = require('../week1/pptxgenjs_helpers/image');
const { addFigureSlide } = require('../../slides-common/js/pptx_layout_primitives');
const { addConceptPanelsSlide } = require('../../slides-common/js/concept_panels');
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
  const coefficientRows = terms => terms.map(term => [termLabels[term.term], fmt(term.estimate),
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
      `This simulated study follows ${r.patients.toLocaleString('en-US')} patients newly diagnosed with diabetes across ${r.practices} primary care practices.`,
      'Each practice provides either structured follow-up with a diabetes nurse or usual care.',
      'The outcome is six-month HbA1c minus HbA1c at diagnosis, measured in percentage points.',
      'A negative change indicates that HbA1c fell over the six months.',
    ],
    question: 'For patients with the same HbA1c at diagnosis, how does the average six-month change in HbA1c differ between structured follow-up and usual care?',
    questionHeight: 1.42,
    assetPath: asset('hba1c_consultation.png'),
  });

  figure('Exploring six-month change in HbA1c', 'eda',
    'Each dot is one patient. Each line is fitted separately within one of the three largest practices per program.',
    'How does six-month change relate to HbA1c at diagnosis within practices? Do the six lines suggest a common slope?');

  addConceptPanelsSlide(pptx, {
    title: 'Random slope model',
    lead: { equation: asset('hba1c_eq_slope.png'),
      text: 'Yᵢⱼ is six-month change for patient i in practice j. Pⱼ is 0 for usual care and 1 for structured follow-up.' },
    drawEquation,
    panels: [
      { title: 'AT A BASELINE HbA1c OF 8%', blocks: [
        { text: 'Bᵢⱼ = baseline HbA1c − 8', bold: true, fontSize: 22 },
        { text: 'At 8%, Bᵢⱼ = 0. The intercept β₀ is the expected change under usual care for a practice with average random effects.', fontSize: 19 },
        { text: 'cⱼ is the practice’s deviation from its program mean at 8%.', fontSize: 19 },
      ] },
      { title: 'BASELINE HbA1c OF 9% VERSUS 8%', blocks: [
        { text: 'Two patients in one practice', bold: true, fontSize: 22 },
        { text: 'The slope for baseline HbA1c is the expected six-month change at 9% minus the expected change at 8%.', fontSize: 19 },
        { text: 'β₁ is the average slope and β₁ + dⱼ is this practice’s slope.', fontSize: 19 },
      ] },
    ],
    takeaway: 'β₂ compares structured follow-up with usual care at the same starting HbA1c.',
  });

  function resultsSlide(title, fixed, interceptSD, slopeSD, residualSD, correlation, takeaway) {
    addConceptPanelsSlide(pptx, {
      title, panels: [
        { title: 'REGRESSION COEFFICIENTS', weight: 1.82, blocks: [
          { h: 3.40, table: { columns: coefficientColumns, rows: coefficientRows(fixed),
            fontSize: 17, headerFontSize: 15, cellPaddingX: .06, firstColumnBold: false } },
        ] },
        { title: 'VARIANCE COMPONENTS', blocks: [
          { h: 2.4, table: { columns: [{ label: 'Source', weight: 1.8 }, { ...column('Variance'), weight: 1.24 }, { ...column('SD'), weight: .76 }],
            rows: [['Practice intercept', interceptSD], ['Practice slope', slopeSD], ['Patient residual', residualSD]]
              .map(([label, sd]) => [label, fmt(sd * sd, 3), fmt(sd)]),
            fontSize: 17, headerFontSize: 15, cellPaddingX: .06, firstColumnBold: false } },
          { text: `Intercept–slope correlation: ${fmt(correlation)}.`, fontSize: 17 },
        ] },
      ],
      takeaway, takeawayHeight: 1.13,
    });
  }

  const program = r.additive_fixed.find(term => term.term === 'programStructured follow-up');
  resultsSlide('HbA1c change · Results from the random slope model', r.additive_fixed,
    r.intercept_sd, r.slope_sd, r.residual_sd, r.slope_correlation,
    `Practice slopes vary (SD ${fmt(r.slope_sd)}). At every starting HbA1c, the model estimates the same program difference: ${fmt(program.estimate)} percentage points (95% CI ${ci(program)}).`);

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
    `For each percentage point higher in baseline HbA1c, the additional fall with structured follow-up is ${fmt(-r.interaction)} points larger (${p(r.interaction_p)}). Within programs, practice slopes have an SD of ${fmt(r.full_slope_sd)}.`);

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
