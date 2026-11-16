const path = require('path');
const fs = require('fs');
const { addBulletVisualSlide, addFigureSlide } = require('../../slides-common/js/pptx_layout_primitives');
const { addConceptPanelsSlide } = require('../../slides-common/js/concept_panels');
const { addDiagnosticsInterpretationSlide, addStructuredSummarySlide } = require('../../slides-common/js/course_builders');
const { equation } = require('./additive_example');

function addAssignmentRecap(pptx, drawEquation) {
  const assets = path.join(__dirname, 'assets/assignment1-recap');
  const results = JSON.parse(fs.readFileSync(path.join(assets, 'model_results.json'), 'utf8'));
  const f = n => Number(n).toFixed(2);
  const feedbackStyle = { takeawayTitle: 'FROM THE SUBMISSIONS', takeawayHeight: 1.35 };
  const setting = addBulletVisualSlide(pptx, {
    title: 'Assignment 1: comparing protein extraction protocols',
    bulletTitle: 'THE EXPERIMENT',
    bullets: [
      'Question: does a modified protocol increase protein yield?',
      'Tissue from each of 10 donors is divided into eight aliquots.',
      'Within each donor, four aliquots are randomly assigned to each protocol: Standard or Modified.',
      'Each aliquot is measured once, giving 80 observations.',
      'Outcome: protein yield in micrograms per milligram of tissue (µg/mg).'
    ],
    bulletFontSize: 20, bulletMinFontSize: 20,
    assetPath: path.join(__dirname, 'assets/assignment1-recap/protein-extraction.png'),
    fit: 'cover',
    note: 'Randomized block design\nBlock: donor; experimental unit: aliquot.',
    noteFontSize: 20, noteMinFontSize: 20, noteHeight: .88
  });
  setting.addNotes('');

  addFigureSlide(pptx, {
    title: 'Assignment 1: exploring protein yield',
    assetPath: path.join(assets, 'eda.png')
  }).addNotes('');

  addFigureSlide(pptx, {
    title: 'Assignment 1: the random-intercept model',
    assetPath: path.join(assets, 'random-intercept-means.png'),
    equation, drawEquation,
    caption: 'Tᵢⱼ = 0 for Standard and 1 for Modified.  i = aliquot; j = donor.',
    captionFontSize: 18, captionMinFontSize: 18, captionHeight: .38,
    ...feedbackStyle,
    takeaway: 'Most reports correctly explained that donors can differ in baseline protein yield, while the model assumes the same protocol difference across donors.'
  }).addNotes('');

  addConceptPanelsSlide(pptx, {
    title: 'Assignment 1: the estimated protocol effect',
    panels: [{ blocks: [
      { table: {
        columns: [
          { label: 'Term', weight: 3.0 },
          { label: 'Estimate\n(µg/mg)', weight: 1.8, align: 'center' },
          { label: 'Estimated\nSE (µg/mg)', weight: 1.9, align: 'center' },
          { label: '95% CI (µg/mg)', weight: 2.8, align: 'center' },
          { label: 'p-value', weight: 1.3, align: 'center' }
        ],
        rows: results.regression_coefficients.map(row => [row.term, f(row.estimate), f(row.standard_error),
          `${f(row.lower_95)} to ${f(row.upper_95)}`, row.p < .001 ? '< .001' : row.p.toFixed(3)]),
        fontSize: 22, headerFontSize: 19, headerHeight: .70, firstColumnBold: false
      }, h: 1.95, gap: .18 },
      { text: `The intercept (${f(results.intercept)} µg/mg) is the expected Standard yield for a donor whose random intercept is zero.`, fontSize: 22, h: .80, gap: .10 },
      { text: `After accounting for donor, Modified increases expected yield by ${f(results.estimate)} µg/mg. The 95% CI supports an increase of about ${f(results.lower_95)} to ${f(results.upper_95)} µg/mg.`, fontSize: 22, h: .95 }
    ] }],
    ...feedbackStyle,
    takeaway: 'Most reports correctly estimated the protocol difference and used the confidence interval to support the conclusion that the modified protocol increases protein yield.'
  }).addNotes('');

  addConceptPanelsSlide(pptx, {
    title: 'Assignment 1: variance components and ICC',
    intro: 'Variance components describe how the variation remaining after accounting for protocol is divided between donors and residual variation.',
    introHeight: .65,
    panels: [
      { blocks: [
        { table: {
          columns: [
            { label: 'Component', weight: 2.3 },
            { label: 'Variance', weight: 1.5, align: 'center' }
          ],
          rows: [['Donor', f(results.donor_variance)],
            ['Residual', f(results.residual_variance)]],
          fontSize: 22, headerFontSize: 18, headerHeight: .50, firstColumnBold: false
        }, h: 1.35, gap: .12 },
        { text: 'Variances: (µg/mg)².', fontSize: 18, h: .30, gap: .12 },
        { text: 'Donor: variation between donors.\nResidual: variation among aliquots from the same donor receiving the same protocol.', fontSize: 20, h: .95 }
      ] },
      { title: 'INTRACLASS CORRELATION', blocks: [
        { text: `${f(results.donor_variance)} / (${f(results.donor_variance)} + ${f(results.residual_variance)})\n= ${f(results.icc)}`, fontSize: 25, bold: true, h: .95, gap: .20 },
        { text: 'About 65% of the variance remaining after accounting for protocol lies between donors.', fontSize: 22, h: 1.05 }
      ] }
    ],
    ...feedbackStyle,
    takeawayHeight: 1.60,
    takeaway: `Some reports described the random donor intercept itself as a variance. Each donor has its own intercept shift. The value ${f(results.donor_variance)} in the Variance column describes how much these shifts vary between donors.`
  }).addNotes('');

  addDiagnosticsInterpretationSlide(pptx, {
    title: 'Assignment 1: checking the random-intercept model',
    plotHeight: 3.0,
    explanationTitle: 'WHAT THE PLOT SUGGESTS',
    diagnostics: [
      { assetPath: path.join(assets, 'residuals-vs-fitted.png'),
        purpose: 'No clear funnel or systematic trend in the conditional residuals.' },
      { assetPath: path.join(assets, 'normal-qq.png'),
        purpose: 'Approximate agreement in the centre, with departures in the lower tail.' }
    ],
    ...feedbackStyle,
    takeawayHeight: 1.10,
    takeaway: 'Some conclusions about model assumptions were stronger than the plots justified.'
  }).addNotes('');

  addStructuredSummarySlide(pptx, {
    title: 'Assignment 1: working with Quarto',
    sections: [
      { title: 'WHAT WENT WELL', paragraphs: [
        'Most reports brought together the analysis code, results, figures and written interpretations.'
      ] },
      { title: 'POINTS TO CONSIDER', paragraphs: [
        'Some reports still contained the template instructions or assignment prompts.',
        'The donor–protocol means were missing from one plot because of an error in their calculation. In another report, eval: false prevented plots and model output from appearing.',
        'Some labels and results used µg/ml or um/mg. The outcome is measured in µg/mg.'
      ] }
    ],
    takeaway: 'Check the rendered HTML: are all requested results and figures present, are warnings resolved, and are the labels and units correct?'
  }).addNotes('');
}

module.exports = { addAssignmentRecap };
