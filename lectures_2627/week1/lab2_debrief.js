const path = require('path');
const { addFigureSlide } = require('../../slides-common/js/pptx_layout_primitives');
const { addConceptPanelsSlide } = require('../../slides-common/js/concept_panels');

function iccEquation(results) {
  const f = (value, digits = 0) => Number(value).toFixed(digits);
  return String.raw`\widehat{\mathrm{ICC}}=\frac{${f(results.random.center_var)}}{${f(results.random.center_var)}+${f(results.random.patient_var)}}\approx ${f(results.random.icc, 2)}`;
}

function addLab2Debrief(pptx, drawEquation, results) {
  const f = (value, digits = 1) => Number(value).toFixed(digits);
  addFigureSlide(pptx, {
    title: 'Time for lab 2',
    assetPath: path.join('assets', 'lab2-break.png')
  });
  addConceptPanelsSlide(pptx, {
    title: 'Lab 2 debrief: center variation and treatment uncertainty',
    panels: [
      { title: 'INTERPRETING THE ICC', weight: 1, blocks: [
        { equation: iccEquation(results), h: .85, gap: .2 },
        { text: `About ${f(100 * results.random.icc, 0)}% of the outcome variance remaining after accounting for treatment is attributable to differences between centers.`, h: 1.35, gap: .22 },
        { text: `The estimated correlation between outcomes of two different patients in the same center is ${f(results.random.icc, 2)}, after accounting for treatment.`, h: 1.35 }
      ] },
      { title: 'COMPARING THE MODELS', weight: 1.3, blocks: [
        { table: {
          columns: [
            { label: 'Model', weight: 3.3 },
            { label: 'Estimate\n(m)', weight: 1.45, align: 'center' },
            { label: 'Standard\nerror (m)', weight: 1.45, align: 'center' }
          ],
          rows: [
            ['Treatment only', f(results.pooled.estimate), f(results.pooled.se, 2)],
            ['Fixed center effects', f(results.fixed.estimate), f(results.fixed.se, 2)],
            ['Random center effects', f(results.random.estimate), f(results.random.se, 2)]
          ],
          fontSize: 20, headerFontSize: 16, headerHeight: .64, firstColumnBold: false
        }, h: 2.14 },
        { text: 'Balanced treatment allocation within centers makes the treatment estimates identical.', h: .7, fontSize: 20 },
        { text: 'Both center models separate center variation from individual variation, giving better-calibrated standard errors in this example.', h: .9, fontSize: 20 }
      ] }
    ],
    takeaway: 'The treatment results agree here, but the models describe center variation differently: separate parameters for these six centers versus a distribution for a population of centers.',
    drawEquation
  });
}

module.exports = { addLab2Debrief, equations: [iccEquation(require('./assets/model_results.json'))] };
