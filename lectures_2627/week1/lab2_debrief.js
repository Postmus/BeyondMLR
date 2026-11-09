const path = require('path');
const { addFigureSlide } = require('../../slides-common/js/pptx_layout_primitives');
const { addConceptPanelsSlide } = require('../../slides-common/js/concept_panels');

function addLab2Debrief(pptx, drawEquation, results) {
  const f = (value, digits = 1) => Number(value).toFixed(digits);
  addFigureSlide(pptx, {
    title: 'Time for lab 2',
    assetPath: path.join('assets', 'lab2-break.png')
  });
  addConceptPanelsSlide(pptx, {
    title: 'Comparing the models',
    panels: [
      { blocks: [
        { table: {
          columns: [
            { label: 'Model', weight: 3.3 },
            { label: 'Estimate (m)', weight: 1.45, align: 'center' },
            { label: 'Standard error (m)', weight: 1.45, align: 'center' }
          ],
          rows: [
            ['Treatment only', f(results.pooled.estimate), f(results.pooled.se, 2)],
            ['Fixed center effects', f(results.fixed.estimate), f(results.fixed.se, 2)],
            ['Random center effects', f(results.random.estimate), f(results.random.se, 2)]
          ],
          fontSize: 22, headerFontSize: 18, headerHeight: .64, firstColumnBold: false
        }, h: 2.50, gap: .25 },
        { text: 'Balanced treatment allocation within centers makes the treatment estimates identical.', h: .70, gap: .16, fontSize: 21 },
        { text: 'Both center models separate center variation from individual variation, giving better-calibrated standard errors in this example.', h: .90, fontSize: 21 }
      ] }
    ],
    takeaway: 'The treatment results agree here, but the models describe center variation differently: separate parameters for these six centers versus a distribution for a population of centers.',
    drawEquation
  });
}

module.exports = { addLab2Debrief, equations: [] };
