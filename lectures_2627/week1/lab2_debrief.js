const path = require('path');
const { addFigureSlide } = require('../../slides-common/js/pptx_layout_primitives');
const { addTeachingTableSlide } = require('../../slides-common/js/teaching_table');
const { addConceptPanelsSlide } = require('../../slides-common/js/concept_panels');

const iccEquation = String.raw`\mathrm{ICC}=\frac{\sigma_c^2}{\sigma_c^2+\sigma^2}`;

function addLab2Debrief(pptx, drawEquation, results) {
  const f = (value, digits = 1) => Number(value).toFixed(digits);
  addFigureSlide(pptx, {
    title: 'Time for Lab 2',
    assetPath: path.join('assets', 'lab2-break.png')
  });
  addTeachingTableSlide(pptx, {
    title: 'Lab 2 debrief: treatment comparison',
    intro: 'Enhanced minus standard care: three analyses of the same 480 patients.',
    columns: [
      { label: 'MODEL', weight: 3.8 },
      { label: 'TREATMENT EFFECT', weight: 3.1, align: 'center' },
      { label: 'SE', weight: 2.7, align: 'center' },
      { label: '95% CONFIDENCE INTERVAL', weight: 3.9, align: 'center' }
    ],
    rows: [['Treatment only', results.pooled], ['Center fixed', results.fixed],
      ['Center random', results.random]].map(([label, r]) => [label,
      `${f(r.estimate)} m`, `${f(r.se, 2)} m`, `${f(r.lower)} to ${f(r.upper)} m`]),
    takeaway: 'In this balanced trial, all three treatment estimates agree. Both center-adjusted models give the same, smaller SE.'
  });
  addConceptPanelsSlide(pptx, {
    title: 'Lab 2 debrief: variance components and ICC',
    intro: 'Interpret the random-intercept model fitted to the rehabilitation data.',
    panels: [
      { title: 'ESTIMATED VARIANCE COMPONENTS', blocks: [
        { text: `Center variance: ${f(results.random.center_var, 0)} m²`, h: .6 },
        { text: `Individual patient variance: ${f(results.random.patient_var, 0)} m²`, h: .75 },
        { equation: iccEquation, h: .8 },
        { text: `Estimated ICC: ${f(results.random.icc, 2)}`, h: .6, bold: true }
      ] },
      { title: 'TWO INTERPRETATIONS OF THE ICC', blocks: [
        { text: `About ${f(100 * results.random.icc, 0)}% of the remaining outcome variance is attributable to differences between centers.`, h: 1.3 },
        { text: `For two different patients in one center:\nestimated outcome correlation = ${f(results.random.icc, 2)},\nafter accounting for treatment.`, h: 1.65 }
      ] }
    ],
    takeaway: 'The variance components and ICC describe outcome variation remaining after accounting for treatment.',
    drawEquation
  });
}

module.exports = { addLab2Debrief, equations: [iccEquation] };
