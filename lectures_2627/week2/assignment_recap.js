const path = require('path');
const { addBulletVisualSlide } = require('../../slides-common/js/pptx_layout_primitives');
const { addTeachingTableSlide } = require('../../slides-common/js/teaching_table');
const { addConceptPanelsSlide } = require('../../slides-common/js/concept_panels');
const { addAdditiveExample } = require('./additive_example');

function addAssignmentRecap(pptx, drawEquation, allocation) {
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
  addAdditiveExample(pptx, drawEquation);

  const f = n => Number(n).toFixed(2);
  const p = n => n < .001 ? '< .001' : Number(n).toFixed(3);
  const results = allocation.results.filter(r => r.design === 'Assignment 1 balanced');
  const recap = addTeachingTableSlide(pptx, {
    title: 'Assignment 1 recap: comparing the models',
    intro: 'Modified-minus-standard effects, estimated SEs and 95% CIs in µg/mg.',
    introHeight: .55,
    columns: [
      { label: 'Model', weight: 3.4 },
      { label: 'Estimate', weight: 1.45, align: 'center' },
      { label: 'Estimated SE', weight: 1.65, align: 'center' },
      { label: '95% CI', weight: 2.6, align: 'center' },
      { label: 'p-value', weight: 1.35, align: 'center' }
    ],
    rows: results.map(r => [r.model, f(r.estimate), f(r.se), `${f(r.lower_95)} to ${f(r.upper_95)}`, p(r.p)]),
    fontSize: 23, tableOptions: { headerFontSize: 19, headerHeight: .72, firstColumnBold: false },
    takeaway: 'All three models estimate the same protocol effect. Accounting for donor reduces the estimated standard error.'
  });
  recap.addNotes('');

  const feedback = addConceptPanelsSlide(pptx, {
    title: 'Assignment 1: feedback on your submissions',
    panels: [{ title: 'PLACEHOLDER', blocks: [
      { text: 'To be completed after reviewing the submissions.', h: .8, fontSize: 24, gap: .3 },
      { text: 'Two or three observations about the analyses, interpretations and reporting.', h: 1.1, fontSize: 24 }
    ] }]
  });
  feedback.addNotes('');
}

module.exports = { addAssignmentRecap };
