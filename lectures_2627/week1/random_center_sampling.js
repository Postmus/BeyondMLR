const fs = require('fs');
const path = require('path');
const { addConceptPanelsSlide } = require('../../slides-common/js/concept_panels');
const { addTeachingTableSlide } = require('../../slides-common/js/teaching_table');
const { addFigureSlide } = require('../../slides-common/js/pptx_layout_primitives');
const { consequences: sharedConsequences } = require('./omitted_center_consequences');

const setup = {
  title: 'Explaining the random-center errors',
  panels: [
    { title: 'MODEL AND PARAMETERS', blocks: [
      { equation: String.raw`Y_{ij}=100+5T_{ij}+c_j+\varepsilon_{ij}`, h: .55 },
      { equation: String.raw`c_j\overset{\mathrm{iid}}{\sim}N(0,10^2)`, h: .5 },
      { equation: String.raw`\varepsilon_{ij}\overset{\mathrm{iid}}{\sim}N(0,5^2)`, h: .5 },
      { text: 'Tᵢⱼ = 0 for standard care and 1 for enhanced care.', h: .8 },
      { text: 'Center effects and individual errors are independent of each other.', h: .8 }
    ] },
    { title: 'WHAT CHANGES ACROSS STUDIES?', blocks: [
      { text: 'Draw new center effects in every repetition: mean 0 and SD 10.', h: .85 },
      { text: 'Patients in the same center share the same drawn center effect.', h: .85 },
      { text: 'Expected outcomes given cⱼ:\nStandard: 100 + cⱼ\nEnhanced: 105 + cⱼ', h: 1.1 },
      { text: 'Averaging over centers gives expected outcomes of 100 and 105.', h: .85 }
    ] }
  ],
  takeaway: 'Return to the two-center teaching example, now drawing new center effects as well as new patients.'
};

const treatmentOnlyError = String.raw`u_{ij}=Y_{ij}-(100+5T_{ij})=c_j+\varepsilon_{ij}`;
const consequences = {
  ...sharedConsequences,
  takeaway: 'Different error structure, same consequences: fixed center effects give center-specific error means; random center effects create dependence within centers.'
};

function addRandomCenterSampling(pptx, drawEquation) {
  const sim = JSON.parse(fs.readFileSync(path.join('assets', 'random-center-sampling.json'), 'utf8'));
  const f = value => Number(value).toFixed(1);
  addConceptPanelsSlide(pptx, { ...setup, drawEquation });
  addTeachingTableSlide(pptx, {
    title: 'One study: centers and patients',
    intro: 'Draw two center effects, then one patient per treatment in each center (rounded values).',
    columns: [
      { label: 'CENTER', weight: 1.3 }, { label: 'TREATMENT', weight: 2.5 },
      { label: 'CENTER EFFECT cⱼ', weight: 2.8, align: 'center' },
      { label: 'DRAWN ERROR εᵢⱼ', weight: 2.8, align: 'center' },
      { label: 'OUTCOME Yᵢⱼ', weight: 2.8, align: 'center' }
    ],
    rows: sim.positions.map(p => [String(p.center), p.treatment + ' care',
      f(p.center_effect), f(p.individual_error), f(p.first_outcome)]),
    takeaway: 'Outcome = 100 + 5Tᵢⱼ + cⱼ + εᵢⱼ. Each center shares cⱼ; individual errors are independent.'
  });
  addTeachingTableSlide(pptx, {
    title: 'What is left in the treatment-only error?',
    intro: 'Use the same sample, but leave center out of the prediction.',
    leadEquation: treatmentOnlyError, drawEquation,
    columns: [
      { label: 'CENTER', weight: 1.3 }, { label: 'TREATMENT', weight: 2.5 },
      { label: 'OUTCOME Yᵢⱼ', weight: 2.6, align: 'center' },
      { label: 'PREDICTED OUTCOME', weight: 3.2, align: 'center' },
      { label: 'ERROR uᵢⱼ', weight: 2.6, align: 'center' }
    ],
    rows: sim.positions.map(p => [String(p.center), p.treatment + ' care',
      f(p.first_outcome), String(p.treatment_component), f(p.first_error)]),
    takeaway: 'Repeat the study, drawing new center effects and new individual errors each time.'
  });
  for (const n of [sim.repetitions]) addFigureSlide(pptx, {
    title: 'Random centers: the error distributions',
    assetPath: path.join('assets', `random-center-errors-${n}.png`),
    caption: 'Each panel follows one patient position. Dashed line: expected error, now zero in every panel.',
    captionFontSize: 18, captionMinFontSize: 18,
    takeaway: n === sim.repetitions
      ? 'Averaging over new centers and patients, every error has the same Normal distribution: mean 0 and variance 125.' : ''
  });
  addFigureSlide(pptx, {
    title: 'Errors vary together within the same center',
    assetPath: path.join('assets', 'random-center-error-pairs.png'),
    caption: 'Each point pairs errors from one repetition. Dashed lines mark the expected errors.',
    captionFontSize: 18, captionMinFontSize: 18,
    takeaway: 'Same center: a shared random effect creates covariance 100. Different centers: independent errors, covariance 0.'
  });
  addTeachingTableSlide(pptx, {
    title: 'Do these errors satisfy our assumptions?',
    intro: 'Consider repetitions that draw both new center effects and new individual errors.',
    columns: [{ label: 'ASSUMPTION', weight: 3 }, { label: 'STATUS', weight: 1.8 },
      { label: 'IN THIS SIMULATION', weight: 7.4 }],
    rows: [
      ['Zero mean', 'Satisfied', 'Mean 0 for every position.\nAverage over new centers and patients.'],
      ['Equal variance', 'Satisfied', 'Variance 100 + 25 = 125 for every position.'],
      ['Independence', 'Violated', 'Errors share cⱼ within a center: covariance 100. Errors from different centers are independent.'],
      ['Normality', 'Satisfied', 'Each error is a sum of independent Normal center and individual contributions.']
    ],
    takeaway: 'The treatment-only errors are identically Normally distributed, but are not independent within centers.',
    fontSize: 20
  });
  addConceptPanelsSlide(pptx, { ...consequences, drawEquation });
}

const equations = [treatmentOnlyError, ...[setup, consequences].flatMap(slide =>
  slide.panels.flatMap(panel => panel.blocks.filter(block => block.equation).map(block => block.equation)))];
module.exports = { addRandomCenterSampling, equations };
