const fs = require('fs');
const path = require('path');
const { addConceptPanelsSlide } = require('../../slides-common/js/concept_panels');
const { addTeachingTableSlide } = require('../../slides-common/js/teaching_table');
const { addFigureSlide } = require('../../slides-common/js/pptx_layout_primitives');

const perspective = {
  title: 'Repeat the study with new patients in the same centers',
  intro: 'Return to the question: does the treatment-only error satisfy our assumptions?',
  panels: [
    { title: 'THE SAME SIX CENTERS', blocks: [
      { text: 'Imagine running the study again in the same six centers.', h: .9 },
      { text: 'Each center retains its underlying contribution to the outcomes.', h: .95 }
    ] },
    { title: 'NEW PATIENTS EACH TIME', blocks: [
      { text: 'Recruit new patients for each repetition, with the same treatment allocation within centers.', h: 1.15 },
      { text: 'The individual patient errors vary from one repetition to the next.', h: .95 }
    ] }
  ],
  takeaway: 'This is the fixed-center perspective. We will explore it with two centers and four patient positions.'
};

const setup = {
  title: 'A simulation with two centers and four patients',
  intro: 'All contributions are known in this simulation, so we can calculate each patient’s error.',
  panels: [
    { title: 'THE FOUR PATIENT POSITIONS', blocks: [
      { text: 'Center 1: contribution −10.\nOne standard-care and one enhanced-care patient.', h: 1.0 },
      { text: 'Center 2: contribution +10.\nOne standard-care and one enhanced-care patient.', h: 1.0 },
      { text: 'Baseline: 100. Treatment effect: 5.', h: .8 }
    ] },
    { title: 'OUTCOMES AND TREATMENT-ONLY ERRORS', blocks: [
      { equation: String.raw`Y_{ij}=100+5T_{ij}+c_j+\varepsilon_{ij}`, h: .55 },
      { text: 'Draw independent Normal individual errors: mean 0 and SD 5.', fontSize: 20, h: .8 },
      { text: 'Calculate the treatment-only error using the known treatment component:', fontSize: 20, h: .8 },
      { equation: String.raw`u_{ij}=Y_{ij}-(100+5T_{ij})=c_j+\varepsilon_{ij}`, h: .55 }
    ] }
  ],
  takeaway: 'Every repetition fills the same four patient positions with newly sampled patients.'
};

const consequences = {
  title: 'What are the consequences of omitting center?',
  lead: {
    equation: String.raw`E[\widehat{\beta}_1]=\beta_1+\bar c_{\text{enhanced}}-\bar c_{\text{standard}}`,
    text: 'Expected estimate = true treatment effect + difference in average center contributions.'
  },
  panels: [
    { title: 'WHERE IS THE ESTIMATE CENTERED?', blocks: [
      { text: 'Balanced allocation within each center gives both groups the same average center contribution.', fontSize: 20, h: .95 },
      { equation: String.raw`E[\widehat{\beta}_1]=\beta_1`, h: .4 },
      { text: 'The treatment estimate is unbiased.', fontSize: 20, h: .4 }
    ] },
    { title: 'HOW MUCH DOES THE ESTIMATE VARY?', blocks: [
      { text: 'Center contributions cancel in every repetition with new patients.', fontSize: 20, h: .65 },
      { text: 'Only the individual patient errors make the estimate vary.', fontSize: 20, h: .65 },
      { text: 'The residual spread also contains center differences, so the usual standard error is too large on average.', fontSize: 20, h: 1.0 }
    ] }
  ],
  takeaway: 'Here, balanced allocation gives an unbiased treatment estimate, but its standard error is too large on average.'
};

const adjustment = {
  title: 'Include center in the expected outcome',
  intro: 'Use the known center contributions to account for the two different error means.',
  panels: [
    { title: 'TREATMENT-ONLY EXPECTED OUTCOME', blocks: [
      { equation: String.raw`100+5T_{ij}`, h: .55 },
      { text: 'The error contains the center contribution:', h: .8 },
      { equation: String.raw`u_{ij}=c_j+\varepsilon_{ij}`, h: .55 },
      { text: 'Expected error: −10 in center 1 and +10 in center 2.', h: .85 }
    ] },
    { title: 'CENTER-ADJUSTED EXPECTED OUTCOME', blocks: [
      { equation: String.raw`100+5T_{ij}+c_j`, h: .55 },
      { text: 'The remaining error is the individual error:', h: .8 },
      { equation: String.raw`\varepsilon_{ij}=Y_{ij}-(100+5T_{ij}+c_j)`, h: .55 },
      { text: 'Expected error: 0 in all four patient positions.', h: .85 }
    ] }
  ],
  takeaway: 'In real data, the center contributions are unknown. We can estimate them using center indicators.'
};

function addFixedCenterSampling(pptx, drawEquation) {
  [perspective, setup].forEach(spec => addConceptPanelsSlide(pptx, { ...spec, drawEquation }));
  const simulation = JSON.parse(fs.readFileSync(path.join('assets', 'fixed-center-sampling.json'), 'utf8'));
  addTeachingTableSlide(pptx, {
    title: 'One repetition: four new patients',
    intro: 'Draw one independent individual error for each patient position and generate the outcome.',
    columns: [
      { label: 'CENTER', weight: 1.8 }, { label: 'PATIENT POSITION', weight: 3.6 },
      { label: 'OUTCOME Yᵢⱼ', weight: 3.0, align: 'center' },
      { label: 'ERROR uᵢⱼ', weight: 3.0, align: 'center' }
    ],
    rows: simulation.positions.map(p => [String(p.center), p.treatment + ' care',
      p.first_outcome.toFixed(1), p.first_error.toFixed(1)]),
    takeaway: 'Subtract the known treatment-only component: 100 for standard care and 105 for enhanced care.'
  });
  for (const n of simulation.stages) {
    addFigureSlide(pptx, {
      title: `Repeated sampling: ${n.toLocaleString('en-GB')} studies`,
      assetPath: path.join('assets', `fixed-center-errors-${n}.png`),
      caption: 'Each panel follows one patient position. Dashed line: expected error. Solid line: zero.',
      captionFontSize: 18, captionMinFontSize: 18
    });
  }
  addFigureSlide(pptx, {
    title: 'Zero covariance within and across centers',
    assetPath: path.join('assets', 'fixed-center-error-pairs.png'),
    caption: 'Each point pairs errors from one repetition. Dashed lines mark the expected errors.',
    captionFontSize: 18, captionMinFontSize: 18
  });
  addTeachingTableSlide(pptx, {
    title: 'Do these errors satisfy our assumptions?',
    intro: 'Consider the error distribution for each of the four patient positions.',
    columns: [
      { label: 'ASSUMPTION', weight: 3.0 }, { label: 'STATUS', weight: 1.8 },
      { label: 'IN THIS SIMULATION', weight: 7.4 }
    ],
    rows: [
      ['Zero mean', 'Violated', 'Mean −10 for both positions in center 1; mean +10 for both positions in center 2.'],
      ['Equal variance', 'Satisfied', 'Variance 25 (SD 5) for every position.'],
      ['Independence', 'Satisfied', 'Errors vary independently within and across centers, giving zero covariance for both pairs.'],
      ['Normality', 'Satisfied', 'Each error distribution is Normal, centered at its own center’s contribution.']
    ]
  });
  addConceptPanelsSlide(pptx, { ...consequences, drawEquation });
  addConceptPanelsSlide(pptx, { ...adjustment, drawEquation });
}

const equations = [setup, consequences, adjustment].flatMap(slide => [
  ...(slide.lead ? [slide.lead.equation] : []),
  ...slide.panels.flatMap(panel => panel.blocks.filter(block => block.equation).map(block => block.equation))
]);
module.exports = { addFixedCenterSampling, equations };
