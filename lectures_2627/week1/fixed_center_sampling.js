const fs = require('fs');
const path = require('path');
const { addConceptPanelsSlide } = require('../../slides-common/js/concept_panels');
const { addTeachingTableSlide } = require('../../slides-common/js/teaching_table');
const { addAssumptionFocusSlide } = require('../../slides-common/js/course_builders');
const { addFigureSlide } = require('../../slides-common/js/pptx_layout_primitives');
const { consequences } = require('./omitted_center_consequences');

const simulationIntroduction = {
  title: 'Using simulation to study model misspecification',
  statement: 'A model is misspecified when it does not adequately describe how the outcomes arise. Simulation lets us investigate the consequences under conditions we control.',
  points: [
    { title: '1  SPECIFY HOW OUTCOMES ARE GENERATED',
      text: 'Choose a model and its parameter values, so we know what is true in the simulation.' },
    { title: '2  GENERATE REPEATED SAMPLES',
      text: 'Repeat the same study many times, drawing new observations each time.' },
    { title: '3  EXAMINE A MODEL THAT LEAVES SOMETHING OUT',
      text: 'Check the error assumptions and examine the consequences for estimates and uncertainty.' }
  ],
  reminder: 'We start by examining the errors when a treatment-only model leaves out differences between centers.'
};

const setup = {
  title: 'Step 1: Specify the data-generating model',
  panels: [
    { title: 'MODEL AND PARAMETERS', blocks: [
      { equation: String.raw`Y_{ij}=100+5T_{ij}+c_j+\varepsilon_{ij}`, h: .55 },
      { equation: String.raw`\varepsilon_{ij}\overset{\mathrm{iid}}{\sim}N(0,5^2)`, h: .5 },
      { text: 'Tᵢⱼ = 0 for standard care and 1 for enhanced care.', h: .8 },
      { text: 'Baseline: 100. Enhanced care adds 5 to the expected outcome.', h: .8 },
      { text: 'Individual errors are independent, with mean 0 and SD 5.', h: .7 }
    ] },
    { title: 'EXPECTED OUTCOMES', blocks: [
      { text: 'Center 1: c₁ = −10.\nCenter 2: c₂ = +10.', h: .8 },
      { table: {
        columns: [{ label: 'CENTER', weight: 1.4 },
          { label: 'STANDARD', weight: 2, align: 'center' },
          { label: 'ENHANCED', weight: 2, align: 'center' }],
        rows: [['1', '90', '95'], ['2', '110', '115']],
        fontSize: 22, headerFontSize: 14, headerHeight: .55
      }, h: 1.8 },
      { text: 'Each center effect shifts both treatment means by the same amount.', h: .85 }
    ] }
  ],
  takeaway: 'This model specifies the outcome distribution for a patient, given their treatment and center.'
};

const treatmentOnlyError = String.raw`u_{ij}=Y_{ij}-(100+5T_{ij})=c_j+\varepsilon_{ij}`;

function addFixedCenterSampling(pptx, drawEquation) {
  addAssumptionFocusSlide(pptx, simulationIntroduction);
  addConceptPanelsSlide(pptx, { ...setup, drawEquation });
  const simulation = JSON.parse(fs.readFileSync(path.join('assets', 'fixed-center-sampling.json'), 'utf8'));
  addTeachingTableSlide(pptx, {
    title: 'Step 2: Generate a sample',
    intro: 'Sample four patients: one receiving each treatment in each center.',
    columns: [
      { label: 'CENTER', weight: 1.3 }, { label: 'TREATMENT', weight: 2.5 },
      { label: 'EXPECTED OUTCOME', weight: 3, align: 'center' },
      { label: 'DRAWN ERROR εᵢⱼ', weight: 2.8, align: 'center' },
      { label: 'OUTCOME Yᵢⱼ', weight: 2.6, align: 'center' }
    ],
    rows: simulation.positions.map(p => [String(p.center), p.treatment + ' care',
      String(p.treatment_component + p.center_effect),
      (p.first_error - p.center_effect).toFixed(1), p.first_outcome.toFixed(1)]),
    takeaway: 'Draw each error independently from N(0, 5²), then add it to that patient’s expected outcome.'
  });
  addTeachingTableSlide(pptx, {
    title: 'Step 3: Examine the treatment-only errors',
    intro: 'Use the same sample, but leave center out of the prediction.',
    leadEquation: treatmentOnlyError,
    drawEquation,
    columns: [
      { label: 'CENTER', weight: 1.3 }, { label: 'TREATMENT', weight: 2.5 },
      { label: 'OUTCOME Yᵢⱼ', weight: 2.6, align: 'center' },
      { label: 'PREDICTED OUTCOME', weight: 3.2, align: 'center' },
      { label: 'ERROR uᵢⱼ', weight: 2.6, align: 'center' }
    ],
    rows: simulation.positions.map(p => [String(p.center), p.treatment + ' care',
      p.first_outcome.toFixed(1), String(p.treatment_component), p.first_error.toFixed(1)]),
    takeaway: 'Repeat this process with new patients in the same centers to study the distribution of these errors.'
  });
  for (const n of simulation.stages) {
    addFigureSlide(pptx, {
      title: `Repeated sampling: ${n.toLocaleString('en-GB')} studies`,
      assetPath: path.join('assets', `fixed-center-errors-${n}.png`),
      caption: 'Each panel follows one patient position. Dashed line: expected error. Solid line: zero.',
      takeaway: n === simulation.repetitions
        ? 'The errors are Normally distributed with equal variance, but their means differ by center: −10 and +10.'
        : '',
      captionFontSize: 18, captionMinFontSize: 18
    });
  }
  addFigureSlide(pptx, {
    title: 'Zero covariance within and across centers',
    assetPath: path.join('assets', 'fixed-center-error-pairs.png'),
    caption: 'Each point pairs errors from one repetition. Dashed lines mark the expected errors.',
    takeaway: 'In this simulation, errors remain independent within and across centers, so their covariance is zero.',
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
  addFigureSlide(pptx, {
    title: 'Bias and precision across repeated studies',
    assetPath: path.join('assets', 'bias-precision.png')
  });
  addConceptPanelsSlide(pptx, { ...consequences, drawEquation });
  addFigureSlide(pptx, {
    title: 'Time for Lab 1',
    assetPath: path.join('assets', 'lab1-break.png')
  });
}

const equations = [treatmentOnlyError, ...[setup, consequences].flatMap(slide => [
  ...(slide.lead ? [slide.lead.equation] : []),
  ...slide.panels.flatMap(panel => panel.blocks.filter(block => block.equation).map(block => block.equation))
])];
module.exports = { addFixedCenterSampling, equations };
