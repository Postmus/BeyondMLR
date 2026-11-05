const { addConceptPanelsSlide } = require('../../slides-common/js/concept_panels');
const { addFigureSlide } = require('../../slides-common/js/pptx_layout_primitives');
const { addTeachingTableSlide } = require('../../slides-common/js/teaching_table');
const path = require('path');

// Projected prompts for lecturer-led demonstrations. All comparisons of analysis
// models reuse the same generated studies.
function addSimulationApproach(pptx) {
  addTeachingTableSlide(pptx, {
    title: 'Using simulation to investigate the consequences of ignoring center',
    intro: 'To investigate whether ignoring center matters for the treatment comparison, we simulate repeated trials from a known model and analyse them using the treatment-only model.',
    introHeight: 1.05,
    columns: [
      { label: 'STEP', weight: 1.1, align: 'center' },
      { label: 'SIMULATION WORKFLOW', weight: 11.1 }
    ],
    rows: [
      ['1', 'Specify a data-generating model that reflects the study design.'],
      ['2', 'Generate a dataset from that model.'],
      ['3', 'Fit the treatment-only model, retaining the treatment estimate and its estimated SE.'],
      ['4', 'Repeat the generation and analysis to build the estimator’s sampling distribution.']
    ],
    fontSize: 23
  });
}

function addSimulationConcept(pptx) {
  addConceptPanelsSlide(pptx, {
    title: 'Assessing bias and calibration',
    panels: [
      { title: 'BIAS: LOCATION OF THE ESTIMATES', blocks: [
        { text: 'We know the true treatment effect because we specify it in the simulation.', h: 1.0 },
        { text: 'Compare the average treatment estimate across studies with that true effect.', h: 1.0 },
        { text: 'Bias is the difference between the expected estimate and the true effect.', h: 1.0 }
      ] },
      { title: 'CALIBRATION OF ESTIMATED UNCERTAINTY', blocks: [
        { text: 'Empirical SE: the standard deviation of treatment estimates across studies.', h: 1.0 },
        { text: 'Mean estimated SE: the average of the SEs estimated by the model across studies.', h: 1.0 },
        { text: 'Does the estimated uncertainty match the sampling variability?', h: 1.0 }
      ] }
    ],
    takeaway: 'Calibration matters for confidence-interval coverage and type I error.'
  });
}

function addFixedSimulationOpening(pptx) {
  addConceptPanelsSlide(pptx, {
    title: 'Our experiment: new patients in the same centers',
    panels: [
      { title: 'HOLD THE STUDY CONDITIONS FIXED', blocks: [
        { text: 'Six centers, 20 patients per center. Randomize 10 to each treatment within every center.', h: 1.0 },
        { text: 'Baseline: 360 m. True treatment effect: 10 m. Independent Normal individual errors: SD 24 m.', h: 1.0 },
        { text: 'Fixed center effects A to F (m):\n−17, +16, +8, −29, +27, −5.', h: .85 }
      ] },
      { title: 'GENERATE DATA AND FIT THE MODEL', blocks: [
        { text: 'Draw new individual errors for new patients. Keep the six center effects unchanged.', h: 1.0 },
        { text: 'Fit the treatment-only model to every simulated study.', h: .95 },
        { text: 'Retain each treatment estimate and its estimated standard error.', h: .95 }
      ] }
    ],
    takeaway: 'The generating model includes center. The treatment-only analysis omits it.'
  });
  addSimulationConcept(pptx);
  addFigureSlide(pptx, {
    title: 'Live demonstration: omitting center',
    assetPath: path.join('assets', 'live-simulation-demo.png')
  });
  addConceptPanelsSlide(pptx, {
    title: 'Summary: unbiased estimates, overstated uncertainty',
    panels: [
      { title: 'ESTIMATE AND ESTIMATED SE', blocks: [
        { text: 'The treatment estimator is unbiased: estimates are centered at the true effect of 10 m.', h: 1.0 },
        { text: 'The mean estimated SE is larger than the empirical SE.', h: 1.0, bold: true },
        { text: 'The model overestimates the sampling variability of the treatment estimator.', h: 1.0 }
      ] },
      { title: 'CONSEQUENCES FOR INFERENCE', blocks: [
        { text: 'Consequently, its usual 95% confidence intervals overcover: they contain the true treatment effect more than 95% of the time.', h: 1.5 },
        { text: 'When the true treatment effect is zero, its tests reject less often than the nominal 5% (type I error).', h: 1.5 }
      ] }
    ]
  });
}

function addRandomSimulationDemonstration(pptx) {
  addConceptPanelsSlide(pptx, {
    title: 'Live demonstration: draw new centers too',
    intro: 'Retain six centers, 10 patients per treatment per center, and a true treatment effect of 10 m.',
    panels: [
      { title: 'CHANGE WHAT VARIES ACROSS STUDIES', blocks: [
        { text: 'Draw independent Normal center effects in every study: mean 0, SD 20 m.', h: 1.0 },
        { text: 'Retain baseline 360 m and independent Normal individual errors with SD 24 m.', h: 1.0 },
        { text: 'Patients in the same center share its drawn center effect.', h: .9 }
      ] },
      { title: 'REPEAT THE TWO CHECKS', blocks: [
        { text: 'Start with treatment only: check bias and compare empirical with estimated SE.', h: 1.0 },
        { text: 'Select the random-intercept analysis for the same simulated studies.', h: 1.0 },
        { text: 'Does accounting for center again improve agreement with the empirical SE?', h: 1.0 }
      ] }
    ],
    takeaway: 'A similar pattern can have a different explanation. What changes in the treatment-only errors?'
  });
}

module.exports = { addSimulationApproach, addSimulationConcept, addFixedSimulationOpening, addRandomSimulationDemonstration };
