const { addConceptPanelsSlide } = require('../../slides-common/js/concept_panels');
const { addFixedCenterSampling } = require('./fixed_center_sampling');

const bridge = {
  title: 'Do we want a separate parameter for every center?',
  intro: 'The fixed-center model accounts for center differences. Now reconsider how we describe them.',
  panels: [
    { title: 'SEPARATE CENTER PARAMETERS', blocks: [
      { text: 'Estimate a mean adjustment for each of the centers in this trial.', h: 1.0 },
      { text: 'This provides a familiar way to adjust the treatment comparison for center.', h: 1.05 }
    ] },
    { title: 'A MODEL FOR VARIATION ACROSS CENTERS', blocks: [
      { text: 'We may also want to describe how much outcomes vary across a population of centers.', h: 1.15 },
      { text: 'Could we represent the center contributions using a shared distribution?', h: 1.05 }
    ] }
  ],
  takeaway: 'Next: model center contributions using a distribution, within the same randomized block design.'
};

function addFixedCenterSection(pptx, drawEquation, results) {
  addFixedCenterSampling(pptx, drawEquation);
  const f = (x, digits = 1) => Number(x).toFixed(digits);
  addConceptPanelsSlide(pptx, {
    title: 'Lab 1 debrief: accounting for center',
    intro: 'Compare the treatment estimates and their uncertainty for the same 480 patients.',
    panels: [['TREATMENT ONLY', results.pooled], ['CENTER AS A FIXED EFFECT', results.fixed]].map(([title, result]) => ({
      title, blocks: [
        { text: `Treatment difference: ${f(result.estimate)} m`, h: .55 },
        { text: `Standard error: ${f(result.se, 2)} m`, h: .55 },
        { text: `95% confidence interval: ${f(result.lower)} to ${f(result.upper)} m`, h: .85 },
        { text: `Residual SD: ${f(result.residual_sd)} m`, h: .55 }
      ]
    })),
    takeaway: 'In this balanced trial, the treatment estimate is unchanged. Accounting for center reduces its standard error.',
    drawEquation
  });
}

function addCenterDistributionBridge(pptx, drawEquation) {
  return addConceptPanelsSlide(pptx, { ...bridge, drawEquation });
}

const equations = [];
module.exports = { addFixedCenterSection, addCenterDistributionBridge, equations };
