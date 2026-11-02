const { addConceptPanelsSlide } = require('../../slides-common/js/concept_panels');
const { addFixedCenterSampling } = require('./fixed_center_sampling');

const model = {
  title: 'Account for center with indicator variables',
  intro: 'Return to the six-center trial: add five indicators and use center 1 as the reference.',
  panels: [{ title: 'FAMILIAR MULTIPLE LINEAR REGRESSION', blocks: [
    { equation: String.raw`Y_{ij}=\alpha+\beta_1T_{ij}+\gamma_2D_{2,ij}+\cdots+\gamma_6D_{6,ij}+\varepsilon_{ij}`, h: .68 },
    { text: 'Dₖ,ᵢⱼ = 1 if patient i in center j belongs to center k, and 0 otherwise.', h: .52 },
    { text: 'α: the expected standard-care outcome in center 1.', h: .48 },
    { text: 'γₖ: the difference between center k and center 1 under standard care.', h: .48 },
    { text: 'β₁: the enhanced-versus-standard mean difference, common to all centers.', h: .48 }
  ] }],
  takeaway: 'Five center indicators allow six center-specific means, with one common treatment effect.'
};

const interpretation = {
  title: 'Interpret the center-adjusted model',
  intro: 'Each center has its own mean adjustment; treatment has one common effect.',
  panels: [
    { title: 'EXPECTED OUTCOMES IN CENTER j', blocks: [
      { text: 'Standard care:', h: .4 },
      { equation: String.raw`\alpha+\gamma_j`, h: .4 },
      { text: 'Enhanced care:', h: .4 },
      { equation: String.raw`\alpha+\gamma_j+\beta_1`, h: .4 },
      { text: 'For reference center 1, γ₁ = 0.', h: .7 }
    ] },
    { title: 'PARAMETERS AND ERRORS', blocks: [
      { text: 'Each observed center receives a separate estimated mean adjustment.', h: .95 },
      { text: 'Patient differences around these center-specific means remain in the error.', h: .95 },
      { text: 'We assume errors have zero mean and equal variance, are Normal, and are independent across patients.', fontSize: 20, h: 1.15 }
    ] }
  ],
  takeaway: 'Center differences now belong to the systematic part of the model.'
};

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
  [model, interpretation].forEach(spec => addConceptPanelsSlide(pptx, { ...spec, drawEquation }));
  const f = (x, digits = 1) => Number(x).toFixed(digits);
  addConceptPanelsSlide(pptx, {
    title: 'What changes when we adjust for center?',
    intro: 'Compare the two regression analyses of the same 480 patients.',
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
  addConceptPanelsSlide(pptx, { ...bridge, drawEquation });
}

const equations = [model, interpretation].flatMap(slide => slide.panels.flatMap(panel =>
  panel.blocks.filter(block => block.equation).map(block => block.equation)));
module.exports = { addFixedCenterSection, equations };
