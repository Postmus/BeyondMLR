const path = require('path');
const { addConceptPanelsSlide, addConceptTableSlide } = require('../../slides-common/js/concept_panels');
const { treatmentOnlyAssumption } = require('./initial_model');

const meanModel = {
  title: 'The treatment-only model',
  intro: 'Patient i, center j: Yᵢⱼ is walking distance; Tᵢⱼ = 0 (standard) or 1 (enhanced).',
  panels: [
    { title: 'SYSTEMATIC PART AND ERROR', blocks: [
      { equation: String.raw`Y_{ij}=\underbrace{\beta_0+\beta_1T_{ij}}_{\text{model-predicted outcome}}+\underbrace{u_{ij}}_{\text{individual error}}`, h: .95, gap: .22 },
      { text: 'β₀ is the expected walking distance under standard care.', h: .72 },
      { text: 'β₁ is the expected difference between enhanced and standard care.', h: .95 },
      { text: 'uᵢⱼ is an individual’s deviation from the model-predicted outcome.', h: .95 }
    ] },
    { title: 'ASSUMPTIONS AND IMPLICATIONS', blocks: [
      { equation: treatmentOnlyAssumption, h: .55 },
      { text: 'Independent Normal errors, with mean zero and a common variance.', fontSize: 20, h: .65 },
      { text: 'For a given treatment, β₀ + β₁Tᵢⱼ is a constant.', fontSize: 20, h: .65 },
      { text: 'Applying the recap rules:\nadding a constant (slide 9)', fontSize: 18, muted: true, h: .56 },
      { equation: String.raw`\begin{aligned}E(Y_{ij})&=\beta_0+\beta_1T_{ij}+E(u_{ij})\\&=\beta_0+\beta_1T_{ij}\end{aligned}`, h: .85 },
      { equation: String.raw`\operatorname{Var}(Y_{ij})=\operatorname{Var}(u_{ij})=\sigma_u^2`, h: .42 }
    ] }
  ]
};

const outcomeDistributions = {
  title: 'From errors to outcome distributions',
  panels: [
    { title: 'ADDING A CONSTANT PRESERVES THE NORMAL SHAPE', blocks: [
      { equation: String.raw`Y_{ij}\sim N\!\left(\beta_0+\beta_1T_{ij},\,\sigma_u^2\right)`, h: .50 },
      { image: path.join('assets', 'outcome-distributions-wide.png'), h: 3.75 },
      { text: 'The means differ by β₁. Both treatment groups have the same spread: SD σᵤ.', fontSize: 20, h: .40 }
    ] }
  ]
};

const rows = [
  { concept: 'Expected\nvalue', meaning: 'The average error over many repetitions.',
    assumption: 'Zero for every patient.', equation: String.raw`E[u_{ij}]=0` },
  { concept: 'Variance', meaning: 'How widely a patient’s error varies around its mean.',
    assumption: 'The same for every patient.', equation: String.raw`\operatorname{Var}(u_{ij})=\sigma_u^2` },
  { concept: 'Covariance', meaning: 'How two patients’ errors vary together around their respective means.',
    assumption: 'Zero for every pair of different patients.', equation: String.raw`\operatorname{Cov}(u_{1j},u_{2j})=0` }
];

const covariance = {
  title: 'Independence implies zero covariance',
  intro: 'Each point pairs two patients’ errors from one repetition. Left: independently generated errors.',
  panels: [
    { title: 'INDEPENDENT ERRORS', blocks: [
      { image: path.join('assets', 'covariance-zero.png'), h: 2.25 },
      { text: 'Knowing one patient’s error gives no information about the other’s. Their covariance is zero.', fontSize: 20, h: .95 }
    ] },
    { title: 'DEPENDENT ERRORS: POSITIVE COVARIANCE', blocks: [
      { image: path.join('assets', 'covariance-positive.png'), h: 2.25 },
      { text: 'The errors tend to move together: above or below their respective means.', fontSize: 20, h: .95 }
    ] }
  ],
  takeaway: 'Both panels have the same means and variances. The dashed lines mark the expected errors.'
};

function addTreatmentOnlyModelSlide(pptx, drawEquation) {
  return addConceptPanelsSlide(pptx, { ...meanModel, drawEquation });
}
function addOutcomeDistributionsSlide(pptx, drawEquation) {
  return addConceptPanelsSlide(pptx, { ...outcomeDistributions, drawEquation });
}
function addErrorRecap(pptx, drawEquation) {
  addConceptTableSlide(pptx, { title: 'Mean, variance and covariance of the errors', rows,
    takeaway: 'These quantities describe where errors are centered, how much they vary, and how they vary together.', drawEquation });
  addConceptPanelsSlide(pptx, { ...covariance, drawEquation });
}
const equations = [...[meanModel, outcomeDistributions].flatMap(slide => slide.panels.flatMap(p => p.blocks.filter(b => b.equation).map(b => b.equation))), ...rows.map(r => r.equation)];
module.exports = { addTreatmentOnlyModelSlide, addOutcomeDistributionsSlide, addErrorRecap, equations };
