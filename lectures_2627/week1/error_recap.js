const path = require('path');
const { addConceptPanelsSlide, addConceptTableSlide } = require('../../slides-common/js/concept_panels');

const meanModel = {
  title: 'Start with a model for the outcome',
  intro: 'For patient i in center j: Yᵢⱼ is walking distance and Tᵢⱼ indicates the assigned treatment.',
  panels: [
    { title: 'SYSTEMATIC PART AND ERROR', blocks: [
      { equation: String.raw`Y_{ij}=\underbrace{\beta_0+\beta_1T_{ij}}_{\text{model-predicted outcome}}+\underbrace{u_{ij}}_{\text{error}}`, h: .9 },
      { text: 'The model predicts an outcome from the patient’s treatment.', h: .85 },
      { text: 'uᵢⱼ is the error: the difference between the observed outcome and that prediction.', h: 1.05 }
    ] },
    { title: 'INTERPRETING THE PREDICTION', blocks: [
      { text: 'Standard care (Tᵢⱼ = 0):', h: .4 },
      { equation: String.raw`\beta_0`, h: .4 },
      { text: 'Enhanced care (Tᵢⱼ = 1):', h: .4 },
      { equation: String.raw`\beta_0+\beta_1`, h: .4 },
      { text: 'β₁ is the difference in model-predicted outcomes between enhanced and standard care.', h: .95 }
    ] }
  ],
  takeaway: 'To complete the statistical model, we need to specify how the errors behave.'
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

function addMeanModelSlide(pptx, drawEquation) {
  return addConceptPanelsSlide(pptx, { ...meanModel, drawEquation });
}
function addErrorRecap(pptx, drawEquation) {
  addConceptTableSlide(pptx, { title: 'Mean, variance and covariance of the errors', rows,
    takeaway: 'These quantities describe where errors are centered, how much they vary, and how they vary together.', drawEquation });
  addConceptPanelsSlide(pptx, { ...covariance, drawEquation });
}
const equations = [...meanModel.panels.flatMap(p => p.blocks.filter(b => b.equation).map(b => b.equation)), ...rows.map(r => r.equation)];
module.exports = { addMeanModelSlide, addErrorRecap, equations };
