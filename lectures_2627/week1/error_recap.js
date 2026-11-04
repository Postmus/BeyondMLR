const path = require('path');
const { addConceptPanelsSlide, addConceptTableSlide } = require('../../slides-common/js/concept_panels');
const { treatmentOnlyAssumption } = require('./initial_model');

const meanModel = {
  title: 'The treatment-only model',
  intro: 'Patient i, center j: Yᵢⱼ is walking distance; Tᵢⱼ = 0 (standard) or 1 (enhanced).',
  panels: [
    { title: 'SYSTEMATIC PART AND ERROR', blocks: [
      { equation: String.raw`Y_{ij}=\underbrace{\beta_0+\beta_1T_{ij}}_{\text{model-predicted outcome}}+\underbrace{u_{ij}}_{\text{error}}`, h: .9 },
      { equation: treatmentOnlyAssumption, h: .55, gap: .18 },
      { text: 'β₁ is the expected difference in walking distance between enhanced and standard care.', h: .95 },
      { text: 'uᵢⱼ is the individual error. Errors are independent and identically distributed: Normal, with mean zero and a common variance.', h: 1.45 }
    ] },
    { title: 'MODEL-PREDICTED OUTCOMES', blocks: [
      { image: path.join('assets', 'conditional-distributions.png'), h: 4.2 }
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
function addErrorRecap(pptx, drawEquation) {
  addConceptTableSlide(pptx, { title: 'Mean, variance and covariance of the errors', rows,
    takeaway: 'These quantities describe where errors are centered, how much they vary, and how they vary together.', drawEquation });
  addConceptPanelsSlide(pptx, { ...covariance, drawEquation });
}
const equations = [...meanModel.panels.flatMap(p => p.blocks.filter(b => b.equation).map(b => b.equation)), ...rows.map(r => r.equation)];
module.exports = { addTreatmentOnlyModelSlide, addErrorRecap, equations };
