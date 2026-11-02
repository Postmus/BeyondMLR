const path = require('path');
const { addConceptPanelsSlide, addConceptTableSlide } = require('../../slides-common/js/concept_panels');

const meanModel = {
  title: 'Start by modelling the expected outcome',
  intro: 'For patient i in center j: Yᵢⱼ is walking distance and Tᵢⱼ indicates the assigned treatment.',
  panels: [
    { title: 'SYSTEMATIC PART AND ERROR', blocks: [
      { equation: String.raw`Y_{ij}=\underbrace{\beta_0+\beta_1T_{ij}}_{\text{expected outcome}}+\underbrace{u_{ij}}_{\text{error}}`, h: .9 },
      { text: 'uᵢⱼ is the error term: the deviation of the observed outcome from the model’s expected outcome.', h: .97 },
      { equation: String.raw`E[u_{ij}]=0`, h: .4 },
      { text: 'The model specifies zero expected error for every patient.', h: .7 }
    ] },
    { title: 'INTERPRETING THE EXPECTED OUTCOME', blocks: [
      { text: 'Standard care (Tᵢⱼ = 0):', h: .4 },
      { equation: String.raw`\beta_0`, h: .4 },
      { text: 'Enhanced care (Tᵢⱼ = 1):', h: .4 },
      { equation: String.raw`\beta_0+\beta_1`, h: .4 },
      { text: 'β₁ is the modelled mean difference between enhanced and standard care.', h: .95 }
    ] }
  ],
  takeaway: 'This is a proposed model for the expected outcomes. We still need to describe the errors.'
};

const rows = [
  { concept: 'Expected\nvalue', meaning: 'The average error over many repetitions.',
    assumption: 'Zero for every patient.', equation: String.raw`E[u_{ij}]=0` },
  { concept: 'Variance', meaning: 'How widely a patient’s error varies around its mean.',
    assumption: 'The same for every patient.', equation: String.raw`\operatorname{Var}(u_{ij})=\sigma_u^2` },
  { concept: 'Covariance', meaning: 'How two patients’ errors vary together around their respective means.',
    assumption: 'Zero for different patients,\nbecause independence is assumed.', equation: String.raw`\operatorname{Cov}(u_{1j},u_{2j})=0` }
];

const covariance = {
  title: 'Covariance: departures from the respective means',
  intro: 'Each point pairs two patients’ errors from one hypothetical repetition.',
  panels: [
    { title: 'ZERO COVARIANCE', blocks: [
      { image: path.join('assets', 'covariance-zero.png'), h: 2.55 },
      { text: 'The departures have no tendency to move together.', fontSize: 20, h: .65 }
    ] },
    { title: 'POSITIVE COVARIANCE', blocks: [
      { image: path.join('assets', 'covariance-positive.png'), h: 2.55 },
      { text: 'The departures tend to move together: above or below their respective means.', fontSize: 20, h: .65 }
    ] }
  ],
  takeaway: 'Both panels have the same means and variances. The dashed lines mark the expected errors.'
};

function addMeanModelSlide(pptx, drawEquation) {
  return addConceptPanelsSlide(pptx, { ...meanModel, drawEquation });
}
function addErrorRecap(pptx, drawEquation) {
  addConceptTableSlide(pptx, { title: 'Mean, variance and covariance of the errors', rows,
    takeaway: 'Independent errors have zero covariance. Zero covariance alone does not establish independence.', drawEquation });
  addConceptPanelsSlide(pptx, { ...covariance, drawEquation });
}
const equations = [...meanModel.panels.flatMap(p => p.blocks.filter(b => b.equation).map(b => b.equation)), ...rows.map(r => r.equation)];
module.exports = { addMeanModelSlide, addErrorRecap, equations };
