const { addConceptPanelsSlide } = require('../../slides-common/js/concept_panels');

// Generic model discussion: separate from the rehabilitation case and its EDA.
// Expectations hold treatment assignments and patient-to-center memberships fixed.
// In the fixed-center formulation c_j is a constant, even when unknown.
// In the random-center formulation expectations average over c_j unless conditioned.
const slides = [
  {
    title: 'Where does an omitted center contribution go?',
    intro: 'Suppose patients in the same center share a contribution that our treatment-only model omits.',
    panels: [
      { title: 'MODEL GENERATING THE OUTCOMES', blocks: [
        { equation: String.raw`Y_{ij}=\beta_0+\beta_1T_{ij}+c_j+\varepsilon_{ij}`, h: .68 },
        { text: 'cⱼ: a shared center contribution that shifts both treatment means.', h: .95 },
        { text: 'εᵢⱼ: the individual error, with expected value zero.', h: .85 }
      ] },
      { title: 'TREATMENT-ONLY MODEL', blocks: [
        { equation: String.raw`Y_{ij}=\beta_0+\beta_1T_{ij}+u_{ij}`, h: .68 },
        { text: 'Keeping the same treatment component, the error becomes:', h: .95 },
        { equation: String.raw`\boxed{u_{ij}=c_j+\varepsilon_{ij}}`, h: .65 }
      ] }
    ],
    takeaway: 'Omitting center does not remove its contribution. It becomes part of the error. Does that error still satisfy our assumptions?'
  },
  {
    title: 'What is the expected error?',
    intro: 'The error is uᵢⱼ = cⱼ + εᵢⱼ in both cases, with E[εᵢⱼ] = 0.',
    panels: [
      { title: 'FIXED CENTER CONTRIBUTION', blocks: [
        { text: 'Hold cⱼ fixed across repeated samples. Only the individual deviations vary.', h: .9 },
        { equation: String.raw`E[u_{ij}]=c_j+E[\varepsilon_{ij}]=c_j`, h: .55 },
        { text: 'The error has a center-specific mean: cⱼ.', h: .7 },
        { text: 'A fixed contribution can be unknown; it is still held constant in this expectation.', fontSize: 19, h: .75 }
      ] },
      { title: 'RANDOM CENTER CONTRIBUTION', blocks: [
        { text: 'Let center contributions vary too,\nwith E[cⱼ] = 0.', h: .9 },
        { equation: String.raw`E[u_{ij}]=E[c_j]+E[\varepsilon_{ij}]=0`, h: .55 },
        { text: 'Averaging over center contributions gives a mean error of zero.', h: .7 },
        { text: 'Given cⱼ, the conditional mean is:\nE[uᵢⱼ | cⱼ] = cⱼ.', fontSize: 19, h: .75 }
      ] }
    ],
    takeaway: 'The expected values differ because we average over different sources of variation.'
  },
  {
    title: 'Departures from the respective means',
    intro: 'Assume independent individual errors εᵢⱼ, also independent of any random center contribution cⱼ.',
    panels: [
      { title: 'FIXED CENTER CONTRIBUTION', blocks: [
        { text: 'Subtract each error’s mean, cⱼ:', h: .5 },
        { equation: String.raw`\begin{aligned}u_{1j}-E[u_{1j}]&=\varepsilon_{1j}\\u_{2j}-E[u_{2j}]&=\varepsilon_{2j}\end{aligned}`, h: 1.0 },
        { text: 'The fixed contribution disappears.\nOnly independent deviations remain.', h: .85 },
        { equation: String.raw`\operatorname{Cov}(u_{1j},u_{2j})=0`, h: .58 }
      ] },
      { title: 'RANDOM CENTER CONTRIBUTION', blocks: [
        { text: 'Subtract each error’s marginal mean, 0:', h: .5 },
        { equation: String.raw`\begin{aligned}u_{1j}-E[u_{1j}]&=c_j+\varepsilon_{1j}\\u_{2j}-E[u_{2j}]&=c_j+\varepsilon_{2j}\end{aligned}`, h: 1.0 },
        { text: 'The shared random contribution remains. Both departures contain cⱼ.', h: .85 },
        { equation: String.raw`\operatorname{Cov}(u_{1j},u_{2j})=\operatorname{Var}(c_j)=\sigma_c^2`, h: .58 }
      ] }
    ],
    takeaway: 'A fixed contribution shifts the means. A random contribution adds shared variation around the means.'
  }
];

function addOmittedCenterSlide(pptx, drawEquation) {
  return addConceptPanelsSlide(pptx, { ...slides[0], drawEquation });
}
function addErrorInterlude(pptx, drawEquation) {
  slides.slice(1).forEach(spec => addConceptPanelsSlide(pptx, { ...spec, drawEquation }));
}

module.exports = { addOmittedCenterSlide, addErrorInterlude, slides };
