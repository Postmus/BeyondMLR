const { addConceptPanelsSlide } = require('../../slides-common/js/concept_panels');

// Generic model discussion: separate from the rehabilitation case and its EDA.
// Expectations hold treatment assignments and patient-to-center memberships fixed.
// In the fixed-center formulation c_j is a constant, even when unknown.
// In the random-center formulation expectations average over c_j unless conditioned.
const slides = [
  {
    title: 'What happens if we leave center out?',
    intro: 'Suppose expected outcomes differ between centers, even under the same treatment.',
    panels: [
      { title: 'ACCOUNTING FOR TREATMENT AND CENTER', blocks: [
        { equation: String.raw`Y_{ij}=\beta_0+\beta_1T_{ij}+c_j+\varepsilon_{ij}`, h: .55 },
        { equation: String.raw`\varepsilon_{ij}\overset{\mathrm{iid}}{\sim}N(0,\sigma^2)`, h: .5 },
        { text: 'cⱼ: shifts the predicted outcome up or down for both treatments in center j.', fontSize: 20, h: .85 },
        { text: 'εᵢⱼ: the remaining error; independent across patients, Normally distributed with mean zero and common variance.', fontSize: 20, h: .95 }
      ] },
      { title: 'OUR TREATMENT-ONLY MODEL', blocks: [
        { equation: String.raw`Y_{ij}=\beta_0+\beta_1T_{ij}+u_{ij}`, h: .5 },
        { text: 'Our prediction uses treatment alone. The difference associated with center therefore remains in the error:', fontSize: 20, h: .9 },
        { equation: String.raw`\boxed{u_{ij}=c_j+\varepsilon_{ij}}`, h: .5 },
        { text: 'Does the original assumption still hold?', fontSize: 20, h: .4 },
        { equation: String.raw`u_{ij}\overset{\mathrm{iid}}{\sim}N(0,\sigma_u^2)\quad\text{?}`, h: .5 }
      ] }
    ],
    takeaway: 'If the model on the left is correct, are the errors in our treatment-only model still independent and identically Normally distributed, with mean zero?'
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
function addFixedRandomPerspectiveSlide(pptx) {
  return addConceptPanelsSlide(pptx, {
    title: 'Center as a fixed or random effect',
    intro: 'What do we imagine changing when we repeat the study?',
    panels: [
      { title: 'CENTER AS A FIXED EFFECT', blocks: [
        { text: 'Imagine recruiting new patients in the same centers.', h: 1.0 },
        { text: 'Each center’s underlying effect cⱼ stays the same across repetitions.', h: 1.0 },
        { text: 'We describe the center differences using separate parameters.', h: 1.0 }
      ] },
      { title: 'CENTER AS A RANDOM EFFECT', blocks: [
        { text: 'Imagine sampling centers from a population of centers, as well as recruiting new patients.', h: 1.0 },
        { text: 'Center effects vary across repetitions according to a distribution.', h: 1.0 },
        { text: 'We describe the center differences using a distribution with an estimated variance.', h: 1.0 }
      ] }
    ],
    takeaway: 'First, we hold the centers fixed and examine what happens when we repeatedly sample new patients.'
  });
}
function addErrorInterlude(pptx, drawEquation) {
  slides.slice(1).forEach(spec => addConceptPanelsSlide(pptx, { ...spec, drawEquation }));
}

module.exports = { addOmittedCenterSlide, addFixedRandomPerspectiveSlide, addErrorInterlude, slides };
