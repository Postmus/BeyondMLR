const { addConceptPanelsSlide } = require('../../slides-common/js/concept_panels');
const { treatmentOnlyAssumption } = require('./initial_model');
const { addTeachingTableSlide } = require('../../slides-common/js/teaching_table');

const fullModelEquation = String.raw`Y_{ij}=\beta_0+\beta_1T_{ij}+c_j+\varepsilon_{ij}`;

function addTreatmentCenterModelSlide(pptx, drawEquation) {
  return addTeachingTableSlide(pptx, {
    title: 'The data-generating model: fixed center effects',
    intro: 'Patient i in center j: Yᵢⱼ is walking distance. Tᵢⱼ = 0 (standard) or 1 (enhanced).',
    leadEquation: fullModelEquation,
    drawEquation,
    columns: [
      { label: 'TERM', weight: 1.5 },
      { label: 'INTERPRETATION', weight: 10.5 }
    ],
    rows: [
      ['β₀', 'Expected standard-care outcome averaged equally across the six centers.'],
      ['β₁', 'Enhanced-minus-standard difference, assumed to be the same in every center.'],
      ['cⱼ', 'Center j’s deviation from the average standard-care outcome.\nThe center deviations sum to zero: c₁ + ⋯ + c₆ = 0.'],
      ['εᵢⱼ', 'Deviation from the expected outcome given treatment and center.\nIndependent Normal errors with mean 0 and common variance σ².']
    ],
    fontSize: 21
  });
}

// Generic model discussion: separate from the rehabilitation case and its EDA.
// Expectations hold treatment assignments and patient-to-center memberships fixed.
// In the fixed-center formulation c_j is a constant, even when unknown.
// In the random-center formulation expectations average over c_j unless conditioned.
const slides = [
  {
    title: 'Omitting center: a mathematical explanation',
    panels: [
      { title: 'THE DATA-GENERATING MODEL', blocks: [
        { equation: fullModelEquation, h: .6, gap: .3 },
        { equation: String.raw`\varepsilon_{ij}\overset{\mathrm{iid}}{\sim}N(0,\sigma^2)`, h: .6, gap: .3 },
        { text: 'cⱼ is center j’s fixed shift from the average expected outcome. The center shifts sum to zero.', fontSize: 21, h: 1.10, gap: .20 },
        { text: 'After accounting for treatment and center, individual errors are independent Normal variables with mean 0 and variance σ².', fontSize: 21, h: 1.25 }
      ] },
      { title: 'THE TRUE ERRORS WHEN CENTER IS OMITTED', titleHeight: .65, blocks: [
        { text: 'We can rewrite the same generated outcomes as:', fontSize: 20, h: .65 },
        { equation: String.raw`Y_{ij}=\beta_0+\beta_1T_{ij}+u_{ij}`, h: .55, gap: .20 },
        { equation: String.raw`\boxed{u_{ij}=c_j+\varepsilon_{ij}}`, h: .55, gap: .20 },
        { equation: String.raw`u_{ij}\sim N(c_j,\sigma^2)`, h: .55, gap: .20 },
        { text: 'These errors remain independent, with equal variances, but their means differ between centers.', fontSize: 21, h: 1.15 }
      ] }
    ]
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

const errorComparison = {
  title: 'True errors and the treatment-only model',
  panels: [
    { title: 'THE TRUE ERROR DISTRIBUTION', blocks: [
      { equation: String.raw`\boxed{u_{ij}=c_j+\varepsilon_{ij}}`, h: .55, gap: .15 },
      { equation: String.raw`u_{ij}\sim N(c_j,\sigma^2)`, h: .55, gap: .15 },
      { text: 'Independent errors.', h: .40, gap: .20 },
      { text: 'Means differ between centers: cⱼ.', h: .65, gap: .15 },
      { text: 'Equal variances: σ².', h: .90 }
    ] },
    { title: 'WHAT THE FITTED MODEL ASSUMES', blocks: [
      { equation: treatmentOnlyAssumption, h: .55, gap: .15 },
      { text: 'A common zero-mean Normal distribution.', fontSize: 20, h: .55, gap: .15 },
      { text: 'Independent errors.', h: .40, gap: .20 },
      { text: 'Mean zero for every patient.', h: .65, gap: .15 },
      { text: 'One common variance, σᵤ², estimated from the data.', h: .90 }
    ] }
  ],
  takeaway: 'The treatment-only model correctly assumes independence and equal variances,\nbut misses the center-specific error means.'
};

function addErrorAssumptionComparison(pptx, drawEquation) {
  return addConceptPanelsSlide(pptx, { ...errorComparison, drawEquation });
}

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
    takeaway: 'We now sample centers as well as patients, while retaining treatment randomization within each center.'
  });
}
function addErrorInterlude(pptx, drawEquation) {
  slides.slice(1).forEach(spec => addConceptPanelsSlide(pptx, { ...spec, drawEquation }));
}

module.exports = { addTreatmentCenterModelSlide, addOmittedCenterSlide, addFixedRandomPerspectiveSlide, addErrorInterlude, addErrorAssumptionComparison, errorComparison, slides };
