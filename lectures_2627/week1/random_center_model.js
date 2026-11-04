const path = require('path');
const { addBulletVisualSlide } = require('../../slides-common/js/pptx_layout_primitives');
const { addConceptPanelsSlide } = require('../../slides-common/js/concept_panels');

const model = String.raw`Y_{ij}=\beta_0+\beta_1T_{ij}+c_j+\varepsilon_{ij}`;
const distributions = String.raw`\begin{aligned}c_j&\overset{\mathrm{iid}}{\sim}N(0,\sigma_c^2)\\\varepsilon_{ij}&\overset{\mathrm{iid}}{\sim}N(0,\sigma^2)\end{aligned}`;
const marginalModel = String.raw`Y_{ij}=\beta_0+\beta_1T_{ij}+u_{ij}`;
const combinedError = String.raw`u_{ij}=c_j+\varepsilon_{ij}`;
const icc = String.raw`\mathrm{ICC}=\frac{\sigma_c^2}{\sigma_c^2+\sigma^2}`;
const centerVariance = String.raw`\sigma_c^2`;
const individualVariance = String.raw`\sigma^2`;
const totalVariance = String.raw`\operatorname{Var}(u_{ij})=\sigma_c^2+\sigma^2`;

function addRandomCenterModel(pptx, drawEquation) {
  addBulletVisualSlide(pptx, {
    title: 'From individual centers to a population of centers',
    bullets: [
      'Besides estimating the treatment effect, the fixed-effects model estimates an effect for each of the six included centers.',
      'Often, our scientific interest is not in these particular centers or their individual effects.',
      'A random-intercept model accounts for center differences by describing how center effects vary across a population of centers.',
      'This gives a more parsimonious model and an estimate of the between-center variance.'
    ],
    textFraction: .57,
    bulletFontSize: 22, bulletMinFontSize: 22,
    assetPath: path.join('assets', 'population-of-centers.png'),
    framed: false
  });

  addConceptPanelsSlide(pptx, {
    title: 'The random-intercept model',
    panels: [
      { title: 'RANDOM-INTERCEPT MODEL', blocks: [
        { equation: model, h: .55, gap: .06 },
        { equation: distributions, h: 1.2, gap: .06 },
        { text: [
          { text: 'For a given center effect, the expected walking distance is β₀ + β₁Tᵢⱼ + cⱼ. This is the ' },
          { text: 'conditional mean', options: { bold: true } },
          { text: '.' }
        ], h: 1.2, gap: .08 },
        { text: 'Individual errors are independent of each other and of the center effects.', h: .85 }
      ] },
      { title: 'IMPLIED MARGINAL MODEL', blocks: [
        { equation: marginalModel, h: .55 },
        { equation: combinedError, h: .55, gap: .2 },
        { text: [
          { text: 'Averaging over center effects gives the expected walking distance β₀ + β₁Tᵢⱼ, because center effects have mean zero. This is the ' },
          { text: 'marginal mean', options: { bold: true } },
          { text: '.' }
        ], h: 1.55, gap: .15 },
        { text: 'Patients in the same center share cⱼ, so their combined errors uᵢⱼ are correlated.', h: .85 }
      ] }
    ],
    takeaway: 'The implied marginal model has the same mean structure as the treatment-only model, but its errors are correlated within centers.',
    drawEquation
  });

  addConceptPanelsSlide(pptx, {
    title: 'Between-center variation and the ICC',
    panels: [
      { title: 'VARIANCE COMPONENTS', blocks: [
        { equation: centerVariance, h: .45 },
        { text: 'Variation between center intercepts.', h: .65, gap: .25 },
        { equation: individualVariance, h: .45 },
        { text: 'Individual variation within centers.', h: .65, gap: .25 },
        { text: 'After accounting for treatment, the total variance is:', h: .85 },
        { equation: totalVariance, h: .6 }
      ] },
      { title: 'INTRACLASS CORRELATION', blocks: [
        { equation: icc, h: .9, gap: .4 },
        { text: 'The proportion of remaining outcome variance attributable to differences between centers.', h: 1.2, gap: .3 },
        { text: 'The correlation between outcomes of two patients in the same center, after accounting for treatment.', h: 1.45 }
      ] }
    ],
    drawEquation
  });
}

module.exports = { addRandomCenterModel, equations: [model, distributions, marginalModel, combinedError, icc, centerVariance, individualVariance, totalVariance] };
