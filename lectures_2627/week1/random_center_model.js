const path = require('path');
const { addBulletVisualSlide } = require('../../slides-common/js/pptx_layout_primitives');
const { addConceptPanelsSlide } = require('../../slides-common/js/concept_panels');
const { addRandomCenterErrorFigures } = require('./random_center_error_figures');

const model = String.raw`Y_{ij}=\beta_0+\beta_1T_{ij}+c_j+\varepsilon_{ij}`;
const distributions = String.raw`\begin{aligned}c_j&\overset{\mathrm{iid}}{\sim}N(0,\sigma_c^2)\\\varepsilon_{ij}&\overset{\mathrm{iid}}{\sim}N(0,\sigma^2)\end{aligned}`;
const outcomeMean = String.raw`\begin{aligned}E(Y_{ij})&=\beta_0+\beta_1T_{ij}+E(c_j)+E(\varepsilon_{ij})\\&=\beta_0+\beta_1T_{ij}\end{aligned}`;
const outcomeVariance = String.raw`\begin{aligned}\operatorname{Var}(Y_{ij})&=\operatorname{Var}(c_j)+\operatorname{Var}(\varepsilon_{ij})\\&=\sigma_c^2+\sigma^2\end{aligned}`;
const outcomeDistribution = String.raw`Y_{ij}\sim N\!\left(\beta_0+\beta_1T_{ij},\,\sigma_c^2+\sigma^2\right)`;
const icc = String.raw`\operatorname{Corr}(Y_{ij},Y_{kj})=\frac{\sigma_c^2}{\sigma_c^2+\sigma^2}=\mathrm{ICC}`;
const withinCenterCovariance = String.raw`\begin{aligned}\operatorname{Cov}(Y_{ij},Y_{kj})&=\operatorname{Var}(c_j)\\&=\sigma_c^2\end{aligned}`;
const totalVariance = String.raw`\operatorname{Var}(Y_{ij})=\sigma_c^2+\sigma^2`;

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
        { text: 'For a given center effect, the expected walking distance is β₀ + β₁Tᵢⱼ + cⱼ.', h: 1.05, gap: .08 },
        { text: 'Individual errors are independent of each other and of the center effects.', h: .85 }
      ] },
      { title: 'WHAT DOES THIS IMPLY FOR Yᵢⱼ?', blocks: [
        { text: 'Recap rules: slides 8–9', fontSize: 17, muted: true, h: .26, gap: .07 },
        { text: 'EXPECTED VALUE: ADD EXPECTATIONS', fontSize: 15, bold: true, h: .24, gap: .04 },
        { equation: outcomeMean, h: .90, gap: .10 },
        { text: 'VARIANCE: ADD INDEPENDENT VARIANCES', fontSize: 15, bold: true, h: .24, gap: .04 },
        { equation: outcomeVariance, h: .90, gap: .10 },
        { text: 'Independent Normal terms give a Normal sum:', fontSize: 17, h: .50, gap: .06 },
        { equation: outcomeDistribution, h: .55 }
      ] }
    ],
    takeaway: 'Patients in the same center share the random effect cⱼ, so their outcomes are correlated.',
    drawEquation
  });

  addRandomCenterErrorFigures(pptx);

  addConceptPanelsSlide(pptx, {
    title: 'Between-center variation and the ICC',
    intro: 'After accounting for treatment',
    panels: [
      { title: 'VARIANCE AND COVARIANCE', blocks: [
        { text: 'One patient:', h: .38, gap: .08 },
        { equation: totalVariance, h: .60, gap: .22 },
        { text: 'Two different patients in the same center (i ≠ k):', h: .70, gap: .08 },
        { equation: withinCenterCovariance, h: .90, gap: .18 },
        { text: 'Their individual errors are independent; their shared center effect creates the covariance.', h: 1.0 }
      ] },
      { title: 'FROM COVARIANCE TO CORRELATION', blocks: [
        { text: 'Divide their covariance by their common variance:', h: .65 },
        { equation: icc, h: .95, gap: .25 },
        { text: 'The correlation between outcomes of two patients in the same center.', h: 1.0, gap: .20 },
        { text: 'The proportion of remaining outcome variance attributable to differences between centers.', h: 1.05 }
      ] }
    ],
    drawEquation
  });
}

module.exports = { addRandomCenterModel, equations: [model, distributions, outcomeMean, outcomeVariance, outcomeDistribution, icc, withinCenterCovariance, totalVariance] };
