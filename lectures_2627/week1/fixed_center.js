const { addConceptPanelsSlide } = require('../../slides-common/js/concept_panels');
const { addFixedCenterExplanation } = require('./fixed_center_sampling');
const { addFigureSlide } = require('../../slides-common/js/pptx_layout_primitives');
const path = require('path');
const betaHat = String.raw`\widehat{\beta}_1`;
const sigmaHat = String.raw`\widehat{\sigma}`;

function calibrationSEEquation(results) {
  const patientsPerTreatment = results.centers * results.patients_per_arm;
  return String.raw`\begin{aligned}\widehat{\mathrm{SE}}(\widehat{\beta}_1)&=\widehat{\sigma}\sqrt{\frac{1}{${patientsPerTreatment}}+\frac{1}{${patientsPerTreatment}}}\\&=${Number(results.fixed.se).toFixed(2)}\ \mathrm{m}\end{aligned}`;
}

function addFixedCenterSection(pptx, drawEquation, results) {
  addFixedCenterExplanation(pptx, drawEquation);
  addFigureSlide(pptx, {
    title: 'Time for lab 1',
    assetPath: path.join('assets', 'lab1-break.png')
  });
  const f = (x, digits = 1) => Number(x).toFixed(digits);
  addConceptPanelsSlide(pptx, {
    title: 'Lab 1 debrief: accounting for center',
    panels: [
      { title: 'THE DESIGN HAS ALREADY DONE\nTHE ADJUSTMENT', titleHeight: .65, blocks: [
        { text: 'Including a covariate in an analysis can remove its contribution to the treatment comparison.', h: .95, gap: .2 },
        { text: 'Here, balanced randomization has already made the center contributions cancel.', h: .95, gap: .2 },
        { text: `Including center in the model therefore leaves the treatment estimate at ${f(results.fixed.estimate)} m, with the same variation across repeated studies.`, h: 1.3 }
      ] },
      { title: 'THE STANDARD ERROR MUST REFLECT THIS', blocks: [
        { text: 'The treatment-only model still counts center differences as residual variation.', h: .7, gap: .07 },
        { text: 'Including center removes those differences from the residual variation used to calculate the treatment effect’s standard error.', h: 1.3, gap: .07 },
        { text: `The estimated individual-error standard deviation is ${f(results.fixed.residual_sd)} m, giving:`, h: .72, gap: .07 },
        { equation: calibrationSEEquation(results), h: 1.08 }
      ] }
    ],
    takeaway: 'The aim is to estimate uncertainty correctly. In this example, that means a smaller standard error, but that is not always the case.',
    drawEquation
  });
}

const equations = [betaHat, sigmaHat, calibrationSEEquation(require('./assets/model_results.json'))];
module.exports = { addFixedCenterSection, equations };
