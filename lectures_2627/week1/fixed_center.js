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

function residualSDEquation(results) {
  return String.raw`\widehat{\sigma}=${Number(results.fixed.residual_sd).toFixed(1)}\ \mathrm{m}`;
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
      { title: 'WHAT CHANGES IN THE MODEL', blocks: [
        { text: '• Including center allows the fitted outcomes to reflect systematic differences between centers.', h: 1.1, gap: .15 },
        { text: '• Those differences no longer remain in the residuals.', h: .75, gap: .15 },
        { text: '• The residual SD now estimates the individual-error SD:', h: .8, gap: .15 },
        { equation: residualSDEquation(results), h: .6 }
      ] },
      { title: 'WHAT CHANGES IN THE SE', blocks: [
        { text: `• The treatment estimate remains ${f(results.fixed.estimate)} m, with the same sampling distribution: balanced allocation already makes center contributions cancel.`, h: 1.65, gap: .22 },
        { text: '• The SE now uses the estimated individual-error SD:', h: .8, gap: .16 },
        { equation: calibrationSEEquation(results), h: 1.08 }
      ] }
    ],
    takeaway: 'Including center makes the SE better calibrated: the residual variance now estimates the variation that actually contributes to the treatment comparison in this randomized block design.',
    drawEquation
  });
}

const results = require('./assets/model_results.json');
const equations = [betaHat, sigmaHat, residualSDEquation(results), calibrationSEEquation(results)];
module.exports = { addFixedCenterSection, equations };
