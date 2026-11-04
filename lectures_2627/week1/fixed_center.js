const { addConceptPanelsSlide } = require('../../slides-common/js/concept_panels');
const { addFixedCenterExplanation } = require('./fixed_center_sampling');
const { addFigureSlide } = require('../../slides-common/js/pptx_layout_primitives');
const path = require('path');
const crypto = require('crypto');
const betaHat = String.raw`\widehat{\beta}_1`;
const sigmaHat = String.raw`\widehat{\sigma}`;
const equationImage = tex => path.join('assets', 'equations', `eq_${crypto.createHash('sha1').update(tex).digest('hex').slice(0, 12)}.png`);

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
      { title: 'SAME TREATMENT ESTIMATE', blocks: [
        { text: `The estimated treatment effect remains ${f(results.fixed.estimate)} m.`, h: .9, gap: .3 },
        { inlineMath: [
          { text: 'With equal numbers in both treatment groups in every center, the two models always give the same treatment estimate (and therefore the same sampling distribution of ' },
          { image: equationImage(betaHat), heightEm: 1.3, baselineFraction: .78 },
          { text: ').' }
        ], h: 2.25 }
      ] },
      { title: 'BETTER-CALIBRATED UNCERTAINTY', blocks: [
        { text: 'After accounting for treatment and center, the residual SD estimates the individual-error SD, σ.', h: 1.1 },
        { inlineMath: [
          { text: 'The fitted model gives ' },
          { image: equationImage(sigmaHat), heightEm: 1.0, baselineFraction: .92 },
          { text: ` = ${f(results.fixed.residual_sd)} m and uses it to estimate the treatment effect’s SE:` }
        ], h: 1.1 },
        { equation: calibrationSEEquation(results), h: 1.25 }
      ] }
    ],
    takeaway: 'The estimated SE now uses the individual variation that determines the treatment estimate’s sampling variability. Better calibration does not always mean a smaller SE.',
    takeawayHeight: 1.1,
    drawEquation
  });
}

const equations = [betaHat, sigmaHat, calibrationSEEquation(require('./assets/model_results.json'))];
module.exports = { addFixedCenterSection, equations };
