const { addConceptPanelsSlide } = require('../../slides-common/js/concept_panels');

// Keep the full patient-level substitution visible before reducing the contrast.
// The epsilon terms belong to the data-generating model; fitted residuals enter
// only on the next slide, where the model estimates its error variance.
const groupDifference = String.raw`\widehat{\beta}_1=\bar Y_{\mathrm{enhanced}}-\bar Y_{\mathrm{standard}}`;
const fullSubstitution = String.raw`\widehat{\beta}_1=\frac{1}{60}\sum_{j=1}^{6}\sum_{i=1}^{10}\left(\beta_0+\beta_1+c_j+\varepsilon_{ij,\mathrm{enhanced}}\right)-\frac{1}{60}\sum_{j=1}^{6}\sum_{i=1}^{10}\left(\beta_0+c_j+\varepsilon_{ij,\mathrm{standard}}\right)`;
const treatmentContrast = String.raw`\widehat{\beta}_1=\beta_1+\bar\varepsilon_{\mathrm{enhanced}}-\bar\varepsilon_{\mathrm{standard}}`;
const trueSE = String.raw`\operatorname{SE}(\widehat{\beta}_1)=\sigma\sqrt{\frac{1}{60}+\frac{1}{60}}`;
const residual = String.raw`\widehat u_{ij}=Y_{ij}-\widehat Y_{ij}`;
const residualVariance = String.raw`\widehat{\sigma}_u^2=\frac{\displaystyle\sum_{j=1}^{6}\sum_{i=1}^{20}\widehat u_{ij}^{\,2}}{120-2}`;
const reportedSE = String.raw`\widehat{\operatorname{SE}}(\widehat{\beta}_1)=\widehat{\sigma}_u\sqrt{\frac{1}{60}+\frac{1}{60}}`;

function addFixedCenterExplanation(pptx, drawEquation) {
  addConceptPanelsSlide(pptx, {
    title: 'What determines the treatment estimate’s sampling variability?',
    panels: [
      { title: 'SUBSTITUTE THE DATA-GENERATING MODEL', blocks: [
        { equation: groupDifference, h: .45, gap: .08 },
        { equation: fullSubstitution, h: 1.1, gap: .25 },
        { text: '• Both groups contain 60 copies of the same baseline, so these terms cancel.', h: .4, gap: .08 },
        { text: '• Both groups contain 10 copies of every center effect, so these terms cancel.', h: .4, gap: .14 },
        { equation: treatmentContrast, h: .5, gap: .08 },
        { text: 'Each epsilon bar averages the individual errors used to generate that group’s outcomes.', fontSize: 19, h: .4, gap: .1 },
        { equation: trueSE, h: .95 }
      ] }
    ],
    drawEquation
  });
  addConceptPanelsSlide(pptx, {
    title: 'How does the treatment-only model estimate the SE?',
    panels: [
      { title: 'FROM FITTED MEANS TO RESIDUALS', blocks: [
        { text: 'The model fits one mean per treatment group.', h: .8 },
        { text: 'Each residual is a patient’s observed outcome minus their fitted group mean:', h: 1.1 },
        { equation: residual, h: .6 },
        { text: 'The fitted group means do not account for center, so the residuals still contain center differences.', h: 1.1 }
      ] },
      { title: 'FROM RESIDUALS TO THE REPORTED SE', blocks: [
        { text: 'Estimate the error variance:', h: .55 },
        { equation: residualVariance, h: 1.68 },
        { text: 'Use its square root in the SE formula:', h: .55 },
        { equation: reportedSE, h: .85 }
      ] }
    ],
    takeaway: 'Center differences contribute to the estimated error variance, although they cancel from the treatment contrast.',
    drawEquation
  });
}

module.exports = { addFixedCenterExplanation, equations: [groupDifference, fullSubstitution, treatmentContrast, trueSE, residual, residualVariance, reportedSE] };
