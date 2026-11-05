const { addConceptPanelsSlide } = require('../../slides-common/js/concept_panels');
const path = require('path');
const crypto = require('crypto');

// Keep the full patient-level substitution visible before reducing the contrast.
// The epsilon terms belong to the data-generating model; fitted residuals enter
// only on the next slide, where the model estimates its error variance.
const groupDifference = String.raw`\widehat{\beta}_1=\bar Y_{\mathrm{enhanced}}-\bar Y_{\mathrm{standard}}`;
const fullSubstitution = String.raw`\widehat{\beta}_1=\frac{1}{60}\sum_{j=1}^{6}\sum_{i=1}^{10}\left(\beta_0+\beta_1+c_j+\varepsilon_{ij,\mathrm{enhanced}}\right)-\frac{1}{60}\sum_{j=1}^{6}\sum_{i=1}^{10}\left(\beta_0+c_j+\varepsilon_{ij,\mathrm{standard}}\right)`;
const treatmentContrast = String.raw`\widehat{\beta}_1=\beta_1+\bar\varepsilon_{\mathrm{enhanced}}-\bar\varepsilon_{\mathrm{standard}}`;
const trueSE = String.raw`\operatorname{SE}(\widehat{\beta}_1)=\sigma\sqrt{\frac{1}{60}+\frac{1}{60}}`;
const modelErrors = String.raw`u_{ij}\overset{\mathrm{iid}}{\sim}N(0,\sigma_u^2)`;
const modelContrast = String.raw`\widehat{\beta}_1=\beta_1+\bar u_{\mathrm{enhanced}}-\bar u_{\mathrm{standard}}`;
const modelVariance = String.raw`\operatorname{Var}_{\mathrm{model}}(\widehat{\beta}_1)=\frac{\sigma_u^2}{60}+\frac{\sigma_u^2}{60}`;
const reportedSE = String.raw`\boxed{\widehat{\operatorname{SE}}(\widehat{\beta}_1)=\widehat{\sigma}_u\sqrt{\frac{1}{60}+\frac{1}{60}}}`;
const sigmaHat = String.raw`\widehat{\sigma}_u`;
const sigmaHatImage = path.join('assets', 'equations', 'eq_' + crypto.createHash('sha1').update(sigmaHat).digest('hex').slice(0, 12) + '.png');

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
    title: 'The SE estimated by the treatment-only model',
    panels: [
      { title: 'THE MODEL’S CALCULATION', blocks: [
        { text: 'The treatment-only model assumes:', h: .38, gap: .06 },
        { equation: modelErrors, h: .5, gap: .1 },
        { equation: modelContrast, h: .55, gap: .12 },
        { equation: modelVariance, h: .65, gap: .1 },
        { text: 'The estimated SE is therefore:', h: .32, gap: .06 },
        { equation: reportedSE, h: .9 }
      ] },
      { title: 'WHAT THE RESIDUAL SD CAPTURES', blocks: [
        { inlineMath: [
          { text: '• ' },
          { image: sigmaHatImage, heightEm: 1.1, baselineFraction: .8 },
          { text: ' is the residual SD from the treatment-only model.' }
        ], h: .95, gap: .2 },
        { text: '• This model fits one mean for each treatment group. Differences between centers remain in the residuals.', h: 1.3, gap: .2 },
        { text: '• Its residual SD therefore reflects both center differences and individual variation.', h: 1.25 }
      ] }
    ],
    takeaway: 'Center differences cancel from the treatment comparison, but remain in the residual variation used to calculate its SE. The estimated SE is therefore too large.',
    drawEquation
  });
}

module.exports = { addFixedCenterExplanation, equations: [groupDifference, fullSubstitution, treatmentContrast, trueSE, modelErrors, modelContrast, modelVariance, reportedSE, sigmaHat] };
