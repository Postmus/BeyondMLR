const { addConceptPanelsSlide } = require('../../slides-common/js/concept_panels');
const { addPooledErrorFigure } = require('./fixed_center_error_figures');

// First explain unbiasedness through balance, then compare the source of
// variability in the true SE with the residual SD used to estimate it.
const groupDifference = String.raw`\widehat{\beta}_1=\bar Y_{\mathrm{enhanced}}-\bar Y_{\mathrm{standard}}`;
const treatmentContrast = String.raw`\widehat{\beta}_1=\beta_1+\bar\varepsilon_{\mathrm{enhanced}}-\bar\varepsilon_{\mathrm{standard}}`;
const unbiasedEstimate = String.raw`E(\widehat{\beta}_1)=\beta_1`;
const trueSE = String.raw`\operatorname{SE}(\widehat{\beta}_1)=\sigma\sqrt{\frac{1}{60}+\frac{1}{60}}`;
const reportedSE = String.raw`\widehat{\operatorname{SE}}(\widehat{\beta}_1)=\widehat{\sigma}_u\sqrt{\frac{1}{60}+\frac{1}{60}}`;

function addFixedCenterExplanation(pptx, drawEquation) {
  addPooledErrorFigure(pptx);
  addConceptPanelsSlide(pptx, {
    title: 'Balanced allocation keeps the treatment estimate unbiased',
    panels: [
      { title: 'BALANCE MAKES CENTER DIFFERENCES CANCEL', blocks: [
        { equation: groupDifference, h: .50, gap: .18 },
        { text: 'Both groups contain 10 patients from every center. The shared baseline and average center contribution cancel.', fontSize: 21, h: .70, gap: .18 },
        { equation: treatmentContrast, h: .55, gap: .18 },
        { text: 'Each epsilon bar averages the individual errors in that treatment group.', fontSize: 21, h: .40, gap: .15 },
        { text: 'These individual errors have mean zero, so:', fontSize: 21, h: .40 },
        { equation: unbiasedEstimate, h: .55 }
      ] }
    ],
    takeaway: 'Balance between the treatment groups makes center differences cancel from the treatment comparison.',
    drawEquation
  });
  addConceptPanelsSlide(pptx, {
    title: 'Omitting center inflates the estimated standard error',
    panels: [
      { title: 'WHAT DETERMINES THE TRUE SE?', titleHeight: .65, blocks: [
        { text: 'Center differences cancel from the treatment contrast.', fontSize: 20, h: .95 },
        { text: 'Only individual errors contribute to its sampling variability.', fontSize: 20, h: .75 },
        { equation: trueSE, h: .80 },
        { text: 'σ is the individual-error SD: variation around the center-specific means.', fontSize: 20, h: .75 }
      ] },
      { title: 'THE POOLED SPREAD ENTERS THE ESTIMATED SE', titleHeight: .65, blocks: [
        { text: 'Pooled errors include variation within centers and differences between center means.', fontSize: 20, h: .95 },
        { text: 'One fitted mean per treatment group leaves center differences in the residuals.', fontSize: 20, h: .75 },
        { equation: reportedSE, h: .80 },
        { text: 'Residual SD reflects the pooled spread rather than the individual-error SD σ.', fontSize: 20, h: .75 }
      ] }
    ],
    takeaway: 'Center differences do not contribute to the treatment contrast’s variability,\nbut they inflate the residual SD used to estimate its SE.',
    drawEquation
  });
  addConceptPanelsSlide(pptx, {
    title: 'Overestimated standard errors overstate uncertainty',
    panels: [
      { title: 'CONFIDENCE INTERVALS', blocks: [
        { text: 'A confidence interval uses the estimated SE to express uncertainty around the treatment estimate.', h: 1.10, gap: .20 },
        { text: 'An inflated SE makes the confidence interval unnecessarily wide.', h: 1.10, gap: .20 },
        { text: 'The reported uncertainty is greater than the actual sampling uncertainty.', h: .95 }
      ] },
      { title: 'HYPOTHESIS TESTS', blocks: [
        { text: 'A test compares the treatment estimate with its estimated SE.', h: 1.10, gap: .20 },
        { text: 'An inflated SE makes the absolute test statistic smaller, generally giving a larger p-value.', h: 1.10, gap: .20 },
        { text: 'We have less power to detect a real treatment effect.', h: .95 }
      ] }
    ],
    takeaway: 'Accounting for center can give narrower, appropriately calibrated confidence intervals and greater power, without changing the treatment estimate in this balanced design.',
    takeawayHeight: 1.05,
    drawEquation
  });
}

module.exports = { addFixedCenterExplanation, equations: [groupDifference, treatmentContrast, unbiasedEstimate, trueSE, reportedSE] };
