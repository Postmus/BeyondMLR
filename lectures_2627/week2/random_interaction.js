const path = require('path');
const { addConceptPanelsSlide } = require('../../slides-common/js/concept_panels');
const { addFigureSlide } = require('../../slides-common/js/pptx_layout_primitives');

function correlationEquation(results) {
  const v = results.variance_components;
  return String.raw`\widehat{\rho}_{cd}=\frac{\widehat{\sigma}_{cd}}{\sqrt{\widehat{\sigma}_c^{\,2}\widehat{\sigma}_d^{\,2}}}=\frac{${v.intercept_slope_covariance.toFixed(1)}}{\sqrt{${v.intercept_variance.toFixed(1)}\times ${v.slope_variance.toFixed(1)}}}\approx ${v.correlation.toFixed(2)}`;
}

function addRandomInteractionResults(pptx, drawEquation, results, equations, fixedInteraction) {
  const f = (n, digits = 1) => Number(n).toFixed(digits);
  const p = n => n < .001 ? '< .001' : f(n, 3);
  const treatment = results.fixed_coefficients.find(r => r.term === 'T');
  const v = results.variance_components;
  const varianceSlide = addConceptPanelsSlide(pptx, {
    title: 'CPAP study: estimated variance components',
    intro: 'Results from the random-intercept and random-slope model fitted to the simulated CPAP data.',
    panels: [{ blocks: [
      { table: {
        columns: [
          { label: 'Component', weight: 6.0 },
          { label: 'Parameter', weight: 2.0, align: 'center' },
          { label: 'Estimate (mmHg²)', weight: 2.0, align: 'center' }
        ],
        rows: [
          ['Control means across hospitals', { equation: equations.centerVarianceSymbol, h: .30 }, f(v.intercept_variance)],
          ['Treatment effects across centers', { equation: equations.slopeVarianceSymbol, h: .30 }, f(v.slope_variance)],
          ['Control-mean / treatment-effect covariance', { equation: equations.covarianceSymbol, h: .24 }, f(v.intercept_slope_covariance)],
          ['Individual residual variation', { equation: equations.errorVarianceSymbol, h: .26 }, f(v.residual_variance)]
        ],
        fontSize: 20, headerFontSize: 18, headerHeight: .62,
        cellPaddingY: .03, firstColumnBold: false, drawEquation
      }, h: 2.27, gap: .04 },
      { text: 'Estimated control-mean / treatment-effect correlation', h: .30, fontSize: 18, bold: true, gap: .01 },
      { equation: correlationEquation(results), h: .88, gap: .01 },
      { text: 'Hospitals with greater control-group reductions tend to have larger treatment effects.', h: .40, fontSize: 20 }
    ] }],
    takeaway: 'Treatment-effect variance is comparable to control-mean variance. Both are smaller than individual residual variance.',
    takeawayHeight: .82, drawEquation
  });
  varianceSlide.addNotes('');

  const averages = addConceptPanelsSlide(pptx, {
    title: 'The average treatment effect across the population of centers',
    intro: 'The same 240 patients in six hospitals, fitted with random intercepts and treatment slopes.',
    panels: [{ blocks: [
      { table: {
        columns: [
          { label: 'Population parameter', weight: 3.5 },
          { label: 'Estimate', weight: 1.35, align: 'center' },
          { label: 'Estimated SE', weight: 1.7, align: 'center' },
          { label: '95% CI', weight: 2.5, align: 'center' },
          { label: 'p-value', weight: 1.2, align: 'center' }
        ],
        rows: results.fixed_coefficients.map(r => [
          r.term === 'T' ? 'Average treatment effect (β₁)' : 'Average control-group reduction (β₀)',
          f(r.estimate, 2), f(r.standard_error, 2), `${f(r.lower_95)} to ${f(r.upper_95)}`, p(r.p)
        ]), fontSize: 22, headerFontSize: 18, headerHeight: .65, firstColumnBold: false
      }, h: 2.25, gap: .20 },
      { text: 'Estimates, estimated SEs and confidence intervals are in mmHg.', h: .45, fontSize: 20, gap: .15 },
      { text: 'The treatment coefficient estimates the average CPAP-minus-control effect across the population of centers.', h: .85, fontSize: 22 }
    ] }],
    takeaway: `Estimated average benefit: ${f(treatment.estimate)} mmHg. The 95% confidence interval includes zero.`,
    drawEquation
  });
  averages.addNotes('');

  const comparison = addConceptPanelsSlide(pptx, {
    title: 'Fixed and random effects: estimates and uncertainty',
    intro: 'Hospital sizes differ, with equal treatment allocation within every hospital.',
    panels: [{ blocks: [
      { table: {
        columns: [
          { label: 'Model', weight: 4.0 },
          { label: 'Estimated treatment\neffect (mmHg)', weight: 2.6, align: 'center' },
          { label: 'Estimated\nSE (mmHg)', weight: 1.7, align: 'center' },
          { label: 'p-value', weight: 1.5, align: 'center' }
        ],
        rows: [
          ['Fixed interaction', f(fixedInteraction.estimate, 2), f(fixedInteraction.standard_error, 2), p(fixedInteraction.p)],
          ['Random intercept and slope', f(treatment.estimate, 2), f(treatment.standard_error, 2), p(treatment.p)]
        ], fontSize: 22, headerFontSize: 18, headerHeight: .70, firstColumnBold: false
      }, h: 2.0, gap: .12 },
      { text: 'Fixed interaction: equally weighted average for these six hospitals. Uncertainty comes from sampling patients.',
        h: .70, fontSize: 21, gap: .10 },
      { text: 'Random intercept and slope: population-average effect. Uncertainty also reflects which hospitals entered the study.',
        h: .70, fontSize: 21 }
    ] }],
    takeaway: 'Unequal hospital sizes allow different estimates. Between-hospital treatment variation adds uncertainty about the population average.',
    takeawayHeight: 1.0, drawEquation
  });
  comparison.addNotes('');

  const interceptOnly = results.random_intercept_model.fixed_coefficients.find(r => r.term === 'T');
  const omittedSlope = addConceptPanelsSlide(pptx, {
    title: 'What if we omit the random treatment slope?',
    intro: 'Back to the CPAP study: both models target the population-average treatment effect.',
    panels: [{ blocks: [
      { table: {
        columns: [
          { label: 'Model', weight: 4.0 },
          { label: 'Estimated treatment\neffect (mmHg)', weight: 2.6, align: 'center' },
          { label: 'Estimated\nSE (mmHg)', weight: 1.7, align: 'center' },
          { label: 'p-value', weight: 1.5, align: 'center' }
        ],
        rows: [
          ['Random intercept only', f(interceptOnly.estimate, 2), f(interceptOnly.standard_error, 2), p(interceptOnly.p)],
          ['Random intercept and slope', f(treatment.estimate, 2), f(treatment.standard_error, 2), p(treatment.p)]
        ], fontSize: 22, headerFontSize: 18, headerHeight: .70, firstColumnBold: false
      }, h: 2.0, gap: .12 },
      { text: 'Random intercept only: assumes the same treatment effect in every center.',
        h: .70, fontSize: 21, gap: .10 },
      { text: 'Random intercept and slope: allows treatment effects to vary across centers.',
        h: .70, fontSize: 21 }
    ] }],
    takeaway: 'When treatment effects vary across centers, a random-intercept-only model can underestimate uncertainty about the population-average effect.',
    takeawayHeight: 1.0, drawEquation
  });
  omittedSlope.addNotes('');

  const lab4 = addFigureSlide(pptx, {
    title: 'Time for Lab 4',
    assetPath: path.join(__dirname, '../week1/assets/lab2-break.png')
  });
  lab4.addNotes('');

  const decomposition = addConceptPanelsSlide(pptx, {
    title: 'Variance decomposition depends on the random-effects structure',
    panels: [
      { title: 'WEEK 1: RANDOM INTERCEPT', blocks: [
        { text: 'The same center contribution applies under both treatments.', h: .95, fontSize: 22, gap: .20 },
        { text: 'Outcome variance separates into a center component and an individual component.', h: 1.15, fontSize: 22, gap: .20 },
        { text: 'This gives one common within-center correlation: the ICC.', h: 1.15, fontSize: 22 }
      ] },
      { title: 'WEEK 2: RANDOM INTERCEPT AND SLOPE', blocks: [
        { text: 'The center contribution depends on treatment.', h: .95, fontSize: 22, gap: .20 },
        { text: 'Outcome variance depends on treatment and combines the relevant variances and covariance.', h: 1.15, fontSize: 22, gap: .20 },
        { text: 'Within-center correlation depends on the treatments received by the two patients.', h: 1.15, fontSize: 22 }
      ] }
    ],
    takeaway: 'Variance decomposition still applies, but there is no longer one common variance breakdown or a single ICC for all patients.',
    takeawayHeight: 1.0, drawEquation
  });
  decomposition.addNotes('');

}

module.exports = { addRandomInteractionResults, correlationEquation };
