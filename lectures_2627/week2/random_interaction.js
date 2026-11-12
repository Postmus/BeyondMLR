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
    title: 'Rehabilitation study: estimated variance components',
    intro: 'Results from the random-intercept and random-slope model fitted to the rehabilitation data.',
    panels: [{ blocks: [
      { table: {
        columns: [
          { label: 'Component', weight: 6.0 },
          { label: 'Parameter', weight: 2.0, align: 'center' },
          { label: 'Estimate (m²)', weight: 2.0, align: 'center' }
        ],
        rows: [
          ['Standard-care means across centers', { equation: equations.centerVarianceSymbol, h: .30 }, f(v.intercept_variance)],
          ['Treatment effects across centers', { equation: equations.slopeVarianceSymbol, h: .30 }, f(v.slope_variance)],
          ['Baseline / treatment-effect covariance', { equation: equations.covarianceSymbol, h: .24 }, f(v.intercept_slope_covariance)],
          ['Individual residual variation', { equation: equations.errorVarianceSymbol, h: .26 }, f(v.residual_variance)]
        ],
        fontSize: 20, headerFontSize: 18, headerHeight: .50,
        cellPaddingY: .03, firstColumnBold: false, drawEquation
      }, h: 2.15, gap: .08 },
      { text: 'Estimated baseline / treatment-effect correlation', h: .30, fontSize: 18, bold: true, gap: .04 },
      { equation: correlationEquation(results), h: .88, gap: .06 },
      { text: 'Centers with higher standard-care means tend to have larger treatment effects.', h: .40, fontSize: 20 }
    ] }],
    takeaway: 'Treatment-effect variation across centers is much larger than baseline variation and almost as large as individual residual variation.',
    takeawayHeight: .82, drawEquation
  });
  varianceSlide.addNotes('These are estimates from the random-intercept and random-slope model fitted to the rehabilitation case-study data, not hypothetical generating values. All four components are estimated jointly by REML. The table uses the same sigma symbols as the model slide. Omit the SD column to keep the focus on variance and covariance. The correlation formula below the table divides the estimated intercept-slope covariance by the square root of the product of the estimated intercept and slope variances. Full-precision inputs give ' + f(v.correlation, 5) + ', rounded to 0.54; the displayed substitution uses the rounded table entries 127.1, 86.4 and 651.6. This is a dimensionless correlation between center baseline and treatment-effect deviations, not an ICC or a correlation between two patient outcomes. The positive correlation describes the fitted population relationship: centers with higher standard-care means tend to have larger enhanced-minus-standard effects. It does not imply that baseline outcomes causally determine treatment benefit. With treatment coded 0/1, all three variances have units m squared and can be compared directly: treatment-effect variance 651.6 is much larger than baseline variance 86.4 and almost equal to individual residual variance 658.4. These are descriptive magnitude comparisons, not percentages of a common total or tests of equality. The treatment-effect variance describes between-center heterogeneity, whereas the SE on slide 27 describes uncertainty about the population-average effect. Variance and covariance estimates are based on six centers and are themselves uncertain; no confidence intervals or tests for these components are presented. The residual variance describes individual variation around the fitted center-specific treatment means. Use the large treatment-effect variation to motivate the following population-average results and fixed-versus-random comparison.');

  const averages = addConceptPanelsSlide(pptx, {
    title: 'The average treatment effect across the population of centers',
    intro: 'The same 120 patients in six centers, fitted with random intercepts and treatment slopes.',
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
          r.term === 'T' ? 'Average treatment effect (β₁)' : 'Average standard-care outcome (β₀)',
          f(r.estimate), f(r.standard_error), `${f(r.lower_95)} to ${f(r.upper_95)}`, p(r.p)
        ]), fontSize: 22, headerFontSize: 18, headerHeight: .65, firstColumnBold: false
      }, h: 2.25, gap: .20 },
      { text: 'Estimates, estimated SEs and confidence intervals are in meters.', h: .45, fontSize: 20, gap: .15 },
      { text: 'The treatment coefficient estimates the average enhanced-minus-standard effect across the population of centers.', h: .85, fontSize: 22 }
    ] }],
    takeaway: `Estimated average benefit: ${f(treatment.estimate)} m. The 95% confidence interval includes zero.`,
    drawEquation
  });
  averages.addNotes('Fit by REML to the unchanged rehabilitation CSV: distance ~ T + (1 + T | center), with T=0 for Standard and T=1 for Enhanced. The intercept and slope random effects are correlated. Use lmerTest coefficient-wise Satterthwaite t inference, with approximately 5 degrees of freedom for both fixed effects. Intervals are estimate plus or minus the corresponding .975 t quantile times the estimated SE, consistent with the two-sided p-values. No Bonferroni adjustment is used: these are two population coefficients, not the earlier family of six simple-effect comparisons. The intercept p-value tests a zero mean walking distance and is included only to retain the familiar coefficient-table format; the scientific focus is the treatment effect. The average point estimate equals the fixed interaction estimate in this balanced design, but its target and uncertainty differ. The CI including zero does not establish the absence of a population-average benefit. The model converges without a singular covariance estimate. See fit_random_interaction.R and assets/random-interaction/model_results.json for full precision and package versions.');

  const comparison = addConceptPanelsSlide(pptx, {
    title: 'Same estimated treatment effect, different uncertainty',
    intro: 'The balanced design gives the same estimated average treatment effect in both models.',
    panels: [{ blocks: [
      { table: {
        columns: [
          { label: 'Model', weight: 4.0 },
          { label: 'Estimated treatment\neffect (m)', weight: 2.6, align: 'center' },
          { label: 'Estimated\nSE (m)', weight: 1.7, align: 'center' },
          { label: 'p-value', weight: 1.5, align: 'center' }
        ],
        rows: [
          ['Fixed interaction', f(fixedInteraction.estimate), f(fixedInteraction.standard_error, 2), p(fixedInteraction.p)],
          ['Random intercept and slope', f(treatment.estimate), f(treatment.standard_error, 2), p(treatment.p)]
        ], fontSize: 22, headerFontSize: 18, headerHeight: .70, firstColumnBold: false
      }, h: 2.0, gap: .12 },
      { text: 'Fixed interaction: uncertainty about the average in these six centers comes from sampling patients.',
        h: .70, fontSize: 21, gap: .10 },
      { text: 'Random intercept and slope: uncertainty about the population average also reflects which centers entered the study.',
        h: .70, fontSize: 21 }
    ] }],
    takeaway: 'Treatment effects vary substantially across centers. With only six centers, generalizing to the population average adds considerable uncertainty.',
    takeawayHeight: 1.0, drawEquation
  });
  comparison.addNotes('Compare the fixed interaction model in Part 1 with the correlated random-intercept and slope model fitted to exactly the same balanced rehabilitation data. Both average point estimates are 21.265 m. The fixed-model estimated SE is approximately 4.6849 m and its p-value is below .001; the random-model estimated SE is 11.42585 m and its p-value is .12179. These address different targets: the average of the six included centers versus the average of a population of centers. Both models account for treatment heterogeneity; the fixed model targets the average in these six centers and does not include uncertainty from sampling different centers. Link directly to the treatment-effect variance on slide 26, sigma_d squared = 651.6111 m squared (SD 25.52667 m). This is variation in treatment effects across centers, not the size of the average treatment effect. In this balanced design, the random-model estimated variance of the population-average treatment estimate is sigma_d squared / 6 + 2 sigma squared / (6 * 10), approximately 108.60 + 21.95 = 130.55 m squared. The fixed-model variance is the patient-error term, approximately 21.95 m squared. Thus between-center treatment heterogeneity supplies most of the population-average uncertainty here. The intercept variance and intercept-slope covariance do not directly enter this balanced within-center treatment contrast. Do not add formulas to the slide. The fixed-model test uses 108 residual degrees of freedom and the random-model test uses approximately 5 Satterthwaite degrees of freedom; the p-value difference therefore reflects the reference distribution as well as the estimated SE. Do not portray random effects as inherently conservative or the two p-values as contradictory tests of an identical target.');

  const lab4 = addFigureSlide(pptx, {
    title: 'Time for Lab 4',
    assetPath: path.join(__dirname, '../week1/assets/lab2-break.png')
  });
  lab4.addNotes('Pause for Lab 4 on random-intercept and random-slope models. This is a lecture break placeholder; the lab case study, dataset and exercises have not yet been developed. Reuse the Week 1 collaborative-learning illustration and the standard title-plus-image break layout. After the lab, return to the rehabilitation lecture data to compare random-intercept-only and random-intercept-and-slope models.');

  const interceptOnly = results.random_intercept_model.fixed_coefficients.find(r => r.term === 'T');
  const omittedSlope = addConceptPanelsSlide(pptx, {
    title: 'What if we omit the random treatment slope?',
    intro: 'Back to rehabilitation: both models target the population-average treatment effect.',
    panels: [{ blocks: [
      { table: {
        columns: [
          { label: 'Model', weight: 4.0 },
          { label: 'Estimated treatment\neffect (m)', weight: 2.6, align: 'center' },
          { label: 'Estimated\nSE (m)', weight: 1.7, align: 'center' },
          { label: 'p-value', weight: 1.5, align: 'center' }
        ],
        rows: [
          ['Random intercept only', f(interceptOnly.estimate), f(interceptOnly.standard_error, 2), p(interceptOnly.p)],
          ['Random intercept and slope', f(treatment.estimate), f(treatment.standard_error, 2), p(treatment.p)]
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
  omittedSlope.addNotes('Post-Lab 4 discussion using the existing rehabilitation observations, not a newly developed lab dataset. Fit distance ~ T + (1 | center) and distance ~ T + (1 + T | center) by REML, with the same 0/1 treatment coding and the same fixed-effects formula. Both models target the population-average enhanced-minus-standard effect. This differs from the pre-lab fixed-versus-random comparison, where the target changes from included centers to a population. The balanced design gives an identical 21.265 m estimate under both random models. The random-intercept-only estimated SE is approximately 5.17 m, with p below .001; the random-slope estimated SE is 11.43 m, with p=.122. Coefficient tests use Satterthwaite degrees of freedom, approximately 113 for the intercept-only treatment coefficient and 5 for the random-slope treatment coefficient. Neither p-value is a test of adding a random slope. The simpler model assumes a common treatment effect and cannot represent shared center-specific deviations in the treatment effect. Some heterogeneity increases its estimated residual variance, but this does not replace the missing center-level treatment-effect variation in uncertainty about the population average. Its smaller estimated SE is therefore not evidence that it estimates that population effect more reliably when treatment effects truly vary. The two fits converge and are nonsingular. General conclusions are conditional on the assumed structures and relevant sampling assumptions; do not claim that adding a slope must always increase estimated SEs in all datasets. The next conceptual slide connects these structures to variance decomposition and the ICC.');

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
  decomposition.addNotes('Connect explicitly to the Week 1 random-intercept model, with its common individual-error variance: every patient in a center shares the same random intercept, so the center and residual variances give one common within-center correlation (intraclass correlation coefficient, ICC). In the current 0/1-coded model, standard-care patients share c_j and enhanced-care patients share c_j + d_j. Their outcome variances therefore differ in general. Distinguish three within-center pair types: two standard-care patients, two enhanced-care patients, and one patient on each treatment. Each pair type can have a different correlation. Random slopes can create this distinction even when the intercept-slope covariance is zero. The covariance is an additional part of the joint structure, not the sole reason one common ICC no longer suffices. Variance decomposition remains possible conditional on treatment, but simply summing the three variance parameters and turning them into percentages would ignore treatment coding and covariance. Keep these principles conceptual and do not add formulas or numerical ICCs to the slide. The earlier variance-component table supports direct comparisons of magnitudes because treatment is coded 0/1 and all three variances have units m squared. This closes the post-Lab 4 discussion by explaining how the common-ICC random-intercept structure differs from the random-slope structure. The Week 1 and Week 2 labels refer to the model structures introduced in each week; the preceding numerical comparison fits both models to the same Week 2 rehabilitation data.');

}

module.exports = { addRandomInteractionResults, correlationEquation };
