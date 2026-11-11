const path = require('path');
const { addBulletVisualSlide, addFigureSlide } = require('../../slides-common/js/pptx_layout_primitives');
const { addConceptPanelsSlide } = require('../../slides-common/js/concept_panels');

const E = {
  model: String.raw`Y_{ij}=\mu+u_i+v_j+\varepsilon_{ij}`,
  patients: String.raw`u_i\sim N(0,\sigma_{\mathrm{patient}}^2)`,
  radiologists: String.raw`v_j\sim N(0,\sigma_{\mathrm{radiologist}}^2)`,
  errors: String.raw`\varepsilon_{ij}\sim N(0,\sigma^2)`
};
function agreementEquation(results) {
  const v = results.variance_components;
  return String.raw`\widehat{\mathrm{ICC}}=\frac{${v.patient.toFixed(2)}}{${v.patient.toFixed(2)}+${v.radiologist.toFixed(2)}+${v.residual.toFixed(2)}}\approx ${results.icc_absolute_agreement_single.toFixed(2)}`;
}

function addCrossedCaseStudy(pptx, drawEquation, results) {
  const assets = path.join(__dirname, 'assets/radiologists');
  const setting = addBulletVisualSlide(pptx, {
    title: 'Case study: agreement between radiologists',
    bulletTitle: 'MEASURING TUMOR DIAMETER',
    bullets: [
      'Simulated study: CT scans from 10 patients assessed independently by five radiologists.',
      'Every radiologist measures the maximum tumor diameter on every scan.',
      'Outcome: tumor diameter in millimeters, giving 50 measurements.',
      'We want to generalize beyond these patients and radiologists.'
    ],
    bulletFontSize: 21, bulletMinFontSize: 21,
    assetPath: path.join(assets, 'radiology-setting.png'),
    note: 'Do different radiologists obtain similar measurements for the same patients?',
    noteFontSize: 20, noteHeight: 1.0
  });
  setting.addNotes('Reuse the old Week 2 simulated radiologist case study, generated with seed 123. Ten patients and five radiologists are fully crossed, with one diameter measurement per patient-radiologist pair. The scan is held fixed for a patient, and radiologists assess scans independently. Patient and radiologist are the two grouping variables. The intended inference concerns the populations represented by these patients and radiologists; treating their effects as random does not by itself establish representative sampling. The right-hand image is an AI-generated fictional illustration of a radiology reading room, not a study photograph or an actual diagnostic scan.');

  const exploratory = addFigureSlide(pptx, {
    title: 'Exploring the radiologists’ measurements',
    assetPath: path.join(assets, 'measurements.png')
  });
  exploratory.addNotes('Patients are on the horizontal axis, and each colored line connects the measurements made by one radiologist. All radiologists tend to measure larger diameters for the same patients. Some radiologists also tend to measure higher than others. The points are the 50 observed measurements, one per patient-radiologist pair, with no overlaid means or individual-observation clouds. Connecting lines help track a radiologist across patients; patient order is arbitrary and does not represent time. Use the plot to motivate two shared sources of variation, then fit the crossed random-intercept model directly.');

  const model = addConceptPanelsSlide(pptx, {
    title: 'A model with two crossed grouping variables',
    lead: { equation: E.model, text: 'Yᵢⱼ is the measurement for patient i made by radiologist j.' },
    panels: [
      { title: 'WHAT CONTRIBUTES TO A MEASUREMENT?', blocks: [
        { text: 'μ: overall mean tumor diameter.', h: .50, fontSize: 22, gap: .12 },
        { text: 'uᵢ: patient i’s deviation from the mean.', h: .55, fontSize: 22, gap: .12 },
        { text: 'vⱼ: radiologist j’s tendency to measure higher or lower.', h: .75, fontSize: 22, gap: .12 },
        { text: 'εᵢⱼ: remaining measurement variation.', h: .50, fontSize: 22 }
      ] },
      { title: 'TWO RANDOM INTERCEPTS', blocks: [
        { equation: E.patients, h: .50, gap: .12 },
        { equation: E.radiologists, h: .50, gap: .12 },
        { equation: E.errors, h: .50, gap: .15 },
        { text: 'Patient effects, radiologist effects and residual errors are mutually independent.', h: .75, fontSize: 21 }
      ] }
    ],
    takeaway: 'Measurements are related when they share the same patient or the same radiologist.',
    drawEquation
  });
  model.addNotes('The fitted model is measurement ~ 1 + (1 | patient) + (1 | radiologist), estimated by REML. Each patient effect is an independent draw from its zero-mean Normal population distribution; each radiologist effect is similarly independent across radiologists; residual errors are independent across observations. These three collections are mutually independent, with common variances within each collection. This is an additive crossed random-intercept model. There is no separate nested higher level and no random-intercept/slope covariance matrix. Measurements of the same patient by different radiologists share u_i; measurements of different patients by the same radiologist share v_j. Measurements sharing neither have zero covariance under the model. With one observation per pair, residual variation cannot be separated from an additional pair-specific contribution. Keep that identifiability detail in notes, without a fixed-effects or interaction detour in this brief section.');

  const v = results.variance_components;
  const fitted = addConceptPanelsSlide(pptx, {
    title: 'Where does measurement variation come from?',
    intro: 'Estimated variance components from the crossed model fitted to the radiologist study.',
    panels: [{ blocks: [
      { table: {
        columns: [
          { label: 'Source of variation', weight: 3.4 },
          { label: 'Estimate\n(mm²)', weight: 2.0, align: 'center' },
          { label: 'Interpretation', weight: 5.7 }
        ],
        rows: [
          ['Patient', v.patient.toFixed(2), 'Differences between patients'],
          ['Radiologist', v.radiologist.toFixed(2), 'Systematic radiologist differences'],
          ['Residual', v.residual.toFixed(2), 'Remaining measurement variation']
        ], fontSize: 21, headerFontSize: 19, headerHeight: .65, firstColumnBold: false
      }, h: 2.10, gap: .18 },
      { equation: agreementEquation(results), h: .80, gap: .12 },
      { text: 'Intraclass correlation coefficient (ICC): absolute agreement for a single measurement.',
        h: .65, fontSize: 21 }
    ] }],
    takeaway: `About ${Math.round(100 * results.icc_absolute_agreement_single)}% of the estimated total variation is between patients. Radiologist differences and residual variation reduce agreement.`,
    drawEquation
  });
  fitted.addNotes('All estimates come from the new reproducible REML fit of the unchanged old simulated dataset. Patient variance = ' + v.patient + ', radiologist variance = ' + v.radiologist + ', residual variance = ' + v.residual + '. The estimated overall mean is ' + results.mean + ' mm. The absolute-agreement single-measurement ICC is patient variance divided by the sum of the patient, radiologist and residual variances, giving ' + results.icc_absolute_agreement_single + '. The displayed calculation rounds inputs to two decimals. This is the model-implied correlation of measurements from two independently sampled different radiologists on the same patient, and quantifies agreement for one randomly selected radiologist’s measurement. Systematic radiologist offsets count against absolute agreement. It is not the reliability of an average over five radiologists. As with the preceding discussion, there are different correlations depending on which grouping variable a pair shares: this slide explicitly selects the same-patient, different-radiologist comparison relevant to agreement. The patient component is the largest estimated source, but do not give a universal qualitative reliability rating or suggest that the ICC is independent of the heterogeneity of the patient population. No interval is shown; variance components and the ICC are themselves estimated with uncertainty from a small simulated study.');
}

module.exports = { addCrossedCaseStudy, equations: Object.values(E), agreementEquation };
