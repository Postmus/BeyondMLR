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
  setting.addNotes('');

  const exploratory = addFigureSlide(pptx, {
    title: 'Exploring the radiologists’ measurements',
    assetPath: path.join(assets, 'measurements.png')
  });
  exploratory.addNotes('');

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
  model.addNotes('');

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
  fitted.addNotes('');
}

module.exports = { addCrossedCaseStudy, equations: Object.values(E), agreementEquation };
