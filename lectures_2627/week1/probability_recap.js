const path = require('path');
const { addBase, addSlideTitle, addBulletBox, addAssetFrame, addNoteBox, addFooter, getSlots } =
  require('../../slides-common/js/pptx_layout_primitives');
const { COLORS, FONT_FAMILIES, RADII, STROKES } = require('../../slides-common/js/blr_theme');
const { autoFontSize } = require('./pptxgenjs_helpers/text');

const equations = [
  String.raw`E(Y)=\mu`,
  String.raw`\operatorname{Var}(Y)=E\!\left[(Y-\mu)^2\right]=\sigma^2`,
  String.raw`\operatorname{SD}(Y)=\sqrt{\operatorname{Var}(Y)}=\sigma`,
  String.raw`\operatorname{Cov}(Y_1,Y_2)=E\!\left[(Y_1-\mu_1)(Y_2-\mu_2)\right]`,
];

function caption(slide, text, box) {
  slide.addText(text, autoFontSize(text, FONT_FAMILIES.sans, {
    ...box, fontSize: 18, minFontSize: 18, maxFontSize: 18,
    color: COLORS.muted, align: 'center', valign: 'mid', margin: 0,
  }));
}

function notationPanel(slide, box) {
  // The background intentionally contains the equation images.
  slide.addShape('roundRect', { ...box, rectRadius: RADII.card,
    fill: { color: COLORS.card }, line: STROKES.card });
}

function baseRecap(pptx, title, bullets, asset, plotHeight) {
  const slide = pptx.addSlide();
  const { text, visual } = getSlots({ textFraction: .40 });
  addBase(slide);
  addSlideTitle(slide, title);
  addBulletBox(slide, { box: text, bullets, fontSize: 22, minFontSize: 22, paraSpaceAfter: 19 });
  // Each white card intentionally contains its plot or notation.
  addAssetFrame(slide, { box: { ...visual, h: plotHeight }, assetPath: path.join('assets', asset) });
  addFooter(slide);
  return { slide, visual };
}

function addProbabilityRecap(pptx, drawEquation) {
  let { slide, visual } = baseRecap(pptx, 'Expected value, variance and standard deviation', [
    'Y is a random variable: its value can differ across repeated realizations.',
    'Expected value μ is its long-run average, or balance point. It need not be the peak.',
    'Variance σ² is the average squared deviation from μ.',
    'Standard deviation σ is the square root of the variance, in the same units as Y.',
  ], 'recap-distributions.png', 3.30);
  caption(slide, 'Both: mean 4, variance 4, SD 2. Dashed lines mark the means.',
    { x: visual.x, y: 4.43, w: visual.w, h: .47 });
  notationPanel(slide, { x: visual.x, y: 4.97, w: visual.w, h: 2.05 });
  drawEquation(slide, equations[0], visual.x + .25, 5.02, visual.w - .50, .38);
  drawEquation(slide, equations[1], visual.x + .25, 5.62, visual.w - .50, .48);
  drawEquation(slide, equations[2], visual.x + .25, 6.32, visual.w - .50, .48);

  ({ slide, visual } = baseRecap(pptx, 'Covariance and independence', [
    'Each point is a pair of values of Y₁ and Y₂.',
    'Covariance measures how deviations from the two means vary together.',
    'Independence means knowing one variable does not change the distribution of the other.',
    'Independence implies zero covariance. The converse does not generally hold.',
  ], 'recap-covariance.png', 3.38));
  caption(slide, 'Both variables have mean 4 and variance 4.\nDashed lines mark the expected values.',
    { x: visual.x, y: 4.51, w: visual.w, h: .65 });
  notationPanel(slide, { x: visual.x, y: 5.28, w: visual.w, h: .76 });
  drawEquation(slide, equations[3], visual.x + .12, 5.36, visual.w - .24, .54);
  addNoteBox(slide, { box: { x: visual.x, y: 6.15, w: visual.w, h: .84 },
    text: 'Positive covariance: deviations from the two means tend to have the same sign.',
    fontSize: 20, minFontSize: 20 });
}

module.exports = { addProbabilityRecap, equations };
