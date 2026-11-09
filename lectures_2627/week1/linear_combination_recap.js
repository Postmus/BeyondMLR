const { addBase, addSlideTitle, addBulletBox, addNoteBox, addFooter, getSlots } =
  require('../../slides-common/js/pptx_layout_primitives');
const { COLORS, FONT_FAMILIES, RADII, STROKES } = require('../../slides-common/js/blr_theme');
const { autoFontSize } = require('./pptxgenjs_helpers/text');

const general = {
  variable: String.raw`Z=a+bY_1+cY_2`,
  mean: String.raw`E(Z)=a+bE(Y_1)+cE(Y_2)`,
  variance: String.raw`\begin{aligned}\operatorname{Var}(Z)={}&b^2\operatorname{Var}(Y_1)+c^2\operatorname{Var}(Y_2)\\&+2bc\,\operatorname{Cov}(Y_1,Y_2)\end{aligned}`,
};
const cases = [
  { label: 'Add a constant', variable: 'a+Y', mean: 'a+\\mu', variance: '\\sigma^2' },
  { label: 'Rescale', variable: 'bY', mean: 'b\\mu', variance: 'b^2\\sigma^2' },
  { label: 'Sum\n(independent)', variable: 'Y_1+Y_2', mean: '\\mu_1+\\mu_2', variance: '\\sigma_1^2+\\sigma_2^2' },
  { label: 'Difference\n(independent)', variable: 'Y_1-Y_2', mean: '\\mu_1-\\mu_2', variance: '\\sigma_1^2+\\sigma_2^2' },
];

function text(slide, value, box, { fontSize = 21, ...options } = {}) {
  slide.addText(value, autoFontSize(value, FONT_FAMILIES.sans, {
    ...box, minFontSize: fontSize, maxFontSize: fontSize, fontSize,
    color: COLORS.text, margin: 0, valign: 'mid', ...options,
  }));
}

function panel(slide, box) {
  // Background intentionally contains the labels and equation images.
  slide.addShape('roundRect', { ...box, rectRadius: RADII.card,
    fill: { color: COLORS.card }, line: STROKES.card });
}

function addLinearCombinationRecap(pptx, drawEquation) {
  let slide = pptx.addSlide();
  const { text: left, visual: right, body } = getSlots({ textFraction: .40 });
  addBase(slide);
  addSlideTitle(slide, 'Linear combinations of random variables');
  addBulletBox(slide, { box: left, fontSize: 22, minFontSize: 22, paraSpaceAfter: 15,
    bullets: [
      'A linear combination multiplies random variables by constants and adds them.',
      'Expected values combine using the same weights.',
      'Variance depends on the squared weights and on covariance.',
      'For independent variables, the covariance term is zero.',
      'Adding a constant changes the mean, but not the variance.',
    ] });
  panel(slide, { x: right.x, y: 1.04, w: right.w, h: 1.36 });
  drawEquation(slide, general.variable, right.x + .25, 1.23, right.w - .50, .50);
  text(slide, 'a, b and c are constants; means and variances are finite.',
    { x: right.x + .20, y: 1.91, w: right.w - .40, h: .30 }, { fontSize: 18, align: 'center', color: COLORS.muted });
  panel(slide, { x: right.x, y: 2.58, w: right.w, h: 1.20 });
  text(slide, 'EXPECTED VALUE', { x: right.x + .25, y: 2.78, w: right.w - .50, h: .28 },
    { fontSize: 16, bold: true, color: COLORS.primary });
  drawEquation(slide, general.mean, right.x + .25, 3.15, right.w - .50, .42);
  panel(slide, { x: right.x, y: 3.94, w: right.w, h: 1.63 });
  text(slide, 'VARIANCE', { x: right.x + .25, y: 4.12, w: right.w - .50, h: .28 },
    { fontSize: 16, bold: true, color: COLORS.primary });
  drawEquation(slide, general.variance, right.x + .22, 4.56, right.w - .44, .80);
  addNoteBox(slide, { box: { x: right.x, y: 5.73, w: right.w, h: 1.29 },
    text: 'The expectation and variance rules apply regardless of distribution.\nIf Y₁ and Y₂ are independent and Normally distributed, Z is also Normally distributed.', fontSize: 18, minFontSize: 18 });
  addFooter(slide);

  slide = pptx.addSlide();
  addBase(slide);
  addSlideTitle(slide, 'Useful special cases');
  text(slide, 'Write μ = E(Y) and σ² = Var(Y), with subscripts for Y₁ and Y₂.',
    { x: body.x, y: 1.10, w: body.w, h: .43 }, { fontSize: 21, color: COLORS.muted });
  const widths = [3.45, 2.85, 2.955, 2.955];
  const tableY = 1.78, headerH = .56, rowH = 1.00;
  // Row backgrounds intentionally contain all cell labels and equations.
  slide.addShape('rect', { x: body.x, y: tableY, w: body.w, h: headerH,
    fill: { color: COLORS.primary }, line: { color: COLORS.primary } });
  let x = body.x;
  ['TRANSFORMATION', 'RANDOM VARIABLE', 'EXPECTED VALUE', 'VARIANCE'].forEach((label, i) => {
    text(slide, label, { x: x + .18, y: tableY + .10, w: widths[i] - .36, h: .36 },
      { fontSize: 15, bold: true, color: COLORS.onPrimary, align: i ? 'center' : 'left' });
    x += widths[i];
  });
  cases.forEach((item, index) => {
    const y = tableY + headerH + index * rowH;
    slide.addShape('rect', { x: body.x, y, w: body.w, h: rowH,
      fill: { color: COLORS.card }, line: STROKES.card });
    text(slide, item.label, { x: body.x + .18, y: y + .12, w: widths[0] - .36, h: rowH - .24 },
      { fontSize: 20, bold: true });
    x = body.x + widths[0];
    ['variable', 'mean', 'variance'].forEach((key, i) => {
      drawEquation(slide, item[key], x + .25, y + .29, widths[i + 1] - .50, .42);
      x += widths[i + 1];
    });
  });
  addNoteBox(slide, { box: { x: body.x, y: 6.51, w: body.w, h: .51 },
    text: 'For independent variables, both a sum and a difference have variance σ₁² + σ₂².',
    fontSize: 20, minFontSize: 20 });
  addFooter(slide);
}

const equations = [...Object.values(general), ...cases.flatMap(item => [item.variable, item.mean, item.variance])];
module.exports = { addLinearCombinationRecap, equations };
