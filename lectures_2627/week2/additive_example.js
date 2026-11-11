const path = require('path');
const { COLORS, FONT_FAMILIES, LAYOUT } = require('../../slides-common/js/blr_theme');
const { addBase, addSlideTitle, addAssetFrame, addFooter } = require('../../slides-common/js/pptx_layout_primitives');

const equation = String.raw`Y_{ij}=\underbrace{\beta_0+c_j}_{\text{Standard-care mean in center }j}+\underbrace{\beta_1T_{ij}}_{\text{Treatment contribution}}+\underbrace{\varepsilon_{ij}}_{\text{Individual deviation}}`;
const interactionEquation = String.raw`Y_{ij}=\underbrace{\beta_0+c_j}_{\text{Standard-care mean in center }j}+\underbrace{(\beta_1+d_j)T_{ij}}_{\text{Treatment contribution}}+\underbrace{\varepsilon_{ij}}_{\text{Individual deviation}}`;

function addKnownMeansExample(pptx, drawEquation, interaction = false) {
  const slide = pptx.addSlide();
  addBase(slide);
  addSlideTitle(slide, interaction ? 'Different treatment effects across centers' : 'A common treatment effect in both centers');
  const body = LAYOUT.body;
  const text = (value, y, h, options = {}) => slide.addText(value, {
    x: body.x, y, w: body.w, h, fontFace: FONT_FAMILIES.sans,
    fontSize: 20, color: COLORS.text, margin: 0, breakLine: false, ...options
  });
  // A custom full-width figure plus model: the shared primitives own framing and type.
  text('Hypothetical example: assume the four true group means are known.', body.y + .04, .42,
    { bold: true, color: COLORS.primary });
  addAssetFrame(slide, { box: { x: body.x, y: 1.63, w: body.w, h: 3.65 },
    assetPath: path.join(__dirname, 'assets/additive-example', interaction ? 'interaction-means.png' : 'additive-means.png'), framed: false });
  drawEquation(slide, interaction ? interactionEquation : equation, body.x + .25, 5.48, body.w - .50, .86);
  text('Tᵢⱼ = 0 for standard care and 1 for enhanced care.  i = patient; j = center.', 6.49, .33,
    { fontSize: 18, align: 'center' });
  text('Individual patient outcomes vary around their group mean through εᵢⱼ.', 6.89, .26,
    { fontSize: 17, color: COLORS.muted, align: 'center' });
  const commonNotes = 'The four numbers are assumed true expected outcomes, not sample estimates. The row and column averages weight both entries equally. Treatment is coded 0/1 and center uses effects coding: beta_0 = 370, c_A = -20, c_B = 20, beta_1 = 20. The grand mean is 380 and is not beta_0 under this parameterization. Center is on the horizontal axis and color denotes treatment. The plotted points contain no individual sampling noise. Y_ij is a patient outcome and epsilon_ij is its zero-mean deviation from the appropriate group mean. The mu subscripts identify treatment and center. ';
  slide.addNotes(commonNotes + (interaction
    ? 'The treatment gap is 10 m in A and 30 m in B, averaging 20 m. The center-specific deviations d_A = -10 and d_B = +10 sum to zero. The standard-care means remain 350 and 390; the enhanced-care means become 360 and 420. Both treatment column averages and the grand mean are unchanged from the additive example, while the row averages become 355 and 405. Row averages do not define c_j under treatment dummy coding. The sloping treatment lines show changes across centers; the vertical gaps show the treatment effects.'
    : 'Both vertical brackets show the same 20 m treatment gap; the line rise from A to B is the 40 m baseline center difference, not beta_1. The next slide introduces treatment-by-center interaction with the same average effect.'));
  addFooter(slide);
  return slide;
}

const addAdditiveExample = (pptx, drawEquation) => addKnownMeansExample(pptx, drawEquation);
const addInteractionExample = (pptx, drawEquation) => addKnownMeansExample(pptx, drawEquation, true);
module.exports = { addAdditiveExample, addInteractionExample, equation, interactionEquation };
