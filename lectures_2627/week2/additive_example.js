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
  slide.addNotes('');
  addFooter(slide);
  return slide;
}

const addAdditiveExample = (pptx, drawEquation) => addKnownMeansExample(pptx, drawEquation);
const addInteractionExample = (pptx, drawEquation) => addKnownMeansExample(pptx, drawEquation, true);
module.exports = { addAdditiveExample, addInteractionExample, equation, interactionEquation };
