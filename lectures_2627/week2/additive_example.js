const path = require('path');
const { COLORS, FONT_FAMILIES, LAYOUT } = require('../../slides-common/js/blr_theme');
const { addBase, addSlideTitle, addAssetFrame, addFooter } = require('../../slides-common/js/pptx_layout_primitives');
const { addConceptPanelsSlide } = require('../../slides-common/js/concept_panels');

const equation = String.raw`Y_{ij}=\underbrace{\beta_0+c_j}_{\text{Standard-protocol mean for donor }j}+\underbrace{\beta_1T_{ij}}_{\text{Protocol contribution}}+\underbrace{\varepsilon_{ij}}_{\text{Aliquot deviation}}`;
const interactionEquation = String.raw`Y_{ij}=\underbrace{\beta_0+c_j}_{\text{Standard-protocol mean for donor }j}+\underbrace{(\beta_1+d_j)T_{ij}}_{\text{Protocol contribution}}+\underbrace{\varepsilon_{ij}}_{\text{Aliquot deviation}}`;
const comparisonEquations = [
  String.raw`Y_{ij}=\beta_0+c_j+\beta_1T_{ij}+\varepsilon_{ij}`,
  String.raw`Y_{ij}=\beta_0+c_j+(\beta_1+{\color{orange}d_j})T_{ij}+\varepsilon_{ij}`
];

function addKnownMeansExample(pptx, drawEquation, interaction = false) {
  const slide = pptx.addSlide();
  addBase(slide);
  addSlideTitle(slide, interaction ? 'What if the protocol difference varies between donors?' : 'The additive model: a common protocol effect across donors');
  const body = LAYOUT.body;
  const text = (value, y, h, options = {}) => slide.addText(value, {
    x: body.x, y, w: body.w, h, fontFace: FONT_FAMILIES.sans,
    fontSize: 20, color: COLORS.text, margin: 0, breakLine: false, ...options
  });
  // A custom full-width figure plus model: the shared primitives own framing and type.
  text(interaction ? 'Illustration with two donors and true protocol effects of 2 and 10 µg/mg.' : 'Illustration with two donors and a true protocol effect of 6 µg/mg in each donor.', body.y + .04, .42,
    { bold: true, color: COLORS.primary });
  addAssetFrame(slide, { box: { x: body.x, y: 1.63, w: body.w, h: 3.65 },
    assetPath: path.join(__dirname, 'assets/additive-example', interaction ? 'interaction-means.png' : 'additive-means.png'), framed: false });
  drawEquation(slide, interaction ? interactionEquation : equation, body.x + .25, 5.48, body.w - .50, .86);
  text('Tᵢⱼ = 0 for Standard and 1 for Modified.  i = aliquot; j = donor.', 6.49, .33,
    { fontSize: 18, align: 'center' });
  text('Individual aliquot yields vary around their group mean through εᵢⱼ.', 6.89, .26,
    { fontSize: 17, color: COLORS.muted, align: 'center' });
  slide.addNotes('');
  addFooter(slide);
  return slide;
}

const addAdditiveExample = (pptx, drawEquation) => addKnownMeansExample(pptx, drawEquation);
function addInteractionExample(pptx, drawEquation) {
  const slide = addConceptPanelsSlide(pptx, {
    title: 'What if the protocol difference varies between donors?',
    drawEquation,
    panels: [
      { title: 'COMMON PROTOCOL EFFECT', blocks: [
        { image: path.join(__dirname, 'assets/additive-example/additive-profile.png'), h: 3.0 },
        { equation: comparisonEquations[0], h: .50 },
        { text: 'Protocol difference: 6 µg/mg in both donors.', fontSize: 20, h: .40 }
      ] },
      { title: 'PROTOCOL EFFECT VARIES', blocks: [
        { image: path.join(__dirname, 'assets/additive-example/interaction-profile.png'), h: 3.0 },
        { equation: comparisonEquations[1], h: .50 },
        { text: 'Protocol differences: 2 and 10 µg/mg.', fontSize: 20, h: .40 }
      ] }
    ],
    takeaway: 'Tᵢⱼ = 0 for Standard and 1 for Modified.  i = aliquot; j = donor.',
    takeawayHeight: .62
  });
  slide.addNotes('');
  return slide;
}
module.exports = { addAdditiveExample, addInteractionExample, equation, interactionEquation, comparisonEquations };
