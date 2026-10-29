const { getSlots, addBase, addSlideTitle, addBulletBox, addNoteBox, addTextBox, addAssetFrame, addFooter } = require('./pptx_layout_primitives');
const { COLORS, RADII, STROKES, TYPOGRAPHY } = require('./blr_theme');

// Study layout used whenever a scientific question and design need a shared visual hierarchy.
function addStudyOverviewSlide(pptx, { title, studyTitle, bullets, groupsTitle, groups, question, assetPath }) {
  const slide = pptx.addSlide();
  const slots = getSlots({ noteHeight: 1.15 });
  addBase(slide);
  addSlideTitle(slide, title);
  addBulletBox(slide, { box: slots.text, title: studyTitle, bullets, fontSize: 19, minFontSize: 17 });
  addNoteBox(slide, { box: slots.note, title: 'RESEARCH QUESTION', text: question, fontSize: 17, minFontSize: 16 });
  if (assetPath) addAssetFrame(slide, { box: slots.visual, assetPath, fit: 'cover' });
  else addTextBox(slide, { box: slots.visual, title: groupsTitle, text: groups, fontSize: 20, minFontSize: 18 });
  addFooter(slide);
  return slide;
}
// Composite layout: the equation, its interpretation and its fitted illustration stay together.
function addConditionalModelSlide(pptx, { title, assetPath, equation, drawEquation, bullets, note }) {
  const slide = pptx.addSlide();
  const slots = getSlots({ noteHeight: 0.9 });
  addBase(slide);
  addSlideTitle(slide, title);
  const top = { ...slots.text, h: 1.50 };
  const bottom = { x: slots.text.x, y: top.y + top.h + 0.12, w: slots.text.w, h: slots.text.h - top.h - 0.12 };
  // The card intentionally contains its title and the equation image.
  slide.addShape('roundRect', { ...top, rectRadius: RADII.card, fill: { color: COLORS.card }, line: STROKES.card });
  slide.addText('CONDITIONAL MODEL', { x: top.x + 0.24, y: top.y + 0.17, w: top.w - 0.48, h: 0.3, ...TYPOGRAPHY.panelTitle, color: COLORS.primary, margin: 0 });
  drawEquation(slide, equation, top.x + 0.35, top.y + 0.60, top.w - 0.70, 0.55);
  addBulletBox(slide, { box: bottom, title: 'INTERPRETATION', bullets, fontSize: 19, minFontSize: 17 });
  addAssetFrame(slide, { box: slots.visual, assetPath });
  addNoteBox(slide, { box: slots.note, text: note, fontSize: 14, minFontSize: 13 });
  addFooter(slide);
  return slide;
}
module.exports = { addStudyOverviewSlide, addConditionalModelSlide };
