const { getSlots, addBase, addSlideTitle, addBulletBox, addNoteBox, addTextBox, addAssetFrame, addFooter } = require('./pptx_layout_primitives');
const { COLORS, RADII, STROKES, TYPOGRAPHY, SPACING } = require('./blr_theme');

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
function addConditionalModelSlide(pptx, {
  title, assetPath, equation, drawEquation, bullets, note,
  modelLabel = 'CONDITIONAL MODEL', coding = '', assumptionEquation = '', noteHeight = 0.9, noteFontSize = 14,
  modelHeight, interpretationTitle = 'INTERPRETATION', bulletFontSize = 19, bulletMinFontSize = 17,
}) {
  const slide = pptx.addSlide();
  const slots = getSlots({ noteHeight });
  addBase(slide);
  addSlideTitle(slide, title);
  const top = { ...slots.text, h: modelHeight ?? (assumptionEquation ? 1.93 : (coding ? 1.80 : 1.50)) };
  const bottom = { x: slots.text.x, y: top.y + top.h + 0.12, w: slots.text.w, h: slots.text.h - top.h - 0.12 };
  // The card intentionally contains its title and the equation image.
  slide.addShape('roundRect', { ...top, rectRadius: RADII.card, fill: { color: COLORS.card }, line: STROKES.card });
  slide.addText(modelLabel, { x: top.x + 0.24, y: top.y + 0.17, w: top.w - 0.48, h: 0.3, ...TYPOGRAPHY.panelTitle, color: COLORS.primary, margin: 0 });
  drawEquation(slide, equation, top.x + 0.35, top.y + (assumptionEquation ? 0.55 : 0.60), top.w - 0.70, assumptionEquation ? 0.44 : 0.55);
  if (assumptionEquation) drawEquation(slide, assumptionEquation, top.x + 0.35, top.y + 1.02, top.w - 0.70, 0.35);
  if (coding) slide.addText(coding, {
    x: top.x + 0.24, y: top.y + (assumptionEquation ? 1.48 : 1.29), w: top.w - 0.48, h: 0.30,
    ...TYPOGRAPHY.bodySmall, fontSize: 15, color: COLORS.muted, margin: 0,
  });
  addBulletBox(slide, { box: bottom, title: interpretationTitle, bullets, fontSize: bulletFontSize, minFontSize: bulletMinFontSize });
  addAssetFrame(slide, { box: slots.visual, assetPath });
  addNoteBox(slide, { box: slots.note, text: note, fontSize: noteFontSize, minFontSize: noteFontSize - 1 });
  addFooter(slide);
  return slide;
}
// Two-column assumptions layout with a vertical stack of diagnostics.
function addAssumptionsDiagnosticsSlide(pptx, {
  title, bullets, residualAssetPath, qqAssetPath, assumptionsHeight = 4.0,
}) {
  const slide = pptx.addSlide();
  const slots = getSlots();
  const plotHeight = (slots.body.h - SPACING.smallGap) / 2;
  addBase(slide);
  addSlideTitle(slide, title);
  addBulletBox(slide, {
    box: { ...slots.text, y: slots.text.y + (slots.text.h - assumptionsHeight) / 2, h: assumptionsHeight },
    title: 'MODEL ASSUMPTIONS', bullets,
    fontSize: 21, minFontSize: 20, paraSpaceAfter: 16,
  });
  addAssetFrame(slide, { box: { ...slots.visual, h: plotHeight }, assetPath: residualAssetPath });
  addAssetFrame(slide, {
    box: { ...slots.visual, y: slots.visual.y + plotHeight + SPACING.smallGap, h: plotHeight },
    assetPath: qqAssetPath,
  });
  addFooter(slide);
  return slide;
}
// Paired diagnostic plots above aligned explanations, with a shared limitation.
function addDiagnosticsInterpretationSlide(pptx, { title, diagnostics, takeaway, plotHeight = 2.40 }) {
  const slide = pptx.addSlide();
  const { body } = getSlots();
  addBase(slide);
  addSlideTitle(slide, title);
  const plotH = plotHeight, noteH = .94;
  const columnW = (body.w - SPACING.gutter) / 2;
  const explanationY = body.y + plotH + SPACING.smallGap;
  const explanationH = body.h - plotH - noteH - 2 * SPACING.smallGap;
  diagnostics.forEach((diagnostic, index) => {
    const x = body.x + index * (columnW + SPACING.gutter);
    addAssetFrame(slide, { box: { x, y: body.y, w: columnW, h: plotH }, assetPath: diagnostic.assetPath });
    addTextBox(slide, {
      box: { x, y: explanationY, w: columnW, h: explanationH },
      title: 'WHAT WE USE THIS PLOT FOR', text: diagnostic.purpose,
      fontSize: 22, minFontSize: 22,
    });
  });
  addNoteBox(slide, { box: { ...body, y: body.y + body.h - noteH, h: noteH },
    text: takeaway, fontSize: 20, minFontSize: 20 });
  addFooter(slide);
  return slide;
}
// A model assumption in plain language, followed by its practical implications.
function addAssumptionFocusSlide(pptx, { title, statement, points, reminder }) {
  const slide = pptx.addSlide();
  const { body } = getSlots();
  addBase(slide);
  addSlideTitle(slide, title);
  addNoteBox(slide, {
    box: { ...body, h: 1.30 }, text: statement, fontSize: 22, minFontSize: 21,
  });
  points.forEach((point, index) => {
    const y = body.y + 1.57 + index * 1.12;
    slide.addText(point.title, {
      x: body.x + 0.18, y, w: body.w - 0.36, h: 0.30,
      ...TYPOGRAPHY.panelTitle, fontSize: 17, color: COLORS.primary, margin: 0,
    });
    slide.addText(point.text, {
      x: body.x + 0.18, y: y + 0.35, w: body.w - 0.36, h: 0.65,
      ...TYPOGRAPHY.body, fontSize: 20, color: COLORS.text, margin: 0, valign: 'top',
    });
  });
  addNoteBox(slide, {
    box: { ...body, y: body.y + body.h - 0.72, h: 0.72 },
    text: reminder, fontSize: 20, minFontSize: 19,
  });
  addFooter(slide);
  return slide;
}
// Ordered summary sections keep the scientific perspective before its consequences.
// Measure the text at its final size rather than shrinking it to fit a card.
function addStructuredSummarySlide(pptx, { title, sections, takeaway }) {
  const { calcTextBox } = require('../../lectures_2627/week1/pptxgenjs_helpers/text');
  const slide = pptx.addSlide();
  const { body } = getSlots();
  addBase(slide); addSlideTitle(slide, title);
  const noteH = .82, pad = .10, gap = .10, labelW = 2.2;
  const textX = body.x + .24 + labelW + .24;
  const textW = body.w - labelW - .72;
  let y = body.y;
  for (const section of sections) {
    const paragraphs = section.paragraphs;
    const heights = paragraphs.map(text => calcTextBox(20, {
      text, w: textW, fontFace: TYPOGRAPHY.body.fontFace, margin: 0, padding: 0,
    }).h);
    const labelH = calcTextBox(17, {
      text: section.title, w: labelW, fontFace: TYPOGRAPHY.panelTitle.fontFace,
      bold: true, margin: 0, padding: 0,
    }).h;
    const contentH = heights.reduce((a, b) => a + b, 0) + .08 * (paragraphs.length - 1);
    const bottomPad = section === sections[0] ? .18 : pad;
    const h = Math.max(labelH, contentH) + pad + bottomPad;
    // Each row background intentionally contains its heading and paragraphs.
    slide.addShape('roundRect', { x: body.x, y, w: body.w, h,
      rectRadius: RADII.card, fill: { color: COLORS.card }, line: STROKES.card });
    slide.addText(section.title, { x: body.x + .24, y: y + pad, w: labelW, h: labelH,
      ...TYPOGRAPHY.panelTitle, fontSize: 17, color: COLORS.primary, margin: 0, valign: 'top' });
    let textY = y + pad;
    paragraphs.forEach((text, i) => {
      slide.addText(text, { x: textX, y: textY, w: textW, h: heights[i],
        ...TYPOGRAPHY.body, fontSize: 20, color: COLORS.text, margin: 0, valign: 'top' });
      textY += heights[i] + .08;
    });
    y += h + gap;
  }
  const noteY = body.y + body.h - noteH;
  if (y > noteY + .001) throw new Error(`Summary content exceeds its available height: ${title}`);
  addNoteBox(slide, { box: { x: body.x, y: noteY, w: body.w, h: noteH },
    text: takeaway, fontSize: 20, minFontSize: 20 });
  addFooter(slide);
  return slide;
}

module.exports = { addStudyOverviewSlide, addConditionalModelSlide, addAssumptionsDiagnosticsSlide, addDiagnosticsInterpretationSlide, addAssumptionFocusSlide, addStructuredSummarySlide };
