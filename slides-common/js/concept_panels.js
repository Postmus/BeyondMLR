// Reusable measured panels for short conceptual explanations and derivations.
const { COLORS, FONT_FAMILIES, LAYOUT, SPACING, STROKES, RADII } = require('./blr_theme');
const { addBase, addSlideTitle, addFooter, addNoteBox, addAssetFrame } = require('./pptx_layout_primitives');
const { calcTextBox } = require('../../lectures_2627/week1/pptxgenjs_helpers/text');
const { addTeachingTable } = require('./teaching_table');
const { addInlineMathParagraph } = require('./inline_math');

function addConceptPanelsSlide(pptx, { title, intro, lead, panels, takeaway, takeawayHeight = .82, drawEquation }) {
  const slide = pptx.addSlide();
  addBase(slide);
  addSlideTitle(slide, title);
  const body = { ...LAYOUT.body };
  if (intro) {
    slide.addText(intro, { x: body.x, y: body.y + .06, w: body.w, h: .48,
      fontFace: FONT_FAMILIES.sans, fontSize: 20, color: COLORS.muted, margin: 0 });
    body.y += .68;
    body.h -= .68;
  }
  if (lead) {
    // A shared equation and interpretation establish the comparison below.
    drawEquation(slide, lead.equation, body.x, body.y, body.w, .6);
    slide.addText(lead.text, { x: body.x, y: body.y + .7, w: body.w, h: .48,
      fontFace: FONT_FAMILIES.sans, fontSize: 20, color: COLORS.muted,
      align: 'center', margin: 0 });
    body.y += 1.3;
    body.h -= 1.3;
  }
  const noteH = takeaway ? takeawayHeight : 0;
  const panelH = body.h - (takeaway ? noteH + .16 : 0);
  const availableW = body.w - SPACING.gutter * (panels.length - 1);
  const totalWeight = panels.reduce((sum, panel) => sum + (panel.weight || 1), 0);
  let panelX = body.x;
  panels.forEach((panel, index) => {
    const panelW = availableW * (panel.weight || 1) / totalWeight;
    const box = { x: panelX, y: body.y, w: panelW, h: panelH };
    panelX += panelW + SPACING.gutter;
    // Backgrounds intentionally contain all panel content.
    slide.addShape('roundRect', { ...box, rectRadius: RADII.card,
      fill: { color: COLORS.card }, line: STROKES.card });
    const x = box.x + .28, w = box.w - .56;
    let y = box.y + .22;
    slide.addText(panel.title, { x, y, w, h: .35, fontFace: FONT_FAMILIES.sans,
      fontSize: 17, bold: true, color: COLORS.primary, margin: 0 });
    y += .58;
    for (const block of panel.blocks) {
      const h = block.h || (block.equation ? .55 : block.image ? 2.25 :
        calcTextBox(block.fontSize || 21, { text: block.text, w, fontFace: FONT_FAMILIES.sans,
          margin: 0, padding: .04 }).h + .07);
      if (block.equation) drawEquation(slide, block.equation, x, y, w, h);
      else if (block.inlineMath) addInlineMathParagraph(slide, { runs: block.inlineMath, box: { x, y, w, h }, fontSize: block.fontSize || 21 });
      else if (block.table) addTeachingTable(slide, { ...block.table, box: { x, y, w, h } });
      else if (block.image) addAssetFrame(slide, { box: { x, y, w, h }, assetPath: block.image, framed: false });
      else slide.addText(block.text, { x, y, w, h, fontFace: FONT_FAMILIES.sans,
        fontSize: block.fontSize || 21, color: block.muted ? COLORS.muted : COLORS.text,
        bold: !!block.bold, margin: 0, valign: 'top', breakLine: false });
      y += h + (block.gap === undefined ? .12 : block.gap);
    }
    if (y - .12 > box.y + box.h - .18) throw new Error(`Panel overflow: ${title} / ${panel.title} (${(y - .12 - box.y).toFixed(2)} in used; ${(box.h - .18).toFixed(2)} in available)`);
  });
  if (takeaway) addNoteBox(slide, { box: { x: body.x, y: body.y + body.h - noteH, w: body.w, h: noteH },
    text: takeaway, fontSize: 20, minFontSize: 20 });
  addFooter(slide);
  return slide;
}

function addConceptTableSlide(pptx, { title, rows, takeaway, drawEquation }) {
  const slide = pptx.addSlide();
  addBase(slide); addSlideTitle(slide, title);
  const widths = [2.15, 5.08, LAYOUT.body.w - 7.23];
  const headerH = .62, rowH = 1.42;
  let y = LAYOUT.body.y + .15;
  const text = (value, box, opts = {}) => slide.addText(value, {
    ...box, fontFace: FONT_FAMILIES.sans, fontSize: 20, color: COLORS.text,
    margin: 0, valign: 'mid', ...opts,
  });
  // Header and row backgrounds intentionally contain their cell content.
  slide.addShape('rect', { x: LAYOUT.body.x, y, w: LAYOUT.body.w, h: headerH,
    fill: { color: COLORS.primary }, line: { color: COLORS.primary } });
  let x = LAYOUT.body.x;
  ['CONCEPT', 'MEANING FOR THE ERRORS', 'TREATMENT-ONLY MODEL'].forEach((label, i) => {
    text(label, { x: x + .2, y: y + .1, w: widths[i] - .4, h: .42 },
      { fontSize: 15, bold: true, color: COLORS.onPrimary });
    x += widths[i];
  });
  y += headerH;
  rows.forEach((row, i) => {
    slide.addShape('rect', { x: LAYOUT.body.x, y, w: LAYOUT.body.w, h: rowH,
      fill: { color: COLORS.card }, line: STROKES.card });
    x = LAYOUT.body.x;
    text(row.concept, { x: x + .2, y: y + .2, w: widths[0] - .4, h: 1.0 }, { bold: true });
    x += widths[0];
    text(row.meaning, { x: x + .2, y: y + .2, w: widths[1] - .4, h: 1.0 });
    x += widths[1];
    text(row.assumption, { x: x + .2, y: y + .13, w: widths[2] - .4, h: .65 });
    drawEquation(slide, row.equation, x + .3, y + .88, widths[2] - .6, .35);
    y += rowH;
  });
  addNoteBox(slide, { box: { x: LAYOUT.body.x, y: 6.27, w: LAYOUT.body.w, h: .75 },
    text: takeaway, fontSize: 19, minFontSize: 19 });
  addFooter(slide);
  return slide;
}

module.exports = { addConceptPanelsSlide, addConceptTableSlide };
