const { COLORS, FONT_FAMILIES, LAYOUT, STROKES } = require('./blr_theme');
const { addBase, addSlideTitle, addFooter, addNoteBox } = require('./pptx_layout_primitives');

// Shared editable table for full slides and smaller conceptual panels.
function addTeachingTable(slide, { box, columns, rows, fontSize = 21, headerFontSize = 16, headerHeight = .55, firstColumnBold = true }) {
  const rowH = (box.h - headerHeight) / rows.length;
  const totalWeight = columns.reduce((sum, column) => sum + column.weight, 0);
  const widths = columns.map(column => box.w * column.weight / totalWeight);
  const drawRow = (values, y, h, header) => {
    // Each row background intentionally contains all cell text.
    slide.addShape('rect', { x: box.x, y, w: box.w, h,
      fill: { color: header ? COLORS.primary : COLORS.card }, line: STROKES.card });
    let x = box.x;
    values.forEach((value, i) => {
      slide.addText(String(value), { x: x + .18, y: y + .10, w: widths[i] - .36, h: h - .20,
        fontFace: FONT_FAMILIES.sans, fontSize: header ? headerFontSize : fontSize,
        bold: header || (firstColumnBold && i === 0), color: header ? COLORS.onPrimary : COLORS.text,
        align: columns[i].align || 'left', margin: 0, valign: 'mid' });
      x += widths[i];
    });
  };
  drawRow(columns.map(column => column.label), box.y, headerHeight, true);
  rows.forEach((row, index) => drawRow(row, box.y + headerHeight + index * rowH, rowH, false));
}

// Editable teaching table with a short introduction and a shared conclusion.
function addTeachingTableSlide(pptx, { title, intro, columns, rows, takeaway, fontSize = 21, leadEquation, drawEquation }) {
  const slide = pptx.addSlide();
  addBase(slide); addSlideTitle(slide, title);
  const body = { ...LAYOUT.body };
  if (intro) {
    slide.addText(intro, { x: body.x, y: body.y + .06, w: body.w, h: .48,
      fontFace: FONT_FAMILIES.sans, fontSize: 20, color: COLORS.muted, margin: 0 });
    body.y += .68; body.h -= .68;
  }
  if (leadEquation) {
    drawEquation(slide, leadEquation, body.x, body.y, body.w, .55);
    body.y += .73; body.h -= .73;
  }
  const noteH = takeaway ? .90 : 0;
  addTeachingTable(slide, { box: { ...body, h: body.h - noteH - (takeaway ? .18 : 0) },
    columns, rows, fontSize });
  if (takeaway) addNoteBox(slide, { box: { x: body.x, y: body.y + body.h - noteH, w: body.w, h: noteH },
    text: takeaway, fontSize: 20, minFontSize: 20 });
  addFooter(slide);
  return slide;
}
module.exports = { addTeachingTable, addTeachingTableSlide };
