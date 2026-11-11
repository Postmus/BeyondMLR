const { COLORS, FONT_FAMILIES, LAYOUT, STROKES } = require('./blr_theme');
const { addBase, addSlideTitle, addFooter, addNoteBox } = require('./pptx_layout_primitives');

// Shared editable table for full slides and smaller conceptual panels.
function addTeachingTable(slide, { box, columns, rows, fontSize = 21, headerFontSize = 16, headerHeight = .55, firstColumnBold = true, cellPaddingY = .10, cellPaddingX = .18, columnGroups, groupHeaderHeight = .38, drawEquation }) {
  const groupH = columnGroups ? groupHeaderHeight : 0;
  const rowH = (box.h - headerHeight - groupH) / rows.length;
  const totalWeight = columns.reduce((sum, column) => sum + column.weight, 0);
  const widths = columns.map(column => box.w * column.weight / totalWeight);
  const drawRow = (values, y, h, header) => {
    // Each row background intentionally contains all cell text.
    slide.addShape('rect', { x: box.x, y, w: box.w, h,
      fill: { color: header ? COLORS.primary : COLORS.card }, line: STROKES.card });
    let x = box.x;
    values.forEach((value, i) => {
      if (!header && columns[i].highlight) {
        // Column highlighting intentionally sits inside the row background.
        slide.addShape('rect', { x, y, w: widths[i], h,
          fill: { color: COLORS.surfaceMuted }, line: STROKES.card });
      }
      if (value && typeof value === 'object' && value.equation) {
        if (!drawEquation) throw new Error('Equation table cells require drawEquation');
        const equationH = Math.min(value.h || .36, h - 2 * cellPaddingY);
        drawEquation(slide, value.equation, x + cellPaddingX, y + (h - equationH) / 2,
          widths[i] - 2 * cellPaddingX, equationH);
      } else slide.addText(String(value), { x: x + cellPaddingX, y: y + cellPaddingY, w: widths[i] - 2 * cellPaddingX, h: h - 2 * cellPaddingY,
        fontFace: FONT_FAMILIES.sans, fontSize: header ? headerFontSize : fontSize,
        bold: header || (firstColumnBold && i === 0), color: header ? COLORS.onPrimary : COLORS.text,
        align: columns[i].align || 'left', margin: 0, valign: 'mid' });
      x += widths[i];
    });
  };
  if (columnGroups) {
    if (columnGroups.reduce((sum, group) => sum + group.count, 0) !== columns.length) throw new Error('Table column groups must cover all columns');
    let x = box.x, first = 0;
    for (const group of columnGroups) {
      const w = widths.slice(first, first + group.count).reduce((sum, width) => sum + width, 0);
      if (group.label) slide.addText(group.label, { x, y: box.y, w, h: groupH,
        fontFace: FONT_FAMILIES.sans, fontSize: headerFontSize, bold: true,
        color: COLORS.primary, align: 'center', margin: 0, valign: 'mid' });
      x += w; first += group.count;
    }
  }
  drawRow(columns.map(column => column.label), box.y + groupH, headerHeight, true);
  rows.forEach((row, index) => drawRow(row, box.y + groupH + headerHeight + index * rowH, rowH, false));
}

// Editable teaching table with a short introduction and a shared conclusion.
function addTeachingTableSlide(pptx, { title, intro, introHeight = .48, columns, rows, takeaway, takeawayHeight = .90, fontSize = 21, tableOptions = {}, leadEquation, drawEquation }) {
  const slide = pptx.addSlide();
  addBase(slide); addSlideTitle(slide, title);
  const body = { ...LAYOUT.body };
  if (intro) {
    slide.addText(intro, { x: body.x, y: body.y + .06, w: body.w, h: introHeight,
      fontFace: FONT_FAMILIES.sans, fontSize: 20, color: COLORS.muted, margin: 0 });
    body.y += introHeight + .20; body.h -= introHeight + .20;
  }
  if (leadEquation) {
    drawEquation(slide, leadEquation, body.x, body.y, body.w, .55);
    body.y += .73; body.h -= .73;
  }
  const noteH = takeaway ? takeawayHeight : 0;
  addTeachingTable(slide, { box: { ...body, h: body.h - noteH - (takeaway ? .18 : 0) },
    columns, rows, fontSize, drawEquation, ...tableOptions });
  if (takeaway) addNoteBox(slide, { box: { x: body.x, y: body.y + body.h - noteH, w: body.w, h: noteH },
    text: takeaway, fontSize: 20, minFontSize: 20 });
  addFooter(slide);
  return slide;
}
module.exports = { addTeachingTable, addTeachingTableSlide };
