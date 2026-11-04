// Native slide text with individually typeset, baseline-aligned math symbols.
const { Canvas } = require('skia-canvas');
const { FONT_FAMILIES, COLORS } = require('./blr_theme');
const { getImageDimensions } = require('../../lectures_2627/week1/pptxgenjs_helpers/image');

function addInlineMathParagraph(slide, { runs, box, fontSize = 21 }) {
  const context = new Canvas(2, 2).getContext('2d');
  context.font = `${fontSize * 96 / 72}px "${FONT_FAMILIES.sans}"`;
  const measure = text => context.measureText(text).width / 96;
  const em = fontSize / 72;
  const lineHeight = em * 1.2;
  const baseline = em * 1.02;
  const tokens = runs.flatMap(run => {
    if (run.image) {
      const h = em * (run.heightEm || 1.3);
      return [{ ...run, h, w: h * getImageDimensions(run.image).aspectRatio }];
    }
    return run.text.split(/(\s+)/).filter(Boolean).map(text => ({ text, w: measure(text) }));
  });
  let line = [], width = 0, lineIndex = 0;
  function flush() {
    while (line.length && /^\s+$/.test(line[line.length - 1].text || '')) line.pop();
    let x = box.x, text = '', textWidth = 0;
    const y = box.y + lineIndex * lineHeight;
    function flushText() {
      if (!text) return;
      slide.addText(text, { x, y, w: textWidth, h: lineHeight,
        fontFace: FONT_FAMILIES.sans, fontSize, color: COLORS.text,
        margin: 0, valign: 'top', wrap: false, breakLine: false });
      x += textWidth;
      text = ''; textWidth = 0;
    }
    for (const token of line) {
      if (token.image) {
        flushText();
        slide.addImage({ path: token.image, x,
          y: y + baseline - token.h * (token.baselineFraction || .8),
          w: token.w, h: token.h });
        x += token.w;
      } else {
        text += token.text; textWidth += token.w;
      }
    }
    flushText();
    line = []; width = 0; lineIndex += 1;
  }
  for (const token of tokens) {
    if (width + token.w > box.w && line.length) flush();
    if (!line.length && /^\s+$/.test(token.text || '')) continue;
    line.push(token); width += token.w;
  }
  if (line.length) flush();
  if (lineIndex * lineHeight > box.h + .01) throw new Error('Inline math paragraph exceeds its allocated height.');
}

module.exports = { addInlineMathParagraph };
