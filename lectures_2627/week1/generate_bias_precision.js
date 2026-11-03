// Four schematic dartboards: columns compare spread, rows compare centering.
// Contract: 1464 x 718 canvas matches the slide body; equal-radius targets,
// aligned row/column labels, a shared key above and equal-weight definitions below.
const fs = require('fs');
const path = require('path');
const { Canvas, loadImage } = require('skia-canvas');
const { COLORS, FONT_FAMILIES } = require('../../slides-common/js/blr_theme');

async function main() {
  const width = 1464, height = 718;
  const radius = 108;
  const columns = [580, 1110], rows = [244, 493];
  const svg = [];
  const color = name => '#' + COLORS[name];
  const text = (x, y, value, { size = 32, bold = false, anchor = 'start' } = {}) =>
    svg.push(`<text x="${x}" y="${y}" font-size="${size}" font-weight="${bold ? 700 : 400}" text-anchor="${anchor}">${value}</text>`);
  const circle = (x, y, r, fill, stroke, sw = 2) =>
    svg.push(`<circle cx="${x}" cy="${y}" r="${r}" fill="${fill}" stroke="${stroke}" stroke-width="${sw}"/>`);
  svg.push(`<svg xmlns="http://www.w3.org/2000/svg" width="${width}" height="${height}" viewBox="0 0 ${width} ${height}">`,
    `<rect width="${width}" height="${height}" fill="${color('card')}"/>`,
    `<g font-family="${FONT_FAMILIES.sans}" fill="${color('text')}">`);
  text(28, 36, 'Bullseye: true treatment effect', { size: 30 });
  text(700, 36, 'Each dart: estimate from one simulated study', { size: 30 });
  ['High precision', 'Low precision'].forEach((label, i) =>
    text(columns[i], 102, label, { bold: true, size: 35, anchor: 'middle' }));
  ['No bias', 'Bias'].forEach((label, i) => text(206, rows[i] + 12, label,
    { bold: true, size: 35, anchor: 'middle' }));

  // Symmetric pairs give exactly the same cluster mean in each precision column.
  // Scale only the spread between columns and translate only the mean between rows.
  const half = [[1.1, .1], [.65, .85], [-.2, 1.25], [-.9, .6]];
  const offsets = half.flatMap(([x, y]) => [[x, y], [-x, -y]]);
  for (let row = 0; row < 2; row++) for (let col = 0; col < 2; col++) {
    const cx = columns[col], cy = rows[row];
    for (let ring = 4; ring >= 1; ring--) circle(cx, cy, radius * ring / 4,
      ring % 2 ? color('surfaceCool') : color('card'), color('cardLine'), 2.5);
    circle(cx, cy, 7, color('primary'), color('primary'));
    const spread = col ? 39 : 16;
    const mean = row ? [46, -29] : [0, 0];
    for (const [dx, dy] of offsets) {
      const x = mean[0] + spread * dx, y = mean[1] + spread * dy;
      if (Math.hypot(x, y) + 6 > radius) throw Error('Dart outside target');
      // Crosses mark dart impact locations; the teal bullseye stays distinct.
      svg.push(`<path d="M ${cx+x-4} ${cy+y-4} l 8 8 m -8 0 l 8 -8" stroke="${color('accent')}" stroke-width="4" stroke-linecap="round"/>`);
    }
  }
  text(28, 652, 'Bias: difference between the average estimate and the true treatment effect.', { size: 32 });
  text(28, 697, 'Precision: how tightly the estimates cluster across repeated studies.', { size: 32 });
  svg.push('</g></svg>');
  const out = path.join(__dirname, 'assets', 'bias-precision');
  const source = svg.join('\n');
  fs.writeFileSync(out + '.svg', source);
  const canvas = new Canvas(width * 2, height * 2);
  canvas.getContext('2d').drawImage(await loadImage(Buffer.from(source)), 0, 0, width * 2, height * 2);
  await canvas.toFile(out + '.png');
  console.log('Generated bias-precision.svg and .png');
}
main().catch(error => { console.error(error); process.exit(1); });
