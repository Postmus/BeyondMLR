// Optional regeneration of the supplied equation assets; requires TeX and Poppler.
const fs = require('fs');
const os = require('os');
const path = require('path');
const cp = require('child_process');
const crypto = require('crypto');
const execFile = require('util').promisify(cp.execFile);
const { slides } = require('./error_interlude');
const initialModelEquations = require('./initial_model');
const { equations: recapEquations } = require('./error_recap');
const { equations: fixedCenterEquations } = require('./fixed_center');
const { equations: samplingEquations } = require('./fixed_center_sampling');
const { equations: randomSamplingEquations } = require('./random_center_sampling');
const { equations: randomModelEquations } = require('./random_center_model');
const { equations: lab2Equations } = require('./lab2_debrief');
const { latexToSvgDataUri } = require('./pptxgenjs_helpers/latex');
const dir = path.join(__dirname, 'assets', 'equations');
const temp = fs.mkdtempSync(path.join(os.tmpdir(), 'blr-interlude-equations-'));
async function main() {
try {
  const equations = new Set(slides.flatMap(slide => slide.panels.flatMap(panel =>
    panel.blocks.filter(block => block.equation).map(block => block.equation))));
  Object.values(initialModelEquations).forEach(tex => equations.add(tex));
  recapEquations.forEach(tex => equations.add(tex));
  fixedCenterEquations.forEach(tex => equations.add(tex));
  samplingEquations.forEach(tex => equations.add(tex));
  randomSamplingEquations.forEach(tex => equations.add(tex));
  randomModelEquations.forEach(tex => equations.add(tex));
  lab2Equations.forEach(tex => equations.add(tex));
  for (const tex of equations) {
    const hash = crypto.createHash('sha1').update(tex).digest('hex').slice(0,12);
    const stem = 'eq_' + hash;
    if (fs.existsSync(path.join(dir, stem + '.png'))) continue;
    const source = '\\documentclass[border=1pt]{standalone}\n\\usepackage{amsmath,amssymb}\n\\begin{document}\n$\\displaystyle ' + tex + '$\n\\end{document}\n';
    fs.writeFileSync(path.join(dir, stem + '.tex'), source);
    fs.writeFileSync(path.join(temp, stem + '.tex'), source);
    fs.writeFileSync(path.join(dir, 'mathjax_' + hash + '.svg'),
      Buffer.from(latexToSvgDataUri(tex).split(',')[1], 'base64'));
    await execFile('pdflatex', ['-interaction=batchmode', '-halt-on-error', '-output-directory', temp, path.join(temp, stem + '.tex')]);
    await execFile('pdftoppm', ['-png', '-singlefile', '-r', '600', path.join(temp, stem + '.pdf'), path.join(dir, stem)]);
  }
  console.log(`Checked ${equations.size} opening and interlude equation assets.`);
} finally {
  fs.rmSync(temp, { recursive: true, force: true });
}
}
main().catch(error => { console.error(error); process.exitCode = 1; });
