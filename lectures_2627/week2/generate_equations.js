// Optional regeneration of cached equations, matching the Week 1 TeX workflow.
const fs = require('fs');
const os = require('os');
const path = require('path');
const crypto = require('crypto');
const execFile = require('util').promisify(require('child_process').execFile);
const { equations } = require('./fixed_interaction');
const { latexToSvgDataUri } = require('../week1/pptxgenjs_helpers/latex');
const dir = path.join(__dirname, 'assets/equations');

async function main() {
  fs.mkdirSync(dir, { recursive: true });
  const temp = fs.mkdtempSync(path.join(os.tmpdir(), 'blr-week2-equations-'));
  try {
    for (const tex of new Set(equations)) {
      const hash = crypto.createHash('sha1').update(tex).digest('hex').slice(0, 12);
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
    console.log(`Checked ${new Set(equations).size} Week 2 equation assets.`);
  } finally {
    fs.rmSync(temp, { recursive: true, force: true });
  }
}
main().catch(error => { console.error(error); process.exitCode = 1; });
