const fs = require('node:fs');
const path = require('node:path');
const { pathToFileURL } = require('node:url');
const { spawnSync } = require('node:child_process');

const root = path.resolve(__dirname, '..');
const input = process.argv[2] && path.resolve(process.argv[2]);
if (!input || path.extname(input).toLowerCase() !== '.pptx' || !fs.existsSync(input)) {
  console.error('Usage: node scripts/render_lecture.js path/to/lecture.pptx [output-directory]');
  process.exit(1);
}
const output = path.resolve(process.argv[3] || path.join(root, 'output', 'pdf'));
const cache = path.join(root, '.cache', 'lecture-build');
fs.mkdirSync(cache, { recursive: true });
const working = fs.mkdtempSync(path.join(cache, 'render-'));
try {
  const runtime = path.join(working, 'runtime');
  fs.mkdirSync(runtime, { mode: 0o700 });
  const profile = pathToFileURL(path.join(working, 'profile')).href;
  const command = process.env.LIBREOFFICE_BIN || 'libreoffice';
  const result = spawnSync(command, [
    `-env:UserInstallation=${profile}`, '--headless', '--convert-to', 'pdf',
    '--outdir', working, input,
  ], {
    encoding: 'utf8', timeout: 120_000,
    env: { ...process.env, XDG_RUNTIME_DIR: runtime, GSETTINGS_BACKEND: 'memory', SAL_USE_VCLPLUGIN: 'svp' },
  });
  if (result.error) {
    const hint = result.error.code === 'ENOENT' ? ' Install LibreOffice or set LIBREOFFICE_BIN.' : '';
    throw new Error(`Cannot run ${command}: ${result.error.message}.${hint}`);
  }
  const pdfName = `${path.basename(input, path.extname(input))}.pdf`;
  const generated = path.join(working, pdfName);
  if (result.status !== 0 || !fs.existsSync(generated)) {
    throw new Error(`PDF export failed.\n${result.stderr || result.stdout || `Exit status: ${result.status}`}`);
  }
  fs.mkdirSync(output, { recursive: true });
  const destination = path.join(output, pdfName);
  fs.copyFileSync(generated, destination);
  console.log(`Rendered ${path.relative(root, destination)}`);
} catch (error) {
  console.error(error.message);
  process.exitCode = 1;
} finally {
  fs.rmSync(working, { recursive: true, force: true });
}
