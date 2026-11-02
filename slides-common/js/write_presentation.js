const fs = require('fs/promises');
const path = require('path');

// PptxGenJS 4.0.1 can declare slide-master parts that it does not export.
// Remove only those dangling content-type declarations after writing the deck.
async function writePresentation(pptx, fileName) {
  // Each deck declares this dependency in its own package.json.
  const JSZip = require(require.resolve('jszip', {
    paths: [path.dirname(path.resolve(fileName))]
  }));
  await pptx.writeFile({ fileName });
  const zip = await JSZip.loadAsync(await fs.readFile(fileName));
  const types = await zip.file('[Content_Types].xml').async('string');
  const cleaned = types.replace(
    /<Override\b[^>]*\bPartName="(\/ppt\/slideMasters\/[^\"]+)"[^>]*\/>/g,
    (entry, part) => zip.file(part.slice(1)) ? entry : ''
  );
  if (cleaned !== types) {
    zip.file('[Content_Types].xml', cleaned);
    await fs.writeFile(fileName, await zip.generateAsync({
      type: 'nodebuffer', compression: 'DEFLATE'
    }));
  }
}

module.exports = { writePresentation };
