const fs = require('fs/promises');
const path = require('path');

// PptxGenJS 4.0.1 emits the notes-master list after the slide list. The OOXML
// presentation sequence requires it before that list (and any handout list).
function normalizePresentationXml(xml) {
  const notes = xml.match(/<p:notesMasterIdLst>[\s\S]*?<\/p:notesMasterIdLst>/);
  const before = xml.search(/<p:(?:handoutMasterIdLst|sldIdLst|sldSz)\b/);
  if (!notes || before < 0 || notes.index < before) return xml;
  return xml.replace(notes[0], '').replace(
    /<p:(?:handoutMasterIdLst|sldIdLst|sldSz)\b/, tag => notes[0] + tag
  );
}

// The native slide-number placeholder uses a fixed ID of 25, which collides
// with content on dense slides. Preserve the field and change only its ID.
function normalizeSlideNumberIds(xml) {
  const counts = new Map();
  let maximum = 0;
  for (const match of xml.matchAll(/<p:cNvPr\b[^>]*\bid="(\d+)"/g)) {
    const id = Number(match[1]);
    counts.set(id, (counts.get(id) || 0) + 1);
    maximum = Math.max(maximum, id);
  }
  return xml.replace(/<p:cNvPr\b[^>]*\bname="Slide Number Placeholder[^\"]*"[^>]*>/g, tag => {
    const id = Number(tag.match(/\bid="(\d+)"/)[1]);
    if (counts.get(id) < 2) return tag;
    counts.set(id, counts.get(id) - 1);
    return tag.replace(/\bid="\d+"/, `id="${++maximum}"`);
  });
}

// PowerPoint 16 rejects schema-ordered notes-master lists when the PptxGenJS
// notes master shares its theme part with the slide master. Give the notes
// master a separate, identical theme; preserve all notes and slide formatting.
async function separateNotesTheme(zip) {
  const relFile = 'ppt/notesMasters/_rels/notesMaster1.xml.rels';
  if (!zip.file(relFile)) return false;
  const xml = await zip.file(relFile).async('string');
  const target = xml.match(/Type="[^\"]*\/theme"\s+Target="(\.\.\/theme\/[^\"]+)"/);
  if (!target) return false;
  const source = path.posix.normalize('ppt/notesMasters/' + target[1]);
  const slideRelFiles = Object.keys(zip.files).filter(name => /^ppt\/slideMasters\/_rels\/[^/]+\.rels$/.test(name));
  let shared = false;
  for (const file of slideRelFiles) {
    if ((await zip.file(file).async('string')).includes(`Target="${target[1]}"`)) shared = true;
  }
  if (!shared) return false;
  let index = 1;
  while (zip.file(`ppt/theme/themeNotes${index}.xml`)) index += 1;
  const destination = `ppt/theme/themeNotes${index}.xml`;
  zip.file(destination, await zip.file(source).async('nodebuffer'));
  zip.file(relFile, xml.replace(`Target="${target[1]}"`, `Target="../theme/themeNotes${index}.xml"`));
  const types = await zip.file('[Content_Types].xml').async('string');
  zip.file('[Content_Types].xml', types.replace('</Types>',
    `<Override PartName="/${destination}" ContentType="application/vnd.openxmlformats-officedocument.theme+xml"/></Types>`));
  return true;
}

// Keep narrowly scoped generator compatibility fixes in the shared exporter.
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
  let changed = cleaned !== types;
  if (cleaned !== types) {
    zip.file('[Content_Types].xml', cleaned);
  }
  const presentation = await zip.file('ppt/presentation.xml').async('string');
  const ordered = normalizePresentationXml(presentation);
  if (ordered !== presentation) {
    zip.file('ppt/presentation.xml', ordered);
    changed = true;
  }
  if (await separateNotesTheme(zip)) changed = true;
  for (const file of Object.keys(zip.files).filter(name => /^ppt\/slides\/slide\d+\.xml$/.test(name))) {
    const xml = await zip.file(file).async('string');
    const normalized = normalizeSlideNumberIds(xml);
    if (normalized !== xml) {
      zip.file(file, normalized);
      changed = true;
    }
  }
  if (changed) {
    await fs.writeFile(fileName, await zip.generateAsync({
      type: 'nodebuffer', compression: 'DEFLATE'
    }));
  }
}

module.exports = { writePresentation, normalizePresentationXml, normalizeSlideNumberIds, separateNotesTheme };
