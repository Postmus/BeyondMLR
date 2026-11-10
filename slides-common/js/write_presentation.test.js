const test = require('node:test');
const assert = require('node:assert/strict');
const { normalizePresentationXml, normalizeSlideNumberIds, separateNotesTheme } = require('./write_presentation');
const JSZip = require('jszip');

test('notes master list precedes the handout and slide lists', () => {
  const master = '<p:sldMasterIdLst/>', notes = '<p:notesMasterIdLst><p:notesMasterId r:id="rId19"/></p:notesMasterIdLst>';
  const slides = '<p:sldIdLst><p:sldId id="256"/></p:sldIdLst>';
  const original = `<p:presentation>${master}<p:handoutMasterIdLst/>${slides}${notes}<p:sldSz/></p:presentation>`;
  const expected = `<p:presentation>${master}${notes}<p:handoutMasterIdLst/>${slides}<p:sldSz/></p:presentation>`;
  assert.equal(normalizePresentationXml(original), expected);
  assert.equal(normalizePresentationXml(expected), expected);
});

test('only a colliding native slide-number ID changes; fields and other shapes remain', () => {
  const xml = '<p:cNvPr id="25" name="Text 23"/><p:cNvPr id="40" name="Text 38"/>' +
    '<p:cNvPr id="25" name="Slide Number Placeholder 0"/><a:fld type="slidenum"><a:t>6</a:t></a:fld>';
  const result = normalizeSlideNumberIds(xml);
  assert.equal(result, xml.replace('id="25" name="Slide Number', 'id="41" name="Slide Number'));
  assert.equal(normalizeSlideNumberIds(result), result);
});

test('valid and unrelated XML remains unchanged', () => {
  const xml = '<p:cNvPr id="2" name="Text 0"/><p:cNvPr id="25" name="Slide Number Placeholder 0"/>';
  assert.equal(normalizeSlideNumberIds(xml), xml);
  assert.equal(normalizePresentationXml('<p:presentation><p:sldIdLst/></p:presentation>'), '<p:presentation><p:sldIdLst/></p:presentation>');
});

test('notes and slide masters get distinct, identical theme parts only when shared', async () => {
  const zip = new JSZip();
  const rel = '<Relationships><Relationship Id="rId1" Type="http://schemas.openxmlformats.org/officeDocument/2006/relationships/theme" Target="../theme/theme1.xml"/></Relationships>';
  zip.file('ppt/notesMasters/_rels/notesMaster1.xml.rels', rel);
  zip.file('ppt/slideMasters/_rels/slideMaster1.xml.rels', rel);
  zip.file('ppt/theme/theme1.xml', '<a:theme name="Course"/>');
  zip.file('[Content_Types].xml', '<Types></Types>');
  assert.equal(await separateNotesTheme(zip), true);
  assert.equal(await zip.file('ppt/theme/themeNotes1.xml').async('string'), '<a:theme name="Course"/>');
  assert.equal(await zip.file('ppt/slideMasters/_rels/slideMaster1.xml.rels').async('string'), rel);
  assert.match(await zip.file('ppt/notesMasters/_rels/notesMaster1.xml.rels').async('string'), /themeNotes1\.xml/);
  assert.match(await zip.file('[Content_Types].xml').async('string'), /PartName="\/ppt\/theme\/themeNotes1.xml"/);
  assert.equal(await separateNotesTheme(zip), false);
});
