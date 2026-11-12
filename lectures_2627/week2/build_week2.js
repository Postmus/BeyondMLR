const pptxgen = require('pptxgenjs');
const fs = require('fs');
const path = require('path');
const crypto = require('crypto');
const { FONT_FAMILIES } = require('../../slides-common/js/blr_theme');
const { defineTemplateMasters, addBrandedTitleSlide, addSectionSlide } = require('../../slides-common/js/pptx_layout_primitives');
const { writePresentation } = require('../../slides-common/js/write_presentation');
const { imageSizingContain } = require('../week1/pptxgenjs_helpers/image');
const { warnIfSlideHasOverlaps, warnIfSlideElementsOutOfBounds } = require('../week1/pptxgenjs_helpers/layout');
const { addWeek2Opening } = require('./fixed_interaction');
const { addCrossedEffectsIntroduction } = require('./crossed_intro');
const { addCrossedCaseStudy } = require('./crossed_case_study');
process.chdir(__dirname);
const pptx = new pptxgen();
pptx.layout = 'LAYOUT_WIDE';
pptx.author = 'Dr Douwe Postmus';
pptx.subject = 'Week 2: treatment interactions, random slopes and multiple grouping variables';
pptx.title = 'Beyond MLR | Week 2';
pptx.lang = 'en-GB';
pptx.theme = { headFontFace: FONT_FAMILIES.sans, bodyFontFace: FONT_FAMILIES.sans, lang: 'en-GB' };
defineTemplateMasters(pptx);
const read = file => JSON.parse(fs.readFileSync(file, 'utf8'));
const week1 = read('../week1/assets/model_results.json');
const results = read('assets/fixed-interaction/model_results.json');
const allocation = read('assets/allocation-imbalance/model_results.json');
const lab3 = read('assets/lab3-debrief/model_results.json');
const randomInteraction = read('assets/random-interaction/model_results.json');
function drawEquation(slide, tex, x, y, w, h) {
  const hash = crypto.createHash('sha1').update(tex).digest('hex').slice(0, 12);
  const file = path.join(__dirname, 'assets/equations', 'eq_' + hash + '.png');
  if (!fs.existsSync(file)) throw new Error('Missing equation; run npm run equations: ' + file);
  slide.addImage({ path: file, ...imageSizingContain(file, x, y, w, h) });
}
addBrandedTitleSlide(pptx, {
  courseLabel: 'BEYOND MULTIPLE LINEAR REGRESSION · WEEK 2',
  mainTitle: 'When treatment effects vary',
  subtitle: 'Interactions, random slopes and crossed random effects',
  author: 'Dr Douwe Postmus',
  affiliation: 'UMCG · Department of Epidemiology',
  badge: 'BLR'
});
addWeek2Opening(pptx, drawEquation, week1, results, allocation, lab3, randomInteraction);
addCrossedEffectsIntroduction(pptx);
addCrossedCaseStudy(pptx, drawEquation, read('assets/radiologists/model_results.json'));
addSectionSlide(pptx, { sectionLabel: 'The End', sectionTitle: 'Questions' });
pptx._slides.forEach((slide, index) => {
  if (!slide._slideObjects.some(object => object._type === 'notes')) slide.addNotes('');
  // The shared title layout has intentional decorative overlaps, as in Week 1.
  if (index > 0) warnIfSlideHasOverlaps(slide, pptx, { ignoreLines: true });
  warnIfSlideElementsOutOfBounds(slide, pptx);
});
writePresentation(pptx, path.join(__dirname, 'Beyond_MLR_Week_2.pptx'))
  .then(() => console.log(`Built Week 2 opening: ${pptx._slides.length} slides.`))
  .catch(error => { console.error(error); process.exitCode = 1; });
