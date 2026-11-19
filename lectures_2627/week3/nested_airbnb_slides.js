const path = require('node:path');
const { addSectionSlide, addFigureSlide } = require('../../slides-common/js/pptx_layout_primitives');
const { addConceptPanelsSlide } = require('../../slides-common/js/concept_panels');
const { addTeachingTableSlide } = require('../../slides-common/js/teaching_table');
const r = require('./assets/nested_airbnb_results.json');

module.exports = function addNestedAirbnbSlides(pptx) {
  const asset = name => path.join(__dirname, 'assets', 'nested_airbnb_' + name + '.png');
  const neighborhood = r.three['neighborhood:district'];
  const total = r.three.district + neighborhood + r.three.Residual;
  const column = (label, weight = 1) => ({ label, weight, align: 'center' });
  const hierarchy = asset('hierarchy');

  addSectionSlide(pptx, { sectionLabel: 'PART 4', sectionTitle: 'Nested random effects' });

  addFigureSlide(pptx, {
    title: 'Chicago neighborhoods are nested within districts',
    assetPath: hierarchy,
    caption: 'Four illustrative listings from three neighborhoods in the Chicago Airbnb data.',
    captionFontSize: 17, captionMinFontSize: 17,
    takeaway: `Each neighborhood belongs to one district. The sample has ${r.listings.toLocaleString('en-US')} listings in ${r.neighborhoods} neighborhoods across ${r.districts} districts.`,
  });

  addFigureSlide(pptx, {
    title: 'Neighborhood mean log nightly prices by district',
    assetPath: asset('eda'),
    caption: 'Each circle shows one neighborhood mean. Larger circles represent more listings.',
    captionFontSize: 17, captionMinFontSize: 17,
    takeaway: 'Does the plot suggest that some districts tend to have higher neighborhood mean log prices than others?',
    takeawayHeight: .95,
  });

  addConceptPanelsSlide(pptx, {
    title: 'A random intercept for district and neighborhood',
    intro: 'Lᵢⱼₖ is the log nightly price of listing i in neighborhood j of district k.',
    panels: [
      { title: 'THREE-LEVEL NULL MODEL', blocks: [
        { image: asset('eq_model'), h: .75, gap: .42 },
        { text: 'β₀ is the overall mean log nightly price.', fontSize: 21 },
        { text: 'The expected log price in Humboldt Park is the overall mean plus the West district effect and the Humboldt Park neighborhood effect.', fontSize: 20 },
      ] },
      { title: 'RANDOM INTERCEPTS AND RESIDUAL', blocks: [
        { image: asset('eq_distributions'), h: 1.7, gap: .22 },
        { text: 'Listings in the same district share uₖ. Listings in the same neighborhood also share vⱼₖ.', fontSize: 20 },
      ] },
    ],
    takeaway: 'The model assumes independence across districts, across neighborhoods and across listing residuals, as well as between the three levels.',
    takeawayHeight: 1,
  });

  addTeachingTableSlide(pptx, {
    title: 'Neighborhood variation before and after accounting for district',
    intro: 'Both null models describe log nightly prices for the same Chicago listings.',
    columns: [{ label: 'Source of variation', weight: 1.65 }, column('Neighborhood only (Lab 5)', 1.28),
      column('District and neighborhood', 1.4)],
    rows: [
      ['Between districts', '—', r.three.district.toFixed(4)],
      ['Between neighborhoods', r.two.neighborhood.toFixed(4), neighborhood.toFixed(4) + ' (within districts)'],
      ['Between listings within a neighborhood', r.two.Residual.toFixed(4), r.three.Residual.toFixed(4)],
      ['Total variance', (r.two.neighborhood + r.two.Residual).toFixed(4), total.toFixed(4)],
    ],
    takeaway: `In the three-level model, about two-thirds of the variance between neighborhood means lies between districts.\n${r.three.district.toFixed(4)} / (${r.three.district.toFixed(4)} + ${neighborhood.toFixed(4)}) ≈ ${Math.round(100 * r.three.district / (r.three.district + neighborhood))}%`,
    takeawayHeight: 1.20,
  });

  addFigureSlide(pptx, {
    title: 'Which pair of listings would you expect to be most alike?',
    assetPath: hierarchy,
    caption: 'Compare listing A with B, then with C, then with D.',
    captionFontSize: 19, captionMinFontSize: 19,
    takeaway: 'For each pair, identify the district and neighborhood effects the two listings share. How would you rank the three correlations?',
    takeawayHeight: .98,
  });

  addConceptPanelsSlide(pptx, {
    title: 'Two correlations from the same model',
    intro: 'For two distinct listings, the correlation in log nightly prices equals their shared variance divided by the total variance.',
    introHeight: .75,
    panels: [
      { title: 'A AND B · SAME NEIGHBORHOOD', blocks: [
        { text: 'Both listings are in Humboldt Park, so they share the West district effect and the Humboldt Park effect.', fontSize: 21 },
        { image: asset('eq_same_neighborhood'), h: .96 },
      ] },
      { title: 'A AND C · SAME DISTRICT', blocks: [
        { text: 'Humboldt Park and West Town are different neighborhoods, so these listings share only the West district effect.', fontSize: 21 },
        { image: asset('eq_same_district'), h: .96 },
      ] },
    ],
    takeaway: 'A and D are in different districts. Their shared variance is zero, so the model implies a correlation of zero.',
    takeawayHeight: 1,
  });

  const predictorLabels = {
    capacity_c: 'Guest capacity (per additional guest place)',
    'room_typePrivate room': 'Private room vs. entire home/apartment',
    'room_typeShared room': 'Shared room vs. entire home/apartment',
    walk10: 'WalkScore (per 10 points)',
  };
  addTeachingTableSlide(pptx, {
    title: 'Fixed-effect estimates for log nightly price',
    intro: 'The two-level model groups listings by neighborhood. The three-level model also groups neighborhoods by district.',
    introHeight: .74,
    columns: [{ label: 'Predictor', weight: 3.05 }, column('Estimate'), column('SE'), column('Estimate'), column('SE')],
    rows: r.two_fixed.map(({ term, estimate, se }) => {
      const three = r.three_fixed.find(row => row.term === term);
      return [predictorLabels[term], estimate.toFixed(4), se.toFixed(5), three.estimate.toFixed(4), three.se.toFixed(5)];
    }),
    tableOptions: { columnGroups: [
      { label: '', count: 1 }, { label: 'TWO LEVELS', count: 2 }, { label: 'THREE LEVELS', count: 2 },
    ], groupHeaderHeight: .5, headerFontSize: 18 },
  });

  addTeachingTableSlide(pptx, {
    title: 'Variation in log nightly price after adjustment',
    intro: 'Each model includes random intercepts for districts and for neighborhoods within districts.',
    columns: [{ label: 'Source of variation', weight: 1.8 }, column('Null model'),
      column('Add guest capacity and room type', 1.48), column('Then add WalkScore', 1.12)],
    rows: [
      ['Between districts', 'district'],
      ['Between neighborhoods within a district', 'neighborhood:district'],
      ['Between listings within a neighborhood', 'Residual'],
    ].map(([label, level]) => [label, ...[r.three, r.three_listing, r.three_adjusted].map(model => model[level].toFixed(4))]),
    tableOptions: { headerHeight: .76, headerFontSize: 17 },
    takeaway: `Adding WalkScore reduces the remaining district variance from ${r.three_listing.district.toFixed(4)} to ${r.three_adjusted.district.toFixed(4)}. With so little district variation left, adding a district intercept makes little difference to the estimates or their SEs in these Chicago data.`,
    takeawayHeight: 1.32,
  });
};
