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
    caption: 'Four illustrative entire homes from three neighborhoods in the Chicago Airbnb data.',
    captionFontSize: 17, captionMinFontSize: 17,
    takeaway: `Each neighborhood belongs to one district. The sample has ${r.listings.toLocaleString('en-US')} entire homes in ${r.neighborhoods} neighborhoods across ${r.districts} districts.`,
  });

  addFigureSlide(pptx, {
    title: 'Neighborhood mean log nightly prices by district',
    assetPath: asset('eda'),
    caption: 'Each circle shows one neighborhood mean. Larger circles represent more entire homes.',
    captionFontSize: 17, captionMinFontSize: 17,
    takeaway: 'Does the plot suggest that some districts tend to have higher neighborhood mean log prices than others?',
    takeawayHeight: .95,
  });

  addConceptPanelsSlide(pptx, {
    title: 'A random intercept for district and neighborhood',
    intro: 'Lᵢⱼₖ is the log nightly price of home i in neighborhood j of district k.',
    panels: [
      { title: 'THREE-LEVEL NULL MODEL', blocks: [
        { image: asset('eq_model'), h: .75, gap: .42 },
        { text: 'β₀ is the overall mean log nightly price.', fontSize: 21 },
        { text: 'The West district effect shifts the mean for every neighborhood in West. The Humboldt Park effect shifts its mean further up or down within West.', fontSize: 20 },
      ] },
      { title: 'RANDOM INTERCEPTS AND RESIDUAL', blocks: [
        { image: asset('eq_distributions'), h: 1.7, gap: .22 },
        { text: 'Listings in the same district share uₖ. Listings in the same neighborhood also share vⱼₖ.', fontSize: 20 },
      ] },
    ],
    takeaway: 'The model assumes the district effects, neighborhood deviations and individual listing errors are mutually independent.',
    takeawayHeight: 1,
  });

  addTeachingTableSlide(pptx, {
    title: 'Neighborhood variation before and after accounting for district',
    intro: 'Both null models describe log nightly prices for the same entire homes as in Lab 5.',
    columns: [{ label: 'Source of variation', weight: 1.65 }, column('Neighborhood only (Lab 5)', 1.28),
      column('District and neighborhood', 1.4)],
    rows: [
      ['Between districts', '—', r.three.district.toFixed(4)],
      ['Between neighborhoods', r.two.neighborhood.toFixed(4), neighborhood.toFixed(4) + ' (within districts)'],
      ['Between listings within a neighborhood', r.two.Residual.toFixed(4), r.three.Residual.toFixed(4)],
      ['Total variance', (r.two.neighborhood + r.two.Residual).toFixed(4), total.toFixed(4)],
    ],
    takeaway: `In the three-level model, differences between districts account for about ${Math.round(100 * r.three.district / (r.three.district + neighborhood))}% of the variation between neighborhood means: ${r.three.district.toFixed(4)} / (${r.three.district.toFixed(4)} + ${neighborhood.toFixed(4)}) ≈ ${(r.three.district / (r.three.district + neighborhood)).toFixed(2)}`,
    takeawayHeight: 1.20,
  });

  const districtVariance = r.three.district.toFixed(4);
  const totalVariance = total.toFixed(4);
  addConceptPanelsSlide(pptx, {
    title: 'Correlations between log nightly prices of Chicago listings',
    panels: [{ blocks: [
      { image: hierarchy, h: 2.63, gap: .22 },
      { h: 2.55, table: {
        columns: [{ label: 'Listings', weight: .75 }, { label: 'Shared effects', weight: 1.95 },
          column('Correlation = shared variance / total variance', 3.3)],
        rows: [
          ['A and B', 'West and Humboldt Park',
            `(${districtVariance} + ${neighborhood.toFixed(4)}) / ${totalVariance} ≈ ${((r.three.district + neighborhood) / total).toFixed(3)}`],
          ['A and C', 'West district only',
            `${districtVariance} / ${totalVariance} ≈ ${(r.three.district / total).toFixed(3)}`],
          ['A and D', 'None', `0 / ${totalVariance} = 0`],
        ],
        fontSize: 20, headerFontSize: 16, cellPaddingX: .12,
      } },
    ] }],
  });

  const predictorLabels = {
    capacity_c: 'Guest capacity (per additional guest place)',
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
      column('Add guest capacity', 1.48), column('Then add WalkScore', 1.12)],
    rows: [
      ['Between districts', 'district'],
      ['Between neighborhoods within a district', 'neighborhood:district'],
      ['Between listings within a neighborhood', 'Residual'],
    ].map(([label, level]) => [label, ...[r.three, r.three_listing, r.three_adjusted].map(model => model[level].toFixed(4))]),
    tableOptions: { headerHeight: .76, headerFontSize: 17 },
    takeaway: 'With guest capacity and WalkScore in the model, the estimated district variance is zero. Adding a district intercept therefore leaves the capacity and WalkScore estimates and their SEs unchanged to the precision shown on slide 31.',
    takeawayHeight: 1.32,
  });

  addFigureSlide(pptx, {
    title: 'Neighborhood WalkScores across Chicago districts',
    assetPath: asset('walkscores'),
    takeaway: 'Districts with higher average WalkScores tend to have higher mean log prices. After accounting for capacity and WalkScore, the model estimates zero remaining district variance in log price.',
    takeawayHeight: 1.05,
  });
};
