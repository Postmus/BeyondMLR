const { addConceptPanelsSlide } = require('../../slides-common/js/concept_panels');
const { addTeachingTableSlide } = require('../../slides-common/js/teaching_table');

function addAllocationImbalance(pptx, data) {
  const f = value => Number(value).toFixed(2);
  const text = (value, h, options = {}) => ({ text: value, h, fontSize: 22, ...options });
  const panels = (title, content, takeaway) => addConceptPanelsSlide(pptx, {
    title, panels: content, takeaway
  });
  const results = data.results.filter(row => row.design === 'Unequal protocol proportions');

  const allocationPanel = (title, allocation) => ({ title, titleHeight: .30, blocks: [
      { table: {
        columns: [
          { label: 'Donor', weight: 1.2 },
          { label: 'Standard', weight: 1.8, align: 'center' },
          { label: 'Modified', weight: 1.8, align: 'center' }
        ],
        rows: [...allocation.map(row => [Number(row.donor.replace('Donor ', '')), row.standard, row.modified]),
          ['Total', allocation.reduce((s, r) => s + r.standard, 0), allocation.reduce((s, r) => s + r.modified, 0)]],
        fontSize: 18, headerFontSize: 18, headerHeight: .48, cellPaddingY: .01, firstColumnBold: false
      }, h: 4.05 }
    ] });
  // Assignment 1 split each donor’s aliquots equally between the two protocols.
  const originalAllocation = data.allocation.map(row => ({
    donor: row.donor,
    standard: (row.standard + row.modified) / 2,
    modified: (row.standard + row.modified) / 2
  }));
  panels('Changing protocol allocation within donors', [
    allocationPanel('ASSIGNMENT 1 ALLOCATION', originalAllocation),
    allocationPanel('HYPOTHETICAL ALLOCATION', data.allocation)
  ], 'Only allocation changes: the true protocol effect remains 6 µg/mg in every donor.');

  addTeachingTableSlide(pptx, {
    title: 'The models now give different protocol estimates',
    intro: 'Unequal allocation within donors; estimated modified-minus-standard difference in protein yield.',
    introHeight: .55,
    columns: [
      { label: 'Model', weight: 3.3 },
      { label: 'Estimate (µg/mg)', weight: 1.5, align: 'center' },
      { label: 'Estimated SE (µg/mg)', weight: 1.8, align: 'center' }
    ],
    rows: results.map(row => [row.model, f(row.estimate), f(row.se)]),
    fontSize: 24,
    takeaway: 'Accounting for donor changes the estimate. The fixed and random donor models give slightly different estimates, with similar estimated standard errors.'
  });

  panels('The models use different comparisons', [
    { title: 'PROTOCOL ONLY', blocks: [
      text('Compare Standard and Modified across all aliquots.', 1.00, { fontSize: 20, bullet: true, gap: .12 }),
      text('The groups may contain different mixtures of donors, so their difference also reflects donor baselines.', 2.10, { fontSize: 20, bullet: true })
    ] },
    { title: 'FIXED DONOR EFFECTS', blocks: [
      text('Compare Standard and Modified within each donor.', 1.00, { fontSize: 20, bullet: true, gap: .12 }),
      text('Aliquots in each comparison share the same donor baseline.', 2.10, { fontSize: 20, bullet: true })
    ] },
    { title: 'RANDOM DONOR EFFECTS', blocks: [
      text('Within donors: compare Standard and Modified in the same donor.', 1.00, { fontSize: 20, bullet: true, gap: .12 }),
      text('Between donors: relate average yield to the proportion assigned to Modified.', 1.70, { fontSize: 20, bullet: true, gap: .17 }),
      text('Both sources contribute to the protocol-effect estimate.', 1.00, { fontSize: 20 })
    ] }
  ], 'So far: the same protocol effect in every donor. What if protocol effects vary?')
    .addNotes('');
}

function addAllocationPrecisionBackup(pptx, data) {
  const f = value => Number(value).toFixed(2);
  const text = (value, h, options = {}) => ({ text: value, h, fontSize: 22, ...options });
  const panels = (title, content, takeaway) => addConceptPanelsSlide(pptx, {
    title, panels: content, takeaway
  });
  const results = data.results.filter(row => row.design === 'Unequal protocol proportions');
  const fixed = results.find(row => row.model === 'Fixed donor effects');
  const random = results.find(row => row.model === 'Random donor effects');
  panels('Why are the estimated standard errors almost identical?', [
    { title: 'MOST PRECISION COMES FROM WITHIN DONORS', titleHeight: .52, blocks: [
      text('Both models compare the two protocols within each donor.', .80, { fontSize: 21, gap: .20 }),
      text('These comparisons are informative because the protocol groups share the same donor baseline. Baseline differences between donors do not obscure these comparisons.', 1.55, { fontSize: 21, gap: .20 }),
      text('Both models obtain most of their precision from this same source.', .80, { fontSize: 21 })
    ] },
    { title: 'BETWEEN DONORS: LITTLE EXTRA PRECISION HERE', titleHeight: .52, blocks: [
      text('The random-intercept model can also use average outcomes from donors with different treatment proportions.', .95, { fontSize: 21, gap: .18 }),
      text('Donors differ substantially in baseline outcomes. A higher donor average could reflect its baseline as well as its treatment allocation.', 1.25, { fontSize: 21, gap: .18 }),
      text('Under the random-intercept assumptions, this noisy evidence adds little precision: the estimated standard error barely decreases.', 1.25, { fontSize: 21 })
    ] }
  ], `The additional evidence shifts the estimate slightly, but adds little precision: the estimated standard errors are ${f(fixed.se)} and ${f(random.se)} µg/mg.`);
}

module.exports = { addAllocationImbalance, addAllocationPrecisionBackup };
