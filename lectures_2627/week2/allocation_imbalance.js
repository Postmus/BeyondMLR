const { addConceptPanelsSlide } = require('../../slides-common/js/concept_panels');
const { addTeachingTableSlide } = require('../../slides-common/js/teaching_table');

function addAllocationImbalance(pptx, data) {
  const f = value => Number(value).toFixed(2);
  const text = (value, h, options = {}) => ({ text: value, h, fontSize: 22, ...options });
  const panels = (title, content, takeaway) => addConceptPanelsSlide(pptx, {
    title, panels: content, takeaway
  });
  const results = data.results.filter(row => row.design === 'Unequal treatment proportions');

  const allocationPanel = (title, allocation) => ({ title, blocks: [
      { table: {
        columns: [
          { label: 'Center', weight: 1.2 },
          { label: 'Standard\ncare', weight: 1.8, align: 'center' },
          { label: 'Enhanced\ncare', weight: 1.8, align: 'center' }
        ],
        rows: [...allocation.map(row => [row.center.replace('Center ', ''), row.standard, row.enhanced]),
          ['Total', allocation.reduce((s, r) => s + r.standard, 0), allocation.reduce((s, r) => s + r.enhanced, 0)]],
        fontSize: 21, headerFontSize: 18, headerHeight: .72, firstColumnBold: false
      }, h: 3.94 }
    ] });
  // Week 1 split the same center totals equally between the two treatments.
  const originalAllocation = data.allocation.map(row => ({
    center: row.center,
    standard: (row.standard + row.enhanced) / 2,
    enhanced: (row.standard + row.enhanced) / 2
  }));
  panels('Changing treatment allocation within centers', [
    allocationPanel('ORIGINAL ALLOCATION', originalAllocation),
    allocationPanel('UNEQUAL ALLOCATION', data.allocation)
  ], 'Only allocation changes: the underlying treatment effect remains 10 m in every center.');

  addTeachingTableSlide(pptx, {
    title: 'The models now give different treatment estimates',
    intro: 'Unequal allocation within centers; estimated enhanced-minus-standard difference in walk distance.',
    introHeight: .55,
    columns: [
      { label: 'Model', weight: 3.3 },
      { label: 'Estimate (m)', weight: 1.5, align: 'center' },
      { label: 'Estimated SE (m)', weight: 1.8, align: 'center' }
    ],
    rows: results.map(row => [row.model, f(row.estimate), f(row.se)]),
    fontSize: 24,
    takeaway: 'Accounting for center changes the estimate substantially. The two center models give slightly different estimates, with almost identical estimated standard errors.'
  });

  panels('The models use different comparisons', [
    { title: 'TREATMENT ONLY', blocks: [
      text('The model compares the average outcome of all enhanced-care patients with that of all standard-care patients.', 1.55, { fontSize: 20, gap: .25 }),
      text('Unequal allocation gives the groups different mixtures of centers. Their difference reflects both treatment and differences in center baselines.', 2.10, { fontSize: 20 })
    ] },
    { title: 'FIXED CENTER EFFECTS', blocks: [
      text('The model allows each center to have its own baseline outcome.', 1.10, { fontSize: 20, gap: .25 }),
      text('It estimates treatment by combining comparisons between enhanced and standard care within the same center.', 1.55, { fontSize: 20, gap: .20 }),
      text('These comparisons remove the shared center baseline.', .85, { fontSize: 20 })
    ] },
    { title: 'RANDOM CENTER EFFECTS', blocks: [
      text('The model uses comparisons both within and between centers.', 1.05, { fontSize: 20, gap: .15 }),
      text('Do centers with more enhanced care have higher average outcomes?', 1.05, { fontSize: 20, gap: .15 }),
      text('Using between-center information assumes baseline center effects are unrelated to treatment allocation.', 1.40, { fontSize: 20 })
    ] }
  ], 'The random-intercept model also uses between-center information under additional assumptions, so its treatment estimate can differ.');

  panels('Fixed versus random center effects', [{ blocks: [
    { table: {
      columns: [
        { label: 'Design', weight: 3.7 },
        { label: 'Treatment estimates', weight: 2.3, align: 'center' },
        { label: 'Estimated standard errors', weight: 2.8, align: 'center' }
      ],
      rows: [
        ['Equal center sizes;\n50:50 allocation within each center', 'Identical', 'Identical'],
        ['Unequal center sizes;\n50:50 allocation within each center', 'Identical', 'Can differ'],
        ['Treatment proportions differ across centers', 'Can differ', 'Can differ']
      ], fontSize: 21, headerFontSize: 18, headerHeight: .62, firstColumnBold: false
    }, h: 3.00, gap: .28 },
    text('With 50:50 allocation within every center, all three models, including the treatment-only model, give the same treatment estimate, even when center sizes differ.', 1.00, { fontSize: 22 })
  ] }], 'So far, we have assumed the same treatment effect in every center. What changes when the treatment effect varies across centers?')
    .addNotes('Scope: Gaussian additive models with independent homoscedastic individual errors and a common treatment effect. The fully balanced row describes the standard analysis used in this lecture: fixed-center OLS versus random-intercept REML, with an interior positive estimate of center variance. Under these conditions, both fits estimate individual-error variance identically and give identical estimated treatment standard errors. This is a general balanced-design result, not a peculiarity of the Week 1 dataset. Other variance-estimation methods or a center-variance estimate at the zero boundary need not preserve exact equality. With unequal center sizes but a constant treatment proportion, the treatment estimator and its true variance remain identical, while estimates of individual-error variance can differ.');
}

function addAllocationPrecisionBackup(pptx, data) {
  const f = value => Number(value).toFixed(2);
  const text = (value, h, options = {}) => ({ text: value, h, fontSize: 22, ...options });
  const panels = (title, content, takeaway) => addConceptPanelsSlide(pptx, {
    title, panels: content, takeaway
  });
  const results = data.results.filter(row => row.design === 'Unequal treatment proportions');
  const fixed = results.find(row => row.model === 'Fixed center effects');
  const random = results.find(row => row.model === 'Random center effects');
  panels('Why are the estimated standard errors almost identical?', [
    { title: 'MOST PRECISION COMES FROM WITHIN CENTERS', titleHeight: .52, blocks: [
      text('Both models compare enhanced and standard care within each center.', .80, { fontSize: 21, gap: .20 }),
      text('These comparisons are informative because the treatment groups share the same center baseline. Baseline differences between centers do not obscure these comparisons.', 1.55, { fontSize: 21, gap: .20 }),
      text('Both models obtain most of their precision from this same source.', .80, { fontSize: 21 })
    ] },
    { title: 'BETWEEN CENTERS: LITTLE EXTRA PRECISION HERE', titleHeight: .52, blocks: [
      text('The random-intercept model can also use average outcomes from centers with different treatment proportions.', .95, { fontSize: 21, gap: .18 }),
      text('Centers differ substantially in baseline outcomes. A higher center average could reflect its baseline as well as its treatment allocation.', 1.25, { fontSize: 21, gap: .18 }),
      text('Under the random-intercept assumptions, this noisy evidence adds little precision: the estimated standard error barely decreases.', 1.25, { fontSize: 21 })
    ] }
  ], `The additional evidence shifts the estimate slightly, but adds little precision: the estimated standard errors are ${f(fixed.se)} and ${f(random.se)} m.`);
}

module.exports = { addAllocationImbalance, addAllocationPrecisionBackup };
