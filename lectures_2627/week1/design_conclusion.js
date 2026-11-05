const { addConceptPanelsSlide } = require('../../slides-common/js/concept_panels');

function addDesignConclusion(pptx) {
  return addConceptPanelsSlide(pptx, {
    title: 'Let the analysis reflect the design',
    panels: [
      { title: 'BALANCE AND INFERENCE', weight: 3, blocks: [
        { text: 'Blocking or stratification creates treatment balance within groups.', fontSize: 20, h: .67, gap: .10 },
        { text: 'With equal treatment allocation within each group, differences between groups cancel from the treatment comparison.', fontSize: 20, h: 1.00, gap: .10 },
        { text: 'Accounting for these groups makes the estimated uncertainty reflect the balance built into the design.', fontSize: 20, h: .67, gap: .10 },
        { text: 'Overestimated standard errors make tests overly conservative: the type I error rate falls below the chosen significance level, and power to detect a treatment effect is reduced.', fontSize: 20, h: 1.35 }
      ] },
      { title: 'THE SAME PRINCIPLE', weight: 2, blocks: [
        { table: {
          columns: [
            { label: 'SETTING', weight: 1.1 },
            { label: 'FACTOR', weight: 1 }
          ],
          rows: [
            ['Rehabilitation trial', 'Center'],
            ['Laboratory experiment', 'Tissue donor'],
            ['Stratified clinical trial', 'Disease severity or sex']
          ],
          fontSize: 19,
          headerFontSize: 14,
          headerHeight: .6,
          firstColumnBold: false
        }, h: 4.0 }
      ] }
    ],
    takeaway: 'Account for blocking and stratification factors in the analysis so that the estimated uncertainty reflects how treatment was allocated.'
  });
}

module.exports = { addDesignConclusion };
