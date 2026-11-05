const { addConceptPanelsSlide } = require('../../slides-common/js/concept_panels');
const { addStructuredSummarySlide } = require('../../slides-common/js/course_builders');

function addGroupingSummary(pptx) {
  addConceptPanelsSlide(pptx, {
    title: 'Summary: Grouped data and statistical inference',
    panels: [
      { title: 'GROUPED OBSERVATIONS', blocks: [
        { text: 'Grouped data arise when multiple observations come from the same unit or setting, which may contribute to their outcomes.' },
        { text: 'EXAMPLES', fontSize: 17, bold: true, gap: .10 },
        { text: 'Patients treated at the same center.' },
        { text: 'Pupils attending the same school.' },
        { text: 'Repeated measurements from the same person.' }
      ] },
      { title: 'THE SETTING WE CONSIDERED', blocks: [
        { text: 'Groups differ in their expected outcome, while the effect of the explanatory variable is the same across groups (no interaction between group and the explanatory variable).' },
        { text: 'After accounting for the explanatory variable and the group effects, the remaining errors are independent and identically distributed.' }
      ] }
    ],
    takeaway: 'These group contributions need to be accounted for to obtain appropriate statistical inference.'
  });

  addPerspectiveSummary(pptx, {
    title: 'Summary: Viewing group effects as fixed',
    panels: [
      { title: 'HOW WE VIEW THE GROUPS', blocks: [
        { text: 'We want to account for differences between the specific groups in our study, without generalizing those differences to a wider population of groups. The individual group effects may themselves be of interest.' },
        { text: 'HOW WE ACCOUNT FOR THEM', fontSize: 17, bold: true, gap: .18 },
        { text: 'Include a separate fixed effect for each group to represent its contribution to the expected outcome.' }
      ] },
      { title: 'WHAT HAPPENS IF WE IGNORE THEM?', blocks: [
        { text: 'The errors remain independent, but have different means across groups. They are therefore no longer identically distributed.' },
        { text: 'When the explanatory variable is unevenly distributed across groups, group differences can enter the estimated effect and introduce bias.' },
        { text: 'Even when group contributions cancel in the comparison and the estimate remains unbiased, its usual standard error can be incorrect.' }
      ] }
    ],
    takeaway: 'Ignoring fixed group differences can affect both the estimated effect and its estimated uncertainty.'
  });

  addPerspectiveSummary(pptx, {
    title: 'Summary: Viewing group effects as random',
    panels: [
      { title: 'HOW WE VIEW THE GROUPS', blocks: [
        { text: 'We consider the observed groups as examples from a wider population. Their contributions to the expected outcome vary randomly across groups.' },
        { text: 'The standard random-intercept model assumes that these group effects have mean zero and are unrelated to the explanatory variable.' },
        { text: 'HOW WE ACCOUNT FOR THEM', fontSize: 17, bold: true, gap: .18 },
        { text: 'Use a random intercept to model the variation between groups and the resulting dependence within groups.' }
      ] },
      { title: 'WHAT HAPPENS IF WE IGNORE THEM?', blocks: [
        { text: 'The errors have the same distribution, but are dependent within groups because observations share a random group effect.' },
        { text: 'OLS remains unbiased when averaging over the random group effects, even when the explanatory variable is unevenly distributed across groups.' },
        { text: 'Its usual standard error can nevertheless be incorrect because it assumes independent errors.' }
      ] }
    ],
    takeaway: 'Ignoring grouping can invalidate standard errors, confidence intervals, and tests, even when the estimated effect itself remains appropriate.'
  });
}

function addPerspectiveSummary(pptx, { title, panels, takeaway }) {
  const view = panels[0].blocks;
  addStructuredSummarySlide(pptx, {
    title,
    sections: [
      { title: 'HOW WE VIEW THE GROUPS', paragraphs: [view.slice(0, -2).map(block => block.text).join(' ')] },
      { title: 'WHAT HAPPENS IF WE IGNORE THEM?', paragraphs: panels[1].blocks.map(block => block.text) },
      { title: 'HOW WE ACCOUNT FOR THEM', paragraphs: [view.at(-1).text] }
    ],
    takeaway
  });
}

module.exports = { addGroupingSummary };
