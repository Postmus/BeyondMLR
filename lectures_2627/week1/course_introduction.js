const path = require('path');
const { addBulletVisualSlide } = require('../../slides-common/js/pptx_layout_primitives');
const { addConceptPanelsSlide } = require('../../slides-common/js/concept_panels');
const { addTeachingTableSlide } = require('../../slides-common/js/teaching_table');

function addCourseIntroduction(pptx) {
  addBulletVisualSlide(pptx, {
    title: 'Why go beyond multiple linear regression?',
    bullets: [
      'Experimental designs and sampling strategies in observational studies often lead to grouped observations.',
      'Ignoring this structure can violate the iid error assumptions, potentially biasing effect estimates or giving incorrectly estimated uncertainty.',
      'We use models that explicitly account for this structure, including linear mixed-effects models.',
      'The course develops your ability to choose, fit and interpret these models.'
    ],
    textFraction: .57,
    bulletFontSize: 22, bulletMinFontSize: 22,
    assetPath: path.join('assets', 'grouped-patients-hospitals.png'),
    framed: false
  });

  addTeachingTableSlide(pptx, {
    title: 'How the course works',
    intro: 'A weekly cycle of preparation, guided practice and independent application.',
    columns: [{ label: 'WHEN', weight: 3 }, { label: 'WHAT YOU DO', weight: 9.2 }],
    rows: [
      ['Before class', 'Read the selected book chapters to prepare for the session.'],
      ['During class', 'Build understanding through lectures and develop data analysis skills in guided R labs.'],
      ['After class', 'Apply the readings and in-class material to a new dataset in the weekly assignment.']
    ],
    fontSize: 23
  });

  addConceptPanelsSlide(pptx, {
    title: 'Assignments and reproducible reporting',
    intro: 'Use Quarto to bring your analysis and interpretation together.',
    panels: [
      { title: 'ONE REPRODUCIBLE REPORT', blocks: [
        { text: 'A Quarto source file (.qmd) combines written explanations with R code.', h: 1.0 },
        { text: 'Rendering runs the analysis and produces a report containing the results.', h: 1.0 },
        { text: 'Explain your choices and interpret the findings alongside the analysis.', h: 1.0 }
      ] },
      { title: 'WHAT TO SUBMIT', blocks: [
        { text: 'The Quarto source file (.qmd), including all R code and annotations.', h: 1.0 },
        { text: 'The rendered HTML report, with the R code visible.', h: .85 },
        { text: 'Use the provided assignment template.', h: .7 }
      ] }
    ],
    takeaway: 'Submit both the source and the rendered report so the analysis can be read and reproduced.'
  });

  addConceptPanelsSlide(pptx, {
    title: 'Certificate and assessment',
    panels: [
      { title: 'COURSE CERTIFICATE', blocks: [
        { text: 'Participate in at least 80% of the in-class meetings.', h: 1.0 },
        { text: 'Submit the weekly data analysis assignments.', h: 1.0 }
      ] },
      { title: 'IF YOU REQUIRE A GRADE', blocks: [
        { text: 'Your grade is based on the final data analysis assignment.', h: 1.0 },
        { text: 'This applies, for example, to research master students and PhD students seeking Epidemiologist B registration.', h: 1.5 }
      ] }
    ]
  });
}

module.exports = { addCourseIntroduction };
