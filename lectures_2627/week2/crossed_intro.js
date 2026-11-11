const { addSectionSlide } = require('../../slides-common/js/pptx_layout_primitives');
const { addConceptPanelsSlide } = require('../../slides-common/js/concept_panels');

function addCrossedEffectsIntroduction(pptx) {
  addSectionSlide(pptx, {
    sectionLabel: 'PART 3',
    sectionTitle: 'Crossed random effects'
  });
  const introduction = addConceptPanelsSlide(pptx, {
    title: 'What if there is more than one grouping variable?',
    intro: 'So far: one grouping variable, center. What changes with two or more grouping variables?',
    panels: [
      { title: 'NESTED GROUPING VARIABLES', blocks: [
        { text: 'Each group at one level belongs to exactly one group at the next level.',
          h: 1.10, fontSize: 22, gap: .10 },
        { text: 'Patients belong to wards, and each ward belongs to one hospital.',
          h: 1.10, fontSize: 22, gap: .10 },
        { text: 'Grouping variables: ward and hospital.', h: .70, fontSize: 20 }
      ] },
      { title: 'CROSSED GROUPING VARIABLES', blocks: [
        { text: 'Neither grouping variable is contained within the other.',
          h: 1.10, fontSize: 22, gap: .10 },
        { text: 'Each patient is assessed by several radiologists, and each radiologist assesses several patients.',
          h: 1.10, fontSize: 22, gap: .10 },
        { text: 'Grouping variables: patient and radiologist.', h: .70, fontSize: 20 }
      ] }
    ],
    takeaway: 'Next: crossed grouping variables, illustrated by patients and radiologists.\nWeek 3: nested grouping variables, such as wards within hospitals.',
    takeawayHeight: 1.0
  });
  introduction.addNotes('Start from the first two weeks: center was the single grouping variable in the rehabilitation data. A random intercept and random treatment slope are two coefficients varying across centers, not two grouping variables. Now consider observations grouped according to two or more grouping variables. Focus nesting and crossing on grouping variables rather than discussing treatment-factor crossing. In the nested example, the observations are patient outcomes; ward and hospital are grouping variables, with each ward belonging to exactly one hospital. In the crossed example, the observation is a measurement made by a radiologist on a patient; patient and radiologist are grouping variables. Measurements can share a patient, a radiologist, or both. Neither grouping variable is contained in the other. If every radiologist assesses every patient the design is fully crossed, but every possible combination need not be observed for a crossed structure. Both arrangements allow more than one shared source of variation. Grouping structure describes relationships among groups; the choice of fixed or random effects is a separate modeling decision. This use of nested is also distinct from a reduced regression model being nested within a full model. Next, briefly develop crossed random effects using the radiologist agreement case study from the old Week 2 lecture. Nested grouping structures are reserved for Week 3, which begins with cluster randomization as an introduction to context-level predictors before observational multilevel analysis. This slide introduces the distinction and course sequence without formulas or software syntax.');
}

module.exports = { addCrossedEffectsIntroduction };
