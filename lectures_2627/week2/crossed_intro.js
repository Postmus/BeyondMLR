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
  introduction.addNotes('');
}

module.exports = { addCrossedEffectsIntroduction };
