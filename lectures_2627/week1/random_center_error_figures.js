const path = require('path');
const { addFigureSlide } = require('../../slides-common/js/pptx_layout_primitives');

function addRandomCenterErrorFigures(pptx) {
  addFigureSlide(pptx, {
    title: 'Combined errors: same mean and variance',
    assetPath: path.join('assets', 'random-center-combined-errors.png'),
    caption: 'Center effects and individual errors are drawn again in every simulated study. Dashed lines: mean 0.',
    captionFontSize: 17, captionMinFontSize: 17,
    takeaway: 'Every combined error cⱼ + εᵢⱼ has mean 0 and variance 20² + 24² = 976 m².',
  });
  addFigureSlide(pptx, {
    title: 'Combined errors: dependence within centers',
    assetPath: path.join('assets', 'random-center-combined-error-pairs.png'),
    caption: 'Each dot pairs errors from one study; center effects and individual errors are redrawn in all 10,000 studies.',
    captionFontSize: 17, captionMinFontSize: 17,
    takeaway: 'Same center: a shared cⱼ gives covariance 20² = 400 m².\nDifferent centers: independent combined errors, covariance 0.',
  });
}

module.exports = { addRandomCenterErrorFigures };
