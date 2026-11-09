const path = require('path');
const { addFigureSlide } = require('../../slides-common/js/pptx_layout_primitives');

function addFixedCenterErrorFigures(pptx) {
  addFigureSlide(pptx, {
    title: 'Errors: different means, same variance',
    assetPath: path.join('assets', 'fixed-center-errors-10000.png'),
    caption: '10,000 studies with fixed center effects; 10 patients per treatment in each center; individual-error SD = 24 m.\nCenter effects (m): A −17, B +16, C +8, D −29, E +27, F −5. Dashed lines: expected errors.',
    captionHeight: .82,
    captionFontSize: 17, captionMinFontSize: 17,
    takeaway: 'The means differ by center; the variation around each patient’s own mean is the same.',
  });
  addFigureSlide(pptx, {
    title: 'Errors: independent within and between centers',
    assetPath: path.join('assets', 'fixed-center-error-pairs.png'),
    caption: 'Each dot: two errors from one of 10,000 studies with fixed center effects. Dashed lines: expected errors.',
    captionFontSize: 17, captionMinFontSize: 17,
    takeaway: 'A fixed center effect shifts the means without creating dependence.\nTheoretical covariance = 0; empirical covariances fluctuate around zero.',
  });
}

function addPooledErrorFigure(pptx) {
  return addFigureSlide(pptx, {
    title: 'The pooled error distribution',
    assetPath: path.join('assets', 'fixed-center-pooled-errors.png'),
    caption: 'Each center contributes equally; the center effects remain fixed across simulations.',
    captionFontSize: 17, captionMinFontSize: 17,
    takeaway: 'The pooled error distribution is a mixture of six center-specific distributions. Because the centers contribute equally and their effects average to zero, the mixture also has mean zero.',
    takeawayHeight: 1.15,
  });
}

module.exports = { addFixedCenterErrorFigures, addPooledErrorFigure };
