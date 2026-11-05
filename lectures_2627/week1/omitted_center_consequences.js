// Parallel conclusions for the fixed- and random-center simulations.
const consequences = {
  title: 'What are the consequences of omitting center?',
  panels: [
    { title: 'BIAS', blocks: [
      { text: 'The treatment estimate is unbiased.', h: .85 },
      { equation: String.raw`E[\widehat{\beta}_1]=\beta_1`, h: .55 },
      { text: 'Balanced treatment allocation makes center effects cancel from the treatment comparison.', h: 1.4 }
    ] },
    { title: 'ESTIMATED UNCERTAINTY', blocks: [
      { text: 'The usual standard error is too large on average.', h: .85, gap: .79 },
      { text: 'The standard error includes center variation that the balanced design eliminates from the treatment comparison.', h: 1.4 }
    ] }
  ],
  takeaway: 'Accounting for center gives a standard error that reflects the balance built into the design.'
};

module.exports = { consequences };
