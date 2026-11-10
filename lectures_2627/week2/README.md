# Beyond MLR: Week 2 starter

The initial section of the revised Week 2 lecture continues the rehabilitation example from Week 1 and introduces cluster randomization. Further Week 2 content remains to be agreed.

The sequence is a Week 1 recap, a comparison of randomization levels, a simulated cluster-randomized trial, the random-intercept model with center-level treatment, numerical uncertainty, and repeated-trial discussion. All models retain a common treatment effect across centers. New equations have plain-language interpretations. Labs and assignments are unchanged.

## Source structure

- `Beyond_MLR_Week_2.pptx`: starter presentation.
- `build_week2.js`: PptxGenJS content and slide order.
- `generate_data.R`: cluster-assigned teaching data, model results and statistical figure.
- `assets/rehabilitation_cluster_data.csv`: simulated patient outcomes under center-level assignment.
- `assets/model_results.json`: calculated results consumed by the deck builder.
- `assets/cluster-centers.png`: patient outcomes and means by center.
- `assets/equations/`: LaTeX source, MathJax SVG and PNG equation fallback.
- `../../slides-common/`: shared course theme, primitives and figure palette.
- `../week1/pptxgenjs_helpers/`: the existing course helper implementation.

The hypothetical trial contains six centers with 80 patients each. Three centers receive each program. The generator reads the existing Week 1 teaching dataset to retain its patient deviations, known center contributions and common 24 m treatment effect, then generates outcomes under center-level assignment. It does not simply relabel the original outcomes. With only six randomized centers, the example is intended to illustrate treatment information and uncertainty under the stated model assumptions. These are teaching data, not results from a real clinical trial.

## Rebuild

Run from this directory after installing the Node dependencies in `package.json`. Also install the dependencies in `../week1`: the shared helpers load packages from that directory. R requires `ggplot2`, `nlme` and `jsonlite`. The existing Week 1 dataset and helpers must be present.

```sh
npm install
npm run build
```

Figure generation finishes before the presentation is built. The supplied equation PNG allows the deck to render without TeX. Slide text and tables are editable; the statistical figure and equation are embedded images.

The repeated-trial discussion uses `#/concepts/randomized-block-design` and `#/concepts/cluster-randomization` in the separate sampling-simulator project. That application uses 20 patients per center and different outcome units, so it illustrates the designs rather than reproducing the lecture numbers.
