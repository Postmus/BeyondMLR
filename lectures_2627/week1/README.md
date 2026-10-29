# Beyond MLR: week 1 lecture draft

This presentation uses simulated knee-rehabilitation data. Center A provides the single-center example; the complete dataset contains six centers with balanced treatment allocation. The data are for teaching and do not represent a real clinical study.

## Source structure

- `Beyond_MLR_Week_1.pptx`: presentation.
- `build_week1.js`: PptxGenJS content and slide order.
- `generate_data.R`: simulation, figures and model calculations.
- `assets/rehabilitation_data.csv`: simulated patient-level dataset.
- `assets/rehabilitation-walk-assessment.png`: generated illustration for the case-study slide.
- `assets/equations/`: LaTeX source, MathJax SVG and high-resolution PNG fallbacks.
- `pptxgenjs_helpers/`: local helpers for text sizing, diagrams, LaTeX and QA.
- `../../slides-common/js/`: shared Beyond MLR theme, layout primitives and course builders, adapted from the LAS reference project.
- `../../slides-common/R/common_fig.R`: shared figure palette and sizing conventions.

Slide text, tables and layout elements are editable. Statistical figures and equations are embedded as images. Equations are authored as LaTeX and rendered to SVG with `latexToSvgDataUri`; PNG fallbacks are embedded for consistent PowerPoint/LibreOffice rendering.

## Rebuild

Run from this directory. Install the Node packages in `package.json` first. R requires `ggplot2`, `nlme` and `jsonlite`. The supplied equation PNGs allow the presentation to render without a local TeX installation.

```sh
npm install
npm run build
```

Figure generation must finish before the presentation is built. The model numbers are calculated by `generate_data.R` and saved to `assets/model_results.json`; the deck builder reads them from that file. The fixed seed keeps the example reproducible.

The simulation demonstration refers to the separate sampling-simulator project, route `#/concepts/randomized-block-design`. It currently compares ordinary regression and a random-intercept model; the presentation's fixed-center comparison is calculated from the lecture dataset.
