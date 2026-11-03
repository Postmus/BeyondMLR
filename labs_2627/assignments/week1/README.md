# Week 1 assignment materials

The assignment compares standard and modified protein extraction protocols using tissue aliquots from ten donors. It applies the three models from the Week 1 lecture and labs to a new experimental setting.

- `assignment.qmd`: student-facing study description, analysis tasks and submission requirements.
- `downloads/protein_yield.csv`: simulated data for 80 aliquots, with four aliquots per protocol within each donor.
- `downloads/report_template.qmd`: report settings, section prompts and empty labelled R chunks. Students supply the analysis code.
- `_generate_data.R`: reproducible data-generation script, requiring only base R. Run it with `Rscript labs_2627/assignments/week1/_generate_data.R` from the repository root, or provide its full path from another working directory.

The assignment belongs to the 2026–2027 website project in `labs_2627`. Render it from the repository root with:

```sh
quarto render labs_2627/assignments/week1/assignment.qmd --to html
```

The preview is written to `labs_2627/_site/assignments/week1/assignment.html`. The project copies the dataset and report template into the accompanying `downloads` folder. The template is excluded from project rendering and remains a downloadable source file. Students render their own completed copy separately.

The generator uses a fixed seed, independent Normal donor contributions and aliquot errors, a common protocol effect, and balanced random allocation within donors. Yield is recorded in micrograms of protein per milligram of tissue. Donor identifiers follow generation order rather than ordering donors by yield.

Checks cover reproducibility, allocation, identifiers, positive finite measurements, all three model fits, treatment comparisons, and the random-effect variance estimates. The blank report template and assignment page were rendered, and the student download links were checked. The original assignments are preserved in `labs_2526/`; the published files in `docs/` remain unchanged until deployment.
