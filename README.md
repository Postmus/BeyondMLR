# Beyond multiple linear regression

Course materials are organized by academic year. Each lab folder is an independent Quarto website project.

| Folder | Purpose |
| --- | --- |
| `labs_2526/` | Archived 2025–2026 website sources, labs, assignments and datasets |
| `labs_2627/` | Current 2026–2027 website sources, labs, assignments and datasets |
| `lectures_2526/` | Previous lecture sources |
| `lectures_2627/` | Revised lecture sources and planning notes |
| `slides-common/` | Shared lecture themes and builders |
| `labs_2526/_site/` | Local build of the archived website |
| `labs_2627/_site/` | Local build of this year's website |
| `docs/` | Published 2026–2027 website files for GitHub Pages |

## Work on this year's website

Run these commands from the repository root:

```sh
quarto preview labs_2627
quarto render labs_2627
```

The website currently contains the Week 1 labs and assignment, grouped under a Week 1 menu in the top navigation. Add further week menus as later weeks are revised. Assignment report templates are copied as downloadable source files and excluded from rendering.

Quarto requires the R packages `knitr` and `rmarkdown` to render the lab pages. The lab setup chunks list the packages students need to run the analyses. The website displays the supplied lab code without executing it. The full render builds both the website and the downloadable lab PDFs, linked from each lab's right-hand side panel. PDF rendering requires a TeX installation; install it once with `quarto install tinytex`. For an HTML-only preview build, use `quarto render labs_2627 --to html`. Use the full render before publishing so the PDF downloads are up to date.

## Consult the previous edition

```sh
quarto preview labs_2526
quarto render labs_2526 --to html
```

The original lab and assignment contents are preserved inside `labs_2526/`. Previous lecture data paths have been updated to point to the relocated datasets. Local student submission folders remain excluded from Git and website rendering.

## Publish updates

Each project builds into its own `_site/` folder. Normal rendering leaves `docs/` unchanged.

Run `quarto render labs_2627` to build the website and PDF downloads. Replace the contents of `docs/` with the fresh build from `labs_2627/_site/`, include a `.nojekyll` file in `docs/`, and commit and push the published files to `master`. Replacing the directory contents also removes retired pages. The published site contains only the current edition; `labs_2526/` remains available in the repository.

The website is hosted at <https://postmus.github.io/BeyondMLR/>. Preview and render commands update local files; GitHub Pages deploys after the published files are pushed.
