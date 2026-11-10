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

The website currently contains the Week 1 labs and assignment, grouped under a Week 1 menu in the top navigation. Model answer sheets are grouped in a separate Lab answers menu, with the week and lab identified in each entry. Add future answer sheets to this menu and further week menus as later weeks are revised. Assignment report templates are copied as downloadable source files and excluded from rendering.

Quarto requires the R packages `knitr` and `rmarkdown` to render the lab pages. The lab setup chunks list the packages students need to run the analyses. The student lab pages display the supplied code without executing it. The HTML-only model answer sheets execute their analysis to include results and plots; rendering them also requires `ggplot2`, `dplyr`, `lme4` and `lmerTest`. The full render builds both the website and the downloadable lab PDFs, linked from each lab's right-hand side panel. PDF rendering requires a TeX installation; install it once with `quarto install tinytex`. For an HTML-only preview build, use `quarto render labs_2627 --to html`. Use the full render before publishing so the PDF downloads are up to date.

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


## Build the lecture decks

Use Node.js 20 or newer and npm. Install the exact JavaScript dependencies from the
repository root with:

```sh
npm ci
```

The root npm workspace currently includes Week 1. Dependencies live in the repository's
ignored `node_modules/`, with exact versions in `package-lock.json`. Shared slide helpers
resolve their dependencies from the same installation. No `NODE_PATH` or dependency
installation under `/tmp` is needed. Add later lecture packages to the root `workspaces`
list when they are ready to join this build setup.

For a normal slide edit, reuse the existing figure assets and rebuild both PowerPoint and PDF:

```sh
npm run build:week1
```

The outputs are `lectures_2627/week1/Beyond_MLR_Week_1.pptx` and
`output/pdf/Beyond_MLR_Week_1.pdf`. To build only the PowerPoint, use `npm run slides:week1`;
to export the existing PowerPoint again, use `npm run render:week1`.

The revised Week 2 opening uses the same root dependencies and shared layout
helpers. Run `npm run build:week2` for its PowerPoint and PDF, or
`npm run rebuild:week2` to regenerate its data, figures and missing equations
first. Its scripts invoke the Week 2 builder directly; Week 2 is not yet a root
npm workspace. See `lectures_2627/week2/README.md` for the current slide sequence.

PDF export requires LibreOffice on `PATH` (or set `LIBREOFFICE_BIN` to its executable).
The renderer creates its own isolated profile and runtime under ignored `.cache/lecture-build/`,
uses a memory-backed settings store, and removes these disposable files afterwards.
It does not require an interactive desktop session. Install the course fonts used by
`slides-common/js/blr_theme.js` to preserve the intended layout on another machine.

When data or figure-generation code changes, use `npm run rebuild:week1` to regenerate
figures first, then build and render the slides. This also requires R and the packages
listed in the Week 1 README. Existing equation PNGs allow ordinary slide rebuilds without
LaTeX; regenerating missing equation images requires the TeX/Poppler tools described there.
