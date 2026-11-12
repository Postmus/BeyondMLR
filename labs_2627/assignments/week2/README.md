# Week 2 assignment materials

The assignment compares standard and enhanced postoperative pain-management protocols after elective knee replacement surgery. Within each of 12 hospitals, 15 patients are randomly assigned to each protocol, giving 360 patients measured once at 24 hours. Pain is recorded on a visual analogue scale from 0 to 10, to one decimal place. The data are simulated for teaching.

Students explore hospital-by-protocol means and conduct one random-intercept and treatment-slope analysis. They interpret the population-average protocol difference and its estimated SE, 95% confidence interval and p-value; the variance components; and the intercept-slope correlation. Conditional residual diagnostics and a written conclusion complete the report. Lab 3 supplies the exploratory-plot workflow, and Lab 4 supplies the model-fitting and interpretation workflow.

- `assignment.qmd`: study description, tasks, submission requirements, and Quarto help.
- `downloads/postoperative_pain.csv`: patient data for students.
- `downloads/report_template.qmd`: the Week 1 report format adapted to this study, with a hidden package-setup chunk including `emmeans`, prompts, and empty labelled analysis chunks. Model fitting is followed by results sections for fixed and random effects. Students interpret the correlation displayed by R, following Lab 4.
- `_generate_data.R`: reproducible base-R data generator, with allocation, identifier, and outcome-range checks.

The website menu links to the assignment. Download resources are copied by the existing website configuration, and the report template is excluded from project rendering.

## Regenerate and preview

From the repository root:

```sh
Rscript labs_2627/assignments/week2/_generate_data.R
quarto render labs_2627/assignments/week2/assignment.qmd --to html
```

The local preview is `labs_2627/_site/assignments/week2/assignment.html`. Publishing the website is a separate step.

## Instructor notes

The generator uses seed 262702. The population standard-care mean is 5.3 and the enhanced-minus-standard difference is -1.1 pain-score points. Hospital intercept and slope deviations follow a joint Normal distribution with standard deviations 0.65 and 0.8, and correlation -0.45. Independent patient errors have standard deviation 1. Scores are rounded to one decimal place; the generated values range from 1.3 to 7.8. Hospital labels run from Hospital 01 to Hospital 12 in generation order; `factor(pain$hospital)` preserves this order for analysis. Treatment assignments are shuffled within hospital.

With Standard as the reference, the intended model is:

```r
lmer(pain_score ~ protocol + (1 + protocol | hospital), data = pain,
     REML = TRUE)
```

The checked fit converges with a nonsingular random-effects covariance matrix. The estimated enhanced-minus-standard difference is -1.2267, with estimated SE 0.1832, Satterthwaite df approximately 11, 95% CI [-1.6298, -0.8235], and p approximately 0.0000339. Estimated intercept, slope, and residual variances are approximately 0.3984, 0.2855, and 0.8776. The estimated intercept-slope correlation is approximately -0.50: higher standard-care pain tends to accompany a more negative protocol difference, corresponding to greater pain reduction.

The estimated SE describes uncertainty about the population-average difference. The treatment-slope variance describes variation in protocol differences across hospitals. These interpretations are central to the assignment.
