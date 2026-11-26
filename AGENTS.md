# Git workflow

- Work directly on `master`. Create or switch to a feature branch only when the user explicitly requests it.
- Before continuing existing work, check whether newer commits are present on other remote branches and reconcile the intended version with `master` while preserving local changes.

# Writing preferences

Apply these preferences throughout the repository, including lectures, labs, assignments, and explanatory text.

- Write contractions in full throughout course prose, using forms such as “you are”, “we will”, and “does not”.
- Explain concepts directly by stating what they mean, what they describe, or what a method does.
- Avoid sentences that define something by stating what it is not. Avoid contrastive wording such as "X, not Y" and "rather than Y" when a direct explanation conveys the point.
- Keep explanations focused on the question being taught. Omit unsolicited caveats and unrelated conceptual detours, such as causal disclaimers when causality is outside the question at hand.
- Keep lecture speaker notes empty. Use empty strings in existing note calls where needed, preserving the presentation library and export infrastructure.
- Preserve relevant statistical distinctions by explaining each concept positively. For example: "The standard error describes uncertainty about the average treatment effect. The treatment-slope variance describes variation in treatment effects between laboratories."

# Teaching code preferences

- Name fitted models for the feature students are comparing, including units when a predictor is rescaled. Use matching names for derived quantities, such as `model_quadratic` and `residual_quadratic`.
- Student lab exercises show the R code for students to run. Render plots and analysis output only in the model answer sheets.
- After fitting a model, use `summary(model)` as the default next line so students can inspect the full model summary.
- Keep teaching code simple and easy to adapt across case studies. Convert grouping variables with `factor(x)` when the default level order is suitable.
- Specify factor levels explicitly when their order matters for the analysis, such as setting the treatment reference category or ordering ordinal categories. Avoid unnecessary label-construction code such as `levels = paste(...)` or `sprintf(...)` for grouping variables.
- Use simple grouping identifiers in new teaching datasets, such as integers 1 to 12 or letters A to E, to make adapting the lab code straightforward.
