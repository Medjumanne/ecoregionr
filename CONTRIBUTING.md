# Contributing to ecoregionr

Thank you for taking the time to contribute! This document explains how to
report bugs, suggest features, and submit code changes.

---

## Code of conduct

This project follows the [Contributor Covenant Code of Conduct](CODE_OF_CONDUCT.md).
By participating you agree to abide by its terms.

---

## Reporting bugs

1. Search the [issue tracker](https://github.com/Medjumanne/ecoregionr/issues)
   to check whether the bug has already been reported.
2. If not, open a **Bug report** issue using the provided template.
3. Include a **minimal reproducible example** (use `reprex::reprex()` if
   possible), your R version, and `sessionInfo()` output.

---

## Suggesting features

Open a **Feature request** issue and describe:

- The use case / scientific motivation.
- What the function signature might look like.
- Any relevant literature or existing implementations.

---

## Submitting a pull request

1. **Fork** the repository and create a new branch:
   ```bash
   git checkout -b feat/your-feature-name
   ```

2. **Install development dependencies**:
   ```r
   devtools::install_dev_deps()
   ```

3. **Make your changes.**  Follow the existing code style:
   - Use `snake_case` for function and variable names.
   - Document every exported function with roxygen2 (`@param`, `@return`,
     `@examples`).
   - Add an entry to `NEWS.md` under the current development version.

4. **Run checks** before pushing:
   ```r
   devtools::document()   # regenerate Rd files and NAMESPACE
   devtools::check()      # full R CMD check — must pass with 0 errors/warnings
   ```

5. Open a pull request against the `main` branch.  Fill in the PR template,
   reference any related issues, and describe what changed and why.

---

## Style guide

- Indentation: 2 spaces (no tabs).
- Line length: ≤ 100 characters.
- Pipe: prefer the native `|>` pipe (R ≥ 4.1) over `%>%`.
- Avoid attaching packages with `library()` inside package functions — use
  `pkg::function()` notation or declare in `DESCRIPTION`.

---

## Adding a new clustering method

If you want to add a third clustering strategy:

1. Create a new file `R/NN_your_method.R`.
2. Export the main function and any helpers via roxygen2 `@export`.
3. Run `devtools::document()` to update `NAMESPACE`.
4. Add a section to `ecoregionr_dual_workflow.Rmd` demonstrating the method.
5. Update `README.md` function table and `NEWS.md`.

---

## Questions?

Open a [Discussion](https://github.com/Medjumanne/ecoregionr/discussions) for
general questions about usage or methodology.
