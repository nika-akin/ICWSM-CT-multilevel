# Contributing to Voat CT Analysis

Thank you for considering contributing! This project follows a reproducible
research workflow using `targets`.

## Development Workflow

1. **Fork** the repository and clone your fork.
2. **Branch**: `git checkout -b feature/your-feature-name`
3. **Code**: Add modular functions in `R/functions.R` or `R/plots.R`.
4. **Pipeline**: Register new targets in `_targets.R`.
5. **Test**: Run `make pipeline` to ensure the full graph succeeds.
6. **Document**: Update `docs/methodology.md` if you add new models.
7. **Commit**: Use clear messages (e.g., `feat: add time-varying cox target`).
8. **PR**: Open a pull request against `main`.

## Code Style

- Follow the [Tidyverse style guide](https://style.tidyverse.org/)
- Use `snake_case` for functions and variables
- Document functions with [roxygen2](https://roxygen2.r-lib.org/) templates


## Reporting Bugs

Use the [Bug Report template](.github/ISSUE_TEMPLATE/bug_report.md) and include:
- `sessionInfo()` output
- The target name that failed (`targets::tar_meta(fields = error)`)
- Minimal reproducible example

## Questions?

Open a [Discussion](https://github.com/nika-akin/voat-ct-analysis/discussions) 
