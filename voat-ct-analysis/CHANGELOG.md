# Changelog

All notable changes to this project will be documented in this file.

## [1.0.0] — 2026-05-12

### Added
- Initial `targets` pipeline with 40+ steps covering RQ1–RQ3
- Modular R functions for preprocessing, survival, latency, and transition models
- Quarto reports: `main_analysis.qmd` and `supplementary.qmd`
- Shiny dashboard with 5 tabs reading from the targets store
- Docker support for containerized reproducibility
- `renv` lockfile for reproducible package management
- GitHub Actions workflow for R CMD check
- Data dictionary and methodology documentation

### Research Outputs
- Survival analysis: Kaplan-Meier, Cox PH with time-varying coefficients, frailty
- Posting latency: Mixed-effects model with habit × reward interactions
- CT transitions: GLMM and GAMM with smooth terms and contour visualizations
- Validation: Subverse score ROC, feature PCA, inter-rater agreement
