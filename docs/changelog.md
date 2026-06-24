# Changelog

All notable changes to this project.

## [2026-06-24] - Repository Reorganization

### Changed
- **R/ directory structure**: Split monolithic `functions.R` into modular files:
  - `01_data_prep.R` - Data I/O and preprocessing functions
  - `02_feature_engineering.R` - Feature calculations (controversy, privilege, habit, tenure, growth)
  - `03_rq1_feedback.R` - Posting latency models (LMM)
  - `04_rq2_radicalization.R` - CT transition models (GLMM/GAMM)
  - `05_rq3_survival.R` - Survival analysis (Cox models)
  - `06_visualization.R` - Visualization functions (renamed from `plots.R`)
- **data/ organization**: Moved all raw CSVs to `data/raw/`
- Created `data/processed/` for pipeline outputs
- Created `data/dictionaries/` with variable documentation
- **New directories**: `outputs/` and `docs/`

### Added
- `data/dictionaries/main_variables.csv` - Complete variable schema
- `docs/methodology.md` - Detailed methods documentation
- `docs/ethics.md` - Ethical considerations statement

### Updated
- `_targets.R` - Updated file paths to `data/raw/`

---

## [Previous versions]

See CHANGELOG.md for earlier version history.
