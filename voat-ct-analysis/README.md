# Voat CT Communities: Rewards and Permanence

[![R-CMD-check](https://github.com/yourusername/voat-ct-analysis/workflows/R-CMD-check/badge.svg)](https://github.com/yourusername/voat-ct-analysis/actions)
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](LICENSE)

> Reproducible research pipeline analyzing rewards, permanence, and radicalization pathways in conspiracy-theory communities on Voat. Built with [`targets`](https://docs.ropensci.org/targets/) for full reproducibility.

## Quick Start

```bash
# 1. Clone the repository
git clone https://github.com/nika-akin/ICWSM-CT-multilevel.git
cd voat-ct-analysis

# 2. One-click setup (installs packages, checks data, creates dirs)
Rscript init.R

# 3. Run the full pipeline
make pipeline

# 4. Generate reports
make reports

# 5. Launch Shiny dashboard
make shiny
```

## Project Structure

```
.
├── _targets.R              # Pipeline definition (~40 targets)
├── init.R                  # Environment setup & dependency check
├── Makefile                # CLI shortcuts
├── renv.lock               # Reproducible R environment (via renv)
├── Dockerfile              # Containerized execution
├── data/                   # Raw data (not tracked by git)
├── R/                      # Modular functions
│   ├── functions.R         # Analysis functions
│   ├── plots.R             # Visualization functions
│   └── shiny_modules.R     # Shiny UI/Server modules
├── reports/                # Quarto reports
│   ├── main_analysis.qmd   # Main manuscript results
│   └── supplementary.qmd   # Diagnostics & sensitivity
├── shiny/                  # Shiny application
│   └── app.R               # Dashboard (reads from targets store)
└── docs/                   # Documentation
    ├── methodology.md      # Design decisions & model specs
    └── data_dictionary.md  # Variable documentation
```

## Requirements

- R >= 4.2.0
- [`renv`](https://rstudio.github.io/renv/) for package management
- GNU Make (optional, for Makefile shortcuts)
- Docker (optional, for containerized runs)

## Data

Place the following files in `data/` before running the pipeline:

| File | Description |
|------|-------------|
| `seperated_annotation.csv` | Main comment/submission annotations |
| `user_info.csv` | User registration dates (tenure) |
| `subverse_scores_wa.csv` | Subverse CT scores (embeddings) |
| `subverse.csv` | Subverse metadata (subscribers, creation date) |

## Shiny App

The dashboard auto-updates from the `targets` store. Launch via:

```r
# From R console
shiny::runApp("shiny/app.R")
```

Tabs:
- **Overview**: Key statistics boxes
- **Survival**: Interactive KM curves (stratified by privilege, controversy, habit)
- **Posting Latency**: Reward-latency exploration (RQ2)
- **CT Transitions**: Marginal predictions from GAMM (RQ3)
- **Data Explorer**: Filterable table of user-week observations

## Docker

```bash
docker build -t voat-ct .
docker run -p 3838:3838 -v $(PWD)/data:/app/data voat-ct
```

## Citation

If you use this code, please cite:

```bibtex
@software{voat_ct_analysis,
  title = {Voat CT Communities: Rewards and Permanence},
  author = {Veronika Batzdorfer},
  year = {2026},
  url = {https://github.com/nika-akin/ICWSM-CT-multilevel}
}
```

## License

MIT © 2026 Veronika Batzdorfer. See [LICENSE](LICENSE) for details.
