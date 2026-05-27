# Why Conspiracy Theory Communities Endure: The Interplay of Feedback, Habits, and Community Context

![License: CC BY 4.0](https://img.shields.io/badge/License-CC%20BY%204.0-lightgrey.svg)
![R](https://img.shields.io/badge/R-%3E%3D%204.3.2-blue.svg)
![Targets](https://img.shields.io/badge/Pipeline-targets-green.svg)
> **A Reproducible `targets` Pipeline for Analyzing User Permanence and Radicalization Pathways in Conspiracy-Theory Communities on Voat**-

----

This repository contains a fully reproducible computational pipeline that analyzes how social feedback, routine behavior, and community context shape user engagement and radicalization in conspiracy-theory (CT) communities on Voat. Voat was a Reddit-like platform that operated from 2014 until its shutdown on December 25, 2020. Its subverses (communities) included both conspiracy and non-conspiracy spaces, making it a natural laboratory for studying reward structures and community context.
The pipeline is built with `targets` (R) and containerized with Docker to ensure that any change to upstream data or code automatically propagates to downstream results.

---
  > RQ1	How does responsiveness to social feedback vary with routine behavior and community context?	Mixed-effects regression (log-transformed latency)
  > RQ2	Which factors predict upshifts toward conspiracy-theory communities?	GLMM, GAMM with smooth terms
  > RQ3	How does persistence vary with social feedback, routine, and community?	Kaplan-Meier, Cox PH with time-varying coefficients

---

Repository Structure
```
voat-ct-analysis/
├── _targets.R              # Pipeline orchestration (40+ targets)
├── renv.lock               # Reproducible R package environment
├── Dockerfile              # Containerized runtime environment
├── Makefile                # High-level workflow commands
│
├── R/                      # Modular R functions
│   ├── 01_data_prep.R
│   ├── 02_feature_engineering.R
│   ├── 03_rq1_feedback.R
│   ├── 04_rq2_radicalization.R
│   ├── 05_rq3_survival.R
│   └── 06_visualization.R
│
├── data/
│   ├── raw/                # Original Voat dump (not in repo; see Data)
│   ├── processed/            # Cleaned and feature-engineered datasets
│   └── dictionaries/         # Data dictionaries for all variables
│
├── reports/
│   ├── manuscript.qmd        # Main Quarto manuscript
│   ├── supplementary.qmd       # Supplementary materials
│   └── figures/              # Generated figures (Fig 1–8)
│
├── shiny/                    # Interactive exploration app
│   └── app.R
│
└── docs/                     # Documentation
    ├── methodology.md
    └── changelog.md
```

The dataset comprises:
2,711 unique users
4,435 subverses (communities)
2,099,255 comments
49,550 submissions
Time span: 2014-06-19 to 2020-12-25
Key Variables
Variable	Description	Construction
`controversy`	Reddit-style controversy score	—
`privilege`	Submission rights (CCP ≥ 10)	Rolling 10-comment window; classified as never / steady / fluctuating
`habit_class`	Posting pattern	Burstiness > 0.7 → bursty; else top 25% frequency → steady regular; else occasional
`sv_score`	Subverse conspiracy score	Embedding-based score; ≥ 0 = CT, < 0 = non-CT
`transition_to_ct`	Weekly CT upshift	Binary: mean SV score increased from previous week
> **Note:** Raw Voat data are not distributed in this repository due to size and privacy constraints. See `data/dictionaries/` for full schema documentation and instructions for obtaining the source dump.
---
Reproducibility
Quick Start (with Docker)
```bash
git clone https://github.com/yourusername/voat-ct-analysis.git
cd voat-ct-analysis
make init      # Build container and restore renv
make pipeline  # Execute the full targets pipeline
make reports   # Render Quarto manuscripts and supplementary materials
```
Local Setup (without Docker)
```bash
git clone https://github.com/yourusername/voat-ct-analysis.git
cd voat-ct-analysis

# Restore exact package versions
R -e "renv::restore()"

# Run the pipeline
R -e "targets::tar_make()"

# Render reports
R -e "quarto::quarto_render('reports/manuscript.qmd')"
```
Pipeline Graph
The `targets` pipeline includes 40+ interdependent targets. Visualize the dependency graph with:
```r
targets::tar_visnetwork()
```
Environment
R: ≥ 4.3.2
Key packages: `targets`, `lme4`, `mgcv`, `survival`, `quarto`, `renv`, `tidyverse`
Docker image: `rocker/tidyverse:4.3.2` (see `Dockerfile`)
---
Key Findings
Habit moderates reward sensitivity (RQ1): Occasional posters are more sensitive to social rewards (faster return after high net votes) than bursty or regular posters—a pattern consistent with reinforcement learning in habitual behavior.
Group identity buffers radicalization (RQ2): Content featuring actors and actions (group-level narratives) is the strongest predictor of not shifting into CT subverses. Abstract trait content (secrecy, patterns, threat) shows weaker effects. High group identity combined with moderate activity maximizes the probability of remaining in non-CT communities.
Privilege instability predicts dropout (RQ3): Fluctuating CCP (comment contribution points) predicts faster dropout than stable privilege, especially in CT communities. Downvotes initially increase dropout hazard, but this effect decays over time.

---
Shiny App
An interactive Shiny application for exploring survival curves and transition probabilities by user segment is included in `shiny/app.R`. Launch with:
```r
shiny::runApp("shiny/app.R")
```
---
Citation
If you use this pipeline or dataset, please cite:
```bibtex
@article{batzdorfer2026voat,
  title={Why Conspiracy Theory Communities Endure: The Interplay of Feedback, Habits, and Community Context},
  author={Batzdorfer, Veronika, Samory, Mattia, Banisch, Sven},
  journal={ICWSM},
  year={2026}
}
```
---
This project is licensed under the Creative Commons Attribution 4.0 International License.
The underlying source code is licensed under the MIT License.

---
For questions about the methodology or data, please open an issue or contact:
Veronika Batzdorfer  
Karlsruhe Institute of Technology (KIT)  
Email: veronika.batzdorfer@kit.edu

---

Acknowledgments
The `targets` ecosystem for reproducible pipelines in R.
The `renv` team for dependency isolation.
Voat community archivists for preserving the dataset.
