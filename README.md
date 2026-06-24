# Why Conspiracy Theory Communities Endure: The Interplay of Feedback, Habits, and Community Context

![License: CC BY 4.0](https://img.shields.io/badge/License-CC%20BY%204.0-lightgrey.svg)
![R](https://img.shields.io/badge/R-%3E%3D%204.3.2-blue.svg)
![Targets](https://img.shields.io/badge/Pipeline-targets-green.svg)
![ICWSM 2026](https://img.shields.io/badge/Conference-ICWSM%202026-orange.svg)

> **A reproducible `targets` pipeline for analyzing multi-scale engagement dynamics in conspiracy-theory communities on Voat**

---

## Overview

This repository contains a fully reproducible computational pipeline analyzing how **social feedback**, **routine behavior**, and **community context** shape user engagement in conspiracy-theory (CT) communities on Voat—a Reddit-like platform that operated from 2014 until its shutdown on December 25, 2020.

**Key insight:** CT communities endure not because of what members *believe*, but because of how they *behave*. Habit formation transforms reward-seeking into automatic routine, while ideological alignment provides resilience against social punishment.

---

## Research Questions

| RQ | Question | Method |
|----|----------|--------|
| **RQ1** | How does responsiveness to social feedback vary with routine behavior and community context? | Linear mixed-effects regression (log-transformed posting latency) |
| **RQ2** | Which factors predict upshifts toward conspiracy-theory communities? | GAMM with binomial link, smooth terms for content cues |
| **RQ3** | How does persistence vary with social feedback, routine, and community? | Cox proportional hazards with time-varying coefficients |

---

## Key Findings

### RQ1: Habit Decouples Behavior from Feedback
Users with strong posting routines show **reduced sensitivity to social reinforcement**. Lagged net votes predict shorter posting latency overall (β = -0.02, p < .001), but this effect attenuates for regular (β = 0.021, p < .001) and bursty posters (β = 0.018, p < .001). Community type (CT vs. non-CT) does not modulate this decoupling.

### RQ2: Group Cues Reduce CT Migration
Counterintuitively, exposure to **group-oriented narratives** (actor/action attributions) *decreases* likelihood of transitioning to CT spaces (edf = 7.1, χ² = 319.3, p < .001). Trait cues (secrecy/pattern/threat) show no significant effect. Habitual engagement increases baseline susceptibility (steady: β = 0.70; bursty: β = 0.68).

### RQ3: Ideology Buffers Dropout—Habit Does Not
Negative feedback increases dropout hazard (HR = 1.12, p < .001). Surprisingly, **habitual posting increases dropout risk** (steady: HR = 1.15; bursty: HR = 1.40, both p < .001)—unless buffered by CT alignment. CT subverses show **80% lower dropout hazard** compared to non-CT spaces (HR = 1.80 for non-CT, p < .001).

---

## Dataset

| Statistic | Value |
|-----------|-------|
| Unique users | 2,711 |
| Subverses (communities) | 4,435 |
| Comments | 2,099,255 |
| Submissions | 49,550 |
| Time span | 2014-06-19 to 2020-12-25 |

### Key Variables

| Variable | Description | Construction |
|----------|-------------|--------------|
| `controversy` | Reddit-style controversy score | `(u+d) × min(u,d) / max(u,d)` |
| `habit_class` | Posting pattern | Burstiness > 0.7 → bursty; else top 25% frequency → steady regular; else occasional |
| `sv_score` | Subverse conspiracy score | Word2Vec embedding + PCA projection on seed CT communities; ≥ 0 = CT |
| `transition_to_ct` | Weekly CT upshift | Binary: mean SV score increased from previous week |
| `tenure` | Account age stratum | Short (≤365d), Mid (365-1000d), Long (>1000d) |

> **Note:** Raw Voat data will be made available at OSF. See `data/dictionaries/` for full schema documentation and instructions for obtaining the source dump. All analyses aggregate at community level; no usernames or identifiable text excerpts are reported.

---

## Repository Structure

```
voat-ct-analysis/
├── _targets.R                    # Pipeline orchestration (40+ targets)
├── renv.lock                     # Reproducible R package environment
├── Dockerfile                    # Containerized runtime environment
├── Makefile                      # High-level workflow commands
│
├── R/                            # Modular R functions
│   ├── 01_data_prep.R            # Data loading, cleaning, filtering
│   ├── 02_feature_engineering.R  # Habit classification, CT scoring, controversy
│   ├── 03_rq1_feedback.R         # Micro-level: posting latency models
│   ├── 04_rq2_radicalization.R   # Meso-level: GAMM for CT upshifts
│   ├── 05_rq3_survival.R         # Macro-level: Cox proportional hazards
│   └── 06_visualization.R        # Publication-quality figures
│
├── data/
│   ├── raw/                      # Original Voat dump (not in repo; see Data Access)
│   ├── processed/                # Cleaned and feature-engineered datasets
│   └── dictionaries/             # Data dictionaries for all variables
│
├── reports/
│   ├── manuscript.qmd            # Main Quarto manuscript (ICWSM submission)
│   ├── supplementary.qmd         # Supplementary materials, appendices
│   └── figures/                  # Generated figures (Fig 1–9)
│
├── shiny/                        # Interactive exploration app
    └── app.R                     # Survival curves, transition probabilities

```

---

## Reproducibility

### Quick Start (with Docker)

```bash
git clone https://github.com/nika-akin/ICWSM-CT-multilevel.git
cd ICWSM-CT-multilevel

make init      # Build container and restore renv
make pipeline  # Execute the full targets pipeline
make reports   # Render Quarto manuscripts and supplementary materials
make poster    # Compile conference poster (requires pdflatex)
```

### Local Setup (without Docker)

```bash
git clone https://github.com/nika-akin/ICWSM-CT-multilevel.git
cd ICWSM-CT-multilevel

# Restore exact package versions
R -e "renv::restore()"

# Run the pipeline
R -e "targets::tar_make()"

# Render reports
R -e "quarto::quarto_render('reports/manuscript.qmd')"

# Visualize pipeline dependencies
R -e "targets::tar_visnetwork()"
```

### Pipeline Architecture

The `targets` pipeline includes **40+ interdependent targets** organized into three analytical streams:

```
Data Preparation → Feature Engineering → [RQ1 | RQ2 | RQ3] → Figures → Reports
                                              ↓
                                         Shared diagnostics & validation
```

---

## Environment

| Component | Version |
|-----------|---------|
| R | ≥ 4.3.2 |
| targets | 1.7+ |
| lme4 | 1.1-35+ |
| mgcv | 1.9-1+ |
| survival | 3.5-8+ |
| quarto | 1.5+ |
| tidyverse | 2.0+ |

**Docker image:** `rocker/tidyverse:4.3.2` (see `Dockerfile`)

---

## Interactive Exploration

A Shiny application for exploring survival curves and transition probabilities by user segment is included in `shiny/app.R`. Launch with:

```r
shiny::runApp("shiny/app.R")
```

Features:
- Interactive Kaplan-Meier curves by habit class and subverse type
- GAMM marginal effects plots for content cues
- Hazard ratio forest plots with confidence intervals
- User-level trajectory visualization

---

## Conference Poster

This repository includes the presentation LaTeX poster for ICWSM 2026:

```
poster.pdf
```
---

## Citation

If you use this pipeline or dataset, please cite:

```bibtex
@inproceedings{batzdorfer2026conspiracy,
  title={Why Conspiracy Theory Communities Endure: The Interplay of Feedback, Habits, and Community Context},
  author={Batzdorfer, Veronika and Samory, Mattia and Banisch, Sven},
  booktitle={Proceedings of the International AAAI Conference on Web and Social Media (ICWSM)},
  year={2026},
  volume={20},
  number={1},
  pages={235--249},
  doi={https://doi.org/10.1609/icwsm.v20i1.42635}
}
```

---




*Last updated: June 2026*
