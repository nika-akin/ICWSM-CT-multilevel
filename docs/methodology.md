# Methodology Documentation

## Overview

This pipeline analyzes multi-scale engagement dynamics in conspiracy-theory (CT) communities on Voat using a reproducible `targets` workflow with 40+ interdependent targets.

## Data Processing Pipeline

### Stage 1: Data Loading (`01_data_prep.R`)
- **Input**: Raw CSV files from Voat dump
- **Functions**: `load_annotation()`, `load_user_info()`, `load_sv_scores()`, `load_subverse()`
- **Output**: Tibbles ready for preprocessing

### Stage 2: Feature Engineering (`02_feature_engineering.R`)
- **Controversy Score**: Reddit-style formula: `(u+d) × min(u/d, d/u)`
- **Privilege Trajectory**: CCP rolling window (10 comments), privilege = net_votes ≥ 10
- **Habit Classification**: 
  - Burstiness > 0.7 → "bursty"
  - Top 25% frequency → "steady regular"  
  - Else → "occasional"
- **Tenure Strata**: Short (≤365d), Mid (365-1000d), Long (>1000d)
- **Subverse Growth**: subscribers / age_in_days

### Stage 3: RQ1 Analysis (`03_rq1_feedback.R`)
**Question**: How does responsiveness to social feedback vary with routine behavior?

**Method**: Linear mixed-effects regression
```r
post_latency_log ~ lagged_net_votes_z * habit_class + sv_growth_z + tenure_strata + (1|user) + (1|subverse)
```

**Key Finding**: Habit decouples behavior from feedback (interaction β > 0, p < .001)

### Stage 4: RQ2 Analysis (`04_rq2_radicalization.R`)
**Question**: Which factors predict upshifts toward CT communities?

**Method**: GAMM with binomial link
```r
transition_to_ct ~ s(mean_trait_lag) + s(mean_group_lag) + s(mean_score_lag) + habit_class + s(user, bs="re")
```

**Key Finding**: Group-oriented narratives decrease CT migration (edf = 7.1, p < .001)

### Stage 5: RQ3 Analysis (`05_rq3_survival.R`)
**Question**: How does persistence vary with social feedback, routine, and community?

**Method**: Extended Cox proportional hazards with time-varying coefficients
```r
Surv(start_numeric, stop_numeric, event) ~ avg_upvotes_window + tt(avg_upvotes_window) + ... + frailty(subverse)
```

**Key Finding**: CT alignment buffers dropout risk (HR = 1.80 for non-CT, p < .001)

### Stage 6: Visualization (`06_visualization.R`)
- APA-style theme function
- Kaplan-Meier survival curves
- Cox model forest plots
- GAMM smooth plots
- Marginal effects plots

## Statistical Software

| Package | Version | Purpose |
|---------|---------|---------|
| lme4 | 1.1-35+ | Mixed-effects models |
| mgcv | 1.9-1+ | Generalized additive models |
| survival | 3.5-8+ | Survival analysis |
| targets | 1.7+ | Pipeline orchestration |
| ggplot2 | 3.4+ | Visualization |

## Reproducibility

All analyses are containerized via Docker (`rocker/tidyverse:4.3.2`) and use `renv` for package management.
