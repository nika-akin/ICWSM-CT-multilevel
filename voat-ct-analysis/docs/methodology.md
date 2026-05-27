# Methodology & Design Decisions

## 1. Platform Context: Voat

Voat was a Reddit-like platform operational from 2014 to 2020. Key structural differences from Reddit:
- **Subverses** instead of subreddits
- **CCP (Comment Contribution Points)** for submission rights (threshold = 10)
- **No downvote disabling**: Downvotes were always visible and impactful
- **Shutdown**: 2020-12-25 (right-censoring for all users)

## 2. Data Preprocessing Pipeline

### 2.1 Temporal Alignment
- `time` constructed from `date` + `time` columns via `lubridate::ymd_hms()`
- All durations computed relative to shutdown date
- 30-day buffer before shutdown to avoid exogenous closure bias

### 2.2 Controversy Score
We use the Reddit controversy formula:

$$
\text{controversy} = (\text{up} + \text{down})^{\min(\text{up}/\text{down}, \text{down}/\text{up})}
$$

With edge cases:
- If upvotes ≤ 0 or downvotes ≤ 0: controversy = 0
- If magnitude = 0: controversy = 0

### 2.3 Privilege Trajectory
CCP is computed over a rolling window of 10 comments. The trajectory is classified as:
- **never_had**: Never reached CCP ≥ 10
- **steady**: Reached CCP ≥ 10 and never dropped below
- **fluctuating**: Gained and lost privilege at least once

### 2.4 Habit Classification
Burstiness is defined as:

$$
B = \frac{\sigma_{\text{gap}} - \mu_{\text{gap}}}{\sigma_{\text{gap}} + \mu_{\text{gap}}}
$$

Classification rules:
| Burstiness | Posts/day | Class |
|:---|:---|:---|
| > 0.7 | — | bursty |
| ≤ 0.7 | > 75th percentile | steady regular |
| ≤ 0.7 | ≤ 75th percentile | occasional |

## 3. Survival Analysis (RQ1)

### 3.1 Dropout Definition
A user-subverse dyad is coded as **dropout** (event = 1) if the last comment occurred ≥ 60 days before platform shutdown. Otherwise, the observation is **right-censored** at shutdown.

### 3.2 Model Specification

**Cox PH with time-varying coefficients:**

$$
h(t | X(t), Z) = h_0(t) \exp\left(\beta_1 X(t) + \beta_2 X(t) \cdot \log(1+t) + \gamma Z + \omega_{\text{subverse}}\right)
$$

Where:
- $X(t)$: Time-varying covariates (upvotes, downvotes in 30-day windows)
- $Z$: Time-invariant covariates (habit, tenure, CT status)
- $\omega_{\text{subverse}}$: Frailty term (Gamma random effect) for subverse clustering

### 3.3 Why Time-Varying?
Schoenfeld residual tests rejected proportional hazards for vote variables. The $\log(1+t)$ interaction captures decaying effects: negative feedback is most impactful early in a user's tenure.

## 4. Posting Latency (RQ2)

### 4.1 DV Transformation
Latency (hours between consecutive posts) is heavily right-skewed (M = 24, Median = 1, Max = 38,027). We use:

$$
\text{latency}_{\log} = \log(1 + \text{latency})
$$

### 4.2 Model
Linear mixed-effects with random intercepts for user and subverse:

$$
\text{latency}_{\log,ijt} = \beta_0 + \beta_1 \text{netvotes}_{t-1} \times \text{habit}_i + \gamma Z + u_i + v_j + \epsilon_{ijt}
$$

## 5. CT Transitions (RQ3)

### 5.1 Outcome Definition
Weekly binary indicator: did the user's mean subverse score increase from week $t-1$ to week $t$?

### 5.2 GLMM vs. GAMM
- **GLMM**: Interpretable fixed effects, random intercept for user
- **GAMM**: Allows nonlinear partial effects via smoothing splines (thin-plate regression splines)

### 5.3 Smooth Terms
All continuous predictors were standardized (z-scores) before entering GAMM smooths:
- `s(mean_trait_lag)`: Cognitive content
- `s(mean_group_lag)`: Group identity content
- `s(mean_score_lag)`: Net votes
- `s(week_index)`: Monotonic time trend
- `s(n_comments)`: Activity volume
- `s(user, bs = "re")`: Random intercept (via penalized spline)

## 6. Validation Strategy

### 6.1 Subverse Scores
- Sampled 30 highest and 30 lowest `sv_score` subverses
- 2 annotators labeled 3 random comments each
- Liberal consensus (≥1 positive) vs. strict consensus (both positive)

### 6.2 Content Features
- PCA on 5 binary features (Action, Actor, Threat, Secrecy, Pattern)
- Varimax rotation; 2 components retained (67% variance)
- Manual annotation on 250-comment subset; Cohen's Kappa computed

## 7. Computational Notes

- **Parallelization**: GAMM fit with `discrete = TRUE` (BAM) for speed
- **Memory**: `tar_option_set(memory = "transient")` to avoid RAM exhaustion
- **Storage**: `format = "qs"` for fast serialization
