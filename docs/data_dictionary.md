# Data Dictionary

## Raw Data Files

### `seperated_annotation.csv`
| Column | Type | Description |
|:---|:---|:---|
| `comment_id` | chr | Unique comment identifier (NA for submissions) |
| `submission_id` | chr | Unique submission identifier (NA for comments) |
| `user` | chr | Username |
| `subverse` | chr | Subverse name |
| `date` | date | Post date |
| `time` | time | Post time |
| `upvotes` | int | Upvote count |
| `downvotes` | int | Downvote count |
| `body` | chr | Post text |
| `Action` | lgl | Automated feature: action verb present |
| `Actor` | lgl | Automated feature: actor/agent present |
| `Threat` | lgl | Automated feature: threat language present |
| `Secrecy` | lgl | Automated feature: secrecy language present |
| `Pattern` | lgl | Automated feature: pattern/recurrence language present |

### `user_info.csv`
| Column | Type | Description |
|:---|:---|:---|
| `user` | chr | Username |
| `reg_date` | date | Account registration date |

### `subverse_scores_wa.csv`
| Column | Type | Description |
|:---|:---|:---|
| `subverse` | chr | Subverse name |
| `sv_score` | dbl | Conspiracy embedding score (≥ 0 = CT) |

### `subverse.csv`
| Column | Type | Description |
|:---|:---|:---|
| `subverse` | chr | Subverse name |
| `subscriber_count` | int | Number of subscribers |
| `date_created` | date | Subverse creation date |

## Engineered Variables

### Preprocessing Targets

| Variable | Source | Construction |
|:---|:---|:---|
| `time` | `date` + `time` | `lubridate::ymd_hms(paste(date, time))` |
| `controversy` | `upvotes`, `downvotes` | Reddit formula; 0 if no opposing votes |
| `controversy_group` | `controversy` | "high" if > 0, else "low" |
| `ccp` | `upvotes`, `downvotes` | Cumulative net votes per user-subverse |
| `has_privilege` | `ccp` | 1 if CCP ≥ 10 |
| `privilege` | `has_privilege` trajectory | "never_had", "steady", "fluctuating" |
| `post_gap` | `time` | Hours between consecutive posts per user |
| `burstiness` | `post_gap` | $(\sigma - \mu) / (\sigma + \mu)$ |
| `posts_per_day` | `N` / `active_days` | Comments per active day |
| `habit_class` | `burstiness`, `posts_per_day` | "bursty", "steady regular", "occasional" |
| `tenure_days` | `reg_date` | Days from registration to shutdown |
| `tenure_strata` | `tenure_days` | "short" (<1yr), "mid" (1-3yr), "long" (>3yr) |
| `sv_age_days` | `date_created` | Subverse age at shutdown |
| `sv_growth` | `subscriber_count` / `sv_age_days` | Daily subscriber growth rate |
| `sv_growth_factor` | `sv_growth` quartile | "slow", "medium", "fast" |
| `conspiracy_group` | `sv_score` | "conspiracy" if ≥ 0, else "non-conspiracy" |

### Survival Targets

| Variable | Description |
|:---|:---|
| `first_comment_date` | First post in user-subverse dyad |
| `last_comment_date` | Last post in user-subverse dyad |
| `time_to_event` | Days from first to last (or shutdown) |
| `event_dropout` | 1 = dropout (≥60 days inactive), 0 = censored |

### Latency Targets

| Variable | Description |
|:---|:---|
| `post_latency` | Hours between consecutive posts |
| `post_latency_log` | `log1p(post_latency)` |
| `netvote` | `upvotes - downvotes` |
| `lagged_net_votes` | Previous post's netvote |
| `lagged_net_votes_z` | Standardized lagged net votes |

### Transition Targets

| Variable | Description |
|:---|:---|
| `week` | Floor date to week |
| `mean_trait` | Mean of `trait` (Secrecy & Pattern & Threat) per week |
| `mean_group` | Mean of `group` (Actor & Action) per week |
| `mean_score` | Mean net votes per week |
| `delta_sv` | Week-to-week change in mean `sv_score` |
| `transition_to_ct` | 1 if `delta_sv` > 0, else 0 |
| `transition_to_ct_lag` | Previous week's transition status |
| `mean_trait_lag` | Standardized lagged trait content |
| `mean_group_lag` | Standardized lagged group content |
| `mean_score_lag` | Standardized lagged net votes |
| `week_index` | Standardized monotonic week counter |
