# _targets.R — Reproducible pipeline for Voat CT Analysis
# Run: targets::tar_make()

library(targets)
library(tarchetypes)

# Package dependencies for the pipeline
tar_option_set(
  packages = c(
    "tidyverse", "lubridate", "lme4", "survival", "survminer", "mgcv",
    "gratia", "slider", "data.table", "broom", "ggpubr", "patchwork",
    "ggeffects", "sjPlot", "forcats", "tidyr", "irr", "pROC", "caret",
    "psych", "hexbin", "gridExtra", "readr", "jtools", "report", "ggplot2",
    "quarto", "withr", "here", "dtplyr"
  ),
  format = "qs",           # fast serialization via qs
  memory = "transient",    # unload memory after each target
  garbage_collection = TRUE,
  seed = 42
)

# Source all R/ files
tar_source("R/")

# =============================================================================
# PIPELINE
# =============================================================================
list(
  # ===========================================================================
  # SECTION 1: Raw Data & I/O (4 targets)
  # ===========================================================================
  tar_target(raw_annotation_file, "data/seperated_annotation.csv", format = "file"),
  tar_target(raw_user_info_file,  "data/user_info.csv",           format = "file"),
  tar_target(raw_sv_scores_file,   "data/subverse_scores_wa.csv",    format = "file"),
  tar_target(raw_subverse_file,    "data/subverse.csv",              format = "file"),

  # ===========================================================================
  # SECTION 2: Load Raw Data (4 targets)
  # ===========================================================================
  tar_target(raw_annotation, load_annotation(raw_annotation_file)),
  tar_target(raw_user_info,  load_user_info(raw_user_info_file)),
  tar_target(raw_sv_scores,  load_sv_scores(raw_sv_scores_file)),
  tar_target(raw_subverse,   load_subverse(raw_subverse_file)),

  # ===========================================================================
  # SECTION 3: Preprocessing (8 targets)
  # ===========================================================================
  tar_target(data_datetime,   add_datetime(raw_annotation)),
  tar_target(data_user_join,  join_user_info(data_datetime, raw_user_info)),
  tar_target(data_sv_join,    join_sv_scores(data_user_join, raw_sv_scores)),
  tar_target(data_meta_join,  join_subverse_meta(data_sv_join, raw_subverse)),
  tar_target(data_controversy, calculate_controversy(data_meta_join)),
  tar_target(data_privilege,  calculate_privilege(data_controversy)),
  tar_target(data_habit,      calculate_habit(data_privilege)),
  tar_target(data_tenure,     calculate_tenure(data_habit)),
  tar_target(data_growth,     calculate_subverse_growth(data_tenure)),
  tar_target(data_clean,      finalize_preprocessing(data_growth)),

  # ===========================================================================
  # SECTION 4: Descriptives & Validation (4 targets)
  # ===========================================================================
  tar_target(descriptives_table, make_descriptives(data_clean)),
  tar_target(validation_sv_sample, sample_subverses_for_validation(data_clean)),
  tar_target(validation_sv_results, validate_sv_scores(validation_sv_sample)),
  tar_target(validation_features_pca, validate_features_pca(data_clean)),

  # ===========================================================================
  # SECTION 5: RQ1 — Survival Analysis (8 targets)
  # ===========================================================================
  tar_target(survival_data, prepare_survival_data(data_clean)),
  tar_target(survival_privilege, prepare_privilege_trajectory(data_clean)),
  tar_target(survival_merged, merge_privilege_survival(survival_data, survival_privilege)),
  tar_target(km_logrank_tests, run_logrank_tests(survival_merged)),
  tar_target(km_plots, plot_kaplan_meier(survival_merged)),
  tar_target(cox_long_data, prepare_cox_long_data(data_clean, survival_merged)),
  tar_target(cox_model, fit_cox_timevarying(cox_long_data)),
  tar_target(cox_diagnostics, diagnose_cox(cox_model, cox_long_data)),
  tar_target(cox_plots, plot_cox_results(cox_model, cox_long_data)),

  # ===========================================================================
  # SECTION 6: RQ2 — Posting Latency (4 targets)
  # ===========================================================================
  tar_target(latency_data, prepare_latency_data(data_clean)),
  tar_target(latency_model, fit_latency_lmm(latency_data)),
  tar_target(latency_diagnostics, diagnose_latency(latency_model, latency_data)),
  tar_target(latency_plots, plot_latency_results(latency_model, latency_data)),

  # ===========================================================================
  # SECTION 7: RQ3 — CT Transitions (8 targets)
  # ===========================================================================
  tar_target(transition_weekly, aggregate_weekly(data_clean)),
  tar_target(transition_data, prepare_transition_data(transition_weekly, data_habit)),
  tar_target(glmm_model, fit_transition_glmm(transition_data)),
  tar_target(gamm_model, fit_transition_gamm(transition_data)),
  tar_target(glmm_plots, plot_glmm_results(glmm_model, transition_data)),
  tar_target(gamm_smooths, plot_gamm_smooths(gamm_model)),
  tar_target(gamm_marginal, compute_gamm_marginals(gamm_model, transition_data)),
  tar_target(gamm_contour, plot_gamm_contour(gamm_model, transition_data)),

  # ===========================================================================
  # SECTION 8: Reports (2 targets)
  # ===========================================================================
  #tar_quarto(main_report, "reports/corrected_report.qmd"),
  #tar_quarto(supplementary_report, "reports/supplementary.qmd"),

  # ===========================================================================
  # SECTION 9: Shiny Export (1 target — list of objects for shiny)
  # ===========================================================================
  tar_target(shiny_export, export_shiny_inputs(
    descriptives_table, km_plots, cox_plots, latency_plots,
    gamm_marginal, gamm_contour, data_clean
  ), format = "rds")
)
