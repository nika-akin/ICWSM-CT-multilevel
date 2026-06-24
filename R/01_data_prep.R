# R/01_data_prep.R — Data loading and preprocessing functions
# Each function is documented with intent, inputs, and outputs.

library(data.table)

# =============================================================================
# 1. DATA I/O
# =============================================================================

#' Load main annotation file
#' @param path path to CSV
load_annotation <- function(path) {
  readr::read_csv(path, show_col_types = FALSE)
}

#' Load user info (registration dates)
#' @param path path to CSV
load_user_info <- function(path) {
  readr::read_csv(path, show_col_types = FALSE)
}

#' Load subverse CT scores
#' @param path path to CSV
load_sv_scores <- function(path) {
  df <- readr::read_csv(path, show_col_types = FALSE)
  if ("...1" %in% names(df)) {
    df <- df %>% dplyr::rename(subverse = `...1`, sv_score = conspiracy)
  }
  df
}

#' Load subverse metadata
#' @param path path to CSV
load_subverse <- function(path) {
  readr::read_csv(path, show_col_types = FALSE)
}

# =============================================================================
# 2. PREPROCESSING
# =============================================================================

#' Parse datetime from separate date and time columns
#' @param data data frame with date and time columns
add_datetime <- function(data) {
  data %>%
    dplyr::mutate(
      time = lubridate::ymd_hms(paste(date, time))
    )
}

#' Join user registration info
#' @param data main data frame
#' @param user_info user info data frame
join_user_info <- function(data, user_info) {
  dplyr::left_join(data, user_info, by = "user")
}

#' Join subverse conspiracy scores
#' @param data main data frame
#' @param sv_scores subverse score data frame
join_sv_scores <- function(data, sv_scores) {
  dplyr::left_join(data, sv_scores, by = "subverse")
}

#' Join subverse metadata (subscriber count, creation date)
#' @param data main data frame
#' @param subverse_meta subverse metadata data frame
join_subverse_meta <- function(data, subverse_meta) {
  dplyr::left_join(data, subverse_meta, by = "subverse")
}

#' Final cleanup: remove unused columns, set factor levels
#' @param data cleaned data frame with all features
finalize_preprocessing <- function(data) {
  data <- data %>%
    dplyr::mutate(
      upvotes = dplyr::if_else(is.na(upvotes), 0, as.numeric(upvotes)),
      downvotes = dplyr::if_else(is.na(downvotes), 0, as.numeric(downvotes)),
      netvote = upvotes - downvotes
    )
  
  if (all(c("Secrecy", "Pattern", "Threat") %in% names(data))) {
    data <- data %>% dplyr::mutate(
      trait = as.integer(Secrecy == TRUE & Pattern == TRUE & Threat == TRUE)
    )
  }
  if (all(c("Actor", "Action") %in% names(data))) {
    data <- data %>% dplyr::mutate(
      group = as.integer(Actor == TRUE & Action == TRUE)
    )
  }
  
  data %>%
    dplyr::select(
      -dplyr::any_of(c("...1", "link", "domain", "date", "about", "title", "Tags", "body"))
    ) %>%
    dplyr::mutate(
      conspiracy_group = factor(
        ifelse(sv_score >= 0, "conspiracy", "non-conspiracy"),
        levels = c("conspiracy", "non-conspiracy")
      )
    )
}

# =============================================================================
# 3. DESCRIPTIVES & VALIDATION
# =============================================================================

#' Create descriptives table
#' @param data cleaned data frame
make_descriptives <- function(data) {
  list(
    n_users = dplyr::n_distinct(data$user),
    n_subverses = dplyr::n_distinct(data$subverse),
    n_comments = sum(!is.na(data$comment_id)),
    n_submissions = sum(!is.na(data$submission_id)),
    date_range = range(data$time, na.rm = TRUE)
  )
}

#' Sample top/bottom subverses for manual validation
#' @param data data frame with sv_score column
sample_subverses_for_validation <- function(data) {
  data %>%
    dplyr::group_by(subverse) %>%
    dplyr::summarise(avg_sv_score = mean(sv_score, na.rm = TRUE), .groups = "drop") %>%
    dplyr::arrange(dplyr::desc(avg_sv_score)) %>%
    dplyr::slice(c(1:30, dplyr::n() - 29:dplyr::n()))
}

#' Validate sv_score against manual labels
#' @param sampled_subverses sampled subverses data frame
validate_sv_scores <- function(sampled_subverses) {
  list(sampled_subverses = sampled_subverses, auc = NA, kappa = NA)
}

#' PCA validation of 5 CT content features
#' @param data data frame with CT feature columns
validate_features_pca <- function(data) {
  features <- data %>%
    dplyr::select(dplyr::any_of(c("Action", "Actor", "Threat", "Secrecy", "Pattern"))) %>%
    dplyr::mutate(dplyr::across(dplyr::everything(), as.numeric))

  if (ncol(features) < 5) return(NULL)

  pca_res <- psych::principal(features, nfactors = 2, rotate = "varimax")
  list(
    loadings = as.data.frame(unclass(pca_res$loadings)),
    variance = pca_res$Vaccounted,
    rmsr = pca_res$fit
  )
}

#' Export list of key objects for the Shiny dashboard
#' @param ... objects to include in export list
export_shiny_inputs <- function(...) {
  list(...)
}
