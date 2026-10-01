#' Create blocked and stratified cross-validation folds for CDM data
#'
#' Builds fold assignments for downstream machine-learning workflows. Samples
#' can be blocked by a grouping variable, so all samples from the same group are
#' assigned to one fold. Fold assignment is stratified by another variable using
#' squared Euclidean distance between each fold's stratum distribution and the
#' global stratum distribution.
#'
#' @param cdm_data An object created by `prepare_data()`.
#' @param n_folds Number of cross-validation folds.
#' @param group_by Optional blocking variable. Use `NULL` for sample-level
#'   folds, a single column name from `cdm_data$env`, or a vector with one value
#'   per sample.
#' @param stratify_by Optional stratification variable. Use `NULL` for no
#'   stratification, a single column name from `cdm_data$env`, or a vector with
#'   one value per sample.
#'
#' @return An object of class `cdm_ml_cv`, containing sample-level fold
#'   assignments, group-level fold assignments, fold stratum distributions, fold
#'   summaries, and the overall squared Euclidean stratification score.
#'
#' @export
cdm_ml_cv <- function(cdm_data, n_folds = 5L, group_by = NULL, stratify_by = NULL) {
  cdm_data <- validate_ml_cv_cdm_data(cdm_data)
  K <- validate_ml_cv_n_folds(n_folds)

  sample_ids <- cdm_data$sample_names
  env <- cdm_data$env[sample_ids, , drop = FALSE]

  group_values <- resolve_ml_cv_variable(
    group_by,
    env = env,
    sample_ids = sample_ids,
    argument = "group_by",
    default = sample_ids
  )
  strata_values <- resolve_ml_cv_variable(
    stratify_by,
    env = env,
    sample_ids = sample_ids,
    argument = "stratify_by",
    default = rep("all", length(sample_ids))
  )

  group_levels <- ordered_ml_cv_levels(group_values)
  strata_levels <- ordered_ml_cv_levels(strata_values)

  sample_info <- data.frame(
    sample_id = sample_ids,
    group = factor(as.character(group_values), levels = group_levels),
    stratum = factor(as.character(strata_values), levels = strata_levels),
    stringsAsFactors = FALSE
  )

  group_strata_counts <- table(sample_info$group, sample_info$stratum)
  group_strata_counts <- as.matrix(group_strata_counts)
  storage.mode(group_strata_counts) <- "double"

  if (K > nrow(group_strata_counts)) {
    stop(
      "n_folds cannot be larger than the number of blocking units: ",
      nrow(group_strata_counts),
      call. = FALSE
    )
  }

  global_counts <- colSums(group_strata_counts)
  global_distribution <- counts_to_distribution(global_counts)
  group_sample_counts <- rowSums(group_strata_counts)
  group_distributions <- t(apply(group_strata_counts, 1L, counts_to_distribution))
  group_difficulty <- group_sample_counts *
    apply(group_distributions, 1L, squared_euclidean_distance, global_distribution)

  group_order <- order(
    -group_difficulty,
    -group_sample_counts,
    rownames(group_strata_counts)
  )
  capacities <- fold_group_capacities(nrow(group_strata_counts), K)

  assignment <- assign_ml_cv_groups(
    group_strata_counts = group_strata_counts,
    group_order = group_order,
    global_distribution = global_distribution,
    capacities = capacities
  )
  assignment <- optimize_ml_cv_assignment(
    assignment = assignment,
    group_strata_counts = group_strata_counts,
    global_distribution = global_distribution
  )

  fold_labels <- sprintf("Fold%02d", seq_len(K))
  names(assignment) <- rownames(group_strata_counts)

  sample_fold <- unname(assignment[as.character(sample_info$group)])
  sample_folds <- data.frame(
    sample_id = sample_info$sample_id,
    fold = sample_fold,
    id = fold_labels[sample_fold],
    group = as.character(sample_info$group),
    stratum = as.character(sample_info$stratum),
    stringsAsFactors = FALSE
  )

  group_folds <- data.frame(
    group = names(assignment),
    fold = as.integer(assignment),
    id = fold_labels[assignment],
    n_samples = as.integer(group_sample_counts[names(assignment)]),
    stringsAsFactors = FALSE
  )
  group_folds <- group_folds[order(group_folds$fold, group_folds$group), , drop = FALSE]
  rownames(group_folds) <- NULL

  fold_counts <- fold_strata_counts(assignment, group_strata_counts, K)
  fold_distances <- fold_cv_scores(fold_counts, global_distribution)

  fold_strata <- fold_strata_table(
    fold_counts = fold_counts,
    global_counts = global_counts,
    fold_labels = fold_labels
  )

  fold_summary <- data.frame(
    fold = seq_len(K),
    id = fold_labels,
    n_groups = as.integer(tabulate(assignment, nbins = K)),
    n_samples = as.integer(rowSums(fold_counts)),
    strata_distance = as.numeric(fold_distances),
    stringsAsFactors = FALSE
  )

  score <- list(
    metric = "squared_euclidean",
    total_distance = sum(fold_distances),
    mean_distance = mean(fold_distances)
  )

  result <- list(
    sample_folds = sample_folds,
    group_folds = group_folds,
    fold_strata = fold_strata,
    fold_summary = fold_summary,
    score = score
  )
  class(result) <- "cdm_ml_cv"
  result
}

validate_ml_cv_cdm_data <- function(cdm_data) {
  if (!inherits(cdm_data, "cdm_data")) {
    stop("cdm_data must be an object created by prepare_data().", call. = FALSE)
  }

  required_fields <- c("env", "sample_names")
  missing_fields <- setdiff(required_fields, names(cdm_data))
  if (length(missing_fields) > 0L) {
    stop(
      "cdm_data is missing fields: ",
      paste(missing_fields, collapse = ", "),
      call. = FALSE
    )
  }

  if (!is.data.frame(cdm_data$env)) {
    stop("cdm_data$env must be a data frame.", call. = FALSE)
  }

  sample_ids <- cdm_data$sample_names
  if (!is.character(sample_ids) ||
      any(is.na(sample_ids) | sample_ids == "") ||
      anyDuplicated(sample_ids)) {
    stop("cdm_data$sample_names must contain unique non-empty sample IDs.", call. = FALSE)
  }

  env_ids <- rownames(cdm_data$env)
  if (is.null(env_ids) ||
      any(is.na(env_ids) | env_ids == "") ||
      anyDuplicated(env_ids)) {
    stop("cdm_data$env must have unique non-empty sample IDs as row names.", call. = FALSE)
  }

  missing_env_ids <- setdiff(sample_ids, env_ids)
  extra_env_ids <- setdiff(env_ids, sample_ids)
  if (length(env_ids) != length(sample_ids) ||
      length(missing_env_ids) > 0L ||
      length(extra_env_ids) > 0L) {
    stop(
      "Sample IDs in cdm_data$env must match cdm_data$sample_names. Missing: ",
      paste(missing_env_ids, collapse = ", "),
      ". Extra: ",
      paste(extra_env_ids, collapse = ", "),
      call. = FALSE
    )
  }

  cdm_data
}

validate_ml_cv_n_folds <- function(n_folds) {
  if (length(n_folds) != 1L ||
      is.na(n_folds) ||
      !is.numeric(n_folds) ||
      !is.finite(n_folds) ||
      n_folds < 2 ||
      abs(n_folds - round(n_folds)) > .Machine$double.eps^0.5) {
    stop("n_folds must be a single integer greater than or equal to 2.", call. = FALSE)
  }

  as.integer(round(n_folds))
}

resolve_ml_cv_variable <- function(x, env, sample_ids, argument, default) {
  if (is.null(x)) {
    return(default)
  }

  if (is.character(x) && length(x) == 1L && x %in% names(env)) {
    values <- env[[x]]
  } else if (is.atomic(x) || is.factor(x)) {
    values <- x
    value_names <- names(values)
    if (!is.null(value_names)) {
      if (any(is.na(value_names) | value_names == "") || anyDuplicated(value_names)) {
        stop(argument, " names must be unique non-empty sample IDs.", call. = FALSE)
      }
      missing_names <- setdiff(sample_ids, value_names)
      extra_names <- setdiff(value_names, sample_ids)
      if (length(missing_names) > 0L || length(extra_names) > 0L) {
        stop(
          argument,
          " names must match cdm_data$sample_names. Missing: ",
          paste(missing_names, collapse = ", "),
          ". Extra: ",
          paste(extra_names, collapse = ", "),
          call. = FALSE
        )
      }
      values <- values[sample_ids]
    }
  } else {
    stop(
      argument,
      " must be NULL, a column name in cdm_data$env, or a vector with one value per sample.",
      call. = FALSE
    )
  }

  if (length(values) != length(sample_ids)) {
    stop(
      argument,
      " must have one value for each sample in cdm_data.",
      call. = FALSE
    )
  }

  values_as_character <- as.character(values)
  if (any(is.na(values_as_character) | values_as_character == "")) {
    stop(argument, " must not contain missing or empty values.", call. = FALSE)
  }

  values
}

ordered_ml_cv_levels <- function(values) {
  if (is.factor(values)) {
    return(levels(droplevels(values)))
  }

  values <- as.character(values)
  unique_values <- unique(values)
  suppressWarnings(numeric_values <- as.numeric(unique_values))
  if (!anyNA(numeric_values)) {
    return(unique_values[order(numeric_values)])
  }

  unique_values
}

counts_to_distribution <- function(counts) {
  total <- sum(counts)
  if (total <= 0) {
    return(rep(0, length(counts)))
  }

  counts / total
}

squared_euclidean_distance <- function(p, q) {
  sum((p - q)^2)
}

fold_group_capacities <- function(n_groups, K) {
  base_capacity <- n_groups %/% K
  extra_capacity <- n_groups %% K
  capacities <- rep(base_capacity, K)
  if (extra_capacity > 0L) {
    capacities[seq_len(extra_capacity)] <- capacities[seq_len(extra_capacity)] + 1L
  }

  capacities
}

assign_ml_cv_groups <- function(group_strata_counts,
                                group_order,
                                global_distribution,
                                capacities) {
  K <- length(capacities)
  assignment <- integer(nrow(group_strata_counts))
  fold_counts <- matrix(0, nrow = K, ncol = ncol(group_strata_counts))
  colnames(fold_counts) <- colnames(group_strata_counts)

  initial_groups <- group_order[seq_len(K)]
  for (fold in seq_len(K)) {
    group_index <- initial_groups[fold]
    assignment[group_index] <- fold
    fold_counts[fold, ] <- group_strata_counts[group_index, ]
  }

  for (group_index in group_order[-seq_len(K)]) {
    candidate_folds <- which(tabulate(assignment, nbins = K) < capacities)
    candidate_scores <- vapply(candidate_folds, function(fold) {
      candidate_counts <- fold_counts
      candidate_counts[fold, ] <- candidate_counts[fold, ] + group_strata_counts[group_index, ]
      total_cv_score(candidate_counts, global_distribution)
    }, numeric(1))

    candidate_sample_counts <- vapply(candidate_folds, function(fold) {
      sum(fold_counts[fold, ]) + sum(group_strata_counts[group_index, ])
    }, numeric(1))

    best_position <- order(candidate_scores, candidate_sample_counts, candidate_folds)[1L]
    best_fold <- candidate_folds[best_position]
    assignment[group_index] <- best_fold
    fold_counts[best_fold, ] <- fold_counts[best_fold, ] + group_strata_counts[group_index, ]
  }

  assignment
}

optimize_ml_cv_assignment <- function(assignment,
                                      group_strata_counts,
                                      global_distribution) {
  K <- max(assignment)
  fold_counts <- fold_strata_counts(assignment, group_strata_counts, K)
  fold_scores <- fold_cv_scores(fold_counts, global_distribution)
  current_score <- sum(fold_scores)
  tolerance <- 1e-12
  improved <- TRUE

  while (improved) {
    improved <- FALSE

    for (i in seq_len(length(assignment) - 1L)) {
      for (j in (i + 1L):length(assignment)) {
        fold_i <- assignment[i]
        fold_j <- assignment[j]
        if (fold_i == fold_j) {
          next
        }

        new_fold_i_counts <- fold_counts[fold_i, ] -
          group_strata_counts[i, ] +
          group_strata_counts[j, ]
        new_fold_j_counts <- fold_counts[fold_j, ] -
          group_strata_counts[j, ] +
          group_strata_counts[i, ]

        new_fold_i_score <- squared_euclidean_distance(
          counts_to_distribution(new_fold_i_counts),
          global_distribution
        )
        new_fold_j_score <- squared_euclidean_distance(
          counts_to_distribution(new_fold_j_counts),
          global_distribution
        )

        new_score <- current_score -
          fold_scores[fold_i] -
          fold_scores[fold_j] +
          new_fold_i_score +
          new_fold_j_score

        if (new_score + tolerance < current_score) {
          assignment[i] <- fold_j
          assignment[j] <- fold_i
          fold_counts[fold_i, ] <- new_fold_i_counts
          fold_counts[fold_j, ] <- new_fold_j_counts
          fold_scores[fold_i] <- new_fold_i_score
          fold_scores[fold_j] <- new_fold_j_score
          current_score <- new_score
          improved <- TRUE
        }
      }
    }
  }

  assignment
}

fold_strata_counts <- function(assignment, group_strata_counts, K) {
  fold_counts <- matrix(
    0,
    nrow = K,
    ncol = ncol(group_strata_counts),
    dimnames = list(NULL, colnames(group_strata_counts))
  )

  for (fold in seq_len(K)) {
    group_indices <- which(assignment == fold)
    fold_counts[fold, ] <- colSums(group_strata_counts[group_indices, , drop = FALSE])
  }

  fold_counts
}

fold_cv_scores <- function(fold_counts, global_distribution) {
  fold_distributions <- matrix(
    0,
    nrow = nrow(fold_counts),
    ncol = ncol(fold_counts)
  )
  for (fold in seq_len(nrow(fold_counts))) {
    fold_distributions[fold, ] <- counts_to_distribution(fold_counts[fold, ])
  }

  apply(fold_distributions, 1L, squared_euclidean_distance, global_distribution)
}

total_cv_score <- function(fold_counts, global_distribution) {
  sum(fold_cv_scores(fold_counts, global_distribution))
}

fold_strata_table <- function(fold_counts, global_counts, fold_labels) {
  fold_totals <- rowSums(fold_counts)
  global_distribution <- counts_to_distribution(global_counts)

  out <- data.frame(
    fold = rep(seq_len(nrow(fold_counts)), each = ncol(fold_counts)),
    id = rep(fold_labels, each = ncol(fold_counts)),
    stratum = rep(colnames(fold_counts), times = nrow(fold_counts)),
    n = as.integer(as.vector(t(fold_counts))),
    proportion = as.vector(t(fold_counts / fold_totals)),
    global_n = rep(as.integer(global_counts), times = nrow(fold_counts)),
    global_proportion = rep(as.numeric(global_distribution), times = nrow(fold_counts)),
    stringsAsFactors = FALSE
  )

  out$proportion[!is.finite(out$proportion)] <- 0
  out
}
