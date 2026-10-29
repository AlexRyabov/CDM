#' Create blocked and stratified cross-validation folds
#'
#' Splits the rows of `data` into cross-validation folds and returns a
#' standard `rsample` resampling object (`rset`), similar to
#' [rsample::group_vfold_cv()]. All rows of one group (block), for example one
#' year or one cruise, are always held out together.
#'
#' * `group` given, `v` a number: groups are distributed over `v` folds. Folds
#'   are balanced by the strata distribution and, with
#'   `balance = "observations"`, by the number of rows (see Details).
#' * `group` given, `v = "leave_block_out"`: every group is its own fold.
#' * `group = NULL`, `v` a number: rows are split at random into `v` folds
#'   with [rsample::vfold_cv()], stratified if `strata` is given.
#' * `group = NULL`, `v = "leave_block_out"`: leave-one-out; every row is its
#'   own fold.
#'
#' Numeric strata with more than five distinct values are binned into
#' quartiles of the rows; other strata are used as categories. Unlike
#' [rsample::group_vfold_cv()], the stratum may vary within a group: the folds
#' are balanced by the stratum distribution of their rows. Strata cannot be
#' used with `"leave_block_out"`.
#'
#' Groups are first assigned in random order, each to the fold that improves
#' the balance most, and then single groups are moved and pairs of groups are
#' swapped between folds while the balance improves. The balance of a fold is
#' the squared Euclidean distance between its stratum proportions and those of
#' all rows; with `balance = "observations"` the squared relative deviation of
#' its number of rows from `nrow(data) / v` is added. With
#' `balance = "groups"` every fold gets the same number of groups (up to one).
#' Use [set.seed()] before the call to make the folds reproducible.
#'
#' @param data A data frame with one row per sample, usually `cdm_data$env`.
#'   Its row names must be the sample IDs, as in `cdm_data$env`, so that the
#'   folds can be matched with the community data.
#' @param group Column with the groups (blocks) whose rows are kept together,
#'   given as a name or a string. `NULL` treats every row as its own group.
#' @param v Number of folds, or `"leave_block_out"` to make every group (or
#'   every row if `group = NULL`) its own fold.
#' @param repeats Number of repeated fold assignments; must be 1 for
#'   `"leave_block_out"`.
#' @param balance `"observations"` to balance the number of rows in the folds,
#'   or `"groups"` to balance the number of groups.
#' @param strata Column used to balance the folds, given as a name or a
#'   string, or `NULL`.
#'
#' @return An `rset` with columns `splits` and `id` (and `id2` for repeats).
#'   For grouped or leave-out folds the class is `cdm_group_vfold_cv`; the
#'   attributes `group`, `strata`, `balance`, `v`, and `repeats` store the
#'   settings.
#'
#' @examples
#' env <- data.frame(
#'   year = rep(2010:2017, each = 5),
#'   month = rep(c(6, 7, 7, 8, 9, 6, 7, 8), each = 5)
#' )
#' rownames(env) <- paste0("s", seq_len(nrow(env)))
#'
#' set.seed(1)
#' folds <- make_cv_folds(env, group = year, v = 4, strata = month)
#' summary(folds)
#'
#' leave_one_year_out <- make_cv_folds(env, group = year, v = "leave_block_out")
#'
#' @export
make_cv_folds <- function(data,
                          group = NULL,
                          v = 5L,
                          repeats = 1L,
                          balance = c("observations", "groups"),
                          strata = NULL) {
  balance <- match.arg(balance)
  if (!is.data.frame(data) || nrow(data) < 2L) {
    stop("data must be a data frame with at least two rows.", call. = FALSE)
  }
  ids <- rownames(data)
  if (anyNA(ids) || any(!nzchar(ids)) || anyDuplicated(ids)) {
    stop("Row names of data must be unique sample IDs.", call. = FALSE)
  }
  group <- resolve_column_argument(rlang::enquo(group), data, "group")
  strata <- resolve_column_argument(rlang::enquo(strata), data, "strata")
  repeats <- validate_count_argument(repeats, "repeats", minimum = 1L)

  leave_block_out <- identical(v, "leave_block_out")
  if (leave_block_out) {
    if (!is.null(strata)) {
      stop("strata cannot be used with v = \"leave_block_out\": every group is its own fold.", call. = FALSE)
    }
    if (repeats != 1L) {
      stop("repeats must be 1 with v = \"leave_block_out\": the folds are fixed.", call. = FALSE)
    }
  } else {
    if (is.character(v)) {
      stop("v must be a number or \"leave_block_out\".", call. = FALSE)
    }
    v <- validate_count_argument(v, "v", minimum = 2L)
    if (is.null(group)) {
      return(rlang::inject(rsample::vfold_cv(data, v = v, repeats = repeats, strata = !!strata)))
    }
  }

  groups <- if (is.null(group)) ids else as.character(data[[group]])
  unique_groups <- unique(groups)
  n_folds <- if (leave_block_out) length(unique_groups) else v
  if (n_folds > length(unique_groups) || n_folds < 2L) {
    stop("v must be between 2 and the number of groups (", length(unique_groups), ").", call. = FALSE)
  }

  stratum <- if (is.null(strata)) rep("all", nrow(data)) else bin_strata(data[[strata]])
  group_strata_counts <- unclass(table(factor(groups, levels = unique_groups), stratum))
  storage.mode(group_strata_counts) <- "double"

  fold_labels <- paste0("Fold", formatC(seq_len(n_folds), width = nchar(n_folds), flag = "0"))
  splits <- list()
  split_ids <- list()
  for (repeat_id in seq_len(repeats)) {
    assignment <- if (leave_block_out) seq_along(unique_groups) else
      assign_groups_to_folds(group_strata_counts, v, balance)
    row_fold <- assignment[match(groups, unique_groups)]
    for (fold in seq_len(n_folds)) {
      splits[[length(splits) + 1L]] <- rsample::make_splits(
        list(analysis = which(row_fold != fold), assessment = which(row_fold == fold)),
        data = data
      )
      split_ids[[length(split_ids) + 1L]] <- c(id = paste0("Repeat", repeat_id), id2 = fold_labels[fold])
    }
  }

  split_ids <- as.data.frame(do.call(rbind, split_ids), stringsAsFactors = FALSE)
  if (repeats == 1L) {
    split_ids <- data.frame(id = split_ids$id2, stringsAsFactors = FALSE)
  }
  rsample::new_rset(
    splits = splits,
    ids = tibble::as_tibble(split_ids),
    attrib = list(v = v, repeats = repeats, group = group, strata = strata, balance = balance),
    subclass = c("cdm_group_vfold_cv", "rset")
  )
}

#' Summarize blocked cross-validation folds
#'
#' Shows for every fold the number of rows and groups, and the stratum
#' distribution of the held-out (`test`) part compared with all rows.
#'
#' @param object A `cdm_group_vfold_cv` object created by [make_cv_folds()].
#' @param ... Not used.
#'
#' @return A list of class `summary.cdm_group_vfold_cv` with the data frames
#'   `folds` and `strata`.
#'
#' @export
summary.cdm_group_vfold_cv <- function(object, ...) {
  data <- object$splits[[1L]]$data
  group <- attr(object, "group")
  strata <- attr(object, "strata")
  stratum <- if (is.null(strata)) rep("all", nrow(data)) else bin_strata(data[[strata]])
  split_names <- if ("id2" %in% names(object)) paste(object$id, object$id2, sep = "_") else object$id

  folds <- do.call(rbind, lapply(seq_len(nrow(object)), function(i) {
    test_rows <- rsample::complement(object$splits[[i]])
    data.frame(
      split = split_names[i],
      n_train = nrow(data) - length(test_rows),
      n_test = length(test_rows),
      groups_test = paste(unique(if (is.null(group)) rownames(data)[test_rows] else
        as.character(data[[group]][test_rows])), collapse = ", "),
      stringsAsFactors = FALSE
    )
  }))
  levels <- sort(unique(stratum))
  strata_table <- do.call(rbind, lapply(seq_len(nrow(object)), function(i) {
    test_rows <- rsample::complement(object$splits[[i]])
    data.frame(split = split_names[i], t(as.matrix(prop.table(table(factor(stratum[test_rows], levels = levels))))),
               check.names = FALSE)
  }))
  overall <- data.frame(split = "all rows", t(as.matrix(prop.table(table(factor(stratum, levels = levels))))),
                        check.names = FALSE)

  result <- list(
    settings = attributes(object)[c("group", "strata", "balance", "v", "repeats")],
    folds = folds,
    strata = rbind(strata_table, overall)
  )
  class(result) <- "summary.cdm_group_vfold_cv"
  result
}

#' @export
print.summary.cdm_group_vfold_cv <- function(x, ...) {
  cat("Blocked cross-validation: v = ", x$settings$v, ", ", nrow(x$folds), " splits, ", x$settings$repeats,
      " repeat(s); group: ", if (is.null(x$settings$group)) "rows" else x$settings$group, "; strata: ",
      if (is.null(x$settings$strata)) "none" else x$settings$strata,
      "; balance: ", x$settings$balance, "\n\n", sep = "")
  print(x$folds, row.names = FALSE)
  if (!is.null(x$settings$strata)) {
    cat("\nStratum proportions in the held-out parts:\n")
    print(x$strata, row.names = FALSE, digits = 2)
  }
  invisible(x)
}

# Returns the column name given as a symbol or a string, or NULL.
resolve_column_argument <- function(quo, data, argument) {
  if (rlang::quo_is_null(quo)) {
    return(NULL)
  }
  column <- if (rlang::quo_is_symbol(quo)) rlang::as_name(quo) else rlang::eval_tidy(quo)
  if (is.null(column)) {
    return(NULL)
  }
  if (!is.character(column) || length(column) != 1L || is.na(column)) {
    stop(argument, " must be a single column name.", call. = FALSE)
  }
  if (!column %in% names(data)) {
    stop("data has no ", argument, " column '", column, "'.", call. = FALSE)
  }
  missing_values <- is.na(data[[column]]) | as.character(data[[column]]) == ""
  if (any(missing_values)) {
    stop(
      "The ", argument, " column '", column, "' has missing or empty values in ",
      sum(missing_values), " rows.",
      call. = FALSE
    )
  }
  column
}

# Numeric strata with more than five distinct values are binned into
# quartiles; other values are used as categories.
bin_strata <- function(x) {
  if (is.numeric(x) && length(unique(x)) > 5L) {
    breaks <- unique(stats::quantile(x, probs = seq(0, 1, by = 0.25)))
    return(as.character(cut(x, breaks = breaks, include.lowest = TRUE)))
  }
  as.character(x)
}

# Assigns groups to v folds. Returns the fold of every group (rows of
# group_strata_counts).
assign_groups_to_folds <- function(group_strata_counts, v, balance) {
  n_groups <- nrow(group_strata_counts)
  global_distribution <- counts_to_distribution(colSums(group_strata_counts))
  target_size <- sum(group_strata_counts) / v
  weight_size <- identical(balance, "observations")

  fold_score <- function(counts) {
    size_term <- if (weight_size) ((sum(counts) - target_size) / target_size)^2 else 0
    sum((counts_to_distribution(counts) - global_distribution)^2) + size_term
  }
  capacities <- if (weight_size) rep(n_groups, v) else {
    capacity <- rep(n_groups %/% v, v)
    extra <- n_groups %% v
    capacity[seq_len(extra)] <- capacity[seq_len(extra)] + 1L
    capacity
  }

  # Greedy start: random group order, the first v groups open the folds.
  assignment <- integer(n_groups)
  fold_counts <- matrix(0, nrow = v, ncol = ncol(group_strata_counts))
  group_order <- sample.int(n_groups)
  for (position in seq_len(n_groups)) {
    g <- group_order[position]
    if (position <= v) {
      best_fold <- position
    } else {
      candidates <- which(tabulate(assignment, nbins = v) < capacities)
      gains <- vapply(candidates, function(fold) {
        fold_score(fold_counts[fold, ] + group_strata_counts[g, ]) - fold_score(fold_counts[fold, ])
      }, numeric(1))
      best_fold <- candidates[which.min(gains)]
    }
    assignment[g] <- best_fold
    fold_counts[best_fold, ] <- fold_counts[best_fold, ] + group_strata_counts[g, ]
  }

  # Local search: move single groups (observations) and swap pairs of groups.
  scores <- apply(fold_counts, 1L, fold_score)
  tolerance <- 1e-12
  improved <- TRUE
  while (improved) {
    improved <- FALSE
    if (weight_size) {
      for (g in seq_len(n_groups)) {
        from <- assignment[g]
        if (sum(assignment == from) == 1L) {
          next
        }
        for (to in setdiff(seq_len(v), from)) {
          new_from <- fold_counts[from, ] - group_strata_counts[g, ]
          new_to <- fold_counts[to, ] + group_strata_counts[g, ]
          new_scores <- c(fold_score(new_from), fold_score(new_to))
          if (sum(new_scores) + tolerance < scores[from] + scores[to]) {
            assignment[g] <- to
            fold_counts[c(from, to), ] <- rbind(new_from, new_to)
            scores[c(from, to)] <- new_scores
            improved <- TRUE
            break
          }
        }
      }
    }
    for (i in seq_len(n_groups - 1L)) {
      for (j in (i + 1L):n_groups) {
        fold_i <- assignment[i]
        fold_j <- assignment[j]
        if (fold_i == fold_j) {
          next
        }
        new_i <- fold_counts[fold_i, ] - group_strata_counts[i, ] + group_strata_counts[j, ]
        new_j <- fold_counts[fold_j, ] - group_strata_counts[j, ] + group_strata_counts[i, ]
        new_scores <- c(fold_score(new_i), fold_score(new_j))
        if (sum(new_scores) + tolerance < scores[fold_i] + scores[fold_j]) {
          assignment[c(i, j)] <- c(fold_j, fold_i)
          fold_counts[c(fold_i, fold_j), ] <- rbind(new_i, new_j)
          scores[c(fold_i, fold_j)] <- new_scores
          improved <- TRUE
        }
      }
    }
  }

  assignment
}

# Converts an rset into a fold table (sample_id, repeat_id, fold) using the
# row names of its data as sample IDs, and checks that it is a valid
# partition: in every repeat each sample is held out exactly once, and the
# rows of a group (if the rset has a group attribute) are held out together.
rset_folds <- function(cv, sample_ids) {
  if (!inherits(cv, "rset") || !"splits" %in% names(cv)) {
    stop("cv must be an rsample resampling object, for example from make_cv_folds().", call. = FALSE)
  }
  data <- cv$splits[[1L]]$data
  data_ids <- rownames(data)
  if (!setequal(data_ids, sample_ids) || length(data_ids) != length(sample_ids)) {
    stop(
      "The rows of the cross-validation data must be the samples of cdm_data, with sample IDs ",
      "as row names. Create the folds from cdm_data$env.",
      call. = FALSE
    )
  }

  repeat_ids <- if ("id2" %in% names(cv)) match(cv$id, unique(cv$id)) else rep(1L, nrow(cv))
  folds <- do.call(rbind, lapply(seq_len(nrow(cv)), function(i) {
    held_out <- rsample::complement(cv$splits[[i]])
    data.frame(
      sample_id = data_ids[held_out],
      repeat_id = rep(repeat_ids[i], length(held_out)),
      fold = rep(sum(repeat_ids[seq_len(i)] == repeat_ids[i]), length(held_out)),
      stringsAsFactors = FALSE
    )
  }))

  for (repeat_id in unique(repeat_ids)) {
    held_out <- folds$sample_id[folds$repeat_id == repeat_id]
    if (length(held_out) != length(data_ids) || anyDuplicated(held_out)) {
      stop(
        "Invalid folds in repeat ", repeat_id, ": every sample must be held out exactly once, but ",
        length(held_out), " held-out rows were found for ", length(data_ids), " samples (",
        sum(duplicated(held_out)), " held out more than once). ",
        "rsample::group_vfold_cv() with strata and pool > 0 can produce such folds.",
        call. = FALSE
      )
    }
  }

  group <- unname(attr(cv, "group"))
  if (!is.null(group) && group %in% names(data)) {
    group_values <- as.character(data[folds$sample_id, group])
    folds_per_group <- tapply(folds$fold, paste(folds$repeat_id, group_values), function(x) length(unique(x)))
    if (any(folds_per_group > 1L)) {
      stop(
        "Invalid folds: groups of '", group, "' are split between folds: ",
        paste(names(folds_per_group)[folds_per_group > 1L], collapse = ", "), ".",
        call. = FALSE
      )
    }
  }

  folds <- folds[order(folds$repeat_id, match(folds$sample_id, sample_ids)), , drop = FALSE]
  rownames(folds) <- NULL
  list(
    folds = folds,
    data = data,
    group = group,
    strata = unname(attr(cv, "strata")),
    balance = attr(cv, "balance")
  )
}

# Returns messages about classes that are missing from the training part of
# a fold. `data` is model data with the class column `.class` and sample IDs
# as row names.
check_missing_classes <- function(folds, data) {
  classes <- levels(data$.class)
  issues <- character()
  for (split in cv_splits(folds)) {
    missing_classes <- setdiff(classes, as.character(data[split$analysis, ".class"]))
    if (length(missing_classes) > 0L) {
      issues <- c(issues, paste0(
        split$id, ": the training part has no samples of class ",
        paste(missing_classes, collapse = ", "), "."
      ))
    }
  }
  issues
}

# Converts a fold table into a list of analysis (training) and assessment
# (held-out) sample IDs.
cv_splits <- function(folds) {
  keys <- unique(folds[c("repeat_id", "fold")])
  keys <- keys[order(keys$repeat_id, keys$fold), , drop = FALSE]
  lapply(seq_len(nrow(keys)), function(i) {
    in_repeat <- folds$repeat_id == keys$repeat_id[i]
    held_out <- in_repeat & folds$fold == keys$fold[i]
    list(
      id = sprintf("Repeat%d_Fold%02d", keys$repeat_id[i], keys$fold[i]),
      repeat_id = keys$repeat_id[i],
      fold = keys$fold[i],
      analysis = folds$sample_id[in_repeat & !held_out],
      assessment = folds$sample_id[held_out]
    )
  })
}

validate_count_argument <- function(x, argument, minimum) {
  if (length(x) != 1L || !is.numeric(x) || !is.finite(x) ||
      abs(x - round(x)) > .Machine$double.eps^0.5 || x < minimum) {
    stop(argument, " must be a single integer >= ", minimum, ".", call. = FALSE)
  }
  as.integer(round(x))
}

counts_to_distribution <- function(counts) {
  total <- sum(counts)
  if (total <= 0) {
    return(rep(0, length(counts)))
  }
  counts / total
}

# Versions of R, CDMr, and the given packages, for provenance records.
package_versions <- function(packages) {
  packages <- unique(c("CDMr", packages))
  versions <- vapply(packages, function(package) {
    tryCatch(as.character(utils::packageVersion(package)), error = function(error) NA_character_)
  }, character(1))
  c(R = paste(R.version$major, R.version$minor, sep = "."), versions)
}
