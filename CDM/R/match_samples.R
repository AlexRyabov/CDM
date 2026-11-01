# Returns, for every row of `rows`, the matching row number of `data`. The
# tables are matched by sample_id when both have this column, otherwise by
# order, which needs the same number of rows. With same_samples = TRUE both
# tables must contain the same samples; otherwise every sample of `rows` must
# be in `data`, and extra rows of `data` are ignored.
match_sample_rows <- function(rows, data, rows_label, data_label, same_samples = FALSE) {
  if ("sample_id" %in% names(rows) && "sample_id" %in% names(data)) {
    row_ids <- id_text(rows$sample_id)
    data_ids <- id_text(data$sample_id)
    check_ids(row_ids, paste0("The sample_id column of ", rows_label))
    check_ids(data_ids, paste0("The sample_id column of ", data_label))
    if (same_samples) {
      report_unmatched(row_ids, data_ids, rows_label, data_label, "sample")
    } else {
      missing_ids <- setdiff(row_ids, data_ids)
      if (length(missing_ids) > 0L) {
        stop(
          data_label, " is missing ", length(missing_ids), " samples of ", rows_label, ": ",
          paste(utils::head(missing_ids, 10L), collapse = ", "),
          if (length(missing_ids) > 10L) ", ...", ".",
          call. = FALSE
        )
      }
    }
    return(match(row_ids, data_ids))
  }
  if (nrow(rows) != nrow(data)) {
    stop(
      rows_label, " and ", data_label, " do not both have a sample_id column, so their rows are ",
      "matched by order and must have the same number (", nrow(rows), " and ", nrow(data), ").",
      call. = FALSE
    )
  }
  seq_len(nrow(rows))
}

# Sample IDs of a table: its sample_id column, otherwise the row numbers.
table_sample_ids <- function(data) {
  if (!"sample_id" %in% names(data)) {
    return(as.character(seq_len(nrow(data))))
  }
  ids <- id_text(data$sample_id)
  check_ids(ids, "The sample_id column")
  ids
}
