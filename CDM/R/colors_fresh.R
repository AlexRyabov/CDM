#' Fresh distinct colors for community types
#'
#' Creates a stable set of distinct colors for categorical CDM outputs such as
#' clusters or community types. The first 20 colors are based on a manually
#' curated distinct-color palette; larger palettes are generated from evenly
#' spaced HCL hues.
#'
#' @param x Either a single positive integer giving the number of colors, or a
#'   vector/factor of cluster or community-type labels. If `x` is a factor, its
#'   levels define the color names and order.
#' @param order Palette order. `"my"` uses the custom order from the original
#'   MATLAB helper and sorts selected colors by hue, `"convenient"` keeps a
#'   practical legend order, and `"rainbow"` sorts selected colors by hue after
#'   preferring darker colors.
#' @param format Output format. `"hex"` returns colors as hexadecimal strings;
#'   `"rgb"` returns an RGB matrix with values between 0 and 1.
#'
#' @return A character vector of hex colors, or an RGB matrix when
#'   `format = "rgb"`. If `x` supplies labels, the returned colors are named
#'   with those labels.
#'
#' @examples
#' colors_fresh(4)
#' colors_fresh(factor(c("type_2", "type_1"), levels = c("type_1", "type_2")))
#'
#' @export
colors_fresh <- function(x,
                         order = c("my", "convenient", "rainbow"),
                         format = c("hex", "rgb")) {
  order <- match.arg(order)
  format <- match.arg(format)

  labels <- color_labels(x)
  n <- if (is.null(labels)) as.integer(x) else length(labels)

  validate_color_count(x, n, labels)

  color_matrix <- fresh_color_matrix(n, order)

  if (!is.null(labels)) {
    rownames(color_matrix) <- labels
  }

  if (identical(format, "rgb")) {
    return(color_matrix)
  }

  colors <- grDevices::rgb(
    color_matrix[, 1],
    color_matrix[, 2],
    color_matrix[, 3]
  )

  if (!is.null(labels)) {
    names(colors) <- labels
  }

  colors
}

color_labels <- function(x) {
  if (is.factor(x)) {
    labels <- levels(x)
    labels <- labels[!is.na(labels) & labels != ""]
    return(labels)
  }

  if (is.numeric(x) && length(x) == 1L) {
    return(NULL)
  }

  if (is.null(x) || is.data.frame(x) || !is.atomic(x)) {
    stop(
      "x must be a positive integer count or a vector of labels.",
      call. = FALSE
    )
  }

  values <- x[!is.na(x)]
  if (length(values) == 0L) {
    stop("x must contain at least one non-missing label.", call. = FALSE)
  }

  if (is.numeric(values)) {
    return(as.character(sort(unique(values))))
  }

  sort(unique(as.character(values)))
}

validate_color_count <- function(x, n, labels) {
  if (!is.null(labels)) {
    if (n < 1L) {
      stop("x must contain at least one label.", call. = FALSE)
    }
    return(invisible(TRUE))
  }

  if (
    length(x) != 1L ||
      is.na(x) ||
      !is.finite(x) ||
      x < 1 ||
      x != as.integer(x)
  ) {
    stop("x must be a single positive integer when used as a count.", call. = FALSE)
  }

  invisible(TRUE)
}

fresh_color_matrix <- function(n, order) {
  if (n <= 20L) {
    colors <- switch(
      order,
      my = order_palette_by_hue(my_color_palette()[seq_len(n), , drop = FALSE]),
      convenient = convenient_color_palette()[seq_len(n), , drop = FALSE],
      rainbow = rainbow_ordered_palette(n)
    )

    return(colors)
  }

  generated_colors <- grDevices::hcl(
    h = seq(15, 375, length.out = n + 1L)[seq_len(n)],
    c = 100,
    l = 65,
    fixup = TRUE
  )

  t(grDevices::col2rgb(generated_colors)) / 255
}

order_palette_by_hue <- function(colors) {
  hsv <- grDevices::rgb2hsv(t(colors))
  colors[order(hsv["h", ]), , drop = FALSE]
}

rainbow_ordered_palette <- function(n) {
  colors <- convenient_color_palette()
  hsv <- grDevices::rgb2hsv(t(colors))
  colors <- colors[order(hsv["v", ])[seq_len(n)], , drop = FALSE]
  order_palette_by_hue(colors)
}

my_color_palette <- function() {
  matrix(
    c(
      204, 51, 0,
      0, 153, 255,
      245, 130, 48,
      145, 30, 180,
      60, 180, 75,
      170, 110, 40,
      255, 200, 25,
      230, 190, 255,
      210, 245, 60,
      230, 25, 75,
      70, 240, 240,
      240, 50, 230,
      250, 190, 212,
      255, 250, 200,
      0, 128, 128,
      170, 255, 195,
      0, 0, 128,
      128, 128, 0,
      255, 215, 180,
      0, 130, 200
    ),
    ncol = 3,
    byrow = TRUE
  ) / 255
}

convenient_color_palette <- function() {
  matrix(
    c(
      230, 25, 75,
      60, 180, 75,
      255, 200, 25,
      0, 0, 128,
      245, 130, 48,
      145, 30, 180,
      70, 240, 240,
      240, 50, 230,
      210, 245, 60,
      250, 190, 212,
      128, 0, 0,
      230, 190, 255,
      170, 110, 40,
      255, 250, 200,
      0, 128, 128,
      170, 255, 195,
      128, 128, 0,
      255, 215, 180,
      0, 130, 200,
      128, 128, 128
    ),
    ncol = 3,
    byrow = TRUE
  ) / 255
}
