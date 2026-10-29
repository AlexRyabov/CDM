#' @keywords internal
"_PACKAGE"

# A saved cdm_model is predicted with S3 methods of other packages: bake()
# methods for the recipe steps (important, themis), predict() for e1071 `svm`
# objects and for parsnip `model_fit` objects (random forest, ranger). These
# methods exist only when the packages' namespaces are loaded, so the imports
# below load them together with CDMr; otherwise a saved model could not be
# predicted in a fresh R session.
#' @importFrom rlang .data
#' @importFrom e1071 svm
#' @importFrom important step_predictor_best
#' @importFrom themis step_smotenc
#' @importFrom parsnip fit_xy
#' @importFrom ranger ranger
NULL
