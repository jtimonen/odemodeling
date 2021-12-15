#' Create a setup using a 'Stan' file
#'
#' @param file Path to a `.stan` file.
#' @return An object of class `OdeModelSetup`.
setup <- function(file) {
  checkmate::assert_file_exists(file, extension = "stan")
  code <- read_file_lines(file)
  name <- file_name_to_model_name(file)
  stanmodels <- create_cmdstan_models(code)
}
