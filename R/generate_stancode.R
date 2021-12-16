#' Generate full 'Stan' code given missing parts
#'
#' @export
#' @param odefun_add_args ODE function additional arguments (vector of
#' Stan code strings).
#' @param odefun_body ODE function body (Stan code string).
#' @param loglik_add_args Log-likelihood function additional arguments
#' (vector of Stan code strings).
#' @param loglik_body Log-likelihood function bod (Stan code string).
#' @param add_data Code to declare additional data (Stan code string).
#' @param middle_blocks The `transformed data`, `parameters`, and
#' `transformed parameters` blocks (Stan code string).
#' @param prior Code that defines the prior for parameters (Stan code string).
#' @param genquant_decl Declarations of generated quantities (Stan code string).
#' @param genquant Code that computes generated quantities (Stan code string).
#' @return Model code as a string and path to file where the model
#' is saved.
#' @family setup functions
generate_stancode <- function(odefun_add_args,
                              odefun_body,
                              loglik_add_args,
                              loglik_body,
                              add_data = "",
                              middle_blocks = "",
                              prior = "",
                              genquant_decl = "",
                              genquant = "",
                              autoformat = TRUE) {
  odefun_args <- parse_add_args(odefun_add_args)
  loglik_args <- parse_add_args(loglik_add_args)
  odefun_signature <- paste(odefun_add_args, collapse = ", ")
  loglik_signature <- paste(loglik_add_args, collapse = ", ")
  code <- stan_template()
  code <- fill_stancode_part(code, odefun_signature, "__ODEFUN_SIGNATURE__")
  code <- fill_stancode_part(code, odefun_args, "__ODEFUN_ARGS__")
  code <- fill_stancode_part(code, odefun_body, "__ODEFUN_BODY__")
  code <- fill_stancode_part(code, loglik_signature, "__LOGLIK_SIGNATURE__")
  code <- fill_stancode_part(code, loglik_args, "__LOGLIK_ARGS__")
  code <- fill_stancode_part(code, loglik_body, "__LOGLIK_BODY__")
  code <- fill_stancode_part(code, add_data, "__ADD_DATA__")
  code <- fill_stancode_part(code, middle_blocks, "__MIDDLE_BLOCKS__")
  code <- fill_stancode_part(code, prior, "__PRIOR__")
  code <- fill_stancode_part(code, genquant_decl, "__GENQUANT_DECL__")
  code <- fill_stancode_part(code, genquant, "__GENQUANT__")
  code <- paste(code, "\n")

  # Automatic formatting using stanc
  if (autoformat) {
    code <- autoformat_stancode(code)
  }
  return(code)
}

#' Template 'Stan' model code
#'
#' @export
#' @return a string
stan_template <- function() {
  filepath <- system.file("template.stan", package = "odetuner")
  read_file_lines(filepath)
}

# Helper function
fill_stancode_part <- function(code, replacement, placeholder) {
  if (is.null(replacement)) {
    replacement <- paste0(placeholder, " <<<<<<<<<<<< MISSING !")
  }
  gsub(pattern = placeholder, fixed = TRUE, x = code, replacement = replacement)
}

# Stan function additional argument vector (with types)
# to just names of arguments in one string
parse_add_args <- function(add_args) {
  splitter <- function(x) {
    x <- trimws(x)
    words <- strsplit(x, split = " ", fixed = TRUE)[[1]]
    words[length(words)]
  }
  paste(sapply(add_args, splitter), collapse = ", ")
}
