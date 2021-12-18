#' Generate 'Stan' code for prior model
#'
#' @param data A list of `StanDeclaration` objects.
#' @param tdata A list of `StanTransformation` objects.
#' @param params A list of `StanParameter` objects.
#' @param tparams A list of `StanTransformation` objects.
#' @return A string defining a complete 'Stan' model.
generate_stancode_prior <- function(data = list(),
                                    tdata = list(),
                                    params = list(),
                                    tparams = list()) {
  checkmate::assert_list(data, "StanDeclaration")
  checkmate::assert_list(tdata, "StanTransformation")
  checkmate::assert_list(params, "StanParameter")
  get_var <- function(x) x$var
  all_vars <- c(
    data,
    lapply(tdata, get_var),
    lapply(params, get_var),
    lapply(tparams, get_var)
  )
  data_b <- create_data_block(all_vars, data)
  tdata_b <- create_transform_block("transformed data", tdata)
  pars_b <- create_params_block(params)
  tpars_b <- create_transform_block("transformed parameters", tparams)
  model_b <- create_model_block(params, params_only = TRUE)
  code <- paste(data_b, tdata_b, pars_b, tpars_b, model_b, "\n")
  autoformat_stancode(code)
}

# Create the data block
create_data_block <- function(all_vars, data) {
  dims_code <- generate_dim_declarations(all_vars)
  dvars_code <- generate_var_declarations(data)
  generate_block("data", c(dims_code, dvars_code))
}

# Create a transform block (transformed data, transformed params, or gq)
create_transform_block <- function(name, transforms) {
  if (length(transforms) == 0) {
    return("")
  }
  get_var <- function(x) x$var
  get_code <- function(x) x$code
  vars <- lapply(transforms, get_var)
  codes <- paste(lapply(transforms, get_code), collapse = "\n")
  decls <- generate_var_declarations(vars)
  generate_block(name, c(decls, codes))
}

# Create the parameters block
create_params_block <- function(params) {
  get_var <- function(x) x$var
  vars <- lapply(params, get_var)
  dvars_code <- generate_var_declarations(vars)
  generate_block("parameters", c(dvars_code))
}

# Create the parameters block
create_model_block <- function(params, params_only) {
  get_code <- function(x) x$prior_code
  codes <- paste(lapply(params, get_code), collapse = "\n")
  if (!params_only) {
    stop("not implemented!")
  }
  generate_block("model", c(codes))
}

# Create a block of Stan code
generate_block <- function(name, parts) {
  code <- paste0(name, " {\n")
  for (p in parts) {
    code <- paste0(code, "\n", p)
  }
  code <- paste0(code, "\n}\n")
  return(code)
}

# Dimensions declaration code
generate_dim_declarations <- function(vars) {
  checkmate::assert_list(vars, "StanDeclaration")
  dim_vars <- list()
  for (var in vars) {
    dim_vars <- c(dim_vars, var$get_dims())
  }
  generate_var_declarations(dim_vars)
}

# Variables declaration code
generate_var_declarations <- function(vars) {
  checkmate::assert_list(vars, "StanDeclaration")
  get_decl <- function(x) {
    x$declaration()
  }
  lines <- sapply(vars, get_decl)
  lines <- unique(lines)
  paste(lines, ";", sep = "", collapse = "\n")
}

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
