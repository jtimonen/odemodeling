#' Generate 'Stan' model code
#'
#' @description Generate 'Stan' model code given the declarations of variables,
#' parameters, functions etc.
#' @export
#' @param timepoints A `StanVector` object defining the vector of time points.
#' @param odefun_data Data needed by the ODE function. List of
#' `StanDeclaration` objects.
#' @param odefun_tdata Transformed data needed by the ODE function.
#' A list of `StanTransformation` objects.
#' @param odefun_params Parameters of the ODE function. A list of
#' `StanParameter` objects.
#' @param odefun_tparams Transformations for parameters of the ODE
#' function. A list of `StanTransformation` objects.
#' @param odefun_body ODE function body (Stan code string).
#' @param loglik_data Data needed by the log likelihood function. List of
#' `StanDeclaration` objects.
#' @param loglik_tdata Transformed data needed by the log likelihood function.
#' A list of `StanTransformation` objects.
#' @param loglik_params Parameters of the log likelihood function. A list of
#' `StanParameter` objects.
#' @param loglik_tparams Transformations for parameters of the log likelihood
#' function. A list of `StanTransformation` objects.
#' @param loglik_body Log likelihood function body (Stan code string).
#' @param gqs Generated quantities. A list of `StanTransformation` objects.
#' @return Model code as a string.
#' @family setup functions
generate_stancode <- function(timepoints,
                              odefun_data = list(),
                              odefun_tdata = list(),
                              odefun_params = list(),
                              odefun_tparams = list(),
                              odefun_body = "",
                              loglik_data = list(),
                              loglik_tdata = list(),
                              loglik_params = list(),
                              loglik_tparams = list(),
                              loglik_body = "",
                              gqs = list()) {
  # Argument checks
  checkmate::assert_class(timepoints, "StanVector")
  checkmate::assert_list(odefun_data, "StanDeclaration")
  checkmate::assert_list(odefun_tdata, "StanTransformation")
  checkmate::assert_list(odefun_params, "StanParameter")
  checkmate::assert_list(odefun_tparams, "StanTransformation")
  checkmate::assert_string(odefun_body, min.chars = 0)
  checkmate::assert_list(loglik_data, "StanDeclaration")
  checkmate::assert_list(loglik_tdata, "StanTransformation")
  checkmate::assert_list(loglik_params, "StanParameter")
  checkmate::assert_list(loglik_tparams, "StanTransformation")
  checkmate::assert_string(loglik_body, min.chars = 0)
  checkmate::assert_list(gqs, "StanTransformation")

  # Prior
  prior <- generate_stancode_prior(
    c(odefun_data, loglik_data),
    c(odefun_tdata, loglik_tdata),
    c(odefun_params, loglik_params),
    c(odefun_tparams, loglik_tparams)
  )

  # Simulator
  simulator <- generate_stancode_simulator(
    timepoints,
    odefun_data,
    odefun_tdata,
    odefun_params,
    odefun_tparams,
    odefun_body,
    loglik_data,
    loglik_tdata,
    loglik_params,
    loglik_tparams,
    gqs
  )

  # Posterior
  posterior <- ""

  # Return
  list(prior = prior, simulator = simulator, posterior = posterior)
}


# Generate 'Stan' code for prior model
generate_stancode_prior <- function(data, tdata, params, tparams) {
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


# Generate 'Stan' code for simulator model
generate_stancode_simulator <- function(timepoints,
                                        odefun_data,
                                        odefun_tdata,
                                        odefun_params,
                                        odefun_tparams,
                                        odefun_body,
                                        loglik_data,
                                        loglik_tdata,
                                        loglik_params,
                                        loglik_tparams,
                                        gqs) {
  odefun <- generate_stancode_odefun(
    odefun_data, odefun_tdata, odefun_params,
    odefun_tparams, odefun_body
  )
  code <- generate_block("functions", odefun)
  autoformat_stancode(code)
}


# Generate Stan code for the ODE function
generate_stancode_odefun <- function(odefun_data, odefun_tdata, odefun_params,
                                     odefun_tparams, odefun_body) {
  data_vars <- c(
    odefun_data,
    lapply(odefun_tdata, get_var)
  )
  par_vars <- c(
    lapply(odefun_params, get_var),
    lapply(odefun_tparams, get_var)
  )
  dim_sign <- generate_dim_signatures(c(data_vars, par_vars))
  data_sign <- generate_var_signatures(data_vars, data = TRUE)
  par_sign <- generate_var_signatures(par_vars, data = FALSE)

  signature <- "real t, vector y"
  if (nchar(dim_sign) > 0) {
    signature <- paste0(signature, ", ", dim_sign)
  }
  if (nchar(data_sign) > 0) {
    signature <- paste0(signature, ", ", data_sign)
  }
  if (nchar(par_sign) > 0) {
    signature <- paste0(signature, ", ", par_sign)
  }
  code <- paste0("vector odefun(", signature, ")")
  if (nchar(odefun_body) == 0) {
    warning("ODE function body is empty!")
  }
  paste0(code, "{\n", odefun_body, "\n}\n")
}

# Create the data block
create_data_block <- function(all_vars, data) {
  dims_code <- generate_dim_declarations(all_vars)
  dvars_code <- generate_var_declarations(data)
  generate_block("data", c(dims_code, dvars_code))
}

# Create a transform block (transformed data, transformed params, or gq)
create_transform_block <- function(name, transforms) {
  vars <- lapply(transforms, get_var)
  codes <- paste(lapply(transforms, get_code), collapse = "\n")
  decls <- generate_var_declarations(vars)
  generate_block(name, c(decls, codes))
}

# Create the parameters block
create_params_block <- function(params) {
  vars <- lapply(params, get_var)
  dvars_code <- generate_var_declarations(vars)
  generate_block("parameters", c(dvars_code))
}

# Create the parameters block
create_model_block <- function(params, params_only) {
  codes <- paste(lapply(params, get_prior_code), collapse = "\n")
  if (!params_only) {
    stop("not implemented!")
  }
  generate_block("model", c(codes))
}

# Create a block of Stan code
generate_block <- function(name, parts) {
  body <- ""
  for (p in parts) {
    if (nchar(p) > 0) {
      body <- paste0(body, "\n", p)
    }
  }
  if (nchar(body) == 0) {
    return("")
  }
  paste0(name, " {\n", body, "\n}\n")
}

# Dimensions signatures
generate_dim_signatures <- function(vars) {
  checkmate::assert_list(vars, "StanDeclaration")
  dim_vars <- list()
  for (var in vars) {
    dim_vars <- c(dim_vars, var$get_dims())
  }
  generate_var_signatures(dim_vars, data = TRUE)
}

# Variables declaration code
generate_var_signatures <- function(vars, data) {
  vars <- unique(vars)
  checkmate::assert_list(vars, "StanDeclaration")
  get_signature <- function(x) x$signature()
  if (data) {
    pre <- "data"
  } else {
    pre <- ""
  }
  signs <- unique(sapply(vars, get_signature))
  paste(pre, signs, collapse = ", ")
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
  vars <- unique(vars)
  get_decl <- function(x) {
    x$declaration()
  }
  lines <- sapply(vars, get_decl)
  lines <- unique(lines)
  if (length(lines) == 0) {
    return("")
  }
  paste(lines, ";", sep = "", collapse = "\n")
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
