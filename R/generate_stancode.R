# Generate 'Stan' code for prior model
generate_stancode_prior <- function(odefun_vars, loglik_vars, other_vars,
                                    compile) {
  all_vars <- c(odefun_vars, loglik_vars)
  all_decls <- lapply(all_vars, get_decl)
  dims <- dims_of_decls(all_decls)
  tdata <- all_vars[sapply(all_vars, is_tdata)]
  params <- all_vars[sapply(all_vars, is_param)]
  tparams <- all_vars[sapply(all_vars, is_tparam)]
  data_b <- generate_data_block(all_decls, list()) # just dimensions
  tdata_b <- generate_transform_block("transformed data", tdata)
  pars_b <- generate_params_block(params)
  tpars_b <- generate_transform_block("transformed parameters", tparams)
  model_b <- generate_model_block(params, params_only = TRUE)
  code <- paste(data_b, tdata_b, pars_b, tpars_b, model_b, sep = "\n")
  code <- autoformat_stancode(code)

  # Return
  data <- NULL
  gqs <- NULL
  StanModelWithCode$new(code, dims, data, tdata, params, tparams, gqs, compile)
}

# Generate 'Stan' code for posterior model
generate_stancode_posterior <- function(N,
                                        odefun_vars,
                                        odefun_body,
                                        odefun_init,
                                        loglik_vars,
                                        loglik_body,
                                        other_vars,
                                        compile) {

  # Function signatures
  odefun_add_signature <- generate_add_signature(odefun_vars, FALSE)
  odefun_add_args <- generate_add_signature(odefun_vars, TRUE)
  so_args <- "solver, rel_tol, abs_tol, max_num_steps, num_steps, x0, t0, t"
  solve_ode_args <- append_to_signature(so_args, odefun_add_args)
  loglik_add_signature <- generate_add_signature(loglik_vars, FALSE)
  loglik_add_args <- generate_add_signature(loglik_vars, TRUE)
  loglik_args <- append_to_signature("x_ode", loglik_add_args)

  # All vars and their declarations
  base_vars <- list(
    stan_var("t0"), stan_array("t", dims = list(N)), odefun_init
  )
  solver_vars <- list(
    stan_var("abs_tol", lower = 0),
    stan_var("rel_tol", lower = 0),
    stan_var("max_num_steps", lower = 0, type = "int"),
    stan_var("num_steps", lower = 0, type = "int"),
    stan_var("solver", lower = 0, type = "int")
  )
  D <- get_dims(odefun_init)[[1]]
  x_ode <- stan_transform(
    decl = stan_vector_array("x_ode", dims = list(N), length = D),
    origin = "param",
    code = paste0("x_ode = solve_ode(", solve_ode_args, ");")
  )
  log_lik <- stan_transform(
    decl = stan_var("log_lik", "real"),
    origin = "param",
    code = paste0("log_lik = log_likelihood(", loglik_args, ");")
  )
  other_vars <- c(other_vars, list(x_ode, log_lik))
  all_vars <- c(odefun_vars, loglik_vars, other_vars, base_vars, solver_vars)
  all_vars <- unique(all_vars)
  all_decls <- lapply(all_vars, get_decl)
  dims <- dims_of_decls(all_decls)

  # Create most blocks
  data <- all_vars[sapply(all_vars, is_data)]
  tdata <- all_vars[sapply(all_vars, is_tdata)]
  params <- all_vars[sapply(all_vars, is_param)]
  tparams <- all_vars[sapply(all_vars, is_tparam)]
  gqs <- all_vars[sapply(all_vars, is_gq)]
  data_b <- generate_data_block(all_decls, data)
  tdata_b <- generate_transform_block("transformed data", tdata)
  pars_b <- generate_params_block(params)
  tpars_b <- generate_transform_block("transformed parameters", tparams)
  model_b <- generate_model_block(params, params_only = FALSE)
  gq_b <- generate_transform_block("generated quantities", gqs)

  # Functions block
  funs_b <- generate_functions_block(
    odefun_add_signature,
    odefun_add_args,
    odefun_body,
    loglik_add_signature,
    loglik_add_args,
    loglik_body
  )

  # Merge the blocks
  code <- paste(
    funs_b, data_b, tdata_b, pars_b, tpars_b, model_b, gq_b,
    sep = "\n"
  )
  code <- autoformat_stancode(code)

  # Return
  StanModelWithCode$new(code, dims, data, tdata, params, tparams, gqs, compile)
}

# Create the functions block
generate_functions_block <- function(odefun_add_sign,
                                     odefun_add_args,
                                     odefun_body,
                                     loglik_add_sign,
                                     loglik_add_args,
                                     loglik_body) {
  odefun <- generate_odefun(odefun_add_sign, odefun_body)
  loglik <- generate_loglik(loglik_add_sign, loglik_body)
  solvers <- functions_template()
  solvers <- fill_stancode_part(solvers, odefun_add_sign, "__ODEFUN_SIGN__")
  solvers <- fill_stancode_part(solvers, odefun_add_args, "__ODEFUN_ARGS__")
  code <- generate_block("functions", c(odefun, solvers, loglik))
  autoformat_stancode(code)
}

# Create additional signature for function
generate_add_signature <- function(all_vars, argmode) {
  no_add_signature <- function(x) {
    name <- get_name(x)
    is_noadd <- name %in% c("t", "t0", "x0")
    return(!is_noadd)
  }
  all_vars <- all_vars[sapply(all_vars, no_add_signature)]
  all_vars <- unique(all_vars)

  data <- all_vars[sapply(all_vars, is_data)]
  tdata <- all_vars[sapply(all_vars, is_tdata)]
  params <- all_vars[sapply(all_vars, is_param)]
  tparams <- all_vars[sapply(all_vars, is_tparam)]

  data_vars <- c(data, tdata)
  par_vars <- c(params, tparams)
  data_decls <- lapply(data_vars, get_decl)
  par_decls <- lapply(par_vars, get_decl)
  dim_sign <- generate_dim_signatures(c(data_decls, par_decls), argmode)
  data_sign <- generate_var_signatures(data_decls, TRUE, argmode)
  par_sign <- generate_var_signatures(par_decls, FALSE, argmode)

  signature <- ""
  signature <- append_to_signature(signature, dim_sign)
  signature <- append_to_signature(signature, data_sign)
  signature <- append_to_signature(signature, par_sign)
  trimws(signature)
}

# Generate Stan code for the ODE function
generate_odefun <- function(add_signature, odefun_body) {
  signature <- "real t, vector y"
  signature <- append_to_signature(signature, add_signature)
  comment <- "\n// ODE system right-hand side \n"
  code <- paste0(comment, "vector odefun(", signature, ")")
  if (nchar(odefun_body) == 0) {
    warning("ODE function body is empty!")
  }
  paste0(code, "{\n", odefun_body, "\n}\n")
}

# Generate Stan code for the log likelihood function
generate_loglik <- function(add_signature, loglik_body) {
  signature <- "array[] vector x_ode"
  signature <- append_to_signature(signature, add_signature)
  comment <- "\n// Compute log likelihood given ODE solution x_ode\n"
  code <- paste0(comment, "real log_likelihood(", signature, ")")
  if (nchar(loglik_body) == 0) {
    warning("log likelihood function body is empty!")
  }
  paste0(code, "{\n", loglik_body, "\n}\n")
}

# Create the data block
generate_data_block <- function(all_decls, data) {
  dims_code <- generate_dim_declarations(all_decls)
  dvars_code <- generate_var_declarations(data)
  code <- generate_block("data", c(dims_code, dvars_code))
  autoformat_stancode(code)
}

# Create a transform block (transformed data, transformed params, or gq)
generate_transform_block <- function(name, transforms) {
  decls <- lapply(transforms, get_decl)
  codes <- paste(lapply(transforms, get_code), collapse = "\n")
  decls <- generate_var_declarations(decls)
  generate_block(name, c(decls, codes))
}

# Create the parameters block
generate_params_block <- function(params) {
  decls <- lapply(params, get_decl)
  dvars_code <- generate_var_declarations(decls)
  generate_block("parameters", c(dvars_code))
}

# Create the parameters block
generate_model_block <- function(params, params_only) {
  codes <- paste(lapply(params, get_prior_code), collapse = "\n")
  if (params_only) {
    target <- ""
  } else {
    target <- "target += log_lik;"
  }
  generate_block("model", c(codes, target))
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
generate_dim_signatures <- function(vars, argmode) {
  checkmate::assert_list(vars, "StanDeclaration")
  dim_vars <- list()
  for (var in vars) {
    dim_vars <- c(dim_vars, var$get_dims())
  }
  generate_var_signatures(dim_vars, TRUE, argmode)
}

# Variables declaration code
generate_var_signatures <- function(vars, data, argmode) {
  if (length(vars) == 0) {
    return("")
  }
  vars <- unique(vars)
  checkmate::assert_list(vars, "StanDeclaration")
  if (argmode) {
    getter <- function(x) x$name
  } else {
    getter <- function(x) x$signature()
  }
  if (data && !argmode) {
    pre <- "data"
  } else {
    pre <- ""
  }
  signs <- unique(sapply(vars, getter))
  code <- paste(pre, signs, collapse = ", ")
  trimws(code)
}

# Get dimensions of each declaration
dims_of_decls <- function(decls) {
  checkmate::assert_list(decls, "StanDeclaration")
  dim_decls <- list()
  for (var in decls) {
    dim_decls <- c(dim_decls, var$get_dims())
  }
  unique(dim_decls)
}

# Dimensions declaration code
generate_dim_declarations <- function(vars) {
  dim_vars <- dims_of_decls(vars)
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

# Template 'Stan' model code
functions_template <- function() {
  filepath <- system.file("template_functions.stan", package = "odetuner")
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
