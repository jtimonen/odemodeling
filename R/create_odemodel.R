#' Create an ODE model from parts of 'Stan' code
#'
#' @export
#' @param odefun_data Additional data needed by the ODE function
#' (character vector of 'Stan' variable declarations).
#' @param odefun_pars Parameters of the ODE function (character vector of
#' 'Stan' parameter declarations).
#' @param odefun_code Body of the ODE function (string of 'Stan' code). Must
#' end in a line that returns the time derivative of the system as a vector.
#' @param odefun_tdata Additional transformed data needed by the ODE function
#' (character vector of 'Stan' variable declarations).
#' @param tdata_code String of 'Stan' code that computes the declared
#' transformed data.
#' @param obsmodel_data Additional data needed by the likelihood function
#' (character vector of 'Stan' variable declarations).
#' @param obsmodel_pars Parameters of the likelihood function
#' (character vector of 'Stan' parameter declarations).
#' @param loglik_code Body of the log likelihood function
#' (string of 'Stan' code). Must end in a line that returns a real number,
#' that is (proportional to) the likelihood.
#' @param genquant Additional generated quantities
#' (character vector of 'Stan' variable declarations).
#' @param genquant_code String of 'Stan' code that computes the declared
#' generated quantities.
#' @param verbose Should this print more information?
#' @param compile Should the model be compiled?
#' @param ... Additional arguments to `cmdstanr::write_stan_file()`.
#' @return An object of class `OdeModel`.
#' @family setup functions
create_odemodel <- function(
                            odefun_data = character(0),
                            odefun_pars = character(0),
                            odefun_code = "",
                            odefun_tdata = character(0),
                            tdata_code = "",
                            obsmodel_data = character(0),
                            obsmodel_pars = character(0),
                            priors = character(0),
                            loglik_code = "",
                            genquant = character(0),
                            genquant_code = "",
                            verbose = FALSE,
                            compile = TRUE,
                            ...) {

  # Generate full Stan model code
  code <- generate_stancode(
    odefun_add_args, odefun_body,
    loglik_add_args, loglik_body,
    add_data, middle_blocks, prior, genquant_decl,
    genquant, autoformat
  )
  if (verbose) cat(code)

  # Write code to temp file and create model
  datasim <- nchar(genquant) > 0
  OdeModel$new(stancode = code, datasim = datasim, compile = compile, ...)
}
