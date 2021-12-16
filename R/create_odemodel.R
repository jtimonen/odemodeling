#' Create an ODE model from parts of 'Stan' code
#'
#' @export
#' @inheritParams generate_stancode
#' @param verbose Should this print more information?
#' @param compile Should the model be compiled?
#' @param ... Additional arguments to `cmdstanr::write_stan_file()`.
#' @return An object of class `OdeModel`.
#' @family setup functions
create_odemodel <- function(odefun_add_args,
                            odefun_body,
                            loglik_add_args,
                            loglik_body,
                            add_data = "",
                            middle_blocks = "",
                            prior = "",
                            genquant_decl = "",
                            genquant = "",
                            verbose = FALSE,
                            compile = TRUE,
                            autoformat = TRUE,
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
