#' Create an ODE model from parts of 'Stan' code
#'
#' @export
#' @inheritParams generate_stancode
#' @param verbose Should this print more information?
#' @param compile Should the 'Stan' model be compiled?
#' @param ... Additional arguments to `cmdstanr::write_stan_file()`.
#' @return An object of class `OdeModelSetup`.
#' @family setup functions
create_odemodel <- function(odefun_add_args,
                            odefun_body,
                            loglik_add_args,
                            loglik_body,
                            data = "",
                            middle_blocks = "",
                            prior = "",
                            genquant_decl = "",
                            genquant = "",
                            verbose = FALSE,
                            compile = TRUE,
                            ...) {
  sc <- generate_stancode(
    odefun_add_args, odefun_body,
    loglik_add_args, loglik_body,
    data, middle_blocks, prior, genquant_decl,
    genquant, ...
  )
  if (verbose) {
    cat(sc$code)
    message(paste("Stan model saved to", sc$file))
  }
  model <- cmdstanr::cmdstan_model(stan_file = sc$file, compile = compile)
  return(model)
}
