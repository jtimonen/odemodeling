#' Create an example ODE model
#'
#' @export
#' @param ... Additional arguments to \code{\link{create_odemodel}}.
#' @return An object of class `OdeModel`.
#' @family setup functions
example_odemodel <- function(...) {
  example_odemodel_gsir(...)
}

# Example
example_odemodel_gsir <- function(...) {

  # Time points
  N <- stan_dim("N", lower = 1) # number of timepoints
  t <- stan_vector("t", length = N) # vector of time points

  # Data
  G <- stan_dim("G", lower = 1) # number of groups
  pop_sizes <- stan_vector("pop_sizes", G) # population sizes in each group
  I0 <- stan_vector("I0", G, lower = 0) # initial num of infected in each group
  contacts <- stan_matrix("contacts", G, G) # contact matrix
  odefun_data <- list(pop_sizes, I0, contacts)

  # Ode function parameters beta and gamma
  beta <- stan_param(stan_var("beta", lower = 0), "beta ~ normal(2, 1);")
  gvar <- stan_vector("gamma", lower = 0, length = G)
  gamma <- stan_param(gvar, "gamma ~ normal(0.3, 0.3);")

  # Observation model parameters phi_inv
  pivar <- stan_vector("phi_inv", lower = 0, length = G)
  phi_inv <- stan_param(pivar, "phi_inv ~ exponential(5);")

  # Works
  a <- generate_stancode(
    timepoints = t, odefun_data = odefun_data,
    odefun_params = list(beta, gamma)
  )

  odefun_code <- "
    vector[2*G] dy_dt; // first G are susceptible, next G are infected
    vector[G] infection_rates;
    vector[G] recovery_rates;
    vector[G] lambda = rep_vector(0.0, G);
    for(g in 1:G) {
      for(h in 1:G) {
        lambda[g] += contacts[g,h] * y[G+h]/pop_sizes[h];
      }
    }
    for(g in 1:G) {
      dy_dt[g] = -  beta * lambda[g] * y[g];
      dy_dt[G+g] =  beta * lambda[g] * y[g] - gamma[g] * y[G+g];
    }
    return dy_dt;
  "

  odefun_tdata <- c("vector[2*G] x0")
  tdata_code <- "
    for(g in 1:G) {
      x0[g] = pop_sizes[g] - I0[g]; // initial number of S
    }
    for(g in 1:G) {
      x0[G + g] = I0[g]; // initial number of I
    }
  "

  obsmodel_data <- c(
    "int<lower=0> y[N,G]", # observations of infected
    "real<lower=0> delta" # small positive number
  )

  loglik_code <- "
    real log_lik = 0.0;
    int G = size(y[1]);
    for(n in 1:size(y)) {
      for(g in 1:G) {
        log_lik += neg_binomial_2_lpmf(y[n,g] | x_ode[n][G+g] + delta,
          phi[g]);
      }
    }
    return(log_lik);
  "

  genquant <- "array[N, G] int y_gen"
  genquant_code <- "
    for(n in 1:N) {
      for(g in 1:G) {
        y_gen[n,g] = neg_binomial_2_rng(x_ode[1,n][G+g] + delta, phi[g]);
      }
    }
  "
  create_odemodel(
    odefun_data, odefun_pars, odefun_code, odefun_tdata, tdata_code,
    obsmodel_data, obsmodel_pars, priors, loglik_code, genquant, genquant_code,
    ...
  )
}
