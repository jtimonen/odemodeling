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
  N <- stan_dim("N", lower = 0) # number of timepoints

  # Data needed by ODE function
  G <- stan_dim("G", lower = 1) # number of groups
  pop_sizes <- stan_vector("pop_sizes", G) # population sizes in each group
  I0 <- stan_vector("I0", G, lower = 0) # initial no. infected in each group
  contacts <- stan_matrix("contacts", G, G) # contact matrix

  # ODE function parameters
  beta <- stan_param(stan_var("beta", lower = 0), "beta ~ normal(2, 1);")
  gamma_decl <- stan_vector("gamma", lower = 0, length = G)
  gamma <- stan_param(gamma_decl, "gamma ~ normal(0.3, 0.3);")

  # All odefun variables
  odefun_vars <- list(pop_sizes, I0, contacts, beta, gamma)

  # Initial value
  D <- stan_dim("D", lower = 0) # number of ODE dimensions
  x0_var <- stan_vector("x0", length = D)
  x0_code <-
    "
    for(g in 1:G) {
      x0[g] = pop_sizes[g] - I0[g]; // initial number of S
    }
    for(g in 1:G) {
      x0[G + g] = I0[g]; // initial number of I
    }
  "
  x0 <- stan_transform(x0_var, "data", x0_code)

  # Observation model data
  delta <- stan_var("delta", lower = 0)
  I_data <- stan_array("I_data", type = "int", dims = list(N, G), lower = 0)

  # Observation model parameters phi_inv
  phi_inv_var <- stan_vector("phi_inv", lower = 0, length = G)
  phi_var <- stan_vector("phi", lower = 0, length = G)
  phi_inv <- stan_param(phi_inv_var, "phi_inv ~ exponential(5);")
  phi <- stan_transform(phi_var, "param", "phi = inv(phi_inv);")

  # All loglik variables
  loglik_vars <- list(delta, I_data, phi_inv, phi)

  # Function bodies
  odefun_body <- "
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

  loglik_body <- "
    real log_lik = 0.0;
    for(n in 1:N) {
      for(g in 1:G) {
        log_lik += neg_binomial_2_lpmf(I_data[n,g] | x_ode[n][G+g] + delta,
          phi[g]);
      }
    }
    return(log_lik);
  "

  # Generated quantity
  I_gen_decl <- stan_array("I_gen", type = "int", dims = c(N, G))
  I_gen_code <- "
    for(n in 1:N) {
      for(g in 1:G) {
        I_gen[n,g] = neg_binomial_2_rng(x_ode[n][G+g] + delta, phi[g]);
      }
    }
  "
  I_gen <- stan_transform(I_gen_decl, "model", I_gen_code)

  # Works
  a <- create_odemodel(
    N = N,
    odefun_vars = odefun_vars,
    odefun_body = odefun_body,
    odefun_init = x0,
    loglik_vars = loglik_vars,
    loglik_body = loglik_body,
    other_vars = list(I_gen)
  )
  return(a)
}
