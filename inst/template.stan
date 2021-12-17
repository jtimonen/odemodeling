data {
  // Dimensions
  __DIMENSIONS__

  // Other needed for the ODE function
  __ODEFUN_DATA__

  // Other needed for computing the likelihood
  __OBSMODEL_DATA__

  // ODE data
  int<lower=1> solver;          // 1 = rk45, 2 = bdf, 11 = rk4
  real<lower=0> rel_tol;        // ODE solver relative tolerance
  real<lower=0> abs_tol;        // ODE solver absolute tolerance
  int<lower=0> max_num_steps;   // ODE solver max num of steps
  int<lower=1> num_steps;       // For non-adaptive solver

  // Binary option switches
  int<lower=0,upper=1> do_likelihood;
  int<lower=0,upper=1> do_genquant;
}

transformed data {
  __TRANSFORMED_DATA__
}

parameters {
  __PARAMETERS__
}

transformed parameters {
  __TRANSFORMED_PARAMETERS__
}

model {
  array[do_likelihood, N] vector[D] x_ode;

  // Prior
  __PRIOR__

  // Likelihood
  if(do_likelihood == 1) {
    x_ode[1] = solve_ode(solver, N, D, rel_tol, abs_tol, max_num_steps,
      num_steps, x0, t0, t, __ODEFUN_ARGS__);
    target += log_likelihood(x_ode[1], __LOGLIK_ARGS__);
  }
}

generated quantities {
  array[do_genquant, N] vector[D] x_ode;
  array[do_genquant] real log_lik;
  __GENQUANT_DECL__
  if(do_genquant == 1) {
    x_ode[1] = solve_ode(solver, N, D, rel_tol, abs_tol, max_num_steps,
        num_steps, x0, t0, t, __ODEFUN_ARGS__);
    log_lik[1] = log_likelihood(x_ode[1], __LOGLIK_ARGS__);
    __GENQUANT__
  }
}
