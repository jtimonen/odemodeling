
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
