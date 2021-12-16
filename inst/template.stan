functions {

  // RK4 solver
  array[] vector ode_rk4_fixed_num_steps(data vector x0, data real t0,
    data array[] real t, data int num_steps,
    __ODEFUN_SIGNATURE__)
  {
    int N = size(t);
    int D = num_elements(x0);
    array[N] vector[D] x;
    for(n in 1:N){
      x[n] = rep_vector(sin(t[n]), D);
    }
    return(x);
  }

  // Define function odefun() here, can have any signature allowed by the
  // Stan ODE interface
  vector odefun(real t, vector y, __ODEFUN_SIGNATURE__
  ) {
    __ODEFUN_BODY__
  }

  // Define function log_likelihood() here
  // First argument has to be array of ODE solutions at data time points
  real log_likelihood(array[] vector x_ode, __LOGLIK_SIGNATURE__) {
    __LOGLIK_BODY__
  }

  // Solve ODE
  array[] vector solve_ode(data int solver, data int N, data int D,
      data array[] real rel_tol, data array[] real abs_tol,
      data array[] int max_num_steps,
      data array[] int num_steps, data vector x0, data real t0,
      data array[] real t,
      __ODEFUN_SIGNATURE__)
  {

    array[N] vector[D] x;
    if (solver==1) {
      x = ode_rk45_tol(odefun, x0, t0, t, rel_tol[1], abs_tol[1],
        max_num_steps[1], __ODEFUN_ARGS__);
    } else if (solver==2) {
      x = ode_bdf_tol(odefun, x0, t0, t, rel_tol[1], abs_tol[1],
        max_num_steps[1], __ODEFUN_ARGS__);
    } else if (solver==3) {
      x = ode_rk4_fixed_num_steps(x0, t0, t, num_steps[1], __ODEFUN_ARGS__);
    } else {
      reject("Solver must be 1, 2, or 3!");
    }
    return(x);
  }

}

data {
  int<lower=1> N;                 // number of time points
  array[N] real t;                // time points
  int<lower=1> D;                 // ODE-system dimension
  real t0;                        // initial time point

  // Define other needed data here, for example
  //  * initial state of ODE system at t0
  //  * observations at timepoints t
  __ADD_DATA__

  // ODE data
  int<lower=1> solver;  // 1 = rk45, 2 = bdf, 3 = rk4
  array[solver<=2] real<lower=0> rel_tol;      // ODE solver relative tolerance
  array[solver<=2] real<lower=0> abs_tol;      // ODE solver absolute tolerance
  array[solver<=2] int<lower=0> max_num_steps; // ODE solver max num of steps
  array[solver>2] int<lower=1> num_steps;      // For non-adaptive solver

  // Binary option switches
  int<lower=0,upper=1> do_likelihood;
  int<lower=0,upper=1> do_genquant;
}

__MIDDLE_BLOCKS__

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
