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
  array[] vector solve_ode(data int solver, data real rel_tol,
      data real abs_tol, data int max_num_steps,
      data int num_steps, data vector x0, data real t0,
      data array[] real t,
      __ODEFUN_SIGNATURE__)
  {
    int N = size(t);
    int D = num_elements(x0);
    array[N] vector[D] x;
    if (solver==1) {
      x = ode_rk45_tol(odefun, x0, t0, t, rel_tol, abs_tol, max_num_steps,
        __ODEFUN_ARGS__);
    } else if (solver==2) {
      x = ode_bdf_tol(odefun, x0, t0, t, rel_tol[1], abs_tol[1],
        max_num_steps[1], __ODEFUN_ARGS__);
    } else if (solver==3) {
      x = ode_rk4_fixed_num_steps(x0, t0, t, num_steps[1], __ODEFUN_ARGS__);
    } else {
      reject("Solver must be 1, 2, or 11!");
    }
    return(x);
  }

}
