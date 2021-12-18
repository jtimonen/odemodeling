
  // RK4 solver
  array[] vector ode_rk4_fixed_num_steps(data vector x0, data real t0,
    data array[] real t, data int num_steps,
    __ODEFUN_SIGN__)
  {
    int N = size(t);
    int D = num_elements(x0);
    array[N] vector[D] x;
    for(n in 1:N){
      x[n] = rep_vector(sin(t[n]), D);
    }
    return(x);
  }

  // Solve ODE
  array[] vector solve_ode(data int solver, data real rel_tol,
      data real abs_tol, data int max_num_steps, data int num_steps,
      data vector x0, data real t0, data array[] real t,
      __ODEFUN_SIGN__)
  {
    int N = size(t);
    int D = num_elements(x0);
    array[N] vector[D] x;
    if (solver==1) {
      x = ode_rk45_tol(odefun, x0, t0, t, rel_tol, abs_tol, max_num_steps,
        __ODEFUN_ARGS__);
    } else if (solver==2) {
      x = ode_bdf_tol(odefun, x0, t0, t, rel_tol, abs_tol, max_num_steps,
        __ODEFUN_ARGS__);
    } else if (solver==3) {
      x = ode_rk4_fixed_num_steps(x0, t0, t, num_steps, __ODEFUN_ARGS__);
    } else {
      reject("Solver must be 1, 2, or 11!");
    }
    return(x);
  }
