
  // RK4 solver
  array[] vector ode_rk4_fixed_num_steps(data vector y0, data real t0,
    data array[] real t, data int num_steps,
    __ODEFUN_SIGN__)
  {
    int N = size(t);
    int D = num_elements(y0);
    array[N] vector[D] y_sol;
    for(n in 1:N){
      y_sol[n] = rep_vector(sin(t[n]), D);
    }
    return(y_sol);
  }

  // Solve ODE
  array[] vector solve_ode(data int solver, data real rel_tol,
      data real abs_tol, data int max_num_steps, data int num_steps,
      data vector y0, data real t0, data array[] real t,
      __ODEFUN_SIGN__)
  {
    int N = size(t);
    int D = num_elements(y0);
    array[N] vector[D] y_sol;
    if (solver==1) {
      y_sol = ode_rk45_tol(odefun, y0, t0, t, rel_tol, abs_tol, max_num_steps,
        __ODEFUN_ARGS__);
    } else if (solver==2) {
      y_sol = ode_bdf_tol(odefun, y0, t0, t, rel_tol, abs_tol, max_num_steps,
        __ODEFUN_ARGS__);
    } else if (solver==3) {
      y_sol = ode_rk4_fixed_num_steps(y0, t0, t, num_steps, __ODEFUN_ARGS__);
    } else {
      reject("Solver must be 1, 2, or 11!");
    }
    return(y_sol);
  }
