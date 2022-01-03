
  // Euler method
  array[] vector ode_euler_fixed_num_steps(data vector y0, data real t0,
      data array[] real t, data int num_steps,
      __ODEFUN_SIGN__) {
    int N = size(t);
    int D = num_elements(y0);
    array[N+1] vector[D] y_sol;
    array[N+1] real t_sol;
    real h;
    real ts;
    vector[D] ys;
    y_sol[1] = y0;
    t_sol[1] = t0;
    for(n in 1:N){
      t_sol[n+1] = t[n];
      h = (t_sol[n+1] - t_sol[n])/num_steps;
      ts = t_sol[n];
      ys = y_sol[n];
      for(s in 1:num_steps) {
        ys = ys + h * odefun(ts, ys, __ODEFUN_ARGS__);
        ts = ts + h;
      }
      y_sol[n+1] = ys;
    }
    return(y_sol[2:(N+1)]);
  }

  // Midpoint method
  array[] vector ode_midpoint_fixed_num_steps(data vector y0, data real t0,
      data array[] real t, data int num_steps,
      __ODEFUN_SIGN__) {
    int N = size(t);
    int D = num_elements(y0);
    array[N+1] vector[D] y_sol;
    array[N+1] real t_sol;
    real h;
    real ts;
    vector[D] ys;
    vector[D] y_mid;
    y_sol[1] = y0;
    t_sol[1] = t0;
    for(n in 1:N){
      t_sol[n+1] = t[n];
      h = (t_sol[n+1] - t_sol[n])/num_steps;
      ts = t_sol[n];
      ys = y_sol[n];
      for(s in 1:num_steps) {
        // Half-Euler step
        y_mid = ys + 0.5 * h * odefun(ts, ys, __ODEFUN_ARGS__);
        // Full step using derivative at midpoint
        ys = ys + h * odefun(ts + 0.5 * h, y_mid, __ODEFUN_ARGS__);
        ts = ts + h;
      }
      y_sol[n+1] = ys;
    }
    return(y_sol[2:(N+1)]);
  }

  // RK4 method
  array[] vector ode_rk4_fixed_num_steps(data vector y0, data real t0,
      data array[] real t, data int num_steps,
      __ODEFUN_SIGN__) {
    int N = size(t);
    int D = num_elements(y0);
    array[N+1] vector[D] y_sol;
    array[N+1] real t_sol;
    real h;
    real ts;
    vector[D] ys;
    vector[D] k1;
    vector[D] k2;
    vector[D] k3;
    vector[D] k4;
    y_sol[1] = y0;
    t_sol[1] = t0;
    for(n in 1:N){
      t_sol[n+1] = t[n];
      h = (t_sol[n+1] - t_sol[n])/num_steps;
      ts = t_sol[n];
      ys = y_sol[n];
      for(s in 1:num_steps) {
        k1 = h * odefun(ts, ys, __ODEFUN_ARGS__);
        k2 = h * odefun(ts + 0.5 * h, ys + 0.5 * k1, __ODEFUN_ARGS__);
        k3 = h * odefun(ts + 0.5 * h, ys + 0.5 * k2, __ODEFUN_ARGS__);
        k4 = h * odefun(ts + h, ys + k3, __ODEFUN_ARGS__);
        ys = ys + (k1 + 2.0 * k2 + 2.0 * k3 + k4) / 6.0;
      }
      y_sol[n + 1] = ys;
    }
    return(y_sol[2:(N+1)]);
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
      y_sol = ode_adams_tol(odefun, y0, t0, t, rel_tol, abs_tol, max_num_steps,
        __ODEFUN_ARGS__);
    } else if (solver==4) {
      y_sol = ode_ckrk_tol(odefun, y0, t0, t, rel_tol, abs_tol, max_num_steps,
        __ODEFUN_ARGS__);
    } else if (solver==101) {
      y_sol = ode_euler_fixed_num_steps(y0, t0, t, num_steps, __ODEFUN_ARGS__);
    } else if (solver==102) {
      y_sol = ode_midpoint_fixed_num_steps(y0, t0, t, num_steps, __ODEFUN_ARGS__);
    } else if (solver==103) {
      y_sol = ode_rk4_fixed_num_steps(y0, t0, t, num_steps, __ODEFUN_ARGS__);
    } else {
      reject("solver must be 1, 2, 3, 4, 101, 102, or 103!");
    }
    return(y_sol);
  }
