functions {
  // Age-stratified SIR system RHS
  vector odefun(real t, vector y, real beta, vector gamma,
      data matrix contacts, data vector pop_sizes) {
    int G = num_elements(pop_sizes);
    vector[2*G] dy_dt; // first G are susceptible, next G are infected
    vector[G] infection_rates;
    vector[G] recovery_rates;
    vector[G] lambda = rep_vector(0.0, G);
    for(g in 1:G){
      for(h in 1:G) {
        lambda[g] += contacts[g,h] * y[G+h]/pop_sizes[h];
      }
    }
    for(g in 1:G){
      dy_dt[g] = -  beta * lambda[g] * y[g];
      dy_dt[G+g] =  beta * lambda[g] * y[g] - gamma[g] * y[G+g];
    }
    return dy_dt;
  }

  // Compute log likelihood
  real log_likelihood(vector[] x, int[,] y, int N, int G, real abs_tol,
      vector phi){
    real log_lik = 0.0;
    for(n in 1:N) {
      for(g in 1:G) {
        log_lik += neg_binomial_2_lpmf(y[n,g] | x[n][G+g] + 10*abs_tol, phi[g]);
      }
    }
    return(log_lik);
  }

  // Compute log prior
  real log_prior(real beta, vector gamma, vector phi_inv){
    real lp = 0.0;
    lp += normal_lpdf(beta | 2, 1);
    lp += normal_lpdf(gamma | 0.3, 0.3);
    lp += exponential_lpdf(phi_inv | 5);
    return(lp);
  }
}

data {
  int<lower=1> N;                 // number of time points
  real t[N];                      // time points
  int<lower=1> G;                 // number of groups
  int<lower=0> I[N,G];            // observations of infected
  vector[G] pop_sizes;            // population sizes in each group
  vector[G] I0;                   // initial number of infected in each group
  matrix[G, G] contacts;          // contact matrix

  real<lower=0> rel_tol;          // ODE solver relative tolerance
  real<lower=0> abs_tol;          // ODE solver absolute tolerance
  int<lower=0> max_num_steps;     // ODE solver maximum number of steps

}

transformed data {
  real t0 = 0.0; // initial time point
  vector[2*G] x0;
  for(g in 1:G){
    x0[g] = pop_sizes[g] - I0[g]; // initial number of S
  }
  for(g in 1:G){
    x0[G + g] = I0[g]; // initial number of I
  }
}

parameters {
  real<lower=0> beta;          // infenction rate
  vector<lower=0>[G] gamma;    // group-specific recovery rates
  vector<lower=0>[G] phi_inv;  // group-specific noise parameters
}

transformed parameters {
  vector[G] phi = inv(phi_inv);
}

model {
  vector[2*G] x[N] = ode_rk45_tol(odefun, x0, t0, t, rel_tol, abs_tol,
    max_num_steps, beta, gamma, contacts, pop_sizes);
  target += log_prior(beta, gamma, phi_inv);
  target += log_likelihood(x, I, N, G, abs_tol, phi);
}

generated quantities {
  vector[2*G] x[N] = ode_rk45_tol(odefun, x0, t0, t, rel_tol, abs_tol,
    max_num_steps, beta, gamma, contacts, pop_sizes);
  real log_lik = log_likelihood(x, I, N, G, abs_tol, phi);
  int I_gen[N, G];
  for(n in 1:N) {
    for(g in 1:G) {
      I_gen[n,g] = neg_binomial_2_rng(x[n][G+g] + 10*abs_tol, phi[g]);
    }
  }
}
