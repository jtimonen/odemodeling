#' Run example
#'
#' @export
#' @param verbose Should this print more information.
#' @return Define return value.
example <- function(verbose = FALSE) {
  sc <- example_stancode_gsir()
  if (verbose) {
    cat(sc$code)
    message(paste("Stan model saved to", sc$file))
  }
  model <- cmdstanr::cmdstan_model(stan_file = sc$file)
  return(model)
}

# Example
example_stancode_gsir <- function() {
  odefun_add_args <- c(
    "real beta", "vector gamma", "data matrix contacts",
    "data vector pop_sizes"
  )
  odefun_body <- "
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
  "
  loglik_add_args <- c("int[,] y", "real delta", "vector phi")
  loglik_body <- "
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
  data <- "
  int<lower=1> G;                 // number of groups
  int<lower=0> y[N,G];            // observations of infected
  vector[G] pop_sizes;            // population sizes in each group
  vector[G] I0;                   // initial number of infected in each group
  matrix[G, G] contacts;          // contact matrix
  real<lower=0> delta;                  // Small positive number
  "
  middle_blocks <- "
transformed data {
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
  "
  prior <- "
  beta ~ normal(2, 1);
  gamma ~ normal(0.3, 0.3);
  phi_inv ~ exponential(5);
  "
  genquant_decl <- "array[N, G] int y_gen;"
  genquant <- "
    for(n in 1:N) {
      for(g in 1:G) {
        y_gen[n,g] = neg_binomial_2_rng(x_ode[1,n][G+g] + delta, phi[g]);
      }
    }
  "
  generate_stancode(
    odefun_add_args, odefun_body, loglik_add_args,
    loglik_body, data, middle_blocks, prior, genquant_decl, genquant
  )
}
