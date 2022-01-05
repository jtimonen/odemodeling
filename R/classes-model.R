#' An ODE model (R6 class)
#'
#' @export
#' @description An ODE model (R6 class). Users are not meant to instantiate
#' objects of this class directly, instead use the [ode_model()] function
#' to create models.
#' @field has_likelihood Is there a likelihood function?
#' @field stanmodel An object of class `StanModelWithCode`.
#' @field odemodeling_version of the package used to create the model
#' @field sig_figs Number of significant figures to use everywhere.
#' @field t_dim A `StanDimension` of the time points array.
#' @field ode_dim A `StanDimension` of the ODE system.
OdeModel <- R6::R6Class("OdeModel", list(
  has_likelihood = NULL,
  stanmodel = NULL,
  odemodeling_version = NULL,
  sig_figs = NULL,
  t_dim = NULL,
  ode_dim = NULL,

  #' @description
  #' Create an `OdeModel` object.
  #'
  #' @param has_likelihood Is there a likelihood function?
  #' @param stanmodel An object of class `StanModelWithCode`
  #' (will be deepcopied)..
  #' @param compile Should the models be compiled.
  #' @param sig_figs Number of significant figures to use in all Stan i/o.
  #' @param t_dim Time points vector dimension variable
  #' (will be deepcopied).
  #' @param ode_dim ODE system dimension variable (will be deepcopied).
  initialize = function(has_likelihood, stanmodel, sig_figs, t_dim, ode_dim) {
    checkmate::assert_integerish(sig_figs, lower = 3, upper = 18)
    checkmate::assert_class(t_dim, "StanDimension")
    checkmate::assert_class(ode_dim, "StanDimension")
    self$has_likelihood <- has_likelihood
    self$stanmodel <- stanmodel$clone(deep = TRUE)
    self$odemodeling_version <- pkg_version("odemodeling")
    self$sig_figs <- sig_figs
    self$t_dim <- t_dim$clone(deep = TRUE)
    self$ode_dim <- ode_dim$clone(deep = TRUE)
  },

  #' @description
  #' Check that the Stan model has been initialized correctly
  assert_stanfile_exists = function() {
    e1 <- self$stanmodel$stan_file_exists()
    if (!(e1)) {
      stop("Stan model file doesn't exist. Please call $reinit().")
    }
    TRUE
  },

  #' @description
  #' (Re)initialize the Stan model
  reinit = function() {
    self$stanmodel$reinit()
    invisible(self)
  },

  #' @description
  #' Print information about the model
  print = function() {
    cat(class_info("OdeModel"), "\n")
    cat(" * ODE dimension: ")
    self$ode_dim$print()
    cat(" * Time points array dimension: ")
    self$t_dim$print()
    cat(" * Number of significant figures in csv files: ")
    cat_number(self$sig_figs)
    cat("\n")
    cat(" * Has likelihood: ")
    cat_number(self$has_likelihood)
    cat("\n")
    invisible(self)
  },

  #' @description
  #' Get the Stan code of the model.
  code = function() {
    self$stanmodel$code
  },

  #' @description Sample parameters of the model
  #' @param t0 Initial time point.
  #' @param t Vector of time points.
  #' @param solver An object of class [OdeSolver].
  #' @param data Other needed data as a list.
  #' @param ... Arguments passed to the `$sample()` method of the
  #' underlying [cmdstanr::CmdStanModel] object.
  #' @return An object of class [OdeModelMCMC].
  sample = function(t0,
                    t,
                    data = list(),
                    solver = rk45(),
                    ...) {

    # Check and handle input
    sd <- create_standata(self, t0, t, solver)
    full_data <- c(sd, data)

    # Actual sampling
    sm <- self$stanmodel
    cmdstanr_mcmc <- sm$sample(data = full_data, sig_figs = self$sig_figs, ...)

    # Return
    OdeModelMCMC$new(
      model = self,
      t0 = t0,
      t = t,
      solver = solver,
      data = data,
      cmdstanr_fit = cmdstanr_mcmc
    )
  },

  #' @description
  #' Sample parameters of the ODE model using many different ODE solver
  #' configurations
  #'
  #' @export
  #' @param solvers List of ODE solvers (possibly the same solver with
  #' different configurations). See [rk45_list()] and [bdf_list()]
  #' for creating this.
  #' @param t0 Initial time point.
  #' @param t Vector of time points.
  #' @param data Other needed data as a list.
  #' @param savedir Directory where results are saved.
  #' @param basename Base name for saved files.
  #' @param chains Number of MCMC chains.
  #' @param ... Additional arguments passed to the `$sample()` method of the
  #' underlying [cmdstanr::CmdStanModel] object.
  #' @return A named list.
  sample_manyconf = function(solvers,
                             t0,
                             t,
                             data = list(),
                             savedir = "results",
                             basename = "odemodelfit",
                             chains = 4,
                             ...) {
    model <- self
    if (!dir.exists(savedir)) {
      message("directory '", savedir, "' doesn't exist, creating it")
      dir.create(savedir)
    }
    checkmate::assert_list(solvers, "OdeSolver")
    L <- length(solvers)
    WT <- matrix(0.0, L, chains)
    ST <- matrix(0.0, L, chains)
    TT <- matrix(0.0, L, chains)
    FN <- c()
    GT <- rep(0.0, L)
    for (j in seq_len(L)) {
      solver <- solvers[[j]]
      conf_str <- solver$to_string()
      cat("=================================================================\n")
      cat(" (", j, ") Sampling with: ", conf_str, "\n", sep = "")
      fn <- file.path(savedir, paste0(basename, "_", j, ".rds"))
      fit <- model$sample(
        t0 = t0,
        t = t,
        data  = data,
        solver = solver,
        chains = chains,
        ...
      )
      cat("Saving result object to ", fn, "\n", sep = "")
      saveRDS(fit, file = fn)
      FN <- c(FN, fn)
      t_total <- fit$time()$chains$total
      gt <- fit$time()$total
      GT[j] <- gt
      WT[j, ] <- fit$time()$chains$warmup
      ST[j, ] <- fit$time()$chains$sampling
      TT[j, ] <- t_total
    }
    times <- list(warmup = WT, sampling = ST, total = TT, grand_total = GT)

    # Return
    list(
      times = times, files = FN, solver = solver
    )
  }
))


# A model (R6 class)
StanModelWithCode <- R6::R6Class("StanModelWithCode",
  public = list(
    model = NULL,
    dims = NULL,
    data = NULL,
    tdata = NULL,
    params = NULL,
    tparams = NULL,
    gqs = NULL,
    code = "",
    get_model = function() {
      mod <- self$model
      if (is.null(mod)) {
        stop("Model not initialized. You need to call $reinit().")
      }
      return(mod)
    },
    get_decls = function(type) {
      dnames <- c("dims", "data", "tdata", "params", "tparams", "gqs")
      checkmate::assert_choice(type, dnames)
      self[[type]]
    },
    initialize = function(code, dims, data, tdata, params, tparams, gqs,
                          compile) {
      if (!compile) {
        message(
          "Not compiling the model. You need to call $reinit() before",
          " being able to sample."
        )
      }
      self$code <- code
      self$dims <- dims
      self$data <- data
      self$tdata <- tdata
      self$params <- params
      self$tparams <- tparams
      self$gqs <- gqs
      if (compile) {
        self$model <- stan_model_from_code(code)
      }
    },
    reinit = function() {
      self$model <- stan_model_from_code(self$code)
      invisible(self)
    },
    print = function() {
      cat_stancode(self$code)
      invisible(self)
    },
    stan_file_exists = function() {
      mod <- self$get_model()
      sf <- mod$stan_file()
      if (file.exists(sf)) {
        return(TRUE)
      }
      FALSE
    },
    names_of = function(type) {
      decls <- self$get_decls(type)
      nams <- sapply(decls, get_name)
      if (length(nams) == 0) {
        return(NULL)
      }
      nams
    },
    data_names = function() {
      nam1 <- self$names_of("dims")
      nam2 <- self$names_of("data")
      unique(c(nam1, nam2))
    },
    param_names = function(inc_transformed = FALSE) {
      nam <- self$names_of("params")
      if (inc_transformed) {
        nam <- c(nam, self$names_of("tparams"))
      }
      unique(nam)
    },
    gq_names = function() {
      self$names_of("gqs")
    },
    data_check = function(data) {
      checkmate::assert_list(data)
      needed <- self$data_names()
      given <- names(data)
      for (name in needed) {
        if (!(name %in% given)) {
          stop(paste0(name, " is missing from the data list!"))
        }
      }
      TRUE
    },
    sample = function(data, ...) {
      self$data_check(data)
      mod <- self$get_model()
      mod$sample(data = data, ...)
    },
    generate_quantities = function(data, fitted_params, ...) {
      self$data_check(data)
      mod <- self$get_model()
      mod$generate_quantities(
        fitted_params = fitted_params,
        data = data, ...
      )
    }
  )
)
