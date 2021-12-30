#' An ODE model (R6 class)
#'
#' @export
#' @field has_likelihood Is there a likelihood function?
#' @field stanmodel An object of class `StanModelWithCode`.
#' @field odetuner_version of the package used to create the model
#' @field sig_figs Number of significant figures to use everywhere.
#' @field t_dim A `StanDimension` of the time points array.
#' @field ode_dim A `StanDimension` of the ODE system.
OdeModel <- R6::R6Class("OdeModel", list(
  has_likelihood = NULL,
  stanmodel = NULL,
  odetuner_version = NULL,
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
    checkmate::assert_integerish(sig_figs, lower = 3)
    checkmate::assert_class(t_dim, "StanDimension")
    checkmate::assert_class(ode_dim, "StanDimension")
    self$has_likelihood <- has_likelihood
    self$stanmodel <- stanmodel$clone(deep = TRUE)
    self$odetuner_version <- pkg_version("odetuner")
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
    cat("An object of class OdeModel. Type ?OdeModel for help. \n", sep = "")
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
