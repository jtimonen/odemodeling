
# Create all 'CmdStanModels'
create_cmdstan_models <- function(code) {
  codes <- list(
    prior = prior_model_code(code),
    simulator = simulator_model_code(code),
    posterior = posterior_model_code(code)
  )
  j <- 0
  models <- list()
  for (code in codes) {
    j <- j + 1
    cat("Creating", names(codes)[j], "model...\n", sep = " ")
    models[[j]] <- cmdstanr::cmdstan_model(cmdstanr::write_stan_file(code))
  }
  names(models) <- names(codes)
  return(models)
}

# Take just posterior model code from full 'Stan' code
posterior_model_code <- function(code) {
  remove_trailing_codeblock(code, "generated quantities")
}

# Take just prior model code from full 'Stan' code
prior_model_code <- function(code) {
  code <- posterior_model_code(code)
  mblock <- strsplit(code, split = "model", fixed = TRUE)[[1]][2]
  mlines <- strsplit(mblock, split = "[\n]")[[1]]
  line_idx <- which(lapply(mlines, rmws) == "target+=log_prior")
  code <- remove_trailing_codeblock(code, "model")
  code_add <- paste("model {", mlines[line_idx], "}", sep = "\n")
  paste(code, code_add, sep = "")
}

# Take just simulation code from full 'Stan' code
simulator_model_code <- function(code) {
  remove_model_block(code)
}

# Remove last code block if it has given name
remove_trailing_codeblock <- function(code, blockname) {
  split <- strsplit(code, split = blockname, fixed = TRUE)
  if (length(split) > 0) {
    return(split[[1]][1])
  }
  code
}

# Remove model block from full 'Stan' code
remove_model_block <- function(code) {
  code_start <- remove_trailing_codeblock(code, "model")
  split <- strsplit(code, split = "generated quantities", fixed = TRUE)
  code_end <- paste0("generated quantities ", split[[1]][2])
  paste(code_start, code_end, sep = "\n")
}

# Remove whitespace and take first chars
rmws <- function(x) {
  trimmed <- gsub(" ", "", x, fixed = TRUE)
  substr(trimmed, 1, 17)
}
