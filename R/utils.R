# Read lines from file
read_file_lines <- function(file) {
  a <- readLines(file)
  paste(a, collapse = "\n")
}

# Create model name from Stan file path
file_name_to_model_name <- function(file) {
  bn <- basename(file)
  strsplit(bn, split = "[.]")[[1]][1]
}

# Autoformat a 'Stan' code string
autoformat_stancode <- function(code) {
  file <- cmdstanr::write_stan_file(code)
  model <- cmdstanr::cmdstan_model(file, compile = FALSE)
  res <- processx::run(
    file.path(cmdstanr::cmdstan_path(), "bin", "stanc"),
    args = c(model$stan_file(), "--auto-format")
  )
  res$stdout
}
