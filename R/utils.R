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
