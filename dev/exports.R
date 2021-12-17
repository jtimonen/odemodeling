# Read namespace and extract exported functions
s <- scan("NAMESPACE", what = "character", sep = "\n")
L <- length(s)
funs <- NULL

# Select rows that define exports
for (i in seq_len(L)) {
  line <- s[i]
  a <- grepl("export(", line, fixed = TRUE)
  if (a) funs <- c(funs, line)
}

# Print text to be pasted to _pkgdown.yml
L <- length(funs)
for (i in seq_len(L)) {
  line <- funs[i]
  f <- substr(line, 8, nchar(line) - 1)
  cat("  -", f, "\n")
}
