require(covr)
exclusions <- list()
cov <- package_coverage(line_exclusions = exclusions, pre_clean = TRUE)
report(cov)

# upload to codecov.io with this command
str <- "codecov(coverage = cov, token = tok)"
cat("Remember to run", odemodeling:::stancode_string(str),
    "using your codecov.io token!\n")
