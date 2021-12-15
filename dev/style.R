library(styler)

# Format the package code according to the tidyverse guide
exclusions <- list()
styler:::style_pkg(exclude_files = exclusions)
