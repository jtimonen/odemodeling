.onAttach <- function(...) {
  msg <- create_startup_message()
  packageStartupMessage(msg)
}

# Create package startup message
create_startup_message <- function() {
  v_ot <- pkg_version("odemodeling")
  msg <- paste0("Attached odemodeling ", v_ot, ".")
  return(msg)
}

# Create package description
pkg_version <- function(pkg_name) {
  Lib <- dirname(system.file(package = pkg_name))
  pkgdesc <- suppressWarnings(
    utils::packageDescription(pkg_name, lib.loc = Lib)
  )
  if (length(pkgdesc) > 1) {
    out <- pkgdesc$Version
  } else {
    out <- ""
  }
  return(out)
}
