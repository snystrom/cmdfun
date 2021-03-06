% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/macros.R
\name{cmd_install_is_valid}
\alias{cmd_install_is_valid}
\title{Macro for constructing boolean check for valid path}
\usage{
cmd_install_is_valid(path_search, util = NULL)
}
\arguments{
\item{path_search}{function output of \code{cmd_path_search()} \strong{NOTE:} When
passing the function, do not pass as: \code{fun()}, but \code{fun} to avoid evaluation.}

\item{util}{value to pass to \code{util} argument of \code{path_search}, allows
building individual functions for each util (if passing one of each),
or for simultaneously checking all utils if setting \code{util = TRUE}. Will
cause error if \code{util = TRUE} but no utils are defined. \strong{NOTE:} There is
no error checking for whether \code{util} is set correctly during the build
process, so ensure correct spelling, etc. to avoid cryptic failures.}
}
\value{
a function returning TRUE or FALSE if a valid install is detected.
With arguments: \code{path} (a path to install location), \code{util} an optional \code{character(1)} to
}
\description{
Macro for constructing boolean check for valid path
}
\examples{
if (.Platform$OS.type == "unix") {
search <- cmd_path_search(option_name = "bin_path", default_path = "/bin/")
valid_install <- cmd_install_is_valid(search)
# Returns TRUE if "/bin/" exists
valid_install()
# Returns FALSE if "bad/path/" doesn't exist
valid_install("bad/path/")

# Also works with options
search_option_only <- cmd_path_search(option_name = "bin_path")
valid_install2 <- cmd_install_is_valid(search_option_only)
options(bin_path = "/bin/")
valid_install2()

# Setting util = TRUE will check that all utils are also installed
search_with_utils <- cmd_path_search(default_path = "/bin", utils = c("ls", "pwd"))
valid_install_all <- cmd_install_is_valid(search_with_utils, util = TRUE)
valid_install_all()
}
}
