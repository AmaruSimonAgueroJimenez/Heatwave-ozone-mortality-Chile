# 00_packages.R
# The R packages of the project, and the function that installs the missing
# ones. Base R only: this file is read before any package is loaded, so that
# the pipeline can start on a computer where nothing is installed yet.
#
# Usage: source("Code/00_packages.R") from Code/run_all.R and Code/04_software.R.

# Every package the scripts, the checks of the methods and the R reports load
project_packages <- c(
  "here", "yaml", "data.table",                 # project folder, parameters, data
  "sf", "sp", "gstat", "automap",               # geometries and interpolation
  "ncdf4", "nanoparquet",                       # reanalysis and population files
  "survival", "dlnm", "msm", "mvtnorm",         # models and checks of the methods
  "ggplot2", "patchwork", "ragg", "scales",     # figures
  "knitr", "rmarkdown", "kableExtra",           # reports
  "jsonlite"                                    # reads renv.lock
)

# Install the packages of `packages` that are missing, from CRAN, as binaries
# where the system has them. With allow = FALSE nothing is installed: the
# function stops and names the command to run. Returns the names it installed.
install_missing_packages <- function(packages, allow) {
  installed <- vapply(packages, requireNamespace, logical(1), quietly = TRUE)
  missing <- packages[!installed]
  if (length(missing) == 0) return(invisible(character(0)))
  command <- paste0("install.packages(c(", paste0('"', missing, '"', collapse = ", "), "))")
  if (!allow) {
    stop("R packages not installed: ", paste(missing, collapse = ", "),
         "\nInstall them with\n  ", command, call. = FALSE)
  }
  message("Installing the R packages that are missing: ", paste(missing, collapse = ", "))
  repositories <- getOption("repos")
  if (is.null(repositories) || !"CRAN" %in% names(repositories) ||
      repositories[["CRAN"]] %in% c("@CRAN@", "")) {
    repositories <- c(CRAN = "https://cloud.r-project.org")
  }
  # The first library when it can be written to, otherwise the personal library
  library_folder <- .libPaths()[1]
  if (file.access(library_folder, mode = 2) != 0) {
    library_folder <- path.expand(Sys.getenv("R_LIBS_USER"))
    dir.create(library_folder, recursive = TRUE, showWarnings = FALSE)
    .libPaths(c(library_folder, .libPaths()))
  }
  # Never stop to ask whether a newer version should be compiled from source
  old <- options(install.packages.compile.from.source = "never")
  on.exit(options(old))
  utils::install.packages(missing, lib = library_folder, repos = repositories)
  still_missing <- missing[!vapply(missing, requireNamespace, logical(1), quietly = TRUE)]
  if (length(still_missing) > 0) {
    stop("These R packages could not be installed: ", paste(still_missing, collapse = ", "),
         "\nTry by hand: ", command, call. = FALSE)
  }
  invisible(missing)
}
