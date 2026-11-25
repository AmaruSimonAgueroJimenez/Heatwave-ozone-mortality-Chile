# 04_software.R
# Gets the software of the R pipeline ready before anything long starts, and
# records what this machine has.
#
# Usage, from the project folder:
#   Rscript Code/04_software.R                installs the R packages that are missing
#   Rscript Code/04_software.R --no-install   only checks, and stops if one is missing
#
# The R packages are those of Code/00_packages.R. A missing package is
# installed from CRAN in its current version. The tracked results were
# produced with the R version and the package versions of renv.lock; other
# versions may change the last digits of a result, and the record written here
# says how this machine differs from that file. renv::restore() installs the
# versions of renv.lock instead.
#
# Python is not part of this pipeline: the reports written in Python have
# their own runner, Code/run_python.py.
#
# Output
#   Output/run_environment.txt   R, operating system, Quarto and package versions

arguments <- commandArgs(trailingOnly = TRUE)
if (!all(arguments %in% "--no-install")) {
  stop("Unknown argument: ", paste(setdiff(arguments, "--no-install"), collapse = " "),
       call. = FALSE)
}

# ---- 1. R packages ----------------------------------------------------------

# Before any package is loaded: on a new computer none may be installed
if (!file.exists(file.path("Code", "00_packages.R"))) {
  stop("Run this script from the project folder: open Heatwave-ozone-mortality-Chile.Rproj ",
       "in RStudio, or start R in that folder.", call. = FALSE)
}
source(file.path("Code", "00_packages.R"))
newly_installed <- install_missing_packages(project_packages, allow = !"--no-install" %in% arguments)

source(here::here("Code", "00_setup.R"))
source(here::here("Code", "01_functions.R"))

lock_file <- here::here("renv.lock")
stopifnot(file.exists(lock_file))
lock <- jsonlite::fromJSON(lock_file, simplifyVector = FALSE)
locked <- vapply(lock$Packages, function(p) p$Version, character(1))
stopifnot(all(project_packages %in% names(locked)))
library_here <- installed.packages()
library_here <- library_here[!duplicated(rownames(library_here)), "Version"]
on_this_machine <- intersect(names(locked), names(library_here))
other_version <- on_this_machine[library_here[on_this_machine] != locked[on_this_machine]]
r_same <- as.character(getRversion()) == lock$R$Version

# ---- 2. Quarto and graphics -------------------------------------------------

quarto_version <- system2(quarto_command(), "--version", stdout = TRUE)

# ---- 3. Record --------------------------------------------------------------

record <- c(
  "Software of the machine that last ran the R pipeline (Code/04_software.R)",
  "",
  sprintf("%s%s", R.version.string,
          if (r_same) "" else sprintf("   (renv.lock: R %s)", lock$R$Version)),
  sprintf("Platform: %s", R.version$platform),
  sprintf("System: %s", utils::sessionInfo()$running),
  sprintf("BLAS: %s", extSoftVersion()[["BLAS"]]),
  sprintf("BLAS threads asked for: %s (set in Code/00_setup.R)", Sys.getenv("VECLIB_MAXIMUM_THREADS")),
  sprintf("LAPACK: %s, version %s", La_library(), La_version()),
  sprintf("Character set: %s", Sys.getlocale("LC_CTYPE")),
  "Collation: C (set in Code/00_setup.R)",
  sprintf("Time zone: %s", Sys.timezone()),
  sprintf("Worker processes: %d", n_workers()),
  sprintf("PDF figures: %s device", pdf_figure_device()$name),
  sprintf("Quarto: %s", quarto_version),
  "",
  "R packages loaded by the project (version here):",
  sprintf("  %-12s %s", project_packages, library_here[project_packages]),
  "",
  if (length(other_version) == 0) {
    "Every package of renv.lock that is installed here has the version of renv.lock."
  } else {
    c("Packages whose version here differs from renv.lock (here, renv.lock):",
      sprintf("  %-12s %s, %s", other_version, library_here[other_version],
              locked[other_version]))
  }
)
writeLines(record, here::here("Output", "run_environment.txt"))
cat(record, sep = "\n")
if (length(newly_installed) > 0) {
  message("\nInstalled in this run: ", paste(newly_installed, collapse = ", "))
}
