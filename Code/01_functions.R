# =============================================================================
# 01_functions.R
# Functions shared by more than one script.
#
# Keep here only code that several scripts must run identically, so that a
# correction made once applies everywhere.
# =============================================================================

# TRUE for dates that fall in the warm season.
#   date   : vector of class Date
#   months : calendar months of the warm season, from
#            get_param("study", "warm_season_months")
is_warm_season <- function(date, months) {
  stopifnot(inherits(date, "Date"))
  stopifnot(is.numeric(months), all(months %in% 1:12))
  as.integer(format(date, "%m")) %in% months
}

# Writes the R version and package versions used in a run next to its results.
#   path : text file to create, for example
#          file.path(dir_tables, "session_info_exposure.txt")
write_session_info <- function(path) {
  writeLines(utils::capture.output(utils::sessionInfo()), path)
}
