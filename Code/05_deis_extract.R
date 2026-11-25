# 05_deis_extract.R
# Cuts the deaths extract used by the analysis from the public DEIS file.
#
# Not a step of the pipeline: run_all.R does not call it, because the extract
# is one of the raw inputs and the public file (870 MB, the whole country since
# 1990) is not needed to run the analysis. The script records how the extract
# was obtained. It writes the extract when it is absent, and otherwise checks
# that the extract in place is what the public file gives.
#
# The extract is the header of the public file followed by its rows with the
# years of death and the region of residence of config$deis_extract, in their
# original order and with their original bytes (Latin-1, semicolons).
#
# Input
#   inputs$deis_official   DEFUNCIONES_FUENTE_DEIS_1990_2023_CIFRAS_OFICIALES.csv,
#                          open data of DEIS, Ministry of Health of Chile
# Output
#   inputs$deis_deaths     written only when it does not exist

source(here::here("Code", "00_setup.R"))

rules <- get_param("deis_extract")
official <- input_path("deis_official")           # checked against the manifest
target <- input_location("deis_deaths")
expected <- read_input_manifest()[input == "deis_deaths"]
stopifnot(nrow(expected) == 1)

# Year of death is the first column and region of residence the eighth. The
# rows are matched as bytes, so that the Latin-1 text is never re-encoded.
years <- seq(rules$years[1], rules$years[2])
pattern <- sprintf("^(%s);([^;]*;){6}\\Q%s\\E;", paste(years, collapse = "|"), rules$region)

# The extract keeps the end of line of the public file
first_bytes <- readBin(official, "raw", n = 1e5)
end_of_line <- if (any(first_bytes == as.raw(13))) "\r\n" else "\n"

rebuilt <- tempfile(fileext = ".csv")
source_file <- file(official, "rb")
rebuilt_file <- file(rebuilt, "wb")
header <- readLines(source_file, n = 1, warn = FALSE)
header_columns <- strsplit(header, ";", fixed = TRUE, useBytes = TRUE)[[1]]
stopifnot(length(header_columns) >= 8, header_columns[2] == "FECHA_DEF",
          header_columns[8] == "NOMBRE_REGION")
writeLines(header, rebuilt_file, sep = end_of_line, useBytes = TRUE)
rows_read <- 0
rows_kept <- 0
repeat {
  lines <- readLines(source_file, n = 200000L, warn = FALSE)
  if (length(lines) == 0) break
  keep <- grepl(pattern, lines, perl = TRUE, useBytes = TRUE)
  writeLines(lines[keep], rebuilt_file, sep = end_of_line, useBytes = TRUE)
  rows_read <- rows_read + length(lines)
  rows_kept <- rows_kept + sum(keep)
}
close(source_file)
close(rebuilt_file)
message("Rows of the public file: ", rows_read, "; rows of the extract: ", rows_kept)

if (file.size(rebuilt) != expected$bytes || unname(tools::md5sum(rebuilt)) != expected$md5) {
  stop("The extract cut from the public file is not the one of Data/input_manifest.csv.",
       call. = FALSE)
}
if (file.exists(target)) {
  verify_input("deis_deaths", target)
  message("The extract in place is the one cut from the public file.")
} else {
  dir.create(dirname(target), recursive = TRUE, showWarnings = FALSE)
  stopifnot(file.copy(rebuilt, target))
  message("Extract written: ", target)
}
