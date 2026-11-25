# 03_input_manifest.R
# Checks that the raw inputs are the files the tracked results were produced
# from, or writes the manifest that says which files those are.
#
# Usage, from the project root:
#   Rscript Code/03_input_manifest.R            check every input
#   Rscript Code/03_input_manifest.R --write    write the manifest from the
#                                               files now in place
#
# Data/input_manifest.csv lists every input file with its size and MD5
# checksum. Each script checks the inputs it reads against it (input_path() in
# Code/00_setup.R), so that no result can come unnoticed from another version
# of a file. Write a new manifest only after a deliberate change of input, and
# commit it together with the results that the new input produces.
#
# Output (with --write)
#   Data/input_manifest.csv   input, file, bytes, md5

source(here::here("Code", "00_setup.R"))
source(here::here("Code", "01_functions.R"))

arguments <- commandArgs(trailingOnly = TRUE)
if (!all(arguments %in% "--write")) {
  stop("Unknown argument: ", paste(setdiff(arguments, "--write"), collapse = " "), call. = FALSE)
}
write_manifest <- "--write" %in% arguments

inputs <- names(get_param("inputs"))
optional <- unlist(get_param("optional_inputs"))
stopifnot(all(optional %in% inputs))
# Inputs that are folders: the files the scripts read in each, relative to it
folder_files <- list(sinca_hourly_dir = sinca_hourly_files(), cams_eac4_dir = cams_month_files())
stopifnot(all(names(folder_files) %in% inputs))

# One row per file of an input, or NULL when the input is not on this machine
describe_input <- function(name) {
  path <- input_location(name)
  if (!file.exists(path)) return(NULL)
  is_folder <- name %in% names(folder_files)
  stopifnot(dir.exists(path) == is_folder)
  files <- if (is_folder) folder_files[[name]] else basename(path)
  full <- if (is_folder) file.path(path, files) else path
  absent <- full[!file.exists(full)]
  if (length(absent) > 0) {
    stop("Input '", name, "': file not found:\n  ", paste(absent, collapse = "\n  "), call. = FALSE)
  }
  data.table(input = name, file = files, bytes = file.size(full),
             md5 = unname(tools::md5sum(full)))
}

if (write_manifest) {
  found <- lapply(setNames(inputs, inputs), describe_input)
  absent <- names(found)[vapply(found, is.null, logical(1))]
  if (length(setdiff(absent, optional)) > 0) {
    stop("Inputs not found: ", paste(setdiff(absent, optional), collapse = ", "),
         "\nSee Data/README.md for where each input comes from.", call. = FALSE)
  }
  manifest <- rbindlist(found)
  # An optional input that is not on this machine keeps the rows it had
  if (length(absent) > 0 && file.exists(input_manifest_file)) {
    kept <- read_input_manifest()[input %in% absent]
    manifest <- rbind(manifest, kept)
    if (nrow(kept) > 0) message("Rows kept as they were: ", paste(unique(kept$input), collapse = ", "))
  }
  manifest <- manifest[order(match(input, inputs))]
  fwrite(manifest, input_manifest_file)
  message("Data/input_manifest.csv: ", nrow(manifest), " files of ",
          uniqueN(manifest$input), " inputs, ",
          format(round(sum(manifest$bytes) / 1e6), big.mark = ","), " MB")
} else {
  manifest <- read_input_manifest()
  stopifnot(all(manifest$input %in% inputs), all(setdiff(inputs, optional) %in% manifest$input))
  # The manifest must list, for each folder, the files the scripts will read
  for (name in names(folder_files)) {
    if (!setequal(manifest[input == name, file], folder_files[[name]])) {
      stop("The files listed for '", name, "' in Data/input_manifest.csv are not those ",
           "the configuration asks for.", call. = FALSE)
    }
  }
  status <- rbindlist(lapply(inputs, function(name) {
    expected <- manifest[input == name]
    summary <- data.table(input = name, files = nrow(expected),
                          megabytes = round(sum(expected$bytes) / 1e6, 1))
    path <- input_location(name)
    if (!file.exists(path)) {
      if (!name %in% optional) {
        stop("Input '", name, "' not found: ", path,
             "\nSee Data/README.md for where each input comes from.", call. = FALSE)
      }
      return(cbind(summary, status = "not on this machine (optional)"))
    }
    verify_input(name, path, manifest)
    cbind(summary, status = "as in the manifest")
  }))
  print(status)
  message("Every input on this machine is the file listed in Data/input_manifest.csv.")
}
