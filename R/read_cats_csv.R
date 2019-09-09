#' Read CATS data from CSV
#'
#' \code{read_cats_csv} reads one or more CSV files and returns a standardized
#' data.frame.
#'
#' @param cats_fp A vector of file paths.
#' @param local_tz Local time zone of deployment data.
#' @return A data.frame with columns:
#' * date_UTC (character)
#' * time_UTC (character)
#' * date_local (character)
#' * time_local (character)
#' * accx (double)
#' * accy (double)
#' * accz (double)
#' * gyrx (double)
#' * gyry (double)
#' * gyrz (double)
#' * magx (double)
#' * magy (double)
#' * magz (double)
#' * temp1 (double)
#' * depth (double)
#' * temp2 (double)
#' * light1 (double)
#' * light2 (double)
#' * syserr (character)
#' * battV (double)
#' * battmA (double)
#' * datetime_UTC (POSIXct)
#' * datetime_local (POSIXct)
#' @md
#' @export
read_cats_csv <- function(filepaths, local_tz) {
  # Check arguments
  if (class(filepaths) != "character") {
    stop("argument \"filepaths\" is not a character vector")
  }
  if (any(!file.exists(filepaths))) {
    stop("argument \"filepaths\" includes missing files")
  }
  if (class(local_tz) != "character") {
    stop("argument \"local_tz\" is not a character vector")
  }
  if (!local_tz %in% OlsonNames()) {
    stop(sprintf("%s is not a valid timezone", local_tz))
  }

  # Column names and types of CATS output files
  cats_col_spec <- tibble::tribble(
    ~col_name,    ~col_type,
    "date_UTC",   "c",
    "time_UTC",   "c",
    "date_local", "c",
    "time_local", "c",
    "accx",       "d",
    "accy",       "d",
    "accz",       "d",
    "gyrx",       "d",
    "gyry",       "d",
    "gyrz",       "d",
    "magx",       "d",
    "magy",       "d",
    "magz",       "d",
    "temp1",      "d",
    "depth",      "d",
    "temp2",      "d",
    "light1",     "d",
    "light2",     "d",
    "syserr",     "c",
    "battV",      "d",
    "battmA",     "d")

  # Utility function for converting date and time columns to POSIXct
  datetime <- function(date, time, tz) {
    lubridate::dmy_hms(paste(date, time), tz = tz)
  }

  # Utility function for reading one CATS CSV output file
  read_one <- function(fp) {
    readr::read_csv(fp,
                    col_names = cats_col_spec$col_name,
                    col_types = paste(cats_col_spec$col_type,
                                      collapse = ""),
                    skip = 1,
                    locale = readr::locale(encoding = "latin1"))
  }

  # Read all CATS CSVs, row bind them, and convert date/time to POSIXct
  purrr::map(filepaths, read_one) %>%
    dplyr::bind_rows() %>%
    dplyr::mutate(datetime_UTC = datetime(date_UTC, time_UTC, "UTC"),
                  datetime_local = datetime(date_local, time_local, local_tz)) %>%
    tibble::as_tibble()
}
