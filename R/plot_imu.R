# Utility function for plotting triaxial sensors (e.g. accelerometer)
plot_triaxial <- function(data, sensor_name, sensor_cols, max_points) {
  # Enquote sensor_name and sensor_cols so they refer to variables in data
  name_var <- rlang::enquo(sensor_name)
  cols_var <- rlang::enquo(sensor_cols)

  # Generate plot
  data %>%
    # Downsample points
    dplyr::slice(seq(1, nrow(data), length.out = max_points)) %>%
    # Convert data to long format
    tidyr::gather(axis, !!name_var, !!cols_var) %>%
    ggplot2::ggplot(ggplot2::aes(datetime_local, !!name_var, color = axis)) +
    ggplot2::geom_line() +
    ggplot2::theme_minimal()
}

#' Plot acceleration
#'
#' \code{plot_acc} plots x, y, and z-axis acceleration.
#'
#' @param data A data.frame with columns:
#' * datetime_local (POSIXct)
#' * accx (double)
#' * accy (double)
#' * accz (double)
#' @return A ggplot object
#' @md
#' @export
plot_acc <- function(data, max_points = 1e4) {
  # Check inputs
  assertthat::assert_that(is.data.frame(data))
  assertable::assert_colnames(data,
                              c("datetime_local", "accx", "accy", "accz"),
                              only_colnames = FALSE)

  # Generate plot
  plot_triaxial(data, acc, accx:accz, max_points)
}

#' Plot magnetometer
#'
#' \code{plot_mag} plots x, y, and z-axis magnetometer
#'
#' @param data A data.frame with columns:
#' * datetime_local (POSIXct)
#' * magx (double)
#' * magy (double)
#' * magz (double)
#' @return A ggplot object
#' @export
plot_mag <- function(data, max_points = 1e4) {
  # Check inputs
  assertthat::assert_that(is.data.frame(data))
  assertable::assert_colnames(data,
                              c("datetime_local", "magx", "magy", "magz"),
                              only_colnames = FALSE)

  # Generate plot
  plot_triaxial(data, mag, magx:magz, max_points)
}

#' Plot gyroscope
#'
#' \code{plot_gyr} plots x, y, and z-axis gyroscope
#'
#' @param data A data.frame with columns:
#' * datetime_local (POSIXct)
#' * gyrx (double)
#' * gyry (double)
#' * gyrz (double)
#' @return A ggplot object
#' @export
plot_gyr <- function(data, max_points = 1e4) {
  # Check inputs
  assertthat::assert_that(is.data.frame(data))
  assertable::assert_colnames(data,
                              c("datetime_local", "gyrx", "gyry", "gyrz"),
                              only_colnames = FALSE)

  # Generate plot
  plot_triaxial(data, gyr, gyrx:gyrz, max_points)
}
