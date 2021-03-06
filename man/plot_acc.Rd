% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot_imu.R
\name{plot_acc}
\alias{plot_acc}
\title{Plot acceleration}
\usage{
plot_acc(data, max_points = 10000)
}
\arguments{
\item{data}{A data.frame with columns:
\itemize{
\item datetime_local (POSIXct)
\item accx (double)
\item accy (double)
\item accz (double)
}}
}
\value{
A ggplot object
}
\description{
\code{plot_acc} plots x, y, and z-axis acceleration.
}
