% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/read_cats_csv.R
\name{read_cats_csv}
\alias{read_cats_csv}
\title{Read CATS data from CSV}
\usage{
read_cats_csv(filepaths, local_tz)
}
\arguments{
\item{local_tz}{Local time zone of deployment data.}

\item{cats_fp}{A vector of file paths.}
}
\value{
A data.frame with columns:
\itemize{
\item date_UTC (character)
\item time_UTC (character)
\item date_local (character)
\item time_local (character)
\item accx (double)
\item accy (double)
\item accz (double)
\item gyrx (double)
\item gyry (double)
\item gyrz (double)
\item magx (double)
\item magy (double)
\item magz (double)
\item temp1 (double)
\item depth (double)
\item temp2 (double)
\item light1 (double)
\item light2 (double)
\item syserr (character)
\item battV (double)
\item battmA (double)
\item datetime_UTC (POSIXct)
\item datetime_local (POSIXct)
}
}
\description{
\code{read_cats_csv} reads one or more CSV files and returns a standardized
data.frame.
}
