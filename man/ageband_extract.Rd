% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ageband_extract.R
\name{ageband_extract}
\alias{ageband_extract}
\title{Easy helper for 'ageband_extract'}
\usage{
ageband_extract(con, schema, table, time_frame = c("FY", "Monthly"))
}
\arguments{
\item{con}{The database connection object to be used}

\item{schema}{The scheme name to extract data from}

\item{table}{The fact table name to extract data from}

\item{time_frame}{"FY"/"Monthly" - the time frame you which to summarise to}
}
\description{
Extract age band table from HRT fact table
}
