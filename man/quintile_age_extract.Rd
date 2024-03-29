% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/quintile_age_extract.R
\name{quintile_age_extract}
\alias{quintile_age_extract}
\title{Easy helper for 'quintile_age_extract'}
\usage{
quintile_age_extract(con, schema, table, time_frame = c("FY", "Monthly"))
}
\arguments{
\item{con}{The database connection object to be used}

\item{schema}{The scheme name to extract data from}

\item{table}{The fact table name to extract data from}

\item{time_frame}{"FY"/"Monthly" - the time frame you which to summarise to}
}
\description{
Extract imd quintile and age band table from HRT fact table
}
