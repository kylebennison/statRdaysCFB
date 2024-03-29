% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get.R
\name{get_anything}
\alias{get_anything}
\title{Get Anything From the CFBD API}
\usage{
get_anything(
  url,
  start_year = 2021,
  end_year = 2021,
  start_week,
  end_week,
  key = my_key()
)
}
\arguments{
\item{url}{string. The url of the API endpoint WITHOUT any parameters added onto the end.
If you have any parameters to add, please use the provided fields and we will add them
to the url for you.}

\item{start_year}{int first year of data to return}

\item{end_year}{int last year of data to return}

\item{start_week}{int first week of data to return}

\item{end_week}{int last week of data to return}

\item{key}{your API key. Use variable my_key() or make a call to Sys.getenv("cfbd_staturdays_key")}
}
\description{
Get data from any API endpoint on cfbdata.com
}
\details{
To view API endpoints, visit https://api.collegefootballdata.com/api/docs/?url=/api-docs.json#/
}
\examples{
get_anything(url = "https://api.collegefootballdata.com/venues")
}
