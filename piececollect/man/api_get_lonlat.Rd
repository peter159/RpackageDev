% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/api_get_lonlat.r
\name{api_get_lonlat}
\alias{api_get_lonlat}
\title{Get longitude and latitude}
\usage{
api_get_lonlat(address, api = c("baidu", "google", "siweituxin", "gaode"),
  verbose = FALSE)
}
\arguments{
\item{address}{address which can be identified by web api}

\item{api}{one of 'baidu','google','siweituxin'. The names can be abbreviated to any unique substring. Default is "baidu"}

\item{verbose}{one of TRUE, FALSE}
}
\value{
`api_get_lonlat()` return a two number vector regarding longitude and latitude
}
\description{
This is the basic interface of getting longitude and latitude from web api
}
\details{
The output of `api_get_lonlat()` will be a vector of two regarding longitude and latitude
}
\examples{
api_get_lonlat(上海普陀区丽和苑)

}
