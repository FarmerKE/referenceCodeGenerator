% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ref_code_gen.R
\name{reference_code}
\alias{reference_code}
\title{Reference Code Generator}
\usage{
reference_code(ref_code = NULL, device_code = NULL)
}
\arguments{
\item{ref_code}{Unique reference code identifier}

\item{device_code}{Optional device code for multi-application}
}
\value{
a string
}
\description{
A unique code generator for invoices, and multi-application business.
}
\examples{
reference_code()
reference_code("SH")
reference_code("SH", 1)
}
