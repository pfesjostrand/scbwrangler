% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/funs_load_edit.R
\name{fix_factors}
\alias{fix_factors}
\title{fix_factors}
\usage{
fix_factors(data, classdic = "default", fr = 2, to = 3)
}
\arguments{
\item{data}{Data frame object of which the vars are to be reclassed.}

\item{classdic}{Either a path to a .csv file or a data frame object.}

\item{fr}{The column in classdic where the names are found}

\item{to}{The column to classdic where class mappings are found}
}
\value{
Returns a reclassed data frame
}
\description{
Data in SCB .px files often comes in factors if the data is not strictly
numeric. E.g years are coded as factors not integers. This at times (often)
a nuisance. fix_factors change the class based on the names of the variable
using a dictionary mapping class to name. The "classdictionary" can either
have two colums named english (due to this function being intended to be
applied after translate_names) and one called class or define which colums
should be used with fr (from) and to; either by index or by column name.
}
\details{
This function works but could do with a little more work...
}
