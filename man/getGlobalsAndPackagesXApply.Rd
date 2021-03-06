% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/getGlobalsAndPackagesXApply.R
\name{getGlobalsAndPackagesXApply}
\alias{getGlobalsAndPackagesXApply}
\title{Identify Globals and Packages from a Map-Reduce Function Call}
\usage{
getGlobalsAndPackagesXApply(
  FUN,
  args = NULL,
  MoreArgs = NULL,
  envir,
  future.globals = TRUE,
  future.packages = NULL,
  debug = getOption("future.debug", FALSE)
)
}
\arguments{
\item{FUN}{...}

\item{args}{...}

\item{MoreArgs}{...}

\item{envir}{...}

\item{future.globals}{...}

\item{future.packages}{...}

\item{debug}{...}
}
\value{
A names list with elements \code{globals}, \code{packages}, and
\code{scanForGlobals}.
}
\description{
Identify Globals and Packages from a Map-Reduce Function Call
}
