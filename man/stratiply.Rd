\name{stratiply}
\alias{stratiply}
\title{Stratify a data frame, apply a function to each strata, and collect results}
\description{
Calls \code{"\link{divide}"} to separate a data frame into one or more stratified frames, applies a function to each strata, and optionally collects the results and returns strata to their original columns.
}
\usage{
stratiply(
    data,
    strata,
    f,
    delimiter = "|",
    bind = FALSE,
    separate = FALSE,
    ...
)
}
\arguments{
\item{data}{Any \code{data.frame}.}
\item{strata}{See the \code{by} argument in \code{"\link{divide}"}.}
\item{f}{Any function that takes a \code{data.frame} as an argument.}
\item{delimiter}{See the \code{sep} argument in \code{"\link{divide}"}. Also used for salvaging original columns.}
\item{bind}{Should the results be binded back to a single \code{data.frame}? Defaults to \code{FALSE}.}
\item{separate}{If \code{bind = TRUE}, should the stratification variables be separated back to their original columns? Defaults to \code{FALSE}.}
\item{...}{Additional arguments passed to \code{f}.}
}
\value{
    If \code{bind = FALSE}, a named \code{list} with the results of \code{f} for each strata. Otherwise the results are binded to a single frame.
}
\author{Alex Zajichek}
\examples{
require(tidyverse)

#1) Return a list
heart_disease \%>\%
    stratiply(
        strata = c("Sex", "HeartDisease"),
        f = head
    )

#2) Unseparated strata column
heart_disease \%>\%
    stratiply(
        strata = c("Sex", "HeartDisease"),
        f = head,
        bind = TRUE
    )

#3) Separated strata column
heart_disease \%>\%
    stratiply(
        strata = c("Sex", "HeartDisease"),
        f = head,
        bind = TRUE,
        separate = TRUE
    )
    
#4) Custom function
heart_disease \%>\%
    stratiply(
        strata = c("Sex", "HeartDisease"),
        f = function(x) 
            x \%>\% 
            select_if(is.numeric) \%>\% 
            map(mean, na.rm = TRUE),
        bind = TRUE,
        separate = TRUE
    )    
    
#5) Regular expression
heart_disease \%>\%
    stratiply(
        strata = matches("^(S|H)"),
        f = function(x) 
            x \%>\% 
            select_if(is.numeric) \%>\% 
            map(mean, na.rm = TRUE),
        bind = TRUE,
        separate = TRUE
    )  
}
