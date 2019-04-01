\name{divide}
\alias{divide}
\title{Divide a data frame into a list by any number of variables.}
\description{
Separates a data frame by one or more stratification variables into a list of data frames whose names are the strata. Removes stratification variables from each frame.
}
\usage{
divide(
    data,
    by,
    sep = "|"
)
}
\arguments{
\item{data}{Any \code{data.frame}.}
\item{by}{Character vector of variable names or a \code{tidyselect::select_helper}.}
\item{sep}{String to separate values of each stratification variable by. Defaults to \code{"|"}.}
}
\value{
A named \code{list} of \code{data.frame} objects
}
\author{Alex Zajichek}
\examples{
require(tidyverse)

#1) Exact match
heart_disease \%>\%
    divide(
        by = "Sex"
    )

#2) Regular expression
heart_disease \%>\%
    divide(
        by = matches("^(S|H)")
    )

#3) Beginning character
heart_disease \%>\%
    divide(
        by = starts_with("S")
    )
    
#4) Negation
heart_disease \%>\%
    select(
        Sex,
        HeartDisease,
        BloodSugar
    ) \%>\%
    divide(
        by = -matches("^(S|H)")
)
}  
