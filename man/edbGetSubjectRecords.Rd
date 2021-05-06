\name{edbGetSubjectRecords}
\alias{edbGetSubjectRecords}
\title{Get Subject Visit and Call Records}
\description{
Returns a dataframe with all visit and call records for a specific SubID
}
\usage{edbGetSubjectRecords(d, ASubID=NULL, Type='ALL')}
\arguments{
  \item{d}{A cleaned enrollment database}
  \item{ASubID}{Numeric SubID}
  \item{Type}{ALL or SCHEDULED}
}
\value{Returns a dataframe including all call and visit records for indicated SubID}
\author{John J. Curtin \email{jjcurtin@wisc.edu}}