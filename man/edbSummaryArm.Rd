\name{edbSummaryArm}
\alias{edbSummaryArm}
\title{Summary of Enrollments by Arm}
\description{
Returns a dataframe with enrollments by Arm (and Sex)
}
\usage{edbSummaryArm(d, Status='COMPLETED')}
\arguments{
  \item{d}{A cleaned enrollment database}
  \item{Status}{Include COMPLETED (Default) or COMPLETED+, which is completed + active}
}
\value{Returns a dataframe with enrollments by Arm (and Sex)}
\author{John J. Curtin \email{jjcurtin@wisc.edu}}