\name{edbGetVisits}
\alias{edbGetVisits}
\title{Get List of Visits}
\description{
Returns a dataframe with visits within the specific time window and VisitType
}
\usage{edbGetVisits(d, Start, End, VisitType='ALL', VisitOutcome='SCHEDULED')}
\arguments{
  \item{d}{A cleaned enrollment database}
  \item{Start}{String to indicate the earliest visit to include.  YYYY/MM/DD}
  \item{End}{String to indicate the latest visit to include.  YYYY/MM/DD}
  \item{VisitType}{ALL or name of visit from Visit.Type}
  \item{VisitOutcome}{Visit outcomes to include:  SCHEDULED(default) or ALL}
}
\value{returns a dataframe including visits within specified window and type}
\author{John J. Curtin \email{jjcurtin@wisc.edu}}