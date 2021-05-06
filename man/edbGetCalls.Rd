\name{edbGetCalls}
\alias{edbGetCalls}
\title{Get Call List}
\description{
Creates a data frame with a list of calls and associated information
}
\usage{edbGetCalls(d, Start, End, CallType='ALL',CallOutcome='SCHEDULED')}
\arguments{
  \item{d}{A cleaned enrollment database}
  \item{Start}{String to indicate earliest date to include for calls.  YYYY/MM/DD}
  \item{End}{String to indicate lastest date to include for calls.  YYYY/MM/DD}
  \item{CallType}{Types of calls to include.  Default is ALL, otherwise, specify a Call.Type}
  \item{CallOutcome}{Call outcomes to include:  SCHEDULED(default) or ALL}
}
\value{Returns a  dataframe including calls as defined above}
\author{John J. Curtin \email{jjcurtin@wisc.edu}}