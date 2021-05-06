\name{rdbGetCallList}
\alias{rdbGetCallList}
\title{Generates a call list for phone recruiting}
\description{
Generates a call lsit for phone recruiting
}
\usage{rdbGetCallList(d, Filter = 'DateDayTime')}
\arguments{
  \item{d}{A cleaned recruiting database as a dataframe}
  \item{Filter}{Four levels of filter are provided:  None, Date, DateDay, and 
  DateDayTime}
}
\value{returns a dataframe that includes the call list}
\author{John J. Curtin \email{jjcurtin@wisc.edu}}