\name{rdbGetEntries}
\alias{rdbGetEntries}
\title{Returns entries for a specific Phone ID}
\description{
Returns all entries associated with a specific Phone number ID
}
\usage{rdbGetEntries(d = NULL, Phone = NULL)}
\arguments{
  \item{d}{A cleaned recruiting database as a dataframe}
  \item{Phone}{10 digit phone number as string: ###-###-####}
}
\value{returns a  dataframe with all entries for that phone number}
\author{John J. Curtin \email{jjcurtin@wisc.edu}}