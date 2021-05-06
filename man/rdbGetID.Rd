\name{rdbGetID}
\alias{rdbGetID}
\title{Finds Phone ID based on last name}
\description{
Returns a Phone ID based on a full or partial match to last name
}
\usage{rdbGetID(d, Last)}
\arguments{
  \item{d}{A cleaned recruiting database as a dataframe}
  \item{Last}{String with full or partial last name}
}
\value{Returns a dataframe with relevant columns from the first participant contact}
\author{John J. Curtin \email{jjcurtin@wisc.edu}}